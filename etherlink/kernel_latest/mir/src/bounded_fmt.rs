// SPDX-FileCopyrightText: [2026] Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Byte-bounded rendering of [`std::fmt::Display`] / [`std::fmt::Debug`]
//! values.
//!
//! # Why this exists
//!
//! A Michelson value can be *tiny in memory but expand exponentially when
//! printed*. A `{ DUP; DUP; PAIR }` loop of `K` steps is a shared-pointer
//! DAG of `K + 1` heap nodes (built for `O(K)` gas, since `DUP` is an
//! `Rc::clone` and `PAIR` stores both children as the same `Rc`), but its
//! *unfolded* form is `2^(K+1) - 1` nodes. When such a value reaches a
//! `FAILWITH`, the interpreter embeds it in
//! [`crate::interpreter::InterpretError::FailedWith`], whose error text
//! renders the value through the (sharing-unaware) iterative `Debug` walk
//! in [`crate::ast`]. That walk visits *both* children of every `Pair`, so
//! a value built for a few thousand milligas materialises a `2^K`-byte
//! string — `K ≈ 30` is enough to OOM the kernel for a fraction of a tez.
//!
//! The `Debug` walk is *stack*-safe (iterative) and the `FailedWith` payload
//! is drained before `Drop`, but neither bounds the *work/size*
//! amplification: the string is still built in full before any per-byte gas
//! charge can observe its length, so charging after the fact cannot prevent
//! the allocation.
//!
//! A related — but *gas-bounded* — vector exists on *types*: the
//! typechecker embeds `Type` / `TypeStack` in
//! [`crate::typechecker::TcError`] via `{:?}`, and a type synthesised during
//! typechecking can be a structurally-shared `Rc` DAG whose sharing-unaware
//! render is far larger than its in-memory size. Unlike the `FAILWITH`
//! *value* (runtime `DUP` is a flat-cost `Rc::clone`, so an `O(K)`-gas value
//! renders to `2^K` nodes), building such a *type* is not cheap: the
//! typechecker's `DUP` re-checks `Duplicable` by walking the *unfolded* type,
//! so a type that renders to `N` nodes costs `Θ(N)` gas to build (the
//! per-type parse cap `MICHELSON_MAXIMUM_TYPE_SIZE` does not bound
//! instruction-synthesised types). The render is therefore bounded by
//! `O(gas)` rather than unbounded — but that ceiling is still large (a
//! max-gas origination/call could synthesise a type rendering to tens of MB),
//! so the typecheck-error sinks are capped too, as defense-in-depth and to
//! keep every error sink uniformly byte-bounded.
//!
//! # Load-bearing assumption
//!
//! The cap works only because every formatter in the rendered value's
//! `Display`/`Debug` chain writes *straight into the caller's formatter*
//! and propagates its `Err` with `?` — no arm swallows the write error or
//! builds a sub-render eagerly via `format!` / `to_string` mid-walk (which
//! would re-materialise the full string in a fresh unbounded sink before
//! this writer ever saw a byte). The MIR error chains satisfy this: the
//! iterative `Debug` impls for `Type` and `TypedValue` thread the formatter
//! through with `?`, and the `thiserror`-derived `Display` impls interpolate
//! their exponential fields directly (`{0:?}`). The optional reason of
//! `TcError::NoMatchingOverload` is rendered through a lazy `Display` adapter
//! rather than an eager `format!`, so it, too, propagates.
//!
//! # What this does
//!
//! [`BoundedWriter`] is a [`std::fmt::Write`] sink that accumulates at most
//! `max_bytes` of UTF-8 and then **aborts the producing format walk** by
//! returning [`std::fmt::Error`]. Because the iterative walks propagate the
//! first write error with `?`, the walk stops after roughly `max_bytes`
//! have been emitted — bounding both the peak allocation and the CPU spent,
//! regardless of how the underlying value is shared. The cap is applied
//! *during* the walk, not after a full `format!`, which is the property the
//! exponential blow-up requires.
//!
//! [`display_bounded`] / [`debug_bounded`] wrap that into a `String` for the
//! kernel error sinks (the native failed-operation receipt path and the
//! cross-runtime `BadRequest` path), appending a truncation marker when the
//! cap is hit. They drive the format machinery with [`write!`] rather than
//! [`ToString::to_string`], so the writer's `Err` is observed and handled
//! here instead of triggering `to_string`'s "a Display implementation
//! returned an error unexpectedly" panic.

use std::fmt::{self, Debug, Display, Write};

/// Maximum number of bytes of rendered error text the kernel persists or
/// returns for a single Michelson interpreter / typechecker error.
///
/// This is a hard ceiling applied while the value is being walked, so the
/// peak allocation cannot exceed roughly this size plus the
/// [`TRUNCATION_SUFFIX`] — independent of the (gas-cheap, structurally
/// shared) value's *unfolded* size. It is deliberately generous: normal
/// `FAILWITH` payloads and error messages are far smaller, so well-behaved
/// errors are rendered in full, while the pathological exponential / giant
/// payloads are cut off well below any out-of-memory threshold.
pub const MAX_INTERPRET_ERROR_RENDER_BYTES: usize = 64 * 1024;

/// Appended to the rendered text when the value did not fit in the byte
/// budget, so a truncated error is not mistaken for a complete one.
pub const TRUNCATION_SUFFIX: &str = "…[truncated]";

/// A [`std::fmt::Write`] sink that accumulates at most `max_bytes` of UTF-8
/// and then aborts the producing format walk.
///
/// Once the budget is reached the sink records that truncation happened and
/// returns [`fmt::Error`] from [`write_str`](BoundedWriter::write_str), which
/// the iterative `Debug` walks propagate with `?`, stopping the walk. The
/// buffer never exceeds `max_bytes` and always ends on a UTF-8 character
/// boundary.
pub struct BoundedWriter {
    buf: String,
    max_bytes: usize,
    truncated: bool,
}

impl BoundedWriter {
    /// Create a writer that accepts at most `max_bytes` bytes of content.
    pub fn new(max_bytes: usize) -> Self {
        BoundedWriter {
            buf: String::new(),
            max_bytes,
            truncated: false,
        }
    }

    /// Whether the budget was exceeded and the content was truncated.
    pub fn truncated(&self) -> bool {
        self.truncated
    }

    /// Consume the writer, returning the accumulated content with
    /// [`TRUNCATION_SUFFIX`] appended iff truncation occurred.
    pub fn into_string(mut self) -> String {
        if self.truncated {
            self.buf.push_str(TRUNCATION_SUFFIX);
        }
        self.buf
    }
}

impl Write for BoundedWriter {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        if self.truncated {
            // Budget already exhausted on a previous write; keep aborting.
            return Err(fmt::Error);
        }
        // Invariant: `buf.len() <= max_bytes`, so this does not underflow.
        let remaining = self.max_bytes - self.buf.len();
        if s.len() <= remaining {
            self.buf.push_str(s);
            Ok(())
        } else {
            // Append the largest prefix of `s` that fits and lands on a
            // char boundary, then abort the walk. Truncating a single
            // oversized leaf (e.g. a multi-megabyte `string` literal) this
            // way bounds the allocation even when one write is huge.
            let mut end = remaining;
            while end > 0 && !s.is_char_boundary(end) {
                end -= 1;
            }
            self.buf.push_str(&s[..end]);
            self.truncated = true;
            Err(fmt::Error)
        }
    }
}

/// Render `value` via [`Display`], truncated to at most `max_bytes` of
/// content (plus a [`TRUNCATION_SUFFIX`] marker when truncated).
///
/// The walk is aborted as soon as the budget is hit, so this is safe to call
/// on attacker-controlled values whose full rendering would be unbounded.
pub fn display_bounded<T: Display + ?Sized>(value: &T, max_bytes: usize) -> String {
    let mut writer = BoundedWriter::new(max_bytes);
    // Ignore the result: an `Err` only means the byte budget was reached,
    // which `into_string` reflects via the truncation marker. Using
    // `write!` (not `to_string`) means this `Err` is handled here rather
    // than panicking inside `ToString`.
    let _ = write!(writer, "{value}");
    writer.into_string()
}

/// Render `value` via [`Debug`], truncated to at most `max_bytes` of content
/// (plus a [`TRUNCATION_SUFFIX`] marker when truncated). See
/// [`display_bounded`].
pub fn debug_bounded<T: Debug + ?Sized>(value: &T, max_bytes: usize) -> String {
    let mut writer = BoundedWriter::new(max_bytes);
    let _ = write!(writer, "{value:?}");
    writer.into_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn short_value_rendered_in_full() {
        assert_eq!(display_bounded("hello", 64), "hello");
        assert_eq!(debug_bounded("hello", 64), "\"hello\"");
    }

    #[test]
    fn exact_fit_is_not_truncated() {
        // "hello" is exactly 5 bytes.
        let mut w = BoundedWriter::new(5);
        let _ = write!(w, "hello");
        assert!(!w.truncated());
        assert_eq!(w.into_string(), "hello");
    }

    #[test]
    fn oversized_value_is_truncated_with_marker() {
        let out = display_bounded(&"x".repeat(1000), 16);
        assert!(out.starts_with(&"x".repeat(16)));
        assert!(out.ends_with(TRUNCATION_SUFFIX));
        // Content is capped at the budget; only the marker follows.
        assert_eq!(out.len(), 16 + TRUNCATION_SUFFIX.len());
    }

    #[test]
    fn truncation_respects_utf8_char_boundaries() {
        // Each '€' is 3 bytes; with a 4-byte budget only one fits.
        let out = display_bounded("€€€€", 4);
        assert!(out.starts_with('€'));
        assert!(out.ends_with(TRUNCATION_SUFFIX));
        // One 3-byte char fits in the 4-byte budget; the partial second
        // char is rejected rather than splitting a code point.
        assert_eq!(out, format!("€{TRUNCATION_SUFFIX}"));
    }

    #[test]
    fn zero_budget_truncates_everything() {
        let out = display_bounded("anything", 0);
        assert_eq!(out, TRUNCATION_SUFFIX);
    }

    /// The motivating case: a writer fed an unbounded producer stops after
    /// the budget. We simulate the exponential `Debug` walk with a producer
    /// that would write "ab" forever; the bounded writer must abort it.
    #[test]
    fn aborts_an_unbounded_producer() {
        struct Forever;
        impl Display for Forever {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                loop {
                    // Each write is checked against the budget; once the
                    // budget is hit `write_str` returns `Err` and `?`
                    // breaks the otherwise infinite loop.
                    f.write_str("ab")?;
                }
            }
        }
        let out = display_bounded(&Forever, 1000);
        assert!(out.ends_with(TRUNCATION_SUFFIX));
        assert_eq!(out.len(), 1000 + TRUNCATION_SUFFIX.len());
    }
}
