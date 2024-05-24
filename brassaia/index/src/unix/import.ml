module Int63 = Optint.Int63

type int63 = Int63.t

module Mtime = struct
  include Mtime

  let span_to_s span = Mtime.Span.to_float_ns span *. 1e-9
  let span_to_us span = Mtime.Span.to_float_ns span *. 1e-3
end
