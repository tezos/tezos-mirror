// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024-2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::fs;
use std::io::Write;
use std::ops::Bound;

use goldenfile::Mint;
use paste::paste;

use crate::jit::JIT;
use crate::machine_state::block_cache::block::Block;
use crate::machine_state::block_cache::block::Interpreted;
use crate::machine_state::block_cache::block::Jitted;
use crate::machine_state::memory::M1M;
use crate::machine_state::registers::XRegister;
use crate::machine_state::registers::XValue;
use crate::state_backend::ManagerRead;
use crate::state_backend::owned_backend::Owned;
use crate::stepper::Stepper;
use crate::stepper::test::TestStepper;
use crate::stepper::test::TestStepperResult::*;

const TESTS_DIR: &str = "../assets/generated";
const GOLDEN_DIR: &str = "tests/expected";
const MAX_STEPS: usize = 1_000_000;

fn check_register_values<S: Stepper>(stepper: &S, check_xregs: &[(XRegister, XValue)])
where
    S::Manager: ManagerRead,
{
    let failure = check_xregs
        .iter()
        .filter_map(|(xreg, xval)| {
            let res = stepper.machine_state().hart.xregisters.read(*xreg);
            (res != *xval).then(|| format!("\n- check {xreg} == {xval} | got {res}"))
        })
        .collect::<String>();

    if !failure.is_empty() {
        panic!("XRegisters conditions not satisfied: {failure}");
    };
}

fn run_test_with_check<B: Block<M1M, Owned>>(
    path: &str,
    check_xregs: &[(XRegister, u64)],
    block_builder: B::BlockBuilder,
) -> B::BlockBuilder {
    // Create a Mint instance: when it goes out of scope (at the end of interpret_test_with_check),
    // all golden files will be compared to the checked-in versions.
    let mut mint = Mint::new(GOLDEN_DIR);
    let mut golden = mint.new_goldenfile(format!("{path}.out")).unwrap();

    let contents = fs::read(format!("{TESTS_DIR}/{path}")).expect("Failed to read binary");

    let mut interpreter: TestStepper<M1M, _, B> =
        TestStepper::new(&contents, None, block_builder).expect("Boot failed");

    let res = interpreter.step_max(Bound::Included(MAX_STEPS));
    // Record the result to compare to the expected result
    writeln!(golden, "{res:?}").unwrap();
    match res {
        Exit { code: 0, .. } => check_register_values(&interpreter, check_xregs),
        Exit { code, steps } => {
            panic!("Failed at test case: {} - Steps done: {}", code >> 1, steps)
        }
        Running { .. } => panic!("Timeout"),
        Exception {
            cause,
            message,
            steps,
        } => match message {
            Some(message) => panic!(
                "Unexpected exception after {steps} steps: {message} (caused by {:?})",
                cause
            ),
            None => panic!("Unexpected exception after {steps} steps: {:?}", cause),
        },
    };

    interpreter.recover_builder()
}

fn interpret_test_with_check(path: &str, check_xregs: &[(XRegister, u64)]) {
    let block_builder = Default::default();
    run_test_with_check::<Interpreted<M1M, Owned>>(path, check_xregs, block_builder);
}

/// For the JIT, we run it twice - the first run to build up the blocks, and the
/// second to run with these blocks already compiled (so that we actually use them).
fn inline_jit_test_with_check(path: &str, check_xregs: &[(XRegister, u64)]) {
    type BlockImpl = Jitted<JIT<M1M, Owned>, M1M, Owned>;

    let block_builder = Default::default();
    let block_builder = run_test_with_check::<BlockImpl>(path, check_xregs, block_builder);

    run_test_with_check::<BlockImpl>(path, check_xregs, block_builder);
}

macro_rules! test_case {
    // Run the test cases without specifying xregisters to check
    ($(#[$m:meta],)* $name: ident, $path: expr) => {
        test_case!($(#[$m],)* $name, $path, &[]);
    };

    // Run the test cases, check xregisters
    ($(#[$m:meta],)* $name: ident, $path: expr, $xchecks: expr) => {
        paste! {
            #[test]
            $(#[$m])*
            fn [< $name _interpreted >]() {
                interpret_test_with_check($path, $xchecks)
            }

            #[test]
            $(#[$m])*
            fn [< $name _inline_jit >]() {
                inline_jit_test_with_check($path, $xchecks)
            }
        }
    };
}

// RV64-UA
test_case!(test_suite_rv64ua_p_amoadd_d, "rv64ua-p-amoadd_d");
test_case!(test_suite_rv64ua_p_amoadd_w, "rv64ua-p-amoadd_w");
test_case!(test_suite_rv64ua_p_amoand_d, "rv64ua-p-amoand_d");
test_case!(test_suite_rv64ua_p_amoand_w, "rv64ua-p-amoand_w");
test_case!(test_suite_rv64ua_p_amomax_d, "rv64ua-p-amomax_d");
test_case!(test_suite_rv64ua_p_amomax_w, "rv64ua-p-amomax_w");
test_case!(test_suite_rv64ua_p_amomaxu_d, "rv64ua-p-amomaxu_d");
test_case!(test_suite_rv64ua_p_amomaxu_w, "rv64ua-p-amomaxu_w");
test_case!(test_suite_rv64ua_p_amomin_d, "rv64ua-p-amomin_d");
test_case!(test_suite_rv64ua_p_amomin_w, "rv64ua-p-amomin_w");
test_case!(test_suite_rv64ua_p_amominu_d, "rv64ua-p-amominu_d");
test_case!(test_suite_rv64ua_p_amominu_w, "rv64ua-p-amominu_w");
test_case!(test_suite_rv64ua_p_amoor_d, "rv64ua-p-amoor_d");
test_case!(test_suite_rv64ua_p_amoor_w, "rv64ua-p-amoor_w");
test_case!(test_suite_rv64ua_p_amoswap_d, "rv64ua-p-amoswap_d");
test_case!(test_suite_rv64ua_p_amoswap_w, "rv64ua-p-amoswap_w");
test_case!(test_suite_rv64ua_p_amoxor_d, "rv64ua-p-amoxor_d");
test_case!(test_suite_rv64ua_p_amoxor_w, "rv64ua-p-amoxor_w");
test_case!(test_suite_rv64ua_p_lrsc, "rv64ua-p-lrsc");

// RV64-UC
test_case!(test_suite_rv64uc_p_rvc, "rv64uc-p-rvc");

// RV64-UD
test_case!(test_suite_rv64ud_p_fadd, "rv64ud-p-fadd");
test_case!(test_suite_rv64ud_p_fclass, "rv64ud-p-fclass");
test_case!(test_suite_rv64ud_p_fcmp, "rv64ud-p-fcmp");
test_case!(test_suite_rv64ud_p_fcvt, "rv64ud-p-fcvt");
test_case!(test_suite_rv64ud_p_fcvt_w, "rv64ud-p-fcvt_w");
test_case!(test_suite_rv64ud_p_fdiv, "rv64ud-p-fdiv");
test_case!(test_suite_rv64ud_p_fmadd, "rv64ud-p-fmadd");
test_case!(test_suite_rv64ud_p_fmin, "rv64ud-p-fmin");
test_case!(test_suite_rv64ud_p_ldst, "rv64ud-p-ldst");
test_case!(test_suite_rv64ud_p_move, "rv64ud-p-move");
test_case!(test_suite_rv64ud_p_recoding, "rv64ud-p-recoding");
test_case!(test_suite_rv64ud_p_structural, "rv64ud-p-structural");

// RV64-UF
test_case!(test_suite_rv64uf_p_fadd, "rv64uf-p-fadd");
test_case!(test_suite_rv64uf_p_fclass, "rv64uf-p-fclass");
test_case!(test_suite_rv64uf_p_fcmp, "rv64uf-p-fcmp");
test_case!(test_suite_rv64uf_p_fcvt, "rv64uf-p-fcvt");
test_case!(test_suite_rv64uf_p_fcvt_w, "rv64uf-p-fcvt_w");
test_case!(test_suite_rv64uf_p_fdiv, "rv64uf-p-fdiv");
test_case!(test_suite_rv64uf_p_fmadd, "rv64uf-p-fmadd");
test_case!(test_suite_rv64uf_p_fmin, "rv64uf-p-fmin");
test_case!(test_suite_rv64uf_p_ldst, "rv64uf-p-ldst");
test_case!(test_suite_rv64uf_p_move, "rv64uf-p-move");
test_case!(test_suite_rv64uf_p_recoding, "rv64uf-p-recoding");

// RV64-UI
test_case!(test_suite_rv64ui_p_add, "rv64ui-p-add");
test_case!(test_suite_rv64ui_p_addi, "rv64ui-p-addi");
test_case!(test_suite_rv64ui_p_addiw, "rv64ui-p-addiw");
test_case!(test_suite_rv64ui_p_addw, "rv64ui-p-addw");
test_case!(test_suite_rv64ui_p_and, "rv64ui-p-and");
test_case!(test_suite_rv64ui_p_andi, "rv64ui-p-andi");
test_case!(test_suite_rv64ui_p_auipc, "rv64ui-p-auipc");
test_case!(test_suite_rv64ui_p_beq, "rv64ui-p-beq");
test_case!(test_suite_rv64ui_p_bge, "rv64ui-p-bge");
test_case!(test_suite_rv64ui_p_bgeu, "rv64ui-p-bgeu");
test_case!(test_suite_rv64ui_p_blt, "rv64ui-p-blt");
test_case!(test_suite_rv64ui_p_bltu, "rv64ui-p-bltu");
test_case!(test_suite_rv64ui_p_bne, "rv64ui-p-bne");
test_case!(test_suite_rv64ui_p_fence_i, "rv64ui-p-fence_i");
test_case!(test_suite_rv64ui_p_jal, "rv64ui-p-jal");
test_case!(test_suite_rv64ui_p_jalr, "rv64ui-p-jalr");
test_case!(test_suite_rv64ui_p_lb, "rv64ui-p-lb");
test_case!(test_suite_rv64ui_p_lbu, "rv64ui-p-lbu");
test_case!(test_suite_rv64ui_p_ld, "rv64ui-p-ld");
test_case!(test_suite_rv64ui_p_ld_st, "rv64ui-p-ld_st");
test_case!(test_suite_rv64ui_p_lh, "rv64ui-p-lh");
test_case!(test_suite_rv64ui_p_lhu, "rv64ui-p-lhu");
test_case!(test_suite_rv64ui_p_lui, "rv64ui-p-lui");
test_case!(test_suite_rv64ui_p_lw, "rv64ui-p-lw");
test_case!(test_suite_rv64ui_p_lwu, "rv64ui-p-lwu");
test_case!(test_suite_rv64ui_p_ma_data, "rv64ui-p-ma_data");
test_case!(test_suite_rv64ui_p_or, "rv64ui-p-or");
test_case!(test_suite_rv64ui_p_ori, "rv64ui-p-ori");
test_case!(test_suite_rv64ui_p_sb, "rv64ui-p-sb");
test_case!(test_suite_rv64ui_p_sd, "rv64ui-p-sd");
test_case!(test_suite_rv64ui_p_sh, "rv64ui-p-sh");
test_case!(test_suite_rv64ui_p_simple, "rv64ui-p-simple");
test_case!(test_suite_rv64ui_p_sll, "rv64ui-p-sll");
test_case!(test_suite_rv64ui_p_slli, "rv64ui-p-slli");
test_case!(test_suite_rv64ui_p_slliw, "rv64ui-p-slliw");
test_case!(test_suite_rv64ui_p_sllw, "rv64ui-p-sllw");
test_case!(test_suite_rv64ui_p_slt, "rv64ui-p-slt");
test_case!(test_suite_rv64ui_p_slti, "rv64ui-p-slti");
test_case!(test_suite_rv64ui_p_sltiu, "rv64ui-p-sltiu");
test_case!(test_suite_rv64ui_p_sltu, "rv64ui-p-sltu");
test_case!(test_suite_rv64ui_p_sra, "rv64ui-p-sra");
test_case!(test_suite_rv64ui_p_srai, "rv64ui-p-srai");
test_case!(test_suite_rv64ui_p_sraiw, "rv64ui-p-sraiw");
test_case!(test_suite_rv64ui_p_sraw, "rv64ui-p-sraw");
test_case!(test_suite_rv64ui_p_srl, "rv64ui-p-srl");
test_case!(test_suite_rv64ui_p_srli, "rv64ui-p-srli");
test_case!(test_suite_rv64ui_p_srliw, "rv64ui-p-srliw");
test_case!(test_suite_rv64ui_p_srlw, "rv64ui-p-srlw");
test_case!(test_suite_rv64ui_p_st_ld, "rv64ui-p-st_ld");
test_case!(test_suite_rv64ui_p_sub, "rv64ui-p-sub");
test_case!(test_suite_rv64ui_p_subw, "rv64ui-p-subw");
test_case!(test_suite_rv64ui_p_sw, "rv64ui-p-sw");
test_case!(test_suite_rv64ui_p_xor, "rv64ui-p-xor");
test_case!(test_suite_rv64ui_p_xori, "rv64ui-p-xori");

// RV64-UM
test_case!(test_suite_rv64um_p_div, "rv64um-p-div");
test_case!(test_suite_rv64um_p_divu, "rv64um-p-divu");
test_case!(test_suite_rv64um_p_divuw, "rv64um-p-divuw");
test_case!(test_suite_rv64um_p_divw, "rv64um-p-divw");
test_case!(test_suite_rv64um_p_mul, "rv64um-p-mul");
test_case!(test_suite_rv64um_p_mulh, "rv64um-p-mulh");
test_case!(test_suite_rv64um_p_mulhsu, "rv64um-p-mulhsu");
test_case!(test_suite_rv64um_p_mulhu, "rv64um-p-mulhu");
test_case!(test_suite_rv64um_p_mulw, "rv64um-p-mulw");
test_case!(test_suite_rv64um_p_rem, "rv64um-p-rem");
test_case!(test_suite_rv64um_p_remu, "rv64um-p-remu");
test_case!(test_suite_rv64um_p_remuw, "rv64um-p-remuw");
test_case!(test_suite_rv64um_p_remw, "rv64um-p-remw");
