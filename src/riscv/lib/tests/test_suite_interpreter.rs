// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use goldenfile::Mint;
use octez_riscv::{
    machine_state::{
        bus::main_memory::M1M,
        mode::Mode,
        registers::{gp, XRegister, XValue},
    },
    stepper::{
        test::{TestStepper, TestStepperResult::*},
        Stepper,
    },
};
use std::{fs, io::Write};

const TESTS_DIR: &str = "../../../tezt/tests/riscv-tests/generated";
const GOLDEN_DIR: &str = "tests/expected";
const MAX_STEPS: usize = 1_000_000;

fn check_register_values<S: Stepper>(stepper: &S, check_xregs: &[(XRegister, XValue)]) {
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

fn interpret_test_with_check(path: &str, exit_mode: Mode, check_xregs: &[(XRegister, u64)]) {
    // Create a Mint instance: when it goes out of scope (at the end of interpret_test_with_check),
    // all golden files will be compared to the checked-in versions.
    let mut mint = Mint::new(GOLDEN_DIR);
    let mut golden = mint.new_goldenfile(format!("{path}.out")).unwrap();

    let contents = fs::read(format!("{TESTS_DIR}/{path}")).expect("Failed to read binary");
    let mut backend = TestStepper::<'_, M1M>::create_backend();
    let mut interpreter =
        TestStepper::new(&mut backend, &contents, None, exit_mode).expect("Boot failed");

    let res = interpreter.step_max(MAX_STEPS);
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
    }
}

macro_rules! test_case {
    // Default start & exit mode (Machine & Machine)
    ($(#[$m:meta],)* $name: ident, $path: expr) => {
        test_case!(
            $(#[$m],)*
            $name,
            $path,
            if $path.contains("rv64u") {
                Mode::User
            } else if $path.contains("rv64s") {
                Mode::Supervisor
            } else {
                Mode::Machine
            }
        );
    };

    // Choose exit mode, default start mode (Based on test name)
    ($(#[$m:meta],)* $name: ident, $path: expr, $mode: expr) => {
        test_case!($(#[$m],)* $name, $path, $mode, &[]);
    };

    // Choose exit mode, default start mode (based on test name), check xregisters
    ($(#[$m:meta],)* $name: ident, $path: expr, $mode: expr, $xchecks: expr) => {
        #[test]
        $(#[$m])*
        fn $name() {
            interpret_test_with_check($path, $mode, $xchecks)
        }
    };
}
// RV64-MI
test_case!(test_suite_rv64mi_p_access, "rv64mi-p-access");
test_case!(#[ignore], test_suite_rv64mi_p_breakpoint, "rv64mi-p-breakpoint");
test_case!(test_suite_rv64mi_p_csr, "rv64mi-p-csr", Mode::User);
test_case!(
    test_suite_rv64mi_p_illegal,
    "rv64mi-p-illegal",
    Mode::Supervisor
);
test_case!(test_suite_rv64mi_p_ld_misaligned, "rv64mi-p-ld-misaligned");
test_case!(test_suite_rv64mi_p_lh_misaligned, "rv64mi-p-lh-misaligned");
test_case!(test_suite_rv64mi_p_lw_misaligned, "rv64mi-p-lw-misaligned");
test_case!(test_suite_rv64mi_p_ma_addr, "rv64mi-p-ma_addr");
test_case!(test_suite_rv64mi_p_ma_fetch, "rv64mi-p-ma_fetch");
test_case!(test_suite_rv64mi_p_mcsr, "rv64mi-p-mcsr");
test_case!(test_suite_rv64mi_p_sbreak, "rv64mi-p-sbreak");
test_case!(
    test_suite_rv64mi_p_scall,
    "rv64mi-p-scall",
    Mode::User,
    &[(gp, 1)] // This checks TESTNUM == 1, see scall.S in the riscv test suite
);
test_case!(test_suite_rv64mi_p_sd_misaligned, "rv64mi-p-sd-misaligned");
test_case!(test_suite_rv64mi_p_sh_misaligned, "rv64mi-p-sh-misaligned");
test_case!(test_suite_rv64mi_p_sw_misaligned, "rv64mi-p-sw-misaligned");
test_case!(test_suite_rv64mi_p_zicntr, "rv64mi-p-zicntr");

// RV64-MZICBO
test_case!(#[ignore], test_suite_rv64mzicbo_p_zero, "rv64mzicbo-p-zero");

// RV64-SI
test_case!(test_suite_rv64si_p_csr, "rv64si-p-csr");
test_case!(test_suite_rv64si_p_dirty, "rv64si-p-dirty", Mode::Machine);
test_case!(
    test_suite_rv64si_p_icache_alias,
    "rv64si-p-icache-alias",
    Mode::Machine
);
test_case!(test_suite_rv64si_p_ma_fetch, "rv64si-p-ma_fetch");
test_case!(test_suite_rv64si_p_sbreak, "rv64si-p-sbreak");
test_case!(
    test_suite_rv64si_p_scall,
    "rv64si-p-scall",
    Mode::Supervisor,
    &[(gp, 1)] // This checks TESTNUM == 1, see scall.S in the riscv test suite
);
test_case!(test_suite_rv64si_p_wfi, "rv64si-p-wfi");

// RV64-SSVNAPOT
test_case!(#[ignore], test_suite_rv64ssvnapot_p_napot, "rv64ssvnapot-p-napot");

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

test_case!(test_suite_rv64ua_v_amoadd_d, "rv64ua-v-amoadd_d");
test_case!(test_suite_rv64ua_v_amoadd_w, "rv64ua-v-amoadd_w");
test_case!(test_suite_rv64ua_v_amoand_d, "rv64ua-v-amoand_d");
test_case!(test_suite_rv64ua_v_amoand_w, "rv64ua-v-amoand_w");
test_case!(test_suite_rv64ua_v_amomax_d, "rv64ua-v-amomax_d");
test_case!(test_suite_rv64ua_v_amomax_w, "rv64ua-v-amomax_w");
test_case!(test_suite_rv64ua_v_amomaxu_d, "rv64ua-v-amomaxu_d");
test_case!(test_suite_rv64ua_v_amomaxu_w, "rv64ua-v-amomaxu_w");
test_case!(test_suite_rv64ua_v_amomin_d, "rv64ua-v-amomin_d");
test_case!(test_suite_rv64ua_v_amomin_w, "rv64ua-v-amomin_w");
test_case!(test_suite_rv64ua_v_amominu_d, "rv64ua-v-amominu_d");
test_case!(test_suite_rv64ua_v_amominu_w, "rv64ua-v-amominu_w");
test_case!(test_suite_rv64ua_v_amoor_d, "rv64ua-v-amoor_d");
test_case!(test_suite_rv64ua_v_amoor_w, "rv64ua-v-amoor_w");
test_case!(test_suite_rv64ua_v_amoswap_d, "rv64ua-v-amoswap_d");
test_case!(test_suite_rv64ua_v_amoswap_w, "rv64ua-v-amoswap_w");
test_case!(test_suite_rv64ua_v_amoxor_d, "rv64ua-v-amoxor_d");
test_case!(test_suite_rv64ua_v_amoxor_w, "rv64ua-v-amoxor_w");
test_case!(test_suite_rv64ua_v_lrsc, "rv64ua-v-lrsc");

// RV64-UC
test_case!(test_suite_rv64uc_p_rvc, "rv64uc-p-rvc");

test_case!(test_suite_rv64uc_v_rvc, "rv64uc-v-rvc");

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

test_case!(test_suite_rv64ud_v_fadd, "rv64ud-v-fadd");
test_case!(test_suite_rv64ud_v_fclass, "rv64ud-v-fclass");
test_case!(test_suite_rv64ud_v_fcmp, "rv64ud-v-fcmp");
test_case!(test_suite_rv64ud_v_fcvt, "rv64ud-v-fcvt");
test_case!(test_suite_rv64ud_v_fcvt_w, "rv64ud-v-fcvt_w");
test_case!(test_suite_rv64ud_v_fdiv, "rv64ud-v-fdiv");
test_case!(test_suite_rv64ud_v_fmadd, "rv64ud-v-fmadd");
test_case!(test_suite_rv64ud_v_fmin, "rv64ud-v-fmin");
test_case!(test_suite_rv64ud_v_ldst, "rv64ud-v-ldst");
test_case!(test_suite_rv64ud_v_move, "rv64ud-v-move");
test_case!(test_suite_rv64ud_v_recoding, "rv64ud-v-recoding");
test_case!(test_suite_rv64ud_v_structural, "rv64ud-v-structural");

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

test_case!(test_suite_rv64uf_v_fadd, "rv64uf-v-fadd");
test_case!(test_suite_rv64uf_v_fclass, "rv64uf-v-fclass");
test_case!(test_suite_rv64uf_v_fcmp, "rv64uf-v-fcmp");
test_case!(test_suite_rv64uf_v_fcvt, "rv64uf-v-fcvt");
test_case!(test_suite_rv64uf_v_fcvt_w, "rv64uf-v-fcvt_w");
test_case!(test_suite_rv64uf_v_fdiv, "rv64uf-v-fdiv");
test_case!(test_suite_rv64uf_v_fmadd, "rv64uf-v-fmadd");
test_case!(test_suite_rv64uf_v_fmin, "rv64uf-v-fmin");
test_case!(test_suite_rv64uf_v_ldst, "rv64uf-v-ldst");
test_case!(test_suite_rv64uf_v_move, "rv64uf-v-move");
test_case!(test_suite_rv64uf_v_recoding, "rv64uf-v-recoding");

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
test_case!(test_suite_rv64ui_p_sub, "rv64ui-p-sub");
test_case!(test_suite_rv64ui_p_subw, "rv64ui-p-subw");
test_case!(test_suite_rv64ui_p_sw, "rv64ui-p-sw");
test_case!(test_suite_rv64ui_p_xor, "rv64ui-p-xor");
test_case!(test_suite_rv64ui_p_xori, "rv64ui-p-xori");

test_case!(test_suite_rv64ui_v_add, "rv64ui-v-add");
test_case!(test_suite_rv64ui_v_addi, "rv64ui-v-addi");
test_case!(test_suite_rv64ui_v_addiw, "rv64ui-v-addiw");
test_case!(test_suite_rv64ui_v_addw, "rv64ui-v-addw");
test_case!(test_suite_rv64ui_v_and, "rv64ui-v-and");
test_case!(test_suite_rv64ui_v_andi, "rv64ui-v-andi");
test_case!(test_suite_rv64ui_v_auipc, "rv64ui-v-auipc");
test_case!(test_suite_rv64ui_v_beq, "rv64ui-v-beq");
test_case!(test_suite_rv64ui_v_bge, "rv64ui-v-bge");
test_case!(test_suite_rv64ui_v_bgeu, "rv64ui-v-bgeu");
test_case!(test_suite_rv64ui_v_blt, "rv64ui-v-blt");
test_case!(test_suite_rv64ui_v_bltu, "rv64ui-v-bltu");
test_case!(test_suite_rv64ui_v_bne, "rv64ui-v-bne");
test_case!(test_suite_rv64ui_v_fence_i, "rv64ui-v-fence_i");
test_case!(test_suite_rv64ui_v_jal, "rv64ui-v-jal");
test_case!(test_suite_rv64ui_v_jalr, "rv64ui-v-jalr");
test_case!(test_suite_rv64ui_v_lb, "rv64ui-v-lb");
test_case!(test_suite_rv64ui_v_lbu, "rv64ui-v-lbu");
test_case!(test_suite_rv64ui_v_ld, "rv64ui-v-ld");
test_case!(test_suite_rv64ui_v_lh, "rv64ui-v-lh");
test_case!(test_suite_rv64ui_v_lhu, "rv64ui-v-lhu");
test_case!(test_suite_rv64ui_v_lui, "rv64ui-v-lui");
test_case!(test_suite_rv64ui_v_lw, "rv64ui-v-lw");
test_case!(test_suite_rv64ui_v_lwu, "rv64ui-v-lwu");
test_case!(test_suite_rv64ui_v_ma_data, "rv64ui-v-ma_data");
test_case!(test_suite_rv64ui_v_or, "rv64ui-v-or");
test_case!(test_suite_rv64ui_v_ori, "rv64ui-v-ori");
test_case!(test_suite_rv64ui_v_sb, "rv64ui-v-sb");
test_case!(test_suite_rv64ui_v_sd, "rv64ui-v-sd");
test_case!(test_suite_rv64ui_v_sh, "rv64ui-v-sh");
test_case!(test_suite_rv64ui_v_simple, "rv64ui-v-simple");
test_case!(test_suite_rv64ui_v_sll, "rv64ui-v-sll");
test_case!(test_suite_rv64ui_v_slli, "rv64ui-v-slli");
test_case!(test_suite_rv64ui_v_slliw, "rv64ui-v-slliw");
test_case!(test_suite_rv64ui_v_sllw, "rv64ui-v-sllw");
test_case!(test_suite_rv64ui_v_slt, "rv64ui-v-slt");
test_case!(test_suite_rv64ui_v_slti, "rv64ui-v-slti");
test_case!(test_suite_rv64ui_v_sltiu, "rv64ui-v-sltiu");
test_case!(test_suite_rv64ui_v_sltu, "rv64ui-v-sltu");
test_case!(test_suite_rv64ui_v_sra, "rv64ui-v-sra");
test_case!(test_suite_rv64ui_v_srai, "rv64ui-v-srai");
test_case!(test_suite_rv64ui_v_sraiw, "rv64ui-v-sraiw");
test_case!(test_suite_rv64ui_v_sraw, "rv64ui-v-sraw");
test_case!(test_suite_rv64ui_v_srl, "rv64ui-v-srl");
test_case!(test_suite_rv64ui_v_srli, "rv64ui-v-srli");
test_case!(test_suite_rv64ui_v_srliw, "rv64ui-v-srliw");
test_case!(test_suite_rv64ui_v_srlw, "rv64ui-v-srlw");
test_case!(test_suite_rv64ui_v_sub, "rv64ui-v-sub");
test_case!(test_suite_rv64ui_v_subw, "rv64ui-v-subw");
test_case!(test_suite_rv64ui_v_sw, "rv64ui-v-sw");
test_case!(test_suite_rv64ui_v_xor, "rv64ui-v-xor");
test_case!(test_suite_rv64ui_v_xori, "rv64ui-v-xori");

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

test_case!(test_suite_rv64um_v_div, "rv64um-v-div");
test_case!(test_suite_rv64um_v_divu, "rv64um-v-divu");
test_case!(test_suite_rv64um_v_divuw, "rv64um-v-divuw");
test_case!(test_suite_rv64um_v_divw, "rv64um-v-divw");
test_case!(test_suite_rv64um_v_mul, "rv64um-v-mul");
test_case!(test_suite_rv64um_v_mulh, "rv64um-v-mulh");
test_case!(test_suite_rv64um_v_mulhsu, "rv64um-v-mulhsu");
test_case!(test_suite_rv64um_v_mulhu, "rv64um-v-mulhu");
test_case!(test_suite_rv64um_v_mulw, "rv64um-v-mulw");
test_case!(test_suite_rv64um_v_rem, "rv64um-v-rem");
test_case!(test_suite_rv64um_v_remu, "rv64um-v-remu");
test_case!(test_suite_rv64um_v_remuw, "rv64um-v-remuw");
test_case!(test_suite_rv64um_v_remw, "rv64um-v-remw");

// RV64-UZFH
test_case!(#[ignore], test_suite_rv64uzfh_p_fadd, "rv64uzfh-p-fadd");
test_case!(#[ignore], test_suite_rv64uzfh_p_fclass, "rv64uzfh-p-fclass");
test_case!(#[ignore], test_suite_rv64uzfh_p_fcmp, "rv64uzfh-p-fcmp");
test_case!(#[ignore], test_suite_rv64uzfh_p_fcvt, "rv64uzfh-p-fcvt");
test_case!(#[ignore], test_suite_rv64uzfh_p_fcvt_w, "rv64uzfh-p-fcvt_w");
test_case!(#[ignore], test_suite_rv64uzfh_p_fdiv, "rv64uzfh-p-fdiv");
test_case!(#[ignore], test_suite_rv64uzfh_p_fmadd, "rv64uzfh-p-fmadd");
test_case!(#[ignore], test_suite_rv64uzfh_p_fmin, "rv64uzfh-p-fmin");
test_case!(#[ignore], test_suite_rv64uzfh_p_ldst, "rv64uzfh-p-ldst");
test_case!(#[ignore], test_suite_rv64uzfh_p_move, "rv64uzfh-p-move");
test_case!(#[ignore], test_suite_rv64uzfh_p_recoding, "rv64uzfh-p-recoding");

test_case!(#[ignore], test_suite_rv64uzfh_v_fadd, "rv64uzfh-v-fadd");
test_case!(#[ignore], test_suite_rv64uzfh_v_fclass, "rv64uzfh-v-fclass");
test_case!(#[ignore], test_suite_rv64uzfh_v_fcmp, "rv64uzfh-v-fcmp");
test_case!(#[ignore], test_suite_rv64uzfh_v_fcvt, "rv64uzfh-v-fcvt");
test_case!(#[ignore], test_suite_rv64uzfh_v_fcvt_w, "rv64uzfh-v-fcvt_w");
test_case!(#[ignore], test_suite_rv64uzfh_v_fdiv, "rv64uzfh-v-fdiv");
test_case!(#[ignore], test_suite_rv64uzfh_v_fmadd, "rv64uzfh-v-fmadd");
test_case!(#[ignore], test_suite_rv64uzfh_v_fmin, "rv64uzfh-v-fmin");
test_case!(#[ignore], test_suite_rv64uzfh_v_ldst, "rv64uzfh-v-ldst");
test_case!(#[ignore], test_suite_rv64uzfh_v_move, "rv64uzfh-v-move");
test_case!(#[ignore], test_suite_rv64uzfh_v_recoding, "rv64uzfh-v-recoding");
