use risc_v_interpreter::add;

#[ocaml::func]
#[ocaml::sig("int32 -> int32 -> int32")]
pub fn octez_risc_v_add(left: u32, right: u32) -> u32 {
    add(left as usize, right as usize) as u32
}
