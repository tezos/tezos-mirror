// Fr

//Provides: Blst_fr_val
function Blst_fr_val(v) {
  return v.v;
}

//Provides: Blst_scalar_val
function Blst_scalar_val(v) {
  return v.v;
}

//Provides: blst_scalar_sizeof
//Requires: wasm_call
function blst_scalar_sizeof() {
  return wasm_call('_blst_scalar_sizeof');
}

//Provides: blst_fr_sizeof
//Requires: wasm_call
function blst_fr_sizeof() {
  return wasm_call('_blst_fr_sizeof');
}

//Provides: Blst_scalar
//Requires: blst_scalar_sizeof
function Blst_scalar() {
  this.v = new globalThis.Uint8Array(blst_scalar_sizeof());
}

//Provides: allocate_scalar_stubs
//Requires: Blst_scalar
function allocate_scalar_stubs(unit) {
  return new Blst_scalar();
}

//Provides: caml_blst_fr_from_lendian_stubs
//Requires: wasm_call
//Requires: Blst_fr_val
function caml_blst_fr_from_lendian_stubs(x, b) {
  var x_c = Blst_fr_val(x);
  var b_c = b;
  var res /* bool */ = wasm_call('_blst_fr_from_lendian', x_c, b_c);
  return res ? 1 : 0;
}

//Provides: caml_blst_lendian_from_fr_stubs
//Requires: wasm_call
//Requires: Blst_fr_val
function caml_blst_lendian_from_fr_stubs(b, x) {
  var x_c = Blst_fr_val(x);
  var b_c = b;
  wasm_call('_blst_lendian_from_fr', b_c, x_c);
  return 0;
}

//Provides: caml_blst_fr_pow_stubs
//Requires: wasm_call
//Requires: Blst_fr_val
function caml_blst_fr_pow_stubs(out, x, exp, exp_nb_bits) {
  var out_c = Blst_fr_val(out);
  var x_c = Blst_fr_val(x);
  var exp_c = exp;
  wasm_call('_blst_fr_pow', out_c, x_c, exp_c, exp_nb_bits);
  return 0;
}

//Provides: caml_blst_fr_is_equal_stubs
//Requires: wasm_call
//Requires: Blst_fr_val
function caml_blst_fr_is_equal_stubs(x, y) {
  var x_c = Blst_fr_val(x);
  var y_c = Blst_fr_val(y);
  return wasm_call('_blst_fr_is_equal', x_c, y_c) ? 1 : 0;
}
//Provides: caml_blst_fr_is_zero_stubs
//Requires: wasm_call
//Requires: Blst_fr_val
function caml_blst_fr_is_zero_stubs(x) {
  var x_c = Blst_fr_val(x);
  return wasm_call('_blst_fr_is_zero', x_c) ? 1 : 0;
}

//Provides: caml_blst_fr_is_one_stubs
//Requires: wasm_call
//Requires: Blst_fr_val
function caml_blst_fr_is_one_stubs(x) {
  var x_c = Blst_fr_val(x);
  return wasm_call('_blst_fr_is_one', x_c) ? 1 : 0;
}

//Provides: Blst_fr
//Requires: blst_fr_sizeof
//Requires: wasm_call
function Blst_fr() {
  this.v = new globalThis.Uint8Array(blst_fr_sizeof());
}
Blst_fr.prototype.compare = function(t) {
  return wasm_call('_blst_fr_compare', this.v, t.v);
};
//Provides: callocate_fr_stubs
//Requires: Blst_fr
function callocate_fr_stubs(unit) {
  return new Blst_fr();
}

//Provides: mallocate_fr_stubs
//Requires: Blst_fr
function mallocate_fr_stubs(unit) {
  return new Blst_fr();
}

//Provides:caml_blst_fr_add_stubs
//Requires: wasm_call
//Requires: Blst_fr_val
function caml_blst_fr_add_stubs(ret, p1, p2) {
  wasm_call('_blst_fr_add', Blst_fr_val(ret), Blst_fr_val(p1), Blst_fr_val(p2));
  return 0;
}

//Provides: caml_blst_fr_mul_stubs
//Requires: wasm_call
//Requires: Blst_fr_val
function caml_blst_fr_mul_stubs(ret, p1, p2) {
  wasm_call('_blst_fr_mul', Blst_fr_val(ret), Blst_fr_val(p1), Blst_fr_val(p2));
  return 0;
}

//Provides: caml_blst_fr_cneg_stubs
//Requires: wasm_call
//Requires: Blst_fr_val
function caml_blst_fr_cneg_stubs(ret, p1, flag) {
  wasm_call('_blst_fr_cneg', Blst_fr_val(ret), Blst_fr_val(p1), flag);
  return 0;
}

//Provides: caml_blst_fr_sqr_stubs
//Requires: wasm_call
//Requires: Blst_fr_val
function caml_blst_fr_sqr_stubs(ret, p1) {
  wasm_call('_blst_fr_sqr', Blst_fr_val(ret), Blst_fr_val(p1));
  return 0;
}

//Provides: caml_blst_fr_eucl_inverse_stubs
//Requires: wasm_call
//Requires: Blst_fr_val
function caml_blst_fr_eucl_inverse_stubs(ret, p1) {
  wasm_call('_blst_fr_eucl_inverse', Blst_fr_val(ret), Blst_fr_val(p1));
  return 0;
}

//Provides: caml_blst_fr_sub_stubs
//Requires: wasm_call
//Requires: Blst_fr_val
function caml_blst_fr_sub_stubs(ret, p1, p2) {
  wasm_call('_blst_fr_sub', Blst_fr_val(ret), Blst_fr_val(p1), Blst_fr_val(p2));
  return 0;
}

//Provides: caml_blst_scalar_from_fr_stubs
//Requires: wasm_call
//Requires: Blst_fr_val, Blst_scalar_val
function caml_blst_scalar_from_fr_stubs(ret, p1) {
  wasm_call('_blst_scalar_from_fr', Blst_scalar_val(ret), Blst_fr_val(p1));
  return 0;
}

//Provides: caml_blst_fr_from_scalar_stubs
//Requires: wasm_call
//Requires: Blst_fr_val, Blst_scalar_val
function caml_blst_fr_from_scalar_stubs(ret, p1) {
  wasm_call('_blst_fr_from_scalar', Blst_fr_val(ret), Blst_scalar_val(p1));
  return 0;
}

//Provides: caml_blst_scalar_to_bytes_stubs
//Requires: wasm_call
//Requires: Blst_scalar_val
function caml_blst_scalar_to_bytes_stubs(ret, p1) {
  wasm_call('_blst_lendian_from_scalar', ret, Blst_scalar_val(p1));
  return 0;
}

//Provides: caml_blst_scalar_of_bytes_stubs
//Requires: wasm_call
//Requires: Blst_scalar_val
function caml_blst_scalar_of_bytes_stubs(ret, p1) {
  wasm_call('_blst_scalar_from_lendian', Blst_scalar_val(ret), p1);
  return 0;
}

//Provides: caml_blst_check_scalar_stubs
//Requires: wasm_call
//Requires: Blst_scalar_val
function caml_blst_check_scalar_stubs(p1) {
  var r = wasm_call('_blst_scalar_fr_check', Blst_scalar_val(p1));
  return r ? 1 : 0;
}

//Provides: caml_blst_fr_memcpy_stubs
//Requires: caml_blst_memcpy, blst_fr_sizeof
//Requires: Blst_fr_val
function caml_blst_fr_memcpy_stubs(dst, src) {
  caml_blst_memcpy(Blst_fr_val(dst), Blst_fr_val(src), blst_fr_sizeof());
  return 0;
}

// Fq

//Provides: Blst_fp_val
function Blst_fp_val(v) {
  return v.v;
}

//Provides: blst_fp_sizeof
//Requires: wasm_call
function blst_fp_sizeof() {
  return wasm_call('_blst_fp_sizeof');
}

//Provides: Blst_fp
//Requires: blst_fp_sizeof
function Blst_fp() {
  this.v = new globalThis.Uint8Array(blst_fp_sizeof());
}

//Provides: allocate_fp_stubs
//Requires: Blst_fp
function allocate_fp_stubs(unit) {
  return new Blst_fp();
}

//Provides: caml_blst_fp_of_bytes_stubs
//Requires: wasm_call
//Requires: Blst_fp_val
function caml_blst_fp_of_bytes_stubs(ret, p1) {
  wasm_call('_blst_fp_from_lendian', Blst_fp_val(ret), p1);
  return 0;
}

//Provides: caml_blst_fp_to_bytes_stubs
//Requires: wasm_call
//Requires: Blst_fp_val
function caml_blst_fp_to_bytes_stubs(ret, p1) {
  wasm_call('_blst_lendian_from_fp', ret, Blst_fp_val(p1));
  return 0;
}

//Provides: caml_blst_fp_add_stubs
//Requires: wasm_call
//Requires: Blst_fp_val
function caml_blst_fp_add_stubs(ret, p1, p2) {
  wasm_call('_blst_fp_add', Blst_fp_val(ret), Blst_fp_val(p1), Blst_fp_val(p2));
  return 0;
}

//Provides: caml_blst_fp_mul_stubs
//Requires: wasm_call
//Requires: Blst_fp_val
function caml_blst_fp_mul_stubs(ret, p1, p2) {
  wasm_call('_blst_fp_mul', Blst_fp_val(ret), Blst_fp_val(p1), Blst_fp_val(p2));
  return 0;
}

//Provides: caml_blst_fp_sqrt_stubs
//Requires: wasm_call
//Requires: Blst_fp_val
function caml_blst_fp_sqrt_stubs(ret, p1) {
  var r = wasm_call('_blst_fp_sqrt', Blst_fp_val(ret), Blst_fp_val(p1));
  return r ? 1 : 0;
}

//Provides: caml_blst_fp_cneg_stubs
//Requires: wasm_call
//Requires: Blst_fp_val
function caml_blst_fp_cneg_stubs(buffer, p, b) {
  wasm_call('_blst_fp_cneg', Blst_fp_val(buffer), Blst_fp_val(p), b);
  return 0;
}

// Fq2

//Provides: Blst_fp2_val
function Blst_fp2_val(v) {
  return v.v;
}

//Provides: blst_fp2_sizeof
//Requires: wasm_call
function blst_fp2_sizeof() {
  return wasm_call('_blst_fp2_sizeof');
}

//Provides: Blst_fp2
//Requires: blst_fp2_sizeof
function Blst_fp2() {
  this.v = new globalThis.Uint8Array(blst_fp2_sizeof());
}

//Provides: allocate_fp2_stubs
//Requires: Blst_fp2
function allocate_fp2_stubs(unit) {
  return new Blst_fp2();
}

//Provides: caml_blst_fp2_add_stubs
//Requires: wasm_call
//Requires: Blst_fp2_val
function caml_blst_fp2_add_stubs(buffer, p, q) {
  wasm_call(
      '_blst_fp2_add',
      Blst_fp2_val(buffer),
      Blst_fp2_val(p),
      Blst_fp2_val(q)
  );
  return 0;
}

//Provides: caml_blst_fp2_mul_stubs
//Requires: wasm_call
//Requires: Blst_fp2_val
function caml_blst_fp2_mul_stubs(buffer, p, q) {
  wasm_call(
      '_blst_fp2_mul',
      Blst_fp2_val(buffer),
      Blst_fp2_val(p),
      Blst_fp2_val(q)
  );
  return 0;
}

//Provides: caml_blst_fp2_sqrt_stubs
//Requires: wasm_call
//Requires: Blst_fp2_val
function caml_blst_fp2_sqrt_stubs(buffer, p) {
  var r = wasm_call('_blst_fp2_sqrt', Blst_fp2_val(buffer), Blst_fp2_val(p));
  return r ? 1 : 0;
}

//Provides: caml_blst_fp2_cneg_stubs
//Requires: wasm_call
//Requires: Blst_fp2_val
function caml_blst_fp2_cneg_stubs(buffer, p, b) {
  wasm_call(
      '_blst_fp2_cneg',
      Blst_fp2_val(buffer),
      Blst_fp2_val(p),
      b
  );
  return 0;
}

//Provides: caml_blst_fp2_assign_stubs
//Requires: wasm_call
//Requires: Blst_fp2_val
//Requires: Blst_fp_val
function caml_blst_fp2_assign_stubs(p, x1, x2) {
  var p_c = Blst_fp2_val(p);
  var x1_c = Blst_fp_val(x1);
  var x2_c = Blst_fp_val(x2);
  wasm_call('_blst_fp2_assign', p_c, x1_c, x2_c);
  return 0;
}

//Provides: caml_blst_fp2_zero_stubs
//Requires: wasm_call
//Requires: Blst_fp2_val
function caml_blst_fp2_zero_stubs(buffer) {
  var buffer_c = Blst_fp2_val(buffer);
  wasm_call('_blst_fp2_zero', buffer_c);
  return 0;
}

//Provides: caml_blst_fp2_one_stubs
//Requires: wasm_call
//Requires: Blst_fp2_val
function caml_blst_fp2_one_stubs(buffer) {
  var buffer_c = Blst_fp2_val(buffer);
  wasm_call('_blst_fp2_set_to_one', buffer_c);
  return 0;
}

//Provides: caml_blst_fp2_of_bytes_components_stubs
//Requires: wasm_call
//Requires: Blst_fp2_val
function caml_blst_fp2_of_bytes_components_stubs(buffer, x1, x2) {
  var buffer_c = Blst_fp2_val(buffer);
  wasm_call(
      '_blst_fp2_of_bytes_components',
      buffer_c,
      x1,
      x2
  );
  return 0;
}

//Provides: caml_blst_fp2_to_bytes_stubs
//Requires: wasm_call
//Requires: Blst_fp2_val
function caml_blst_fp2_to_bytes_stubs(buffer, p) {
  var p_c = Blst_fp2_val(p);
  wasm_call('_blst_fp2_to_bytes', buffer, p_c);
  return 0;
}

// Fq12

//Provides: Blst_fp12_val
function Blst_fp12_val(v) {
  return v.v;
}

//Provides: blst_fp12_sizeof
//Requires: wasm_call
function blst_fp12_sizeof() {
  return wasm_call('_blst_fp12_sizeof');
}

//Provides: Blst_fp12
//Requires: blst_fp12_sizeof
function Blst_fp12() {
  this.v = new globalThis.Uint8Array(blst_fp12_sizeof());
}

//Provides: allocate_fp12_stubs
//Requires: Blst_fp12
function allocate_fp12_stubs(unit) {
  return new Blst_fp12();
}

//Provides: caml_blst_fp12_mul_stubs
//Requires: wasm_call
//Requires: Blst_fp12_val
function caml_blst_fp12_mul_stubs(buffer, p, q) {
  wasm_call(
      '_blst_fp12_mul',
      Blst_fp12_val(buffer),
      Blst_fp12_val(p),
      Blst_fp12_val(q)
  );
  return 0;
}

//Provides: caml_blst_fp12_is_equal_stubs
//Requires: wasm_call
//Requires: Blst_fp12_val
function caml_blst_fp12_is_equal_stubs(p, q) {
  var b = wasm_call('_blst_fp12_is_equal', Blst_fp12_val(p), Blst_fp12_val(q));
  return b ? 1 : 0;
}

//Provides: caml_blst_fp12_is_zero_stubs
//Requires: wasm_call
//Requires: Blst_fp12_val
function caml_blst_fp12_is_zero_stubs(p) {
  var b = wasm_call('_blst_fp12_is_zero', Blst_fp12_val(p));
  return b ? 1 : 0;
}

//Provides: caml_blst_fp12_is_one_stubs
//Requires: wasm_call
//Requires: Blst_fp12_val
function caml_blst_fp12_is_one_stubs(p) {
  var b = wasm_call('_blst_fp12_is_one', Blst_fp12_val(p));
  return b ? 1 : 0;
}

//Provides: caml_blst_fp12_inverse_stubs
//Requires: wasm_call
//Requires: Blst_fp12_val
function caml_blst_fp12_inverse_stubs(buffer, p) {
  wasm_call('_blst_fp12_inverse', Blst_fp12_val(buffer), Blst_fp12_val(p));
  return 0;
}

//Provides: caml_blst_fp12_sqr_stubs
//Requires: wasm_call
//Requires: Blst_fp12_val
function caml_blst_fp12_sqr_stubs(buffer, p) {
  wasm_call('_blst_fp12_sqr', Blst_fp12_val(buffer), Blst_fp12_val(p));
  return 0;
}

//Provides: caml_blst_fp12_one_stubs
//Requires: wasm_call
//Requires: Blst_fp12_val
function caml_blst_fp12_one_stubs(buffer) {
  var buffer_c = Blst_fp12_val(buffer);
  wasm_call('_blst_fp12_set_to_one', buffer_c);
  return 0;
}

//Provides: caml_blst_fp12_to_bytes_stubs
//Requires: wasm_call
//Requires: Blst_fp12_val
function caml_blst_fp12_to_bytes_stubs(buffer, p) {
  var p_c = Blst_fp12_val(p);
  var buffer_c = buffer;
  wasm_call('_blst_fp12_to_bytes', buffer_c, p_c);
  return 0;
}

//Provides: caml_blst_fp12_of_bytes_stubs
//Requires: wasm_call
//Requires: Blst_fp12_val
function caml_blst_fp12_of_bytes_stubs(buffer, p) {
  var buffer_c = Blst_fp12_val(buffer);
  var p_c = p;
  wasm_call('_blst_fp12_of_bytes', buffer_c, p_c);
  return 0;
}

//Provides: caml_blst_fp12_pow_stubs
//Requires: wasm_call
//Requires: Blst_fp12_val
function caml_blst_fp12_pow_stubs(out, x, exp, exp_nb_bits) {
  var out_c = Blst_fp12_val(out);
  var x_c = Blst_fp12_val(x);
  var exp_c = exp;
  var r = wasm_call('_blst_fp12_pow', out_c, x_c, exp_c, exp_nb_bits);
  return r ? 2 : 0;
}

//Provides: caml_blst_fp12_in_group_stubs
//Requires: wasm_call
//Requires: Blst_fp12_val
function caml_blst_fp12_in_group_stubs(x) {
  var x_c = Blst_fp12_val(x);
  var r = wasm_call('_blst_fp12_in_group', x_c);
  return r ? 1 : 0;
}

// P1
//Provides: Blst_p1_val
function Blst_p1_val(v) {
  return v.v;
}

//Provides: Blst_p1_affine_val
function Blst_p1_affine_val(v) {
  return v.v;
}

//Provides: blst_p1_sizeof
//Requires: wasm_call
function blst_p1_sizeof() {
  return wasm_call('_blst_p1_sizeof');
}

//Provides: Blst_p1
//Requires: blst_p1_sizeof
function Blst_p1() {
  this.v = new globalThis.Uint8Array(blst_p1_sizeof());
}

//Provides: allocate_p1_stubs
//Requires: Blst_p1
function allocate_p1_stubs(unit) {
  return new Blst_p1();
}

//Provides: blst_p1_affine_sizeof
//Requires: wasm_call
function blst_p1_affine_sizeof() {
  return wasm_call('_blst_p1_affine_sizeof');
}

//Provides: Blst_p1_affine
//Requires: blst_p1_affine_sizeof
function Blst_p1_affine() {
  this.v = new globalThis.Uint8Array(blst_p1_affine_sizeof());
}

//Provides: allocate_p1_affine_stubs
//Requires: Blst_p1_affine
function allocate_p1_affine_stubs(unit) {
  return new Blst_p1_affine();
}

//Provides: caml_blst_p1_to_affine_stubs
//Requires: wasm_call
//Requires: Blst_p1_val, Blst_p1_affine_val
function caml_blst_p1_to_affine_stubs(buffer, p) {
  wasm_call('_blst_p1_to_affine', Blst_p1_affine_val(buffer), Blst_p1_val(p));
  return 0;
}

//Provides: caml_blst_p1_from_affine_stubs
//Requires: wasm_call
//Requires: Blst_p1_val, Blst_p1_affine_val
function caml_blst_p1_from_affine_stubs(buffer, p) {
  wasm_call('_blst_p1_from_affine', Blst_p1_val(buffer), Blst_p1_affine_val(p));
  return 0;
}

//Provides: caml_blst_p1_double_stubs
//Requires: wasm_call
//Requires: Blst_p1_val
function caml_blst_p1_double_stubs(buffer, p) {
  wasm_call('_blst_p1_double', Blst_p1_val(buffer), Blst_p1_val(p));
  return 0;
}

//Provides: caml_blst_p1_add_or_double_stubs
//Requires: wasm_call
//Requires: Blst_p1_val
function caml_blst_p1_add_or_double_stubs(buffer, p, q) {
  wasm_call(
      '_blst_p1_add_or_double',
      Blst_p1_val(buffer),
      Blst_p1_val(p),
      Blst_p1_val(q)
  );
  return 0;
}

//Provides: caml_blst_p1_is_inf_stubs
//Requires: wasm_call
//Requires: Blst_p1_val
function caml_blst_p1_is_inf_stubs(p) {
  var r = wasm_call('_blst_p1_is_inf', Blst_p1_val(p));
  return r ? 1 : 0;
}

//Provides: caml_blst_p1_in_g1_stubs
//Requires: wasm_call
//Requires: Blst_p1_val
function caml_blst_p1_in_g1_stubs(p) {
  var r = wasm_call('_blst_p1_in_g1', Blst_p1_val(p));
  return r ? 1 : 0;
}

//Provides: caml_blst_p1_equal_stubs
//Requires: wasm_call
//Requires: Blst_p1_val
function caml_blst_p1_equal_stubs(p, q) {
  var r = wasm_call('_blst_p1_is_equal', Blst_p1_val(p), Blst_p1_val(q));
  return r ? 1 : 0;
}

//Provides: caml_blst_p1_cneg_stubs
//Requires: wasm_call
//Requires: Blst_p1_val
function caml_blst_p1_cneg_stubs(p, b) {
  wasm_call('_blst_p1_cneg', Blst_p1_val(p), b);
  return 0;
}

//Provides: caml_blst_p1_mult_stubs
//Requires: wasm_call
//Requires: Blst_p1_val
//Requires: integers_int32_of_uint32
function caml_blst_p1_mult_stubs(buffer, p, n, size) {
  wasm_call(
      '_blst_p1_mult',
      Blst_p1_val(buffer),
      Blst_p1_val(p),
      n,
      integers_int32_of_uint32(size)
  );
  return 0;
}

//Provides: caml_blst_p1_serialize_stubs
//Requires: wasm_call
//Requires: Blst_p1_val
function caml_blst_p1_serialize_stubs(buffer, p) {
  wasm_call('_blst_p1_serialize', buffer, Blst_p1_val(p));
  return 0;
}

//Provides: caml_blst_p1_compress_stubs
//Requires: wasm_call
//Requires: Blst_p1_val
function caml_blst_p1_compress_stubs(buffer, p) {
  wasm_call('_blst_p1_compress', buffer, Blst_p1_val(p));
  return 0;
}

//Provides: caml_blst_p1_deserialize_stubs
//Requires: wasm_call
//Requires: Blst_p1_affine_val
function caml_blst_p1_deserialize_stubs(buffer, p) {
  var r /* int */ = wasm_call(
      '_blst_p1_deserialize',
      Blst_p1_affine_val(buffer),
      p
  );
  return r;
}

//Provides: caml_blst_p1_uncompress_stubs
//Requires: wasm_call
//Requires: Blst_p1_affine_val
function caml_blst_p1_uncompress_stubs(buffer, p) {
  var r /* int */ = wasm_call(
      '_blst_p1_uncompress',
      Blst_p1_affine_val(buffer),
      p
  );
  return r;
}

//Provides: caml_blst_p1_hash_to_curve_stubs
//Requires: wasm_call
//Requires: Blst_p1_val
//Requires: integers_int32_of_uint32
function caml_blst_p1_hash_to_curve_stubs(
    buffer,
    msg,
    msg_length,
    dst,
    dst_length,
    aug,
    aug_length
) {
  wasm_call(
      '_blst_hash_to_g1',
      Blst_p1_val(buffer),
      msg,
      integers_int32_of_uint32(msg_length),
      dst,
      integers_int32_of_uint32(dst_length),
      aug,
      integers_int32_of_uint32(aug_length)
  );
  return 0;
}

//Provides: caml_blst_p1_hash_to_curve_stubs_bytecode
//Requires: caml_blst_p1_hash_to_curve_stubs
function caml_blst_p1_hash_to_curve_stubs_bytecode(
    buffer,
    msg,
    msg_length,
    dst,
    dst_length,
    aug,
    aug_length
) {
  return caml_blst_p1_hash_to_curve_stubs(
      buffer,
      msg,
      msg_length,
      dst,
      dst_length,
      aug,
      aug_length
  );
}

//Provides: caml_blst_p1_memcpy_stubs
//Requires: caml_blst_memcpy, blst_p1_sizeof
//Requires: Blst_p1_val
function caml_blst_p1_memcpy_stubs(dst, src) {
  caml_blst_memcpy(Blst_p1_val(dst), Blst_p1_val(src), blst_p1_sizeof());
  return 0;
}

//Provides: caml_blst_p1_set_coordinates_stubs
//Requires: wasm_call
//Requires: Blst_p1_val, Blst_fp_val
function caml_blst_p1_set_coordinates_stubs(buffer, x, y) {
  var buffer_c = Blst_p1_val(buffer);
  var x_c = Blst_fp_val(x);
  var y_c = Blst_fp_val(y);
  wasm_call('_blst_p1_set_coordinates', buffer_c, x_c, y_c);
  return 0;
}

// P2

//Provides: Blst_p2_val
function Blst_p2_val(v) {
  return v.v;
}

//Provides: Blst_p2_affine_val
function Blst_p2_affine_val(v) {
  return v.v;
}

//Provides: blst_p2_sizeof
//Requires: wasm_call
function blst_p2_sizeof() {
  return wasm_call('_blst_p2_sizeof');
}

//Provides: Blst_p2
//Requires: blst_p2_sizeof
function Blst_p2() {
  this.v = new globalThis.Uint8Array(blst_p2_sizeof());
}

//Provides: blst_p2_affine_sizeof
//Requires: wasm_call
function blst_p2_affine_sizeof() {
  return wasm_call('_blst_p2_affine_sizeof');
}

//Provides: Blst_p2_affine
//Requires: blst_p2_affine_sizeof
function Blst_p2_affine() {
  this.v = new globalThis.Uint8Array(blst_p2_affine_sizeof());
}

//Provides: allocate_p2_stubs
//Requires: Blst_p2
function allocate_p2_stubs(unit) {
  return new Blst_p2();
}

//Provides: allocate_p2_affine_stubs
//Requires: Blst_p2_affine
function allocate_p2_affine_stubs(unit) {
  return new Blst_p2_affine();
}

//Provides: caml_blst_p2_to_affine_stubs
//Requires: wasm_call
//Requires: Blst_p2_val, Blst_p2_affine_val
function caml_blst_p2_to_affine_stubs(buffer, p) {
  wasm_call('_blst_p2_to_affine', Blst_p2_affine_val(buffer), Blst_p2_val(p));
  return 0;
}

//Provides: caml_blst_p2_from_affine_stubs
//Requires: wasm_call
//Requires: Blst_p2_val, Blst_p2_affine_val
function caml_blst_p2_from_affine_stubs(buffer, p) {
  wasm_call('_blst_p2_from_affine', Blst_p2_val(buffer), Blst_p2_affine_val(p));
  return 0;
}

//Provides: caml_blst_p2_double_stubs
//Requires: wasm_call
//Requires: Blst_p2_val
function caml_blst_p2_double_stubs(buffer, p) {
  wasm_call('_blst_p2_double', Blst_p2_val(buffer), Blst_p2_val(p));
  return 0;
}

//Provides: caml_blst_p2_add_or_double_stubs
//Requires: wasm_call
//Requires: Blst_p2_val
function caml_blst_p2_add_or_double_stubs(buffer, p, q) {
  wasm_call(
      '_blst_p2_add_or_double',
      Blst_p2_val(buffer),
      Blst_p2_val(p),
      Blst_p2_val(q)
  );
  return 0;
}

//Provides: caml_blst_p2_is_inf_stubs
//Requires: wasm_call
//Requires: Blst_p2_val
function caml_blst_p2_is_inf_stubs(p) {
  var r = wasm_call('_blst_p2_is_inf', Blst_p2_val(p));
  return r ? 1 : 0;
}

//Provides: caml_blst_p2_in_g2_stubs
//Requires: wasm_call
//Requires: Blst_p2_val
function caml_blst_p2_in_g2_stubs(p) {
  var r = wasm_call('_blst_p2_in_g2', Blst_p2_val(p));
  return r ? 1 : 0;
}

//Provides: caml_blst_p2_equal_stubs
//Requires: wasm_call
//Requires: Blst_p2_val
function caml_blst_p2_equal_stubs(p, q) {
  var r = wasm_call('_blst_p2_is_equal', Blst_p2_val(p), Blst_p2_val(q));
  return r ? 1 : 0;
}

//Provides: caml_blst_p2_cneg_stubs
//Requires: wasm_call
//Requires: Blst_p2_val
function caml_blst_p2_cneg_stubs(p, b) {
  wasm_call('_blst_p2_cneg', Blst_p2_val(p), b);
  return 0;
}
//Provides: caml_blst_p2_mult_stubs
//Requires: wasm_call
//Requires: Blst_p2_val
//Requires: integers_int32_of_uint32
function caml_blst_p2_mult_stubs(buffer, p, n, size) {
  wasm_call(
      '_blst_p2_mult',
      Blst_p2_val(buffer),
      Blst_p2_val(p),
      n,
      integers_int32_of_uint32(size)
  );
  return 0;
}

//Provides: caml_blst_p2_serialize_stubs
//Requires: wasm_call
//Requires: Blst_p2_val
function caml_blst_p2_serialize_stubs(buffer, p) {
  wasm_call('_blst_p2_serialize', buffer, Blst_p2_val(p));
  return 0;
}

//Provides: caml_blst_p2_compress_stubs
//Requires: wasm_call
//Requires: Blst_p2_val
function caml_blst_p2_compress_stubs(buffer, p) {
  wasm_call('_blst_p2_compress', buffer, Blst_p2_val(p));
  return 0;
}

//Provides: caml_blst_p2_deserialize_stubs
//Requires: wasm_call
//Requires: Blst_p2_affine_val
function caml_blst_p2_deserialize_stubs(buffer, p) {
  var r /* int */ = wasm_call(
      '_blst_p2_deserialize',
      Blst_p2_affine_val(buffer),
      p
  );
  return r;
}

//Provides: caml_blst_p2_uncompress_stubs
//Requires: wasm_call
//Requires: Blst_p2_affine_val
function caml_blst_p2_uncompress_stubs(buffer, p) {
  var r /* int */ = wasm_call(
      '_blst_p2_uncompress',
      Blst_p2_affine_val(buffer),
      p
  );
  return r;
}

//Provides: caml_blst_p2_hash_to_curve_stubs
//Requires: wasm_call
//Requires: Blst_p2_val
//Requires: integers_int32_of_uint32
function caml_blst_p2_hash_to_curve_stubs(
    buffer,
    msg,
    msg_length,
    dst,
    dst_length,
    aug,
    aug_length
) {
  wasm_call(
      '_blst_hash_to_g2',
      Blst_p2_val(buffer),
      msg,
      integers_int32_of_uint32(msg_length),
      dst,
      integers_int32_of_uint32(dst_length),
      aug,
      integers_int32_of_uint32(aug_length)
  );
  return 0;
}

//Provides: caml_blst_p2_hash_to_curve_stubs_bytecode
//Requires: caml_blst_p2_hash_to_curve_stubs
function caml_blst_p2_hash_to_curve_stubs_bytecode(
    buffer,
    msg,
    msg_length,
    dst,
    dst_length,
    aug,
    aug_length
) {
  return caml_blst_p2_hash_to_curve_stubs(
      buffer,
      msg,
      msg_length,
      dst,
      dst_length,
      aug,
      aug_length
  );
}

//Provides: caml_blst_p2_memcpy_stubs
//Requires: caml_blst_memcpy, blst_p2_sizeof
//Requires: Blst_p2_val
function caml_blst_p2_memcpy_stubs(dst, src) {
  caml_blst_memcpy(Blst_p2_val(dst), Blst_p2_val(src), blst_p2_sizeof());
  return 0;
}

//Provides: caml_blst_p2_set_coordinates_stubs
//Requires: wasm_call
//Requires: Blst_p2_val, Blst_fp2_val
function caml_blst_p2_set_coordinates_stubs(buffer, x, y) {
  var buffer_c = Blst_p2_val(buffer);
  var x_c = Blst_fp2_val(x);
  var y_c = Blst_fp2_val(y);
  wasm_call('_blst_p2_set_coordinates', buffer_c, x_c, y_c);
  return 0;
}
// Pairing

//Provides: caml_blst_miller_loop_stubs
//Requires: wasm_call
//Requires: Blst_fp12_val, Blst_p2_affine_val, Blst_p1_affine_val
function caml_blst_miller_loop_stubs(buffer, g2, g1) {
  wasm_call(
      '_blst_miller_loop',
      Blst_fp12_val(buffer),
      Blst_p2_affine_val(g2),
      Blst_p1_affine_val(g1)
  );
  return 0;
}

//Provides: caml_blst_miller_loop_list_stubs
//Requires: wasm_call
//Requires: Blst_fp12_val
//Requires: Blst_p1_affine_val, Blst_p2_affine_val
//Requires: Blst_p1_val, Blst_p2_val
//Requires: Blst_fp12, Blst_p1_affine, Blst_p2_affine
function caml_blst_miller_loop_list_stubs(out, points_array, length) {
  var out_c = Blst_fp12_val(out);
  wasm_call('_blst_fp12_set_to_one', out_c);

  var tmp = Blst_fp12_val(new Blst_fp12());
  var tmp_p1 = Blst_p1_affine_val(new Blst_p1_affine());
  var tmp_p2 = Blst_p2_affine_val(new Blst_p2_affine());

  for (var i = 0; i < length; i++) {
    wasm_call(
        '_blst_p1_to_affine',
        tmp_p1,
        Blst_p1_val(points_array[i + 1][1])
    );
    wasm_call(
        '_blst_p2_to_affine',
        tmp_p2,
        Blst_p2_val(points_array[i + 1][2])
    );
    wasm_call('_blst_miller_loop', tmp, tmp_p2, tmp_p1);
    wasm_call('_blst_fp12_mul', out_c, out_c, tmp);
  }
  return 0;
}

//Provides: caml_blst_final_exponentiation_stubs
//Requires: wasm_call
//Requires: Blst_fp12_val
function caml_blst_final_exponentiation_stubs(buffer, p) {
  wasm_call('_blst_final_exp', Blst_fp12_val(buffer), Blst_fp12_val(p));
  return 0;
}

//Provides: caml_blst_g1_pippenger_stubs
//Requires: integers_int32_of_uint32
//Requires: Blst_p1_affine, Blst_scalar
//Requires: Blst_p1_val, Blst_fr_val, Blst_p1_affine_val, Blst_scalar_val
//Requires: wasm_call
function caml_blst_g1_pippenger_stubs(
    buffer,
    jacobian_list,
    scalars,
    start,
    npoints
) {
  var npoints_c = integers_int32_of_uint32(npoints);
  var start_c = integers_int32_of_uint32(start);

  var addr_ps = new Array(npoints_c);

  var addr_scalars_bs = new Array(npoints_c);

  var scalar = Blst_scalar_val(new Blst_scalar());

  for (var i = 0; i < npoints_c; i++) {
    var bs = Blst_scalar_val(new Blst_scalar());
    var ps = Blst_p1_affine_val(new Blst_p1_affine());
    wasm_call(
        '_blst_p1_to_affine',
        ps,
        Blst_p1_val(jacobian_list[start_c + i + 1])
    );
    addr_ps[i] = ps;
    wasm_call(
        '_blst_scalar_from_fr',
        scalar,
        Blst_fr_val(scalars[start_c + i + 1])
    );
    wasm_call('_blst_lendian_from_scalar', bs, scalar);
    addr_scalars_bs[i] = bs;
  }
  var scratch_size = wasm_call(
      '_blst_p1s_mult_pippenger_scratch_sizeof',
      npoints_c
  );
  var scratch = new globalThis.Uint8Array(scratch_size);

  wasm_call(
      '_blst_p1s_mult_pippenger',
      Blst_p1_val(buffer),
      addr_ps,
      npoints_c,
      addr_scalars_bs,
      256,
      scratch
  );
  return 0;
}

//Provides: caml_blst_g2_pippenger_stubs
//Requires: integers_int32_of_uint32
//Requires: Blst_p2_affine, Blst_scalar
//Requires: Blst_p2_val, Blst_fr_val, Blst_p2_affine_val, Blst_scalar_val
//Requires: wasm_call
function caml_blst_g2_pippenger_stubs(
    buffer,
    jacobian_list,
    scalars,
    start,
    npoints
) {
  var npoints_c = integers_int32_of_uint32(npoints);
  var start_c = integers_int32_of_uint32(start);

  var addr_ps = new Array(npoints_c);

  var addr_scalars_bs = new Array(npoints_c);

  var scalar = Blst_scalar_val(new Blst_scalar());

  for (var i = 0; i < npoints_c; i++) {
    var bs = Blst_scalar_val(new Blst_scalar());
    var ps = Blst_p2_affine_val(new Blst_p2_affine());
    wasm_call(
        '_blst_p2_to_affine',
        ps,
        Blst_p2_val(jacobian_list[start_c + i + 1])
    );
    addr_ps[i] = ps;
    wasm_call(
        '_blst_scalar_from_fr',
        scalar,
        Blst_fr_val(scalars[start_c + i + 1])
    );
    wasm_call('_blst_lendian_from_scalar', bs, scalar);
    addr_scalars_bs[i] = bs;
  }
  var scratch = new globalThis.Uint8Array(
      wasm_call('_blst_p2s_mult_pippenger_scratch_sizeof', npoints_c)
  );

  wasm_call(
      '_blst_p2s_mult_pippenger',
      Blst_p2_val(buffer),
      addr_ps,
      npoints_c,
      addr_scalars_bs,
      256,
      scratch
  );
  return 0;
}

//Provides: caml_blst_fr_inner_product_stubs
//Requires: wasm_call
//Requires: callocate_fr_stubs, Blst_fr_val
function caml_blst_fr_inner_product_stubs(
    buffer,
    fr_array_left,
    fr_array_right,
    length
) {
  var tmp = Blst_fr_val(callocate_fr_stubs());
  var buffer_c = Blst_fr_val(buffer);
  var length_c = length;
  for (var i = 0; i < length_c; i++) {
    wasm_call(
        '_blst_fr_mul',
        tmp,
        Blst_fr_val(fr_array_left[i + 1]),
        Blst_fr_val(fr_array_right[i + 1])
    );
    wasm_call('_blst_fr_add', buffer_c, tmp, buffer_c);
  }
  return 0;
}

//Provides: Blst_p1_affine_array
//Requires: blst_p1_affine_sizeof
function Blst_p1_affine_array(n) {
  this.chunk_len = blst_p1_affine_sizeof();
  this.v = new globalThis.Uint8Array(this.chunk_len * n);
}

Blst_p1_affine_array.prototype.nth = function(n) {
  var start = n * this.chunk_len;
  var stop = start + this.chunk_len;
  return this.v.subarray(start, stop);
};

//Provides: allocate_p1_affine_array_stubs
//Requires: Blst_p1_affine_array
function allocate_p1_affine_array_stubs(n) {
  return new Blst_p1_affine_array(n);
}

//Provides: caml_blst_p1_affine_array_set_p1_points_stubs
//Requires: Blst_p1_val
//Requires: wasm_call
function caml_blst_p1_affine_array_set_p1_points_stubs(buffer, l, n) {
  for (var i = 0; i < n; i++) {
    var p = Blst_p1_val(l[i + 1]);
    wasm_call('_blst_p1_to_affine', buffer.nth(i), p);
  }
  return 0;
}

//Provides: caml_blst_p1_affine_array_get_stubs
//Requires: wasm_call
//Requires: Blst_p1_val
function caml_blst_p1_affine_array_get_stubs(buffer, list, i) {
  var buffer_c = Blst_p1_val(buffer);
  wasm_call('_blst_p1_from_affine', buffer_c, list.nth(i));
  return 0;
}

//Provides: caml_blst_g1_pippenger_contiguous_affine_array_stubs
//Requires: Blst_fr_val, Blst_p1_val, Blst_scalar_val
//Requires: Blst_scalar, integers_int32_of_uint32
//Requires: wasm_call
function caml_blst_g1_pippenger_contiguous_affine_array_stubs(
    buffer,
    affine_list,
    scalars,
    start,
    len
) {
  var start_c = integers_int32_of_uint32(start);
  var len_c = integers_int32_of_uint32(len);

  var addr_ps = new Array(len_c);
  var addr_scalars_bs = new Array(len_c);
  var scalar = Blst_scalar_val(new Blst_scalar());

  for (var i = 0; i < len_c; i++) {
    var bs = Blst_scalar_val(new Blst_scalar());
    wasm_call(
        '_blst_scalar_from_fr',
        scalar,
        Blst_fr_val(scalars[start_c + i + 1])
    );
    wasm_call('_blst_lendian_from_scalar', bs, scalar);
    addr_scalars_bs[i] = bs;
    addr_ps[i] = affine_list.nth(start_c + i);
  }

  var scratch_size = wasm_call(
      '_blst_p1s_mult_pippenger_scratch_sizeof',
      len_c
  );
  var scratch = new globalThis.Uint8Array(scratch_size);

  wasm_call(
      '_blst_p1s_mult_pippenger',
      Blst_p1_val(buffer),
      addr_ps,
      len_c,
      addr_scalars_bs,
      256,
      scratch
  );

  return 0;
}

//Provides: Blst_p2_affine_array
//Requires: blst_p2_affine_sizeof
function Blst_p2_affine_array(n) {
  this.chunk_len = blst_p2_affine_sizeof();
  this.v = new globalThis.Uint8Array(this.chunk_len * n);
}

Blst_p2_affine_array.prototype.nth = function(n) {
  var start = n * this.chunk_len;
  var stop = start + this.chunk_len;
  return this.v.subarray(start, stop);
};

//Provides: allocate_p2_affine_array_stubs
//Requires: Blst_p2_affine_array
function allocate_p2_affine_array_stubs(n) {
  return new Blst_p2_affine_array(n);
}

//Provides: caml_blst_p2_affine_array_set_p2_points_stubs
//Requires: Blst_p2_val
//Requires: wasm_call
function caml_blst_p2_affine_array_set_p2_points_stubs(buffer, l, n) {
  for (var i = 0; i < n; i++) {
    var p = Blst_p2_val(l[i + 1]);
    wasm_call('_blst_p2_to_affine', buffer.nth(i), p);
  }
  return 0;
}

//Provides: caml_blst_p2_affine_array_get_stubs
//Requires: wasm_call
//Requires: Blst_p2_val
function caml_blst_p2_affine_array_get_stubs(buffer, list, i) {
  var buffer_c = Blst_p2_val(buffer);
  wasm_call('_blst_p2_from_affine', buffer_c, list.nth(i));
  return 0;
}

//Provides: caml_blst_g2_pippenger_contiguous_affine_array_stubs
//Requires: Blst_fr_val, Blst_p2_val, Blst_scalar_val
//Requires: Blst_scalar, integers_int32_of_uint32
//Requires: wasm_call
function caml_blst_g2_pippenger_contiguous_affine_array_stubs(
    buffer,
    affine_list,
    scalars,
    start,
    len
) {
  var start_c = integers_int32_of_uint32(start);
  var len_c = integers_int32_of_uint32(len);

  var addr_ps = new Array(len_c);
  var addr_scalars_bs = new Array(len_c);
  var scalar = Blst_fr_val(new Blst_scalar());

  for (var i = 0; i < len_c; i++) {
    var bs = Blst_scalar_val(new Blst_scalar());
    wasm_call(
        '_blst_scalar_from_fr',
        scalar,
        Blst_fr_val(scalars[start_c + i + 1])
    );
    wasm_call('_blst_lendian_from_scalar', bs, scalar);
    addr_scalars_bs[i] = bs;
    addr_ps[i] = affine_list.nth(start_c + i);
  }
  var scratch_size = wasm_call(
      '_blst_p2s_mult_pippenger_scratch_sizeof',
      len_c
  );
  var scratch = new globalThis.Uint8Array(scratch_size);
  wasm_call(
      '_blst_p2s_mult_pippenger',
      Blst_p2_val(buffer),
      addr_ps,
      len_c,
      addr_scalars_bs,
      256,
      scratch
  );

  return 0;
}

//Provides: caml_built_with_blst_portable_stubs
function caml_built_with_blst_portable_stubs(unit) {
  return 0;
}

//Provides: caml_blst_fr_of_montgomery_le_stubs
//Requires: caml_failwith
function caml_blst_fr_of_montgomery_le_stubs(vx, vx0, vx1, vx2, vx3) {
  caml_failwith('Not implemented');
}

//Provides: caml_blst_fr_to_montgomery_le_stubs
//Requires: caml_failwith
function caml_blst_fr_to_montgomery_le_stubs(vx0, vx1, vx2, vx3, vx) {
  caml_failwith('Not implemented');
}
