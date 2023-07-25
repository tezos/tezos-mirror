
//Provides: bls_allocate_mlbytes
//Requires: MlBytes, caml_failwith, caml_array_of_bytes
function bls_allocate_mlbytes(x) {
  var g = globalThis;
  var M = g._BLS12381;
  if (!M) caml_failwith('bls12-381 was not initialized');
  var a = caml_array_of_bytes(x);
  var p = M._malloc(a.length);
  M.HEAPU8.set(a, p);
  return p;
}

//Provides: bls_free
//Requires: caml_failwith
function bls_free(p) {
  var g = globalThis;
  var M = g._BLS12381;
  if (!M) caml_failwith('bls12-381 was not initialized');
  M._free(p);
}

//Provides: wasm_call
//Requires: Ml_Bigarray, MlBytes
//Requires: caml_failwith, caml_ml_bytes_length, caml_array_of_bytes
function wasm_call() {
  var g = globalThis;
  var M = g._BLS12381;
  var f = arguments[0];
  if (!M) caml_failwith('bls12-381 was not initialized');
  if (!M[f]) caml_failwith(f + ' is not implemented');
  var args = Array.prototype.slice.call(arguments, 1);
  // argsc is the array of argument that we will pass to wasm,
  // it will be mutated before the call.
  var argsc = args.slice();
  // argsu is used to handle the case where multiple arguments
  // are physically the same.
  // We don't want to copy the piece of memory multiple time into wasm
  var argsu = new g.Map();
  for (var i = 0; i < args.length; i++) {
    var x = args[i];
    if (typeof x == 'number' || typeof x == 'boolean' || x === null) {
      // Theses primitive types can be passed to wasm
      continue;
    } else if (x instanceof g.Uint8Array) {
      if (argsu.get(x)) {
        argsc[i] = argsu.get(x);
      } else {
        var p = M._malloc(x.length);
        M.HEAPU8.set(x, p);
        argsu.set(x, p);
        argsc[i] = p;
      }
    } else if (x instanceof MlBytes) {
      if (argsu.get(x)) {
        argsc[i] = argsu.get(x);
      } else {
        var a = caml_array_of_bytes(x);
        var p = M._malloc(a.length);
        M.HEAPU8.set(a, p);
        argsu.set(x, p);
        argsc[i] = p;
      }
    } else if (
      x instanceof Array &&
      x.every(function(x) {
        return x instanceof g.Uint8Array;
      })
    ) {
      var ps = new g.Uint32Array(x.length);
      for (var k = 0; k < x.length; k++) {
        if (argsu.get(x[k])) {
          ps[k] = argsu.get(x[k]);
        } else {
          var p = M._malloc(x[k].length);
          M.HEAPU8.set(x[k], p);
          argsu.set(x[k], p);
          ps[k] = p;
        }
      }
      var p = M._malloc(ps.length * 4);
      M.HEAPU32.set(ps, p / 4);
      argsu.set(ps, p);
      argsc[i] = p;
    } else {
      // Other types of arguments are not handled yet
      // Try to report and informative error
      var r = '(';
      for (i = 0; i < args.length; i++) {
        if (i != 0) r += ', ';
        var err = '*ERR*';
        var x = args[i];
        var typeof_x = typeof x;
        // Valid argument type
        if (typeof_x == 'number' || typeof_x == 'boolean') r += typeof_x;
        else if (x instanceof g.Uint8Array) r += 'uint8';
        // Invalid argument type
        else if (x instanceof MlBytes) r += 'bytes';
        else if (x == null) r += err + 'null';
        else if (x == undefined) r += err + 'undefined';
        else r += err + typeof_x;
      }
      r += ')';
      g.console.error(
          'call_wasm: unsupported argument type: ' + f + r,
          new Error()
      );
      caml_failwith(
          'call_wasm: ' + f + ' called with unsupported argument type: ' + r
      );
    }
  }

  // Call into wasm
  var r = M[f].apply(null, argsc);

  // Copy the memory back into the JS world
  var it = argsu.entries();
  for (;;) {
    var e = it.next();
    if (e.done) break;
    var x = e.value[0];
    var p = e.value[1];
    if (x instanceof g.Uint8Array) {
      x.set(new g.Uint8Array(M.HEAPU8.buffer, p, x.length));
      M._free(p);
    } else if (x instanceof g.Uint32Array) {
      M._free(p);
    } else if (x instanceof MlBytes) {
      var a = caml_array_of_bytes(x);
      for (var i = 0; i < a.length; i++) {
        a[i] = M.HEAPU8[p + i];
      }
      M._free(p);
    } else {
      caml_failwith('call_wasm: Impossible');
    }
  }
  return r;
}

//Provides: caml_blst_memcpy
function caml_blst_memcpy(dest, src, size) {
  if (src.length != size) src = src.subarray(0, size);
  dest.set(src, 0);
}
