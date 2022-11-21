// Open Source License
// Copyright (c) 2022 DaiLambda, Inc. <contact@dailambda,jp>
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.

// The array must be copied for later mutation.
//Provides: tzbytes_copy_array_of_bytes
//Requires: caml_array_of_bytes, caml_ml_bytes_length
function tzbytes_copy_array_of_bytes(a)
{
    var aa = caml_array_of_bytes (a);
    var aaa = new globalThis.Uint8Array(caml_ml_bytes_length(a));
    aaa.set(aa);
    return aaa;
}

//Provides: tzbytes_copy_bytes
//Requires: caml_ml_bytes_length, caml_create_bytes, caml_blit_bytes
function tzbytes_copy_bytes(a)
{
    var len = caml_ml_bytes_length(a);
    var b = caml_create_bytes(len);
    caml_blit_bytes(a, 0, b, 0, len);
    return b;
}

//Provides: bytes_logand
//Requires: tzbytes_copy_array_of_bytes, caml_bytes_of_array
function bytes_logand(a, b) {
    var aa = tzbytes_copy_array_of_bytes(a);
    var ab = tzbytes_copy_array_of_bytes(b);
    if (aa.length < ab.length){ [aa, ab] = [ab, aa]; }
    // aa is longer
    var diff = aa.length - ab.length;
    for (var i = 0, j = diff; i < ab.length; i++, j++) {
	ab[i] &= aa[j];
    }
    return caml_bytes_of_array(ab);
}

//Provides: bytes_logor
//Requires: tzbytes_copy_array_of_bytes, caml_bytes_of_array
function bytes_logor(a, b) {
    var aa = tzbytes_copy_array_of_bytes(a);
    var ab = tzbytes_copy_array_of_bytes(b);
    if (aa.length < ab.length){ [aa, ab] = [ab, aa]; }
    // aa is longer
    var diff = aa.length - ab.length;
    for (var i = diff, j = 0; i < aa.length; i++, j++) {
	aa[i] |= ab[j];
    }
    return caml_bytes_of_array(aa);
}

//Provides: bytes_logxor
//Requires: tzbytes_copy_array_of_bytes, caml_bytes_of_array
function bytes_logxor(a, b) {
    var aa = tzbytes_copy_array_of_bytes(a);
    var ab = tzbytes_copy_array_of_bytes(b);
    if (aa.length < ab.length){ [aa, ab] = [ab, aa]; }
    // aa is longer
    var diff = aa.length - ab.length;
    for (var i = diff, j = 0; i < aa.length; i++, j++) {
	aa[i] ^= ab[j];
    }
    return caml_bytes_of_array(aa);
}

//Provides: bytes_lognot
//Requires: tzbytes_copy_array_of_bytes, caml_bytes_of_array
function bytes_lognot(a) {
    var aa = tzbytes_copy_array_of_bytes(a);
    for (var i = 0; i < aa.length; i++) {
	aa[i] = ~aa[i];
    }
    return caml_bytes_of_array(aa);
}

//Provides: bytes_shift_left
//Requires: tzbytes_copy_array_of_bytes, caml_bytes_of_array, caml_invalid_argument, caml_ml_bytes_length, tzbytes_copy_bytes
function bytes_shift_left(a, n) {
    if ( n < 0 ) {
	caml_invalid_argument("bytes_shift_left");
    }

    if ( n == 0 ) { return tzbytes_copy_bytes(a); }

    var shift = n % 8;
    var nbytes = n >> 3;
    var len_a = caml_ml_bytes_length(a);

    var aa = tzbytes_copy_array_of_bytes (a);

    if ( shift == 0 ) {
	var ra = new globalThis.Uint8Array(len_a+nbytes);
	ra.set(aa);
    } else {
        var len_r = len_a + nbytes + 1;
        var shift_right = 8 - shift;
    
        var ra = new globalThis.Uint8Array(len_r);
    
        ra[0] = aa[0]  >> shift_right;
    
        for(var i = 1; i < len_a; i++) {
    	ra[i] = (((aa[i-1] << 8) + aa[i]) >> shift_right) & 255;
        }
    
        ra[len_a] = (aa[len_a-1] << shift) & 255;
    }

    return caml_bytes_of_array(ra);
}

//Provides: bytes_shift_right
//Requires: tzbytes_copy_array_of_bytes, caml_bytes_of_array, caml_invalid_argument, caml_ml_bytes_length, tzbytes_copy_bytes, caml_create_bytes
function bytes_shift_right(a, n) {
    if ( n < 0 ) {
	caml_invalid_argument("bytes_shift_right");
    }

    if ( n == 0 ) { return tzbytes_copy_bytes(a); }

    var shift = n % 8;
    var nbytes = n >> 3;
    var len_a = caml_ml_bytes_length(a);

    var aa = tzbytes_copy_array_of_bytes (a);

    if ( len_a <= nbytes ) {
	return caml_create_bytes(0);
    }

    var len_r = len_a - nbytes;

    if (shift == 0) {
	ra = aa.slice(0,len_r);
    } else {
	var ra = new globalThis.Uint8Array(len_r);
	ra[0] = aa[0] >> shift;
	for(var i = 1; i < len_r; i++) {
	    ra[i] = (((aa[i-1] << 8) + aa[i]) >> shift) & 255;
	}
    }

    return caml_bytes_of_array(ra);
}
