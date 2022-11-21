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

#include <string.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

#define SWAP(ty, a, b) ({ \
            ty tmp = (a); \
            (a) = (b); \
            (b) = (tmp); \
        })
    
CAMLprim value bytes_logand(value a, value b)
{
    //               <------ l ---->
    //        <diff>
    // a     |aaaaaaaaaaaaaaaaaaaaaa|
    // b            |bbbbbbbbbbbbbbb|
    // r            |rrrrrrrrrrrrrrr|

    CAMLparam2(a, b);
    CAMLlocal1(r);

    mlsize_t la = caml_string_length(a);
    mlsize_t lb = caml_string_length(b);

    // make [a] always longer than (or same length as) [b]
    if ( la < lb ) {
        SWAP(mlsize_t, la, lb);
        SWAP(value, a, b);
    }
    
    mlsize_t diff = la - lb;

    r = caml_alloc_string(lb);

    unsigned char *pa = Bytes_val(a);
    unsigned char *pb = Bytes_val(b);
    unsigned char *pr = Bytes_val(r);

    pa += diff;

    for(mlsize_t i = 0; i < lb; i++, pr++, pa++, pb++){
        *pr = *pa & *pb;
    }
    CAMLreturn(r);
}

CAMLprim value bytes_logor(value a, value b)
{
    //        <---------len-------->
    //        <diff>
    // a     |aaaaaaaaaaaaaaaaaaaaaa|
    // b            |bbbbbbbbbbbbbbb|
    // r     |aaaaaa|rrrrrrrrrrrrrrr|

    CAMLparam2(a, b);
    CAMLlocal1(r);

    mlsize_t la = caml_string_length(a);
    mlsize_t lb = caml_string_length(b);

    // make [a] always longer than (or same length as) [b]
    if ( la < lb ) {
        SWAP(mlsize_t, la, lb);
        SWAP(value, a, b);
    }

    mlsize_t diff = la - lb;

    r = caml_alloc_string(la);

    unsigned char *pa = Bytes_val(a);
    unsigned char *pb = Bytes_val(b);
    unsigned char *pr = Bytes_val(r);

    // diff part

    memcpy(pr, pa, diff);

    // the rest

    pr += diff;
    pa += diff;

    for(mlsize_t i = diff; i < la; i++, pr++, pa++, pb++){
        *pr = *pa | *pb;
    }
    CAMLreturn(r);
}

CAMLprim value bytes_logxor(value a, value b)
{
    //        <---------len-------->
    //        <diff>
    // a     |aaaaaaaaaaaaaaaaaaaaaa|
    // b            |bbbbbbbbbbbbbbb|
    // r     |aaaaaa|rrrrrrrrrrrrrrr|

    CAMLparam2(a, b);
    CAMLlocal1(r);

    mlsize_t la = caml_string_length(a);
    mlsize_t lb = caml_string_length(b);

    // make [a] always longer than (or same length as) [b]
    if ( la < lb ) {
        SWAP(mlsize_t, la, lb);
        SWAP(value, a, b);
    }

    mlsize_t diff = la - lb;

    r = caml_alloc_string(la);

    unsigned char *pa = Bytes_val(a);
    unsigned char *pb = Bytes_val(b);
    unsigned char *pr = Bytes_val(r);

    // diff part

    memcpy(pr, pa, diff);

    // the rest

    pr += diff;
    pa += diff;

    for(mlsize_t i = diff; i < la; i++, pr++, pa++, pb++){
        *pr = *pa ^ *pb;
    }
    CAMLreturn(r);
}

CAMLprim value bytes_lognot(value a)
{
    CAMLparam1(a);
    CAMLlocal1(r);

    mlsize_t l = caml_string_length(a);

    r = caml_alloc_string(l);

    unsigned char *pa = Bytes_val(a);
    unsigned char *pr = Bytes_val(r);

    for(mlsize_t i = 0; i < l; i++){
        *pr++ = ~*pa++;
    }
    CAMLreturn(r);
}

CAMLprim value bytes_shift_left(value a, value vn)
{
    CAMLparam2(a, vn);
    CAMLlocal1(r);

    mlsize_t n = Long_val(vn);

    if (n < 0) caml_invalid_argument("bytes_shift_left");

    int shift = n % 8;
    mlsize_t nbytes = n / 8;
    mlsize_t len_a = caml_string_length(a);

    if ( shift == 0 ) {

        //     <----- len_a ---->
        // a  |aaaaaaaaaaaaaaaaaa|
        // r  |aaaaaaaaaaaaaaaaaa|0000000000|
        //                        <-nbytes->
        //     <--------- len_r ----------->

        mlsize_t len_r = len_a + nbytes;

        r = caml_alloc_string(len_r);

        unsigned char *pa = Bytes_val(a);
        unsigned char *pr = Bytes_val(r);

        memcpy(pr, pa, len_a);

        bzero(pr + len_a, nbytes);

    } else {
        mlsize_t len_r = len_a + nbytes + 1;
        int shift_right = 8 - shift;

        //         <---- len_a ----->
        // a      |aaaaaaaaaaaaaaaaaa|
        //     <--------- len_r------------------>
        // r  |000|aaaaaaaaaaaaaaaaaa|000|00000000|
        //      ^                      ^  <nbytes>
        //      |                      |
        //    shift_right bits      shift bits

        r = caml_alloc_string(len_r);

        unsigned char *pa = Bytes_val(a);
        unsigned char *pr = Bytes_val(r);

        // first byte
        *pr++ = *pa >> shift_right;

        // middle
        for(mlsize_t i = 1; i < len_a; i++, pa++){
            *pr++ = (((*pa << 8) + *(pa+1)) >> shift_right) & 255;
        }

        // last byte before 0s
        *pr++ = (*pa << shift) & 255;

        // 0s
        bzero(pr, len_r - len_a - 1);
    }

    CAMLreturn(r);
}


CAMLprim value bytes_shift_right(value a, value vn)
{
    CAMLparam2(a, vn);
    CAMLlocal1(r);

    mlsize_t n = Long_val(vn);

    if ( n < 0 ) caml_invalid_argument("bytes_shift_right");

    int shift = n % 8;
    mlsize_t nbytes = n / 8;
    mlsize_t len_a = caml_string_length(a);

    if ( len_a <= nbytes ) {
        r = caml_alloc_string(0);
        CAMLreturn(r);
    }
    
    mlsize_t len_r = len_a - nbytes;
    r = caml_alloc_string(len_r);

    unsigned char *pa = Bytes_val(a);
    unsigned char *pr = Bytes_val(r);

    if ( shift == 0 ) {

        //     <---- len_a ------------>
        // a  |aaaaaaaaaaaaaaaaaaaaaaaaa|
        // r  |aaaaaaaaaaaaaa|<-nbytes->
        //     <--- len_r -->

        memcpy(pr, pa, len_r);

    } else {

        //     <---- len_a ----------->
        // a  |aaaaaaaaaaaaaaa|aaaaaaaa|
        //     <--- len_r----> <nbytes>
        // r  |000|aaaaaaaaaaa|
        //      ^
        //      |
        //     shift bits

        // first byte
        *pr++ = *pa >> shift;

        // the rest
        for(mlsize_t i = 1; i < len_r; i++, pa++, pr++){
            *pr = (((*pa << 8) + *(pa+1)) >> shift) & 255;
        }
    }

    CAMLreturn(r);
}
