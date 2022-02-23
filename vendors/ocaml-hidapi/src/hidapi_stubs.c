/* --------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  --------------------------------------------------------------------------- */

#include <stdio.h>
#include <hidapi.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/bigarray.h>

typedef struct hid_device_info hid_device_info_t;

#define Hid_val(v) (*((hid_device **) Data_custom_val(v)))
#define Hidinfo_val(v) (*((hid_device_info_t **) Data_custom_val(v)))

#define Gen_custom_block(SNAME, CNAME, MNAME)                           \
    static int compare_##SNAME(value a, value b) {                      \
        CNAME *aa = MNAME(a), *bb = MNAME(b);                           \
        return (aa == bb ? 0 : (aa < bb ? -1 : 1));                     \
    }                                                                   \
                                                                        \
    static struct custom_operations hidapi_##SNAME##_ops = {		\
        .identifier = "hidapi_" #SNAME,					\
        .finalize = custom_finalize_default,                            \
        .compare = compare_##SNAME,                                     \
        .compare_ext = custom_compare_ext_default,                      \
        .hash = custom_hash_default,                                    \
        .serialize = custom_serialize_default,                          \
        .deserialize = custom_deserialize_default                       \
    };                                                                  \
                                                                        \
    static value alloc_##SNAME (CNAME *a) {                             \
        value custom = alloc_custom(&hidapi_##SNAME##_ops, sizeof(CNAME *), 0, 1); \
        MNAME(custom) = a;                                              \
        return custom;                                                  \
    }

Gen_custom_block(hid, hid_device, Hid_val)
Gen_custom_block(hidinfo, hid_device_info_t, Hidinfo_val)

CAMLprim value stub_hid_error(value dev) {
    CAMLparam1(dev);
    CAMLlocal2(ret, msg);

    const wchar_t *err_string = hid_error(Hid_val(dev));

    if (err_string == NULL) ret = Val_unit;

    else {
	char buf[1024] = {0};
	snprintf(buf, sizeof(buf), "%ls", err_string);
	ret = caml_alloc(1, 0);
	msg = caml_copy_string(buf);
	Store_field(ret, 0, msg);
    }

    CAMLreturn(ret);
}

static value copy_device_info (hid_device_info_t *di) {
    CAMLparam0();
    CAMLlocal5(result, path, sn, ms, ps);
    CAMLlocal3(ret1, ret2, ret3);

    result = caml_alloc_tuple(10);
    char buf[1024] = {0};

    path = caml_copy_string(di->path);
    Store_field(result, 0, path);

    Store_field(result, 1, Val_int(di->vendor_id));
    Store_field(result, 2, Val_int(di->product_id));

    if (di->serial_number == NULL)
	Store_field(result, 3, Val_unit);
    else {
	snprintf(buf, sizeof(buf), "%ls", di->serial_number);
	ret1 = caml_alloc(1, 0);
	sn = caml_copy_string(buf);
	Store_field(ret1, 0, sn);
	Store_field(result, 3, ret1);
    }

    Store_field(result, 4, Val_int(di->release_number));

    if (di->manufacturer_string == NULL)
	Store_field(result, 5, Val_unit);
    else {
	snprintf(buf, sizeof(buf), "%ls", di->manufacturer_string);
	ret2 = caml_alloc(1, 0);
	ms = caml_copy_string(buf);
	Store_field(ret2, 0, ms);
	Store_field(result, 5, ret2);
    }

    if (di->product_string == NULL)
	Store_field(result, 6, Val_unit);
    else {
	snprintf(buf, sizeof(buf), "%ls", di->product_string);
	ret3 = caml_alloc(1, 0);
	ps = caml_copy_string(buf);
	Store_field(ret3, 0, ps);
	Store_field(result, 6, ret3);
    }

    Store_field(result, 7, Val_int(di->usage_page));
    Store_field(result, 8, Val_int(di->usage));
    Store_field(result, 9, Val_int(di->interface_number));

    CAMLreturn(result);
}

CAMLprim value stub_hid_init(value unit) {
    return Val_int(hid_init());
}

CAMLprim value stub_hid_exit(value unit) {
    return Val_int(hid_exit());
}

CAMLprim value stub_hid_enumerate(value vendor_id, value product_id) {
    CAMLparam2(vendor_id, product_id);
    CAMLlocal2(ret, hidinfo);

    struct hid_device_info *di = hid_enumerate(Int_val(vendor_id), Int_val(product_id));

    if (di == NULL) ret = Val_unit;

    else {
	ret = caml_alloc(1, 0);
	hidinfo = alloc_hidinfo(di);
	Store_field(ret, 0, hidinfo);
    }

    CAMLreturn(ret);
}

CAMLprim value stub_hid_free_enumeration(value hid_info) {
    hid_free_enumeration(Hidinfo_val(hid_info));
    return Val_unit;
}

CAMLprim value stub_hid_enumerate_next(value hid_info) {
    CAMLparam1(hid_info);
    CAMLlocal4(ret, info, nextopt, next);

    hid_device_info_t *di = Hidinfo_val(hid_info);

    info = copy_device_info(di);
    ret = caml_alloc_tuple(2);

    if (!di->next) nextopt = Val_unit;
    else {
	nextopt = caml_alloc(1, 0);
	next = alloc_hidinfo(di->next);
	Store_field(nextopt, 0, next);
    }

    Store_field(ret, 0, info);
    Store_field(ret, 1, nextopt);

    CAMLreturn(ret);
}

CAMLprim value stub_hid_open(value vendor_id, value product_id) {
    CAMLparam2(vendor_id, product_id);
    CAMLlocal2(ret, hid);

    hid_device* h = hid_open(Int_val(vendor_id), Int_val(product_id), NULL);
    if (h == NULL) ret = Val_unit;
    else {
	ret = caml_alloc(1, 0);
	hid = alloc_hid(h);
	Store_field(ret, 0, hid);
    }

    CAMLreturn(ret);
}

CAMLprim value stub_hid_open_path(value path) {
    CAMLparam1(path);
    CAMLlocal2(ret, hid);

    hid_device* h = hid_open_path(String_val(path));
    if (h == NULL) ret = Val_unit;
    else {
	ret = caml_alloc(1, 0);
	hid = alloc_hid(h);
	Store_field(ret, 0, hid);
    }

    CAMLreturn(ret);
}

CAMLprim value stub_hid_write(value dev, value data, value len) {
    return Val_int(hid_write(Hid_val(dev), Caml_ba_data_val(data), Int_val(len)));
}

CAMLprim value stub_hid_read_timeout(value dev, value data, value len, value ms) {
    return Val_int(hid_read_timeout(Hid_val(dev), Caml_ba_data_val(data), Int_val(len), Int_val(ms)));
}

CAMLprim value stub_hid_set_nonblocking(value dev, value nonblock) {
    return Val_int(hid_set_nonblocking(Hid_val(dev), Bool_val(nonblock)));
}

CAMLprim value stub_hid_close(value dev) {
    hid_close(Hid_val(dev));
    return Val_unit;
}

/* --------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  --------------------------------------------------------------------------- */
