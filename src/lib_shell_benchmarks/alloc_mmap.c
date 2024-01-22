#include <string.h>
#include <sys/types.h>
#include <sys/mman.h>

#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

CAMLprim value caml_alloc_by_mmap(value size)
{
    CAMLparam1(size);
    CAMLlocal1(pair);

    size_t len = Long_val(size);

    void *p;

    p = mmap(NULL, len, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);

    if (!p)
    {
        caml_failwith("caml_alloc_by_mmap: memory over");
    }

    memset(p, 0, len);

    pair = caml_alloc_small(2,0);
    Store_field(pair, 0, (value)p);
    Store_field(pair, 1, size);

    CAMLreturn(pair);
}

CAMLprim void caml_free_by_mmap(value pair)
{
    CAMLparam1(pair);
    
    munmap((void *)Field(pair,0), Long_val(Field(pair, 1)));

    CAMLreturn0;
}
