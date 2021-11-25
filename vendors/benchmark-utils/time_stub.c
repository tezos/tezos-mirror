/*****************************************************************************/
/*                                                                           */
/* Open Source License                                                       */
/* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     */
/* Copyright (c) 2018 Nomadic Labs. <nomadic@tezcore.com>                    */
/*                                                                           */
/* Permission is hereby granted, free of charge, to any person obtaining a   */
/* copy of this software and associated documentation files (the "Software"),*/
/* to deal in the Software without restriction, including without limitation */
/* the rights to use, copy, modify, merge, publish, distribute, sublicense,  */
/* and/or sell copies of the Software, and to permit persons to whom the     */
/* Software is furnished to do so, subject to the following conditions:      */
/*                                                                           */
/* The above copyright notice and this permission notice shall be included   */
/* in all copies or substantial portions of the Software.                    */
/*                                                                           */
/* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*/
/* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  */
/* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   */
/* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*/
/* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   */
/* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       */
/* DEALINGS IN THE SOFTWARE.                                                 */
/*                                                                           */
/*****************************************************************************/


#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <time.h>
#include <stdlib.h>

#ifndef __USE_GNU
#define __USE_GNU
#endif

/* #include <sched.h> */

#ifdef __MACH__
  #include <mach/mach_time.h>
#endif

CAMLprim value stub_get_time_ns()
{
    #ifdef __MACH__
      uint64_t t = clock_gettime_nsec_np(CLOCK_UPTIME_RAW);
      return caml_copy_int64(t);
    #else
      struct timespec ts;
      if (clock_gettime(CLOCK_REALTIME, &ts) != 0)
        return caml_copy_int64(0);
      else {
        uint64_t t = (uint64_t)ts.tv_nsec + 1000000000 * (uint64_t)ts.tv_sec;
        return caml_copy_int64(t);
      }
    #endif
}

int flush_cache(size_t cache_size_bytes)
{
     /* using the parameters from 'Numerical Recipes' for our PRNG
      * source wikipedia https://en.wikipedia.org/wiki/Linear_congruential_generator */
     const unsigned int modu  = (unsigned int) cache_size_bytes;
     const unsigned int mult  = 1664525;
     const unsigned int incr  = 1013904223;

     char* buffer = malloc(cache_size_bytes);
     unsigned int index = 0;
     char prev = buffer[0];

     /* We set the number of iterations to cache_size_bytes, maybe that's too much. */
     for(unsigned int i = 0; i < modu; ++i) {
          const char tmp = buffer[index];
          buffer[index] = prev;
          prev = tmp;
          index = (index * mult + incr) % modu;
     }

     free(buffer);

     /* Return prev to create a fake dependency in order to prevent
        "sufficiently smart" compilers transforming this function into a big
        "nop". */

     return (int) prev;
}

CAMLprim value stub_flush_cache(value cache_size_bytes)
{
     CAMLparam1(cache_size_bytes);

     const uint64_t numbytes = Int64_val(cache_size_bytes);

     (void) flush_cache((size_t) numbytes);

     CAMLreturn(Val_unit);
}

/*
int raw_sched_setaffinity(int pid)
{
  cpu_set_t my_set;

  CPU_ZERO(&my_set);
  CPU_SET(pid, &my_set);

  return (int) sched_setaffinity(0, sizeof(cpu_set_t), &my_set);
}
*/

CAMLprim value stub_sched_setaffinity(value pid)
{
  CAMLparam1(pid);

  /* int res = raw_sched_setaffinity(Int_val(pid)); */
  int res = -1;

  CAMLreturn(Val_int(res));
}
