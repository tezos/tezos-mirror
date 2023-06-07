#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <time.h>

#ifdef __MACH__
  #include <mach/mach_time.h>
  #define CLOCK CLOCK_UPTIME_RAW
#else
  #define CLOCK CLOCK_MONOTONIC
#endif                      

CAMLprim int64_t caml_clock_gettime(value _useless)
{
    #ifdef __MACH__
      uint64_t t = clock_gettime_nsec_np(CLOCK);
      return ((int64_t) t);
    #else
      struct timespec ts;
      if (clock_gettime(CLOCK, &ts) != 0)
        return ((int64_t) 0);
      else {
        uint64_t t = (uint64_t)ts.tv_nsec + 1000000000 * (uint64_t)ts.tv_sec;
        return ((int64_t) t);
      }
    #endif
}

CAMLprim value caml_clock_gettime_byte(value useless)
{
     return caml_copy_int64(caml_clock_gettime(useless));
}

CAMLprim int64_t caml_clock_getres(value _useless)
{
    struct timespec ts;

    if ( clock_getres(CLOCK, &ts) != 0 ) {
        return (int64_t) 0;
    } else {
        uint64_t t = (uint64_t)ts.tv_nsec + 1000000000 * (uint64_t)ts.tv_sec;
        return (int64_t) t;
    }
}

CAMLprim value caml_clock_getres_byte(value useless)
{
     return caml_copy_int64(caml_clock_getres(useless));
}
