#include <time.h>
#include <stdlib.h>
#include <stdio.h>

#ifndef __USE_GNU
#define __USE_GNU
#endif

#include <sched.h>

int get_res(clockid_t clk_id)
{
     struct timespec ts;

     if (clock_getres(clk_id, &ts) != 0) {
          abort();
     } else {
          int t = (int)ts.tv_nsec + 1000000000 * (int)ts.tv_sec;
          return t;
     }
}

int get_time(clockid_t clk_id)
{
     struct timespec ts;

     if (clock_gettime(clk_id, &ts) != 0) {
          abort();
     } else {
          int t = (int)ts.tv_nsec + 1000000000 * (int)ts.tv_sec;
          return t;
     }
}

double get_time_lln(clockid_t clk_id, int nsamples)
{
     int start, stop;
     double* array;

     array = malloc(sizeof(double) * nsamples);

     if(!array) {
          printf("get_time_lln: error");
          abort();
     }

     for(int i = 0; i < nsamples; i++) {
          start = get_time(clk_id);
          stop  = get_time(clk_id);
          array[i] = (double) (stop - start);
     }

     double mean = 0.0;
     double rcp  = 1.0 / ((double) nsamples);

     for(int i = 0; i < nsamples; i++)
          mean += array[i];

     free(array);

     return (mean * rcp);
}

int main()
{
     int res, start, stop;

     res = get_res(CLOCK_REALTIME);
     start = get_time(CLOCK_REALTIME);
     stop = get_time(CLOCK_REALTIME);

     printf("realtime: res = %d, min dt = %d, avg = %f\n", res, stop - start, get_time_lln(CLOCK_REALTIME, 10000));

     res = get_res(CLOCK_MONOTONIC);
     start = get_time(CLOCK_MONOTONIC);
     stop = get_time(CLOCK_MONOTONIC);

     printf("monotonic: res = %d, min dt = %d\n", res, stop - start);

     res = get_res(CLOCK_PROCESS_CPUTIME_ID);
     start = get_time(CLOCK_PROCESS_CPUTIME_ID);
     stop = get_time(CLOCK_PROCESS_CPUTIME_ID);

     printf("cputime_id: res = %d, min dt = %d\n", res, stop - start);
}
