#!/bin/bash

# Stop sccache and print stats in a pre-collapsed section of the job log.

echo -e "\e[0Ksection_start:$(date +%s):sccache_stop[collapsed=true]\r\e[0KStop sccache"
sccache --stop-server || true
echo -e "\e[0Ksection_end:$(date +%s):sccache_stop\r\e[0K"
