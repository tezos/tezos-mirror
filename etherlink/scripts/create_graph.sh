#!/bin/bash

: > data_ticks.tmp
: > data_gas.tmp

dir="$1"
graph_dir="$2"
chunk_size="${3:-30}"

usage() {
  cat << EOF
Usage:
  $0 <csv_directory> <graph_dir> [chunk_size]

Arguments:
  csv_directory     Path to the directory containing CSV logs (e.g., data_dir/kernel_logs).
  graph_dir         Output directory for graph chunks.
  chunk_size        (Optional) Number of blocks per graph. Defaults to 30.
EOF
  exit 1
}

if [ "$#" -lt 2 ]; then
  usage
fi

i=0
find "$dir" -maxdepth 1 -name '*.csv' | sort -V | while IFS= read -r file; do
  [ -f "$file" ] || continue
  echo "Processing: $file"
  base=$(basename "$file")
  name=$(echo "$base" | grep -o '[0-9]\+')

  awk -v i=$i -v name="$name" '
    BEGIN { section = ""; max_tick = 0 }
    /^ticks_used$/ {
      section = "ticks";
      max_tick = 0;
      next
    }
    /^gas_used$/ {
      if (section == "ticks") {
        print i, max_tick, name >> "data_ticks.tmp"
      }
      section = "gas";
      next
    }
    /^[0-9]/ {
      if (section == "ticks" && $1 > max_tick) {
        max_tick = $1
      } else if (section == "gas") {
        print i, $1, name >> "data_gas.tmp"
      }
    }
    END {
      if (max_tick > 0 && section == "ticks") {
        print i, max_tick, name >> "data_ticks.tmp"
      }
    }
  ' "$file"

  i=$((i + 1))
done

mkdir -p "$graph_dir"

max_index=$(awk 'BEGIN {max=0} {if ($1+0 > max) max=$1} END {print max}' data_ticks.tmp)
for ((start = 0; start <= max_index; start += chunk_size)); do
  end=$((start + chunk_size - 1))
  if ((end > max_index)); then end=$max_index; fi

  block_start=$(awk -v idx="$start" '$1 == idx { print $3; exit }' data_ticks.tmp)
  block_end=$(awk -v idx="$end" '$1 == idx { print $3; exit }' data_ticks.tmp)
  output_file="${graph_dir}/blocks_${block_start}_to_${block_end}.png"

  gnuplot -persist << EOF
set terminal pngcairo size 800,600
set output '$output_file'

set xlabel "Replay ID"
set xtics rotate by -45
set xdata
set xtics format ""

set ytics nomirror
set y2tics
set ylabel "Ticks Used (% of 3e10)"
set y2label "Gas Used (% of 3e7)"

set xtics ($(awk -v s="$start" -v e="$end" '$1 >= s && $1 <= e { printf "\"%s\" %d\n", $3, $1 }' data_ticks.tmp | paste -sd, -))

plot \
    "< awk -v s=$start -v e=$end '\$1 >= s && \$1 <= e' data_ticks.tmp" using 1:(\$2 / 30000000000.0 * 100) title 'Ticks (%)' with linespoints pt 7 lc rgb 'blue' axes x1y1, \
    "< awk -v s=$start -v e=$end '\$1 >= s && \$1 <= e' data_gas.tmp" using 1:(\$2 / 30000000.0 * 100) title 'Gas (%)' with linespoints pt 7 lc rgb 'red' axes x1y2

# set xrange [$start:$end]
EOF

done

rm -f data_ticks.tmp data_gas.tmp
