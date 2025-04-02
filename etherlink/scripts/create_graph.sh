#!/bin/bash

: > data_ticks.tmp
: > data_gas.tmp

dir="$1"
output="${2:-out.png}"

if [ -z "$dir" ]; then
  echo "Usage: $0 <csv_directory> [output_file.png]"
  exit 1
fi

# Process CSV files
i=0
for file in "$dir"/*.csv; do
  [ -f "$file" ] || continue
  echo "Processing: $file"
  base=$(basename "$file")
  name=$(echo "$base" | grep -o '[0-9]\+')

  awk -v i=$i -v name="$name" '
    BEGIN { section = ""; tick_sum = 0 }
    /^ticks_used$/ { section = "ticks"; next }
    /^gas_used$/ {
        if (section == "ticks") {
            print i, tick_sum, name >> "data_ticks.tmp"
        }
        section = "gas"; next
    }
    /^[0-9]/ {
        if (section == "ticks") {
            tick_sum += $1
        } else if (section == "gas") {
            print i, $1, name >> "data_gas.tmp"
        }
    }
    END {
        if (tick_sum > 0 && section == "ticks") {
            print i, tick_sum, name >> "data_ticks.tmp"
        }
    }
    ' "$file"

  i=$((i + 1))
done

# Plot with gnuplot
gnuplot -persist << EOF
set terminal pngcairo size 800,600
set output '$output'

set xlabel "Replay ID"
set xtics rotate by -45
set xdata
set xtics format ""

set ytics nomirror
set y2tics
set ylabel "Ticks Used (% of 3e10)"
set y2label "Gas Used"

# Map x-index to numeric IDs
set xtics ($(awk '!seen[$1]++ { printf "\"%s\" %d\n", $3, $1 }' data_ticks.tmp | paste -sd, -))

plot \
    'data_ticks.tmp' using 1:(\$2 / 30000000000.0 * 100) title 'Ticks (%)' with linespoints pt 7 lc rgb 'blue' axes x1y1, \
    'data_gas.tmp' using 1:2 title 'Gas' with linespoints pt 7 lc rgb 'red' axes x1y2
EOF

# Open image
if command -v xdg-open > /dev/null; then
  xdg-open "$output"
elif command -v open > /dev/null; then
  open "$output"
else
  echo "Graph saved to $output (could not auto-open)"
fi

# Cleanup
rm -f data_ticks.tmp data_gas.tmp
