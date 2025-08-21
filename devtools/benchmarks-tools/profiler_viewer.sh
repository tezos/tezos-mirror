#!/usr/bin/env bash

##############################################################################
#                                                                            #
# SPDX-License-Identifier: MIT                                               #
# SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>       #
#                                                                            #
##############################################################################

# Translate a profiler text output to json

# From:
# 2024-12-16T22:40:45.015-00:00
# prepare_data ................................................................... 1         1368.105ms 100%
# 2024-12-16T22:40:46.383-00:00
# lwt_worker_lwt_handlers ........................................................ 1         2327.835ms 100%
#   BLS .......................................................................... 1         1677.783ms 100%
#   Ed255519 ..................................................................... 1           87.102ms  99%
#   P256 ......................................................................... 1          507.328ms 100%
#   Secp256k1 .................................................................... 1           55.509ms 100%
# 2024-12-16T22:40:48.711-00:00
# eio_worker_lwt_handlers ........................................................ 1         2545.143ms 102%
#   BLS .......................................................................... 1         1719.494ms 101%
#   Ed255519 ..................................................................... 1          144.398ms 108%
#   P256 ......................................................................... 1          563.150ms 102%
#   Secp256k1 .................................................................... 1          117.906ms 108%
# 2024-12-16T22:40:51.260-00:00
# eio_worker_eio_handler_1_domains ............................................... 1         2272.277ms 102%
#   BLS .......................................................................... 1         1620.084ms 101%
#   Ed255519 ..................................................................... 1           87.296ms 113%
#   P256 ......................................................................... 1          505.604ms 102%
#   Secp256k1 .................................................................... 1           59.059ms 121%
# 2024-12-16T22:40:53.532-00:00
# eio_worker_eio_handler_2_domains ............................................... 1         1153.309ms 204%
#   BLS .......................................................................... 1          823.329ms 202%
#   Ed255519 ..................................................................... 1           44.293ms 220%
#   P256 ......................................................................... 1          255.167ms 204%
#   Secp256k1 .................................................................... 1           30.097ms 234%
# 2024-12-16T22:40:54.685-00:00
# eio_worker_eio_handler_4_domains ............................................... 1          580.687ms 403%
#   BLS .......................................................................... 1          415.767ms 400%
#   Ed255519 ..................................................................... 1           22.650ms 414%
#   P256 ......................................................................... 1          126.214ms 407%
#   Secp256k1 .................................................................... 1           15.149ms 452%
# 2024-12-16T22:40:55.266-00:00
# eio_worker_eio_handler_8_domains ............................................... 1          299.904ms 798%
#   BLS .......................................................................... 1          211.680ms 801%
#   Ed255519 ..................................................................... 1           12.684ms 774%
#   P256 ......................................................................... 1           65.167ms 805%
#   Secp256k1 .................................................................... 1            8.375ms 802%
# 2024-12-16T22:40:55.566-00:00
# eio_worker_eio_handler_16_domains .............................................. 1          161.059ms 1541%
#   BLS .......................................................................... 1          108.325ms 1588%
#   Ed255519 ..................................................................... 1            7.336ms 1490%
#   P256 ......................................................................... 1           35.012ms 1557%
#   Secp256k1 .................................................................... 1            5.665ms 1426%

# To:
# [{ "section" : "prepare_data", "count": 1, "duration":1368.105, "cpu":100, "sub":[]},
#  { "section":"lwt_worker_lwt_handlers", "count": 1, "duration":2327.853, "cpu":100" "sub":[
#     { "section":"BLS", "count": 1, "duration":1677.783, "cpu":100},
#     { "section":"Ed255519 ", "count": 1, "duration":87.102, "cpu":99},
#     { "section":"P256", "count": 1, "duration":507.328, "cpu":100},
#     { "section":"Secp256k1", "count": 1, "duration":55.509, "cpu":100}
#   ]},
# ...
# ]

# The input is a text file with the profiler output
# The output is a json file with the profiler output

if [ "$#" -ne 3 ]; then
  echo "Usage: $0 <input> <output> <vega_lite_file>"
  exit 1
fi

input=$1
output=$2

echo "[" > "$output"

# sections starts with a timestamp
# sub-sections are indented
# all lines have the same format : "section_name ... count duration cpu%"

section=""
sub_section=""

while IFS= read -r line; do
  # is it a section?
  if [[ $line =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{3}-[0-9]{2}:[0-9]{2}$ ]]; then
    # if we have a section, close it
    read -r line
    if [ -n "$section" ]; then
      # if no sub-section, copy the section
      if [ -z "$sub_section" ]; then
        echo "  { \"subsection\" : \"$section\", \"count\": $count, \"duration\":\"$duration\", \"cpu\":$cpu }" >> "$output"
      fi
      echo "  ]}," >> "$output"
    fi
    # start a new section
    sub_section=""

    # shellcheck disable=SC2086
    {
      section=$(echo $line | cut -d' ' -f1)
      count=$(echo $line | cut -d' ' -f3)
      duration=$(echo $line | cut -d' ' -f4 | sed 's/ms//')
      cpu=$(echo $line | cut -d' ' -f5 | sed 's/%//')
    }
    echo "  { \"section\" : \"$section\", \"count\": $count, \"duration\":\"$duration\", \"cpu\":$cpu, \"sub\":[" >> "$output"
  else
    # is it a sub-section?
    if [[ $line =~ ^\ +[A-Za-z0-9_]+ ]]; then
      # if we have a sub-section, close it
      if [ -n "$sub_section" ]; then
        echo "    ," >> "$output"
      fi
      # start a new sub-section
      # shellcheck disable=SC2086
      {
        sub_section=$(echo $line | cut -d' ' -f1)
        count=$(echo $line | cut -d' ' -f3)
        duration=$(echo $line | cut -d' ' -f4 | sed 's/ms//')
        cpu=$(echo $line | cut -d' ' -f5 | sed 's/%//')
      }
      echo "    { \"subsection\" : \"$sub_section\", \"count\": $count, \"duration\":\"$duration\", \"cpu\":$cpu }" >> "$output"
    fi

  fi

done < "$input"

# close the last section
echo "  ]}" >> "$output"

echo "]" >> "$output"

# using this json produce a vegalite graph with each section as a bar and each sub-section as a stacked bar
# the duration is the size of the bar, color is the sub-section
# the x-axis is the section, the y-axis is the duration

# shellcheck disable=SC2154
cat > "$3" << EOF
{
  "$schema": "https://vega.github.io/schema/vega-lite/v5.json",
  "width":600,
  "height":400,
  "data": {
    "values": $(cat "$output")
  },
  "transform" : [
    {
      "flatten": ["sub"]
    }
  ],
  "params": [

    {"name": "select",
     "select": {"type": "point", "fields": ["sub.subsection"]}}

  ],
  "mark": {
    "type": "bar",
    "stroke": "black",
    "cursor": "pointer",
    "tooltip": true
  },
  "encoding": {
    "y": {
    "aggregate": "sum",
      "field": "sub.duration",
      "type": "quantitative",
      "axis": {
        "title": "Duration (ms)"
      },
      "sort": "y"
    },
    "x": {
      "field": "section",
      "type": "nominal",

      "axis": {
        "title": "Profiling section",
        "labelAngle": -30,
        "labelAlign": "right",
        "labelfonSize":25
      },
      "sort": "y"
    },
     "tooltip": [
      {"field": "section", "type": "nominal", "title": "Section"},
      {"field": "sub.subsection", "type": "nominal", "title": "Profiling function"},
      {"field": "sub.duration", "type": "quantitative", "title": "Duration (ms)"},
      {"field": "sub.cpu", "type": "quantitative", "title": "CPU %"},
      {"field": "sub.count", "type": "quantitative", "title": "Count"}
   ],
    "fillOpacity": {
      "condition": {"param": "select", "value": 1},
      "value": 0.3
    },
    "opacity": {
      "condition": {"param": "select", "value": 1},
      "value": 0.2
    },
    "color": {
      "field": "sub.subsection",
      "type": "nominal",
      "legend": {
        "title": "Profiling function"
      },
      "scale": {
        "scheme": "set3"
      },
      "strokeWidth": {
           "condition": [
             {
              "param": "select",
              "value": 1
             }
             ],
          "value": 0
    }
    }
    }
  }

EOF

# if vl-convert is available, convert the vega-lite to a url

if command -v vl-convert &> /dev/null; then
  echo -e "\033[33mTo display and edit the graph, open in your local browser:\033[0m"
  printf "%s/view\n" "$(vl-convert vl2url --input "$3")"
  echo "Generating out.svg file"
  vl-convert vl2svg --input "$3" --output ./out.svg
else
  echo "Install vl-convert using \"cargo install vl-convert\" to generate fancy outputs"
fi

exit 0
