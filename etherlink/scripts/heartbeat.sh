#!/bin/bash

if [[ $# -ne 7 ]]; then
  echo 'illegal number of arguments: heartbeat <slack token> <slack channel id> <erc20 contract addr> <secret key 1> <address 1> <secret key 2> <address 2>'
  echo 'both keys should contain tokens of the erc20 contract given'
  exit 1
fi

which curl
if [[ "$?" -eq 1 ]]; then
  echo "missing curl"
  exit 1
fi

which npm
if [[ "$?" -eq 1 ]]; then
  echo "missing npm"
  exit 1
fi

npm list eth-cli
if [[ "$?" -eq 1 ]]; then
  echo "missing eth-cli npm dependency"
  exit 1
fi

SLACK_TOKEN="Authorization: Bearer $1"
SLACK_COVERAGE_CHANNEL="channel=$2"

contract=$3
pk1=$4
addr1=$5
pk2=$6
addr2=$7
oregon=https://node-ore.ghostnet.etherlink.com
dublin=https://node-dub.ghostnet.etherlink.com
node=https://node.ghostnet.etherlink.com

# 3 mn
timeout_time=180

confirmation_blocks=2

report="Report:"

add_report_msg() {
  report+="
• $1"
}

send_msg() {
  msg="text=$1"
  curl --silent \
    -H "${SLACK_TOKEN}" \
    -d "${SLACK_COVERAGE_CHANNEL}" \
    -d "${msg}" \
    -X POST https://slack.com/api/chat.postMessage -o slack-response.json
}

add_timeout_msg() {
  network="$1"
  network_name="${network#https://}"
  network_name="${network_name%.ghostnet.etherlink.com}"
  add_report_msg "<!here> 🚨 *$2 timeouted with endpoint* <${network}|${network_name}>"
}

add_failed_tx_msg() {
  network="$1"
  network_name="${network#https://}"
  network_name="${network_name%.ghostnet.etherlink.com}"
  add_report_msg "<!here> 🚨 *$2 failed with endpoint* <${network}|${network_name}>"
}

add_not_included_tx_msg() {
  hash="${1}"
  add_report_msg "<!here> 🚨 *$3* '${hash}' absent in explorer.* (rpc status_code: '$2')"
}

add_good_health_msg() {
  hash="${1}"
  hash_prefix=$(echo "${hash}" | cut -c 1-18)
  hash_suffix=$(echo "${hash}" | cut -c 63-66)
  network="$2"
  network_name="${network#https://}"
  network_name="${network_name%.ghostnet.etherlink.com}"

  add_report_msg "✅ $3 transaction <https://explorer.etherlink.com/tx/$hash|${hash_prefix}...${hash_suffix}> using <${network}|${network_name}>"
}

check_tx_applied() {
  op_hash=$(echo "$1" | tr -d '"')
  cmd=(-o /dev/null -s -w "%{http_code}\n" -X "GET" "https://explorer.etherlink.com/api/v2/transactions/${op_hash}" -H "accept: application/json")
  echo "> curl" "${cmd[@]}"
  status_code=$(curl "${cmd[@]}")
  echo "${status_code}"
  if [[ "${status_code}" -ne 200 ]]; then
    add_not_included_tx_msg "${op_hash}" "${status_code}"
    return 1
  else
    return 0
  fi
}

submit_tx_and_check() {
  tx_type="$1"
  shift
  cmd=("$@")
  echo "> " "${cmd[@]}"
  op_hash=$(timeout ${timeout_time} "${cmd[@]}")
  res_code=$?
  echo "${op_hash}"
  if [[ "${res_code}" -eq 124 ]]; then
    add_timeout_msg "${network}" "${tx_type}"
  elif [[ "${res_code}" -ne 0 ]]; then
    add_failed_tx_msg "${network}" "${tx_type}"
  else
    check_tx_applied "${op_hash}" "${tx_type}"
    tx_applied=$?
    if [[ "${tx_applied}" -eq 0 ]]; then
      add_good_health_msg "${op_hash}" "${network}" "${tx_type}"
    fi
  fi
}
tx() {
  pk="$1"
  addr="$2"
  network="$3"
  cmd=(npm exec -- eth transaction:send --pk "${pk}" --to "${addr}" --value 1 --network "${network}" --confirmation-blocks="${confirmation_blocks}")
  submit_tx_and_check "Transfer" "${cmd[@]}"
}

allow_erc20_tx() {
  contract="$1"
  pk="$2"
  from="$3"
  network="$4"
  cmd=(npm exec -- eth contract:send "erc20@${contract}" "approve(\"${from}\",100000000000000)" --network "${network}" --pk "${pk}" --confirmation-blocks="${confirmation_blocks}")
  submit_tx_and_check "ERC20 approve" "${cmd[@]}"
}

erc20_tx() {
  contract="$1"
  pk="$2"
  from="$3"
  to="$4"
  network="$5"
  cmd=(npm exec -- eth contract:send "erc20@${contract}" "transferFrom(\"${from}\",\"${to}\",100000000000000)" --network "${network}" --pk "${pk}" --confirmation-blocks="${confirmation_blocks}")
  submit_tx_and_check "ERC20 transferFrom" "${cmd[@]}"
}

allow_erc20_tx "${contract}" "${pk1}" "${addr1}" "${oregon}"
erc20_tx "${contract}" "${pk1}" "${addr1}" "${addr2}" "${oregon}"
add_report_msg "--------------"
allow_erc20_tx "${contract}" "${pk2}" "${addr2}" "${dublin}"
erc20_tx "${contract}" "${pk2}" "${addr2}" "${addr1}" "${dublin}"
add_report_msg "--------------"
tx "${pk1}" "${addr2}" "${node}"
tx "${pk2}" "${addr1}" "${node}"
tx "${pk1}" "${addr2}" "${oregon}"
tx "${pk2}" "${addr1}" "${dublin}"
send_msg "${report}"
