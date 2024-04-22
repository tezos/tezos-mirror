#!/bin/sh

FILE=script-inputs/active_protocol_versions_without_number

PROTO_CURRENT=$(sed -n '1p' $FILE)
PROTO_NEXT=$(sed -n '2p' $FILE)

echo "PROTO_CURRENT=$PROTO_CURRENT ; export PROTO_CURRENT"
echo "PROTO_NEXT=$PROTO_NEXT ; export PROTO_NEXT"
