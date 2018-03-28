#!/bin/bash

# Run this script from cmd line and pipe output to txt file
# ./time_seq_curl_calls.sh > time_seq_curl_calls.txt
# will produce a 1 column 300 row matrix of response times.

for i in `seq 1 300`;
do
 curl -X POST http://192.168.1.207/ocpu/library/hellotripv2/R/predict/json -F "lookup_result=@lookup_result.json" -o /dev/null -s -w %{time_total}\\n
done
