#!/bin/bash
#
# Run this script from cmd line and pipe output to txt file
# ./time_seq_curl_calls.sh > time_seq_curl_calls.txt
# will produce a 1 column 300 row matrix of response times.
# NOTE: Modify the following 2 args to fit to your settings.

# url to be tested
testurl=http://localhost:8004/ocpu/library/mylibrary/R/predict/json

# any additional stuff like e.g. args to curl (optional - can be empty)
additonalres="-F arg1=@some_file.json"

for i in `seq 1 300`;
do
 curl -X POST $testurl $additonalres -o /dev/null -s -w %{time_total}\\n
done
