#!/bin/bash

# helper fun to emphasize a string by making it yellow
function make_yellow {
   echo "\e[93m${1}\e[0m"
}

# parse vars loop
for i in "$@"
do
case $i in
   
    --maxreq=*)
    MAXREQ="${i#*=}"
    shift
    ;;
    
    --concmin=*)
    CONCMIN="${i#*=}"
    shift
    ;;

    --concmax=*)
    CONCMAX="${i#*=}"
    shift
    ;;

    --concby=*)
    CONCBY="${i#*=}"
    shift
    ;;

    --testurl=*)
    TESTURL="${i#*=}"
    shift
    ;;

    *)
    ;;
esac
done

# construct array of concurrencies
CONCS=$(seq ${CONCMIN} ${CONCBY} ${CONCMAX})

# execute js script for each 
for conc in ${CONCS}
do
  # maybe add check if colours defined in all universally possible terminals? 
  echo -e "Latency test on '${TESTURL}' with $(make_yellow ${conc}) concurrent calls until ${MAXREQ} total requests fired."
  
  node latency_test_js.js ${MAXREQ} ${conc} ${TESTURL}
done

