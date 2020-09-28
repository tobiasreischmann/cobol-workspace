#!/bin/bash
uppername=$(eval echo "$1" | awk '{print toupper($0)}')
eval /home/cobol/cobol-unit-test/run-ut -p $1 ${uppername}_C ${uppername}_M ${uppername}_T