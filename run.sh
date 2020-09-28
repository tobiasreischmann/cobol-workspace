#!/bin/bash
uppername=$(eval echo "$1" | awk '{print toupper($0)}')
eval cobc -x -j $1/src/${uppername}_M.CBL 