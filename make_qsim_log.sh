#!/bin/bash
# -------------------------------------------------------------
# this script executes make [rule] and generates a log file
# -------------------------------------------------------------
# SYNTAX: ./make_qsim_log.sh [subdirectory] [make rule] OR
#         ./make_qsim_log.sh [subdirectory]
# If no make rule is set the default is used.
# ------------------------------------------------------------
if [ $# -gt 2 ] || [ $# -lt 1 ]
then
  echo "Number of arguments must be 1 or 2."
  exit
fi

# change to requested QSim directory
qsimDir=$1
if [ ! -d $qsimDir ]
then
  echo "QSim subdirectory does not exist: ${qimDir}"
  exit
fi
cd $qsimDir

if [ $# -eq 2 ]
then
  make $2 2>&1 | tee ../make_${qsimDir}.log
else
  make    2>&1 | tee ../make_${qsimDir}.log
fi
