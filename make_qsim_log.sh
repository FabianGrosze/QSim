#!/bin/bash
#-----------------------------------------------------------------#
# This script executes make [make rule] and generates a log file. #
#-----------------------------------------------------------------#
# SYNTAX: ./make_qsim_log.sh [subdirectory] [make rule] OR        #
#         ./make_qsim_log.sh [subdirectory]                       #
# NOTE: If no make rule is set, 'clean_release' is used creating  #
#       an executable compiled from a clean compilation state.    #
#-----------------------------------------------------------------#

# check number of input arguments
if [ $# -gt 2 ] || [ $# -lt 1 ]
then
  echo "Number of arguments must be 1 or 2."
  exit
fi

# check for requested QSim directory
qsimDir=$1
if [ ! -d $qsimDir ]
then
  echo "QSim subdirectory does not exist: ${qimDir}"
  exit
fi

# change to requested directory
cd $qsimDir

# set make rule
if [ $# -eq 2 ]
then
  makeRule=$2
else
  makeRule=clean_release
fi

# compile QSim and generate log file
make $makeRule 2>&1 | tee ../make_${qsimDir}.log
