#! /bin/bash

#echo "qsim3d_gerris.bash is going to start QSim3D on voss-cln-preprocess->vhpc01head.bafg.de qsub-torque Batch Queue"
echo "qsim3d_gerris.bash is going to start QSim3D on voss-cln-preprocess-> slurm Batch Queue"

# echo "Anzahl der Aufrufparameter= $# "
if [ $# -lt 1 ]
then
  echo "Number of parameter when invoking qsim3d is only $# ; at least one, the Modell-Directory is needed."
  exit -1
fi

DIR="$1"
if [ ! -d $DIR ]
then
  echo -e "Modell-Directory >$DIR< does not exist"
  exit -2
else
  echo -e "Modell-Directory >$DIR< exists"
fi

echo $PATH
export PATH

cd $DIR
pwd -P

# SLURM :
/usr/bin/sbatch qsim3d.sbatch

# Submit the first job and save the JobID as JOBONE
# J1ID=$(qsub qsim3d_starter.qsub)
# # J1ID=$(/opt/torque-6.1.2-client/bin/qsub qsim3d_starter.qsub)
# # echo "first Job ID=$J1ID going to do computation"
# Submit the second job, use JOBONE as depend, save JobID
# J2ID=$(qsub -W depend=afterany:$J1ID@192.168.54.254 publi)
# # J2ID=$(/opt/torque-6.1.2-client/bin/qsub -W depend=afterany:$J1ID@192.168.54.17 publi)
# # echo "second Job ID=$J2ID waiting to clear results"

echo "qsim3d_gerris.bash done"

exit 0
