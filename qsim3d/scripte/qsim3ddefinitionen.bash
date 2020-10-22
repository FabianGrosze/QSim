#! /bin/bash

echo "bash-script >>> qsim3ddefinitionen <<< stellt Definitionsdateien bereit," 
echo "die an der Schnittstelle von Qsim3d und Gerris benötigt werden. Dies sind:"
echo "AParamParam.xml , Definition welche globalen Modell-Parameter in der aktuellen QSim(1D und 3D)-Version Verwendung finden."
echo "ausgabekonzentrationen_beispiel.txt , enthält eine Liste der Namen der Modell-Variablen, die von der aktuellen QSim-3D Version ausgegeben werden können"
echo " desweiteren: EreigGParam.xml, ModellG3Param.xml, WetterParam.xml"

# echo "Anzahl der Aufrufparameter= $# "
if [ $# -ne 1 ]
then
  echo "Anzahl der Aufrufparameter ist leider $# ; genau einer, das Modellverzeichnis, wird benötigt."
  exit -1
fi

DIR="$1"
if [ ! -d $DIR ]
then
  echo -e "Modellverzeichnis >$DIR< existiert nicht"
  exit 2
else
  echo -e "Modellverzeichnis >$DIR< existiert"
fi

# echo "hier müsste das script jetzt was machen"
P="/home/Wyrwa/qmodelle/definitionen/AParamParam.xml"
cp $P $DIR
B="/home/Wyrwa/qmodelle/definitionen/ausgabekonzentrationen_beispiel.txt"
cp $B $DIR
E="/home/Wyrwa/qmodelle/definitionen/EreigGParam.xml"
cp $E $DIR
M="/home/Wyrwa/qmodelle/definitionen/ModellG3Param.xml"
cp $M $DIR
W="/home/Wyrwa/qmodelle/definitionen/WetterParam.xml"
cp $W $DIR

echo "qsim3ddefinitionen regulär beendet." 
exit 0
