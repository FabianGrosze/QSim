#! /bin/bash

#! /bin/bash

echo "Das bash-script >>> isoq <<< dient dazu, "
echo "die Visualisierung von mehrdimensionalen Verteilungen von Gütevariablen,"
echo "die von QSim-3D simuliert werden, "
echo "mithilfe eines Visualisierungswerkzeugs wie Paraview oder VisIT"
echo "zu veranlassen und das gewünschte Bild in der Datei isoq.png bereitzustellen."
echo "Als Aufrufparameter müssen das Modellverzeichnis, der Zeitpunkt (in ganzen Simulationssekunden) 
      und der Name der gewünschten Variablen angegeben werden."

# echo "Programmaufruf:  $0"
# echo "Anzahl der Aufrufparameter= $# "
if [ $# -ne 3 ] 
then
   echo "Anzahl der Aufrufparameter= $# ; es müssten aber 3 sein."
   echo "isoq benötigt als Aufrufparameter; Modellverzeichnis, Zeitpunkt und Variablenname"
   exit -1
fi

echo "erster Eingabeparameter Modellverzeichnis:  $1"
DIR="$1"
if [ ! -d $DIR ]
then
  echo -e "Modellverzeichnis >$DIR< existiert nicht"
  exit 2
else
  echo -e "Modellverzeichnis >$DIR< existiert"
fi
echo "zweiter Eingabeparameter Zeitpunkt=  $2"
echo "dritter Eingabeparameter Variable:  $3"

beispiel="/home/Wyrwa/qmodelle/beispielbild.png"
iso="${DIR}${3}${2}isoq.png"
cp $beispiel $iso

echo "Momentan (18jun15 wy) kopiert isoq ein Beispielbild in $iso"
exit 0


# cat tmp list >ls_transinfo
