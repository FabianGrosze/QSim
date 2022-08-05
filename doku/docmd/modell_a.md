ModellA.txt {#lnk_modell_a}
===========

Die Datei ModellA.txt definiert für QSim1D alle Stränge des eindimensionalen 
Modells mit ihren Stationen, Wehren und Randbedingungen.

In QSim3D enthält die Datei lediglich die Kopfzeilen und die Angabe einer 
geographischen Position des Modells.

Beispiel:

<a href="./exp/MODELLA.txt" target="_blank">MODELLA.txt</a>


| Zeile   | Inhalt |
| ------- | ------ |
| Zeile 1 | Versions-Kopfzeile\n Die Zeile wird von Gerris benutzt und von QSim-3D nicht ausgewertet. |
| Zeile 2 | Modellname und ggf. weitere Angaben, maximal 255 Zeichen.\n Die Zeile wird von Gerris benutzt und von QSim-3D nicht ausgewertet. |
| Zeile 3 | Für das Modellgebiet repräsentative geographische Breite und Länge, jeweils als Dezimalgrad und im Format F5.2. |

Textquelle: modell_a.md ; Codesources: eingabe.f95 ;  
zurück: \ref lnk_datenmodell
 