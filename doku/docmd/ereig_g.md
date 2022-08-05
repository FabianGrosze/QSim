EreigG.txt  {#lnk_ereig_g}
==========

In der Datei EreigG.txt wird das zu simulierende Ereignis definiert.
Das Ereignis umfasst einen definierten Zeitraum.
Die Simulation erfolgt in äquidistanten Zeitschritten, deren Schrittweite und 
Anzahl hier angegeben ist.

Für jede Randbedingung gemäß 
<a href="./exp/points" target="_blank">points-Datei</a> werden ein Werte-Satz 
(konstante Verhältnisse im ganzen Ereignis) oder mehrere Werte-Sätze angegeben 
(Ganglinie).

Die Ganglinien können ein unregelmäßiges Zeitschema haben.
Das Zeitschema der verschiedenen Randbedingungen kann verschieden sein.
QSim-3D interpoliert fehlende Zeitschritte.

Beispiel:\n
<a href="./exp/EREIGG.txt" target="_blank">EREIGG.txt</a>


## Gliederung der Datei

* Kopfbereich
* Detailbereich

Der Detailbereich besteht mehreren Blöcken, je einem pro Randbedingung des 
Modells.

Jeder Randbedingungsblock besteht aus einer Kopfzeile und mindestens einer 
Wertzeile.

## Kopfbereich
Der Kopfbereich hat sechs Zeilen.

| Position |Inhalt |
| Kopf, 1. Zeile | \ref lnk_kopfzeile "Versions-Kopfzeile" mit "EreigG" als Dateityp |
| Kopf, 2. Zeile | Modellname und ggf. weitere Angaben, maximal 255 Zeichen.\n
  Die Zeile wird von QSim-3D nicht ausgewertet. |
| Kopf, 3. Zeile | Ereignisname und ggf. weitere Angaben, maximal 255 Zeichen.\n
  Die Zeile wird von QSim-3D nicht ausgewertet. |
| Kopf, 4. Zeile | Datum und Uhrzeit des Ereignisstarts in vier Feldern. |
| Kopf, 5. Zeile | Datum und Uhrzeit des Ereignisendes in vier Feldern.\n
  In einem fünften Feld folgt die Zeitschrittweite der Berechnung in Minuten. |
| Kopf, 6. Zeile | Verschiedene Optionen und Steuerwerte der QSim-Berechnung, wie im Folgenden beschrieben. |

### Kopfbereich - Optionszeile
Bis auf das 10. Feld haben alle Felder die Länge 1.

| Position | Inhalt | 
| -------- | ------ | 
| 6.Kopfzeile, 1.Feld | Ausgabe-Option, für QSim-3D ohne Bedeutung. |
| 6.Kopfzeile, 2.Feld | Option pH-Berechnung: 0 = nein, 1 = ja. |
| 6.Kopfzeile, 3.Feld | Option Dispersionskoeffizient: 1 = Einlesen, 0 = Berechnen. |
| 6.Kopfzeile, 4.Feld | Option Temperaturberechnung: 1 = nur Temperaturberechnung, 0 = alle Berechnungen. |
| 6.Kopfzeile, 5.Feld | Option Tracer: 1 = nur Tracerberechnung, 0 = alle Berechnungen. |
| 6.Kopfzeile, 6.Feld | Option Erosion: 1 = mit Erosion, 0 = ohne Erosion. |
| 6.Kopfzeile, 7.Feld | Option Abweichung, für QSim-3D ohne Bedeutung. |
| 6.Kopfzeile, 8.Feld | Nummer der zu verwendenden Transportgleichung gemäß den in <a href="./exp/EreigGParam.xml"
target="_blank">EreigGParam.xml</a> definierten (TpEquations-Element).\n
iverfahren Adv.Diff. 1-cip, 2-Lax_wendroff, 3-ultimate Quickest  |
| 6.Kopfzeile, 9.Feld | Nummer der zu verwendenden Methode zur Berechnung des longitudinalen Dispersionskoeffizienten
  gemäß den in <a href="./exp/EreigGParam.xml" target="_blank">EreigGParam.xml</a> definierten (DlEquations-Element).\n
  Wird von QSim-3D nur berücksichtigt, wenn der Schalter für den Dispersionskoeffizienten (Feld 3) auf 0 gesetzt ist.\n
 ilongDis long.Disp.koeff. 1-Deng 2-Li 3-Iwasa 4-Elder |
| 6.Kopfzeile, ;10.Feld | Gewichtungsfaktor im Format F4.2 für die Berechnung des longitudinalen Dispersionskoeffizienten (0 bis 1).\n
  Wird von QSim-3D nur berücksichtigt, wenn der Schalter für den Dispersionskoeffizienten (Feld 3) auf 0 gesetzt ist.\n
  FlongDis |
| 6.Kopfzeile, ;11.Feld | iColi 1-Gewässerhygiene-berechnung |
| 6.Kopfzeile, ;12.Feld | ikonsS ??\n |
| 6.Kopfzeile, ;13.Feld | iSchwer ??\n |
| 6.Kopfzeile, ;14.Feld | iphy Verfahren für die Oberflächenbelüftung oxygen() 1-Kirchesch,2-Kirchesch,3-Wolf,4-Melching |


## Detailbereich - Randbedingungsblock - Kopfzeile
Der Randbedingungsblock-Kopf besteht aus einer Zeile mit folgenden Feldern:

| Position | Inhalt |
| -------- | ------ |
| Randb.block-Kopfzeile, 1.Feld | Nummer der Zone der Randbedingung gemäß <a href="./exp/points" target="_blank">points-Datei</a>, Feldlänge ist 5. |
| Randb.block-Kopfzeile, 2.Feld | Nummer der Randbedingung gemäß points-Datei, Feldlänge ist 5 |
| Randb.block-Kopfzeile, 3.Feld | Art der Eingabewerte der Randbedingung (Feldlänge 1):\n
  0 = Tagesmittelwerte,\n
  1 = uhrzeitbezogene Einzelwerte |
| Randb.block-Kopfzeile, 4.Feld | Anzahl der Werte der Randbedingung im Simulationszeitraum:\n
  1 = konstante Randbedingung über den gesamten Zeitraum,\n
  2 = Zeitreihe;\n
  Der Wert ist gleichzeitig die Anzahl der folgenden Wertzeilen im Randbedingungsblock. |


## Detailbereich - Randbedingungsblock - Wertzeilen

Auf die Kopfzeilen folgen so viele Wertzeilen, wie im vierten Feld der Kopfzeile 
des Randbedingungsblocks angegeben.
Jede Wertzeile definiert die Parameterwerte der Randbedingung für einen Zeitpunkt.

| Position | Inhalt | 
| Randb.block-Wertzeile, Felder ;1 bis 4 | Zeitpunkt der Wertzeile | 
| Randb.block-Wertzeile, 5. ;Feld | In QSim der Q-Wert der Randbedingung zum angegebenen Zeitpunkt, in QSim-3D nicht benutzt und auf 0 gesetzt.
  Die Feldlänge ist 13. | 
| Randb.block-Wertzeile, 6. ;Feld und folgende|Parameterwerte aller in <a href="./exp/EreigGParam.xml" target="_blank">EreigGParam.xml</a> definierten Parameter (ParamSetDef-Element).
  Jedes Feld hat die im Format-Attribut der Parameter-Elemente der Definitionsdatei angegebene Länge. | 


Textquelle: ereig_g.md ; Codesources: eingabe.f95 ;  
zurück: \ref lnk_datenmodell
