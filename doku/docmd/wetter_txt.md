Wetter.txt {#lnk_wetter_txt}
==========

Die Wetter-Datei definiert für die Wetterstationen des Modells jeweils eine 
Zeitreihe verschiedener meteorologischer Parameter.

Beispiel:\n
<a href="./exp/Wetter.txt" target="_blank">Wetter.txt</a>\n
<a href="./exp/WETTER.txt" target="_blank">WETTER.txt</a>


## Gliederung der Datei

* Kopfbereich
* Detailbereich

Der Detailbereich besteht mehreren Blöcken, je einem pro Wetterstation des 
Modells.\n
Jeder Wetterstationsblock besteht aus einer Kopfzeile und mindestens einer 
Wertzeile.

## Kopfbereich
Der Kopfbereich hat vier Zeilen.

| Position | Inhalt |
| -------- | ------ |
| Kopf, 1.Zeile | \ref lnk_kopfzeile "Versions-Kopfzeile" mit "Wetter" als Dateityp | 
| Kopf, 2.Zeile | Modellname und ggf. weitere Angaben, maximal 255 Zeichen.\n
  Die Zeile wird von QSim-3D nicht ausgewertet. | 
| Kopf, 3.Zeile | Ereignisname und ggf. weitere Angaben, maximal 255 Zeichen.\n
  Die Zeile wird von QSim-3D nicht ausgewertet. | 
| Kopf, 4.Zeile | Zwei Felder, jeweils mit Feldlänge 1.\n
  Das erste Feld gibt die Anzahl der Wetterstationen an, d.h. die Anzahl der Wetterstationsblöcke der Datei.\n
  Das zweite Feld ist ein Schalter für die Zeitbasis der Werte der Wertzeilen:\n
  0 = Werte sind Tageswerte,\n
  1 = Werte sind uhrzeitbezogene Einzelwerte | 


## Detailbereich - Wetterstationsblock - Kopfzeile

Der Wetterstationsblock-Kopf besteht aus einer Zeile mit zwei Feldern.

Das erste Feld gibt die Nummer der Wetterstation an, Feldlänge ist 8.

Die Nummern der Wetterstationen korrespondieren mit den Werten des ersten 
Parameters (WStation) der T-Parametergruppe der \ref lnk_modell_g3 , d.h. der 
Wetterstations-Zuordnung der Zonen des Modells.

Das zweite Feld gibt die Anzahl der Wertzeilen der Wetterstation an, Feldlänge 
ist 5.


## Detailbereich - Wetterstationsblock - Wertzeilen

Auf die Kopfzeilen folgen so viele Wertzeilen, wie im zweiten Feld der Kopfzeile 
des Wetterstationsblocks angegeben.
Jede Wertzeile definiert die meteorologischen Parameter der Wetterstation für 
einen Zeitpunkt.

| Position | Inhalt| 
| Stationsblock-Wertzeile, Felder 1 bis 4| Zeitpunkt der Wertzeile|  
| Stationsblock-Wertzeile, 5.Feld und folgende | Werte aller in <a href="./exp/WetterParam.xml" target="_blank">WetterParam.xml</a> definierten Parameter (ParamSetDef-Element).
  Jedes Feld hat die im Format-Attribut der Parameter-Elemente der Definitionsdatei angegebene Länge.\n
  \n
  Wenn die Wetterdaten als Tageswerte angegeben sind (zweites Feld der vierten Kopfzeile ist 0), ist die Uhrzeit der Wertzeilen irrelevant.
  Maximale und minimale Lufttemperatur des Tages (sechstes und siebtes Feld) sind belegt.\n
  Wenn die Uhrzeitoption gesetzt ist (1), ist nur das erste der beiden Temperaturfelder belegt und gibt die Lufttemperatur zum entsprechenden Zeitpunkt an.
  Das zweite Lufttemperaturfeld wird von QSim-3D ignoriert.
  (Es ist entweder auf den Nullwert gesetzt &ndash; -99.99 &ndash; oder auf den selben Wert wie das erste Temperaturfeld.) |


Textquelle: wetter_txt.md ; Codesources: eingabe.f95 ;  
zurück: \ref lnk_datenmodell
 