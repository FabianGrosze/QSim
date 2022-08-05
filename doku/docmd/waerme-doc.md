Wärmehaushalt {#lnk_waerme}
=============

![](Waermehaushalts.png "")

Die Wärmebilanz eines Gewässers bestimmt die Temperaturverteilung im 
Wasserkörper.

Zur Berechnung der Wärmebilanz, sind im Modell die folgenden lokalen 
Wärmeeinträge und -austräge (Wärmeflüsse) implementiert:

- Globalstrahlung <br> 
(*Sonneneinstrahlung, Licht*)
- Ausstrahlung <br> 
(*langwellige Wärmestrahlung des Wasserkörpers nach oben*)
- Gegenstrahlung <br> 
(*langwellige Wärmestrahlung der Atmosphäre ins Gewässer*)
- Verdunstung/Kondensation <br> 
(*abhängig von der Dampfdruckdifferenz zwischen 
Wasseroberfläche und darüberliegender Luft verdunstet Wasser aus dem 
Wasserkörper in die Luft oder kondensiert aus der Luft in den Wasserkörper.
Dabei wird dem Wasser entweder Verdunstungswärme entzogen oder 
Kondensationswärme zugeführt.*)
- Konvektion <br>
(*Wärmeleitung zwischen Wasseroberfläche und Luft*)
- Wärmeleitung Sediment <br>
(*Aufwärmung des Sediments durch einfallende Strahlung, bzw. 
Wärmeabgabe vom Sediment in den Wasserkörper*)
- Zuflüsse/Einleitungen <br>
(*Natürliche Zuflüsse und anthropogene Wärmeeinleitungen als linienförmige
oder punktuelle Einleitungen*)

Wärmeentstehung im Wasserkörper etwa durch biologische Prozesse 
(Respiration) oder Reibungswärme ist sehr gering und wird vernachlässigt.
D.h. die Wärmebilanz ist von anderen als oben aufgelisteten 
Stoffumsetzungsprozessen in guter Näherung unabhängig. 

Ein- und Austrag von Wärme ins Wasser erfolgt demnach nur infolge der 
\ref lnk_wetter_rb und der [Zuflussranddaten](\ref lnk_randbedingungen).
<!-- #mf: prüfen, ob Link zu zuflussranddaten (Zufluss-RBs) korrekt ist -->

Die folgenden beiden Unterkapitel beschreiben die Berechnung
- der \subpage lnk_strahlung und
- der \subpage lnk_wtemp


Textquelle: waerme-doc.md ; codesources: temperw.f90, temperw_huelle.f95  ; 
zurück: \ref lnk_ueberblick
