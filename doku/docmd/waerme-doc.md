Wärmehaushalt {#lnk_waerme}
=============

![](Waermehaushalts.png "")

Die Temperaturverteilung im Wasserkörper ist ein Resultat der Wärmebilanz.

Im Modell sind die folgenden lokalen Wärmeeinträge und -austräge 
(Wärmeflüsse) implementiert:

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
(*Wärmeleitung zwischen Wasseroberfläche Luft*)
- Wärmeleitung Sediment <br>
(*Aufwärmung des Sediments durch einfallende Strahlung, bzw. 
Wärmeabgabe vom Sediment in den Wasserkörper)

**Wärmeentstehung** im Wasserkörper etwa durch biologische Prozesse 
(Respiration) oder Reibungswärme ist sehr gering und wird vernachlässigt.
D.h. die Wärmebilanz ist von anderen Stoffumsetzungsprozessen in guter 
Näherung unabhängig. 

Ein- und Austrag von Wärme ins Wasser erfolgt demnach nur infolge der 
\ref wetter_rb.

Die folgenden beiden Unterkapitel beschreiben die Berechnung
- der \subpage lnk_strahlung und
- der \subpage lnk_wtemp


zurück: \ref lnk_ueberblick

aus Datei waerme-doc.md
