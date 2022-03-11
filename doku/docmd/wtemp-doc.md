Wassertemperatur {#lnk_wtemp}
================

Die Wassertemperatur ergibt sich aus der Wärmebilanz eines Gewässers. Sie ist
ein wichtiger Faktor für die Gewässergüte, da sie Stoffumsatzprozesse und damit
z.B. chemische Reaktionsraten oder Wachstumsraten von Organismen beeinflusst.

Im BfG-Gewässergütemodell werden folgende Prozesse zur Berechnung der Wassertemperatur berücksichtigt:

* [Strahlung](\ref lnk_strahlung);
* [Verdunstung](\ref lnk_verdunstung);
* [Konvektion](\ref lnk_konvektion); 
* Direkter Wärmeaustausch mit dem Sediment;
* Aufwärmung des Gewässerbodens durch die, das Wasser durchdringende Strahlung;
* Aufwärmung des Wasserkörpers durch die am Gewässerboden reflektierte Strahlung;
* [Zuflüsse und linienförmige oder punktuelle Einleitungen.](\ref lnk_waermeeinleitung)

<!-- #mf: prüfen, dass auch tatsäächl. alle im Wtemp-Code auftauchen und verwendet werden + 
ob hier in Liste etwas fehlt -->

An Wetterdaten werden für die Modellierung des Wärmehaushalts neben der
Strahlung Tagesmittelwerte der relativen Luftfeuchte, der Windgeschwindigkeit 
und des Bedeckungsgrades sowie 
Tageswerte der minimalen und maximalen Lufttemperatur benötigt.

Weitere Details zum Wassertemperatur-Modul sind in den folgenden Abschnitten 
beschrieben: 
- \subpage lnk_wtemp_prozesse
- \subpage lnk_wtemp_vars
- \subpage lnk_wtemp_umsetzung


Textquelle: wtemp-doc.md ; Codesources: TEMPERW.f90, temperw_huelle.f95 ;
Vorläufer: \subpage Waermebilanz;  zurück: \ref lnk_waerme
