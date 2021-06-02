Wassertemperatur {#lnk_wtemp}
================

Im BfG-Gewässergütemodell werden folgende Prozesse zur Berechnung der Wassertemperatur berücksichtigt:

* [Strahlung](\ref Strahlung);
* [Verdunstung](\ref lnk_verdunstung);
* Konvektion; 
* Direkter Wärmeaustausch mit dem Sediment;
* Aufwärmung des Gewässerbodens durch die, das Wasser durchdringende Strahlung;
* Aufwärmung des Wasserkörpers durch die am Gewässerboden reflektierte Strahlung;
* linienförmige und punktuelle Einleitungen.

An Wetterdaten werden für die Modellierung des Wärmehaushalts neben der
Strahlung Tagesmittelwerte der relativen Luftfeuchte, der Windgeschwindigkeit und des Bedeckungsgrades sowie 
Tageswerte der minimalen und maximalen Lufttemperatur benötigt.


Weitere Details zum Wassertemperatur-Modul sind in den folgenden Abschnitten 
beschrieben: 
- \subpage lnk_wtemp_prozesse
- \subpage lnk_wtemp_vars
- \subpage lnk_wtemp_umsetzung

Veröffentlichungen/weitere Dokumentation
----------------------------------------

Bisher existiert eine Dokumentation des Temperatur-Moduls als Kapitel 6 der 
<a href="./pdf/QSimDoku_ncycWy.pdf" target="_blank">Kurzdoku</a> (Version vom 22. Nov. 2017)

Eine Modellbeschreibung in englischer Sprache ist im Anhang D des IKSR Berichts 
<a href="http://bibliothek.bafg.de/index.asp?detsuche_systematik=online+280" target="_blank">
Estimation of the effects of climate change scenarios on future Rhine water temperature development </a> zu finden.

Die ausführliche Beschreibung dieses Modellbausteins einschließlich der Angabe sämtlicher Formeln findet sich in der\n <a href="./pdf/Temperatur_Doku_Volker.pdf" target="_blank">Dokumentation Temperatur</a> von Volker Kirchesch \n


zurück: \ref lnk_ueberblick; Code in Datei TEMPERW.f90 und temperw_huelle.f95

aus Datei wtemp-doc.md
