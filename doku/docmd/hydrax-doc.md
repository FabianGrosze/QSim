Hydrax {#lnk_hydrax}
=========

Der Gütemodellierung mit QSim1D ist eine hydraulische Modellierung mit dem 
hydrodynamischen Modell HYDRAX vorgeschaltet. HYDRAX ist ein eindimensionales 
instationäres hydrodynamisches Modell für die Berechnung der Wasserstände, 
Abflüsse und Fließgeschwindigkeiten in Fließgewässern (Oppermann 1989, 
Oppermann et al. 2015). Der Kern des Programms besteht in der numerischen 
Lösung der Saint Venant Gleichungen mit dem impliziten Differenzenschema nach 
Preismann (CUNGE 1980). Das nichtlineare Gleichungssystem wird iterativ mit 
2 verschiedenen Double Sweep Techniken für baum- und netzartige Gewässergrafen 
gelöst (CUNGE 1980, FRJASINOV 1970). Während mit dem Gauß’schen Lösungsverfahren 
die Rechenzeit mit der 3. Potenz der Ortspunktanzahl wächst, steigt der 
Zeitbedarf durch die Ausnutzung der Bandmatrixstruktur bei der Double Sweep 
Methode nur linear mit der Ortspunktanzahl. Gleichzeitig reduziert sich der 
Speicherplatzbedarf erheblich. Neben den beiden Graflösungen für rückgekoppelte 
Modelltypen gibt es noch eine Graflösung für rückwirkungsfreie bzw. durch die 
Vorgabe einer unteren Randbedingung entkoppelte Teilgrafen. Sie verbindet die 
Teilgrafen bilanzmäßig. Ergänzt wird das hydrodynamische Modell durch Wehr- und 
Polderbausteine. Ein Wehr ist zeit-, abfluss- oder wasserstandsabhängig 
steuerbar. Gewässerstrecken können in Abhängigkeit vom Wasserstand oder Abfluss 
zu- oder abgeschaltet werden. Die Nutzung verschiedener Arten von Randbedingung 
ermöglicht eine flexiblere Anpassung an die verfügbaren Daten. 

Das Programm HYDRAX kann sowohl instationär (Modul: HYDRAEXZ) als auch 
stationär (Modul: HYDRAEXS) betrieben werden. Das Programm HYDRAX liegt als 
eine Microsoft Visual C Version vor. Die Ergebnisse der Wassermengensimulation 
werden direkt als Eingabedaten von QSim benutzt. Die Dokumentation der 
aktuellen HYDRAX- Version 5 ist verfügbar unter DOI: 10.5675/HYDRAX.

<!-- #todo: checke Link zu Hydrax-Doku -->

Textquelle: hydrax-doc.md ; 
zurück zu: \ref lnk_qsim_aufbau oder \ref lnk_ueberblick
