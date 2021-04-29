Überblick {#lnk_ueberblick}
==========


Was wird simuliert? 
-------------------

<!-- In der Liste könnte man auch auf die Subpages verlinken 
und die Subpages entsprechend anordnen...-->
Für die Simulation der Stoffumsetzungs-Prozesse, die im fließenden Wasser 
ablaufen, verwendet QSim zwölf Module. Diese sind: 
- Das [Wärmebilanzmodul](\ref lnk_wtemp),
- 7 biochemische Module zur Beschreibung des Seston-Gehalts, 
des [pH-Wertes](\ref lnk_ph), der Nährstoffe ([N](\ref Stickstoff), 
[P](\ref Phosphor), [Si](\ref Silizium)), des [organischen Kohlenstoffs](\ref BSB) 
und des [Sauerstoffgehalts](\ref Sauerstoffgehalt), 
- 3 biologische Module zur Erfassung von [Phyto-](\ref lnk_algendoc) und 
[Zooplankton](\ref lnk_rotatorien) sowie 
[benthischen Filtrierern](\ref Dreissena) und 
- Ein [Sedimentmodul](\ref Sedimentflux) zur Berechnung der frühdiagenetischen 
Prozesse, welche Sauerstoff-, Kohlenstoff- und Nährstoff-Flüsse 
hervorrufen.

Eine kurze Beschreibung dieser Stoffumsetzungs-Module finden Sie in 
<a href="http://bibliothek.bafg.de/webopac/index.asp?detsuche_systematik=online+492" target="_blank">Schöl et al. 2014</a>, 
ausführlichere Informationen sind in den Unterkapitel der einzelnen Module verfügbar siehe Tabelle unten).

<br/><br/>
Wie QSim die von der Strömung des Wassers hervorgerufenen advektiven 
und diffusiven Transportprozesse der o.g. Wasserinhaltstoffe simuliert, 
erläutert der Abschnitt [Stofftransport](#Stofftransport).

<br/><br/>
Eine Einführung in die Gewässergütesimulation finden Sie auf der 
<a href="http://www.bafg.de/DE/08_Ref/U2/01_mikrobiologie/QSIM/qsim_node.html"  target="_blank">
Website der Bundesanstalt für Gewäserkunde</a>, in der 
<a href="http://bibliothek.bafg.de/webopac/index.asp?detsuche_systematik=online+321" target="_blank"> 
QSim Übersicht</a> \ref b und in der 
<a href="./pdf/kurzdoku13_1ber.pdf" target="_blank"> Kurz-Dokumentation 
QSim</a> von Volker Kirchesch.

QSim ist Teil einer Modellfamilie: \subpage lnk_konzept.

Das folgende Kapitel \subpage about_doc gibt ein paar Tipps, wie 
dieses Dokumentationsportal zu lesen und zu verwenden ist.

Im Kapitel \subpage about_tiqusim wird die Entwicklungsgeschichte 
von QSim beschrieben.


| Baustein | QSim Subroutine | Version | 
|----------|-----------------|---------|
| [Sediment-Flüsse](\ref Sedimentflux) | sedflux() |  ### zurückgestellt ### Aug.19 | 
| [Rotatorien](\ref lnk_rotatorien)  | konsum()  | 13.401_15okt18       | 
| [Chorophium (Schlickkrebs)](\ref coroph)       | coroph()  |  ### in Bearbeitung  | 
| [Dreissena polymorpha (Muscheln)](\ref Dreissena)    |   dreissen() |   13.401_15okt18  | 
| [heterotrophe Nanoflagellaten (HNF)](\ref hnf)   |   hnf()	 |   ### inaktiv weil fehlerhaft ### 13.401_15okt18  |
| [(Kiesel-, Grün-, Blau-) Algen](\ref lnk_algendoc) |  algaeski(), algaesgr(), algaesbl()  | 13.401_15okt18  | 
| [benthische Algen](\ref albenth)  |  albenth()	|  13.401_15okt18 |
| [Makrophyten (Wasserpflanzen)](\ref mphyt)  |  mphyt()	|  13.401_15okt18 |
| [organischer Kohlenstoff](\ref BSB)	      |  orgc()	    |  13.401_15okt18 | 
| [Stickstoff](\ref Stickstoff) |  ncyc()	    |  13.401_15okt18 | 
| [pH-Wert](\ref lnk_ph)	  |  ph()	    |  13.401_15okt18 | 
| [Wärmebilanz/Temperatur](\ref lnk_wtemp) 1) +wetter |  temperw() |  13.401_15okt18 | 
| [Phosphor](\ref Phosphor) | po4s()      |  13.401_15okt18 | 
| [Silizium](\ref Silizium) |  silikat()  |  13.401_15okt18 | 
| [Sauerstoffgehalt] (\ref Sauerstoffgehalt) | oxygen() | 13.401_15okt18  | 
| *Schwebstoffe*, Übernahme nicht geplant, ersetzt durch [Verteilung (Schwebstoff, Salz)] (\ref schwebstoff_salz) |  schwebstoff_salz() ersetzt schweb und erosion  |  |
| *Schwermetalle* 	 |  Schwermetalle() |  *externe Entwicklung*  | 
| [Coliforme Bakterien/Keime (Hygiene)] (\ref coliform) |  coliform()      |  13.401_15okt18 |

Die Bausteine sind in der Reihenfolge ihrer Bearbeitung aufgeführt.


Aus Datei: ueberblick-doc.md


