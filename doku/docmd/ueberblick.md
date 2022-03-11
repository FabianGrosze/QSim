Überblick {#lnk_ueberblick}
==========
 
 
Was wird simuliert?
-------------------

<!-- In der Liste könnte man auch auf die Subpages verlinken
und die Subpages entsprechend anordnen...-->
Für die Simulation der einzelnen Stoffumsetzungs-Prozesse, die im fließenden Wasser
ablaufen, verwendet QSim Module. Diese sind:
- Das Modul für den \ref lnk_waerme,
- 6 bio-chemische Module zur Beschreibung von \ref lnk_sauerstoff, und 
  \ref lnk_ph sowie der \ref lnk_naehrstoffe \ref lnk_stickstoff, \ref lnk_phosphor, 
  \ref lnk_silikat und \ref lnk_orgC .
- 3 biologische Module zur Erfassung von \ref lnk_primaer : 
  [Phytoplankton (Algen)](\ref lnk_phytoplankton), 
  [benthischen Algen](\ref lnk_albenth) und 
  [Makrophyten (Wasserpflanzen)](\ref lnk_makrophyt)
- 2 biologische Module zur Erfassung der ersten trophischen Ebene (\ref lnk_konsumenten) :
 [Zooplankton](\ref lnk_rotatorien) sowie 
 [benthischen Filtrierern (Muscheln)](\ref lnk_dreissena)
- Ein [Sedimentmodul](\ref lnk_sediment) zur Berechnung der frühdiagenetischen
 Prozesse, welche Sauerstoff-, Kohlenstoff- und Nährstoff-Flüsse hervorrufen.
- sowie weitere Module für: \ref coliform , \ref lnk_schwermetalle , \ref lnk_schweb und \ref Aufenthaltszeit

Eine kurze Beschreibung dieser Stoffumsetzungs-Module finden Sie in
<a href="http://bibliothek.bafg.de/webopac/index.asp?detsuche_systematik=online+492" target="_blank">Schöl et al. 2014</a>,
ausführlichere Informationen sind in den Unterkapitel der einzelnen Module verfügbar.

Wie QSim die von der Strömung des Wassers hervorgerufenen advektiven
und diffusiven Transportprozesse der o.g. Wasserinhaltstoffe simuliert,
erläutert der Abschnitt [Stofftransport](#Stofftransport).
Auch der Transport eines Tracers, welcher keinem Stoffumsatz unterliegt ist mit QSim möglich.

Eine Einführung in die Gewässergütesimulation finden Sie auf der
<a href="http://www.bafg.de/DE/08_Ref/U2/01_mikrobiologie/QSIM/qsim_node.html" target="_blank">
Website der Bundesanstalt für Gewäserkunde</a>, in der
<a href="http://bibliothek.bafg.de/webopac/index.asp?detsuche_systematik=online+321" target="_blank">
QSim Übersicht</a> und in der
<a href="./pdf/kurzdoku13_1ber.pdf" target="_blank"> Kurz-Dokumentation
QSim</a> von Volker Kirchesch.

QSim ist der Simulationskern für die Gewässergüte.
Es ist eingebunden in eine Software-Umgebung zu der die hydraulischen Treiber, 
die graphische Benutzeroberfläche Gerris u.a. gehören: \subpage lnk_konzept.

Falls Sie selbst mit QSim arbeiten wollen, hilft Ihnen der Abschnitt \ref lnk_download weiter.

Das folgende Kapitel \subpage about_doc gibt ein paar Tipps, wie
dieses Dokumentationsportal zu lesen und zu verwenden ist.

Im Kapitel \subpage about_tiqusim wird die Entwicklungsgeschichte
von QSim beschrieben.


Aus Datei: ueberblick.md ; zurück zu <a href="index.html">main</a>