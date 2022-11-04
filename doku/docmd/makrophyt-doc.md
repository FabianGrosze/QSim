Makrophyten {#lnk_makrophyt}
================

\warning Momentan ist das Modul ausgeschaltet, das heißt, Makrophyten werden 
nicht simuliert. Es handelt sich um eine alte Dokumentation.


Das Wachstum der Makrophyten wird in QSim stark vereinfacht abgebildet. 
Für die Makrophyten als Ganzes (keine Unterscheidung von Wuchsformen) wird ein 
schematischer Jahresgang des Biomasseverlaufs angenommen. Dabei werden der 
Saison-Start und das -Ende sowie die maximal unter optimalen Bedingungen 
erreichbare Biomasse (und die minimale Biomasse außerhalb der Saison) strangweise 
eingegeben, der ideale saisonale Verlauf selbst folgt jedoch einem einfachen, 
vorgegebenen Schema (s.u.).

Relativ zum angenommenen Idealverlauf wird das Wachstum der Makrophyten in 
Abhängigkeit des zur Verfügung stehenden Lichts berechnet, wobei die 
Gesamt-Absorption des Wassers nur bedingt eine modifizierende Rolle spielt. 
Es wird für die Makrophyten eine feste optimale Lichtintensiät als 
Kompensationslichtintensität angenommen.

Im Modell setzen Makrophyten beim Wachstum Sauerstoff frei, wobei die 
Pflanzenbiomasse und die Lichtintensität sowie bei der Sauerstofffreisetzung 
auch ein Temperaturfaktor berücksichtigt werden. 

Eine direkte Nährstoff- Trübungs- oder Fließgeschwindigeitsabhängigkeit des 
Makrophytenwachstums wird im Modell derzeit jedoch nicht abgebildet. 
Es sind auch keine direkten Interaktionen mit Nährstoffpools, der Sedimentation 
oder anderen Organismen implementiert. 

Ein derzeit deaktivierter Abschnitt hat früher noch - rudimentär - das 
Wachstum der Aufwuchsalgen beschrieben (siehe Details).

\note Der derzeitige Makrophytenbaustein ist eher rudimentär und bildet 
hauptsächlich die Sauerstoffproduktion durch Makrophyten dort, wo sie eigegeben 
wurden, vereinfacht ab.


Details zum Makrophyten-Modul sind in den folgenden Abschnitten beschrieben:

- \subpage lnk_makrophyt_prozesse : Erläuterung der im Makrophyten-Baustein 
implementierten Prozesse 

- \subpage lnk_makrophyt_vars : Auflistung der verwendeten Formelzeichen und Variablen 

- \subpage lnk_makrophyt_umsetzung : Details zum Code und der numerischen Umsetzung 

\n\n

Textquelle: makrophyt-doc.md ; Codesources: mphyt.f90;
zurück zu \ref lnk_ueberblick
