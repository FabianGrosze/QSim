Makrophyten {#lnk_makrophyt}
================

Das Wachstum der Makrophyten wird in QSim 13.4 stark vereinfacht abgebildet. 
Für die Makrophyten als Ganzes (keine Unterscheidung von Wuchsformen) wird ein 
schematischer Jahresgang des Biomasseverlaufs angenommen. Dabei werden der 
Saison-Start und das -Ende sowie die maximal unter optimalen Bedingungen 
erreichbare Biomasse (und die minimale Biomasse außerhalb der Saison) strangweise 
eingegeben, der ideale saisonale Verlauf selbst folgt jedoch einem einfachen, 
vorgegebenen Schema (s.u.).

Relativ zum angenommenen Idealverlauf wird das Wachstum der Makrophyten in 
Abhängigkeit des zur Verfügung stehenden Lichts berechnet, wobei die 
Gesamt-Absorption des Wassers nur scheinbar eine modifizierende Rolle spielt. Für 
die Makrophyten wird dabei eine optimale Lichtintensiät von 60 µE/m²/s und 20 
µE/m²/s als Kompensationslichtintensität angenommen (Setzung im Code).

Makrophyten setzen beim Wachstum im Modell QSim 13.4 Sauerstoff frei, wobei die 
Pflanzenbiomasse und die Lichtintensität berücksichtigt werden. Eine direkte 
Nährstoff- Trübungs- oder Fließgeschwindigeitsabhängigkeit des Makrophytenwachstums 
wird im Modell derzeit jedoch nicht abgebildet. Es sind auch keine direkten 
Interaktionen mit Nährstoffpools, der Sedimentation oder anderen Organismen 
implementiert. 


Details zum Makrophyten-Modul sind in den folgenden Abschnitten beschrieben:

- \subpage lnk_mphyt_prozesse : Erläuterung der im Makrophyten-Baustein 
implementierten Prozesse 

- \subpage lnk_mphyt_vars : Auflistung der verwendeten Formelzeichen und Variablen 

- \subpage lnk_mphyt_umsetzung : Details zum Code und der numerischen Umsetzung 

\n\n

Textquelle: makrophyt-doc.md ; Codesources: mphyt.f90; Vorläuferversion: \subpage mphyt
