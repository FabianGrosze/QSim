Phosphor {#lnk_phosphor}
=======================

Der Pflanzennährstoff Phosphor kommt im Gewässer als anorganischer oder 
organisch gebundener partikulärer oder gelöster Phosphor vor. 

Im Modell wird zwischen Gesamt-Phosphor (Summe aus gelöstem und partikulärem 
Phosphor) und ortho-Phosphat unterschieden. Zu Beginn jeder Simulation wird die 
Gesamtphosphorkonzentration am oberen Modellrand des Simulationsgebietes und der 
Einleiter und Nebengewässer um die in Bakterien-, Algen- und Rotatorienbiomasse 
gebundenen Anteile sowie um den ortho-Phosphat-Anteil reduziert. Der restliche 
Phosphor entspricht dem organischen Anteil des Detritus und der gelösten 
organischen Verbindungen. Die Änderung des Gesamt-Phosphors erfolgt durch 
Sedimentation von partikulärem organisch gebundenem Phosphor (Bakterien, Algen 
und Detritus) und dem Austausch von anorganischem Phosphor (PO4-P) mit dem 
Sediment. 

Während ortho-Phosphat sofort für Algen verfügbar ist, gilt dies für den organisch 
gebundenen Phosphor erst nach dessen bakteriellem Abbau (Respiration der 
heterotrophen Bakterien). Im Modell wird bei der Zersetzung der Bakterien-, Algen-, 
Rotatorien- und Dreissenabiomasse ortho-Phosphat freigesetzt. Ortho-Phosphat in 
durch Rotatorien und Dreissena als Faeces ausgeschiedener Algenbiomasse wird 
ebenfalls erst beim bakteriellen Abbau freigesetzt. Dabei errechnet sich die bei 
den beschriebenen Prozessen freigesetzte Phosphormenge aus den respirierten 
Biomassen und dem Phosphor-Gehalt von Bakterien, Algen und Detritus. 

<!-- #mf: 2. Ebene noch aus word doc übernehmen + Kommentare 0. Ebene durchgehen
und einarbeiten -->

Details zum Phosphor-Modul sind in den folgenden Abschnitten beschrieben:

- \subpage lnk_phosphor_prozesse : Erläuterung der im Phosphor-Baustein 
implementierten Prozesse 

- \subpage lnk_phosphor_vars : Auflistung der verwendeten Formelzeichen und Variablen 

- \subpage lnk_phosphor_umsetzung : Details zum Code und der numerischen Umsetzung 

\n\n

Textquelle: phosphor-doc.md ; Codesource: ncyc.f90 ; 
Vorläuferversion: \subpage Phosphor
<!-- #mf: auch hier die Vorläuferversion evtl. löschen, weil Text übernommen 
(und erweitert) wurde, aber nicht wesentlich verändert -->
