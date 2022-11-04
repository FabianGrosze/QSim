Phosphor {#lnk_phosphor}
=======================

Die Anwendung von Düngemitteln, Einträge über Kläranlagen, Luftverschmutzungen 
sowie die Degradation entwässerter Feuchtgebiete führen zu überhöhten 
Phosphor-Emissionen in die Gewässer. Ein Teil dieses Phosphors wird im 
Gewässer umgesetzt und/oder zurückgehalten (Retention). Unterschiedliche Prozesse 
können zur Nährstoffretention in Fluss und Aue beitragen. 

Der Pflanzennährstoff Phosphor kommt im Gewässer als anorganischer oder 
organisch gebundener partikulärer oder gelöster Phosphor vor. 

Im Modell wird zwischen Gesamt-Phosphor (Ges-P, Summe aus gelöstem und 
partikulärem Phosphor) und gelöstem reaktivem Phosphat (SRP, für „soluble 
reactive phosphorus“) unterschieden. 

Ges-P und SRP gelangen über Zuflüsse und Einleitungen an den Modellrändern ins 
Gewässer.

Die Änderung des Gesamt-Phosphors erfolgt durch Sedimentation von partikulärem 
gebundenem Phosphor (Algen und Detritus). Der Austausch von anorganischem Phosphor 
(PO4-P) mit dem Sediment ist zurzeit abgeschaltet. 

Während SRP sofort für Algen verfügbar ist, gilt dies für den organisch 
gebundenen Phosphor erst nach dessen bakteriellem Abbau (Respiration der 
heterotrophen Bakterien). Im Modell wird bei der Zersetzung der Algen- und der
Rotatorienbiomasse SRP freigesetzt. Dabei errechnet sich die bei 
den beschriebenen Prozessen freigesetzte Phosphormenge aus den respirierten 
Biomassen und dem Phosphor-Gehalt von Algen und Detritus. 

Details zum Phosphor-Modul sind in den folgenden Abschnitten beschrieben:

- \subpage lnk_phosphor_prozesse : Erläuterung der im Phosphor-Baustein 
implementierten Prozesse 

- \subpage lnk_phosphor_vars : Auflistung der verwendeten Formelzeichen und Variablen 

- \subpage lnk_phosphor_umsetzung : Details zum Code und der numerischen Umsetzung

\n\n

Textquelle: phosphor-doc.md ; Codesource: ncyc.f90 
