Stickstoff {#lnk_stickstoff}
=======================

Die Anwendung von Düngemitteln, Einträge über Kläranlagen, Luftverschmutzungen 
sowie die Degradation entwässerter Feuchtgebiete führen zu überhöhten 
Stickstoff-Emissionen in die Gewässer. Ein Teil dieses Stickstoffs wird im 
Gewässer umgesetzt und/oder zurückgehalten (Retention). Unterschiedliche Prozesse 
können zur Nährstoffretention in Fluss und Aue beitragen. Dabei wird zwischen 
einem temporären Rückhalt und der dauerhaften Entfernung aus dem Ökosystem 
unterschieden. 

In QSim werden die Änderung von Gesamt-Stickstoff, Ammoniumstickstoff, 
Nitritstickstoff und Nitratstickstoff durch Einleitungen und Umsatzprozesse 
berechnet. Zu Beginn jeder Simulation wird die Gesamt-Stickstoffkonzentration am 
oberen Rand des Simulationsgebietes sowie der Einleiter und der Nebengewässer um 
die in Algen- und Zooplanktonbiomasse gebundenen Anteile sowie um die anorganischen
Stickstoffkomponenten reduziert. Der restliche Stickstoff ist der organische Anteil 
des Detritus und der gelösten organischen Verbindungen.

Für die N-Retention werden in QSim die folgenden Prozesse berücksichtigt: Die 
Sedimentation des organischen Materials abzüglich der durch sessile Mineralisierung 
wieder freigesetzten gelösten N-Komponenten (NH4-Flux aus dem Sediment, dieser führt 
zu negativen Retentionswerten) sowie die durch benthische Nitrifikation und/oder 
Denitrifikation entstehenden NO3-Fluxe in oder aus dem Sediment. QSim enthält auch 
die Retention über benthische Filtrierer (Aufnahme in benthische Biomasse). 

Weitere Veränderungen auf der Fließstrecke erfahren der Gesamt-Stickstoffgehalt 
sowie der anorganische Stickstoff durch punktförmige und diffuse Einleitungen. 
Ebenso wird mit den suspendierten Nitrifikanten (Nitrosomonas und Nitrobakter) 
verfahren, die zur Beschreibung der Nitrifikation in der fließenden Welle benötigt 
werden. Ihr Wachstum wird in Abhängigkeit von den Milieubedingungen berechnet 
(sessile Nitrifikation siehe Sedimentbaustein). Dabei ist die lokale Wachstumsrate 
der Nitrifikanten abhängig von der vorherrschenden Wassertemperatur, dem aktuellen 
Sauerstoffgehalt und dem Angebot an Ammoniumstickstoff (Nitrosomonas) und 
Nitritstickstoff (Nitrobakter). Außerdem wird die Hemmung des Wachstums durch die 
pH-abhängigen Konzentrationen an Ammoniak (wirkt auf die Wachstumsrate der 
Nitrobakter) und salpetriger Säure (wirkt auf die Wachstumsrate der Nitrosomonas) 
berücksichtigt. Ferner wird die Konzentrationsänderung der einzelnen 
Organismengruppen durch Sedimentation und Mortalität berechnet.

Bei der Bilanz des Ammoniumstickstoffs werden neben der bakteriellen Oxidation 
von Ammonium noch andere Prozesse berücksichtigt. Diese umfassen die 
Ammoniumaufnahme durch Algen und Makrophyten sowie die Ammonium-Nachlieferung 
beim Abbau organischen Materials. Außerdem wird in QSim der 
Ammonium-Austauschprozess mit dem Sediment berücksichtigt. 
Bei der Veränderung des Nitratstickstoffs dient die Oxidation des Nitrits als 
Quellterm, die Aufnahme durch Algen und Makrophyten als Senkenterm. Der Austausch 
mit dem Sediment kann je nach Situation entweder als Quell- oder als Senkenterm 
auftreten.


Details zum Stickstoff-Modul sind in den folgenden Abschnitten beschrieben:

- \subpage lnk_stickstoff_prozesse : Erläuterung der im Kohlenstoff-Baustein 
implementierten Prozesse 

- \subpage lnk_stickstoff_vars : Auflistung der verwendeten Formelzeichen und Variablen 

- \subpage lnk_stickstoff_umsetzung : Details zum Code und der numerischen Umsetzung 

\n\n

Textquelle: stickstoff-doc.md ; Codesource: ncyc.f90 ; Vorläuferversion: \subpage Stickstoff
<!-- #mf: auch hier die Vorläuferversion evtl. löschen, weil Text übernommen 
(und erweitert) wurde, aber nicht wesentlich verändert -->
