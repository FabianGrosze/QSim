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

Für die N-Retention werden in QSim momentan nur die 
Sedimentation des organischen Materials und die Retention über benthische 
Filtrierer (Aufnahme in benthische Biomasse) berücksichtigt.
Eine Freisetzung von gelösten N-Komponenten (NH4-Flux aus dem Sediment, dieser führt 
zu negativen Retentionswerten) sowie die durch benthische Nitrifikation und/oder 
Denitrifikation entstehenden NO3-Fluxe in oder aus dem Sediment werden momentan
nicht berücksichtigt. 

Weitere Veränderungen auf der Fließstrecke erfahren der Gesamt-Stickstoffgehalt 
sowie der anorganische Stickstoff durch punktförmige und diffuse Einleitungen. 
Suspendierte Nitrifikanten (Nitrosomonas und Nitrobakter) 
werden zur Beschreibung der Nitrifikation in der fließenden Welle benötigt. 
Ihr Wachstum wird in Abhängigkeit von den Milieubedingungen berechnet. 
Dabei ist die lokale Wachstumsrate 
der Nitrifikanten abhängig von der vorherrschenden Wassertemperatur, dem aktuellen 
Sauerstoffgehalt und dem Angebot an Ammoniumstickstoff (Nitrosomonas) und 
Nitritstickstoff (Nitrobakter). Außerdem wird die Hemmung des Wachstums durch die 
pH-abhängigen Konzentrationen an Ammoniak (wirkt auf die Wachstumsrate der 
Nitrobakter) und salpetriger Säure (wirkt auf die Wachstumsrate der Nitrosomonas) 
berücksichtigt. Ferner wird die Konzentrationsänderung der einzelnen 
Organismengruppen durch Sedimentation und Mortalität berechnet.

Bei der Bilanz des Ammoniumstickstoffs werden neben der bakteriellen Oxidation 
von Ammonium noch andere Prozesse berücksichtigt. Diese umfassen die 
Ammoniumaufnahme durch Algen sowie die Ammonium-Nachlieferung 
beim Abbau organischen Materials. 
Bei der Veränderung des Nitratstickstoffs dient die Oxidation des Nitrits als 
Quellterm, die Aufnahme durch Algen als Senkenterm.


Details zum Stickstoff-Modul sind in den folgenden Abschnitten beschrieben:

- \subpage lnk_stickstoff_prozesse : Erläuterung der im Kohlenstoff-Baustein 
implementierten Prozesse 

- \subpage lnk_stickstoff_vars : Auflistung der verwendeten Formelzeichen und Variablen 

- \subpage lnk_stickstoff_umsetzung : Details zum Code und der numerischen Umsetzung 

\n\n

Textquelle: stickstoff-doc.md ; Codesource: ncyc.f90
