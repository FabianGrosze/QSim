Organischer Kohlenstoff {#lnk_orgC}
=======================

Der gesamte organische Kohlenstoffgehalt (TOC, Total Organic Carbon) im Wasser 
setzt sich aus gelösten (dissolved, DOC) und partikulären (POC) Anteilen zusammen. 
Dabei wird beim POC zusätzlich noch zwischen lebender Biomasse und nicht lebender 
organischer Materie (= Detritus) unterschieden. 

Im Modul Organischer Kohlenstoff wird die Aufteilung der organischen Fraktionen 
und deren Verknüpfung untereinander in QSim umgesetzt. Als Kohlenstofffraktionen 
gibt es partikuläre schwer und leicht abbaubare, gelöste schwer und leicht abbaubare 
sowie monomere und refraktäre Kohlenstoff-Verbindungen (BILLEN 1991). Die 
refraktären Kohlenstoffverbindungen stellen dabei lediglich eine Kohlenstoffsenke 
dar. Zur Vervollständigung der Gesamtbilanz des POC wird die lebende Biomasse mit 
einbezogen, die in den Modulen Algen und Rotatorien berechnet wird.

Bei der Berechnung der Konzentrationsänderungen der einzelnen Fraktionen mit 
Ausnahme der monomeren Fraktion werden nach festgelegten Anteilen die abgestorbene 
Algen- und Rotatorienbiomasse sowie die Faecesbildung durch Rotatorien und Dreissena 
als Quellen und die Sedimentation als Senke des Detritus berücksichtigt.

Durch Hydrolyse der partikulären Fraktionen werden Kohlenstoffe in die gelösten 
Fraktionen überführt. In einem weiteren Schritt gelangt dann die gelöste Fraktion 
ebenfalls durch Hydrolyse, aber auch durch exoenzymatische Aktivität in die monomere Fraktion, 
die das Substrat der heterotrophen Atmung der Bakterien ist. Der Umsatz der 
monomeren Kohlenstoffe geschieht durch Bakterien, die unter Sauerstoffverbrauch 
Kohlendioxid und eigene Biomasse bilden. Das bedeutet, dass in diesem Ansatz die 
Bakterienbiomasse (BM) explizit berechnet wird. Aus dem terminalen Schritt wird 
der biochemische Sauerstoffbedarf (BSB) abgeleitet. Die exoenzymatische Aktivität 
im vorherigen Schritt ist an die Konzentration der Bakterienbiomasse gekoppelt. 

<center> 
 \image html orgc.klein.png ""
 <a href="./img/orgc.ppt" target="_blank">Download des Schaubilds als .ppt</a>
 \image latex orgc.png "Bilanz des organischen Kohlenstoffs" width=0.95\textwidth
</center>
<!-- #mf: Frage an Andreas & Tanja: passt das Bild? Bzw. TExt und Bild gegenchecken -->

Details zum Kohlenstoff-Baustein sind in den folgenden Abschnitten beschrieben:
- \subpage lnk_orgC_prozesse : Erläuterung der im Kohlenstoff-Baustein 
implementierten Prozesse 

- \subpage lnk_orgC_vars : Auflistung der verwendeten Formelzeichen und Variablen 

- \subpage lnk_orgc_umsetzung : Details zum Code und der numerischen Umsetzung 

\n\n

Textquelle: kohlenstoff-doc.md ; Codesource: orgC.f90 
