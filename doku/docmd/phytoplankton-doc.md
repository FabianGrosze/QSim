Phytoplankton {#lnk_phytoplankton}
================

Unter Phytoplankton werden die im Wasser suspendierten Algen verstanden, denen 
als wichtigste Primärproduzenten besondere Bedeutung im Gewässer zukommt.  Algen 
besitzen die Fähigkeit zur sauerstoffbildenden Photosynthese, bei der 
Kohlendioxid als Kohlenstoffquelle, Licht als Energiequelle und Wasser als 
Elektronendonator (Reduktionsmittel) dient.  Die Photosynthese lässt sich 
vereinfacht mit folgender Summenformel beschreiben:

\f[
 6\,\text{CO}_2 + 6\,\text{H}_2\text{O} \rightarrow \text{C}_6\text{H}_{12}\text{O}_6 + 6\,\text{O}_2 \quad \text{--2902\,kJ}
\f]

Primärproduzenten stehen am Anfang der Nahrungskette und bilden die 
Nahrungsgrundlage für planktische und benthische herbivore Tiere und für die am 
biochemischen Abbau beteiligten Bakterien und Pilze (Destruenten).  Zum anderen 
sind sie wesentliche Lieferanten des für die meisten Organismen 
lebens¬notwendigen Sauerstoffs.
Die Biomasse und das Wachstum des Phytoplanktons spielen in Fließgewässern für 
das Nahrungsnetz und dessen Abbildung im Modell prinzipiell eine große Rolle. 
Damit kann ein wesentlicher Anteil der Primärproduktion und damit der Energie- 
und Stoffflüsse in Zusammenhang stehen, wenn das abgebildete Gewässer 
planktonführend ist 
<a href="https://www.gewaesser-bewertung.de/index.php?article_id=460&clang=0}{www.gewaesser-bewertung.de"> Website: Typologie nach WRRL, 03.12.2021</a>. 
Dies trifft vor allem auf Gewässer mit großem Einzugsgebiet, insbesondere auf 
langsam fließende und/oder staugeregelte Gewässer zu.\par

Für die Photosynthese des Phytoplanktons ist nur ein Teil der sichtbaren 
Strahlung nutzbar, deren Anteil wird im Modell letztlich aus der Globalstrahlung 
abgeleitet. Die Berechnung des Unterwasserlichtklimas ist oft ein wesentlicher 
Teil des Phytoplanktonbausteins von Gütemodellen, eine Abhängigkeit des 
Lichtspektrums von der Wassertiefe wird in der vorliegenden Modellversion -- der 
Einfachheit halber -- jedoch nicht (mehr) berücksichtigt.\par

Die Photosyntheserate und die Wachstumsrate der Algen hängen in QSim von der 
Temperatur, der Lichtintensität und dem Nährstoffangebot ab. Vereinfachend wird 
im Modell eine Wachstumsrate proportional zur Photosyntheserate angenommen. In 
Abhängigkeit von der Lichtintensität werden bei der Photosyntheserate 
($P$-$I$-Kurve) im Modell drei Bereiche unterschieden. Im lichtlimitierten 
Bereich steigt die Photosyntheserate mit zunehmender Lichtintensität fast linear 
an, im lichtgesättigten Bereich konvergiert sie gegen ihr Maximum, bei hohen 
Lichtintensitäten nimmt die Photosyntheserate wieder ab (Lichthemmung bzw. 
Photoinhibition). Der beschriebene Zusammenhang der Lichtabhängigkeit des 
Wachstums wird im Modell algengruppen-spezifisch abgebildet.\par

Es wird im Modell des Weiteren angenommen, dass die am häufigsten in der 
Algenbiomasse vorkommenden Elemente Kohlenstoff (C), Sauerstoff (O\f$_2\f$) und 
Wasserstoff (H) unbegrenzt zur Verfügung stehen, während die Nährstoffe 
Stickstoff (N), Phosphor (P) und bei Kieselalgen auch Silizium (Si) das Wachstum 
limitieren können.\par

Für den Baustoffwechsel und den Betriebsstoffwechsel *(Nennt man 
die wirklich so? Besonders ersteres klingt seltsam.)* wird vom Phytoplankton 
Energie aus dem Abbau organischer Kohlenstoffverbindungen unter Verbrauch von 
Sauerstoff benötigt. Im Modell wird dabei zwischen Grundrespiration und 
lichtabhängiger Respiration unterschieden.\par

Zusätzlich zur Respiration erfahren Algen im Modell externe Verluste durch 
Zooplankton und Zoobenthosorganismen (*Dreissena, *Corophium*), 
Sedimentation und Mortalität infolge Ressourcenmangel und letalen physikalischen 
bzw. chemischen Umweltbedingungen.\par

Bei der Beschreibung der Photosynthese wird der Begriff Brutto-Photosynthese 
hier für die primäre Kohlenstofffixierung insgesamt genutzt. Als 
Netto-Photosynthese wird die Brutto-Photosynthese abzüglich der internen 
respiratorischen Verluste und der Mortalität bezeichnet 
*(Wirklich inklusive der Mortalität? Das höre ich so zum ersten 
Mal.)*, dabei bleiben die in anderen Modulen berücksichtigten externen Verluste 
außen vor.\par

In QSim werden drei Algenklassen unterschieden, die -- nach BfG Erfahrung 
*(Das würde ich so nicht schreiben.)* -- in den großen 
Fließgewässern Deutschlands in der Regel am bedeutsamsten sind: Chlorophyceae 
(Grünalgen), Bacillariophyceae (Kieselalgen bzw. Diatomeen) und Cyanophyceae 
(Blaualgen bzw. Cyanobakterien). In der vorliegenden QSim-Version werden alle 
oben genannten Prozesse für Kieselalgen, Grünalgen und Blaualgen in getrennten 
Modulen mit gleichem Aufbau simuliert, eine Umstrukturierung zur Vermeidung von 
Redundanzen ist in Arbeit.\par

Als Biomasseparameter für die quantitative Eingabe des Phytoplanktons wird 
Chlorophyll-a (Chl-a) verwendet, da es gebräuchlich und relativ einfach zu 
bestimmen ist. Die prozentuale Algengruppenzusammensetzung (bezogen auf Chl-a) 
kann durch direkte Messung (Sonden) oder durch Umrechnung der 
mikroskopisch-quantitativen Daten ermittelt werden. 

Details zum Algen-Modul sind in den folgenden Abschnitten beschrieben:

- \subpage lnk_phyto_prozesse : Erläuterung der im Algen-Baustein 
implementierten Prozesse 

- \subpage lnk_phyto_vars : Auflistung der verwendeten Formelzeichen und Variablen 

- \subpage lnk_phyto_umsetzung : Details zum Code und der numerischen Umsetzung 

\n\n

Textquelle: phytoplankton-doc.md ; Codesources: albenth.f90, algaesbl.f90,
algaesgr.f90
