Strahlung {#lnk_strahlung}
=========

Licht, oder besser die Strahlung, ist ein zentraler Faktor für den Wärmehaushalt
eines Gewässers. Die Globalstrahlung spielt aber auch für die Primärproduzenten 
und damit die Produktivität eines Gewässers eine zentrale Rolle.

Die Strahlung wird im BfG-Gewässergütemodell zur Simulation folgender 
Prozesse benötigt:

- Wärmehaushalt des Fließgewässers <br>
 *(Wellenlänge 290 - 4000 nm)*;
- Wachstum der Algen und Makrophyten durch Photosynthese <br>
 *(Wellenlänge 400 – 700 nm)*;
- Photolytisches Absterben der fäkalcoliformen Bakterien <br>
 *(Wellenlänge 290 - 380 nm)*. 

Die einzelnen Strahlungskomponenten werden in QSim aus der Globalstrahlung 
und der Wolkenbedeckung berechnet. 
Die Globalstrahlung und die Wolkenbedeckung basieren auf Messdaten und werden als 
Randbedingung in das Modell eingespeist. 

Details zur Berechnung der Strahlungskomponenten, sowie zur Umsetzung im 
Programmcode sind in den folgenden Abschnitten beschrieben: 
- \subpage lnk_strahlung_prozesse
- \subpage lnk_strahlung_vars
- \subpage lnk_strahlung_umsetzung

Textquelle: strahlung-doc.md; Codesources: strahlg.f90, sasu.f95; zurück: \ref lnk_waerme

<!-- #mf: noch etwas die Prozesse oben ausführen? Evtl. noch die verschiedenen
Strahlungskomponenten unterscheiden und kurz erläutern, dafür evtl. allg. Text aus dem 
nächsten Reiter rausziehen -->
