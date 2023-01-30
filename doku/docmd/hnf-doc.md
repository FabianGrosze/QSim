Heterotrophe Nanoflagellaten (HNF)  {#lnk_hnf}
==================================

\warning Momentan ist das Modul ausgeschaltet, das heißt, HNF werden nicht
simuliert.

Heterotrophe Nanoflagellaten (HNF) sind einzellige tierische Organismen mit 
einer Zellgröße von 2 bis 20 µm, die sich von Bakterien ernähren. HNF können 
neben Ciliaten und Rotatorien entscheidend die Zooplanktongemeinschaft in 
großen Flüssen prägen (Bergfeld et al. 2009). In QSim wird das mikrobielle 
Nahrungsnetz modelliert, indem die Entwicklung von heterotrophen Nanoflagellaten 
(HNF) und ihr Einfluss auf Bakterien integriert wurden. Die Chrysomonadengattung 
Spumella wurde als Modellorganismus für HNF ausgewählt, weil sie die dominante 
Gattung im Rhein ist (WEITERE & ARNDT 2002, PRAST ET AL. 2003). Die Modellierung 
der HNF-Biomasse ist in Formel [32] dargestellt. Die Grazingrate der HNF durch 
Metazoen und benthische Filtrierer wird aus dem filtrierten Wasservolumen pro 
Zeitschritt analog zur Grazingrate der Algen berechnet.  Es wird die Annahme 
getroffen, dass alle im filtrierten Wasservolumen enthaltenen Beuteorganismen 
aufgenommen werden ohne eine Präferenz für bestimmte Beuteorganismen.


Weitere Details zum Baustein sind in den folgenden Abschnitten
beschrieben:
- \subpage lnk_hnf_prozesse : Erläuterung der im Baustein A implementierten Prozesse
- \subpage lnk_hnf_vars : Auflistung der verwendeten Formelzeichen und Variablen
- \subpage lnk_hnf_umsetzung : Details zum Code und der numerischen Umsetzung


\n\n

Textquelle: hnf-doc.md ; Codesource: hnf.f90

