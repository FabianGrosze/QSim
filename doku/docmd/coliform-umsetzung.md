Coliforme Bakterien/Keime (Hygiene) - Umsetzung {#lnk_coliform_umsetzung}
===============================

## Herkunft ##
Programm zur Berechnung der Konzentration E. Coli, faecal coliformer und 
coliformer Bakterien in Fliessgewässer \n
AUTOR:VOLKER KIRCHESCH \n
STAND:15.08.2017   \n



## Schnittstellenbeschreibung ##
SUBROUTINE COLIFORM (\ref coli, \ref doscf, \ref extks, \ref tempw, &
                             \ref rau, \ref tiefe, \ref vmitt, \ref schwi,  &
                             \ref tflie,                             &
                             \ref control, *jjj*)

 
## IT-Realisierung ##

 RateCde Grundmortalitätsrate coliformer Bakterien bei 20°C\n
 etaCde  Temperaturkoeffizient \n
 RateCIe Inaktivierungskoeffizient im Licht"  \n
 xnueCe  dimensionsloser Parameter zur Beschreibung der Inaktiv. im Licht \n
 RateCGe Coliforme Verlustrate durch Grazing \n
 RateCSe Coliforme Verlustrate durch Sedimentation \n


\n\n

Textquelle: coliform-umsetzung.md ; Codesources: coliform.f90 und coliform_huelle.f95 ; 
zurück \ref lnk_coliform
