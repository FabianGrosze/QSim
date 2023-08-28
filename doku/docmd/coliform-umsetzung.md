Coliforme Bakterien/Keime (Hygiene) - Umsetzung {#lnk_coliform_umsetzung}
===============================

## Herkunft ##
Programm zur Berechnung der Konzentration E. Coli, faecal coliformer und 
coliformer Bakterien in Fliessgewässer \n
AUTOR:VOLKER KIRCHESCH \n
STAND:15.08.2017   \n



## Schnittstellenbeschreibung ##
SUBROUTINE COLIFORM (\ref tiefe,\ref rau,\ref vmitt,\ref vabfl,\ref elen,
\ref flae,\ref flag,\ref tflie
,\ref schwi,\ref ss,\ref zooind,*grote*,\ref chla,\ref tempw,\ref jiein,*ecoli*  &\n
,\ref qeinl, *colil*, *qeinll*,\ref anze,\ref iorla,\ref iorle,\ref ieinls,\ref ilbuhn
,\ref coli,\ref doscf,\ref extks,\ref mstr,\ref azstrs &\n
,*ratecd*, *etacd*, *rateci*, *xnuec*, *ratecg*, *ratecs*, *ifehl* & \n
,\ref kontroll ,\ref iglob )

 
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
