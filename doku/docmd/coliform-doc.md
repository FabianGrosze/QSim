Coliforme Bakterien/Keime (Hygiene) {#lnk_coliform}
===================================

\image html badestelle_rhein.png "Krankheitskeime im Wasser können für badenden 
Menschen gefährlich sein."

## Herkunft
Programm zur Berechnung der Konzentration E. Coli, faecal coliformer und 
coliformer Bakterien in Fliessgewässer \n
AUTOR:VOLKER KIRCHESCH \n
STAND:15.08.2017   \n

## Teilprozesse
Fäkalcoliforme Bakterien vermehren sich im Gewässer nicht, sondern sterben 
rasch ab.\n\n
Berücksichtigt wird dabei:\n
* Grundverlustrate (temperaturabhängig)\n
* Licht-Dosis abhängige Verlustrate\n\n
Nicht gesondert berücksichtigt ist bisher:\n
* Grazing / Sedimentation

## Dokumentation und Veröffentlichungen
<a href="./pdf/18_Becker_ReWaM_intern_BfG_FLUSSHYGIENE_Koblenz26_10_2018_AB_IH.pdf" target="_blank">Modellierung hygienischer Belastungen in Fließgewässern</a>

<a href="./pdf/19_Fischer_SAME16-Potsdam_Hygienemodellierung-Berlin.pdf" 
target="_blank">To swim or not to swim</a>

<a href="./pdf/18_Fischer_FLUSSHYGIENE_Abschluss_final_Mri.pdf" target="_blank">
Simulation von Maßnahmen zur langfristigen Verbesserung der hygienischen 
Wasserqualität</a>

## Schnittstellenbeschreibung / IT-Realisierung
SUBROUTINE COLIFORM (\ref tiefe,\ref rau,\ref vmitt,\ref vabfl,\ref elen,
\ref flae,\ref flag,\ref tflie
,\ref schwi,\ref ss,\ref zooind,*grote*,\ref chla,\ref tempw,\ref jiein,*ecoli*  &\n
,\ref qeinl, *colil*, *qeinll*,\ref anze,\ref iorla,\ref iorle,\ref ieinls,\ref ilbuhn
,\ref coli,\ref doscf,\ref extks,\ref mstr,\ref azstrs &\n
,*ratecd*, *etacd*, *rateci*, *xnuec*, *ratecg*, *ratecs*,\ref ifehl & \n
,\ref kontroll ,\ref iglob )

\ref globaleParameter :\n
 RateCde Grundmortalitätsrate coliformer Bakterien bei 20°C\n
 etaCde  Temperaturkoeffizient \n
 RateCIe Inaktivierungskoeffizient im Licht"  \n
 xnueCe  dimensionsloser Parameter zur Beschreibung der Inaktiv. im Licht \n
 RateCGe Coliforme Verlustrate durch Grazing \n
 RateCSe Coliforme Verlustrate durch Sedimentation \n

Textquelle: coliform-doc.md ; Codesources: coliform_huelle.f95 ;  
zurück: \ref lnk_weitere_stoffe
 