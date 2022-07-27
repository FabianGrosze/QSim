Sauerstoff - Umsetzung {#lnk_sauerstoff_umsetzung}
===============================

## Herkunft ##

Der Sauerstoffgehalt im Wasser wird mit der Subroutine oxygen() bilanziert.\n

Ein Programm zur Berechnung des Sauerstoffgehaltes in Fließgewässern \n
Autor : Volker Kirchesch          \n                                
entnommen aus Version qsim13.301_28mae18\n 

Numerisch wird die Temperatur behandelt wie eine Konzentrationen.
Die im folgenden genannten lokalen Wärmeeinträge und -austräge (Wärmeflüsse) 
sind quasi der *Stoffumsatz* der "Temperatur-Konzentration":\n

## Schnittstellenbeschreibung ##
SUBROUTINE oxygen()\n
( \ref vo2, \ref tempw, \ref rau, \ref vmitt, \ref tiefe, \ref rhyd, \ref flae, \ref tflie, \ref go2n
 , \ref dalgki, \ref dalggr, \ref dalgak, \ref dalgag, \ref akinh4   &\n
 , \ref agrnh4, \ref akino3, \ref agrno3, \ref bsbt, \ref hjo2, \ref flag, \ref elen, \ref ior
 , \ref anze, \ref dzres1, \ref dzres2, \ref hschlr              &\n
 , *eo2*, \ref qeinl, \ref vabfl, \ref po2p, \ref po2r, \ref so2ein, \ref do2o2d, \ref salgo
 , \ref dalgo, \ref dalgao, \ref o2ein1, \ref jiein             &\n
 , \ref opgrmi, \ref opgrma, \ref opkimi, \ref opkima, \ref albewg, \ref alberg, \ref abeowg, \ref abeorg
 , \ref opblmi, \ref opblma, \ref ablnh4        &\n
 , \ref ablno3, \ref dalgbl, \ref dalgab, \ref albewk, \ref alberk, \ref abeowk, \ref abeork, \ref ro2dr
 , \ref wge, *idwe*, \ref fkm, \ref uhrz      &\n
 , \ref zooro2, \ref ro2hnf, \ref ilbuhn, \ref iwied, \ref vo2z, *suso2n*
 , \ref nkzs, \ref dh2d, *o2l*, *qeinll*             &\n
 , \ref iorla, \ref iorle, \ref ieinls, \ref agnh4z, \ref aknh4z, \ref abnh4z, \ref dalgkz, \ref dalgbz
 , \ref dalggz, \ref agno3z, \ref akno3z          &\n
 , \ref abno3z, \ref algakz, \ref algagz, \ref algabz, \ref vz1, \ref tempwz, \ref saett, \ref mstr
 , \ref cpfad, \ref ij, \ref itags, \ref monats             &\n
 , \ref dc_denw, \ref toc_csb, \ref wlage, \ref hws, *etemp*, \ref dh2de, \ref ifehl, \ref ifhstr
 , \ref azstrs                          &\n 
 , \ref zooind, *grote*, \ref iphy, \ref kontroll, *jjj*)\n

<!-- #mf: sind nicht in Aufruf in oxygen.f90 (aber waren hier):
 \ref vnh4, \ref vno3, \ref bsbbet, \ref iglob -->
 
<!-- #mf: bei Variablen mit * funktioniert die Referenz nicht -->

 
## IT-Realisierung ##
Die QSim3D Subroutine oxygen_huelle() ruft die QSim-Subroutine oxygen() auf. \n
Zum Hüllroutinen-Konzept siehe: \ref lnk_huellen

### Berechnungsablauf ###
*zweistufiges Berechnungsverfahren*
In jedem Zeitschritt wird in einer ersten Stufe ein Zwischenwert des 
Sauerstoffgehalts infolge der in \ref lnk_sauerstoff_prozesse aufgeführten 
biochemischen Zehrungs- und Produktionsprozesse berechnet.

In einer zweiten Stufe wird dann ausgehend von dem Zwischenwert die Belüftung 
über die Gewässeroberfläche ermittelt.

### Zeitliche Diskretisierung der Sauerstoffänderung ###
<!-- ehem. Link zur page: diskretO2 -->
Weil die Belüftungsrate vom Sauerstoffgehalt selbst abhängig ist, wird hier eine 
semi-implizite Diskretisierung zur Berechnung des Sauerstoffgehaltes am Ende des
aktuellen Zeitschritts  \f$ {O_2}(t + \Delta t)\f$ verwendet:
\f[ 
  {O_2}(t + \Delta t) - {O_2}(t) = \frac{\widetilde{\Delta O_2} + 
  (b \cdot \Delta_{saett})}{1 + 0.5 \cdot b}
\f]\n 
mit
\f[ 
   b = \Delta t \cdot \frac{ ( k_l + k_w ) \cdot f_{temp} }{h} 
\f] 
<!-- mf: alter Link (würde ich evtl. rauslassen und stattdessen die Formelzeichen
hier auch nochmal erklären: siehe \ref lueftO2\n -->





### Aufruf ###

## Rueckgabewerte/Resultate: ##


&nbsp 
Textquelle: wtemp-umsetzung.md ; Codesources: TEMPERW.f90, temperw_huelle() in temperw_huelle.f95 ;  
zurück: \ref lnk_wtemp