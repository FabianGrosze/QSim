Silikat - Umsetzung {#lnk_si_umsetzung}
========================= 

## Herkunft ##
silikat() \n 
EIN PROGRAMM zu Berechnung des geloesten Silikats    \n
AUTOR : VOLKER KIRCHESCH                      \n
entnommen aus Version qsim13.301_28mae18\n 

## Schnittstellenbeschreibung ##
see silicate.f90

<!--
SUBROUTINE silikat()\n
( \ref si, \ref flag, \ref elen, \ref ior, *esi*, *qeinl*, \ref vabfl, \ref anze, \ref tflie, \ref jiein, \ref aki         &\n
, \ref albewk, \ref alberk, \ref tiefe, \ref tempw, \ref ilbuhn, \ref akkssi, \ref qmx_sk, \ref q_sk          &\n
, \ref up_siz, \ref siz, \ref algakz, \ref akitbr, \ref akibrz, \ref hjsi, \ref nkzs, \ref dh2d, \ref dh2de, \ref mstr    &\n
, \ref iorla, \ref iorle, \ref ieinls, \ref flae, *qeinll*, *sil*, \ref itags, \ref uhrz, \ref azstrs         &\n
, \ref kontroll , *jjj* )    \n
-->

<!-- #mf: Referenz jjj funktioniert noch nicht -->

silikat() wird von der Hüllroutine silikat_huelle() aufgerufen. Zum 
Hüllroutinen-Konzept siehe: \ref lnk_huellen

## Ergänzungen Silikat {#lnk_si_aufteilung}

An den Zufluss-Rändern wird der Siliziumanteil in den Kieselalgen,\n
QSqm-Variable Q_SK, gespeichert in  planktonic_variable(32 \n
gleich dem maximalen Siliziumanteil der Kiesel-Algenbiomasse, \n
QSqm-Variable Qmx_SK, gespeichert in QSimDatenfelder aus APARAM.txt (siehe dazu eingabe())\n
in RB_werte_aktualisieren() gesetzt.

\n\n

Text source: silikat-umsetzung.md; Code sources: module_silicate.f90, silicate.f90 
and silicate_wrapper_3d.f95; go back to \ref lnk_silikat oder  \ref lnk_randbedingungen
