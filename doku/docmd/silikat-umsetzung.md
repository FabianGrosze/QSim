Silikat - Umsetzung {#lnk_silikat_umsetzung}
===================

## Herkunft ##
silikat() \n 
EIN PROGRAMM zu Berechnung des geloesten Silikats    \n
AUTOR : VOLKER KIRCHESCH                      \n
entnommen aus Version qsim13.301_28mae18\n 

## Schnittstellenbeschreibung ##

SUBROUTINE silikat()\n
( \ref si, \ref flag, \ref elen, \ref ior, *esi*, *qeinl*, \ref vabfl, \ref anze, \ref tflie, \ref jiein, \ref aki         &\n
, \ref albewk, \ref alberk, \ref tiefe, \ref tempw, \ref ilbuhn, \ref akkssi, \ref qmx_sk, \ref q_sk          &\n
, \ref up_siz, \ref siz, \ref algakz, \ref akitbr, \ref akibrz, \ref hjsi, \ref nkzs, \ref dh2d, \ref dh2de, \ref mstr    &\n
, \ref iorla, \ref iorle, \ref ieinls, \ref flae, *qeinll*, *sil*, \ref itags, \ref uhrz, \ref azstrs         &\n
, \ref kontroll , \ref iglob )    \n

silikat() wird von der Hüllroutine silikat_huelle() aufgerufen. Zum Hüllroutinen-Konzept siehe: \ref hüllen

\n\n

aus Datei: silikat-umsetzung.md; Code in Datei: silikat.f90
