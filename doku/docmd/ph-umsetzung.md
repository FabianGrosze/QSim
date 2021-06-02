pH - Umsetzung {#lnk_ph_umsetzung}
==================

## Herkunft ##

EIN PROGRAMM ZUR BERECHNUNG DES PH-WERTES EINES GEWAESSERS 

aus dem m-Wert und der Kohlensaeuresumme

AUTOR:VOLKER KIRCHESCH

entnommen aus Version qsim13.301_28mae18


## Schnittstellenbeschreibung ##

call ph()

( \ref mw, \ref pw, \ref ca, \ref lf, \ref tempw, \ref tflie, \ref susn, \ref bsbt, \ref dalgki

&, \ref dalggr, \ref dalgak, \ref dalgag, \ref po2p, \ref po2r, \ref rau, \ref vmitt, \ref tiefe

&, \ref flae, \ref vabfl
&, \ref flag, \ref elen, \ref ior, \ref anze, \ref vph                                 &

&, *elfl*, *cal*, *qeinll*, \ref iorla, \ref iorle, \ref ieinls                     &
&, \ref ssalg, \ref stind, \ref albewg                                     &
&, \ref alberg, \ref albewk, \ref alberk, \ref wge                               &
&, \ref abl, \ref dalgbl, \ref dalgab, *idwe*, \ref iwied, \ref fkm, \ref ij, \ref resdr              &
&, \ref dzres1, \ref dzres2, \ref aki, \ref agr                                  &
&, \ref ilbuhn, *eph*, *emw*, *elf*, *eca*, \ref vco2, \ref qeinl, \ref jiein                &
&, \ref mstr, \ref cpfad, \ref rhyd, \ref wlage, \ref hws, \ref itags, \ref monats, \ref uhrz                           &
&, \ref azstrs, \ref iphy , \ref kontroll , \ref iglob )

Die QSim3D Subroutine ph_huelle() dient dem Aufruf der QSimD-subroutine ph(). 
(Zum Hüllroutinen-Konzept siehe: \ref hüllen )


## IT-Realisierung ##

...

Textquelle: ph-umsetzung.md ; Codesources: ph.f90 ph_kern.f90 phstart.f90 ; zurück: \ref 

