Phosphor - Umsetzung {#lnk_phosphor_umsetzung}
========================= 

## Herkunft ##
po4s() \n 
EIN PROGRAMM zu Berechnung des Gesamt-Phosphats und des geloesten Phosphats \n
AUTOR : VOLKER KIRCHESCH                      \n
entnommen aus Version qsim14.05\n 

## Schnittstellenbeschreibung ##

SUBROUTINE po4s()\n
( \ref gelp, \ref flag, \ref elen, \ref ior, \ref tiefe                      &\n
&, \ref dalggr, \ref dalgki, \ref dalgag, \ref dalgak                        &\n
&, *ep*, \ref qeinl, \ref vabfl, \ref anze, \ref tflie, \ref dzres1          &\n
&, \ref dzres2, \ref jiein, \ref sedalk, \ref sedalb, \ref sedalg            &\n
&, \ref albewg, \ref alberg, \ref albewk, \ref alberk, \ref resdr, \ref aki  &\n
&, \ref agr, \ref exdrvk, \ref exdrvg, \ref pl0, \ref abl, \ref dalgbl       &\n
&, \ref dalgab, \ref exdrvb, \ref gesp, \ref orgcsd, \ref zooind             &\n
&, *grote*, *pzoo*, *egesp*, \ref ilbuhn, \ref iwied, *cd*, *cp*          &\n
&, \ref cm, \ref bac, \ref bsbctp, \ref qmx_pk, \ref q_pk, \ref up_pkz       &\n
&, \ref qmx_pg, \ref q_pg, \ref up_pgz, \ref qmx_pb, \ref q_pb               &\n
&, \ref up_pbz, *epl0*, \ref gelpz, \ref agrtbr, \ref akitbr, \ref abltbr    &\n
&, \ref agrbrz, \ref akibrz, \ref ablbrz, \ref algakz, \ref algagz           &\n
&, \ref algabz, \ref hjpo4, \ref nkzs, \ref dh2d, \ref dh2de, \ref mstr      &\n 
&,\ref iorla, \ref iorle, \ref ieinls, \ref flae, *qeinll*, *gpl*, *gespl*   &\n
&, \ref hgespz, \ref algdrk, \ref algdrg, \ref algdrb, \ref itags            &\n
&, \ref monats, \ref uhrz, \ref azstrs, \ref kontroll , *jjj* )    \n


po4s() wird von der Hüllroutine po4s_huelle() aufgerufen. 

Zum Hüllroutinen-Konzept siehe: \ref lnk_huellen

\n\n
### Rand und Anfangsbedingungen ###
Aufteilung im Zufluss mit naehr_start(); Siehe dazu auch 
\ref lnk_randbedingungen_ergaenzen . 

## Phosphor-Aufteilung Zufluss {#lnk_po4s_aufteilung}
Phophor-Gehalte im Zufluss werden auf den Maximalwert gesetzt in 
randwert_planctonic():\n
     planktonic_variable(31+nk)= transfer_parameter_p(32) ! *Q_PK*=*Qmx_PK* \n
     planktonic_variable(34+nk)= transfer_parameter_p(11) ! *Q_PG*=*Qmx_PG* \n
     planktonic_variable(36+nk)= transfer_parameter_p(55) ! *Q_PB*=*Qmx_PB* \n

\n\n

Textquelle: phosphor-umsetzung.md; Code: ncyc.f90 und po4s_huelle.f95; 
zurück \ref lnk_phosphor oder \ref lnk_randbedingungen_ergaenzen
