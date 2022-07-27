Organischer Kohlenstoff - Umsetzung {#lnk_orgc_umsetzung}
========================= 

## Herkunft ##
SUBROUTINE orgc()\n
Programm zur Berechnung des biochemischen Sauerstoffbedarfs (BSB)\n
AUTOR : VOLKER KIRCHESCH      \n                                   
entnommen aus Version qsim13.301_28mae18\n 

## Schnittstellenbeschreibung ##

<code>
call orgc() \ref obsb, \ref ocsb, \ref tiefe, \ref rau, \ref tflie, \ref vmitt
, \ref flae, \ref zooind, \ref abszo, \ref tempw, \ref vbsb, \ref bsbt, \ref flag, \ref elen, \ref ior, \ref anze  &  \n                 
, \ref ecsb, \ref ebsb, \ref qeinl, \ref vabfl, \ref sdbsb, \ref zexki, \ref zexgr
, \ref bsbbet, \ref dkimor, \ref dgrmor, \ref jiein, \ref bsbgr, \ref bsbki, \ref akbcm &\n
, \ref agbcm, \ref pfl, \ref ezind, \ref abl, \ref abbcm, \ref bsbbl, \ref csbbl
, \ref dblmor, \ref zexbl, \ref drfaeb, \ref csbki, \ref csbgr, \ref ischif, \ref echla &\n
, \ref evkigr, \ref eantbl, \ref aki, \ref agr, \ref drfaek, \ref drfaeg
, \ref drfaes, \ref ssdr, *orgcsd*, \ref orgcsd0, \ref orgcsd_abb, *cd*, *cp*, *cm*, *bac*, *ecd* &\n
, *ecp*, *ecm*, *ebac*, \ref toc_csb, *grote*, \ref vcsb, \ref vkigr
, \ref antbl, \ref hnfbac, \ref bsbhnf, \ref chnf, \ref zbac &\n
, \ref bvhnf, \ref echnf, \ref fbsgr, \ref frfgr, \ref fbsgrs, \ref frfgrs
, \ref bacmua, \ref dorgss, \ref ilbuhn, \ref iwied, \ref fkm, \ref bsbct, *qeinll* &\n
, \ref iorla, \ref iorle, \ref ieinls, \ref pl0, \ref q_pk, \ref q_pb, \ref q_pg
, *pzoo*, \ref nl0, \ref q_nk, \ref q_nb, \ref q_ng, \ref nzoo, *etemp*, \ref bsbctp &\n
, \ref don, *hsdflub*, \ref hype, \ref hymxde, \ref ksd1e, \ref ksd2e
, \ref ksme, \ref upbace, \ref jdoc1, \ref jdoc2, \ref ybace, \ref rsgbace &\n
, \ref nkzs, \ref mstr, \ref itags, \ref monats, \ref uhrz, \ref  azstrs
, *bsbzoo* &\n
, \ref kontroll, *i*) !!wy\n      
</code> \n

## Randbedingungen ergänzen/erschließen ##
An Rändern kann nur der C-BSB5 und CSB-Wert vorgegeben werden. Intern werden aber
Bilanzen für die o.g. 7 Konzentrationen aufgestellt. 
Aufgrund plausibler Annahmen wird eine Aufteilung in der Subroutine 
orgc_start() vorgenommen siehe dazu \ref orgc_aufteilung.
Siehe dazu auch \ref randbedingungen_ergaenzen .
<!-- #mf: welche 7 Konz. sind gemeint? (bezieht sich auf Listen in lnk_orgC_prozesse -->

\n\n

Textquelle: kohlenstoff-umsetzung.md; Code: orgC.f90; orgc_huelle.f95; zurück \ref lnk_orgC
