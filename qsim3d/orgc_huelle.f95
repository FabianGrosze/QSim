! --------------------------------------------------------------------------- !
!  QSim - Programm zur Simulation der Wasserqualität                          !
!                                                                             !
!  Copyright (C) 2022                                                         !
!  Bundesanstalt für Gewässerkunde                                            !
!  Koblenz (Deutschland)                                                      !
!  http://www.bafg.de                                                         !
!                                                                             !
!  Dieses Programm ist freie Software. Sie können es unter den Bedingungen    !
!  der GNU General Public License, Version 3, wie von der Free Software       !
!  Foundation veröffentlicht, weitergeben und/oder modifizieren.              !
!                                                                             !
!  Die Veröffentlichung dieses Programms erfolgt in der Hoffnung, dass es     !
!  Ihnen von Nutzen sein wird, aber ohne irgendeine Garantie, sogar ohne die  !
!  implizite Garantie der Makrtreife oder der Verwendbarkeit für einen        !
!  bestimmten Zweck.                                                          !
!                                                                             !
!  Details finden Sie in der GNU General Public License.                      !
!  Sie sollten ein Exemplar der GNU General Public License zusammen mit       !
!  diesem Programm erhalten haben.                                            !
!  Falls nicht, siehe http://www.gnu.org/licenses/.                           !
!                                                                             !
!  Programmiert von                                                           !
!  1979 bis 2018   Volker Kirchesch                                           !
!  seit 2011       Jens Wyrwa, Wyrwa@bafg.de                                  !
! --------------------------------------------------------------------------- !

subroutine orgc_huelle(i)
   use modell
   use QSimDatenfelder
   use aparam
   implicit none
   integer :: i,j
   real, dimension(1,2,100)        :: eCD, eCP
   real, dimension(1,100)          :: eCM, eBAC
   real :: hsdFluB
   !> i ist die lokale Knotennummer auf dem jeweiligen Prozessor und läuft von 1 bis part
   iglob = (i+meinrang*part)
   if (iglob > knotenanzahl2D) return ! überstehende Nummern nicht bearbeiten.
   !if(iglob.eq.kontrollknoten)print*,'orgc test return ',i
   !RETURN
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Datenübergabe:
   obsb(1) = planktonic_variable_p(17+(i-1)*number_plankt_vari) ! C-BSB5 Randvorgabe
   obsb(2) = obsb(1)
   ocsb(1) = planktonic_variable_p(18+(i-1)*number_plankt_vari) ! CSB Randvorgabe
   ocsb(2) = ocsb(1)
   tiefe(1) = rb_hydraul_p(2+(i-1)*number_rb_hydraul) ! water_depth(i)
   tiefe(2) = tiefe(1)
   rau(1:2) = strickler( zone(point_zone(iglob))%reib , tiefe(1) ) ! Strickler Reibungsbeiwert
   tflie = real(deltat)/86400 ! Umwandlung des Zeitschritts von integer sekunden (QSim3D) in real Tage (QSim)
   vmitt(1) = rb_hydraul_p(1+(i-1)*number_rb_hydraul) ! Geschwindigkeitsbetrag
   vmitt(2) = vmitt(1)
   bl01(1) = planktonic_variable_p(44+(i-1)*number_plankt_vari) !! schwerabbaubare Kohlenstoffverbindungen als Sauerstoffäquivalent
   bl01(2) = bl01(1)
   bl02(1) = planktonic_variable_p(45+(i-1)*number_plankt_vari) ! leichtabbaubare Kohlenstoffverbindungen als Sauerstoffäquivalent
   bl02(2) = bl02(1)
   zooind(1) = planktonic_variable_p(50+(i-1)*number_plankt_vari) ! Anzahl der Rotatorien
   zooind(2) = zooind(1)
   abszo(1) = transfer_quantity_p(6+(i-1)*number_trans_quant) ! Absterberate Zooplankton
   abszo(2) = abszo(1)
   tempw(1) = planktonic_variable_p( 1+(i-1)*number_plankt_vari)  ! Wassertemperatur
   tempw(2) = tempw(1)
   vbsb(1) = planktonic_variable_p(46+(i-1)*number_plankt_vari) ! Rückgabe/Kontroll-wert ??
   vbsb(2) = vbsb(1)
   bsbt(1) = transfer_quantity_p(1+(i-1)*number_trans_quant) ! Kohlenstoffbürtige Sauerstoffzehrung je Zeitschritt (nur Rückgabe)
   bsbt(2) = bsbt(1)
   flag(1) = 0         ! keine Einleitungen
   flag(2) = flag(1)
   elen(1) = 1         ! Elementlänge (nicht verwendet)
   elen(2) = elen(1)
   ior = 1             ! Laufindex
   anze = 1            ! Anzahl der Profile im aktuellen Strang
   ecsb(1) = 0.0      ! keine Einleitung
   ebsb(1) = 0.0      ! keine Einleitung
   qeinl(1) = 0.0      ! kein Abfluss Einleitung
   vabfl(1) = 2.5     ! wird nur bei Einleitungen verwendet, die sind aber ausgeschaltet.
   vabfl(2) = vabfl(1)
   sdbsb(1) = 0.0      ! unbenutzt
   sdbsb(2) = sdbsb(1)
   zexki(1) = transfer_quantity_p(16+(i-1)*number_trans_quant) ! Ausscheidungen der Rotatorien infolge Konsums von Kieselalgen in mg/l je Zeitschritt
   zexki(2) = zexki(1)
   zexgr(1) = transfer_quantity_p(17+(i-1)*number_trans_quant) ! Ausscheidungen der Rotatorien infolge Konsums von Grünalgen in mg/l je Zeitschritt
   zexgr(2) = zexgr(1)
   bsbbet(1) = benthic_distribution_p(7+(i-1)*number_benth_distr) ! Sauerstoffverbrauch durch Organismen auf Makrophyten; Ausgabevariable für bsbtb
   bsbbet(2) = bsbbet(1)
   dkimor(1) = transfer_quantity_p(7+(i-1)*number_trans_quant) ! Absterberate Kieselalgen
   dkimor(2) = dkimor(1)
   dgrmor(1) = transfer_quantity_p(8+(i-1)*number_trans_quant) ! Absterberate Grünlalgen
   dgrmor(2) = dgrmor(1)
   jiein(1) = 0        ! null Punkt-Einleitungen
   jiein(2) = jiein(1)
   !jetzt direkt aus QSimDatenfelder bsbgr=transfer_parameter_p( 8) ! C-BSB5-Erhöhung Grünalgen  ?? Aparam.txt
   !jetzt direkt aus QSimDatenfelder bsbki=transfer_parameter_p(29) ! C-BSB5-Erhöhung Kieselalgen ?? Aparam.txt
   akbcm(1) = planktonic_variable_p(24+(i-1)*number_plankt_vari) ! Verhältnis Chlorophyll-a zu Kohlenstoff Kieselalgen
   akbcm(2) = akbcm(1)
   agbcm(1) = planktonic_variable_p(25+(i-1)*number_plankt_vari) ! Verhältnis Chlorophyll-a zu Kohlenstoff Gruenalgen
   agbcm(2) = agbcm(1)
   pfl(1) = benthic_distribution_p(3+(i-1)*number_benth_distr) ! Trockengewicht Wasserpflanzen in g/m²
   pfl(2) = pfl(1)
   ezind(1) = 0.0 ! Einleitungswert Rotatorien (keine Einleitungen in QSim3D)
   ezind(2) = ezind(1)
   abl(1) = planktonic_variable_p(10+(i-1)*number_plankt_vari) ! Anteil blau-Algen
   abl(2) = abl(1)
   abbcm(1) = planktonic_variable_p(26+(i-1)*number_plankt_vari) ! Verhältnis Chlorophyll-a zu Kohlenstoff der blau-Algen
   abbcm(2) = abbcm(1)
   !jetzt direkt aus QSimDatenfelder bsbbl=transfer_parameter_p(52) ! C-BSB5-Erhöhung Blaualgen ?? Aparam.txt
   !jetzt direkt aus QSimDatenfelder csbbl=transfer_parameter_p(53) ! > CSB-Erhöhung Blaualgen | Aparam.txt
   dblmor(1) = transfer_quantity_p(9+(i-1)*number_trans_quant) ! Absterberate Blaulalgen
   dblmor(2) = dblmor(1)
   zexbl(1) = transfer_quantity_p(18+(i-1)*number_trans_quant) ! Ausscheidungen der Rotatorien infolge Konsums von Blaualgen
   zexbl(2) = zexbl(1)
   !jetzt direkt aus QSimDatenfelder csbki=transfer_parameter_p(30) ! CSB-Erhöhung Kieselalgen | Aparam.txt
   !jetzt direkt aus QSimDatenfelder csbgr=transfer_parameter_p(9) ! CSB-Erhöhung Grünalgen | Aparam.txt
   ischif(1:2) = zone(point_zone(iglob))%schiff%schifffahrts_zone ! hier unbenutzt
   echla(1) = 0.0  ! Einleitungswert (keine Einleitungen in QSim3D)
   evkigr(1) = 0.0  ! Einleitungswert (keine Einleitungen in QSim3D)
   eantbl(1) = 0.0  ! Einleitungswert (keine Einleitungen in QSim3D)
   aki(1) = planktonic_variable_p(8+(i-1)*number_plankt_vari) ! Anteil kiesel-Algen
   aki(2) = aki(1)
   agr(1) = planktonic_variable_p(9+(i-1)*number_plankt_vari) ! Anteil gruen-Algen
   agr(2) = agr(1)
   drfaeb(1:2) = transfer_quantity_p(15+(i-1)*number_trans_quant) ! Ausscheidungen der Dreissena-Muscheln infolge Konsums von Blaualgen
   drfaek(1:2) = transfer_quantity_p(13+(i-1)*number_trans_quant) ! Faecesbildung der Muscheln infolge Konsum Kieselalgen
   drfaeg(1:2) = transfer_quantity_p(14+(i-1)*number_trans_quant) ! Faecesbildung der Muscheln infolge Konsum Grünlalgen
   drfaes(1:2) = transfer_quantity_p(95+(i-1)*number_trans_quant) ! Ausscheidungen der Dreissena-Muscheln infolge Konsums von Schwebstoffen
   ssdr(1) = benthic_distribution_p(4+(i-1)*number_benth_distr) ! Schwebstoffaufnahme durch Dreissena ??
   ssdr(2) = ssdr(1)
   orgCsd(1,1) = benthic_distribution_p(6+(i-1)*number_benth_distr) ! Gesamtmase Kohlenstoff, die je Zeitschritt sedimentiert
   orgCsd(1,2) = orgCsd(1,1)
   orgCsd0(1) = benthic_distribution_p(50+(i-1)*number_benth_distr) !  sedimentiertes organ. Material
   orgCsd0(2) = orgCsd0(1)
   orgCsd_abb(1,1) = benthic_distribution_p(51+(i-1)*number_benth_distr) ! sedimentiertes biologisch abbaubares organ. Material
   orgCsd_abb(1,2) = orgCsd_abb(1,1)
   CD(1,1) = planktonic_variable_p(37+(i-1)*number_plankt_vari) ! leicht abbaubare gelöste organische C-Verbindungen
   CD(1,2) = CD(1,1)
   CD(2,1) = planktonic_variable_p(38+(i-1)*number_plankt_vari) ! schwer abbaubare gelöste organische C-Verbindungen
   CD(2,2) = CD(2,1)
   CP(1,1) = planktonic_variable_p(39+(i-1)*number_plankt_vari) ! leicht abbaubare partikuläre organische C-Verbindungen
   CP(1,2) = CP(1,1)
   CP(2,1) = planktonic_variable_p(40+(i-1)*number_plankt_vari) ! schwer abbaubare partikuläre organische C-Verbindungen
   CP(2,2) = CP(2,1)
   CM(1) = planktonic_variable_p(41+(i-1)*number_plankt_vari) ! monomolekularen organischen C-Verbindungen
   CM(2) = CM(1)
   BAC(1) = planktonic_variable_p(42+(i-1)*number_plankt_vari) ! Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen
   BAC(2) = BAC(1)
   !keine Einleitung
   eCD(1,1,1) = 0.0 !
   eCD(1,1,2) = 0.0 !
   eCD(1,2,1) = 0.0 !
   eCD(1,2,2) = 0.0 !
   eCP(1,1,1) = 0.0 !
   eCP(1,1,2) = 0.0 !
   eCP(1,2,1) = 0.0 !
   eCP(1,2,2) = 0.0 !
   eCM(1,1) = 0.0 !
   eCM(1,2) = 0.0 !
   eBAC (1,1) = 0.0 !                                                                               &
   eBAC (1,2) = 0.0 !                                                                               &
   O2BSB(1) = planktonic_variable_p(43+(i-1)*number_plankt_vari) ! Sauerstoff-Kohlenstoffverhältnis beim C-Abbau (mgO2/mgC)
   O2BSB(2) = O2BSB(1)
   !jetzt direkt aus QSimDatenfelder TOC_CSB = 3.1 !  in orgc_start hart codiert
   !jetzt direkt aus QSimDatenfelder GRote=transfer_parameter_p(67) ! Gewicht einer Rotatorie µg  | Aparam.txt
   vcsb(1) = planktonic_variable_p(47+(i-1)*number_plankt_vari) ! Rückgabe/Kontroll-wert ??
   vcsb(2) = vcsb(1)
   vkigr(1) = planktonic_variable_p(19+(i-1)*number_plankt_vari) ! Anteil der Kieselalgen am Gesamt-Chlorophyll-a ??
   vkigr(2) = vkigr(1)
   antbl(1) = planktonic_variable_p(20+(i-1)*number_plankt_vari) ! Anteil der Blaualgen am Gesamt-Chlorophyll-a
   antbl(2) = antbl(1)
   HNFBAC(1) = transfer_quantity_p(11+(i-1)*number_trans_quant) ! Verlust der Bakterien durch HNF-Grazing
   HNFBAC(2) = HNFBAC(1)
   BSBHNF(1) = transfer_quantity_p(10+(i-1)*number_trans_quant) ! Absterben und Exkretion Heterotropher Naloflagelaten
   BSBHNF(2) = BSBHNF(1)
   CHNF(1:2) = planktonic_variable_p(48+(i-1)*number_plankt_vari) ! C-Masse der heterotrophen Nanoflagelaten
   if (CHNF(1) <= 0.0) CHNF(1:2) = 0.0 ! CHNF=-1 meint keine HNF
   eCHNF(1) = 0.0  ! Einleitungswert (keine Einleitungen in QSim3D)
   fbsgr(1) = planktonic_variable_p(55+(i-1)*number_plankt_vari) ! Faktors zur Berechnung der ablagerungsfreien Grenzkonzentration von refraktärem Material ????
   fbsgr(2) = fbsgr(1)
   frfgr(1) = planktonic_variable_p(56+(i-1)*number_plankt_vari) ! Faktors zur Berechnung der ablagerungsfreien Grenzkonzentration von refraktärem Material ????
   frfgr(2) = frfgr(1)
   fbsgrs = 0.0  ! Einleitungswert (keine Einleitungen in QSim3D)
   frfgrs = 0.0  ! Einleitungswert (keine Einleitungen in QSim3D)
   BACmua(1) = transfer_quantity_p(4+(i-1)*number_trans_quant) !  Ausgabekonzentration Summe Aufnahme+Respirationsrate heterotrophe Bakterien
   BACmua(2) = BACmua(1)
   dorgSS(1) = transfer_quantity_p(19+(i-1)*number_trans_quant) ! Veränderung von suspendierten Sedimenten aus C-Verbindungen
   dorgSS(2) = dorgSS(1)
   ilbuhn = 0          ! keine Buhnen
   iwied = 0      ! unbenutzte Variable
   fkm (1) = 0.0  ! Flusskilometer unbenutzt
   bsbct(1) = transfer_quantity_p(47+(i-1)*number_trans_quant) ! mineralisierter Kohlenstoffgehalt in der Wassersäule | Rückgabewert
   bsbct(2) = bsbct(1)
   qeinlL(1) = 0.0 ! keine linienquelle
   iorLa(1) = 0 ! keine linienquelle
   iorLe(1) = 0 ! keine linienquelle
   ieinLs(1) = 0 ! keine linienquelle
   pl0(1) = planktonic_variable_p(58+(i-1)*number_plankt_vari)  !  P/C Verhältnis
   pl0(2) = pl0(1)
   Q_PK(1) = planktonic_variable_p(31+(i-1)*number_plankt_vari) ! Phosphoranteil der Algenbiomasse kiesel
   Q_PK(2) = Q_PK(1)
   Q_PB(1) = planktonic_variable_p(36+(i-1)*number_plankt_vari) ! Phosphoranteil der Algenbiomasse blau
   Q_PB(2) = Q_PB(1)
   Q_PG(1) = planktonic_variable_p(34+(i-1)*number_plankt_vari) ! Phosphornteil der Algenbiomasse gruen
   Q_PG(2) = Q_PG(1)
   nl0(1) = planktonic_variable_p(57+(i-1)*number_plankt_vari) ! Verhältnis von Stickstoff zu Kohlenstoff in organischem Material
   nl0(2) = nl0(1)
   Q_NK(1) = planktonic_variable_p(30+(i-1)*number_plankt_vari) ! Stickstoffanteil der Algenbiomasse kiesel
   Q_NK(2) = Q_NK(1)
   Q_NB(1) = planktonic_variable_p(35+(i-1)*number_plankt_vari) ! Stickstoffanteil der Algenbiomasse blau
   Q_NB(2) = Q_NB(1)
   Q_NG(1) = planktonic_variable_p(33+(i-1)*number_plankt_vari) ! Stickstoffanteil der Algenbiomasse gruen
   Q_NG(2) = Q_NG(1)
   bsbctP(1) = transfer_quantity_p(2+(i-1)*number_trans_quant) ! Phosphorfreisetzung beim Abbau org. Kohlenstoffverbidungen
   bsbctP(2) = bsbctP(1)
   doN(1) = transfer_quantity_p(3+(i-1)*number_trans_quant) ! mineralisierter N-Gehalt in der Wassersäule ; Ammoniumfreisetzung beim Abbau org. Kohlenstoffverbidungen
   doN(2) = doN(1)
   hsdFluB = 0.0 ! unbenutzte Variable
   JDOC1(1) = benthic_distribution_p(48+(i-1)*number_benth_distr) ! Flux gelöster org. Kohlenstoffe aus dem Sediment, leicht abbaubar
   JDOC1(2) = JDOC1(1)
   JDOC2(1) = benthic_distribution_p(49+(i-1)*number_benth_distr) ! Flux gelöster org. Kohlenstoffe aus dem Sediment, schwer abbaubar
   JDOC2(2) = JDOC2(1)
   ! aparam_lesen() jetzt direkt in QSimDatenfelder::hype ! Hydrolyserate für die leichtabbaubaren partikulären organischen C-Verbindungen APARAM.txt
   !jetzt direkt aus QSimDatenfelder hymxDe  ! maximale Hydrolyserate für die gelösten organischen C-Verbindungen | APARAM.txt
   !jetzt direkt aus QSimDatenfelder KsD1e  ! Halbsättigungskonstante für die Hydrolyse der gelösten leichtabbaubaren organischen C-Verbindungen | APARAM.txt
   !jetzt direkt aus QSimDatenfelder KsD2e  ! Halbsättigungskonstante für die Hydrolyse der gelösten schwerabbaubaren organischen C-Verbindungen | APARAM.txt
   !jetzt direkt aus QSimDatenfelder KsMe  ! Halbsättigungskonstante für die Aufnahme von Kohlenstoff durch heterotrophen Bakterien | APARAM.txt
   !jetzt direkt aus QSimDatenfelder upBACe  ! max. Aufnahmerate monomerer C-Verbindungen d. Bakterien | APARAM.txt
   !jetzt direkt aus QSimDatenfelder YBACe ! Ertragskoeffizient für Bakterienbiomasse | APARAM.txt
   !jetzt direkt aus QSimDatenfelder rsGBACe ! Grundrespirationsrate HBAC bei Optimumstemperatur | APARAM.txt
   nkzs(1) = 1         ! nur eine Tiefenschicht
   nkzs(2) = nkzs(1)
   mstr = 1            ! Strangzähler; orgc so benutzen , dass nur der 1. Strang mit nur einem Knoten/Profil berechnet wird
   !itags=tag         ! Startzeitpunkt aus module_QSimDatenfelder
   !monats=monat
   uhrz = uhrzeit_stunde ! Uhrzeit module::modell zeitsekunde()
   zBAC(1) = transfer_quantity_p(91+(i-1)*number_trans_quant) ! Konsum von BAC durch Zoopankton
   zBAC(2) = zBAC(1)
   !bsbZoo = 1.6  ! in orgc_start hart codiert jetzt direkt aus QSimDatenfelder
   ! kontroll,i ! Für QSim3D hinzugefügt (kontrollausgabemöglichkeit)
   !kontroll=(i.eq.number_plankt_point)
   kontroll = (iglob == kontrollknoten)
   if (kontroll) print*,'orgc_huelle davor: CHNF,BAC,obsb,ocsb,bsbt,bl01,bl02,vbsb,bsbt'  &
       ,CHNF(1),BAC(1),obsb(1),ocsb(1),bsbt(1),bl01(1),bl02(1),vbsb(1),bsbt(1)
   if (kontroll) print*,'orgc_huelle davor: BAC,bacmua,CM,zBAC',BAC(1),BACmua(1),CM(1),zBAC(1)
   !version  Stoffumsatzroutinen aus der QSim1D Version 13_40 vom 15. Oktober 2018 in QSim3D
   call orgC(obsb,ocsb,TIEFE,RAU,TFLIE,VMITT,flae,zooind,abszo,tempw,vbsb,bsbt,flag,elen,ior,anze              &
             ,ecsb,ebsb,qeinl,vabfl,sdbsb,zexki,zexgr,bsbbet,dkimor,dgrmor,jiein,bsbgr,bsbki,akbcm             &
             ,agbcm,pfl,ezind,abl,abbcm,bsbbl,csbbl,dblmor,zexbl,drfaeb,csbki,csbgr,ischif,echla               &
             ,evkigr,eantbl,aki,agr,drfaek,drfaeg,drfaes,ssdr,orgCsd,orgCsd0,orgCsd_abb,CD,CP,CM,BAC,eCD       &
             ,eCP,eCM,eBAC,TOC_CSB,GROT,vcsb,vkigr,antbl,HNFBAC,BSBHNF,CHNF,zBAC                               &
             ,BVHNF,eCHNF,fbsgr,frfgr,fbsgrs,frfgrs,BACmua,dorgSS,ilbuhn,iwied,fkm,bsbct,qeinlL                &
             ,iorLa,iorLe,ieinLs,pl0,Q_PK,Q_PB,Q_PG,pZoo,nl0,Q_NK,Q_NB,Q_NG,nzoo,etemp,bsbctP                  &
             ,doN,hsdFluB,HyP1,hymxD,KsD1,KsD2,KsM,upBAC,JDOC1,JDOC2,YBAC,rsGBAC                               &
             ,nkzs,mstr,itags,monats,uhrz,azStrs,bsbZoo                                                        &
             ,kontroll ,i )
   
   !version qsim13.301_28mae18wy_3D
   !       call orgC(obsb,ocsb,TIEFE,RAU,TFLIE,VMITT,flae,zooind,abszo,tempw,vbsb,bsbt,flag,elen,ior,anze             &
   !                ,ecsb,ebsb,qeinl,vabfl,sdbsb,zexki,zexgr,bsbbet,dkimor,dgrmor,jiein,bsbgr,bsbki,akbcm             &
   !                ,agbcm,pfl,ezind,abl,abbcm,bsbbl,csbbl,dblmor,zexbl,drfaeb,csbki,csbgr,ischif,echla               &
   !                ,evkigr,eantbl,aki,agr,drfaek,drfaeg,drfaes,ssdr,orgCsd,orgCsd0,orgCsd_abb,CD,CP,CM,BAC,eCD       &
   !                ,eCP,eCM,eBAC,TOC_CSB,GRote,vcsb,vkigr,antbl,HNFBAC,BSBHNF,CHNF,zBAC                              &
   !                ,BVHNF,eCHNF,fbsgr,frfgr,fbsgrs,frfgrs,BACmua,dorgSS,ilbuhn,iwied,fkm,bsbct,qeinlL                &
   !                ,iorLa,iorLe,ieinLs,pl0,Q_PK,Q_PB,Q_PG,pZoo,nl0,Q_NK,Q_NB,Q_NG,nzoo,etemp,bsbctP                  &
   !                ,doN,hsdFluB,hyPe,hymxDe,KsD1e,KsD2e,KsMe,upBACe,JDOC1,JDOC2,YBACe,rsGBACe                        &
   !                ,nkzs,mstr,itags,monats,uhrz, azStrs, bsbZoo                                                      &
   !                ,kontroll,i)           !!wy
   !version 13_30
   !  call orgC(obsb,ocsb,TIEFE,RAU,TFLIE,VMITT,flae,zooind,abszo,tempw,vbsb,bsbt,flag,elen,ior,anze            &
   !                ,ecsb,ebsb,qeinl,vabfl,sdbsb,zexki,zexgr,bsbbet,dkimor,dgrmor,jiein,bsbgr,bsbki,akbcm             &
   !                ,agbcm,pfl,ezind,abl,abbcm,bsbbl,csbbl,dblmor,zexbl,drfaeb,csbki,csbgr,ischif,echla               &
   !                ,evkigr,eantbl,aki,agr,drfaek,drfaeg,drfaes,ssdr,orgCsd,orgCsd0,orgCsd_abb,CD,CP,CM,BAC,eCD       &
   !                ,eCP,eCM,eBAC,TOC_CSB,GRote,vcsb,vkigr,antbl,HNFBAC,BSBHNF,CHNF                                   &
   !                ,BVHNF,eCHNF,fbsgr,frfgr,fbsgrs,frfgrs,BACmua,dorgSS,ilbuhn,iwied,fkm,bsbct,qeinlL                &
   !                ,iorLa,iorLe,ieinLs,pl0,Q_PK,Q_PB,Q_PG,pZoo,nl0,Q_NK,Q_NB,Q_NG,nzoo,etemp,bsbctP                  &
   !                ,doN,hsdFluB,hyPe,hymxDe,KsD1e,KsD2e,KsMe,upBACe,JDOC1,JDOC2,YBACe,rsGBACe                        &
   !                ,nkzs,mstr,itags,monats,uhrz,1                                                               &
   !                ,kontroll,i)           !!wy
   !version 13_10
   !       call orgC(obsb,ocsb,TIEFE,RAU,TFLIE,VMITT,bl01,bl02                        &
   !                ,zooind,abszo,tempw,vbsb,bsbt,flag,elen,ior,anze                  &
   !                ,ecsb,ebsb,qeinl,vabfl,sdbsb,zexki,zexgr,bsbbet,dkimor,dgrmor     &
   !                ,jiein,bsbgr,bsbki,akbcm,agbcm,pfl,ezind                          &
   !                ,abl,abbcm,bsbbl,csbbl,dblmor,zexbl,drfaeb                        &
   !                ,csbki,csbgr,ischif,echla,evkigr,eantbl                           &
   !                ,aki,agr,drfaek,drfaeg,drfaes,ssdr,orgCsd                         &
   !                ,CD,CP,CM,BAC,O2BSB,GRote,vcsb,vkigr,antbl                        &
   !                ,HNFBAC,BSBHNF,CHNF,eCHNF,fbsgr,frfgr,fbsgrs,frfgrs               &
   !                ,BACmua,dorgSS,ilbuhn,iwied,fkm,bsbct                             &
   !                ,pl0,Q_PK,Q_PB,Q_PG,pZoo,nl0,Q_NK,Q_NB,Q_NG,nzoo                  &
   !                ,bsbctP,doN,hyPe,hymxDe,KsD1e,KsD2e,KsMe,upBACe                   &
   !                ,YBACe,rsGBACe,nkzs,mstr,itags,monats,uhrz                        &
   !                ,kontroll,i)
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Datenrückgabe:
   if (kontroll) print*,'orgc_huelle nachher: BAC,bacmua,CM,zBAC',BAC(1),BACmua(1),CM(1),zBAC(1)
   planktonic_variable_p(17+(i-1)*number_plankt_vari) = obsb(1) !
   planktonic_variable_p(18+(i-1)*number_plankt_vari) = ocsb(1) !
   ! entfallen planktonic_variable_p(44+(i-1)*number_plankt_vari) = bl01(1) !
   ! entfallen planktonic_variable_p(45+(i-1)*number_plankt_vari) = bl02(1) !
   planktonic_variable_p(46+(i-1)*number_plankt_vari) = vbsb(1) !
   planktonic_variable_p(47+(i-1)*number_plankt_vari) = vcsb(1) !
   transfer_quantity_p(1+(i-1)*number_trans_quant) = bsbt(1) !
   benthic_distribution_p(7+(i-1)*number_benth_distr) = bsbbet(1) ! Sauerstoffverbrauch durch Organismen auf Makrophyten, Ausgabewert bsbtb
   benthic_distribution_p(6+(i-1)*number_benth_distr) = orgCsd(1,1) ! Gesamtmase Kohlenstoff, die je Zeitschritt sedimentiert
   benthic_distribution_p(50+(i-1)*number_benth_distr) = orgCsd0(1) ! teil des? sedimentierten organ. Material
   benthic_distribution_p(51+(i-1)*number_benth_distr) = orgCsd_abb(1,1) ! sedimentiertes biologisch abbaubares organ. Material
   planktonic_variable_p(37+(i-1)*number_plankt_vari) = CD(1,1) !
   planktonic_variable_p(38+(i-1)*number_plankt_vari) = CD(2,1) !
   planktonic_variable_p(39+(i-1)*number_plankt_vari) = CP(1,1) !
   planktonic_variable_p(40+(i-1)*number_plankt_vari) = CP(2,1) !
   planktonic_variable_p(41+(i-1)*number_plankt_vari) = CM(1) !
   planktonic_variable_p(42+(i-1)*number_plankt_vari) = BAC(1) !
   ! entfallen planktonic_variable_p(43+(i-1)*number_plankt_vari) = O2BSB(1) !
   planktonic_variable_p(55+(i-1)*number_plankt_vari) = fbsgr(1) !
   planktonic_variable_p(56+(i-1)*number_plankt_vari) = frfgr(1) !
   transfer_quantity_p(4+(i-1)*number_trans_quant) = BACmua(1) ! Ausgabekonzentration
   transfer_quantity_p(19+(i-1)*number_trans_quant) = dorgSS(1) !
   transfer_quantity_p(47+(i-1)*number_trans_quant) = bsbct(1) ! mineralisierter Kohlenstoffgehalt in der Wassersäule | Rückgabewert
   planktonic_variable_p(58+(i-1)*number_plankt_vari) = pl0(1) !
   planktonic_variable_p(57+(i-1)*number_plankt_vari) = nl0(1) !
   transfer_quantity_p(2+(i-1)*number_trans_quant) = bsbctP(1) !
   transfer_quantity_p(3+(i-1)*number_trans_quant) = doN(1) !
   benthic_distribution_p(48+(i-1)*number_benth_distr) = JDOC1(1) ! Flux gelöster org. Kohlenstoffe aus dem Sediment, leicht abbaubar
   benthic_distribution_p(49+(i-1)*number_benth_distr) = JDOC2(1) ! Flux gelöster org. Kohlenstoffe aus dem Sediment, schwer abbaubar
   !if(kontroll) print*,'orgc_huelle danach: bsbt, obsb, vbsb,bl01,' &
   !  ,transfer_quantity_p(1+(i-1)*number_trans_quant),planktonic_variable_p(17+(i-1)*number_plankt_vari),  &
   !   planktonic_variable_p(46+(i-1)*number_plankt_vari),bl01(1)
   if (kontroll) print*,'orgc_huelle danach: CHNF,BAC,obsb,ocsb,bsbt,bl01,bl02,vbsb,bsbt'  &
       ,CHNF(1),BAC(1),obsb(1),ocsb(1),bsbt(1),bl01(1),bl02(1),vbsb(1),bsbt(1)
   return
end subroutine orgc_huelle
