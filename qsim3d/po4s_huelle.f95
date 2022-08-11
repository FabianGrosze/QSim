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
subroutine po4s_huelle(i)
   use modell
   use QSimDatenfelder
   use aparam
   implicit none
   integer :: i,j,k,nk
   iglob = (i+meinrang*part)
   kontroll = iglob == kontrollknoten ! Erweiterung QSim3D wy
   nk = (i-1)*number_plankt_vari
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Datenübergabe:
   gelp(1) = planktonic_variable_p( 6+nk)  ! gelöster ortho-Phosphat-Phosphor tiefengemittelt
   gelp(2) = gelp(1)
   flag(1) = 0         ! keine Einleitungen
   flag(2) = flag(1)
   elen(1) = 1         ! Elementlänge (nicht verwendet)
   elen(2) = elen(1)
   ior = 1             ! Laufindex
   tiefe(1) = rb_hydraul_p(2+(i-1)*number_rb_hydraul) ! Wassertiefe
   tiefe(2) = tiefe(1)
   dalggr(1) = transfer_quantity_p(21+(i-1)*number_trans_quant) ! Zuwachs Grün-Algen 1.0 !### veri13.3 ###
   dalggr(2) = dalggr(1)
   dalgki(1) = transfer_quantity_p(20+(i-1)*number_trans_quant) ! Zuwachs Kiesel-Algen
   dalgki(2) = dalgki(1)
   dalgag(1) = transfer_quantity_p(24+(i-1)*number_trans_quant) ! Respiration Grün-Algen !### veri13.3 ### 0.1
   dalgag(2) = dalgag(1)
   dalgak(1) = transfer_quantity_p(23+(i-1)*number_trans_quant) ! Respiration Kiesel-Algen
   dalgak(2) = dalgak(1)
   ep(1) = 0.0        ! keine Einleitung
   qeinl(1) = 0.0      ! kein Abfluss Einleitung
   vabfl(1) = 2.5     ! wird nur bei Einleitungen verwendet, die sind aber ausgeschaltet.
   vabfl(2) = vabfl(1)
   anze = 1            ! Anzahl der Profile im aktuellen Strang
   tflie = real(deltat)/86400 ! Umwandlung des Zeitschritts von integer sekunden (T-QSim) in real Tage (QSim)
   dzres1(1) = transfer_quantity_p(27+(i-1)*number_trans_quant) ! Grund-Respiration Konsumenten
   dzres1(2) = dzres1(1)
   dzres2(1) = transfer_quantity_p(28+(i-1)*number_trans_quant) ! Fraßabhängige Respirationsrate Konsumenten
   dzres2(2) = dzres2(1)
   jiein(1) = 0        ! null Punkt-Einleitungen
   ! Phosphor-Austrag durch Sedimentation von Algen der Klassen grün, kiesel und blau
   sedalk(1) = benthic_distribution_p(26+(i-1)*number_benth_distr) ! Sedimentierte Menge an Kiesel-Algen
   sedalk(2) = sedalk(1)
   sedalb(1) = benthic_distribution_p(28+(i-1)*number_benth_distr) ! Sedimentierte Menge an Blau-Algen
   sedalb(2) = sedalb(1)
   sedalg(1) = benthic_distribution_p(27+(i-1)*number_benth_distr) ! Sedimentierte Menge an Grün-Algen
   sedalg(2) = sedalg(1)
   ! Verbrauch durch Wachstum bentischer Algen albewg(1000)*Qmx_PG ok
   albewg(1) = benthic_distribution_p(13+(i-1)*number_benth_distr) ! Wachstum benthischer gruen-Algen
   albewg(2) = albewg(1)
   ! Freisetzung durch Respiration von bentischen Algen alberg*Qmx_PG ok
   alberg(1) = benthic_distribution_p(11+(i-1)*number_benth_distr) ! Respiration benthischer gruen-Algen
   alberg(2) = alberg(1)
   albewk(1) = benthic_distribution_p(14+(i-1)*number_benth_distr) ! Wachstum benthischer kiesel-Algen
   albewk(2) = albewk(1)
   alberk(1) = benthic_distribution_p(12+(i-1)*number_benth_distr) ! Respiration benthischer kiesel-Algen
   alberk(2) = alberk(1)
   resdr(1) = benthic_distribution_p(15+(i-1)*number_benth_distr) ! Respirationsrate benthischer Filtrierer (Dreissena-Muscheln)
   resdr(2) = resdr(1)
   aki(1) = planktonic_variable_p(8+(i-1)*number_plankt_vari) ! Anteil kiesel-Algen
   aki(2) = aki(1)
   agr(1) = planktonic_variable_p(9+(i-1)*number_plankt_vari) ! Anteil gruen-Algen
   agr(2) = agr(1)
   exdrvk(1) = benthic_distribution_p(29+(i-1)*number_benth_distr) ! exkretierte Biomasse der Muscheln beim Verzehr von Kiesel-Algen
   exdrvk(2) = exdrvk(1)
   exdrvg(1) = benthic_distribution_p(30+(i-1)*number_benth_distr) ! exkretierte Biomasse der Muscheln beim Verzehr von Grün-Algen
   exdrvg(2) = exdrvg(1)
   exdrvb(1) = benthic_distribution_p(31+(i-1)*number_benth_distr) ! exkretierte Biomasse der Muscheln beim Verzehr von Blau-Algen
   exdrvb(2) = exdrvb(1)
   pl0(1) = planktonic_variable_p(58+nk)  !  P/C Verhältnis von Phosphor zu Kohlenstoff in organischem Material
   pl0(2) = pl0(1)
   abl(1) = planktonic_variable_p(10+nk) !  ???
   abl(2) = abl(1)
   dalgbl(1) = transfer_quantity_p(22+(i-1)*number_trans_quant) ! Zuwachs Blau-Algen
   dalgbl(2) = dalgbl(1)
   dalgab(1) = transfer_quantity_p(25+(i-1)*number_trans_quant) ! Respiration Blau-Algen
   dalgab(2) = dalgab(1)
   exdrvb(1) = benthic_distribution_p(31+(i-1)*number_benth_distr) ! exkretierte Biomasse der Muscheln beim Verzehr von Blau-Algen
   exdrvb(2) = exdrvb(1)
   gesp(1) = planktonic_variable_p(68+nk)  ! gesamter ortho-Phosphat-Phosphor
   gesp(2) = gesp(1)
   ! Phosphor-Austrag durch Sedimentation von org. Kohlenstoffverbidungen   ok
   orgCsd(1,1) = benthic_distribution_p(6+(i-1)*number_benth_distr) ! Gesamtmasse Kohlenstoff, die je Zeitschritt sedimentiert
   orgCsd(1,2) = orgCsd(1,1) ! ### Feld-dimensionsänderung in neuer QSim-Version
   zooind(1) = planktonic_variable_p(50+(i-1)*number_plankt_vari) ! Anzahl der Rotatorien
   zooind(2) = zooind(1)
   ! GRote  direkt aus QSimDatenfelder =transfer_parameter_p(67) ! Gewicht einer Rotatorie µg  | Aparam.txt
   ! pZoo  JETZT direkt aus QSimDatenfelder  = transfer_value_p(3) !! Phosphoranteil in der Rotatorienbiomasse mgP/mgBiom.
   ! bl01(1) = planktonic_variable_p(44+(i-1)*number_plankt_vari) !! schwerabbaubare Kohlenstoffverbindungen als Sauerstoffäquivalent
   ! bl01(2) = bl01(1)
   ! bl02(1) = planktonic_variable_p(45+(i-1)*number_plankt_vari) ! leichtabbaubare Kohlenstoffverbindungen als Sauerstoffäquivalent
   ! bl02(2) = bl02(1)
   egesP(1) = 0.0       ! keine Einleitung
   ilbuhn = 0          ! keine Buhnen
   iwied = 0      ! unbenutzte Variable
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
   ! 5.2.2 Phosphorfreisetzung beim Abbau org. Kohlenstoffverbidungen ok
   bsbctP(1) = transfer_quantity_p(2+(i-1)*number_trans_quant) ! Phosphorfreisetzung beim Abbau org. Kohlenstoffverbidungen
   bsbctP(2) = bsbctP(1)
   ! direkt aus QSimDatenfelder Qmx_PK=transfer_parameter_p(32) ! max. Phosphoranteil Kiesel-Algenbiomasse
   Q_PK(1) = planktonic_variable_p(31+nk) ! Phosphoranteil der Kiesel-Algenbiomasse
   Q_PK(2) = Q_PK(1)
   do j = 1,num_lev_trans ! P-Aufnahmerate der Kiesel-Algen tiefenaufgelöst
      up_PKz(j,1) = trans_quant_vert_p(j+(5-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert)
      up_PKz(j,2) = up_PKz(j,1)
   end do
   ! direkt aus QSimDatenfelder Qmx_PG=transfer_parameter_p(11) ! max. P-Gehalt der Grünalgen
   Q_PG(1) = planktonic_variable_p(34+(i-1)*number_plankt_vari) ! Phosphornteil der Algenbiomasse gruen
   Q_PG(2) = Q_PG(1)
   ! Aufnahmeraten kommen aus den Algenroutinen
   do j = 1,num_lev_trans ! P-Aufnahmerate der Grün-Algen, tiefenaufgelöst
      up_PGz(j,1) = trans_quant_vert_p(j+(6-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !### veri13.3 ### 0.01
      up_PGz(j,2) = up_PGz(j,1)
   end do
   ! direkt aus QSimDatenfelder Qmx_PB=transfer_parameter_p(55) ! max. Phosphoranteil in der Blau-Algenbiomasse
   Q_PB(1) = planktonic_variable_p(36+nk) ! Phosphoranteil der Blau-Algenbiomasse
   Q_PB(2) = Q_PB(1)
   do j = 1,num_lev_trans ! P-Aufnahmerate der Blau-Algen, tiefenaufgelöst
      up_PBz(j,1) = trans_quant_vert_p(j+(7-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert)
      up_PBz(j,2) = up_PBz(j,1)
   end do
   epl0(1) = 0.0        ! keine Einleitung
   do j = 1,num_lev ! geloester ortho-Phosphat-P, tiefenaufgelöst
      gelpz(j,1) = plankt_vari_vert_p(j+(6-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev)
      gelpz(j,2) = gelpz(j,1)
   end do
   ! do j=1,num_lev_trans ! ???
   !    dalggz(j,1) = trans_quant_vert_p(j+(13-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
   !    dalggz(j,2) = dalggz(j,1)
   !    dalgkz(j,1) = trans_quant_vert_p(j+(12-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
   !    dalgkz(j,2) = dalgkz(j,1)
   !    dalgbz(j,1) = trans_quant_vert_p(j+(14-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
   !    dalgbz(j,2) = dalgbz(j,1)
   ! end do
   agrtbr(1) = transfer_quantity_p(49+(i-1)*number_trans_quant) ! ?
   agrtbr(2) = agrtbr(1)
   akitbr(1) = transfer_quantity_p(48+(i-1)*number_trans_quant) ! ?
   akitbr(2) = akitbr(1)
   abltbr(1) = transfer_quantity_p(50+(i-1)*number_trans_quant) ! ?
   abltbr(2) = abltbr(1)
   ! Verbrauch durch Wachstum planktischer Algen agrbrz(50,1000) *up_PGz(50,1000)  ok
   do j = 1,num_lev_trans ! brutto Wachstum Grün-Algen-Biomasse, tiefenaufgelöst
      agrbrz(j,1) = trans_quant_vert_p(j+(24-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! 0.1 !### veri13.3 ###
      agrbrz(j,2) = agrbrz(j,1)
   end do
   do j = 1,num_lev_trans ! Wachstum Kiesel+Blau Algen-Biomasse, tiefenaufgelöst
      akibrz(j,1) = trans_quant_vert_p(j+(23-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
      akibrz(j,2) = akibrz(j,1)
      ablbrz(j,1) = trans_quant_vert_p(j+(25-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
      ablbrz(j,2) = ablbrz(j,1)
   end do
   ! Freisetzung durch Respiration von planktischen Algen
   do j = 1,num_lev_trans !  Respirierte Algenbiomasse kiesel, grün, blau, tiefenaufgelöst
      algakz(j,1) = trans_quant_vert_p(j+(18-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
      algakz(j,2) = algakz(j,1)
      algagz(j,1) = trans_quant_vert_p(j+(19-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
      algagz(j,2) = algagz(j,1)
      algabz(j,1) = trans_quant_vert_p(j+(20-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
      algabz(j,2) = algabz(j,1)
   end do
   hJPO4(1,1) = benthic_distribution_p(32+(i-1)*number_benth_distr) ! Phosphat-Freisetzung aus dem Sediment ! 0.01 !### veri13.3 ###
   hJPO4(1,2) = hJPO4(1,1)
   nkzs(1) = 1         ! nur eine Tiefenschicht
   nkzs(2) = nkzs(1)
   dH2D = -2.0         ! bisher nur 2D-Tiefengemittelt ??? konstante Tiefenschichtung ???
   dH2De = 0.0   ! unbenutzt
   mstr = 1            ! Strangzähler
   iorLa(1) = 0        ! AnfangsKnoten der Linienquelle; nicht verwendet
   iorLe(1) = 0        ! EndKnoten der Linienquelle; nicht verwendet
   ieinLs(1) = 0       ! null Linien-Einleitungen
   ieinLs(2) = ieinLs(1)
   flae(1) = 1000.0 !! unbenutzt da keine Einleitung
   flae(2) = flae(1)
   qeinlL(1) = 0.0           ! Zufluss Linienquelle; nicht verwendet
   gPL(1) = 0.0            ! Phosphatgehalt Linienquelle; nicht verwendet
   gesPL(1) = 0.0            ! Phosphatgehalt Linienquelle; nicht verwendet
   ! neu in 13_3
   ! hgesPz(1,1,1) = gesp(1)  ! hgesPz(mstr,1,ior) = gesP(ior)
   do j = 1,num_lev_trans !  Respirierte Algenbiomasse kiesel, grün, blau, tiefenaufgelöst
      hgesPz(1,j,1) = plankt_vari_vert_p(j+(15-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev)
      hgesPz(1,j,2) = hgesPz(1,1,1)
   end do
   algdrk(1) = benthic_distribution_p(38+(i-1)*number_benth_distr) ! Kiesel-Algen-Konsum-bentisch (Muscheln) in mg/l
   algdrk(2) = algdrk(1)
   algdrg(1) = benthic_distribution_p(40+(i-1)*number_benth_distr) ! grün-Algen-Konsum-bentisch (Muscheln) in mg/l
   algdrg(2) = algdrg(1)
   algdrb(1) = benthic_distribution_p(41+(i-1)*number_benth_distr) ! blau-Algen-Konsum-bentisch (Muscheln) in mg/l
   algdrb(2) = algdrb(1)
   itags = tag           ! Tag im Monat module::modell zeitsekunde()    (unbenutzt)
   monats = monat          ! Monat im Jahr module::modell zeitsekunde() (unbenutzt)
   uhrz = uhrzeit_stunde ! Uhrzeit module::modell zeitsekunde() (unbenutzt)
   ! azStrs=1 in QSim3D
   !if(i.eq.kontrollknoten) print*,'po4s_huelle vorher knoten ', i,' gelp=', gelp(1) &
   !                              ,' gesp=', gesp(1) !&
   !                              !,' deltat=', tflie,' Tiefe=',tiefe(1) &
   !                              !,dzres2(1),Q_PG(1),aki(1)
   if (iglob == kontrollknoten) print*,'po4s vorher: gesp,gelp,Q_PK,bl01',gesp(1),gelp(1),Q_PK(1),bl01(1)
   
   ! qsim13.301_28mae18=13.401_15okt18
   call po4s(gelp,flag,elen,ior,tiefe                                &
             ,dalggr,dalgki,dalgag,dalgak                                      &
             ,ep,qeinl,vabfl,anze,tflie,dzres1,dzres2                          &
             ,jiein,sedalk,sedalb,sedalg                                       &
             ,albewg,alberg,albewk,alberk,resdr,aki,agr,exdrvk,exdrvg,pl0      &
             ,abl,dalgbl,dalgab,exdrvb,gesP,orgCsd                             &
             ,zooind,GROT,pZoo,egesP,ilbuhn,iwied                             &
             ,CD,CP,CM,BAC,bsbctP,Qmx_PK,Q_PK,up_PKz                           &
             ,Qmx_PG,Q_PG,up_PGz,Qmx_PB,Q_PB,up_PBz,epl0                       &
             ,gelpz,agrtbr,akitbr,abltbr,agrbrz                                &
             ,akibrz,ablbrz,algakz,algagz,algabz,hJPO4,nkzs,dH2D               &
             ,dH2De,mstr,iorLa,iorLe,ieinLs,flae,qeinlL,gPL,gesPL,hgesPz       &
             ,algdrk,algdrg,algdrb,itags,monats,uhrz,azStrs                    &
             ,kontroll ,iglob )
   ! Version 13.3
   !            call po4s(gelp,flag,elen,ior,tiefe,dalggr,dalgki,dalgag,dalgak,ep,qeinl,vabfl,anze,tflie        &
   !                     ,dzres1,dzres2,jiein,sedalk,sedalb,sedalg,albewg,alberg,albewk,alberk,resdr            &
   !                     ,aki,agr,exdrvk,exdrvg,pl0,abl,dalgbl,dalgab,exdrvb,gesP,orgCsd,zooind,GRote           &
   !                     ,pZoo,egesP,ilbuhn,iwied,CD,CP,CM,BAC,bsbctP,Qmx_PK,Q_PK,up_PKz,Qmx_PG,Q_PG            &
   !                     ,up_PGz,Qmx_PB,Q_PB,up_PBz,epl0,gelpz,agrtbr,akitbr,abltbr                             &
   !                     ,agrbrz,akibrz,ablbrz,algakz,algagz,algabz,hJPO4,nkzs,dH2D,dH2De,mstr,iorLa,iorLe      &
   !                     ,ieinLs,flae,qeinlL,gPL,gesPL                                                          &
   !     &               ,hgesPz,algdrk,algdrg,algdrb,itags,monats,uhrz,1)    ! azStrs=1 in QSim3D
   !if(i.eq.kontrollknoten) print*,'po4s_huelle nachher knoten ', i,' gelp=' &
   !                              , gelp(1),' gesp=', gesp(1),' pl0=',pl0(1)
   if (iglob == kontrollknoten) print*,'po4s nachher: gesp,gelp,Q_PK,bl01',gesp(1),gelp(1),Q_PK(1),bl01(1)
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Datenrückgabe:
   do k = 1,number_plankt_vari
      if (isnan(planktonic_variable_p(k+nk))) then
         print*,'nach po4s: isnan(planktonic_variable_p  node#',iglob,' variable# ',k
         if (meinrang == 0)print*,'planktonic_variable_name:',planktonic_variable_name(k)
      endif
   end do
   planktonic_variable_p( 6+nk) = gelp(1)
   !planktonic_variable_p(58+nk) = pl0(1)! Rückgabewert nur bei Einmischung durch Einleitung (in QSim3D nicht verwendet)
   planktonic_variable_p(68+nk) = gesp(1)
   !planktonic_variable_p(31+nk) = Q_PK(1)! Rückgabewert nur bei Einmischung durch Einleitung (in QSim3D nicht verwendet)
   !planktonic_variable_p(34+nk) = Q_PG(1) ! Rückgabewert nur bei Einmischung durch Einleitung (in QSim3D nicht verwendet)
   !planktonic_variable_p(36+nk) = Q_PB(1) ! Rückgabewert nur bei Einmischung durch Einleitung (in QSim3D nicht verwendet)
   do j = 1,num_lev
      plankt_vari_vert_p(j+(6-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = &
                                                                                  gelpz(j,1) ! gelöster Phosphor tiefenaufgelöst
      plankt_vari_vert_p(j+(15-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = &
                                                                                   hgesPz(1,j,1) ! gesamt Phosphor ?? tiefenaufgelöst
   end do ! alle j tiefenlevels
   return
end subroutine po4s_huelle

!> Subroutine aufteilung_po4s() Quelle: po4s_huelle.f95\n
!! zurück: \ref lnk_phosphor, siehe auch: \ref lnk_po4s_aufteilung \n
!! Subroutine funktionslos. Setzung erfolgt inzwischen in randwert_planctonic().
subroutine aufteilung_po4s()
   !phosphor aufteilung ....
   !              hcgelpE = eP(iein) RB
   !              hcgespE = egesP(iein) RB
   !              hcpl0E = epl0(iein)??? wo kommt das her ???
   return
end subroutine aufteilung_po4s
