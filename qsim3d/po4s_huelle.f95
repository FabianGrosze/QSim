!---------------------------------------------------------------------------------------
!
!   QSim - Programm zur Simulation der Wasserqualität
!
!   Copyright (C) 2020 Bundesanstalt für Gewässerkunde, Koblenz, Deutschland, http://www.bafg.de
!
!   Dieses Programm ist freie Software. Sie können es unter den Bedingungen der 
!   GNU General Public License, Version 3,
!   wie von der Free Software Foundation veröffentlicht, weitergeben und/oder modifizieren. 
!   Die Veröffentlichung dieses Programms erfolgt in der Hoffnung, daß es Ihnen von Nutzen sein wird, 
!   aber OHNE IRGENDEINE GARANTIE, sogar ohne die implizite Garantie der MARKTREIFE oder der VERWENDBARKEIT FÜR EINEN BESTIMMTEN ZWECK. 
!   Details finden Sie in der GNU General Public License.
!   Sie sollten ein Exemplar der GNU General Public License zusammen mit diesem Programm erhalten haben. 
!   Falls nicht, siehe http://www.gnu.org/licenses/.  
!   
!	Programmiert von:
!	1979 bis 2018 Volker Kirchesch
!	seit 2011 Jens Wyrwa, Wyrwa@bafg.de
!
!---------------------------------------------------------------------------------------

!> \page Phosphor Phosphor
!! <center> 
!! \image html phosphor2.png "Stationen im Phosphorkreislaufs"
!! </center>
!!
!! <h2>Herkunft</h2>
!!     po4s()\n
!!     EIN PROGRAMM zu Berechnung des o-PO4-Gehalts  \n                    
!!     AUTOR : VOLKER KIRCHESCH            \n                               
!!     entnommen aus Version qsim13.301_28mae18\n 
!!
!! <h2>Teilprozesse</h2>
!! <h3>gelöster Phosphor</h3>
!! <ul>
!! <li> Verbrauch durch Wachstum planktischer Algen </li>
!! <li> Verbrauch durch Wachstum bentischer Algen </li>
!! <li> Freisetzung durch Respiration von planktischen Algen </li>
!! <li> Freisetzung durch Respiration von bentischen Algen </li>
!! <li> Freisetzung beim Abbau von Kohlenstoffverbindungen</li>
!! <li> Freisetzung aus dem Sediment</li>
!! <li> Freisetzung durch Zooplankton</li>
!! <li> Freisetzung durch Muscheln (benthische Filtrierer)</li>
!! </ul>
!! <h3>gesamter Phosphor in der Wassersäule</h3>
!! <ul>
!! <li> Austrag durch Sedimentation von org. Kohlenstoffverbidungen </li>
!! <li> Austrag durch Sedimentation von planktischen Algen </li>
!! <li> Freisetzung aus dem Sediment</li>
!! <li> Freisetzung/Verbrauch Muscheln (benthische Filtrierer)</li>
!! <li> Freisetzung/Verbrauch bentische Algen</li>
!! </ul>
!! \n
!! <h2>Schnittstellenbeschreibung</h2>
!! SUBROUTINE po4s()\n
!! ( \ref gelp, \ref flag, \ref elen, \ref ior, \ref tiefe                                &\n
!! &, \ref dalggr, \ref dalgki, \ref dalgag, \ref dalgak                                      &\n
!! &, *ep*, \ref qeinl, \ref vabfl, \ref anze, \ref tflie, \ref dzres1, \ref dzres2                          &\n
!! &, \ref jiein, \ref sedalk, \ref sedalb, \ref sedalg                                       &\n
!! &, \ref albewg, \ref alberg, \ref albewk, \ref alberk, \ref resdr, \ref aki, \ref agr, \ref exdrvk, \ref exdrvg, \ref pl0      &\n
!! &, \ref abl, \ref dalgbl, \ref dalgab, \ref exdrvb, \ref gesp, \ref orgcsd                             &\n
!! &, \ref zooind, \ref grote, *pzoo*, *egesp*, \ref ilbuhn, \ref iwied                             &\n
!! &, *cd*, *cp*, \ref cm, \ref bac, \ref bsbctp, \ref qmx_pk, \ref q_pk, \ref up_pkz                           &\n
!! &, \ref qmx_pg, \ref q_pg, \ref up_pgz, \ref qmx_pb, \ref q_pb, \ref up_pbz, *epl0*                       &\n
!! &, \ref gelpz, \ref agrtbr, \ref akitbr, \ref abltbr, \ref agrbrz                                &\n
!! &, \ref akibrz, \ref ablbrz, \ref algakz, \ref algagz, \ref algabz, \ref hjpo4, \ref nkzs, \ref dh2d               &\n
!! &, \ref dh2de, \ref mstr, \ref iorla, \ref iorle, \ref ieinls, \ref flae, *qeinll*, *gpl*, *gespl*, \ref hgespz       &\n
!! &, \ref algdrk, \ref algdrg, \ref algdrb, \ref itags, \ref monats, \ref uhrz, \ref azstrs                    &\n
!! &, \ref kontroll , \ref iglob )    \n
!! \n
!! po4s() wird von der Hüllroutine po4s_huelle() aufgerufen. Zum Hüllroutinen-Konzept siehe: \ref hüllen
!! 
!! <h2>Rand und Anfangsbedingungen</h2>
!! Aufteilung im Zufluss mit naehr_start(); Siehe dazu auch \ref randbedingungen_ergaenzen .
!! 
!! <h2>Dokumentation</h2>
!! Bisher existiert eine Dokumentation des Phosphor-Moduls als Kapitel 9 der
!! <a href="./pdf/QSimDoku_ncycWy.pdf" target="_blank">Kurzdoku</a> (Version vom 22. Nov. 2017)
!! \n\n
!! zurück: \ref lnk_ueberblick, Quelle: po4s_huelle.f95

!> Beschreibung siehe \ref Phosphor, Quelle: po4s_huelle.f95
      SUBROUTINE po4s_huelle(i)
      use modell                                                 
      use QSimDatenfelder
      implicit none
      integer :: i,j,k,nk

      iglob=(i+meinrang*part)
      kontroll = iglob.eq.kontrollknoten ! Erweiterung QSim3D wy
      nk=(i-1)*number_plankt_vari

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Datenübergabe: 

      gelp(1) = planktonic_variable_p( 6+nk)  ! gelöster ortho-Phosphat-Phosphor tiefengemittelt
      gelp(2) = gelp(1)
      flag(1)=0         ! keine Einleitungen
      flag(2)=flag(1)
      elen(1)=1         ! Elementlänge (nicht verwendet)
      elen(2)=elen(1)
      ior=1             ! Laufindex
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
      qeinl(1)= 0.0      ! kein Abfluss Einleitung
      vabfl(1) = 2.5     ! wird nur bei Einleitungen verwendet, die sind aber ausgeschaltet.
      vabfl(2) = vabfl(1)
      anze=1            ! Anzahl der Profile im aktuellen Strang
      tflie = real(deltat)/86400 ! Umwandlung des Zeitschritts von integer sekunden (T-QSim) in real Tage (QSim)
      dzres1(1) = transfer_quantity_p(27+(i-1)*number_trans_quant) ! Grund-Respiration Konsumenten
      dzres1(2) = dzres1(1)
      dzres2(1) = transfer_quantity_p(28+(i-1)*number_trans_quant) ! Fraßabhängige Respirationsrate Konsumenten
      dzres2(2) = dzres2(1)

      jiein(1)=0        ! null Punkt-Einleitungen
      ! Phosphor-Austrag durch Sedimentation von Algen der Klassen grün, kiesel und blau
      sedalk(1)=benthic_distribution_p(26+(i-1)*number_benth_distr) ! Sedimentierte Menge an Kiesel-Algen
      sedalk(2) = sedalk(1)
      sedalb(1)=benthic_distribution_p(28+(i-1)*number_benth_distr) ! Sedimentierte Menge an Blau-Algen
      sedalb(2) = sedalb(1)
      sedalg(1)=benthic_distribution_p(27+(i-1)*number_benth_distr) ! Sedimentierte Menge an Grün-Algen
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
      resdr(1)=benthic_distribution_p(15+(i-1)*number_benth_distr) ! Respirationsrate benthischer Filtrierer (Dreissena-Muscheln)
      resdr(2) = resdr(1)
      aki(1) = planktonic_variable_p(8+(i-1)*number_plankt_vari) ! Anteil kiesel-Algen
      aki(2) = aki(1)
      agr(1) = planktonic_variable_p(9+(i-1)*number_plankt_vari) ! Anteil gruen-Algen                        
      agr(2) = agr(1)
      exdrvk(1)=benthic_distribution_p(29+(i-1)*number_benth_distr) ! exkretierte Biomasse der Muscheln beim Verzehr von Kiesel-Algen
      exdrvk(2) = exdrvk(1)
      exdrvg(1)=benthic_distribution_p(30+(i-1)*number_benth_distr) ! exkretierte Biomasse der Muscheln beim Verzehr von Grün-Algen
      exdrvg(2) = exdrvg(1)
      exdrvb(1)=benthic_distribution_p(31+(i-1)*number_benth_distr) ! exkretierte Biomasse der Muscheln beim Verzehr von Blau-Algen
      exdrvb(2) = exdrvb(1)
      pl0(1)  = planktonic_variable_p(58+nk)  !  P/C Verhältnis von Phosphor zu Kohlenstoff in organischem Material
      pl0(2)  = pl0(1)

      abl(1) = planktonic_variable_p(10+nk) !  ???                                                              
      abl(2) = abl(1)
      dalgbl(1) = transfer_quantity_p(22+(i-1)*number_trans_quant) ! Zuwachs Blau-Algen
      dalgbl(2) = dalgbl(1)
      dalgab(1) = transfer_quantity_p(25+(i-1)*number_trans_quant) ! Respiration Blau-Algen
      dalgab(2) = dalgab(1)
      exdrvb(1)=benthic_distribution_p(31+(i-1)*number_benth_distr) ! exkretierte Biomasse der Muscheln beim Verzehr von Blau-Algen
      exdrvb(2) = exdrvb(1)
      gesp(1) = planktonic_variable_p(68+nk)  ! gesamter ortho-Phosphat-Phosphor
      gesp(2) = gesp(1)
      ! Phosphor-Austrag durch Sedimentation von org. Kohlenstoffverbidungen   ok
      orgCsd(1,1)=benthic_distribution_p(6+(i-1)*number_benth_distr) ! Gesamtmasse Kohlenstoff, die je Zeitschritt sedimentiert
      orgCsd(1,2)=orgCsd(1,1) ! ### Feld-dimensionsänderung in neuer QSim-Version

      zooind(1) = planktonic_variable_p(50+(i-1)*number_plankt_vari) ! Anzahl der Rotatorien
      zooind(2) = zooind(1)
      ! GRote  direkt aus QSimDatenfelder =transfer_parameter_p(67) ! Gewicht einer Rotatorie µg  | Aparam.txt
      ! pZoo  JETZT direkt aus QSimDatenfelder  = transfer_value_p(3) !! Phosphoranteil in der Rotatorienbiomasse mgP/mgBiom.
      ! bl01(1) = planktonic_variable_p(44+(i-1)*number_plankt_vari) !! schwerabbaubare Kohlenstoffverbindungen als Sauerstoffäquivalent
      ! bl01(2) = bl01(1)
      ! bl02(1) = planktonic_variable_p(45+(i-1)*number_plankt_vari) ! leichtabbaubare Kohlenstoffverbindungen als Sauerstoffäquivalent
      ! bl02(2) = bl02(1)
      egesP(1) = 0.0       ! keine Einleitung
      ilbuhn=0          ! keine Buhnen
      iwied=0      ! unbenutzte Variable

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
      do j=1,num_lev_trans ! P-Aufnahmerate der Kiesel-Algen tiefenaufgelöst
         up_PKz(j,1) = trans_quant_vert_p(j+(5-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert)
         up_PKz(j,2) = up_PKz(j,1)
      end do

      ! direkt aus QSimDatenfelder Qmx_PG=transfer_parameter_p(11) ! max. P-Gehalt der Grünalgen
      Q_PG(1) = planktonic_variable_p(34+(i-1)*number_plankt_vari) ! Phosphornteil der Algenbiomasse gruen
      Q_PG(2) = Q_PG(1)
      ! Aufnahmeraten kommen aus den Algenroutinen
      do j=1,num_lev_trans ! P-Aufnahmerate der Grün-Algen, tiefenaufgelöst
         up_PGz(j,1) = trans_quant_vert_p(j+(6-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !### veri13.3 ### 0.01  
         up_PGz(j,2) = up_PGz(j,1)
      end do
      ! direkt aus QSimDatenfelder Qmx_PB=transfer_parameter_p(55) ! max. Phosphoranteil in der Blau-Algenbiomasse
      Q_PB(1) = planktonic_variable_p(36+nk) ! Phosphoranteil der Blau-Algenbiomasse
      Q_PB(2) = Q_PB(1)
      do j=1,num_lev_trans ! P-Aufnahmerate der Blau-Algen, tiefenaufgelöst
         up_PBz(j,1) = trans_quant_vert_p(j+(7-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert)
         up_PBz(j,2) = up_PBz(j,1)
      end do
      epl0(1) = 0.0        ! keine Einleitung

      do j=1,num_lev ! geloester ortho-Phosphat-P, tiefenaufgelöst
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
      do j=1,num_lev_trans ! brutto Wachstum Grün-Algen-Biomasse, tiefenaufgelöst
         agrbrz(j,1) = trans_quant_vert_p(j+(24-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! 0.1 !### veri13.3 ###
         agrbrz(j,2) = agrbrz(j,1)
      end do

      do j=1,num_lev_trans ! Wachstum Kiesel+Blau Algen-Biomasse, tiefenaufgelöst
         akibrz(j,1) = trans_quant_vert_p(j+(23-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
         akibrz(j,2) = akibrz(j,1)
         ablbrz(j,1) = trans_quant_vert_p(j+(25-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
         ablbrz(j,2) = ablbrz(j,1)
      end do
      ! Freisetzung durch Respiration von planktischen Algen 
      do j=1,num_lev_trans !  Respirierte Algenbiomasse kiesel, grün, blau, tiefenaufgelöst
         algakz(j,1) = trans_quant_vert_p(j+(18-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
         algakz(j,2) = algakz(j,1)
         algagz(j,1) = trans_quant_vert_p(j+(19-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
         algagz(j,2) = algagz(j,1)
         algabz(j,1) = trans_quant_vert_p(j+(20-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
         algabz(j,2) = algabz(j,1)
      end do
      hJPO4(1,1)= benthic_distribution_p(32+(i-1)*number_benth_distr) ! Phosphat-Freisetzung aus dem Sediment ! 0.01 !### veri13.3 ###
      hJPO4(1,2)= hJPO4(1,1)
      nkzs(1)=1         ! nur eine Tiefenschicht
      nkzs(2)= nkzs(1)
      dH2D=-2.0         ! bisher nur 2D-Tiefengemittelt ??? konstante Tiefenschichtung ???

      dH2De =0.0   ! unbenutzt
      mstr=1            ! Strangzähler
      iorLa(1)=0        ! AnfangsKnoten der Linienquelle; nicht verwendet
      iorLe(1)=0        ! EndKnoten der Linienquelle; nicht verwendet
      ieinLs(1)=0       ! null Linien-Einleitungen
      ieinLs(2)=ieinLs(1)
      flae(1) = 1000.0 !! unbenutzt da keine Einleitung
      flae(2) = flae(1)
      qeinlL(1)=0.0           ! Zufluss Linienquelle; nicht verwendet
      gPL(1) = 0.0            ! Phosphatgehalt Linienquelle; nicht verwendet 
      gesPL(1) = 0.0            ! Phosphatgehalt Linienquelle; nicht verwendet 

      ! neu in 13_3
      ! hgesPz(1,1,1) = gesp(1)  ! hgesPz(mstr,1,ior) = gesP(ior)
      do j=1,num_lev_trans !  Respirierte Algenbiomasse kiesel, grün, blau, tiefenaufgelöst
         hgesPz(1,j,1) = plankt_vari_vert_p(j+(15-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev)
         hgesPz(1,j,2) = hgesPz(1,1,1)
      end do
      algdrk(1)=benthic_distribution_p(38+(i-1)*number_benth_distr) ! Kiesel-Algen-Konsum-bentisch (Muscheln) in mg/l
      algdrk(2) = algdrk(1)
      algdrg(1)=benthic_distribution_p(40+(i-1)*number_benth_distr) ! grün-Algen-Konsum-bentisch (Muscheln) in mg/l
      algdrg(2) = algdrg(1)
      algdrb(1)=benthic_distribution_p(41+(i-1)*number_benth_distr) ! blau-Algen-Konsum-bentisch (Muscheln) in mg/l
      algdrb(2) = algdrb(1)
      itags=tag           ! Tag im Monat module::modell zeitsekunde() 	(unbenutzt)
      monats=monat          ! Monat im Jahr module::modell zeitsekunde() (unbenutzt)
      uhrz=uhrzeit_stunde ! Uhrzeit module::modell zeitsekunde() (unbenutzt)
      ! azStrs=1 in QSim3D    

      !if(i.eq.kontrollknoten) print*,'po4s_huelle vorher knoten ', i,' gelp=', gelp(1) &
      !                              ,' gesp=', gesp(1) !&
      !                              !,' deltat=', tflie,' Tiefe=',tiefe(1) &
      !                              !,dzres2(1),Q_PG(1),aki(1)
      if(iglob.eq.kontrollknoten) print*,'po4s vorher: gesp,gelp,Q_PK,bl01',gesp(1),gelp(1),Q_PK(1),bl01(1)
 
! qsim13.301_28mae18=13.401_15okt18
      call po4s(gelp,flag,elen,ior,tiefe                                &
     &,dalggr,dalgki,dalgag,dalgak                                      &
     &,ep,qeinl,vabfl,anze,tflie,dzres1,dzres2                          &
     &,jiein,sedalk,sedalb,sedalg                                       &
     &,albewg,alberg,albewk,alberk,resdr,aki,agr,exdrvk,exdrvg,pl0      &
     &,abl,dalgbl,dalgab,exdrvb,gesP,orgCsd                             &
     &,zooind,GRote,pZoo,egesP,ilbuhn,iwied                             &
     &,CD,CP,CM,BAC,bsbctP,Qmx_PK,Q_PK,up_PKz                           &
     &,Qmx_PG,Q_PG,up_PGz,Qmx_PB,Q_PB,up_PBz,epl0                       &
     &,gelpz,agrtbr,akitbr,abltbr,agrbrz                                &
     &,akibrz,ablbrz,algakz,algagz,algabz,hJPO4,nkzs,dH2D               &
     &,dH2De,mstr,iorLa,iorLe,ieinLs,flae,qeinlL,gPL,gesPL,hgesPz       &
     &,algdrk,algdrg,algdrb,itags,monats,uhrz,azStrs                    &
     &,kontroll ,iglob )    

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
      if(iglob.eq.kontrollknoten) print*,'po4s nachher: gesp,gelp,Q_PK,bl01',gesp(1),gelp(1),Q_PK(1),bl01(1)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Datenrückgabe: 

      do k=1,number_plankt_vari
         if(isnan(planktonic_variable_p(k+nk)))then
            print*,'nach po4s: isnan(planktonic_variable_p  node#',iglob,' variable# ',k
            if(meinrang==0)print*,'planktonic_variable_name:',planktonic_variable_name(k)
         endif
      end do

      planktonic_variable_p( 6+nk) = gelp(1)

      !planktonic_variable_p(58+nk) = pl0(1)! Rückgabewert nur bei Einmischung durch Einleitung (in QSim3D nicht verwendet)

      planktonic_variable_p(68+nk) = gesp(1)

      !planktonic_variable_p(31+nk) = Q_PK(1)! Rückgabewert nur bei Einmischung durch Einleitung (in QSim3D nicht verwendet)

      !planktonic_variable_p(34+nk) = Q_PG(1) ! Rückgabewert nur bei Einmischung durch Einleitung (in QSim3D nicht verwendet)
      !planktonic_variable_p(36+nk) = Q_PB(1) ! Rückgabewert nur bei Einmischung durch Einleitung (in QSim3D nicht verwendet)
      do j=1,num_lev
         plankt_vari_vert_p(j+(6-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) =  &
         gelpz(j,1) ! gelöster Phosphor tiefenaufgelöst 
         plankt_vari_vert_p(j+(15-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) =  &
         hgesPz(1,j,1) ! gesamt Phosphor ?? tiefenaufgelöst 
      end do ! alle j tiefenlevels


      RETURN 
      END subroutine po4s_huelle 
!----+-----+----
!> \page po4s_aufteilung Phosphor-Aufteilung Zufluss
!! Phophor-Gehalte im Zufluss werden auf den Maximalwert gesetzt in randwert_planctonic():\n
!!     planktonic_variable(31+nk)= transfer_parameter_p(32) ! *Q_PK*=*Qmx_PK* \n
!!     planktonic_variable(34+nk)= transfer_parameter_p(11) ! *Q_PG*=*Qmx_PG* \n
!!     planktonic_variable(36+nk)= transfer_parameter_p(55) ! *Q_PB*=*Qmx_PB* \n
!! \n\n
!! zurück: \ref Phosphor und \ref randbedingungen_ergaenzen, Quelle: po4s_huelle.f95

!> Subroutine aufteilung_po4s() Quelle: po4s_huelle.f95\n
!! zurück: \ref Phosphor, siehe auch: \ref po4s_aufteilung \n
!! Subroutine funktionslos. Setzung erfolgt inzwischen in randwert_planctonic().
      SUBROUTINE aufteilung_po4s()
!phosphor aufteilung ....
!              hcgelpE = eP(iein) RB
!              hcgespE = egesP(iein) RB
!              hcpl0E = epl0(iein)??? wo kommt das her ???
      RETURN 
      END subroutine aufteilung_po4s

!! Hallo Volker,\n
!! weißt Du, wo in QSim der Wert der Variable epl0, also des Phosphor-Kohlenstoff-Verhältnissen in der Einleitung bestimmt wird?
!! \n Als Randwert ist er in EREIGG.txt nicht drin und die Subroutine po4s() geht davon aus, das sie diese Variable übergeben bekommt.
!! \n Gruß Jens 
!!     orgc.f90:      pl0(ior) = orgPn/(ocsbt/2.8) \n

!! <h1>Schnittstellenbeschreibung</h1>
!! 
!! po4s_huelle() ist eine QSim3d Hüllroutine (siehe dazu auch: \ref hüllen) zum Aufruf der QSim Subroutine po4s()
!! mittels der folgenden Übergabeparameter:
!! \n
!!<table >
!!<tr><th>     Variablen-Name QSim	 </th><th> Daten-Feld T-QSim	</th><th> Beschreibung </th></tr>
!!<tr><td> gelp(1) </td><td> planktische_variablen::planktonic_variable_p ( 6+nk) </td><td> gelöster ortho-Phosphat-Phosphor tiefengemittelt </td></tr>
!!<tr><td> flag(1)</td><td>0 </td><td> keine Einleitungen </td></tr>
!!<tr><td> elen(1)</td><td>1 </td><td> Elementlänge (nicht verwendet) </td></tr>
!!<tr><td> ior</td><td>1 </td><td> Laufindex </td></tr>
!!<tr><td> tiefe(1) </td><td> randbedingungen rb_hydraul_p (2+(i-1)*number_rb_hydraul) </td><td> Wassertiefe  </td></tr>
!!<tr><td> </td><td> </td><td> </td></tr>
!!<tr><td> dalggr(1) </td><td> uebergabe_werte transfer_quantity_p (21+(i-1)*number_trans_quant) </td><td> Zuwachs Grün-Algen </td></tr>
!!<tr><td> dalgki(1) </td><td> uebergabe_werte transfer_quantity_p (20+(i-1)*number_trans_quant) </td><td> Zuwachs Kiesel-Algen </td></tr>
!!<tr><td> dalgag(1) </td><td> uebergabe_werte transfer_quantity_p (24+(i-1)*number_trans_quant) </td><td> Respiration Grün-Algen </td></tr>
!!<tr><td> dalgak(1) </td><td> uebergabe_werte transfer_quantity_p (23+(i-1)*number_trans_quant) </td><td> Respiration Kiesel-Algen </td></tr>
!!<tr><td> </td><td> </td><td> </td></tr>
!!<tr><td> ep(1) </td><td> 0.0 </td><td> keine Einleitung </td></tr>
!!<tr><td> qeinl(1)</td><td> 0.0 </td><td> kein Abfluss Einleitung </td></tr>
!!<tr><td> vabfl(1) </td><td> 2.5 </td><td> wird nur bei Einleitungen verwendet, die sind aber ausgeschaltet. </td></tr>
!!<tr><td> anze</td><td>1 </td><td> Anzahl der Profile im aktuellen Strang </td></tr>
!!<tr><td> tflie </td><td> real(deltat)/86400 </td><td> Umwandlung des Zeitschritts von integer sekunden (T-QSim) in real Tage (QSim) </td></tr>
!!<tr><td> dzres1(1) </td><td> uebergabe_werte transfer_quantity_p (27+(i-1)*number_trans_quant) </td><td> Grund-Respiration Konsumenten </td></tr>
!!<tr><td> dzres2(1) </td><td> uebergabe_werte transfer_quantity_p (28+(i-1)*number_trans_quant) </td><td> Fraßabhängige Respirationsrate Konsumenten </td></tr>
!!<tr><td> </td><td> </td><td> </td></tr>
!!<tr><td> jiein(1)</td><td>0 </td><td> null Punkt-Einleitungen </td></tr>
!!<tr><td> sedalk(1)</td><td>benthische_verteilungen::benthic_distribution_p (26+(i-1)*number_benth_distr) </td><td> Sedimentierte Menge an Kiesel-Algen (Phosphor Austrag) </td></tr>
!!<tr><td> sedalb(1)</td><td>benthische_verteilungen::benthic_distribution_p (28+(i-1)*number_benth_distr) </td><td> Sedimentierte Menge an Blau-Algen </td></tr>
!!<tr><td> sedalg(1)</td><td>benthische_verteilungen::benthic_distribution_p (27+(i-1)*number_benth_distr) </td><td> Sedimentierte Menge an Grün-Algen </td></tr>
!!<tr><td> </td><td> </td><td> </td></tr>
!!<tr><td> albewg(1) </td><td> benthische_verteilungen::benthic_distribution_p (13+(i-1)*number_benth_distr) </td><td> Wachstum benthischer gruen-Algen (Phosphor Verbrauch) </td></tr>
!!<tr><td> alberg(1) </td><td> benthische_verteilungen::benthic_distribution_p (11+(i-1)*number_benth_distr) </td><td> Respiration benthischer gruen-Algen (Phosphor Freisetzung) </td></tr>
!!<tr><td> albewk(1) </td><td> benthische_verteilungen::benthic_distribution_p (14+(i-1)*number_benth_distr) </td><td> Wachstum benthischer kiesel-Algen </td></tr>
!!<tr><td> alberk(1) </td><td> benthische_verteilungen::benthic_distribution_p (12+(i-1)*number_benth_distr) </td><td> Respiration benthischer kiesel-Algen </td></tr>
!!<tr><td> resdr(1)</td><td>benthische_verteilungen::benthic_distribution_p (15+(i-1)*number_benth_distr) </td><td> Respirationsrate benthischer Filtrierer (Dreissena-Muscheln) </td></tr>
!!<tr><td> aki(1) </td><td> planktische_variablen::planktonic_variable_p (8+(i-1)*number_plankt_vari) </td><td> Anteil kiesel-Algen </td></tr>
!!<tr><td> agr(1) </td><td> planktische_variablen::planktonic_variable_p (9+(i-1)*number_plankt_vari) </td><td> Anteil gruen-Algen  </td></tr>
!!<tr><td> exdrvk(1)</td><td>benthische_verteilungen::benthic_distribution_p (29+(i-1)*number_benth_distr) </td><td> exkretierte Biomasse der Muscheln beim Verzehr von Kiesel-Algen </td></tr>
!!<tr><td> exdrvg(1)</td><td>benthische_verteilungen::benthic_distribution_p (30+(i-1)*number_benth_distr) </td><td> exkretierte Biomasse der Muscheln beim Verzehr von Grün-Algen </td></tr>
!!<tr><td> exdrvb(1)</td><td>benthische_verteilungen::benthic_distribution_p (31+(i-1)*number_benth_distr) </td><td> exkretierte Biomasse der Muscheln beim Verzehr von Blau-Algen </td></tr>
!!<tr><td> \ref pl0  </td><td> </td><td>  P/C Verhältnis von Phosphor zu Kohlenstoff in organischem Material </td></tr>
!!<tr><td> </td><td> </td><td> </td></tr>
!!<tr><td> abl(1) </td><td> planktische_variablen::planktonic_variable_p (10+nk) </td><td>  ???  </td></tr>
!!<tr><td> dalgbl(1) </td><td> uebergabe_werte transfer_quantity_p (22+(i-1)*number_trans_quant) </td><td> Zuwachs Blau-Algen </td></tr>
!!<tr><td> dalgab(1) </td><td> uebergabe_werte transfer_quantity_p (25+(i-1)*number_trans_quant) </td><td> Respiration Blau-Algen </td></tr>
!!<tr><td> exdrvb(1)</td><td>benthische_verteilungen::benthic_distribution_p (31+(i-1)*number_benth_distr) </td><td> exkretierte Biomasse der Muscheln beim Verzehr von Blau-Algen </td></tr>
!!<tr><td> gesp(1) </td><td> planktische_variablen::planktonic_variable_p (68+nk)  </td><td> gesamter ortho-Phosphat-Phosphor </td></tr>
!!<tr><td> orgCsd(1)</td><td>benthische_verteilungen::benthic_distribution_p (6+(i-1)*number_benth_distr) </td><td> Gesamtmasse Kohlenstoff, die je Zeitschritt sedimentiert </td></tr>
!!<tr><td> </td><td> </td><td> </td></tr>
!!<tr><td> zooind(1) </td><td> planktische_variablen::planktonic_variable_p (50+(i-1)*number_plankt_vari) </td><td> Anzahl der Rotatorien </td></tr>
!!<tr><td> GRote</td><td> ! direkt aus QSimDatenfelder </td><td> Gewicht einer Rotatorie µg  | Aparam.txt </td></tr>
!!<tr><td> pZoo  </td><td> transfer_value_p(3) !! Phosphoranteil in der Rotatorienbiomasse mgP/mgBiom. </td></tr>
!!<tr><td> bl01(1) </td><td> planktische_variablen::planktonic_variable_p (44+(i-1)*number_plankt_vari) </td><td> schwerabbaubare Kohlenstoffverbindungen als Sauerstoffäquivalent </td></tr>
!!<tr><td> bl02(1) </td><td> planktische_variablen::planktonic_variable_p (45+(i-1)*number_plankt_vari) </td><td> leichtabbaubare Kohlenstoffverbindungen als Sauerstoffäquivalent </td></tr>
!!<tr><td> egesP(1) </td><td> 0.0 </td><td> keine Einleitung </td></tr>
!!<tr><td> ilbuhn</td><td>0 </td><td> keine Buhnen </td></tr>
!!<tr><td> iwied</td><td>0 </td><td> unbenutzte Variable
!!<tr><td> </td><td> </td><td> </td></tr>
!!<tr><td> CD(1,1) </td><td> planktische_variablen::planktonic_variable_p (37+(i-1)*number_plankt_vari) </td><td> leicht abbaubare gelöste organische C-Verbindungen </td></tr>
!!<tr><td> CD(2,1) </td><td> planktische_variablen::planktonic_variable_p (38+(i-1)*number_plankt_vari) </td><td> schwer abbaubare gelöste organische C-Verbindungen </td></tr>
!!<tr><td> CP(1,1) </td><td> planktische_variablen::planktonic_variable_p (39+(i-1)*number_plankt_vari) </td><td> leicht abbaubare partikuläre organische C-Verbindungen </td></tr>
!!<tr><td> CP(2,1) </td><td> planktische_variablen::planktonic_variable_p (40+(i-1)*number_plankt_vari) </td><td> schwer abbaubare partikuläre organische C-Verbindungen </td></tr>
!!<tr><td> CM(1) </td><td> planktische_variablen::planktonic_variable_p (41+(i-1)*number_plankt_vari) </td><td> monomolekularen organischen C-Verbindungen </td></tr>
!!<tr><td> BAC(1) </td><td> planktische_variablen::planktonic_variable_p (42+(i-1)*number_plankt_vari) </td><td> Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen </td></tr>
!!<tr><td> bsbctP(1) </td><td> uebergabe_werte transfer_quantity_p (2+(i-1)*number_trans_quant) </td><td> Phosphorfreisetzung beim Abbau org. Kohlenstoffverbidungen </td></tr>
!!<tr><td> Qmx_PK</td><td> ! direkt aus QSimDatenfelder </td><td> max. Phosphoranteil Kiesel-Algenbiomasse </td></tr>
!!<tr><td> Q_PK(1) </td><td> planktische_variablen::planktonic_variable_p (31+nk) </td><td> Phosphoranteil der Kiesel-Algenbiomasse </td></tr>
!!<tr><td>    up_PKz(j,1) </td><td> uebergabe_werte trans_quant_vert_p (j+(5-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> P-Aufnahmerate der Kiesel-Algen tiefenaufgelöst </td></tr>
!!<tr><td> </td><td> </td><td> </td></tr>
!!<tr><td> Qmx_PG</td><td>! direkt aus QSimDatenfelder </td><td> max. P-Gehalt der Grünalgen </td></tr>
!!<tr><td> Q_PG(1) </td><td> planktische_variablen::planktonic_variable_p (34+(i-1)*number_plankt_vari) </td><td> Phosphornteil der Algenbiomasse gruen </td></tr>
!!<tr><td> Q_PG(2) </td><td> Q_PG(1) </td></tr>
!!<tr><td>    up_PGz(j,1) </td><td> uebergabe_werte trans_quant_vert_p (j+(6-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> P-Aufnahmerate der Grün-Algen, tiefenaufgelöst , Raten kommen aus den Algenroutinen </td></tr>
!!<tr><td> Qmx_PB</td><td>! direkt aus QSimDatenfelder </td><td> max. Phosphoranteil in der Blau-Algenbiomasse </td></tr>
!!<tr><td> Q_PB(1) </td><td> planktische_variablen::planktonic_variable_p (36+nk) </td><td> Phosphoranteil der Blau-Algenbiomasse </td></tr>
!!<tr><td>    up_PBz(j,1) </td><td> uebergabe_werte trans_quant_vert_p (j+(7-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> P-Aufnahmerate der Blau-Algen, tiefenaufgelöst </td></tr>
!!<tr><td> epl0(1) </td><td> 0.0  </td><td> keine Einleitung
!!<tr><td> </td><td> </td><td> </td></tr>
!!<tr><td>    gelpz(j,1) </td><td> planktische_variablen::plankt_vari_vert_p (j+(6-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) </td><td> geloester ortho-Phosphat-P, tiefenaufgelöst </td></tr>
!!<tr><td>    dalggz(j,1) </td><td> uebergabe_werte trans_quant_vert_p (j+(13-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> ??? </td></tr>
!!<tr><td>    dalgkz(j,1) </td><td> uebergabe_werte trans_quant_vert_p (j+(12-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> ??? </td></tr>
!!<tr><td>    dalgbz(j,1) </td><td> uebergabe_werte trans_quant_vert_p (j+(14-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> ??? </td></tr>
!!<tr><td> agrtbr(1) </td><td> uebergabe_werte transfer_quantity_p (49+(i-1)*number_trans_quant) </td><td> ? </td></tr>
!!<tr><td> akitbr(1) </td><td> uebergabe_werte transfer_quantity_p (48+(i-1)*number_trans_quant) </td><td> ? </td></tr>
!!<tr><td> abltbr(1) </td><td> uebergabe_werte transfer_quantity_p (50+(i-1)*number_trans_quant) </td><td> ? </td></tr>
!!<tr><td>    agrbrz(j,1) </td><td> uebergabe_werte trans_quant_vert_p (j+(24-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> Verbrauch durch Wachstum ? Grün-Algen-Biomasse, tiefenaufgelöst </td></tr>
!!<tr><td> </td><td> </td><td> </td></tr>
!!<tr><td>    akibrz(j,1) </td><td> uebergabe_werte trans_quant_vert_p (j+(23-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> Wachstum? Kiesel+Blau Algen-Biomasse, tiefenaufgelöst </td></tr>
!!<tr><td>    ablbrz(j,1) </td><td> uebergabe_werte trans_quant_vert_p (j+(25-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> </td></tr>
!!<tr><td>    algakz(j,1) </td><td> uebergabe_werte trans_quant_vert_p (j+(18-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> Freisetzung durch Respirierte Algenbiomasse kiesel, grün, blau, tiefenaufgelöst </td></tr>
!!<tr><td>    algagz(j,1) </td><td> uebergabe_werte trans_quant_vert_p (j+(19-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> </td></tr>
!!<tr><td>    algabz(j,1) </td><td> uebergabe_werte trans_quant_vert_p (j+(20-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) </td><td> </td></tr>
!!<tr><td> hJPO4(1,1)</td><td>benthische_verteilungen::benthic_distribution_p (32+(i-1)*number_benth_distr) </td><td> Phosphat-Freisetzung aus dem Sediment </td></tr>
!!<tr><td> nkzs(1)</td><td>1 </td><td> nur eine Tiefenschicht </td></tr>
!!<tr><td> dH2D</td><td>-2.0 </td><td> bisher nur 2D-Tiefengemittelt ??? konstante Tiefenschichtung ??? </td></tr>
!!<tr><td> </td><td> </td><td> </td></tr>
!!<tr><td> dH2De </td><td>0.0   </td><td> unbenutzt </td></tr>
!!<tr><td> mstr</td><td>1 </td><td> Strangzähler </td></tr>
!!<tr><td> iorLa(1)</td><td>0 </td><td> AnfangsKnoten der Linienquelle; nicht verwendet </td></tr>
!!<tr><td> iorLe(1)</td><td>0 </td><td> EndKnoten der Linienquelle; nicht verwendet </td></tr>
!!<tr><td> ieinLs(1)</td><td>0 </td><td> null Linien-Einleitungen </td></tr>
!!<tr><td> flae(1) </td><td> 1000.0 !! unbenutzt da keine Einleitung </td></tr>
!!<tr><td> qeinlL(1)</td><td>0.0 </td><td> Zufluss Linienquelle; nicht verwendet </td></tr>
!!<tr><td> gPL(1) </td><td> 0.0 </td><td> Phosphatgehalt Linienquelle; nicht verwendet  </td></tr>
!!<tr><td> gesPL(1) </td><td> 0.0 </td><td> Phosphatgehalt Linienquelle; nicht verwendet  </td></tr>
!! </table>
!! \n\n
!! zurück: \ref lnk_ueberblick, Quelle: po4s_module.f95



!!
!! <h2>Aufruf</h2>
!! po4s_huelle ist eine QSim3D Hüllroutine (siehe dazu auch: \ref hüllen) zum Aufruf der QSim1D Subroutine:\n\n
!!
!!       SUBROUTINE po4s(\ref gelp, \ref flag, \ref elen, \ref ior, \ref tiefe, \ref dalggr, \ref dalgki, \ref dalgag, \ref dalgak, *ep*, \ref qeinl, \ref vabfl, \ref anze, \ref tflie        &
!!                     , \ref dzres1, \ref dzres2, \ref jiein, \ref sedalk, \ref sedalb, \ref sedalg, \ref albewg, \ref alberg, \ref albewk, \ref alberk, \ref resdr            & 
!!                     , \ref aki, \ref agr, \ref exdrvk, \ref exdrvg, \ref pl0, \ref abl, \ref dalgbl, \ref dalgab, \ref exdrvb, \ref gesP, \ref orgCsd, \ref zooind, \ref GRote           &
!!                     , *pZoo*, *egesP*, \ref ilbuhn, \ref iwied, *CD*, *CP*, \ref CM, \ref BAC, \ref bsbctP, \ref Qmx_PK, \ref Q_PK, \ref up_PKz, \ref Qmx_PG, \ref Q_PG            &
!!                     , \ref up_PGz, \ref Qmx_PB, \ref Q_PB, \ref up_PBz, *epl0*, \ref gelpz, \ref agrtbr, \ref akitbr, \ref abltbr                             &
!!                     , \ref agrbrz, \ref akibrz, \ref ablbrz, \ref algakz, \ref algagz, \ref algabz, \ref hJPO4, \ref nkzs, \ref dH2D, \ref dH2De, \ref mstr, \ref iorLa, \ref iorLe      &
!!                     , \ref ieinLs, \ref flae, *qeinlL*, *gPL*, *gesPL*                                                          &
!!     &               , \ref hgesPz, \ref algdrk, \ref algdrg, \ref algdrb, \ref itags, \ref monats, \ref uhrz,1)    ! azStrs=1 in QSim3D    
!! \n\n 
!!
!! <h2>Übergabe-Parameter:</h2>
!!<table >
!!<tr><th>     Variablen-Name		</th><th> Beschreibung				</th><th> Kommuniziert mit:						</th><th> Daten-Typ, Feld-Dimension </th></tr>
!!<tr><td>     gelp			</td><td> geloester ortho-Phosphat-P, tiefengemittelt </td><td>  Transportkonzentration	po4s	</td><td> real, Dimension(1000) </td></tr>
!!<tr><td>     gesP 			</td><td> gesamt ortho-Phosphat-P, tiefengemittelt </td><td>  Transportkonzentration po4s	</td><td> real, Dimension(1000) </td></tr>
!!<tr><td>     pl0			</td><td> Verhältnis von Phosphor zu Kohlenstoff in organischem Material </td><td>  Transportkonzentration orgc		</td><td> real, Dimension(1000) </td></tr>
!!<tr><td>     gelpz			</td><td> tiefenaufgelöst gelp</td><td> 		</td><td> real, Dimension(50,1000) </td></tr>
!!<tr><td></td></tr> </td></tr>
!!<tr><td>     bsbctP			</td><td>  Phosphorfreisetzung beim Abbau org. Kohlenstoffverbidungen ??	</td><td>  orgc		</td><td> real, Dimension(1000) </td></tr>
!!<tr><td>     orgCsd 			</td><td>  Sedimentierte Menge an organischem Kohlenstoff (ohne lebende Algen und Rotatorien)	</td><td> ORGC		</td><td> real </td></tr>
!!<tr><td></td></tr>
!!<tr><td>     agr,aki,abl		</td><td>  Anteil gruen, kiesel und blau -Algen					</td><td> ALGAESGR, ALGAESKI, ALGAESBL	</td><td>   </td></tr>
!!<tr><td>     agrbrz,akibrz,ablbrz	</td><td>  Gehalt (Wachstum?) Biomasse gruen, kiesel und blau -Algen									</td><td> ALGAESGR, ALGAESKI, ALGAESBL	</td><td>   </td></tr>
!!<tr><td>     algagz,algakz,algabz	</td><td>  Respirierte Algenbiomasse der Algengruppen gruen, kiesel und blau, tiefenaufgelöst	</td><td> ALGAESGR, ALGAESKI, ALGAESBL	</td><td>  real, Dimension(50,1000) </td></tr>
!!<tr><td>     Q_PK,Q_PG,Q_P		</td><td>  Phosphoranteil der Algenbiomasse gruen, kiesel und blau 		</td><td>Transportkonzentrationen ALGAESGR, ALGAESKI, ALGAESBL	</td><td>  real, Dimension(50,1000)   </td></tr>
!!<tr><td>     Qmx_PG,Qmx_PK,Qmx_PB	</td><td>  max. Phosphoranteil der Algenbiomasse gruen, kiesel und blau </td><td> ALGAESGR, ALGAESKI, ALGAESBL	</td><td>   </td></tr>
!!<tr><td>     up_PGz,up_PKz,up_PBz	</td><td>  P-Aufnahmerate der Algengruppen gruen, kiesel und blau		</td><td> ALGAESGR, ALGAESKI, ALGAESBL	</td><td>   </td></tr>
!!<tr><td>     sedalk,sedalb,sedalg	</td><td>  Sedimentierte Menge an Algen der Algenklassen gruen, kiesel und blau </td><td> ALGAESGR, ALGAESKI, ALGAESBL	</td><td>   </td></tr>
!!<tr><td>     albewg,albewk		</td><td>  Wachstum bentischer grün- und kiesel-Algen  				</td><td> ALBENTH		</td><td> real </td></tr>
!!<tr><td>     alberg,alberk		</td><td>  Respiration bentischer grün- und kiesel-Algen  			</td><td> ALBENTH		</td><td> real </td></tr>
!!<tr><td></td></tr>
!!<tr><td>     hJPO4    		</td><td>  Phosphorfreisetzungsrate aus dem Sediment	</td><td> sedflux		</td><td>  </td></tr>
!!<tr><td></td></tr>
!!<tr><td>     dzres1			</td><td>  Grundrespirationsrate Rotatorien					</td><td> KONSUM			</td><td>   </td></tr>
!!<tr><td>     dzres2			</td><td>  Futterabhängige Respirationsrate Rotatorien				</td><td> KONSUM			</td><td>   </td></tr>
!!<tr><td>     resdr 			</td><td>  Respirationsrate benthischer Filtrierer (Muscheln)			</td><td> DREISSEN		</td><td>   </td></tr>
!!<tr><td>     exdrvg,exdrvk,exdrvb 	</td><td>  Teilmenge der verzehrten g,k,b Algen, die exkretiert wird	</td><td> DREISSEN		</td><td>   </td></tr>
!!<tr><td></td></tr>
!!<tr><td>     ep			</td><td> Punkteinleitun gelP	</td><td> 		</td><td> real, Dimension(100)   </td></tr>
!!<tr><td>     egesP			</td><td> Punkteinleitun gesP 	</td><td> 		</td><td> real, Dimension(100)  </td></tr>
!!<tr><td>     epl0			</td><td> Punkteinleitun pl0 	</td><td> 		</td><td> real, Dimension(100) </td></tr>
!!<tr><td>     gesPL,gPL		</td><td> Linienquellen für gesP und gelP</td><td> 	</td><td>  real, Dimension(100) </td></tr>
!!<tr><td>     qeinl,qeinlL 		</td><td> Volumenstrom Punkteinl. Linieneinl.			</td><td> eingelesen ???	</td><td> real, Dimension(100) </td></tr>
!!<tr><td>     flag			</td><td> Einleitungsflag, 							</td><td> eingelesen ???	</td><td> integer, Dimension(1000) </td></tr>
!!<tr><td>     jiein 			</td><td> Anzahl Einleitungen 							</td><td> eingelesen ???	</td><td> integer, Dimension(1000) 	</td></tr>
!!<tr><td>     ieinLs 			</td><td> Anzahl der Linienquellen im Strang (mstr) 				</td><td> eingelesen ???	</td><td> integer, Dimension(50) 	</td></tr>
!!<tr><td>     iorLa 			</td><td> AnfangsKnoten der Linienquelle ieinL des Strangs mstr 		</td><td> eingelesen ???	</td><td>    	</td></tr>
!!<tr><td>     iorLe 			</td><td> EndKnoten der Linienquelle ieinL des Strangs mstr 			</td><td> eingelesen ???	</td><td>   	</td></tr>
!!<tr><td></td></tr>
!!<tr><td>     tiefe,vabfl		</td><td>  hydraulische Parameter					</td><td> eingelesen ???	</td><td>   </td></tr>
!!<tr><td>     flae			</td><td>  Querschnittsfläche des Gewässerkörpers				</td><td> nur für Linienquelle benötigt, eingelesen sysgenou 	</td><td>   </td></tr>
!!<tr><td>     tflie			</td><td>  Fließzeit in Tagen (Zeitschrittweite)				</td><td> ?			</td><td>   </td></tr>
!!<tr><td>     ior,anze,mstr,elen	</td><td>  Zähler, Anzahl im Strang strangnr., Elementlänge			</td><td> eingelesen ??? 	</td><td>   </td></tr>
!!<tr><td>     ilbuhn			</td><td>  Schalter, ob Buhnen im Querprofil 					</td><td> eingelesen ???	</td><td>   </td></tr>
!!<tr><td>     nkzs 			</td><td>  Anzahl der Schichten (Tiefenverteilung 2D) 				</td><td> .			</td><td> integer, Dimension(1000) </td></tr>
!!<tr><td>     dH2D 			</td><td>  Schichtdicke Tiefenverteilung (tiefenaufgel.2D) konstante Abstände	</td><td> .			</td><td> real </td></tr>
!!<tr><td></td></tr>
!!<tr><td>     itrein			</td><td>  unklar  Übergabe in 13.00 		</td><td> diverse		</td><td> integer, Dimension(50,100,100)   </td></tr>
!!<tr><td></td></tr>
!!<tr><td>     zooind			</td><td>  Anzahl der Rotatorien unbenutzt					</td><td> KONSUM			</td><td>   </td></tr>
!!<tr><td>     iwied, GRote, dH2De	</td><td>  unbenutzt				</td><td> .			</td><td>   </td></tr>
!!<tr><td>     CD,CP,CM,BAC 		</td><td>  unbenutzt Transportkonzentrationen 	</td><td> orgc	unbenutzt		</td><td td></tr>
!!<tr><td>     dalgag,dalgak,dalgab	</td><td>  unbenutzt	</td><td> 		</td><td>  </td></tr>
!!<tr><td>     dalggr,dalgki,dalgbl 	</td><td>  unbenutzt	</td><td> 		</td><td>  </td></tr>
!!<tr><td>     dalgkz,dalgbz,dalggz	</td><td>  unbenutzt 	</td><td> 		</td><td>  </td></tr>
!!<tr><td>     pZoo,bl01,bl02		</td><td>  unbenutzt	</td><td> 		</td><td>  </td></tr>
!!<tr><td>     abltbr,akitbr,agrtbr	</td><td>  unbenutzt	Biomasse gruen, kiesel und blau -Algen 				</td><td> ALGAESGR, ALGAESKI, ALGAESBL	</td><td>   </td></tr>
!!</table>
!!
!! <h2>Transportkonzentrationen</h2>
!! <table >
!! <tr><th> Variablenname </th><th> Beschreibung 	</th><th> Subroutine 	</th></tr>
!! <tr><td> gesP 		</td><td> GesamtPhosphor 		</td><td> po4s 		</td></tr>
!! <tr><td> gelP 		</td><td> gelöster ortho-Phosphat-P 		</td><td> po4s 		</td></tr>
!! <tr><td> pl0 		</td><td> Verhältnis von Phosphor zu Kohlenstoff in organischem Material </td><td> orgC 		</td></tr>
!! <tr><td> Q_PG 		</td><td> Phosphoranteil der Algenbiomasse gruen </td><td> algaesgr 	</td></tr>
!! <tr><td> Q_PK 		</td><td> Phosphoranteil der Algenbiomasse kiesel</td><td> algaeski 	</td></tr>
!! <tr><td> Q_PB 		</td><td> Phosphoranteil der Algenbiomasse blau	 </td><td> algaesbl 	</td></tr>
!!</table> \n
!! WIRD VON ALLEN PROZESSEN AUFGERUFEN !!! (parallel)
!! \n\n

!
!! <h2>Formeln</h2>
!! gelPt-gelP(ior) =(agrP(1)+akiP(1)+ablP(1))+dop+Psed+gelPzo+gelPdr \n 
!! mit:\n 
!! agrp(nkz) = algagz(nkz,ior)*Q_PG(ior)-up_PGz(nkz,ior)*agrbrz(nkz,ior)+alberg(ior)*Qmx_PG-albewg(ior)*Qmx_PG  \n 
!! algagz-respirierte Algenbiomasse (gruen),tiefenaufgelöst   \n 
!! P-Freisetzung infolge Respiration der planktischen Algen, Aufnahme infolge Wachstum der planktischen Algen\n 
!! Respiration und Wachstum bentischer Algen.
!! dop = bsbctP(ior) \n 
!! Einfluss durch C-Abbau; dop ist die P-Aenderung beim Abbau durch s...? und Organismen auf Makrophyten\n                                
!! bsbctP - Phosphorfreisetzung beim Abbau org. Kohlenstoffverbidungen ??\n 
!! Psed = hJPO4(mstr,ior)*tflie/Tiefe(ior)\n 
!! gelpzo = dzres1(ior)*0.01+(dzres2(ior)*hconKi*Q_PK(ior))+(dzres2(ior)*hcongr*Q_PG(ior))+(dzres2(ior)*hconbl*Q_PB(ior))\n
!! dzres1-respirierte Rotatorienbiomasse/d  dzres2-respirierte Algenbiomasse/d (konsum.f90) \n                    
!! gelpdr = resdr(ior)*0.01+exdrvk(ior)*Q_PK(ior)+exdrvg(ior)*Q_PG(ior)+exdrvb(ior)*Q_PB(ior)   Einfluss von Dreissena  \n                 
!! -- \n\n                                                          
!! gesPt-gesP(ior) = Psed -orgCsd(ior)*pl0(ior)-sedalk(ior)*Q_PK(ior)-sedalb(ior)*Q_PB(ior)-sedalg(ior)*Q_PG(ior)  \n                     

