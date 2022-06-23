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
!> SUBROUTINE konsum_huelle() wird beschrieben in: \ref lnk_rotatorien \n
!! Quelle konsum.huelle.f95
subroutine konsum_huelle(i)
   use modell
   use QSimDatenfelder
   use aparam
   implicit none
   !    integer                        :: anze
   !    integer, Dimension(1000)       :: jiein, nkzs, flag
   !    real                           :: irmaxK, irmaxG, irmaxB, irmaxn, irmax, mueRot, mormax, mormin, morRot
   !    real, Dimension(50)            :: irz
   !    real, Dimension(100)           :: ezind, qeinl
   !    real, Dimension(1000)          :: tempw, vo2, zooind, abszo, zexki, zexgr, zexbl, aki, agr, abl, vkigr
   !    real, Dimension(1000)          :: elen, ir, dzres1, dzres2, vabfl, rmuas, iras, rakr, rbar
   !    real, Dimension(1000)          :: CHNF, zHNF, HNFza, algzok, algzog, algzob
   !    real, Dimension(50,1000)       :: akiz, agrz, ablz, algzkz, algzgz, algzbz
   !    double precision               :: Qquell,QSenk
   integer :: i,j
   iglob = (i+meinrang*part)
   !! aus module::aparam
   !! , irmaxe, GROT
   !if(i.eq.1)print*,'Rotatorien konsumieren drauflos'
   ! vkigr,TEMPW,VO2,TFLIE
   vkigr(1) = planktonic_variable_p(19+(i-1)*number_plankt_vari) ! Anteil der Kieselalgen am Gesamt-Chlorophyll-a ??
   vkigr(2) = vkigr(1)
   tempw(1) = planktonic_variable_p( 1+(i-1)*number_plankt_vari) ! Wasser-Temperatur
   tempw(2) = tempw(1)
   vo2(1) = planktonic_variable_p( 2+(i-1)*number_plankt_vari) ! Sauerstoffgehalt tiefengemittelt
   vo2(2) = vo2(1)
   tflie = real(deltat)/86400 ! Umwandlung des Zeitschritts von integer sekunden (T-QSim) in real Tage (QSim)
   ! ezind,ZOOIND,abszo,ir,flag,elen,ior,anze,qeinl,vabfl
   ezind = 0.0  ! Einleitungswert (keine Einleitungen in QSim-3D)
   zooind(1) = planktonic_variable_p(50+(i-1)*number_plankt_vari) ! Anzahl der Rotatorien
   zooind(2) = zooind(1)
   !abszo(1) = transfer_quantity_p(6+(i-1)*number_trans_quant) ! Absterberate Zooplankton
   !abszo(2) = abszo(1)
   !ir(1) = transfer_quantity_p(42+(i-1)*number_trans_quant) ! Ingestionsrate der Rotatorien in mg/(l*h) | konsum()
   !ir(2) = ir(1)
   flag(1) = 0         ! keine Einleitungen
   flag(2) = flag(1)
   elen(1) = 1         ! Elementlänge (nicht verwendet)
   elen(2) = elen(1)
   ior = 1             ! Laufindex
   anze = 1            ! Anzahl der Profile im aktuellen Strang
   qeinl(1) = 0.0      ! kein Abfluss Einleitung
   qeinl(2) = qeinl(1)
   vabfl(1) = 0.0     ! wird nur bei Einleitungen verwendet, die sind aber ausgeschaltet.
   vabfl(2) = vabfl(1)
   ! jiein,FopIRe,GROT,dzres1,dzres2,zresge
   jiein(1) = 0       ! keine Punkt-Einleitungen
   ! FopIRe ! Halbsättigungskonstante für Futteraufnahme d. Rotatorien ! von aparam_lesen() jetzt direkt in module QSimDatenfelder
   ! GROT ! Gewicht Rotatorie ! von aparam_lesen() jetzt direkt in module QSimDatenfelder
   !dzres1(1) = transfer_quantity_p(27+(i-1)*number_trans_quant) ! Grund-Respiration Konsumenten
   !dzres1(2) = dzres1(1)
   !dzres2(1) = transfer_quantity_p(28+(i-1)*number_trans_quant) ! Fraßabhängige Respirationsrate Konsumenten
   !dzres2(2) = dzres2(1)
   ! zresge ! Grundrespiration Rotatorien ! von aparam_lesen() jetzt direkt in module QSimDatenfelder
   ! irmaxe,zexki,zexgr,zexbl                                         &
   ! IRMAXe ! max. Gewichtsspez. Algenaufnahmerate d. Rotatorien ! von aparam_lesen() jetzt direkt in module QSimDatenfelder
   !zexki(1) = transfer_quantity_p(16+(i-1)*number_trans_quant)
   ! Ausscheidungen der Rotatorien infolge Konsums von Kieselalgen
   !zexki(2) = zexki(1)
   !zexgr(1) = transfer_quantity_p(17+(i-1)*number_trans_quant) ! Grünalgen
   !zexgr(2) = zexgr(1)
   !zexbl(1) = transfer_quantity_p(18+(i-1)*number_trans_quant) ! Blaualgen
   !zexbl(2) = zexbl(1)
   ! aki,agr,abl,iwied,rmuas,iras                                     &
   aki(1) = planktonic_variable_p(8+(i-1)*number_plankt_vari) ! Biomasse  Kiesel-Algen ! keine Rückgabevariable
   aki(2) = aki(1)
   agr(1) = planktonic_variable_p(9+(i-1)*number_plankt_vari) !  Biomasse Grün-Algen
   agr(2) = agr(1)
   abl(1) = planktonic_variable_p(10+(i-1)*number_plankt_vari) !  Biomasse Blau-Algen
   abl(2) = abl(1)
   ! ### iwied = 0 : allererster Zeitschritt, danach iwied = 1 ###
   iwied = 1
   ! rmuas ! Ausgabeparameter
   ! iras ??? Ausgabeparameter
   ! rakr,rbar,CHNF,zHNF,ilbuhn,zakie,zagre,zable,HNFza,algzok        &
   ! rakr ! Ausgabeparameter
   ! rbar ! Ausgabeparameter
   CHNF(1:2) = planktonic_variable_p(48+(i-1)*number_plankt_vari) ! C-Masse der heterotrophen Nanoflagelaten
   if (CHNF(1) <= 0.0) CHNF(1:2) = 0.0 ! CHNF=-1 meint keine HNF
   !zHNF(1) = transfer_quantity_p(74+(i-1)*number_trans_quant) ! Aufnahmerate der HNF
   !zHNF(2) =   zHNF(1)
   !   ir - Ingestionsrate in mg/(l*h)
   !   zHNF - Aufnahmerate der HNF
   !   ir/A - Filtriertes Wasservolumen l/h
   !   zHNF(ior) = ir(ior)*CHNF(ior)/(agr(ior)+aki(ior)+abl(ior))
   ilbuhn = 0          ! keine Buhnen
   ! ZAKIe ! Filtrierbarkeit Kieselalgen ! von aparam_lesen() jetzt direkt in module QSimDatenfelder
   ! zagre!  Filtrierbarkeit Grünalgen ! von aparam_lesen() jetzt direkt in module QSimDatenfelder
   ! zable! Filtrierbarkeit der Blaualgen durch Rotatorien ! von aparam_lesen() jetzt direkt in module QSimDatenfelder
   !HNFza(1) = transfer_quantity_p(75+(i-1)*number_trans_quant) !   Ausgabe: HNFza(ior) = (zHNF(ior)/CHNF(ior))*24.
   !HNFza(2) = HNFza(1)
   !algzok(1) = transfer_quantity_p(53+(i-1)*number_trans_quant) ! kiesel-Algen-Konsum Zoo-Plankton in mg/l
   !algzok(2) = algzok(1)
   ! algzog,algzob,akiz,agrz,ablz,algzkz,algzgz,algzbz,nkzs,monats    &
   !algzog(1) = transfer_quantity_p(72+(i-1)*number_trans_quant) ! gruen-Algen-Konsum Zoo-Plankton in mg/l
   !algzog(2) = algzog(1)
   !algzob(1) = transfer_quantity_p(73+(i-1)*number_trans_quant) ! blau-Algen-Konsum Zoo-Plankton in mg/l
   !algzob(2) = algzob(1)
   do j = 1,num_lev
      akiz(j,1) = plankt_vari_vert_p(j+( 8-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! Biomasse kiesel-Algen tiefenaufgelöst
      akiz(j,2) = akiz(j,1)
      agrz(j,1) = plankt_vari_vert_p(j+( 9-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! Biomasse gruen-Algen tiefenaufgelöst
      agrz(j,2) = agrz(j,1)
      ablz(j,1) = plankt_vari_vert_p(j+(10-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) ! Biomasse blau-Algen tiefenaufgelöst
      ablz(j,2) = ablz(j,1)
      !algzkz(j,1) = trans_quant_vert_p(j+(26-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! Kiesel-Algen-Konsum durch Zoo-Plankton in mg/l
      !algzkz(j,2) = algzkz(j,1)
      !algzgz(j,1) = trans_quant_vert_p(j+(27-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! Grün-Algen-Konsum durch Zoo-Plankton in mg/l
      !algzgz(j,2) = algzgz(j,1)
      !algzbz(j,1) = trans_quant_vert_p(j+(28-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) ! Blau-Algen-Konsum durch Zoo-Plankton in mg/l
      !algzbz(j,2) = algzbz(j,1)
   end do
   nkzs(1) = 1         ! nur eine Tiefenschicht
   nkzs(2) = nkzs(1)
   monats = monat  ! Monat im Jahr module::modell zeitsekunde()
   ! itags,mstr)
   itags = tag     ! Tag im Monat module::modell zeitsekunde()
   uhrz = uhrzeit_stunde ! Uhrzeit
   mstr = 1        ! Strangzähler | nur ein Profil in einem Strang
   ! azStrs=1 parameter in 3D
   TGZoo(1,1) = planktonic_variable_p(76+(i-1)*number_plankt_vari) ! Zooplanktongewicht
   TGZoo(1,2) = TGZoo(1,1)
   BAC(1) = planktonic_variable_p(42+(i-1)*number_plankt_vari) ! Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen
   BAC(2) = BAC(1)
   kontroll = (iglob == kontrollknoten)
   if (kontroll) then
      print*,'konsum vorher: tflie = ',tflie,' uhrz = ',uhrz
      print*,'GROT',GROT
      print*,'zooind,abszo,ir',zooind(1),abszo(1),ir(1)
      print*,'dzres1,dzres2',dzres1(1),dzres2(1)
      print*,'zexki,zexgr,zexbl',zexki(1),zexgr(1),zexbl(1)
      print*,'rmuas,iras',rmuas(1),iras(1)
      print*,'rakr,rbar,zHNF,HNFza,algzok',rakr(1),rbar(1),zHNF(1),HNFza(1),algzok(1)
      print*,'algzog,algzob,algzkz,algzgz,algzbz',algzog(1),algzob(1),algzkz(1,1),algzgz(1,1),algzbz(1,1)
   end if  ! kontrolle
   !version  Stoffumsatzroutinen aus der QSim1D Version 13_40 vom 15. Oktober 2018 in QSim3D
   call konsum(vkigr,TEMPW,VO2,TFLIE                                          &
               ,ezind,ZOOIND,abszo,ir,flag,elen,ior,anze,qeinl,vabfl          &
               ,jiein,FOPTR,GROT,dzres1,dzres2,ZRESG                          &
               ,IRMAX,zexki,zexgr,zexbl                                       &
               ,aki,agr,abl,iwied,rmuas,iras,TGZoo,BAC,zBAC                   &
               ,rakr,rbar,CHNF,zHNF,ilbuhn,ZAKI,ZAGR,ZABL,HNFza,algzok        &
               ,algzog,algzob,akiz,agrz,ablz,algzkz,algzgz,algzbz,nkzs,monats &
               ,itags,uhrz,mstr,azStrs                                        &
               ,kontroll,iglob)
   ! vkigr,TEMPW,VO2,TFLIE
   ! ezind,ZOOIND,abszo,ir,flag,elen,ior,anze,qeinl,vabfl
   planktonic_variable_p(50+(i-1)*number_plankt_vari) = zooind(1)  ! Anzahl der Rotatorien
   planktonic_variable_p(76+(i-1)*number_plankt_vari) = TGZoo(1,1) ! Zooplanktongewicht
   transfer_quantity_p(6+(i-1)*number_trans_quant) = abszo(1)      ! Absterberate Zooplankton
   transfer_quantity_p(42+(i-1)*number_trans_quant) = ir(1)        ! Ingestionsrate der Rotatorien in mg/(l*h) | konsum()
   ! jiein,FopIRe,GROT,dzres1,dzres2,zresge
   transfer_quantity_p(27+(i-1)*number_trans_quant) = dzres1(1)    ! Grund-Respiration Konsumenten
   transfer_quantity_p(28+(i-1)*number_trans_quant) = dzres2(1)    ! Fraßabhängige Respirationsrate Konsumenten
   ! irmaxe,zexki,zexgr,zexbl                                         &
   ! IRMAXe ! max. Gewichtsspez. Algenaufnahmerate d. Rotatorien ! von aparam_lesen() jetzt direkt in module QSimDatenfelder
   transfer_quantity_p(16+(i-1)*number_trans_quant) = zexki(1)
   ! Ausscheidungen der Rotatorien infolge Konsums von Kieselalgen
   transfer_quantity_p(17+(i-1)*number_trans_quant) = zexgr(1)     ! Grünalgen
   transfer_quantity_p(18+(i-1)*number_trans_quant) = zexbl(1)     ! Blaualgen
   ! aki,agr,abl,iwied,rmuas,iras                                   
   transfer_quantity_p(76+(i-1)*number_trans_quant) = rmuas(1)     !  = mueRot-respRg ! Nettowachstumsrate Rotatorien ? Ausgabeparameter
   transfer_quantity_p(79+(i-1)*number_trans_quant) = iras(1)      !  Ausgabe Ingestionsrate
   ! rakr,rbar,CHNF,zHNF,ilbuhn,zakie,zagre,zable,HNFza,algzok        &
   transfer_quantity_p(77+(i-1)*number_trans_quant) = rakr(1)      !  = iras(ior)*respaR ! Fraßabhängige Respiration ? Ausgabeparameter
   transfer_quantity_p(78+(i-1)*number_trans_quant) = rbar(1)      !  = respRg ! GRund?-Respiration ? Ausgabeparameter
   transfer_quantity_p(74+(i-1)*number_trans_quant) = zHNF(1)      ! Aufnahmerate der HNF
   transfer_quantity_p(91+(i-1)*number_trans_quant) = zBAC(1)      ! Aufnahmerate der BAC
   transfer_quantity_p(75+(i-1)*number_trans_quant) = HNFza(1)     !   Ausgabe: HNFza(ior) = (zHNF(ior)/CHNF(ior))*24.
   transfer_quantity_p(53+(i-1)*number_trans_quant) = algzok(1)    ! kiesel-Algen-Konsum Zoo-Plankton in mg/l
   ! algzog,algzob,akiz,agrz,ablz,algzkz,algzgz,algzbz,nkzs,monats    &
   transfer_quantity_p(72+(i-1)*number_trans_quant) = algzog(1)    ! gruen-Algen-Konsum Zoo-Plankton in mg/l
   transfer_quantity_p(73+(i-1)*number_trans_quant) = algzob(1)    ! blau-Algen-Konsum Zoo-Plankton in mg/l
   do j = 1,num_lev_trans
      trans_quant_vert_p(j+(26-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = algzkz(j,1)  ! Kiesel-Algen-Konsum durch Zoo-Plankton in mg/l
      trans_quant_vert_p(j+(27-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = algzgz(j,1) ! Grün-Algen-Konsum durch Zoo-Plankton in mg/l
      trans_quant_vert_p(j+(28-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = algzbz(j,1) ! Blau-Algen-Konsum durch Zoo-Plankton in mg/l
   end do
   ! itags,mstr)
   
   if (kontroll) then
      print*,'konsum nachher: tflie = ',tflie,' uhrz = ',uhrz
      print*,'GROT',GROT
      print*,'zooind,abszo,ir',zooind(1),abszo(1),ir(1)
      print*,'dzres1,dzres2',dzres1(1),dzres2(1)
      print*,'zexki,zexgr,zexbl',zexki(1),zexgr(1),zexbl(1)
      print*,'rmuas,iras',rmuas(1),iras(1)
      print*,'rakr,rbar,zHNF,HNFza,algzok',rakr(1),rbar(1),zHNF(1),HNFza(1),algzok(1)
      print*,'algzog,algzob,algzkz,algzgz(,algzbz',algzog(1),algzob(1),algzkz(1,1),algzgz(1,1),algzbz(1,1)
   end if  ! kontrolle
   return
end subroutine konsum_huelle
!----+-----+----
!      end module konsum_module
