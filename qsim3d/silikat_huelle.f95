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

subroutine silikat_huelle(i)
   use modell
   use QSimDatenfelder
   use aparam
   implicit none
   integer :: i,j,nk
   iglob = (i+meinrang*part)
   kontroll = iglob == kontrollknoten ! Erweiterung QSim3D wy
   nk = ((i-1)*number_plankt_vari)
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Datenübergabe:
   si(1) = planktonic_variable_p( 7+nk)  ! silikat-Silizium-Konzentration (tiefengemittelt)
   si(2) = si(1)
   flag(1) = 0         ! keine Einleitungen
   flag(2) = flag(1)
   elen(1) = 1         ! Elementlänge (nicht verwendet)
   elen(2) = elen(1)
   ior = 1             ! Laufindex
   esi(1) = 0.0       ! keine Einleitung
   qeinl(1) = 0.0      ! kein Abfluss Einleitung
   vabfl(1) = 2.5     ! wird nur bei Einleitungen verwendet, die sind aber ausgeschaltet.
   vabfl(2) = vabfl(1)
   anze = 1            ! Anzahl der Profile im aktuellen Strang
   tflie = real(deltat)/86400 ! Umwandlung des Zeitschritts von integer sekunden (T-QSim) in real Tage (QSim)
   jiein(1) = 0        ! null Punkt-Einleitungen
   aki(1) = planktonic_variable_p(8+(i-1)*number_plankt_vari) ! Anteil kiesel-Algen (wird nicht verwendet)
   aki(2) = aki(1)
   albewk(1) = benthic_distribution_p(14+(i-1)*number_benth_distr) ! Wachstum benthischer kiesel-Algen
   albewk(2) = albewk(1)
   alberk(1) = benthic_distribution_p(12+(i-1)*number_benth_distr) ! Respiration benthischer kiesel-Algen
   alberk(2) = alberk(1)
   tiefe(1) = rb_hydraul_p(2+(i-1)*number_rb_hydraul) ! Wassertiefe aus randbedingungen.h
   tiefe(2) = tiefe(1)
   tempw(1) = planktonic_variable_p(1+nk)  ! Wassertemperatur
   tempw(2) = tempw(1)
   !sised(1) =benthic_distribution_p(2+(i-1)*number_benth_distr) ! Siliziumgehalt im Sediment
   !sised(2) = sised(1)
   ilbuhn = 0          ! keine Buhnen
   ! direkt aus QSimDatenfelder akkssi=transfer_parameter_p(26)! unbenutzt
   ! direkt aus QSimDatenfelder Qmx_SK=transfer_parameter_p(33) ! max. Siliziumanteil der Kiesel-Algenbiomasse aus APARAM.txt
   Q_SK(1) = planktonic_variable_p(32+nk)  ! Siliziumgehalt Kieselalgen
   Q_SK(2) = Q_SK(1)
   do j = 1,num_lev_trans ! Si-Aufnahmerate der Kiesel-Algen tiefenaufgelöst
      !### veri13.3 ### trans_quant_vert_p(j+(4-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = 0.1!### veri13.3 ###
      up_Siz(j,1) = trans_quant_vert_p(j+(4-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert)
      up_Siz(j,2) = up_Siz(j,1)
   end do
   do j = 1,num_lev
      siz(j,1) = & !plankt_vari_vert_p(7,j,i) ! silikat(7)-Silizium-Konzentration (tiefenaufgelöst)
                 plankt_vari_vert_p(j+(7-1)*num_lev+(i-1)*number_plankt_vari*num_lev)
      siz(j,2) = siz(j,1)
   end do
   do j = 1,num_lev_trans ! ?? unbenutzt
      dalgkz(j,1) = trans_quant_vert_p(j+(12-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
      dalgkz(j,2) = dalgkz(j,1)
   end do
   akitbr(1) = transfer_quantity_p(48+(i-1)*number_trans_quant) ! Kieselalgen ?? unbenutzt !
   akitbr(2) = akitbr(1)
   do j = 1,num_lev_trans ! brutto Wachstum Biomasse Kiesel-Algen tiefenaufgelöst
      !### veri13.3 ###trans_quant_vert_p(j+(23-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = 0.1!### veri13.3 ###
      akibrz(j,1) = trans_quant_vert_p(j+(23-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) !
      akibrz(j,2) = akibrz(j,1)
   end do
   !### veri13.3 ### benthic_distribution_p(46+(i-1)*number_benth_distr) = 0.1!### veri13.3 ###
   hJSi(1,1) = benthic_distribution_p(46+(i-1)*number_benth_distr) ! Silizium-Flux aus dem Sediment
   hJSi(1,2) = hJSi(1,1)
   !SiRuek(1) =benthic_distribution_p(25+(i-1)*number_benth_distr) ! Siliziumgehalt im Sediment
   !SiRuek(2) = SiRuek(1)
   !Skmor(1) = planktonic_variable_p(69+nk)  ! Silizium in schwebenden, abgestorbenen Kieselalgen
   !Skmor(2) = Skmor(1)
   nkzs(1) = 1         ! nur eine Tiefenschicht
   nkzs(2) = nkzs(1)
   dH2D = -2.0         ! bisher nur 2D-Tiefengemittelt ??? konstante Tiefenschichtung ???
   dH2De = 0.0   ! unbenutzt
   mstr = 1            ! Strangzähler
   iorLa(1) = 0              ! AnfangsKnoten der Linienquelle; nicht verwendet
   iorLe(1) = 0              ! EndKnoten der Linienquelle; nicht verwendet
   ieinLs(1) = 0             ! keine Linienquellen
   ieinLs(2) = ieinLs(1)
   flae(1) = 0.0           ! unbenutzt da keine Einleitung
   flae(2) = flae(1)
   qeinlL(1) = 0.0           ! Zufluss Linienquelle; nicht verwendet
   SiL(1) = 0.0            ! Siliziumgehalt Linienquelle; nicht verwendet
   itags = tag               ! Tag im Monat module::modell zeitsekunde()        (unbenutzt)
   uhrz = uhrzeit_stunde     ! Uhrzeit module::modell zeitsekunde()        (unbenutzt)
   if (iglob == kontrollknoten) print*,'silikat_huelle vorher knoten ', iglob  &
       ,' si,hJSi(1,1),Skmor,Q_SK,sised,SiRuek,tiefe,tempw = ' &
       ,  si(1),hJSi(1,1),Skmor(1),Q_SK(1),sised(1),SiRuek(1),tiefe(1),tempw(1)
   ! qsim1d 13.40 15okt18
   call silikat(si,flag,elen,ior,esi,qeinl,vabfl,anze,tflie,jiein,aki         &
                ,albewk,alberk,tiefe,tempw,ilbuhn,akkssi,Qmx_SK,Q_SK          &
                ,up_Siz,Siz,algakz,akitbr,akibrz,hJSi,nkzs,dH2D,dH2De,mstr    &
                ,iorLa,iorLe,ieinLs,flae,qeinlL,SiL,itags,Uhrz,azStrs         &
                ,kontroll ,iglob )
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Datenrückgabe:
   planktonic_variable_p( 7+nk) = si(1)
   benthic_distribution_p(2+(i-1)*number_benth_distr) = sised(1)
   planktonic_variable_p(32+nk) = Q_SK(1)
   do j = 1,num_lev
      plankt_vari_vert_p(j+(7-1)*num_lev+(i-1)*number_plankt_vari*num_lev) = &
                                                                                  siz(j,1) ! silikat(7)-Silizium-Konzentration (tiefenaufgelöst)
   end do
   benthic_distribution_p(25+(i-1)*number_benth_distr) = SiRuek(1) ! Siliziumgehalt im Sediment
   planktonic_variable_p(69+nk) = Skmor(1)
   if (iglob == kontrollknoten) print*,'silikat_huelle nachher knoten ', iglob  &
       ,' si,hJSi(1,1),Skmor,Q_SK,sised,SiRuek = ',planktonic_variable_p(7+nk),hJSi(1,1)       &
       ,planktonic_variable_p(69+nk),planktonic_variable_p(32+nk) &
       ,benthic_distribution_p(2+(i-1)*number_benth_distr),benthic_distribution_p(25+(i-1)*number_benth_distr)
   if (isnan(si(1))) then
      write(fehler,*)'isnan(si(1)) i = ',iglob
      call qerror(fehler)
   end if ! isnan
   return
end subroutine silikat_huelle
!----+-----+----
!> die Subroutine ini_silikat() steht in silikat_huelle.f95\n
!! schreibt zunächst testwerte in die Siliziumvariablen. \n
!! ### in initialisieren() ausgeschaltet wird über Randbedingungen gesetzt ### skmor bleibt am Start auf 0 ???
subroutine ini_silikat()
   use modell
   implicit none
   integer :: i,j,nk
   do i = 1,number_plankt_point
      nk = ((i-1)*number_plankt_vari)
      planktonic_variable( 7+nk) = 10 ! si
      do j = 1,num_lev
         plankt_vari_vert(j+(7-1)*num_lev+(i-1)*number_plankt_vari*num_lev) = planktonic_variable( 7+nk) ! siz(j,1) ! silikat(7)-Silizium-Konzentration (tiefenaufgelöst)
      end do
      planktonic_variable(32+nk) = 0.004 ! Q_SK
      planktonic_variable(69+nk) = 0.0 ! Skmor
   end do
   do i = 1,number_benthic_points
      benthic_distribution(2+(i-1)*number_benth_distr) = 0.0 !!  sised
   end do
   do i = 1,number_trans_quant_points
      do j = 1,num_lev_trans
         trans_quant_vert(j+(4-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert) = 0.0 ! 0.025 ! up_Siz(j,1) ! Si-Aufnahmerate der Kiesel-Algen
      end do
   end do
   return
end subroutine ini_silikat

