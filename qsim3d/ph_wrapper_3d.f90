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
subroutine ph_wrapper_3d(i)
   use modell
   use QSimDatenfelder
   use module_ph, only: ph
   
   implicit none
   integer, intent(in) :: i !< lokale Knotennummer auf dem jeweiligen Prozessor; läuft von 1 bis part
   
   real    :: flaes, tiefes, raus
   
   ! TODO (schoenung, july 2022): iPhy should be checked while it's read in, not here!
   if (iphy < 1 .or. iphy > 4) then
      write(fehler,*)'ph_huelle: aeration flag iphy',iphy,' out of bounds i,meinrang = ',i,meinrang
      call qerror(fehler)
   endif
   
   iglob = i + meinrang * part 
   kontroll = iglob == kontrollknoten
   tiefes = rb_hydraul_p(2+(i-1)*number_rb_hydraul)
   tflie = real(deltat) / 86400.
   raus = strickler(zone(point_zone(iglob))%reib, tiefes)
   flaes = 1000.0
   
   if (kontroll) then
      print*, 'before ph:'
      print*, '  iglob    = ', iglob
      print*, '  meinrang = ', meinrang
      print*, '  i        = ', i
      print*, '  part     = ', part
      print*, '  lf       = ', planktonic_variable_p(65+(i-1)*number_plankt_vari)
      print*, '  ph       = ', planktonic_variable_p(66+(i-1)*number_plankt_vari)
   endif
   
   call ph(planktonic_variable_p(62+(i-1)*number_plankt_vari),           &! mw
           planktonic_variable_p(63+(i-1)*number_plankt_vari),           &! pw
           planktonic_variable_p(64+(i-1)*number_plankt_vari),           &! ca
           planktonic_variable_p(65+(i-1)*number_plankt_vari),           &! lf
           planktonic_variable_p( 1+(i-1)*number_plankt_vari),           &! tempw
           planktonic_variable_p(66+(i-1)*number_plankt_vari),           &! vph
           transfer_quantity_p(26+(i-1)*number_trans_quant),             &! vco2
           tflie,                                                        &! tflie
           raus,                                                         &! rau
           rb_hydraul_p(1+(i-1)*number_rb_hydraul),                      &! vmitt
           rb_hydraul_p(2+(i-1)*number_rb_hydraul),                      &! tiefe
           rb_hydraul_p(2+(i-1)*number_rb_hydraul),                      &! rhyd
           flaes,                                                        &! flae
           wge_T(zone(point_zone(iglob))%wettstat%wetterstations_nummer),&! wge
           zone(point_zone(iglob))%wettstat%wetterstations_lage,         &! WLage
           rb_hydraul_p(3+(i-1)*number_rb_hydraul),                      &! hWS
           iphy,                                                         &! iphy
           transfer_quantity_p(47+(i-1)*number_trans_quant),             &! bsbct
           benthic_distribution_p(15+(i-1)*number_benth_distr),          &! resdr
           transfer_quantity_p(27+(i-1)*number_trans_quant),             &! dzres1 
           transfer_quantity_p(28+(i-1)*number_trans_quant),             &! dzres2
           transfer_quantity_p(20+(i-1)*number_trans_quant),             &! dalgki
           transfer_quantity_p(21+(i-1)*number_trans_quant),             &! dalggr
           transfer_quantity_p(22+(i-1)*number_trans_quant),             &! dalgbl
           transfer_quantity_p(23+(i-1)*number_trans_quant),             &! dalgak
           transfer_quantity_p(24+(i-1)*number_trans_quant),             &! dalgag
           transfer_quantity_p(25+(i-1)*number_trans_quant),             &! dalgab 
           benthic_distribution_p(11+(i-1)*number_benth_distr),          &! alberg
           benthic_distribution_p(12+(i-1)*number_benth_distr),          &! alberk
           benthic_distribution_p(13+(i-1)*number_benth_distr),          &! albewg
           benthic_distribution_p(14+(i-1)*number_benth_distr),          &! albewk 
           transfer_quantity_p(29+(i-1)*number_trans_quant),             &! susn
           transfer_quantity_p(30+(i-1)*number_trans_quant),             &! po2p
           transfer_quantity_p(31+(i-1)*number_trans_quant),             &! po2r
           planktonic_variable_p(52+(i-1)*number_plankt_vari),           &! ssalg
           planktonic_variable_p(59+(i-1)*number_plankt_vari),           &! stind
           kontroll, iglob)
   
   if (kontroll) then
      print*, 'after ph:'
      print*, '  iglob    = ', iglob
      print*, '  meinrang = ', meinrang
      print*, '  i        = ', i
      print*, '  part     = ', part
      print*, '  lf       = ', planktonic_variable_p(65+(i-1)*number_plankt_vari)
      print*, '  ph       = ', planktonic_variable_p(66+(i-1)*number_plankt_vari)
   endif
   
   return
end subroutine ph_wrapper_3d


!> die Subroutine ini_ph() steht in der datei ph_huelle.f95
!! sie schreibt zunächst Nullen (ph=7) in die ph-variablen (mw,pw,ca,lf)
!!
!! ### ausgeschaltet in initialisieren() ### Vorbelegung durch randwerte
subroutine ini_ph()
   use modell
   implicit none
   integer i
   do i = 1,number_plankt_point
      planktonic_variable(62+(i-1)*number_plankt_vari) = 0.0 ! 1.5 ! mw 2.5
      planktonic_variable(63+(i-1)*number_plankt_vari) = 0.0 ! pw
      planktonic_variable(64+(i-1)*number_plankt_vari) = 0.0 ! 41.0 ! ca  41.0
      planktonic_variable(65+(i-1)*number_plankt_vari) = 0.0 ! 402.0 ! lf #### momentan von ini_salz gesetzt ???402.0
      planktonic_variable(66+(i-1)*number_plankt_vari) = 7.0 ! vph
   end do
   return
end subroutine ini_ph
