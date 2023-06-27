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

subroutine water_temperature_wrapper_3d(i)
   use modell
   use qsimdatenfelder
   use module_metabolism
   implicit none
  
   integer, intent(in) :: i
   integer :: j, nk
   real    :: btiefe
  
   iglob = i + meinrang * part
   control = iglob == kontrollknoten
   nk = (i-1)*number_plankt_vari 
   tflie = real(deltat)/86400.
   
  
   if (control) then
      print*,'before temperw:'
      print*, '   temperw   = ', planktonic_variable_p(1+nk)
      print*, '   extk      = ', transfer_quantity_p(54+(i-1)*number_trans_quant)
      print*, '   tiefe     = ', rb_hydraul_p(2+(i-1)*number_rb_hydraul)
      print*, '   temperwz1 = ', plankt_vari_vert_p(1+(1-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev)
   endif
   
   btiefe = rb_hydraul_p(2+(i-1)*number_rb_hydraul)
   if (btiefe <= min_tief) btiefe = min_tief 
   dh2d = btiefe 
   
   call water_temperature(                                       & !
            planktonic_variable_p(1+nk),                         & ! tempw
            benthic_distribution_p(1+(i-1)*number_benth_distr),  & ! tsed
            transfer_quantity_p(62+(i-1)*number_trans_quant),    & ! templ
            transfer_quantity_p(63+(i-1)*number_trans_quant),    & ! ro
            transfer_quantity_p(64+(i-1)*number_trans_quant),    & ! schwi
            transfer_quantity_p(67+(i-1)*number_trans_quant),    & ! wtyp
            transfer_quantity_p(66+(i-1)*number_trans_quant),    & ! cloud
            transfer_quantity_p(54+(i-1)*number_trans_quant),    & ! extk
            zone(point_zone(iglob))%seditemp%extiks,             & ! extks
            zone(point_zone(iglob))%wettstat%wetterstations_lage,& ! wlage
            transfer_quantity_p(65+(i-1)*number_trans_quant),    & ! wge
            rb_hydraul_p(3+(i-1)*number_rb_hydraul),             & ! hws
            btiefe,                                              & ! tiefe
            zone(point_zone(iglob))%seditemp%wuebk,              & ! wuebk
            zone(point_zone(iglob))%seditemp%spewks,             & ! spewks
            zone(point_zone(iglob))%seditemp%psrefs,             & ! psrefs
            iform_verdr,                                         & ! iform_verdr
            dh2d,                                                & ! dh2d
            tflie,                                               & ! tflie
            control, iglob)
            

   if (control) then
      print*,' after temperw_kern:'
      print*, '   meinrang = ', meinrang
      print*, '   i        = ', i
      print*, '   tempw    = ', planktonic_variable_p(1+nk)
   endif
   
   if (rb_hydraul_p(2+(i-1)*number_rb_hydraul) <= min_tief) then 
      ! for dry nodes water temperature equals air tempature
      planktonic_variable_p(1+nk) = transfer_quantity_p(62+(i-1)*number_trans_quant)
   endif
      
   return
end subroutine water_temperature_wrapper_3d
