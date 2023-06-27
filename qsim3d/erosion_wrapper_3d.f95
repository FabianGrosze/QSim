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
!> Subroutine erosion_wrapper_3d() simulaties erosion of suspended Matters
!! 
!! Quelle: erosion_wrapper_3d.f95
subroutine erosion_wrapper_3d(i)
   use modell
   use qsimdatenfelder
   use module_aparam
   use module_metabolism
   implicit none
   
   integer, intent(in) :: i
   
   integer :: npla, ntra
   real    :: tiefe_s, flaes, rau_s, dsedHs
   
   
   iglob = i+meinrang*part
   control = iglob == kontrollknoten
   
   npla = (i-1) * number_plankt_vari
   ntra = (i-1) * number_trans_quant
   tflie = real(deltat)/86400.
   
   tiefe_s = rb_hydraul_p(2+(i-1)*number_rb_hydraul)
   tiefe_s = max(min_tief, tiefe_s)
   rau_s = strickler(zone(point_zone(iglob))%reib, tiefe_s)


   call erosion(                                      & !
            planktonic_variable_p(53+npla),           & ! ss
            planktonic_variable_p(52+npla),           & ! ssalg
            rb_hydraul_p(1+(i-1)*number_rb_hydraul),  & ! vmitt
            tiefe_s,                                  & ! tiefe
            rau_s,                                    & ! rau
            zone(point_zone(iglob))%erosi%tau_krit,   & ! tausc
            zone(point_zone(iglob))%erosi%sed_roh,    & ! sedroh
            zone(point_zone(iglob))%erosi%M_eros,     & ! m_eros
            zone(point_zone(iglob))%erosi%n_eros,     & ! n_eros
            tflie,                                    & ! tflie
            transfer_quantity_p(12+ntra),             & ! sseros
            transfer_quantity_p(68+ntra),             & ! dsedh
            control, iglob)


   
end subroutine erosion_wrapper_3d
