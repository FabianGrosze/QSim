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

subroutine coliform_bacteria_wrapper_3d(i)
   use modell
   use qsimdatenfelder
   use module_metabolism
   implicit none
   
   integer, intent(in) :: i
   integer             :: nk
   
   iglob = i + meinrang*part
   kontroll = iglob == kontrollknoten
   nk = (i-1)*number_plankt_vari
   
   ! convert timestep from seconds in days
   tflie = real(deltat)/86400 
   tiefe(1) = rb_hydraul_p(2+(i-1)*number_rb_hydraul)
   rau(1) = strickler(zone(point_zone(iglob))%reib , tiefe(1))
   
   if (kontroll) then
      print*, "before coliform_bacteria:"
      print*, "   coli = ", planktonic_variable_p(61+nk)
      print*, ""
   endif
   
   call coliform_bacteria(                                             &
      planktonic_variable_p(61+nk),                                    & ! coli
      planktonic_variable_p(70+nk),                                    & ! doscf
      zone(point_zone(iglob))%seditemp%extiks,                         & ! extks,
      planktonic_variable_p(1+nk),                                     & ! tempw
      rau(1),                                                          & ! rau,
      tiefe(1),                                                        & ! tiefe,
      rb_hydraul_p(1+(i-1)*number_rb_hydraul),                         & ! vmitt,
      schwi_t(zone(point_zone(iglob))%wettstat%wetterstations_nummer), & ! schwi
      tflie,                                                           & ! tflie
      kontroll,                                                        & ! kontroll
      iglob)                                                             ! jjj
   

   if (kontroll) then
      print*, "after coliform_bacteria:"
      print*, "   coli = ", planktonic_variable_p(61+nk)
      print*, ""
   endif
   return
end subroutine coliform_bacteria_wrapper_3d
