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
!> Wrapper for calling macrophytes().
subroutine macrophytes_wrapper_3d(i)
   use modell
   use QSimDatenfelder
   use module_macrophytes, only: macrophytes
   implicit none
   integer, intent(in) :: i
   
   integer :: nk
   
   iglob = i + meinrang*part
   kontroll = iglob == kontrollknoten
   
   ! Umwandlung des Zeitschritts von integer Sekunden (QSim3D) in real Tage (QSim1D)
   tflie = real(deltat)/86400.

   if (kontroll) then 
      print*, 'before macrophytes'
       print*, '  pfl  = ', benthic_distribution_p(3+(i-1)*number_benth_distr)
      print*, '   po2p = ', transfer_quantity_p(30+(i-1)*number_trans_quant)
      print*, '   po2r = ', transfer_quantity_p(31+(i-1)*number_trans_quant)
      print*, ''
   endif
   
   call macrophytes(                                               & !
               benthic_distribution_p(3+(i-1)*number_benth_distr), & ! pfl
               zone(point_zone(iglob))%macrodicht%pflmax,          & ! pflmax
               zone(point_zone(iglob))%macrodicht%pflmin,          & ! pflmin
               planktonic_variable_p( 1+(i-1)*number_plankt_vari), & ! tempw
               transfer_quantity_p(64+(i-1)*number_trans_quant),   & ! schwi
               transfer_quantity_p(54+(i-1)*number_trans_quant),   & ! extk
               rb_hydraul_p(2+(i-1)*number_rb_hydraul),            & ! tiefe
               zone(point_zone(iglob))%macrophyt%starttag,         & ! itstart
               zone(point_zone(iglob))%macrophyt%startmonat,       & ! mstart
               zone(point_zone(iglob))%macrophyt%maxtag,           & ! itmax
               zone(point_zone(iglob))%macrophyt%maxmonat,         & ! mmax
               zone(point_zone(iglob))%macrophyt%endtag,           & ! itend
               zone(point_zone(iglob))%macrophyt%endmonat,         & ! mend
               tag,                                                & ! itags
               monat,                                              & ! monats
               sonnenaufgang,                                      & ! sa
               sonnenuntergang,                                    & ! su
               tflie,                                              & ! tflie 
               transfer_quantity_p(30+(i-1)*number_trans_quant),   & ! po2p
               transfer_quantity_p(31+(i-1)*number_trans_quant),   & ! po2r
               kontroll, iglob)
   
   if (kontroll) then 
      print*, 'after macrophytes():'
      print*, '   pfl  = ', benthic_distribution_p(3+(i-1)*number_benth_distr)
      print*, '   po2p = ', transfer_quantity_p(30+(i-1)*number_trans_quant)
      print*, '   po2r = ', transfer_quantity_p(31+(i-1)*number_trans_quant)
      print*, ''
   endif
   
   return
end subroutine macrophytes_wrapper_3d
