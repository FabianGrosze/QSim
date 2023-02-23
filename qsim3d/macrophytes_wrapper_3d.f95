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
subroutine macrophytes_wrapper_3d(i)
   use modell
   use QSimDatenfelder
   ! use module_metabolism, only: macrophytes
   
   implicit none
   
   integer, intent(in) :: i
   integer             :: nk
   
   
   ! macrophytes are currently turned off.
   ! Only the return values of `subroutine macrophytes` will be given values here
   ! to avoid potential conflicts with other modules that may use these variables
   benthic_distribution_p(3+(i-1)*number_benth_distr) = 0.0 ! pfl
   zone(point_zone(iglob))%macrodicht%pflmax          = 0.0 ! pflmax
   transfer_quantity_p(30+(i-1)*number_trans_quant)   = 0.0 ! po2p
   transfer_quantity_p(31+(i-1)*number_trans_quant)   = 0.0 ! po2r
   
   
   
   ! iglob = i + meinrang * part
   ! nk = (i-1)*number_plankt_vari
   ! kontroll = iglob == kontrollknoten
   ! 
   ! ! convert timestep from seconds (QSim3D) into days (QSim)
   ! tflie = real(deltat)/86400.
   ! 
   ! if (kontroll) then   
   !    print*,"before macrophytes"
   !    print*, "   pfl    = ", benthic_distribution_p(3+(i-1)*number_benth_distr)
   !    print*, "   pflmax = ", zone(point_zone(iglob))%macrodicht%pflmax
   !    print*, ""
   ! endif
   ! 
   ! call macrophytes(                                            &
   !          benthic_distribution_p(3+(i-1)*number_benth_distr), & ! pfl
   !          zone(point_zone(iglob))%macrodicht%pflmin,          & ! pflmin
   !          zone(point_zone(iglob))%macrodicht%pflmax,          & ! pflmax
   !          rb_hydraul_p(2+(i-1)*number_rb_hydraul),            & ! tiefe
   !          planktonic_variable_p( 1+nk),                       & ! tempw
   !          transfer_quantity_p(64+(i-1)*number_trans_quant),   & ! schwi
   !          transfer_quantity_p(54+(i-1)*number_trans_quant),   & ! extk
   !          tag,                                                & ! itags
   !          monat,                                              & ! monats
   !          zone(point_zone(iglob))%macrophyt%starttag,         & ! itstart
   !          zone(point_zone(iglob))%macrophyt%startmonat,       & ! mstart
   !          zone(point_zone(iglob))%macrophyt%maxtag,           & ! itmax
   !          zone(point_zone(iglob))%macrophyt%maxmonat,         & ! mmax
   !          zone(point_zone(iglob))%macrophyt%endtag,           & ! itend
   !          zone(point_zone(iglob))%macrophyt%endmonat,         & ! mend
   !          sonnenaufgang,                                      & ! sa
   !          sonnenuntergang,                                    & ! su
   !          tflie,                                              & ! tflie
   !          transfer_quantity_p(30+(i-1)*number_trans_quant),   & ! po2p
   !          transfer_quantity_p(31+(i-1)*number_trans_quant),   & ! po2r
   !          kontroll, iglob)
   ! 
   ! if (kontroll) then   
   !    print*,"after macrophytes"
   !    print*, "   pfl    = ", benthic_distribution_p(3+(i-1)*number_benth_distr)
   !    print*, "   pflmax = ", zone(point_zone(iglob))%macrodicht%pflmax
   !    print*, "   po2p   = ", transfer_quantity_p(30+(i-1)*number_trans_quant)
   !    print*, "   po2r   = ", transfer_quantity_p(31+(i-1)*number_trans_quant)
   !    print*, ""
   ! endif

   return
end subroutine macrophytes_wrapper_3d
