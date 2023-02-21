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

subroutine albenth_huelle(i)
   use modell
   use QSimDatenfelder
   implicit none
   
   integer, intent(in) :: i
   integer             :: nk
   
   ! benthic algae are currently turned off.
   ! Only the return values of `subroutine albenth` will be given values here
   ! to avoid potential conflicts with other modules that may use these variables
   benthic_distribution_p(13+(i-1)*number_benth_distr) = 0.0 ! albewg
   benthic_distribution_p(14+(i-1)*number_benth_distr) = 0.0 ! albewk
   benthic_distribution_p(11+(i-1)*number_benth_distr) = 0.0 ! alberg
   benthic_distribution_p(12+(i-1)*number_benth_distr) = 0.0 ! alberk
   benthic_distribution_p(10+(i-1)*number_benth_distr) = 0.0 ! cmatgr
   benthic_distribution_p( 9+(i-1)*number_benth_distr) = 0.0 ! cmatki
   
   
   
   ! iglob = (i+meinrang*part)
   ! nk = (i-1)*number_plankt_vari
   ! kontroll = iglob == kontrollknoten
   
   ! if (kontroll)print*,'albenth_huelle  iglob,i,nk,meinrang = ',iglob,i,nk,meinrang
   ! tflie = real(deltat)/86400 ! Umwandlung des Zeitschritts von integer sekunden (QSim3D) in real Tage (QSim)
   
   ! call albenth(                                                       &
   !    benthic_distribution_p(72+(i-1)*number_benth_distr),             & ! abegm2
   !    benthic_distribution_p(73+(i-1)*number_benth_distr),             & ! abekm2
   !    planktonic_variable_p( 3+nk),                                    & ! vnh4
   !    planktonic_variable_p( 5+nk),                                    & ! vno3
   !    planktonic_variable_p( 6+nk),                                    & ! gelp 
   !    planktonic_variable_p( 7+nk),                                    & ! si
   !    rb_hydraul_p(1+(i-1)*number_rb_hydraul),                         & ! vmitt 
   !    planktonic_variable_p(1+nk),                                     & ! tempw
   !    schwi_T(zone(point_zone(iglob))%wettstat%wetterstations_nummer), & ! schwi 
   !    transfer_quantity_p(54+(i-1)*number_trans_quant),                & ! extk
   !    rb_hydraul_p(2+(i-1)*number_rb_hydraul),                         & ! tiefe
   !    tflie,                                                           & ! tflie
   !    benthic_distribution_p(13+(i-1)*number_benth_distr),             & ! albewg
   !    benthic_distribution_p(11+(i-1)*number_benth_distr) ,            & ! alberg
   !    benthic_distribution_p(14+(i-1)*number_benth_distr) ,            & ! albewk
   !    benthic_distribution_p(12+(i-1)*number_benth_distr),             & ! alberk
   !    benthic_distribution_p(10+(i-1)*number_benth_distr),             & ! cmatgr
   !    benthic_distribution_p(9+(i-1)*number_benth_distr),              & ! cmatki
   !    kontroll, jjj)
   
   return
end subroutine albenth_huelle
