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

subroutine stofftransport_schism()
   use netcdf
   use modell
   use schism_glbl, only:su2,sv2,tr_el,eta2, npa, nsa, nea, dt
   use schism_msgp, only: myrank,parallel_abort !,nproc
   
   implicit none
   include 'netcdf.inc'
   integer,parameter :: maxsubst = 60      ! max. number of substeps
   integer nt, n,j,k, subtim, diff, diffprev, alloc_status
   real :: laeng, cu_max, cu_min, dt_sub, sumwicht
   real , allocatable , dimension (:,:) :: zwischen
   integer :: num_sub
   integer nti(maxsubst)
   if (meinrang == 0) then !! prozessor 0 only
      print*,"stofftransport_schism: startzeitpunkt, zeitpunkt,endzeitpunkt" ,startzeitpunkt, zeitpunkt, endzeitpunkt
      call qerror("preliminary Interrupt")
   endif
   return
end subroutine stofftransport_schism
!----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
subroutine sc_read_rough()
   use netcdf
   use modell
   implicit none
   integer :: n
   do n = 1,number_benthic_points
      benthic_distribution(5+(n-1)*number_benth_distr) = 70.0 !! Strickler Reibungsbeiwert Kst_rau (Mannings n, here: Kst=1/n)
   enddo ! alle n Knoten
   print*,"##### preliminary ##### modellg: call sc_read_rough: Strickler = 70 ######"
   return
end subroutine sc_read_rough
!----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
