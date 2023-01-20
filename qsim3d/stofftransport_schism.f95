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
      use schism_msgp, only: myrank,parallel_abort, comm !,nproc
   
      implicit none
      include 'netcdf.inc'
      integer,parameter :: maxsubst = 60      ! max. number of substeps
      integer nt, n,j,k, subtim, diff, diffprev, alloc_status, ierr
      real :: laeng, cu_max, cu_min, dt_sub, sumwicht , difnum_max_l
      real , allocatable , dimension (:,:) :: zwischen
      integer :: num_sub
      integer nti(maxsubst)
   
      if (meinrang == 0) then !! prozessor 0 only
         print*,'stofftransport_schism: startzeitpunkt, zeitpunkt,endzeitpunkt=' ,startzeitpunkt, zeitpunkt, endzeitpunkt
         print*,'stofftransport_schism: izeit,na_transinfo,ne_transinfo=',izeit,na_transinfo,ne_transinfo
         !call qerror("preliminary Interrupt")
         call transinfo_schritte(startzeitpunkt, endzeitpunkt)
      endif
      call mpi_barrier (mpi_komm_welt, ierr)
      call MPI_Bcast(na_transinfo,1,MPI_INT,0,mpi_komm_welt,ierr)
      call MPI_Bcast(ne_transinfo,1,MPI_INT,0,mpi_komm_welt,ierr)
      
!      num_sub=1 !6?
!      do nt = 1,num_sub ! alle Transport (zwischen) Zeitschritte ??????????????????????????????????????????#############
!?? alle subschritte ???

      call get_schism_step(na_transinfo) !!****
      print*,meinrang,'stofftransport_schism: did get_schism_step()'
      call mpi_barrier (mpi_komm_welt, ierr)
!...  Recompute vgrid and calculate rewetted pts
      !if(inunfl==0) then
        call levels0(0,izeit) !(iths_main,it)
      !else
      !  call levels1(iths_main,it)
      !endif
      if(myrank==0) write(16,*) 'done recomputing levels0...'

         ! do_transport_tvd_imp(it,ntr,difnum_max_l)
         ! integer, intent(in) :: it !time stepping #; info only
         ! ntr=ntracers=number_plankt_vari
         ! real(rkind), intent(out) :: difnum_max_l !max. horizontal diffusion number reached by this process (check stability)
         
         !if (meinrang == 0)print*,  &
         !   '### no transport warning ### stofftransport_schism: do_transport_tvd_imp() not yet active ####'
      !print*,meinrang,myrank,'=meinrang,myrank'
      !print*,'mpi_komm_welt,comm,MPI_COMM_WORLD=',mpi_komm_welt,comm,MPI_COMM_WORLD 
      !myrank=meinrang
      !comm=mpi_komm_welt 
      
      call do_transport_tvd_imp(izeit,number_plankt_vari,difnum_max_l)

!      end do ! all sub timesteps
      
      !print*,'### no transport warning ### stofftransport_schism: do_transport_tvd_imp() not yet active ####'

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
   end do ! alle n Knoten
   print*,"##### preliminary ##### modellg: call sc_read_rough: Strickler = 70 ######"
   return
end subroutine sc_read_rough
!----+-----+-----+-----+-----+-----+-----+-----+-----+-----+

subroutine schism_transport_parameters
      use modell
      use schism_glbl, only: max_iadjust_mass_consv,              &
                           dtb_min_transport,                     &
                           mnweno1,nweno1,isten1,wmat1,wts1,      &
                           mnweno2,nweno2,isten2,wmat2,wts2,      &
                           fwts2,itvd_e,                          &
                           dt,max_subcyc,ne,nea,nquad
      implicit none
      integer istat
                          
      max_iadjust_mass_consv=0 ! iadjust_mass_consv0=0 !Enforce mass conservation for a tracer
      max_subcyc=10
      dtb_min_transport=dt/max_subcyc !min dt for transport allowed
      if(meinrang==0)print*,' schism_transport_parameters: dt=',dt,dtb_min_transport,max_subcyc
      
      mnweno1=0       !maxium number of p1 polynomial 
      allocate(nweno1(ne),stat=istat)     !number of p1 polynomial 
      nweno1=0
      !stencil of P1 polynomial (3 elements #,mnweno1 polynomials,ne)
      allocate(isten1(3,mnweno1,ne),stat=istat)
      isten1=0
      !polynomial coefficients at quadrature points (3 poly. coeffcients, mnweno1 ploynomials, 2 quadrature points, 3 sides, ne elem.)
      allocate(wmat1(3,mnweno1,nquad,4,ne),stat=istat) ! nquad from param.nml
      wmat1=0.0
      !coefficients for final p1 polynomial weight (3 components, xy direction,mnweno1 polynomials,ne)
      allocate(wts1(3,2,mnweno1,ne),stat=istat)
      wts1=0.0

      mnweno2=0       !maxium number of p2 polynomial 
      allocate(nweno2(ne),stat=istat)
      nweno2=0     !number of p2 polynomial
      !stencil of P2 polynomial (6 element #,mnweno2 polynomials,ne)
      allocate(isten2(6,mnweno2,ne),stat=istat)
      isten2=0
      ! wmat2 polynomial coefficient at quadrature points (3 poly. coeffcients, mnweno2 ploynomials, 2 quadrature points, 3 sides, ne elem.)
      allocate(wmat2(6,mnweno2,nquad,4,ne),stat=istat)
      wmat2=0.0
      ! wts2 coefficient for calculating final p2 polynomial weight (6 directions,5 components,mnweno2 polynomials,ne)
      allocate(wts2(6,5,mnweno2,ne),stat=istat)
      wts2=0.0
      ! fwts2 coefficients used in smoother calculation (3,ne)
      allocate(fwts2(5,ne),stat=istat)
      fwts2=0.0

!     TVD/WENO scheme will be used if itvd_e=1 
      allocate(itvd_e(nea),stat=istat)
      itvd_e=0 ! init. for upwind

      if(meinrang==0)print*,'schism_transport_parameters: ### warning ### init. only for upwind ; done'
      return
end subroutine schism_transport_parameters
!----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
