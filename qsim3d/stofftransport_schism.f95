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
!> \page Transport_SCHISM Transportinformationen von SCHISM
!! dargestellt im Bericht ...
!!
!! \n\n
!!    subroutine do_transport_tvd(it,ltvd,ntr,difnum_max_l) \n
!!    integer, intent(in) :: it !time stepping #; info only \n
!!    logical, intent(in) :: ltvd !true if TVD is used (must be for all tracers) \n
!!    integer, intent(in) :: ntr !# of tracers (for dimensioning) \n
!!    real(rkind), intent(out) :: difnum_max_l !max. horizontal diffusion number reached by this process (check stability)
!! \n\n
!!   use schism_glbl, only:\n
!!   Hydro/schism_init.F90:su2=0.d0; sv2=0.d0 | param.nml:  iof_hydro(26) = 1 !horizontal vel vector defined at side [m/s] needed for QSim\n
!!   tr_el   <- alle 3D-Konzentratienen in Elementmitte, tracer concentration @ prism center; used as temp. storage. tr_el(ntracers,nvrt,nea2)\n
!!   tr_nd0  <- Initial tracer conc. @ node and whole levels \n
!!   rkind   <- rkind = 8 Default real datatype \n
!!+++nvrt    <- , nvrt-Number of vertical layers\n
!!   ,ne,nea,nea2\n
!!   integer,save :: ne_global    ! Global number of elements\n
!!   integer,save :: ne           ! Local number of resident elements\n
!!   integer,save :: neg          ! Local number of ghost elements\n
!!+++integer,save :: nea          ! Local number of elements in augmented subdomain (ne+neg)\n
!!   integer,save :: neg2         ! Local number of 2-tier ghost elements\n
!!   integer,save :: nea2         ! Local number of elements in 2-tier augmented subdomain (ne+neg+neg2)\n
!! \n
!!   & ,tempmin,tempmax,saltmin,saltmax - Misc. variables shared between routines\n
!!   ,ns,nsa\n
!!   integer,save :: ns_global    ! Global number of sides\n
!!   integer,save :: ns           ! Local number of resident sides\n
!!   integer,save :: nsg          ! Local number of ghost sides\n
!!+++integer,save :: nsa          ! Local number of sides in augmented subdomain (ns+nsg) \n
!! \n
!!   natrm  ! natrm=12 !# of _available_ tracer models at the moment (including T,S) \n
!!   ihconsv ! beim Transport \=0 bewirkt nur clipping innerhalb tempmin-tempmax\n
!!   isconsv  !  beim Transport \=0 bewirkt nur clipping innerhalb saltmin-saltmax\n
!!   param.nml: !----------------------------------------------------------------------- \n
!!   ! Heat and salt exchange. isconsv=1 needs ihconsv=1; ihconsv=1 needs nws=2. \n
!!   ! If isconsv=1, need to compile with precip/evap module turned on. \n
!!   ihconsv = 0 !heat exchange option \n
!!   isconsv = 0 !evaporation/precipitation model \n
!!   !----------------------------------------------------------------------- \n
!!   ihdif ! Horizontal diffusion if(ihdif/=0)    ihdif=0 means all hdif=0 and no hdif.gr3 is needed \n
!!   h_tvd ! cut-off depth (m) for tvd \n
!!   errmsg ! Error message string, character(len=2000),save :: errmsg \n
!!   dt !   Zeitschritt in sekunden von param.nml CORE wird gespiegelt auf param.out.nml in outputs read(15,nml=CORE)  time_r=dt !time remaining \n
!!   namelist /CORE/ipre,ibc,ibtp,ntracer_gen,ntracer_age,sed_class,eco_class,nspool,ihfskip,msc2,mdc2,dt,rnday
!!   flux_adv_vface ! unmodified vertical fluxes (positive upward) ! allocate flux_adv_vface(nvrt,ntracers,nea)
!!                  ! flux_adv_vface from step routine. This routine cannot handle\n
!!   ! flux_adv_hface(nvrt,nsa) ! original horizontal flux (the local x-direction) \n
!!                  ! vnor1=su2(k,j)*snx(j)+sv2(k,j)*sny(j)\n
!!                  ! vnor2=su2(k-1,j)*snx(j)+sv2(k-1,j)*sny(j)\n
!!                  ! flux_adv_hface(k,j)=(zs(k,j)-zs(k-1,j))*distj(j)*(vnor1+vnor2)/2 !normal * area = flux (in local x-direction)\n
!!   su2,sv2 <- 3D-Geschwindigkeiten an den Kantenmitten, x & y-component of velocity at side centers & whole levels
!!              allocate su2(nvrt,nsa),sv2(nvrt,nsa)\n
!!   distj ! allocate(distj(nsa)! Side lengths\n
!!   sny,snx ! allocate snx(nsa),sny(nsa) ! side normal\n
!!   zs ! allocate zs(nvrt,nsa)! z-coord. (local frame - vertical up)\n
!!   idry_e ! wet/dry flag element dry==1\n
!!   idry_e_2t &  wet/dry flag including 2-tier ghost zone\n
!!   idry_s ! wet/dry flag side , idry_s(nsa)\n
!!   i34 ! elem. type (3 or 4) ! wird von read_mesh_nc_sc() gelesen , i34(nea)\n
!!   eta2 - Elevation at nodes at current timestep  allocate(eta2(npa)) \n
!!   dp ! Node depths , allocate(dp(npa ????? )), wird gebraucht zur Unterscheidung tvd !! knoten_z aus local_to_global_0000 eta2 aus *.nc \n
!!###  integer,save :: np_global    ! Global number of nodes
!!###  integer,save :: np           ! Local number of resident nodes
!!###  integer,save :: npg          ! Local number of ghost nodes
!!###  integer,save :: npa          ! Local number of nodes in augmented subdomain (np+npg)
!!###  integer,save :: npg2         ! Local number of 2-tier ghost nodes
!!###  integer,save :: npa2          ! Local number of nodes in 2-tier augmented subdomain (np+npg+npg2)
!!###  integer,save,allocatable :: iplg(:)      ! Local-to-global node index table (augmented)
!!###  type(llist_type),save,pointer :: ipgl(:) ! Global-to-local node index table (augmented)
!!   elnode  ! Element-node tables , allocate(elnode(4,nea))\n
!!   itvd_e ! TVD/WENO scheme will be used if itvd_e=1 and min(total depth @ 3 nodes) >=h_tvd ,  itvd_e(nea)\n
!!   kbs ! Side bottom vertical indices , kbs(nsa) \n
!!   kbe !  Element bottom vertical indices , allocate(kbe(nea) \n
!!   isbs ! local side to _global_ open bndry segment mapping , allocate(isbs(nsa)) ,
!!        ! if(isbs(isd)>0) then !open bnd; no sharing between processes, if(isbs(j)==-1) then !land bnd \n
!!   isbnd ! ! local node to _global_ open bndry segment flags , allocate(isbnd(-2:2,npa) \n
!!   isdel ! Side-element tables allocate(isdel(2,nsa) \n
!!   isidenode ! Side-node tables , allocate(isidenode(2,nsa) \n
!!   bdy_frc ! body force at prism center , bdy_frc(ntracers,nvrt,nea) \n
!!   flx_sf ! surface b.c. , flx_sf(ntracers,nea) \n
!!   flx_bt ! bottom b.c. , flx_bt(ntracers,nea)  \n
!!   elside !  Element-side tables , allocate(elside(4,nea) \n
!!   ic3 ! Element-side-element tables , allocate(ic3(4,nea) \n
!!   ssign  ! Sign associated with each side of an element , allocate(ssign(4,nea) \n
!!   iegl2 ! Global-to-local element index table (2-tier augmented) , allocate(iegl2(2,ne_global ?) \n
!!   ze !  z-coord (local frame - vertical up) , ze(nvrt,nea) \n
!!   dfh ! diffusivity , dfh(nvrt,npa) \n
!!   area ! Element areas , allocate(area(nea) \n
!!   ntrs ! A tracer model is activated iff ntrs()>0 | integer,save :: ntrs(natrm) | transport_TVD_QS.F90:if(ntrs(jj)<=0) cycle \n
!!   itrtype ! boundary types for tracers from "bctides.in", itrtype(natrm,max(1,nope_global)) \n
!!   trobc ! nudging factor , trobc(natrm,nope_global) \n
!!   irange_tr ! irange_tr(2,natrm) , read(31,*) trth(irange_tr(1,i):irange_tr(2,i),1,1,k) Index ranges of each module: \n
!!   trth ! time series of b.c. for T,S, tracers ! trth(:,:,:,:) , allocate ... trth(ntracers,nvrt,mnond_global,max(1,nope_global)) \n
!!   hdif ! horizontal diffusivity , hdif(nvrt,npa) , hdif.gr3\n
!!   delj ! Distance between adjacent elements of an internal side; used only in horizontal diffusion , delj(ns)\n
!! \n
!!       use schism_msgp, only: myrank,comm,ierr  & \n
!!      & , parallel_abort &         ! Abort parallel environment \n
!!      & , exchange_e3dw &        ! 3D-whole-level ghost element exchange \n
!!      & , exchange_e3d_tr2 &     ! Tracer transport ghost element exchange of type (ntracers,nvrt,nm) where nm>=nea \n
!!      & , exchange_e3d_2t_tr  &     ! 2-tier ghost elem. exchange of type (ntracers,nvrt,nm) where nm>=nea2 \n
!!      & , exchange_s3d_tr2 &     ! ghost side exchange of type (ntracers,nvrt,nm) where nm>=nsa \n
!!      & , exchange_e2di_2t &     ! 2-tier ghost elem. exchange of type (nm) where nm>=nea2 \n
!!      & , exchange_s3dw            ! 3D-whole-level ghost side exchange \n
!! \n\n
!! aus Datei stofftransport_schism.f95; zurück zu \ref lnk_datenstruktur oder \ref lnk_transport_numerik
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
   end do ! alle n Knoten
   print*,"##### preliminary ##### modellg: call sc_read_rough: Strickler = 70 ######"
   return
end subroutine sc_read_rough
!----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
