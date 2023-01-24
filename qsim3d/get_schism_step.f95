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
!> <h1> SUBROUTINE get_schism_step </h1>
!! reads next timestep from result netcdf-file.
!! \n\n
!! aus Datei get_schism_step.f95 ; zurück: \ref lnk_transport_schism .
subroutine get_schism_step(nt)
      use netcdf
      use modell
      use schism_glbl, only:su2,sv2,tr_el,eta2                       &
      ,npa,np, nsa,ne, nea, nvrt, ns_global,ne_global,np_global      &
      ,ielg,iplg,islg,isidenode, znl, zs, dp,idry,idry_e,idry_e_2t   &
      ,idry_s,nea2,dfh,hdif,flux_adv_vface,ntracers,ze,zs,iegrpv,    &
       npa2, nea2, nsa2, znl, kbp
      
      use schism_msgp, only: myrank,nproc,parallel_abort,            &
      exchange_s3dw,nnbr,nbrrank,nnbr_2t,nbrrank_2t,                 &
      exchange_e3d_2t_tr, exchange_p2d
      
      implicit none
      
      include "netcdf.inc"
      
      integer :: nt, nst, nin ,iret, varid !, np_p
      integer :: i,j,k,l,m,n, istat,ierr
      character (len = 400) :: dateiname, chari
      integer :: start4(4), count4(4)
      integer :: start3(3), count3(3)
      integer :: start2(2), count2(2)
      integer :: ndims, nVars, nGlobalAtts, unlimdimid, nAtts
      integer , allocatable , dimension (:) :: dlength
      character(256) , allocatable , dimension (:) :: dname
      integer, dimension(nf90_max_var_dims) :: dimids
      integer , allocatable , dimension (:) :: vxtype, vndims
      character(256) , allocatable , dimension (:) :: vname
      real vel_norm, vel_dir, vel_sum, minwert, maxwert, tempi, sump, minima, maxima
      !> arrays to read stored variables from .nc files, each process its part
      real , allocatable , dimension (:) :: var_p
      real , allocatable , dimension (:) :: var1_p
      real , allocatable , dimension (:) :: var2_p
      ! array into which var_p variables are gathered (recombined) on process 0
      real , allocatable , dimension (:) :: var_g
      real , allocatable , dimension (:) :: var1_g
      real , allocatable , dimension (:) :: var2_g
      
      if((nt.lt.1).or.(nt.gt.transinfo_anzahl))then
         print*,'nt,transinfo_anzahl=',nt,transinfo_anzahl
         call qerror('get_schism_step no valid timestep number')
      endif
      if(maxstack.lt.nea)then
         write(fehler,*)meinrang,' get_schism_step: maxstack.lt.nea',maxstack,nea
         call qerror(fehler)
      endif
      if(maxstack.lt.nsa)then
         write(fehler,*)meinrang,' get_schism_step: maxstack.lt.nsa',maxstack,nsa
         call qerror(fehler)
      endif
      if (meinrang == 0) then
         allocate(var_g(proz_anz*maxstack),stat = istat)
         allocate(var1_g(proz_anz*maxstack),stat = istat)
         allocate(var2_g(proz_anz*maxstack),stat = istat)
         if (istat /= 0) call qerror("allocate var2_g( failed")
      endif
      allocate(var_p(maxstack),stat = istat)
      allocate(var1_p(maxstack),stat = istat)
      allocate(var2_p(maxstack),stat = istat)
      if (istat /= 0) call qerror("allocate var2_p( failed")
      nst = transinfo_stack(transinfo_zuord(nt))
      nin = transinfo_instack(transinfo_zuord(nt))
      if (meinrang == 0)then
         print*,'get_schism_step: nt,zuord,zeit = ',nt,transinfo_zuord(nt),transinfo_zeit(transinfo_zuord(nt))   &
               ,'|',transinfo_instack(nt),'-th timestep in stack=',transinfo_stack(nt)
      endif
      istat=0
      if(.not.allocated(tr_el))then
         allocate(tr_el(ntracers,nvrt,nea2), stat = istat )
         if (istat /= 0) then
            write(fehler,*)'get_schism_step:  return value allocate tr_el :', istat
            call qerror(fehler)
         else
            print*,meinrang,'get_schism_step: allocate(tr_el',ntracers,nvrt,nea2
         end if
         tr_el=0.0
      endif !tr_el allocated
 
      call mpi_barrier (mpi_komm_welt, ierr)
   
      !######################### open stack schout_******_*.nc ################################################
      write(chari,*),nst
      write(dateiname,"(2A,I6.6,3A)")trim(modellverzeichnis),"outputs_schism/schout_",meinrang,"_",trim(adjustl(chari)),".nc" !schout_0001_1.nc
      iret = nf_open(dateiname, NF_NOWRITE, ncid)
      if (iret /= 0) then ! open error
         call check_err( iret )
         write(fehler,*)meinrang,' get_schism_step: nf_open failed, iret = ',iret,' dateiname=',trim(dateiname)
         call qerror(fehler)
      !else ! no open error
      !   print*,meinrang,"get_schism_step opens: ", trim(adjustl(dateiname)), ncid
      end if ! open error
      call mpi_barrier (mpi_komm_welt, ierr)!#!
      
      !! get Dimensions
      call check_err( nf90_inquire(ncid, ndims, nVars, nGlobalAtts, unlimdimid) ) !--- overview
      allocate (dlength(ndims),dname(ndims), stat = istat)
      allocate (vxtype(nVars),vndims(nVars),vname(nVars),  stat = istat )
      do j = 1,ndims
         iret = nf90_Inquire_Dimension(ncid, j, dname(j), dlength(j))
         call check_err(iret)
         if (iret /= 0) print*,meinrang,j," get_schism_step nf90_Inquire_Dimension failed iret = ",iret
      end do ! all dimensions
      call mpi_barrier (mpi_komm_welt, ierr)!#!

      !######################### allocate for nodes ################################################
      if(.not.allocated(znl))allocate(znl(nvrt,npa2),stat = istat); znl=0.0
      if(.not.allocated(kbp))then
         print*,meinrang,' kbp not allocated in get_schism_step',npa2
         allocate(kbp(npa2),stat = istat)
         kbp=0.0
      !else
      !   print*,'kbp allocated in get_schism_step'
      endif

      !######################### wetdry_node ################################################
      call check_err( nf_inq_varid(ncid,"wetdry_node", varid) )
      call check_err( nf90_inquire_variable(ncid,varid,vname(varid),vxtype(varid),vndims(varid),dimids, nAtts) )
      call mpi_barrier (mpi_komm_welt, ierr)!#!
      if (dlength(dimids(1)) > maxstack)call qerror("wetdry_node:dlength(dimids(1)) > maxstack")
      n=dlength(dimids(1))
      !print*,meinrang,' get_schism_step wetdry_node dlength(dimids(1))=',n,' np,npa,maxstack=', np,npa,maxstack
      !! initialize
      var_p = 666.666
      if (meinrang == 0) var_g = 777.777
      call mpi_barrier (mpi_komm_welt, ierr)
      if(.not.allocated(idry))allocate(idry(npa),stat = istat)
      if (istat /= 0) call qerror("allocate idry( failed")
      start2 = (/ 1, nin /)
      count2 = (/ npa, 1 /) ! nodenumber first dimension
      iret = nf90_get_var(ncid, varid, var_p(1:npa), start2, count2 )
      call check_err(iret)
      if (iret /= 0) print*,meinrang,' get_schism_step nf90_get_var wetdry_node failed iret = ',iret
      !print*,meinrang,' Knoten trocken var_p from...until ',minval(var_p(1:np)),maxval(var_p(1:np))
      idry(1:npa) = int(var_p(1:npa))
      !print*,meinrang,' Knoten trocken idry from...until ',minval(idry(1:np)),maxval(idry(1:np))
      call mpi_barrier (mpi_komm_welt, ierr)
      ! gather var_p into var_g
      call MPI_Gather(var_p, maxstack, MPI_FLOAT, var_g, maxstack, MPI_FLOAT, 0, mpi_komm_welt, ierr)
      if (ierr /= 0) then
         write(fehler,*)"get_schism_step MPI_Gather(var_p idry failed : ", ierr
         call qerror(fehler)
      end if
      call mpi_barrier (mpi_komm_welt, ierr)
      !! recombine into global numbers
      if (meinrang == 0) then
         !print*,' knoten_trocken allocated to size=',size( knoten_trocken )
         knoten_trocken = -555 ! init
         do j = 1,proz_anz ! all processes/ranks
            do k = 1,np_sc(j) ! all nodes at this rank
               knoten_trocken(iplg_sc(j,k)) = int(var_g((j-1)*maxstack+k))
            end do
         end do
         print*,'get_schism_step knoten_trocken minwert, maxwert = ',minval(knoten_trocken),maxval(knoten_trocken)
      end if ! meinrang==0
      call mpi_barrier (mpi_komm_welt, ierr)

      !######################### elev eta2 p rb_hydraul(3 ################################################
      !        if(iof_hydro(1)==1) call writeout_nc(id_out_var(5),'elev',1,1,npa,swild(1:npa))
      ! find elev
      call check_err( nf_inq_varid(ncid,"elev", varid) )
      call check_err( nf90_inquire_variable(ncid,varid,vname(varid),vxtype(varid),vndims(varid),dimids, nAtts) )
      call mpi_barrier (mpi_komm_welt, ierr)!#!
      if (dlength(dimids(1)) > maxstack)call qerror("elev:dlength(dimids(1)) > maxstack")
      n=dlength(dimids(1))
      !print*,meinrang,' get_schism_step elev dlength(dimids(1))=',n,' np,npa,maxstack=', np,npa,maxstack
      !! initialize
      var_p = 666.666
      if (meinrang == 0) var_g = 777.777
      call mpi_barrier (mpi_komm_welt, ierr)
      if(.not.allocated(eta2))allocate(eta2(npa2),stat = istat); eta2=0.0
      if (istat /= 0) call qerror("allocate eta2( failed")
      !! get data
      start2 = (/ 1, nin /)
      count2 = (/ npa, 1 /) ! nodenumber first dimension
      iret = nf90_get_var(ncid, varid, var_p(1:npa), start2, count2 )
      call check_err(iret)
      if (iret /= 0) print*,meinrang," get_schism_step nf90_get_var elev failed iret = ",iret
      eta2(1:npa) = var_p(1:npa)
      !print*,meinrang,' elev np  from...until ',minval(eta2(1:np)),maxval(eta2(1:np))
      call mpi_barrier (mpi_komm_welt, ierr)
      ! gather var_p into var_g
      call MPI_Gather(var_p, maxstack, MPI_FLOAT, var_g, maxstack, MPI_FLOAT, 0, mpi_komm_welt, ierr)
      if (ierr /= 0) then
         write(fehler,*)"get_schism_step MPI_Gather(var_p elev failed : ", ierr
         call qerror(fehler)
      end if
      !print*,meinrang,'get_schism_step before eta2 min/max np,npa,npa2 = ',  &
      !      minval(eta2(1:np)),maxval(eta2(1:np)),  &
      !      minval(eta2(np+1:npa)),maxval(eta2(np+1:npa)),  &
      !      minval(eta2(npa+1:npa2)),maxval(eta2(npa+1:npa2))
      call mpi_barrier (mpi_komm_welt, ierr)
      !! recombine into global numbers
      if (meinrang == 0) then
         if(.not.allocated(p))then
            call qerror('get_schism_step .not. allocated(p)')
         !else
         !   print*,' p allocated to size=',size( p )
         endif
         p = -555.555 ! init
         !print*,meinrang," get_schism_step elev recombine into global",npa,varid,trim(adjustl(vname(varid)))
         do j = 1,proz_anz ! all processes/ranks
            !print*,j," get_schism_step np_sc=",np_sc(j)
            do k = 1,np_sc(j) ! all nodes at this rank
               p(iplg_sc(j,k)) = var_g((j-1)*maxstack+k)
            end do
         end do
         print*,'get_schism_step wsp minwert, maxwert = ',minval(p),maxval(p)
         do k=1,knotenanzahl2D
            if(p(k) .lt. -100.0)print*,k,' get_schism_step p<100 ',p(k)
         enddo
         ! set water level at elements
         do j = 1,number_plankt_point ! all j elements
            sump=0.0
            do m=1,cornernumber(j)
               sump=sump+p(elementnodes(j,m))
            end do ! all m corners
            rb_hydraul(3+(j-1)*number_rb_hydraul) = sump/real(cornernumber(j))
         end do ! all j elements
         ! rb_hydraul(1+(j-1)*number_rb_hydraul) = u(j)
         ! rb_hydraul(2+(j-1)*number_rb_hydraul) = p(j)-knoten_z(j) ! Tiefe
         ! rb_hydraul(3+(j-1)*number_rb_hydraul) = p(j)
         ! benthic_distribution(44+(j-1)*number_benth_distr) = 0.1      ! ks #### jetzt anders gelöst mit zone()%reib
         ! benthic_distribution(45+(j-1)*number_benth_distr) = 0.1*u(j) ! utau
      end if ! proc. 0 only
      call mpi_barrier (mpi_komm_welt, ierr)
      
      ! complement ghost element values
      call exchange_p2d(eta2)
      
      print*,meinrang,'get_schism_step after eta2 min/max np,npa,npa2 = ',  &
            minval(eta2(1:np)),maxval(eta2(1:np)),  &
            minval(eta2(np+1:npa)),maxval(eta2(np+1:npa)),  &
            minval(eta2(npa+1:npa2)),maxval(eta2(npa+1:npa2))
      
      !######################### dp tiefe depth rb_hydraul(2  ################################################
      ! find depth
      call check_err( nf_inq_varid(ncid,"depth", varid) )
      call check_err( nf90_inquire_variable(ncid,varid,vname(varid),vxtype(varid),vndims(varid),dimids, nAtts) )
      call mpi_barrier (mpi_komm_welt, ierr)!#!
      if (dlength(dimids(1)) > maxstack)call qerror("depth:dlength(dimids(1)) > maxstack")
      n=dlength(dimids(1))
      !!print*,meinrang,' get_schism_step depth dlength(dimids(1))=',n,' np,npa,maxstack=', np,npa,maxstack
      !!! initialize
      var_p = 666.666
      if (meinrang == 0) var_g = 777.777
      call mpi_barrier (mpi_komm_welt, ierr)
      if(.not.allocated(dp)) allocate(dp(npa2),stat=istat); dp=0.0
      if (istat /= 0) call qerror("allocate dp( failed")
      !!! get data
      start2 = (/ 1, nin /)
      count2 = (/ npa, 1 /) ! nodenumber first dimension
      iret = nf90_get_var(ncid, varid, var_p(1:npa), start2, count2 )
      if (iret /= 0) print*,meinrang," get_schism_step nf90_get_var dp failed iret = ",iret
      call check_err(iret)
      dp(1:npa) = var_p(1:npa)
      !print*,meinrang,'get_schism_step dp before min/max np,npa,npa2 = ',  &
      !      minval(dp(1:np)),maxval(dp(1:np)),  &
      !      minval(dp(np+1:npa)),maxval(dp(np+1:npa)),  &
      !      minval(dp(npa+1:npa2)),maxval(dp(npa+1:npa2))
      
      ! complement ghost element values
      call exchange_p2d(dp)
      
      !print*,meinrang,'get_schism_step dp after min/max np,npa,npa2 = ',  &
      !      minval(dp(1:np)),maxval(dp(1:np)),  &
      !      minval(dp(np+1:npa)),maxval(dp(np+1:npa)),  &
      !      minval(dp(npa+1:npa2)),maxval(dp(npa+1:npa2))

      call mpi_barrier (mpi_komm_welt, ierr)
      ! gather var_p into var_g
      call MPI_Gather(var_p, maxstack, MPI_FLOAT, var_g, maxstack, MPI_FLOAT, 0, mpi_komm_welt, ierr)
      if (ierr /= 0) then
         write(fehler,*)"get_schism_step MPI_Gather(var_p dp failed : ", ierr
         call qerror(fehler)
      end if
      call mpi_barrier (mpi_komm_welt, ierr)
      !!! recombine into global numbers
      if (meinrang == 0) then
         if(.not.allocated(knoten_z))then
            call qerror('get_schism_step .not. allocated(knoten_z)')
         endif
         knoten_z = -555.555 ! init
         do j = 1,proz_anz ! all processes/ranks
            do k = 1,np_sc(j) ! all nodes at this rank
               knoten_z(iplg_sc(j,k)) = -1.0 * var_g((j-1)*maxstack+k)
            end do
         end do
         do k=1,knotenanzahl2D
            if(knoten_z(k) .lt. -100.0)print*,k,' get_schism_step p<100 ',p(k)
         enddo
         ! set water depth at elements
         do j = 1,number_plankt_point ! all j elements
            sump=0.0
            do m=1,cornernumber(j)
               sump=sump+knoten_z(elementnodes(j,m))
            end do ! all m corners
            rb_hydraul(2+(j-1)*number_rb_hydraul) = sump/real(cornernumber(j))
            rb_hydraul(2+(j-1)*number_rb_hydraul) = rb_hydraul(3+(j-1)*number_rb_hydraul)-rb_hydraul(2+(j-1)*number_rb_hydraul)
         end do ! all j elements
         print*,'### get_schism_step bathymetry ###, knoten_z minwert, maxwert = '  &
               ,minval(knoten_z),maxval(knoten_z)
      end if ! proc. 0 only
      call mpi_barrier (mpi_komm_welt, ierr)
      
      !######################### diffusivity(time, nSCHISM_hgrid_node, ################################################
      !        if(iof_hydro(21)==1) call writeout_nc(id_out_var(25),'diffusivity',2,nvrt,npa,dfh)
      call check_err( nf_inq_varid(ncid,"diffusivity", varid) )
      call check_err( nf90_inquire_variable(ncid,varid,vname(varid),vxtype(varid),vndims(varid),dimids, nAtts) )
      call mpi_barrier (mpi_komm_welt, ierr)!#!
      if (dlength(dimids(1)) > maxstack)call qerror("diffusivity:dlength(dimids(1)) > maxstack")
      n=dlength(dimids(1))
      !print*,meinrang,' get_schism_step diffusivity dlength(dimids(1))=',n,' np,npa,maxstack=', np,npa,maxstack
      !! initialize
      var_p = 666.666
      if (meinrang == 0) var_g = 777.777
      call mpi_barrier (mpi_komm_welt, ierr)
      if(.not.allocated(dfh))allocate(dfh(nvrt,npa),stat = istat)
      if (istat /= 0) call qerror("allocate dfh( failed")
      !! get data
      minwert=0.0; maxwert=0.0
      do i = 1,nvrt
         start3 = (/i, 1, nin /)
         count3 = (/1, npa, 1 /) ! nodenumber first dimension
         iret = nf90_get_var(ncid, varid, var_p(1:npa), start3, count3 )
         call check_err(iret)
         if (iret /= 0) print*,meinrang," get_schism_step nf90_get_var diffusivity failed iret = ",iret,i
         dfh(i,1:npa) = var_p(1:npa)
         minima=minval(dfh(i,1:np))
         maxima=maxval(dfh(i,1:np))
         call mpi_barrier (mpi_komm_welt, ierr)
         minwert=min(minwert,minima)
         maxwert=max(maxwert,maxima)
      end do ! all i levels
      call mpi_reduce(minwert,minima,1,MPI_FLOAT,mpi_min,0,mpi_komm_welt,ierr)
      call mpi_reduce(maxwert,maxima,1,MPI_FLOAT,mpi_max,0,mpi_komm_welt,ierr)
      if(meinrang==0)print*,'diffusivity dfh at nodes  from...until ',minima,maxima

      !######################### hdif(time, nSCHISM_hgrid_node ################################################
      !        call writeout_nc(id_out_var(noutput+3),'hdif',2,nvrt,npa,hdif)  ! horizontal diffusivity
      !  real(rkind),save,allocatable :: hdif(:,:) !horizontal diffusivity
      !allocate hdif(nvrt,npa)
      call check_err( nf_inq_varid(ncid,"hdif", varid) )
      call check_err( nf90_inquire_variable(ncid,varid,vname(varid),vxtype(varid),vndims(varid),dimids, nAtts) )
      call mpi_barrier (mpi_komm_welt, ierr)!#!
      if (dlength(dimids(1)) > maxstack)call qerror("hdif:dlength(dimids(1)) > maxstack")
      n=dlength(dimids(1))
      !print*,meinrang,' get_schism_step hdif dlength(dimids(1))=',n,' np,npa,maxstack=', np,npa,maxstack
      !! initialize
      var_p = 666.666
      if (meinrang == 0) var_g = 777.777
      call mpi_barrier (mpi_komm_welt, ierr)
      if(.not.allocated(hdif))allocate(hdif(nvrt,npa),stat = istat)
      if (istat /= 0) call qerror("allocate hdif( failed")
      !! get data
      minwert=0.0; maxwert=0.0
      do i = 1,nvrt
         start3 = (/i, 1, nin /)
         count3 = (/1, npa, 1 /) ! nodenumber first dimension
         iret = nf90_get_var(ncid, varid, var_p(1:npa), start3, count3 )
         call check_err(iret)
         if (iret /= 0) print*,meinrang," get_schism_step nf90_get_var hdif failed iret = ",iret,i
         hdif(i,1:npa) = var_p(1:npa)
         minima=minval(hdif(i,1:np))
         maxima=maxval(hdif(i,1:np))
         call mpi_barrier (mpi_komm_welt, ierr)
         minwert=min(minwert,minima)
         maxwert=max(maxwert,maxima)
      end do ! all i levels
      call mpi_reduce(minwert,minima,1,MPI_FLOAT,mpi_min,0,mpi_komm_welt,ierr)
      call mpi_reduce(maxwert,maxima,1,MPI_FLOAT,mpi_max,0,mpi_komm_welt,ierr)
      if(meinrang==0)print*,'hor.diff. hdif at nodes  from...until ',minima,maxima

      !######################### wetdry_elem ################################################
      call check_err( nf_inq_varid(ncid,"wetdry_elem", varid) )
      call check_err( nf90_inquire_variable(ncid,varid,vname(varid),vxtype(varid),vndims(varid),dimids, nAtts) )
      call mpi_barrier (mpi_komm_welt, ierr)!#!
      if (dlength(dimids(1)) > maxstack)call qerror("wetdry_elem:dlength(dimids(1)) > maxstack")
      !! initialize
      var_p = 666.666
      if (meinrang == 0) var_g = 777.777
      call mpi_barrier (mpi_komm_welt, ierr)
      if(.not.allocated(idry_e)) allocate(idry_e(nea),stat=istat);
      if (istat /= 0) call qerror('allocate idry_e( failed')
      if(.not.allocated(idry_e_2t)) allocate(idry_e_2t(nea2),stat=istat);
      if (istat /= 0) call qerror('allocate idry_e_2t( failed')
      !! get data
      start2 = (/ 1, nin /)
      count2 = (/ nea, 1 /) ! nodenumber first dimension
      iret = nf90_get_var(ncid, varid, var_p(1:nea), start2, count2 )
      if (iret /= 0) print*,meinrang," get_schism_step nf90_get_var idry_e failed iret = ",iret
      call check_err(iret)
      idry_e(1:nea) = int(var_p(1:nea))
      call mpi_barrier (mpi_komm_welt, ierr)
      ! gather var_p into var_g
      call MPI_Gather(var_p, maxstack, MPI_FLOAT, var_g, maxstack, MPI_FLOAT, 0, mpi_komm_welt, ierr)
      if (ierr /= 0) then
         write(fehler,*)"get_schism_step MPI_Gather(var_p idry_e failed : ", ierr
         call qerror(fehler)
      end if
      call mpi_barrier (mpi_komm_welt, ierr)
      !! recombine into global numbers
      if (meinrang == 0) then
         element_trocken=0
         do j = 1,proz_anz ! all processes/ranks
            do k = 1,ne_sc(j) ! all elements at this rank
               element_trocken(ielg_sc(j,k)) = int(var_g((j-1)*maxstack+k))
            end do
         end do
         print*,'get_schism_step: element_trocken min,max = ',minval(element_trocken),maxval(element_trocken)
      end if !meinrang==0
      call mpi_barrier (mpi_komm_welt, ierr)
      
      !######################### zelem ################################################
      call check_err( nf_inq_varid(ncid,"zelem", varid) )
      call check_err( nf90_inquire_variable(ncid,varid,vname(varid),vxtype(varid),vndims(varid),dimids, nAtts) )
      if(.not.allocated(ze)) allocate(ze(nvrt,nea),stat=istat);
      if (istat /= 0) call qerror('allocate ze( failed')
      ze=0.0
      call mpi_barrier (mpi_komm_welt, ierr)!#!
      if (dlength(dimids(1)) > maxstack)call qerror("temp_elem:dlength(dimids(1)) > maxstack")
      !! initialize
      var_p = 666.666
      if (meinrang == 0) var_g = 777.777
      call mpi_barrier (mpi_komm_welt, ierr)
      minwert=0.0; maxwert=0.0
      do i = 1,nvrt
         start3 = (/i, 1, nin /)
         count3 = (/1, nea, 1 /) ! nodenumber first dimension
         iret = nf90_get_var(ncid, varid, var_p(1:nea), start3, count3 )
         call check_err(iret)
         if (iret /= 0) print*,meinrang," get_schism_step nf90_get_var temp_elem failed iret = ",iret,i
         ze(i,1:ne) = var_p(1:ne)
         
          !! recombine into global numbers
         call MPI_Gather(var_p, maxstack, MPI_FLOAT, var_g, maxstack, MPI_FLOAT, 0, mpi_komm_welt, ierr)
         if (meinrang == 0) then
            if(.not.allocated(element_z)) allocate(element_z(nvrt,ne_global),stat=istat);
            element_z(i,:)=0.0 !initialize
            do j = 1,proz_anz ! all j processes/ranks
               do k = 1,ne_sc(j) ! all elements at this rank
                  element_z(i,ielg_sc(j,k)) = var_g((j-1)*maxstack+k)
               end do ! all k elements on this processor
            end do ! all j processes
         end if ! proc. 0 only
         call mpi_barrier (mpi_komm_welt, ierr)
         
      end do ! all i levels
      
      ! fill ghost elements ne:nea
      !#call exchange_s3dw(ze)
      
      do i = 1,nvrt
         print*,meinrang,' level from .nc i=',i,' ze(nea)  from...until '  &
               ,minval(ze(i,1:nea)),maxval(ze(i,1:nea))   &
               ,' ze(ne)  from...until',minval(ze(i,1:ne)),maxval(ze(i,1:ne))
      end do ! all i levels
      call mpi_barrier (mpi_komm_welt, ierr)
      
      !other way round
      !call levels0(0,0)
      !do i = 1,nvrt
      !   print*,meinrang,' level by levels0 i=',i,' ze(nea)  from...until '  &
      !         ,minval(ze(i,1:nea)),maxval(ze(i,1:nea))   &
      !         ,' ze(ne)  from...until',minval(ze(i,1:ne)),maxval(ze(i,1:ne))
      !end do ! all i levels
      !call mpi_barrier (mpi_komm_welt, ierr)


      !######################### temp_elem(time, nSCHISM_hgrid_face ################################################
      !        if(iof_hydro(29)==1) call writeout_nc(id_out_var(32),'temp_elem',6,nvrt,nea,tr_el(1,:,:))
      ! planktische_variablen.f95:      allocate( tr_el(ntracers,nvrt,nea2), stat = as )
      call check_err( nf_inq_varid(ncid,"temp_elem", varid) )
      call check_err( nf90_inquire_variable(ncid,varid,vname(varid),vxtype(varid),vndims(varid),dimids, nAtts) )
      call mpi_barrier (mpi_komm_welt, ierr)!#!
      if (dlength(dimids(1)) > maxstack)call qerror("temp_elem:dlength(dimids(1)) > maxstack")
      !! initialize
      var_p = 666.666
      if (meinrang == 0) var_g = 777.777
      call mpi_barrier (mpi_komm_welt, ierr)
      do i = 1,nvrt
         start3 = (/i, 1, nin /)
         count3 = (/1, nea, 1 /) ! nodenumber first dimension
         iret = nf90_get_var(ncid, varid, var_p(1:nea), start3, count3 )
         call check_err(iret)
         if (iret /= 0) print*,meinrang," get_schism_step nf90_get_var temp_elem failed iret = ",iret,i
         tr_el(1,i,1:nea) = var_p(1:nea)
         !!###tr_el(1,i,1:ne) = 22.22*real(i) !### test exchange_e3d_2t_tr
         !print*,meinrang,i,'temp_elem before from...until ne',  &
         !       minval(tr_el(1,i,1:ne)),maxval(tr_el(1,i,1:ne))
      end do ! all i levels
      !print*,meinrang,'nnbr,nbrrank min/max',nnbr,minval(nbrrank(:)),maxval(nbrrank(:))
      !print*,meinrang,'nnbr_2t,nbrrank_2t min/max',nnbr_2t,minval(nbrrank_2t(:)),maxval(nbrrank_2t(:))
      
      call mpi_barrier (mpi_komm_welt, ierr)
 
      !######################### salt_elem(time, nSCHISM_hgrid_face ################################################
      !        if(iof_hydro(30)==1) call writeout_nc(id_out_var(33),'salt_elem',6,nvrt,nea,tr_el(2,:,:))
      call check_err( nf_inq_varid(ncid,"salt_elem", varid) )
      call check_err( nf90_inquire_variable(ncid,varid,vname(varid),vxtype(varid),vndims(varid),dimids, nAtts) )
      call mpi_barrier (mpi_komm_welt, ierr)!#!
      if (dlength(dimids(1)) > maxstack)call qerror("salt_elem:dlength(dimids(1)) > maxstack")
      !! initialize
      var_p = 666.666
      if (meinrang == 0) var_g = 777.777
      call mpi_barrier (mpi_komm_welt, ierr)
      minwert=0.0; maxwert=0.0
      do i = 1,nvrt
         start3 = (/i, 1, nin /)
         count3 = (/1, nea, 1 /) ! nodenumber first dimension
         iret = nf90_get_var(ncid, varid, var_p(1:nea), start3, count3 )
         call check_err(iret)
         if (iret /= 0) print*,meinrang," get_schism_step nf90_get_var salt_elem failed iret = ",iret,i
         tr_el(2,i,1:nea) = var_p(1:nea)
         !tr_el(2,i,1:ne) = 35.0 ! ### exchange test
         !print*,meinrang,i,'salt_elem before from...until ne,nea,nea2 ',  &
         !     minval(tr_el(2,i,1:ne)),maxval(tr_el(2,i,1:ne)),  &
         !     minval(tr_el(2,i,1+ne:nea)),maxval(tr_el(2,i,1+ne:nea)),  &
         !     minval(tr_el(2,i,1+nea:nea2)),maxval(tr_el(2,i,1+nea:nea2))
      end do ! all i levels
      call mpi_barrier (mpi_komm_welt, ierr)
      
      ! complement ghost element values from neighbouring processes/domains
      call exchange_e3d_2t_tr(tr_el)
      
      !do i = 1,nvrt
      !   print*,meinrang,i,'salt_elem after from...until ne,nea,nea2 ',  &
      !        minval(tr_el(2,i,1:ne)),maxval(tr_el(2,i,1:ne)),  &
      !        minval(tr_el(2,i,1+ne:nea)),maxval(tr_el(2,i,1+ne:nea)),  &
      !        minval(tr_el(2,i,1+nea:nea2)),maxval(tr_el(2,i,1+nea:nea2))
      !end do ! all i levels

      !######################### flux_adv_vface(time, nSCHISM_hgrid_face ################################################
      ! call writeout_nc(id_out_var(noutput+1),'flux_adv_vface',6,nvrt,nea,flux_adv_vface(:,1,:))
      ! allocate   flux_adv_vface(nvrt,ntracers,nea)
      ! real(rkind),save,allocatable :: flux_adv_vface(:,:,:) !unmodified vertical fluxes (positive upward)
      ! real(rkind),save,allocatable :: total_mass_error(:) !(ntracers) Total mass error after advection step for mass correction
      if(.not.allocated(flux_adv_vface))allocate(flux_adv_vface(nvrt,ntracers,nea))
      !flux_adv_vface=-1.d34 !used in transport; init. as flags
      flux_adv_vface=0.0
      call check_err( nf_inq_varid(ncid,"flux_adv_vface", varid) )
      call check_err( nf90_inquire_variable(ncid,varid,vname(varid),vxtype(varid),vndims(varid),dimids, nAtts) )
      call mpi_barrier (mpi_komm_welt, ierr)!#!
      if (dlength(dimids(1)) > maxstack)call qerror("flux_adv_vface:dlength(dimids(1)) > maxstack")
      !! initialize
      var_p = 666.666
      if (meinrang == 0) var_g = 777.777
      call mpi_barrier (mpi_komm_welt, ierr)
      minwert=0.0; maxwert=0.0
      do i = 1,nvrt
         start3 = (/i, 1, nin /)
         count3 = (/1, nea, 1 /) ! nodenumber first dimension
         iret = nf90_get_var(ncid, varid, var_p(1:nea), start3, count3 )
         call check_err(iret)
         if (iret /= 0) print*,meinrang," get_schism_step nf90_get_var salt_elem failed iret = ",iret,i
         flux_adv_vface(i,1,1:nea) = var_p(1:nea)
         minima=minval(flux_adv_vface(i,1,1:ne))
         maxima=maxval(flux_adv_vface(i,1,1:ne))
         call mpi_barrier (mpi_komm_welt, ierr)
         minwert=min(minwert,minima)
         maxwert=max(maxwert,maxima)
      end do ! all i levels
      call mpi_reduce(minwert,minima,1,MPI_FLOAT,mpi_min,0,mpi_komm_welt,ierr)
      call mpi_reduce(maxwert,maxima,1,MPI_FLOAT,mpi_max,0,mpi_komm_welt,ierr)
      if(meinrang==0)print*,'flux_adv_vface  ### all concentrations ???? ###'
      if(meinrang==0)print*,'flux_adv_vface  from...until ',minima,maxima
      call mpi_barrier (mpi_komm_welt, ierr)

      !######################### wetdry_side ################################################
      call check_err( nf_inq_varid(ncid,"wetdry_side", varid) )
      call check_err( nf90_inquire_variable(ncid,varid,vname(varid),vxtype(varid),vndims(varid),dimids, nAtts) )
      call mpi_barrier (mpi_komm_welt, ierr)!#!
      if (dlength(dimids(1)) > maxstack)call qerror("wetdry_side:dlength(dimids(1)) > maxstack")
      !! initialize
      var_p = 666.666
      if (meinrang == 0) var_g = 777.777
      call mpi_barrier (mpi_komm_welt, ierr)
      if(.not.allocated(idry_s)) allocate(idry_s(nsa),stat=istat);
      if (istat /= 0) call qerror('allocate idry_s( failed')
      !! get data
      start2 = (/ 1, nin /)
      count2 = (/ nsa, 1 /) ! sidenumber first dimension
      iret = nf90_get_var(ncid, varid, var_p(1:nsa), start2, count2 )
      if (iret /= 0) print*,meinrang," get_schism_step nf90_get_var idry_s failed iret = ",iret
      call check_err(iret)
      idry_s(1:nsa) = int(var_p(1:nsa))
      if (meinrang == 0)print*,'get_schism_step: idry_s min,max = ',minval(idry_s),maxval(idry_s)
      call mpi_barrier (mpi_komm_welt, ierr)

      !######################### zside ################################################
      call check_err( nf_inq_varid(ncid,"zside", varid) )
      call check_err( nf90_inquire_variable(ncid,varid,vname(varid),vxtype(varid),vndims(varid),dimids, nAtts) )
      if(.not.allocated(zs)) allocate(zs(nvrt,nsa),stat=istat);
      if (istat /= 0) call qerror('allocate zs( failed')
      zs=0.0
      call mpi_barrier (mpi_komm_welt, ierr)!#!

      !######################### hvel_side(time, nSCHISM_hgrid_edge, nSCHISM_vgrid_layers, two ################################################
      !## if(iof_hydro(27)==1) call writeout_nc(id_out_var(30),'hvel_side',8,nvrt,nsa,su2,sv2)  !su2(nvrt,nsa),sv2(nvrt,nsa)
      if (meinrang == 0) then
         ed_vel_x(:) = 0.0 ; ed_vel_y(:) = 0.0
      end if !meinrang==0
      if(.not.allocated(su2))allocate(su2(nvrt,nsa),stat=istat)
      if (istat /= 0) call qerror("allocate su2( failed")
      if(.not.allocated(sv2))allocate(sv2(nvrt,nsa),stat=istat)
      if (istat /= 0) call qerror("allocate sv2( failed")
      call mpi_barrier (mpi_komm_welt, ierr)
      
      iret = nf_inq_varid(ncid,"hvel_side", varid)
      if (iret /= 0) then
         call check_err( iret )
         write(fehler,*)" get_schism_step: nf_inq_varid(ncid, >  > hvel_side <  < failed, iret = ",iret, " rank = ",meinrang
         call qerror(fehler)
      end if
      iret = nf90_inquire_variable(ncid,varid,vname(varid),vxtype(varid),vndims(varid),dimids, nAtts)
      call check_err( iret )
      if (iret /= 0)call qerror("get_schism_step nf90_inquire_variable hvel_side failed")
      if (dlength(dimids(1)) > maxstack)call qerror("wetdry_elem:dlength(dimids(1)) > maxstack")
      !! initialize
      var1_p(:) = 666.666; var2_p(:) = 666.666
      do i = 1,nvrt
         ! float hvel_side(time, nSCHISM_hgrid_edge, nSCHISM_vgrid_layers, two) ;
         start4 = (/1, i, 1  , nin /)
         count4 = (/1, 1, nsa, 1 /) ! side/edgenumber second dimension ??
         iret = nf90_get_var(ncid, varid, var1_p(1:nsa), start4, count4 )
         if (iret /= 0) print*,meinrang," get_schism_step nf90_get_var hvel_side failed iret = ",iret
         call check_err(iret)
         su2(i,1:nsa) = var1_p(1:nsa)
         start4 = (/2, i, 1  , nin /)
         count4 = (/1, 1, nsa, 1 /) ! side/edgenumber second dimension ??
         iret = nf90_get_var(ncid, varid, var2_p(1:nsa), start4, count4 )
         if (iret /= 0) print*,meinrang," get_schism_step nf90_get_var hvel_side failed iret = ",iret
         call check_err(iret)
         sv2(i,1:nsa) = var2_p(1:nsa)
         call mpi_barrier (mpi_komm_welt, ierr)
         call MPI_Gather(var1_p, maxstack, MPI_FLOAT, var1_g, maxstack, MPI_FLOAT, 0, mpi_komm_welt, ierr)
         call MPI_Gather(var2_p, maxstack, MPI_FLOAT, var2_g, maxstack, MPI_FLOAT, 0, mpi_komm_welt, ierr)

         !! recombine into global numbers !ed_vel_x, ed_vel_y
         if (meinrang == 0) then
            do j = 1,proz_anz ! all j processes/ranks
               do k = 1,ns_sc(j) ! all edges/sides at this rank
                  !!!!ed_vel_x(islg_sc(j,k)) = ed_vel_x(islg_sc(j,k))+var1_g((j-1)*maxstack+k)
                  !!!!ed_vel_y(islg_sc(j,k)) = ed_vel_y(islg_sc(j,k))+var2_g((j-1)*maxstack+k)
                  !#! if(i==1)ed_vel_x(islg_sc(j,k)) = var1_g((j-1)*maxstack+k)
                  ed_vel_x(islg_sc(j,k)) = var1_g((j-1)*maxstack+k)
                  !#! if(i==1)ed_vel_y(islg_sc(j,k)) = var2_g((j-1)*maxstack+k)
                  ed_vel_y(islg_sc(j,k)) = var2_g((j-1)*maxstack+k)
               end do ! all k sides on this processor
            end do ! all j processes
         end if ! proc. 0 only
         call mpi_barrier (mpi_komm_welt, ierr)
         !!!!if (meinrang == 0) then
         !!!!   ed_vel_x(:)/real(nvrt)
         !!!!   ed_vel_y(:)/real(nvrt)
         !!!!endif ! proc. 0 only
         !!!!call mpi_barrier (mpi_komm_welt, ierr)
         if (meinrang == 0) then
            maxima=0.0
            do n = 1,number_plankt_point! all n elements
               vel_sum=0.0
               if(cornernumber(n)==0)call qerror('get_schism_step: cornernumber(n)==0')
               do j=1,cornernumber(n)
                  k=elementedges(n,j)
                  vel_sum=vel_sum+sqrt(ed_vel_x(k)**2 + ed_vel_y(k)**2)
               end do ! all j corners
               rb_hydraul(1+(n-1)*number_rb_hydraul)=vel_sum/real(cornernumber(n)) !### tiefenaufgelöst ???
               if(maxima<rb_hydraul(1+(n-1)*number_rb_hydraul))maxima=rb_hydraul(1+(n-1)*number_rb_hydraul)
            end do ! all n elements
            print*,'level=',i,  &
            ' vel.mag. rb_hydraul(1 at elements mean from adjacent edge-vel. su2,sv2 | max=',maxima

            if(.not.allocated(u))then
               call qerror('get_schism_step .not. allocated(u)')
            endif
            u = 0.0 ! init node vel.
            do n = 1,kantenanzahl
               u(top_node(n))=u(top_node(n))+sqrt(ed_vel_x(n)**2 + ed_vel_y(n)**2)
               u(bottom_node(n))=u(bottom_node(n))+sqrt(ed_vel_x(n)**2 + ed_vel_y(n)**2)
            end do ! alle kanten
            do n = 1,knotenanzahl2D
               if(knot_kant(n)==0)call qerror('get_schism_step knot_kant(n)==0')
               u(n)=u(n)/real(knot_kant(n))
            end do ! alle n knoten
            maxima=maxval(u)
            print*,'level=',i,  &
            ' vel.mag. u at nodes mean from linked edge-vel. su2,sv2 | max=',maxima
         end if !meinrang==0
      end do ! all i levels

      !######################### close clean return
      iret = nf_close(ncid)
      if (iret /= 0) print*,meinrang,' get_schism_step: nf_close(ncid) failed iret,ncid = ',iret,ncid
      call check_err(iret)
      if (meinrang == 0) then
         deallocate(var_g,var1_g,var2_g,stat = istat)
         if (istat /= 0) call qerror("deallocate var_g( failed")
      endif
      deallocate(var_p,var1_p,var2_p,stat = istat)
      if (istat /= 0) call qerror("deallocate var_p( failed")
      deallocate (dlength,dname, stat = istat)
      deallocate (vxtype,vndims,vname,  stat = istat )
      !print*,meinrang,'leaving get_schism_step'
      call mpi_barrier (mpi_komm_welt, ierr)
      return
end subroutine get_schism_step
      
!################################################################################################################################
      !call qerror('get_schism_step ### in development ###')
   
   !!!!!  znl depth discretisation at nodes !###   call writeout_nc(id_out_var(40),'znl',8,nvrt,npa,znl)
   !if ( .not. allocated(znl))allocate(znl(nvrt,npa))
   !if (meinrang == 0) var_g(:) = 777.777
   !var_p(:) = 0.0
   !call mpi_barrier (mpi_komm_welt, ierr)
   !call check_err( nf_inq_varid(ncid,"znl", varid) )
   !call check_err( nf90_inquire_variable(ncid,varid,vname(varid),vxtype(varid),vndims(varid),dimids, nAtts) )

!----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
