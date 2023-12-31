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
   use schism_glbl, only:su2,sv2,tr_el,eta2  &
   ,npa, nsa, nea, nvrt, ns_global,ne_global,np_global  &
   ,ielg,iplg,islg,isidenode, znl, zs
   use schism_msgp, only: myrank,nproc,parallel_abort
   
   implicit none
   include "netcdf.inc"
   
   integer :: nt, nst, nin ,iret, varid !, np_p
   integer :: i,j,k,l,m,n, istat
   character (len = 400) :: filename, chari
   integer :: start4(4), count4(4)
   integer :: start3(3), count3(3)
   integer :: start2(2), count2(2)
   integer :: ndims, nVars, nGlobalAtts, unlimdimid, nAtts
   integer , allocatable , dimension (:) :: dlength
   character(256) , allocatable , dimension (:) :: dname
   integer, dimension(nf90_max_var_dims) :: dimids
   integer , allocatable , dimension (:) :: vxtype, vndims
   character(256) , allocatable , dimension (:) :: vname
   real vel_norm, vel_dir, minwert, maxwert, tempi
   !> arrays to read stored variables from .nc files, each process its part
   real , allocatable , dimension (:) :: var_p
   real , allocatable , dimension (:) :: var1_p
   real , allocatable , dimension (:) :: var2_p
   ! array into which var_p variables are gathered (recombined) on process 0
   real , allocatable , dimension (:) :: var_g
   real , allocatable , dimension (:) :: var1_g
   real , allocatable , dimension (:) :: var2_g
   
   character(len= 50), parameter      :: calling_routine = 'get_schism_step'
   character(len=300)                 :: nc_error_prefix
   
   if (meinrang == 0) then
      allocate(var_g(proz_anz*maxstack),var1_g(proz_anz*maxstack),var2_g(proz_anz*maxstack),stat = istat)
      if (istat /= 0) call qerror("allocate var_g( failed")
   endif
   allocate(var_p(maxstack),var1_p(maxstack),var2_p(maxstack),stat = istat)
   if (istat /= 0) call qerror("allocate var_p( failed")
   if (meinrang == 0) then
      nst = transinfo_stack(transinfo_zuord(nt))
      nin = transinfo_instack(transinfo_zuord(nt))
      print*,"get_schism_step: nt,zuord,zeit = ",nt,transinfo_zuord(nt),transinfo_zeit(transinfo_zuord(nt))
   endif ! proc. 0 only
   call mpi_barrier (mpi_komm_welt, ierr)
   call MPI_Bcast(nst,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(nin,1,MPI_INT,0,mpi_komm_welt,ierr)
   call mpi_barrier (mpi_komm_welt, ierr)
   
   if (nst /= nst_prev) then ! proceed to next stack
      if (meinrang == 0)print*,"get_schism_step proceeds to next stack ",nst,ncid
      if (ncid /= -333) then
         nc_error_prefix = ''
         write(nc_error_prefix, '(a," - Rank ",i)') trim(calling_routine), meinrang
         call check_err( trim(nc_error_prefix), nf_close(ncid) ) ! close previous
      endif
      !! open nc files
      write(chari,*),nst
      write(filename,"(2A,I4.4,3A)")trim(modellverzeichnis),"outputs_schism/schout_",meinrang,"_",trim(adjustl(chari)),".nc" !schout_0001_1.nc
      nc_error_prefix = trim(calling_routine)//" - "//trim(filename)
      call check_err( trim(nc_error_prefix), nf_open(filename, NF_NOWRITE, ncid) )
      nst_prev = nst
   endif !next stack
   call mpi_barrier (mpi_komm_welt, ierr)!#!
   
   !! get Dimensions
   nc_error_prefix = trim(calling_routine)//" - "//trim(filename)
   call check_err( trim(nc_error_prefix), nf90_inquire(ncid, ndims, nVars, nGlobalAtts, unlimdimid) ) !--- overview
   allocate (dlength(ndims),dname(ndims), stat = istat)
   allocate (vxtype(nVars),vndims(nVars),vname(nVars),  stat = istat )
   do j = 1,ndims
      call check_err( trim(nc_error_prefix), nf90_Inquire_Dimension(ncid, j, dname(j), dlength(j)) )
   enddo ! all dimensions
   call mpi_barrier (mpi_komm_welt, ierr)!#!
   
   !!!!! elev -> p
   nc_error_prefix = ''
   write(nc_error_prefix, '(a," - Rank ",i," - elev")') trim(calling_routine), meinrang
   call check_err( trim(nc_error_prefix), nf_inq_varid(ncid,"elev", varid) )
   call check_err( trim(nc_error_prefix), nf90_inquire_variable(ncid,varid,vname(varid),vxtype(varid),vndims(varid),dimids, nAtts) )
   call mpi_barrier (mpi_komm_welt, ierr)!#!
   if (dlength(dimids(1)) > maxstack) call qerror("elev:dlength(dimids(1)) > maxstack")
   !! initialize
   do j = 1,maxstack
      var_p(j) = 666.666
   enddo ! all j
   if (meinrang == 0) then
      do j = 1,proz_anz*maxstack
         var_g(j) = 777.777
      enddo ! all j
   endif ! proc. 0 only
   call mpi_barrier (mpi_komm_welt, ierr)
   !! get data
   start2 = (/ 1, nin /)
   count2 = (/ npa, 1 /) ! nodenumber first dimension
   call check_err(trim(nc_error_prefix), nf90_get_var(ncid, varid, var_p(1:npa), start2, count2 ))
   eta2(1:npa) = var_p(1:npa)
   !print*,meinrang," get_schism_step eta2(3)=",eta2(3),iplg(3)
   !print*,meinrang," get_schism_step eta2(3)=",eta2(3),iplg(3)
   !print*,meinrang," eta2(topnode side 7",isidenode(1,7),") =",eta2(isidenode(1,7))
   !print*,meinrang," eta2(bottomnode side 7",isidenode(2,7),") =",eta2(isidenode(2,7))
   call mpi_barrier (mpi_komm_welt, ierr)
   ! gather var_p into var_g
   call MPI_Gather(var_p, maxstack, MPI_FLOAT, var_g, maxstack, MPI_FLOAT, 0, mpi_komm_welt, ierr)
   if (ierr /= 0) then
      write(fehler,*)"get_schism_step MPI_Gather(var_p elev failed : ", ierr
      call qerror(fehler)
   endif
   call mpi_barrier (mpi_komm_welt, ierr)
   !! recombine into global numbers
   if (meinrang == 0) then
      !print*,meinrang," get_schism_step elev recombine into global",npa,varid,trim(adjustl(vname(varid)))
      do j = 1,proz_anz ! all processes/ranks
         !print*,j," get_schism_step np_sc=",np_sc(j)
         do k = 1,np_sc(j) ! all nodes at this rank
            p(iplg_sc(j,k)) = var_g((j-1)*maxstack+k)
         enddo
      enddo
      minwert =  99999.9
      maxwert = -minwert
      do j = 1,knotenanzahl2D
         if (p(j) > maxwert)maxwert = p(j)
         if (p(j) < minwert)minwert = p(j)
      enddo ! all j
      print*," get_schism_step wsp minwert, maxwert = ",minwert, maxwert
   endif ! proc. 0 only
   call mpi_barrier (mpi_komm_welt, ierr)
   !print*,meinrang," get_schism_step elev recombined"
   
   !!!!! dahv -> u,dir
   nc_error_prefix = ''
   write(nc_error_prefix, '(a," - Rank ",i," - dahv")') trim(calling_routine), meinrang
   call check_err( trim(nc_error_prefix), nf_inq_varid(ncid, "dahv", varid) )
   call check_err( trim(nc_error_prefix), nf90_inquire_variable(ncid,varid,vname(varid),vxtype(varid),vndims(varid),dimids, nAtts) )
   if (dlength(dimids(2)) > maxstack) call qerror("dahv:dlength(dimids(2)) > maxstack")
   
   !! initialize
   do j = 1,maxstack
      var_p(j) = 666.666
   enddo ! all j
   if (meinrang == 0) then
      do j = 1,proz_anz*maxstack
         var_g(j) = 777.777
      enddo ! all j
   endif ! proc. 0 only
   call mpi_barrier (mpi_komm_welt, ierr)
   
   !! get data vel-x
   nc_error_prefix = ''
   write(nc_error_prefix, '(a," - Rank ",i," - dahv1")') trim(calling_routine), meinrang
   start3 = (/1, 1, nin /)
   count3 = (/1, npa, 1 /) ! nodenumber second dimension
   call check_err(trim(nc_error_prefix), nf90_get_var(ncid, varid, var_p(1:npa), start3, count3 ))
   !print*,meinrang," dahv_x (topnode side 7",isidenode(1,7),") =",var_p(isidenode(1,7))
   !print*,meinrang," dahv_x (bottomnode side 7",isidenode(2,7),") =",var_p(isidenode(2,7))
   call mpi_barrier (mpi_komm_welt, ierr)
   ! gather var_p into var_g
   call MPI_Gather(var_p, maxstack, MPI_FLOAT, var_g, maxstack, MPI_FLOAT, 0, mpi_komm_welt, ierr)
   if (ierr /= 0) then
      write(fehler,*)"get_schism_step MPI_Gather(var_p dahv1 failed : ", ierr
      call qerror(fehler)
   endif
   call mpi_barrier (mpi_komm_welt, ierr)
   !! recombine into global numbers
   if (meinrang == 0) then
      do j = 1,proz_anz ! all processes/ranks
         do k = 1,np_sc(j) ! all nodes at this rank
            u(iplg_sc(j,k)) = var_g((j-1)*maxstack+k)
         enddo
      enddo
   endif ! proc. 0 only
   call mpi_barrier (mpi_komm_welt, ierr)
   
   !! get data vel-y
   nc_error_prefix = ''
   write(nc_error_prefix, '(a," - Rank ",i," - dahv2")') trim(calling_routine), meinrang
   start3 = (/2, 1, nin /)
   count3 = (/1, npa, 1 /) ! nodenumber second dimension
   call check_err(trim(nc_error_prefix), nf90_get_var(ncid, varid, var_p(1:npa), start3, count3 ))
   !print*,meinrang," dahv_y (topnode side 7",isidenode(1,7),") =",var_p(isidenode(1,7))
   !print*,meinrang," dahv_y (bottomnode side 7",isidenode(2,7),") =",var_p(isidenode(2,7))
   call mpi_barrier (mpi_komm_welt, ierr)
   ! gather var_p into var_g
   call MPI_Gather(var_p, maxstack, MPI_FLOAT, var_g, maxstack, MPI_FLOAT, 0, mpi_komm_welt, ierr)
   if (ierr /= 0) then
      write(fehler,*)"get_schism_step MPI_Gather(var_p dahv2 failed : ", ierr
      call qerror(fehler)
   endif
   call mpi_barrier (mpi_komm_welt, ierr)
   !! recombine into global numbers
   if (meinrang == 0) then
      do j = 1,proz_anz ! all processes/ranks
         do k = 1,np_sc(j) ! all nodes at this rank
            dir(iplg_sc(j,k)) = var_g((j-1)*maxstack+k)
         enddo
      enddo
      !! split vel in norm and direction
      do j = 1,number_plankt_point
         vel_x(j) = u(j)
         vel_y(j) = dir(j)
         vel_norm = u(j)**2.0
         vel_dir = 0.0
         if (vel_norm > 0.0)vel_dir = atan(dir(j)/u(j))
         vel_norm = (vel_norm+dir(j)**2.0)**0.5
         u(j) = vel_norm
         dir(j) = vel_dir
      enddo
      minwert =  99999.9
      maxwert = -minwert
      do j = 1,knotenanzahl2D
         if (u(j) > maxwert)maxwert = u(j)
         if (u(j) < minwert)minwert = u(j)
      enddo ! all j
      !print*," get_schism_step u minwert, maxwert=",minwert, maxwert
   endif ! proc. 0 only
   call mpi_barrier (mpi_komm_welt, ierr)
   
   !!!!! hvel_side -> su2,sv2
   ! if(iof_hydro(26)==1) call writeout_nc(id_out_var(30),'hvel_side',8,nvrt,nsa,su2,sv2)
   nc_error_prefix = ''
   write(nc_error_prefix, '(a," - Rank ",i," - hvel_side")') trim(calling_routine), meinrang
   call check_err(trim(nc_error_prefix), nf_inq_varid(ncid,"hvel_side", varid))
   call check_err(trim(nc_error_prefix), nf90_inquire_variable(ncid,varid,vname(varid),vxtype(varid),vndims(varid),dimids, nAtts))
   
   !! initialize
   var_p(:) = 666.666
   if (meinrang == 0) var_g(:) = 777.777
   call mpi_barrier (mpi_komm_welt, ierr)
   
   nc_error_prefix = ''
   do i = 1,nvrt
      ! float hvel_side(time, nSCHISM_hgrid_edge, nSCHISM_vgrid_layers, two) ;
      start4 = (/1, i, 1  , nin /)
      count4 = (/1, 1, nsa, 1 /) ! side/edgenumber second dimension ??
      write(nc_error_prefix, '(a," - Rank ",i," - hvel_side1")') trim(calling_routine), meinrang
      call check_err(trim(nc_error_prefix), nf90_get_var(ncid, varid, var1_p(1:nsa), start4, count4 ))
      su2(i,1:nsa) = var1_p(1:nsa)
      start4 = (/2, i, 1  , nin /)
      count4 = (/1, 1, nsa, 1 /) ! side/edgenumber second dimension ??
      write(nc_error_prefix, '(a," - Rank ",i," - hvel_side2")') trim(calling_routine), meinrang
      call check_err(trim(nc_error_prefix), nf90_get_var(ncid, varid, var2_p(1:nsa), start4, count4 ))
      sv2(i,1:nsa) = var2_p(1:nsa)
      call mpi_barrier (mpi_komm_welt, ierr)
      call MPI_Gather(var1_p, maxstack, MPI_FLOAT, var1_g, maxstack, MPI_FLOAT, 0, mpi_komm_welt, ierr)
      call MPI_Gather(var2_p, maxstack, MPI_FLOAT, var2_g, maxstack, MPI_FLOAT, 0, mpi_komm_welt, ierr)
   enddo ! all i levels
   !print*,meinrang," nvrt=1 su2(7)=",su2(1,7)," sv2=",sv2(1,7)," global edge/side number=",islg(7)
   !print*,meinrang," nvrt=2 su2(7)=",su2(2,7)," sv2=",sv2(2,7)
   !print*,meinrang," global topnode number =",iplg(isidenode(1,7)),  &
   !&                " global bottomnode number =",iplg(isidenode(2,7))
   call mpi_barrier (mpi_komm_welt, ierr)
   !! recombine into global numbers !ed_vel_x, ed_vel_y
   if (meinrang == 0) then
      ed_vel_x(:) = 0.0 ; ed_vel_y(:) = 0.0
      do j = 1,proz_anz ! all j processes/ranks
         do k = 1,ns_sc(j) ! all edges/sides at this rank
            !do i=1,nvrt
            !#ed_vel_x(islg_sc(j,k))=ed_vel_x(islg_sc(j,k))+ var_g3(1,i,(j-1)*maxstack+k)
            !#ed_vel_y(islg_sc(j,k))=ed_vel_y(islg_sc(j,k))+ var_g3(2,i,(j-1)*maxstack+k)
            !enddo ! all i levels
            !erstmal nur level nvrt
            ed_vel_x(islg_sc(j,k)) = var1_g((j-1)*maxstack+k)
            ed_vel_y(islg_sc(j,k)) = var2_g((j-1)*maxstack+k)
         enddo ! all k sides on this processor
         !print*,j," ed_vel =",ed_vel_x(islg_sc(j,7)),ed_vel_y(islg_sc(j,7))," global edge/side number=",islg_sc(j,7)
      enddo ! all j processes
   endif ! proc. 0 only
   call mpi_barrier (mpi_komm_welt, ierr)
   !!!!!  zs depth discretisation of edges/sides !###   call writeout_nc(id_out_var(39),'zs',8,nvrt,nsa,zs)
   if ( .not. allocated(zs))allocate(zs(nvrt,nsa))
   if (meinrang == 0) var_g(:) = 777.777
   var_p(:) = 0.0
   call mpi_barrier (mpi_komm_welt, ierr)
   
   nc_error_prefix = ''
   write(nc_error_prefix, '(a," - Rank ",i," - zs")') trim(calling_routine), meinrang
   call check_err(trim(nc_error_prefix), nf_inq_varid(ncid,"zs", varid))
   call check_err(trim(nc_error_prefix), nf90_inquire_variable(ncid,varid,vname(varid),vxtype(varid),vndims(varid),dimids, nAtts))
   do i = 1,nvrt
      start3 = (/i, 1, nin /)
      count3 = (/1, nsa, 1 /) ! nodenumber second dimension
      call check_err(trim(nc_error_prefix), nf90_get_var(ncid, varid, var_p(1:nsa), start3, count3 ))
      zs(i,1:nsa) = var_p(1:nsa)
   enddo ! all i levels
   !print*,meinrang," zs(7)=",zs(1,7),zs(2,7)
   
   !!!!!  znl depth discretisation at nodes !###   call writeout_nc(id_out_var(40),'znl',8,nvrt,npa,znl)
   if ( .not. allocated(znl))allocate(znl(nvrt,npa))
   if (meinrang == 0) var_g(:) = 777.777
   var_p(:) = 0.0
   call mpi_barrier (mpi_komm_welt, ierr)
   
   nc_error_prefix = ''
   write(nc_error_prefix, '(a," - Rank ",i," - znl")') trim(calling_routine), meinrang
   call check_err( trim(nc_error_prefix), nf_inq_varid(ncid,"znl", varid) )
   call check_err( trim(nc_error_prefix), nf90_inquire_variable(ncid,varid,vname(varid),vxtype(varid),vndims(varid),dimids, nAtts) )
   do i = 1,nvrt
      start3 = (/i, 1, nin /)
      count3 = (/1, npa, 1 /) ! nodenumber second dimension
      call check_err( trim(nc_error_prefix), nf90_get_var(ncid, varid, var_p(1:npa), start3, count3 ) )
      znl(i,1:npa) = var_p(1:npa)
      !#############
   enddo ! all i levels
   !################
   call mpi_barrier (mpi_komm_welt, ierr)
   ! gather var_p into var_g
   call MPI_Gather(var_p, maxstack, MPI_FLOAT, var_g, maxstack, MPI_FLOAT, 0, mpi_komm_welt, ierr)
   if (ierr /= 0) then
      write(fehler,*)"get_schism_step MPI_Gather(var_p dahv1 failed : ", ierr
      call qerror(fehler)
   endif
   call mpi_barrier (mpi_komm_welt, ierr)
   !! recombine into global numbers
   if (meinrang == 0) then
      do j = 1,proz_anz ! all processes/ranks
         do k = 1,np_sc(j) ! all nodes at this rank
            ! ????u(iplg_sc(j,k))=var_g((j-1)*maxstack+k)
         enddo
      enddo
   endif ! proc. 0 only
   call mpi_barrier (mpi_komm_welt, ierr)
   !dingdong=##########
   !print*,meinrang," znl (topnode side 7",   isidenode(1,7),") =",znl(1,isidenode(1,7)),znl(2,isidenode(1,7))
   !print*,meinrang," znl (bottomnode side 7",isidenode(2,7),") =",znl(1,isidenode(2,7)),znl(2,isidenode(2,7))
   goto 7777
   !!!!! temp -> planktonic_variable_name(1)
   if (meinrang == 0) then
      do j = 1,proz_anz*maxstack
         var_g(j) = 777.777
      enddo ! all j
   endif ! proc. 0 only
   call mpi_barrier (mpi_komm_welt, ierr)
   
   iret = nf_inq_varid(ncid, "temp", varid)
   if (iret /= 0) then !no temp
      if (meinrang == 0) then ! message only once
         print*,"get_schism_step: nf_inq_varid(ncid, >  > temp <  < failed, iret = ",iret, " rank = ",meinrang
         print*,"initialize temp to zero "
      endif ! message only once
      do j = 1,maxstack
         var_p(j) = 0.0
      enddo ! all j
   else ! with temp
      nc_error_prefix = ''
      write(nc_error_prefix, '(a," - Rank ",i," - temp")') trim(calling_routine), meinrang
      call check_err( trim(nc_error_prefix), nf90_inquire_variable(ncid,varid,vname(varid),vxtype(varid),vndims(varid),dimids, nAtts) )
      if (dlength(dimids(2)) > maxstack) call qerror("temp:dlength(dimids(2)) > maxstack")
      !! initialize
      do j = 1,maxstack
         var_p(j) = 666.666
      enddo ! all j
      !! get data vel-x
      start3 = (/1, 1, nin /)
      count3 = (/1, npa, 1 /) ! nodenumber second dimension
      call check_err( trim(nc_error_prefix), nf90_get_var(ncid, varid, var_p(1:npa), start3, count3 ))
   endif
   call mpi_barrier (mpi_komm_welt, ierr)
   
   ! gather var_p into var_g
   call MPI_Gather(var_p, maxstack, MPI_FLOAT, var_g, maxstack, MPI_FLOAT, 0, mpi_komm_welt, ierr)
   if (ierr /= 0) then
      write(fehler,*)"get_schism_step MPI_Gather(var_p temp failed : ", ierr
      call qerror(fehler)
   endif
   call mpi_barrier (mpi_komm_welt, ierr)
   !! recombine into global numbers
   if (meinrang == 0) then
      do j = 1,proz_anz ! all processes/ranks
         do k = 1,np_sc(j) ! all nodes at this rank
            !planktonic_variable(1+(iplg_sc(j,k)-1)*number_plankt_vari)=var_g((j-1)*maxstack+k)
         enddo
      enddo
      minwert = 999999999.9
      maxwert = -999999999.9
      do j = 1,knotenanzahl2D
         tempi = planktonic_variable(1+(j-1)*number_plankt_vari)
         if (tempi > maxwert)maxwert = tempi
         if (tempi < minwert)minwert = tempi
      enddo ! all j
      print*," get_schism_step temp minwert, maxwert = ",minwert, maxwert
      !!### if(minwert.lt. -1.0)call qerror("This is no ice simulation")
      !!### if(maxwert.gt. 90.0)call qerror("This is no steam simulation")
   endif ! proc. 0 only
   call mpi_barrier (mpi_komm_welt, ierr)
   
   !!!!! salt -> planktonic_variable_name(72)
   if (meinrang == 0) then
      do j = 1,proz_anz*maxstack
         var_g(j) = 777.777
      enddo ! all j
   endif ! proc. 0 only
   call mpi_barrier (mpi_komm_welt, ierr)
   
   iret = nf_inq_varid(ncid, "salt", varid)
   if (iret /= 0) then ! no salt
      if (meinrang == 0) then ! message only once
         print*,"get_schism_step: nf_inq_varid(ncid, >  > salt <  < failed, iret = ",iret, " rank = ",meinrang
         print*,"initialize salt to zero "
      endif ! message only once
      do j = 1,maxstack
         var_p(j) = 0.0
      enddo ! all j
   else ! with salt
      nc_error_prefix = ''
      write(nc_error_prefix, '(a," - Rank ",i," - salt")') trim(calling_routine), meinrang
      call check_err( trim(nc_error_prefix), nf90_inquire_variable(ncid,varid,vname(varid),vxtype(varid),vndims(varid),dimids, nAtts) )
      if (dlength(dimids(2)) > maxstack) call qerror("salt:dlength(dimids(2)) > maxstack")
      !! initialize
      do j = 1,maxstack
         var_p(j) = 666.666
      enddo ! all j
      !! get data vel-x
      start3 = (/1, 1, nin /)
      count3 = (/1, npa, 1 /) ! nodenumber second dimension
      call check_err( trim(nc_error_prefix), nf90_get_var(ncid, varid, var_p(1:npa), start3, count3 ))
   endif ! with salt
   call mpi_barrier (mpi_komm_welt, ierr)
   ! gather var_p into var_g
   call MPI_Gather(var_p, maxstack, MPI_FLOAT, var_g, maxstack, MPI_FLOAT, 0, mpi_komm_welt, ierr)
   if (ierr /= 0) then
      write(fehler,*)"get_schism_step MPI_Gather(var_p salt failed : ", ierr
      call qerror(fehler)
   endif
   call mpi_barrier (mpi_komm_welt, ierr)
   !! recombine into global numbers
   if (meinrang == 0) then
      do j = 1,proz_anz ! all processes/ranks
         do k = 1,np_sc(j) ! all nodes at this rank
            !planktonic_variable(72+(iplg_sc(j,k)-1)*number_plankt_vari)=var_g((j-1)*maxstack+k)
         enddo
      enddo
   endif ! proc. 0 only
   call mpi_barrier (mpi_komm_welt, ierr)
   !!!!! AGE_1 -> planktonic_variable_name(74)= "       alter_arith"
   if (meinrang == 0) then
      do j = 1,proz_anz*maxstack
         var_g(j) = 777.777
      enddo ! all j
   endif ! proc. 0 only
   call mpi_barrier (mpi_komm_welt, ierr)
   
   iret = nf_inq_varid(ncid,"AGE_1", varid)
   if (iret /= 0) then ! no AGE_1
      if (meinrang == 0) then ! message only once
         print*,"get_schism_step: nf_inq_varid(ncid, >  > AGE_1 <  < failed, iret = ",iret, " rank = ",meinrang
         print*,"initialize age_1 to zero "
      endif ! message only once
      do j = 1,maxstack
         var_p(j) = 0.0
      enddo ! all j
   else ! with AGE_1
      nc_error_prefix = ''
      write(nc_error_prefix, '(a," - Rank ",i," - AGE_1")') trim(calling_routine), meinrang
      call check_err( trim(nc_error_prefix), nf90_inquire_variable(ncid,varid,vname(varid),vxtype(varid),vndims(varid),dimids, nAtts) )
      if (dlength(dimids(2)) > maxstack)call qerror("AGE_1:dlength(dimids(2)) > maxstack")
      !! initialize
      do j = 1,maxstack
         var_p(j) = 666.666
      enddo ! all j
      !! get data vel-x
      start3 = (/1, 1, nin /)
      count3 = (/1, npa, 1 /) ! nodenumber second dimension
      call check_err( trim(nc_error_prefix), nf90_get_var(ncid, varid, var_p(1:npa), start3, count3 ))
   endif ! with age
   call mpi_barrier (mpi_komm_welt, ierr)
   ! gather var_p into var_g
   call MPI_Gather(var_p, maxstack, MPI_FLOAT, var_g, maxstack, MPI_FLOAT, 0, mpi_komm_welt, ierr)
   if (ierr /= 0) then
      write(fehler,*)"get_schism_step MPI_Gather(var_p AGE_1 failed : ", ierr
      call qerror(fehler)
   endif
   call mpi_barrier (mpi_komm_welt, ierr)
   !! recombine into global numbers
   if (meinrang == 0) then
      do j = 1,proz_anz ! all processes/ranks
         do k = 1,np_sc(j) ! all nodes at this rank
            !planktonic_variable(74+(iplg_sc(j,k)-1)*number_plankt_vari)=var_g((j-1)*maxstack+k)
         enddo
      enddo
   endif ! proc. 0 only
   
   7777 continue
   call mpi_barrier (mpi_komm_welt, ierr)
   !! close, clean, return
   if (meinrang == 0) then
      deallocate(var_g,var1_g,var2_g,stat = istat)
      if (istat /= 0) call qerror("deallocate var_g( failed")
   endif
   deallocate(var_p,var1_p,var2_p,stat = istat)
   if (istat /= 0) call qerror("deallocate var_p( failed")
   deallocate (dlength,dname, stat = istat)
   deallocate (vxtype,vndims,vname,  stat = istat )
   call mpi_barrier (mpi_komm_welt, ierr)
   if (meinrang == 0) then
      do j = 1,number_plankt_point ! all j nodes
         rb_hydraul(1+(j-1)*number_rb_hydraul) = u(j)
         rb_hydraul(2+(j-1)*number_rb_hydraul) = p(j)-knoten_z(j) ! Tiefe
         rb_hydraul(3+(j-1)*number_rb_hydraul) = p(j)
         benthic_distribution(44+(j-1)*number_benth_distr) = 0.1      ! ks #### jetzt anders gelöst mit zone()%reib
         benthic_distribution(45+(j-1)*number_benth_distr) = 0.1*u(j) ! utau
      enddo ! all j nodes
      print*,"### stofftransport_schism: ks and utau, only first guess ###"
   endif ! proc. 0 only
   call mpi_barrier (mpi_komm_welt, ierr)
   if (meinrang == 0) then !! all boundary nodes inflow ####
      do j = 1,number_plankt_point ! all j nodes
         if (knoten_rand(j) > 0 ) inflow(j) = .true.
      enddo ! all j nodes
   endif ! proc. 0 only
   call mpi_barrier (mpi_komm_welt, ierr)
end subroutine get_schism_step
!----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
