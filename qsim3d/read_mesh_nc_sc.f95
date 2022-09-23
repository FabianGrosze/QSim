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
!> SUBROUTINE read_mesh_nc_sc()
!!    get SCHISM mesh and partitioning \n
!!    check for necessary variables  \n
!!    elev=iof_hydro(1) in param.nml \n
!!    (dahv)=iof_hydro(16) in param.nml \n
!!    hvel_side=iof_hydro(26) in param.nml \n
!! \n\n
!! aus Datei read_mesh_nc_sc.f95; zurück zu \ref lnk_transport_schism
subroutine read_mesh_nc_sc() !meinrang.eq.0
   use netcdf
   use modell
   use schism_glbl, only: su2, sv2, tr_el, eta2, npa, nsa, nea, nvrt, ns_global, &
                          ne_global, np_global, ielg, iplg, islg, RNDAY, dt,     &
                          rkind, xnd, ynd, dp00, kbp00, i34, elnode, isidenode,  &
                          snx, sny, distj,ntracers,ne,np,ns,kbe,isdel,delj,      &
                          start_year,start_month,start_day,start_hour,utc_start, &
                          nrec,nspool,kz,ics,xlon,ylat,area,elside,ic3,isbs,     &
                          ssign,kbs,iegl2
   !use schism_msgp, only: nproc, myrank
   implicit none
   include 'netcdf.inc'
   
   integer                           :: IPRE,IBC,IBTP,NTRACER_GEN,NTRACER_AGE,SED_CLASS,ECO_CLASS,IHFSKIP,MSC2,MDC2
   integer                           :: i,j,k,n,m,mm, neta_global,nr
   integer                           :: lfdb, istat, nproc
   character (len = longname)        :: dateiname,systemaufruf
   character(len = 72)               :: fgb,fgb2,fdb  ! Processor specific global output file name
   character(len = 400)              :: textline
   integer                           :: ne_l, np_l, ns_l ! numbers on each process
   integer                           :: irank
   integer                           :: nmax, nmin, tn, bn
   real                              :: dtout, h0, h_s, h_c, theta_b, theta_f
   real,allocatable,dimension (:)    :: ztot, sigma
   integer,allocatable,dimension(:,:):: nm2
   integer,allocatable,dimension(:)  :: kbp00_global , i34_global
   real                              :: xmax, xmin, ymax, ymin, zmax, zmin, xmaxi, xmini, ymaxi, ymini, zmaxi, zmini
   real                              :: rzone, distmax, distmin, sum_area, totalarea
   real dummy
   integer nummy, rank_min,rank_max,id_min,id_max
   
   namelist /CORE/   IPRE,IBC,IBTP,NTRACER_GEN,NTRACER_AGE,SED_CLASS,ECO_CLASS,NSPOOL,IHFSKIP,MSC2,MDC2,DT,RNDAY
   
      !print*,meinrang,' read_mesh_nc_sc starts ',proz_anz
      
!#### read param.nml #############################################################################!
      if (meinrang == 0) then !! nur prozessor 0
         ! read param.out.nml
         write(dateiname,"(2A,I4.4,3A)")trim(modellverzeichnis),"outputs_schism/param.out.nml"
         open(15,file = dateiname,delim = 'apostrophe',status = 'old')
         read(15,nml = CORE)
         deltat = int(dt)
         print*,"read_mesh_nc_sc: param.out.nml DT,RNDAY = ",DT,deltat,RNDAY
         close(15)
      endif ! process0 only
      call mpi_barrier (mpi_komm_welt, ierr)

!#### read local_to_global_0000 files ############################################################!
      write(dateiname,"(2A)")trim(modellverzeichnis),"outputs_schism/local_to_global_000000"
      i=len_trim(dateiname)
      write(dateiname(i-5:i),'(i6.6)') meinrang
      open(10,file=trim(dateiname),status='old')
!     header info 
      read(10,*)ns_global,ne_global,np_global,nvrt,nproc,ntracers ! ,ntrs(:) ??? !global info
      read(10,'(A)')textline ! ; print*,meinrang,trim(textline)  !write(10,*)'Header:'
      n_elemente     = ne_global
      knotenanzahl2D = np_global
      kantenanzahl   = ns_global
      if (meinrang == 0)print*,"read_mesh_nc_sc: local_to_global ns_global,ne_global,np_global= "  &
                             ,ns_global,ne_global,np_global
      if(proz_anz .ne. nproc )then
         print*,trim(dateiname)
         print*,meinrang,' proz_anz .ne. nproc ',proz_anz,nproc
         call qerror('incompatible process number SCHISM - QSim')
      endif
      read(10,*)ne,nea
      if(allocated(ielg)) deallocate(ielg); allocate(ielg(nea));
      do i=1,ne
         read(10,*)j,ielg(j)
      enddo
      read(10,*)np,npa
      print*,meinrang,'local_to_global:',np,npa
      if(allocated(iplg)) deallocate(iplg); allocate(iplg(npa));
      do i=1,np
         read(10,*)j,iplg(j)
      enddo
      read(10,*)ns,nsa
      if(allocated(islg)) deallocate(islg); allocate(islg(nsa));
      do i=1,ns
         read(10,*)j,islg(j)
      enddo
      read(10,'(A)')textline ! ; print*,meinrang,trim(textline)  !write(10,*)'Header:'
      read(10,*)start_year,start_month,start_day,start_hour,utc_start
      read(10,*)nrec,dummy,nspool,nvrt,kz, &
     &         h0,h_s,h_c,theta_b,theta_f,ics
      if(allocated(ztot)) deallocate(ztot); allocate(ztot(nvrt))
      if(allocated(sigma)) deallocate(sigma); allocate(sigma(nvrt))
      do k=1,kz-1
         read(10,*)ztot(k)
      enddo !k
      do k=1,nvrt-kz+1
         read(10,*)sigma(k)
      enddo !k
      read(10,*)i,j
      if((i.ne.np).or.(j.ne.ne))call qerror('(i.ne.np).or.(j.ne.ne)')
      if(ics==1) then
         allocate(xnd(npa),ynd(npa),dp00(npa),kbp00(npa))
         xmin=1.0e9 ; xmax=-1.0*xmin
         ymin=1.0e9 ; ymax=-1.0*ymin
         do m=1,np
            read(10,*)xnd(m),ynd(m),dp00(m),kbp00(m)
            if(xmin.gt.xnd(m))xmin=xnd(m);if(xmax.lt.xnd(m))xmax=xnd(m)
            if(ymin.gt.ynd(m))ymin=ynd(m);if(ymax.lt.ynd(m))ymax=ynd(m)
         enddo !m
      else !lat/lon
         allocate(xlon(npa),ylat(npa),dp00(npa),kbp00(npa))
         do m=1,np
            read(10,*)xlon(m),ylat(m),dp00(m),kbp00(m)
            xlon(m)=xlon(m)/180.d0*pi ; ylat(m)=ylat(m)/180.d0*pi
         enddo !m
      endif !ics
      allocate(i34(nea),elnode(4,nea))
      allocate(kbe(nea),area(nea),ic3(4,nea),elside(4,nea),ssign(4,nea))
      sum_area=0.0
      do m=1,ne
         read(10,*)i34(m),(elnode(mm,m),mm=1,i34(m))
         read(10,*)kbe(m),area(m),ic3(1,m),ic3(2,m),ic3(3,m),ic3(4,m)
         sum_area=sum_area+area(m)
         read(10,*)elside(1,m),elside(2,m),elside(3,m),elside(4,m),ssign(1,m),ssign(2,m),ssign(3,m),ssign(4,m)
      enddo !m
      sum_area=sum_area/1000000 ! km²
      allocate(isidenode(2,nsa),isdel(2,nsa))
      allocate(distj(nsa),delj(nsa),snx(nsa),sny(nsa),isbs(nsa),kbs(nsa))
      do i=1,ns
         read(10,*)j,isidenode(1:2,j)
         read(10,*)distj(i),delj(i),snx(i),sny(i),isbs(i),kbs(i),isdel(1,i),isdel(2,i)
      enddo !i
      close(10)
      print*,meinrang,' area [km²]=',sum_area,' bottom left=',xmin,ymin,' top right=',xmax,ymax,' nodes,elements=',np,ne
      call mpi_reduce(xmax,xmaxi,1,MPI_DOUBLE_PRECISION,mpi_max,0,mpi_komm_welt,ierr)
      call mpi_reduce(ymax,ymaxi,1,MPI_DOUBLE_PRECISION,mpi_max,0,mpi_komm_welt,ierr)
      call mpi_reduce(xmin,xmini,1,MPI_DOUBLE_PRECISION,mpi_min,0,mpi_komm_welt,ierr)
      call mpi_reduce(ymin,ymini,1,MPI_DOUBLE_PRECISION,mpi_min,0,mpi_komm_welt,ierr)
      call mpi_reduce(sum_area,totalarea,1,MPI_FLOAT,mpi_sum,0,mpi_komm_welt,ierr)
      call mpi_barrier (mpi_komm_welt, ierr)
      if (meinrang==0)print*,'area[km²]=',totalarea,' bottom left=',xmini,ymini,' top right=',xmaxi,ymaxi

 !#### read global_to_local.prop #################################################################!
      if(allocated(iegl2)) deallocate(iegl2); allocate(iegl2(2,ne_global))
      allocate(ipgl_rank(np_global)) ; allocate(ipgl_id(np_global))
      call mpi_barrier (mpi_komm_welt, ierr)
      if(meinrang==0) then
         write(dateiname,"(2A)")trim(modellverzeichnis),"outputs_schism/global_to_local.prop"
         open(32,file=trim(dateiname),status='unknown')
         do i=1,ne_global
            read(32,*)j,nummy,iegl2(1,i),iegl2(2,i)
            !read(32,'(i8,1x,i6,1x,i6,1x,i6)')j,nummy,iegl2(1,i),iegl2(2,i)
            !write(32,'(i8,1x,i6,1x,i6,1x,i6)')ie,iegrpv(ie),iegl2(1,ie),iegl2(2,ie)
         enddo !i
         do i=1,np_global
            read(32,*)j, ipgl_rank(i), ipgl_id(i)
            !read(32,'(i8,1x,i6,1x,i6)')j, ipgl_rank(i), ipgl_id(i)
         enddo !i
         close(32)
      endif ! meinrang==0
      call mpi_barrier (mpi_komm_welt, ierr)
      call MPI_Bcast(ipgl_rank,np_global,MPI_INT,0,mpi_komm_welt,ierr)
      call MPI_Bcast(ipgl_id,np_global,MPI_INT,0,mpi_komm_welt,ierr)
      rank_min=777777 ; rank_max=-777777 ; id_min=777777 ; id_max=-777777 ;j=0
      do i=1,np_global
         if(rank_min.gt.ipgl_rank(i))rank_min=ipgl_rank(i)
         if(rank_max.lt.ipgl_rank(i))rank_max=ipgl_rank(i)
         if(ipgl_rank(i).eq.meinrang)then
            j=j+1
            if(id_min.gt.ipgl_id(i))id_min=ipgl_id(i)
            if(id_max.lt.ipgl_id(i))id_max=ipgl_id(i)
         endif
      enddo !i
      !print*,meinrang,' global_to_local.prop ranks=',rank_min,rank_max,' id=',j,id_min,id_max,np,npa
      call mpi_reduce(j,k,1,MPI_INT,mpi_sum,0,mpi_komm_welt,ierr)
      call mpi_reduce(np,m,1,MPI_INT,mpi_sum,0,mpi_komm_welt,ierr)
      call mpi_reduce(npa,n,1,MPI_INT,mpi_sum,0,mpi_komm_welt,ierr)
      if(meinrang==0)print*,'mpi_sum=',k,m,n,'np_global',np_global

      j=0
      do i=1,np
         if(ipgl_id(iplg(i)).ne.i) j=j+1
      enddo
      !print*,meinrang,' global-local node-number missmatch ',j,' of ',np

!#### read manning.gr3 ###########################################################################!
      
      if (meinrang == 0) then !! nur prozessor 0
         write(dateiname,"(2A)")trim(modellverzeichnis),"outputs_schism/manning.gr3"
         open(14,file = trim(dateiname),status = 'old',iostat = istat)
         !open(14,file = 'zone.gr3',status = 'old',iostat = istat)
         if (istat /= 0) call qerror('read_mesh_nc_sc: manning.gr3 open failure')
         read(14,*); read(14,*) n,k
         if(n.ne.n_elemente)call qerror('manning.gr3 element number missmatch')
         if(k.ne.knotenanzahl2D)call qerror('manning.gr3 node number missmatch')
         print*,'read_mesh_nc_sc: manning.gr3: n_elemente,knotenanzahl2D = ',n,k
         allocate (knoten_x(knotenanzahl2D),knoten_y(knotenanzahl2D),knoten_z(knotenanzahl2D),   &
               knoten_zone(knotenanzahl2D),knoten_rang(knotenanzahl2D),    &
               knoten_rand(knotenanzahl2D),knoten_flaeche(knotenanzahl2D), stat = istat )
         do i = 1,knotenanzahl2D
            read(14,*)n,knoten_x(i),knoten_y(i),knoten_z(i)
            if (n /= i) call qerror('reading zone.gr3 nodes: something gone wrong')
            !if ((knoten_zone(i) < 0) .or. (knoten_zone(i) > 300)) then
            !   write(fehler,*)' knoten #',i,': Zonennummer darf nicht negativ oder größer als 300 sein; ist aber = ',knoten_zone(i)
            !   call qerror(fehler)
            !end if
         enddo
      
         xmax = -999999999999.9 ; xmin = 999999999999.9
         ymax = -999999999999.9 ; ymin = 999999999999.9
         zmax = -99999999       ; zmin = 99999999
         do i = 1,np
            if (xmax <= knoten_x(i))xmax = knoten_x(i)
            if (xmin >= knoten_x(i))xmin = knoten_x(i)
            if (ymax <= knoten_y(i))ymax = knoten_y(i)
            if (ymin >= knoten_y(i))ymin = knoten_y(i)
            if (zmax <= knoten_z(i))zmax = knoten_z(i)
            if (zmin >= knoten_z(i))zmin = knoten_z(i)
         end do ! alle i Knoten
         print*,'manning.gr3 bottom left=',xmin,ymin,' top right=',xmax,ymax,' Mannings n ',zmax, zmin

      end if !! prozess 0 only
      
      call mpi_barrier (mpi_komm_welt, ierr)
      return !! preliminary end

      
      if (meinrang == 0) then !! nur prozessor 0
         !Read local_to_global_0000 for global info
      write(dateiname,'(4A)')trim(modellverzeichnis),'outputs_schism','/','local_to_global_0000'
      open(10, file = dateiname, status = 'old', iostat = istat)
      if (istat /= 0) then
         write(fehler,*)'read_mesh_nc_sc local_to_global_0000 failed, rank = ',meinrang
         call qerror(trim(fehler))
      end if
      read(10,*)ns_global,ne_global,np_global,nvrt,nproc !,ntracers
      print*,'read_mesh_nc_sc: local_to_global_0000 ',trim(adjustl(dateiname)), &
             ' points, elements, sides levels, processes'                       &
            ,np_global,ne_global,ns_global,nvrt,nproc
      rewind(10)
      close(10)
      if ((n_elemente /= ne_global) .or. (knotenanzahl2D /= np_global)) then
         write(fehler,*)'element + nodenumber dissence zone.gr3:',n_elemente,knotenanzahl2D ; call qerror(fehler)
      else
         knotenanzahl2D = np_global
         n_elemente = ne_global
         kantenanzahl = ns_global
      endif
      
      if (nproc /= proz_anz) then
         write(fehler,*)'read_mesh_nc_sc: given number of processes = ', proz_anz, ', needed number of processes = ', nproc
         call qerror(fehler)
      else
         print*,'read_mesh_nc_sc: equal number of processes QSim+SCHISM = ', proz_anz,nproc
      endif !! wrong process number
      knotenanzahl3D = nvrt*np_global
      !allocate( np(0:proz_anz-1),ns(0:proz_anz-1),ne(0:proz_anz-1),stat=istat)
      !if(istat/=0) call qerror('Allocation error np')
      !do i=0,proz_anz-1
      !   ! Read in local-global mappings from all ranks
      !   lfdb=len_trim(dateiname)
      !   !Find max. for dimensioning
      !   write(dateiname(lfdb-3:lfdb),'(i4.4)') i
      !   open(10,file=dateiname,status='old')
      !   if(.not.zeile(10))call qerror('get_local_to_global erste zeile')
      !   if(.not.zeile(10))call qerror('get_local_to_global zweite zeile')
      !   read(10,*)ne(i)
      !   !print*,'get_local_to_global: ne(', i, ')=',ne(i)
      !   close(10)
      !enddo ! all i processes
      !print*,'element number=',n_elemente,sum(ne)
      !print*,'read_mesh_nc_sc: using hydraulic event: ',trim(adjustl(modellverzeichnis))//'outputs_schism'
      !systemaufruf='ls -thora '//trim(adjustl(modellverzeichnis))//'outputs_schism'
      !call system(trim(systemaufruf),istat)
   end if !! prozess 0 only
   call mpi_barrier (mpi_komm_welt, ierr)
   !if(meinrang.ne.0)allocate( np(0:proz_anz-1),ns(0:proz_anz-1),ne(0:proz_anz-1),stat=istat)
   if (rkind /= 8)call qerror('read_mesh_nc_sc: rkind /= 8') ! Default real datatype from schism_glbl needs to be 8=DOUBLE_PRECISION
   call MPI_Bcast(dt,1,MPI_DOUBLE_PRECISION,0,mpi_komm_welt,ierr)
   ! print* ,meinrang,"MPI_Bcast(dt",ierr
   call MPI_Bcast(RNDAY,1,MPI_DOUBLE_PRECISION,0,mpi_komm_welt,ierr)
   ! MPI_FLOAT, mpi_real, MPI_DOUBLE, MPI_LONG_DOUBLE,MPI_REAL2, MPI_REAL4 and MPI_REAL8
   call MPI_Bcast(deltat,1,MPI_INT,0,mpi_komm_welt,ierr)
   ! print* ,meinrang,"MPI_Bcast(deltat",ierr
   call MPI_Bcast(knotenanzahl3D,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(knotenanzahl2D,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(n_elemente,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(kantenanzahl,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(ns_global,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(ne_global,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(np_global,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(nproc,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(nvrt,1,MPI_INT,0,mpi_komm_welt,ierr)
   call mpi_barrier (mpi_komm_welt, ierr)
   
      return !! preliminary end
   
   ! print*,meinrang,"read_mesh_nc_sc: dt,deltat,RNDAY=",dt,deltat,RNDAY
   !!!! read first part: local_to_global_meinrang !!!!
   !print*,meinrang,'-',adjustl(trim(modellverzeichnis)),'outputs_schism','/','local_to_global_0000'
   if (longname < len(adjustl(trim(modellverzeichnis)))+36)call qerror('read_mesh_nc_sc: longname too short')
   write(dateiname,'(4A)')adjustl(trim(modellverzeichnis)),'outputs_schism','/','local_to_global_0000'
   lfdb = len_trim(dateiname)
   write(dateiname(lfdb-3:lfdb),'(i4.4)') meinrang
   open(10+meinrang, file = trim(dateiname), status = 'old', iostat = istat)
   !print*,"read_mesh_nc_sc read:  dateiname ",trim(dateiname)
   if (istat /= 0) then
      write(fehler,*)'read_mesh_nc_sc open local_to_global_* failed, rank = ',meinrang
      call qerror(trim(fehler))
   end if
   if (.not.zeile(10+meinrang)) call qerror('get_local_to_global erste zeile')
   if (.not.zeile(10+meinrang)) call qerror('get_local_to_global zweite zeile')
   read(10+meinrang,*)nea
   !call mpi_barrier (mpi_komm_welt, ierr)
   allocate( ielg(nea))
   allocate(i34(nea),elnode(4,nea))
   do i = 1,nea
      read(10+meinrang,*)j,ielg(i)
   enddo !i
   !print*, trim(adjustl(dateiname)),' local number elements=',nea,j,ielg(j)
   read(10+meinrang,*)npa
   allocate(eta2(npa))
   allocate(xnd(npa),ynd(npa),dp00(npa),kbp00(npa))
   !call mpi_barrier (mpi_komm_welt, ierr)
   allocate( iplg(npa) )
   do i = 1,npa
      read(10+meinrang,*)j,iplg(i)
   enddo !i
   ! print*, trim(adjustl(dateiname)),' local number points=',npa,j,iplg(j)
   
   read(10+meinrang,*)nsa
   allocate( su2(nvrt,nsa),sv2(nvrt,nsa) )
   allocate( isidenode(2,nsa), snx(nsa), sny(nsa), distj(nsa) )
   !call mpi_barrier (mpi_komm_welt, ierr)
   allocate( islg(nsa) )
   do i = 1,nsa
      read(10+meinrang,*, iostat = istat)j,islg(i)
      if (istat /= 0) then
         write(fehler,*)'read error get_local_to_global islg rank = ',meinrang
         call qerror('fehler')
      end if
   enddo
   
   !!!! read second part:  !!!!
   !write(10,*)'Header:'
   !write(10,*)start_year,start_month,start_day,start_hour,utc_start
   !write(10,*)nrec,real(dt*nspool),nspool,nvrt,kz,real(h0),real(h_s),real(h_c),real(theta_b),real(theta_f),ics
   !write(10,*)(real(ztot(k)),k=1,kz-1),(real(sigma(k)),k=1,nvrt-kz+1)
   read(10+meinrang,*)textline! ; print*,textline
   read(10+meinrang,*)textline! ; print*,textline
   read(10+meinrang,*)textline! ; print*,textline
   read(10+meinrang,*)textline! ; print*,textline
   read(10+meinrang,*)np,ne,ns
   if ((np /= npa) .or. (ne /= nea) .or. (ns /= nsa)) then
      call qerror('local_to_global np /= npa')
   else
      !print*,meinrang,"read_mesh_nc_sc: npa,nea,nsa=",npa,nea,nsa
   endif !numbers wrong
   do i = 1,np
      read(10+meinrang,*)xnd(i),ynd(i),dp00(i),kbp00(i)
   enddo !all nodes i
   do m = 1,ne
      read(10+meinrang,*)i34(m),(elnode(mm,m),mm = 1,i34(m))
   enddo !m
   do m = 1,ns
      read(10+meinrang,*)(isidenode(mm,m),mm = 1,2),snx(m),sny(m),distj(m)
   enddo !m
   rewind(10+meinrang)
   close(10+meinrang)
   !print*,meinrang,"read_mesh_nc_sc: isidenode(nsa)= ",isidenode(1,nsa),isidenode(2,nsa)
   call mpi_barrier (mpi_komm_welt, ierr)
   if (meinrang == 0) then !! nur prozessor 0
      allocate(ne_sc(proz_anz), np_sc(proz_anz), ns_sc(proz_anz),stat = istat)
      if (istat /= 0) call qerror('Allocation error: np')
      allocate(ztot(nvrt),sigma(nvrt) ,stat = istat)
      if (istat /= 0) call qerror('Allocation error: ztot')
      allocate( kbp00_global(knotenanzahl2D) ,stat = istat)
      if (istat /= 0) call qerror('Allocation error: kbp00_global')
      print*,"read_mesh_nc_sc: allocate 0 1"
      !         if(istat/=0) stop 'Allocation error: kbe'
      allocate( ielg_sc(proz_anz,n_elemente) ) !! too large, who cares?
      allocate( iplg_sc(proz_anz,knotenanzahl2D) )
      allocate( islg_sc(proz_anz,kantenanzahl) )
      allocate( i34_global(n_elemente), nm2(4,n_elemente), stat = istat)
      allocate( top_node(kantenanzahl), bottom_node(kantenanzahl), stat = istat)
      allocate( edge_normal_x(kantenanzahl), edge_normal_y(kantenanzahl), &
      cell_bound_length(kantenanzahl), stat = istat)
      print*,"read_mesh_nc_sc: allocate 0 2"
      top_node(:) = -1; bottom_node(:) = -1
      edge_normal_x(:) = -77.7; edge_normal_y(:) = -77.7; cell_bound_length(:) = -77.7
      
      if (istat /= 0) call qerror('Allocation error: i34_global')
      allocate( ed_flux(kantenanzahl), ed_area(kantenanzahl), stat = istat )
      if (istat /= 0) call qerror('Allocation error: ed_flux')
      allocate( ed_vel_x(kantenanzahl), ed_vel_y(kantenanzahl), stat = istat )
      if (istat /= 0) call qerror('Allocation error: ed_vel_x')
      ! init
      ed_flux(:) = -1; ed_area(:) = -1; ed_vel_x(:) = -1; ed_vel_y(:) = -1
      print*,"read_mesh_nc_sc: allocate 0 3"
      allocate (elementnodes(n_elemente,4), stat = istat)
      if (istat /= 0) call qerror('allocate elementnodes failed')
      allocate (cornernumber(n_elemente), stat = istat )
      if (istat /= 0) call qerror('Allocation error: cornernumber')
      allocate (element_zone(n_elemente), stat = istat )
      if (istat /= 0) call qerror('allocate element_zone failed')
      allocate (element_rand(n_elemente), stat = istat )
      if (istat /= 0) call qerror('allocate element_zone failed')
      do n = 1,n_elemente !initialize zones and boundaries
         element_zone(n) = 0
         element_rand(n) = 0
      end do ! alle Elemente!
      print*,"read_mesh_nc_sc: allocate 0 4"
      ! reread on process 0
      write(dateiname,'(4A)')trim(modellverzeichnis),'outputs_schism','/','local_to_global_0000'
      print*,"read_mesh_nc_sc reread:  dateiname ",trim(dateiname)
      lfdb = len_trim(dateiname)
      maxstack = 0
      do irank = 1,proz_anz,1 ! all ranks
         write(dateiname(lfdb-3:lfdb),'(i4.4)') irank-1
         open(10, file = dateiname, status = 'old', iostat = istat)
         !print*,"do irank dateiname=",trim(adjustl(dateiname))
         if (istat /= 0) then
            call qerror('open 10 failed')
            !else
            !print*,'success open 10 ',trim(adjustl(dateiname))
         endif ! open failed
         if ( .not. zeile(10))call qerror('Lesefehler 1')
         if ( .not. zeile(10))call qerror('Lesefehler 2')
         read(10,*)ne_l
         do k = 1,ne_l
            read(10,*)j,ielg_sc(irank,k)
         enddo !k
         read(10,*)np_l
         !print*,'irank,np_l=',irank,np_l,trim(adjustl(dateiname))
         do k = 1,np_l
            read(10,*)j,iplg_sc(irank,k)
            knoten_rang(iplg_sc(irank,k)) = irank
         enddo !k
         read(10,*)ns_l
         do k = 1,ns_l
            read(10,*)j,islg_sc(irank,k)
         enddo !k
         ! print*,irank,"read_mesh_nc_sc: np_l,ne_l,ns_l=",np_l,ne_l,ns_l
         ! take care that maxstack is large enough for everything
         if (maxstack < ne_l)maxstack = ne_l
         if (maxstack < np_l)maxstack = np_l
         if (maxstack < ns_l)maxstack = ns_l
         if ( .not. zeile(10))call qerror('get_local_to_global Header missing')!read(10+meinrang,*) !'Header:'
         read(10,*)start_year,start_month,start_day,start_hour,utc_start
         if (irank == 1)print*,'Start Date = ',start_year,start_month,start_day,start_hour,utc_start
         jahr = start_year
         monat = start_month
         tag = start_day
         stunde = int(start_hour)
         minute = int( start_hour*60.0 - int(start_hour)*int(60) )
         sekunde = int( start_hour*3600.0 - int(start_hour)*int(3600) )
         ! version='v10'
         read(10,*)nrec,dtout,nspool,nvrt,kz,h0,h_s,h_c,theta_b,theta_f,ics
         !print*,'read_mesh_nc_sc: nrec,dtout,nspool=',nrec,dtout,nspool
         read(10,*)(ztot(k),k = 1,kz-1),(sigma(k),k = 1,nvrt-kz+1)
         !print*,'read_mesh_nc_sc: ztot(k),kz,nvrt=',ztot(1),kz,nvrt
         read(10,*)np_sc(irank),ne_sc(irank),ns_sc(irank)
         !print*,irank,'read_mesh_nc_sc: np_sc,ne_sc,ns_sc=',np_sc(irank),ne_sc(irank),ns_sc(irank)
         if (np_sc(irank) /= np_l) call qerror('np_sc(irank) /= np_l')
         read(10,*, iostat = istat) ( knoten_x(iplg_sc(irank,m)), knoten_y(iplg_sc(irank,m)), knoten_z(iplg_sc(irank,m)),  &
              kbp00_global(iplg_sc(irank,m)),m = 1,np_sc(irank) )
         if (istat /= 0)call qerror('read(10,*) ( knoten_x... failed')
         read(10,*, iostat = istat) ( i34_global(ielg_sc(irank,m)), &
              (nm2(mm,m),mm = 1,i34_global(ielg_sc(irank,m))), m = 1,ne_sc(irank) )
         if (istat /= 0)call qerror('read(10,*)( i34_global(ielg(irank,m))... failed')
         do m = 1,ns_sc(irank)
            read(10,*,iostat = istat)tn,bn,  &
                 edge_normal_x(islg_sc(irank,m)),edge_normal_y(islg_sc(irank,m)),cell_bound_length(islg_sc(irank,m))
            !snx_l(m),sny_l(m),distj_l(m)
            if (istat /= 0)call qerror('read(10,*) local_to_global (top +bottom node)... failed')
            if ( ((tn <= 0) .or. (bn <= 0)) .or. ((tn > np_sc(irank)) .or. (bn > np_sc(irank))) ) then
               print*, irank, tn, bn, np_sc(irank)
               call qerror('isidenode not o.k.')
            endif ! node numbers ok?
            if ((tn <= 0) .or. (tn > np_sc(irank)))print*,irank,tn,np_sc(irank)," tn wrong"
            if ((bn <= 0) .or. (bn > np_sc(irank)))print*,irank,bn,np_sc(irank)," bn wrong"
            top_node(   islg_sc(irank,m)) = iplg_sc(irank,tn)
            bottom_node(islg_sc(irank,m)) = iplg_sc(irank,bn)
            !if(m.eq.3)then
            !   print*,irank," edge 3: ",islg_sc(irank,m),tn,bn,top_node(islg_sc(irank,m)),bottom_node(islg_sc(irank,m))
            !endif ! example edge
            !!! vnor1=su2(k,j)*snx(j)+sv2(k,j)*sny(j)
            !!! vnor2=su2(k-1,j)*snx(j)+sv2(k-1,j)*sny(j)
            !!! flux_adv_hface(k,j)=(zs(k,j)-zs(k-1,j))*distj(j)*(vnor1+vnor2)/2 !normal * area = flux (in local x-direction)
         end do !all m, 1 to nc_sc
         close(10)
         
         ! Reconstruct connectivity table
         !print*,irank,"read_mesh_nc_sc: ne_sc=",ne_sc(irank)
         do m = 1,ne_sc(irank)  ! Elements
            j = ielg_sc(irank,m) ! global element number
            if (j > n_elemente) call qerror('element number error in read_mesh_nc_sc')
            cornernumber(j) = i34_global(j)
            if ((cornernumber(j) < 3) .or. (cornernumber(j) > 4)) call qerror('cornernumber neither 3 nor 4 in read_mesh_nc_sc')
            do mm = 1,cornernumber(j)
               i = nm2(mm,m) ! local node number
               if (i > np_sc(irank) .or. i <= 0) then
                  print*,',nm2(1....4,',m,')) = ',nm2(1,m),nm2(2,m),nm2(3,m),nm2(4,m)
                  print*,',i,np(irank),irank,nm2(mm,m),mm,m,j,cornernumber(j) = ',  &
                         i,np_sc(irank),irank,nm2(mm,m),mm,m,j,cornernumber(j)
                  call qerror('cornernumber error in read_mesh_nc_sc')
               endif
               elementnodes(j,mm) = iplg_sc(irank,i)
            enddo !mm
         enddo !m   ! Elements
      enddo ! all irank
      deallocate( nm2 )
      do n = 1,kantenanzahl !! check integrity of side-node connectivity
         if ((top_node(n) <= 0) .or. (top_node(n) > knotenanzahl2D))print*,"top_node(",n,")wrong",top_node(n)
         if ((bottom_node(n) <= 0) .or. (bottom_node(n) > knotenanzahl2D))print*,"bottom_node(",n,")wrong",bottom_node(n)
      end do ! all n sides=edges
      summ_ne = 0 ! needed by vtk output
      do n = 1,n_elemente
         summ_ne = summ_ne+cornernumber(n)+1
      end do ! all Elements
      
      distmax = -999999999999.9
      distmin = 999999999999.9
      do n = 1,kantenanzahl ! all edges
         if (distmax <= cell_bound_length(n))distmax = cell_bound_length(n)
         if (distmin >= cell_bound_length(n))distmin = cell_bound_length(n)
      end do ! all n edges/sides
      element_vorhanden = .true.
      xmax = -999999999999.9
      xmin = 999999999999.9
      ymax = -999999999999.9
      ymin = 999999999999.9
      zmax = -999999999999.9
      zmin = 999999999999.9
      do n = 1,knotenanzahl2D
         if (xmax <= knoten_x(n))xmax = knoten_x(n)
         if (xmin >= knoten_x(n))xmin = knoten_x(n)
         if (ymax <= knoten_y(n))ymax = knoten_y(n)
         if (ymin >= knoten_y(n))ymin = knoten_y(n)
         knoten_z(n) = knoten_z(n)*(-1.0) ! bathymety elevation upwards
         if (zmax <= knoten_z(n))zmax = knoten_z(n)
         if (zmin >= knoten_z(n))zmin = knoten_z(n)
      end do ! alle Knoten
      print*,'local_to_global_0000:'
      print*,'x-koordinate max+min', xmax, xmin
      print*,'y-koordinate max+min', ymax, ymin
      print*,'Sohlhöhe max+min', zmax, zmin
      print*,'maxstack = ',maxstack
      print*,'edge length distmax+distmin', distmax, distmin
   end if !! prozess 0 only
   call mpi_barrier (mpi_komm_welt, ierr)
   call MPI_Bcast(maxstack,1,MPI_INT,0,mpi_komm_welt,ierr)
   nst_prev = -333 ! initialize
   ncid = -333
   call mpi_barrier (mpi_komm_welt, ierr)
   ! get boundary numbers from zone.gr3
   if (meinrang == 0) then !! prozess 0 only
      do j = 1,knotenanzahl2D ! initialize all j nodes with  boundary number 0 = inside node
         knoten_rand(j) = 0
      end do ! alle j node
      rewind(14)
      read(14,*); read(14,*) !header+numbers
      do i = 1,knotenanzahl2D
         read(14,*)
      enddo;
      
      do i = 1,n_elemente
         read(14,*)
      enddo
      
      read(14,*) ianz_rb
      read(14,*) neta_global
      ! print*,'read_mesh_nc_sc: open+closed boundaries',ianz_rb,neta_global
      do j = 1,ianz_rb ! all j open boundaries
         read(14,*) nr
         do i = 1,nr
            read(14,*) n
            knoten_rand(n) = j
         enddo
         print*,'read_mesh_nc_sc: zone.gr3 open boundary ',j, ' has ',nr,' nodes'
      end do ! all j open boundaries
      close(14)
   end if !! prozess 0 only
   
   call mpi_barrier (mpi_komm_welt, ierr)
   if (meinrang == 0) print*,'read_mesh_nc_sc finished'
   return
end subroutine read_mesh_nc_sc
      
