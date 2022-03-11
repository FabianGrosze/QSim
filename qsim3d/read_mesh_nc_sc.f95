!---------------------------------------------------------------------------------------
!
!   QSim - Programm zur Simulation der Wasserqualität
!
!   Copyright (C) 2020 Bundesanstalt für Gewässerkunde, Koblenz, Deutschland, http://www.bafg.de
!
!   Dieses Programm ist freie Software. Sie können es unter den Bedingungen der 
!   GNU General Public License, Version 3,
!   wie von der Free Software Foundation veröffentlicht, weitergeben und/oder modifizieren. 
!   Die Veröffentlichung dieses Programms erfolgt in der Hoffnung, daß es Ihnen von Nutzen sein wird, 
!   aber OHNE IRGENDEINE GARANTIE, sogar ohne die implizite Garantie der MARKTREIFE oder der VERWENDBARKEIT FÜR EINEN BESTIMMTEN ZWECK. 
!   Details finden Sie in der GNU General Public License.
!   Sie sollten ein Exemplar der GNU General Public License zusammen mit diesem Programm erhalten haben. 
!   Falls nicht, siehe http://www.gnu.org/licenses/.  
!   
!	Programmiert von:
!	1979 bis 2018 Volker Kirchesch
!	seit 2011 Jens Wyrwa, Wyrwa@bafg.de
!
!---------------------------------------------------------------------------------------

!> SUBROUTINE read_mesh_nc_sc() 
!!    get SCHISM mesh and partitioning \n
!!    check for necessary variables  \n
!!    elev=iof_hydro(1) in param.nml \n
!!    (dahv)=iof_hydro(16) in param.nml \n
!!    hvel_side=iof_hydro(26) in param.nml \n
!! \n\n
!! aus Datei read_mesh_nc_sc.f95; zurück zu \ref Transport_SCHISM
      SUBROUTINE read_mesh_nc_sc() !meinrang.eq.0
      use netcdf
      use modell
      use schism_glbl
!	  , only:su2,sv2,tr_el,eta2  &
!	 &                     ,npa, nsa, nea, nvrt, ns_global,ne_global,np_global  &
!	 &                     ,ielg,iplg,islg,rnday ,dt, rkind &
!	 &                     ,xnd,ynd,dp00,kbp00, i34,elnode, isidenode,snx,sny,distj 
      use schism_msgp
!	  , only:nproc,myrank
	  implicit none
      include 'netcdf.inc'
	  
!     Name list
      integer :: ntracer_gen,ntracer_age,sed_class,eco_class !,flag_fib
      namelist /CORE/ipre,ibc,ibtp,ntracer_gen,ntracer_age,sed_class,eco_class, &
     &nspool,ihfskip,msc2,mdc2,dt,rnday

      integer :: i,j,k,n,m,mm, istat, nr
      character (len=longname) :: dateiname,systemaufruf
!      integer :: start_year,start_month,start_day
!      real*8 :: start_hour,utc_start !, dt
!      character(len=72) :: fgb,fgb2,fdb  ! Processor specific global output file name
      character(len=400) :: textline
      integer :: ne_l, np_l, ns_l
      integer ::irank
	  integer :: nmax, nmin, tn, bn
      integer,allocatable , dimension (:,:) :: nm2
	  integer,allocatable , dimension (:) :: kbp00_global , i34_global
      real :: xmax, xmin, ymax, ymin, zmax, zmin, rzone, distmax, distmin, dtout, xg,yg

      if(meinrang.eq.0)then !! nur prozessor 0
	     ! print*,'read_mesh_nc_sc starts'
	  
	     ! read param.out.nml
	     write(dateiname,"(2A,I4.4,3A)")trim(modellverzeichnis),"outputs_schism/param.out.nml"
	     open(15,file=dateiname,delim='apostrophe',status='old')
         read(15,nml=CORE)
		 deltat=int(dt)
		 print*,"param.out.nml DT,RNDAY=",DT,deltat,RNDAY
		 close(15)

	     !--- zone.gr3 
         open(14,file='zone.gr3',status='old',iostat=istat)
         if(istat/=0) call qerror('read_mesh_nc_sc: zone.gr3 open failure')
         read(14,*); read(14,*) ne,np
         ! print*,'read_mesh_nc_sc: zone.gr3: ne,np=',ne,np
         n_elemente=ne
         knotenanzahl2D=np
         allocate (knoten_x(knotenanzahl2D),knoten_y(knotenanzahl2D),knoten_z(knotenanzahl2D),   &
     &             knoten_zone(knotenanzahl2D),knoten_rang(knotenanzahl2D),    &
     &             knoten_rand(knotenanzahl2D),knoten_flaeche(knotenanzahl2D), stat = istat )
         do i=1,np
            read(14,*)n,knoten_x(i),knoten_y(i),rzone
            if(n.ne.i) call qerror('reading zone.gr3 nodes: something gone wrong')
            knoten_zone(i)=int(rzone)
            if((knoten_zone(i).lt. 0).or.(knoten_zone(i).gt. 300))then
               write(fehler,*)' knoten #',i,': Zonennummer darf nicht negativ oder größer als 300 sein; ist aber=',knoten_zone(i)
               call qerror(fehler)
            end if
         enddo
         xmax=-999999999999.9 ; xmin=999999999999.9  
         ymax=-999999999999.9 ; ymin=999999999999.9  
         nmax=-99999999       ; nmin= 99999999
         do i=1,np
            if(xmax.le.knoten_x(i))xmax=knoten_x(i)
            if(xmin.ge.knoten_x(i))xmin=knoten_x(i)  
            if(ymax.le.knoten_y(i))ymax=knoten_y(i)  
            if(ymin.ge.knoten_y(i))ymin=knoten_y(i) 
            if(nmax.le.knoten_zone(i))nmax=knoten_zone(i)
            if(nmin.ge.knoten_zone(i))nmin=knoten_zone(i)
         end do ! alle i Knoten
         print*,'zone.gr3: ne,np=',ne,np
         print*,'zone.gr3: x-koordinate max+min', xmax, xmin
         print*,'zone.gr3: y-koordinate max+min', ymax, ymin
         print*,'zone.gr3: Zone max+min', nmax, nmin 

         !Read local_to_global_0000 for global info
         write(dateiname,'(4A)')trim(modellverzeichnis),'outputs_schism','/','local_to_global_0000'
         open(10, file=dateiname, status='old', iostat = istat)
         if(istat/=0) then
            write(fehler,*)'read_mesh_nc_sc local_to_global_0000 failed, rank=',meinrang
            call qerror(trim(fehler))
         end if
         read(10,*)ns_global,ne_global,np_global,nvrt,nproc !,ntracers
         !print*,'read_mesh_nc_sc: local_to_global_0000 ',trim(adjustl(dateiname))
		 print*,'local_to_global_0000: points, elements, sides levels, processes',np_global,ne_global,ns_global,nvrt,nproc
	     rewind(10)
         close(10)
         if((n_elemente.ne.ne_global).or.(knotenanzahl2D.ne.np_global))then
            write(fehler,*)'element + nodenumber dissence zone.gr3:',n_elemente,knotenanzahl2D ; call qerror(fehler)
		 else
            knotenanzahl2D= np_global
		    n_elemente=     ne_global
			kantenanzahl=   ns_global
         endif
         if(nproc.ne.proz_anz)then
            write(fehler,*)'read_mesh_nc_sc: given number of processes=', proz_anz, ', needed number of processes=', nproc
            call qerror(fehler)
         !else 
         !   print*,'read_mesh_nc_sc: equal number of processes QSim+SCHISM =', proz_anz,nproc
         endif !! wrong process number
         knotenanzahl3D=nvrt*np_global
        ! do i=0,proz_anz-1
        !    ! Read in local-global mappings from all ranks
        !    lfdb=len_trim(dateiname)
        !    !Find max. for dimensioning
        !    write(dateiname(lfdb-3:lfdb),'(i4.4)') i
		!    print*,trim(dateiname)
        !    open(10,file=dateiname,status='old', iostat = istat)
		!  	 if(istat/=0) then
        !       write(fehler,*)'read_mesh_nc_sc open local_to_global_* crashed, i=',i
        !       call qerror(trim(fehler))
        !    end if
        !    if(.not.zeile(10))call qerror('get_local_to_global erste zeile')
        !    if(.not.zeile(10))call qerror('get_local_to_global zweite zeile')
        !    close(10)
		!	 print*," seems o.k. ",i
        ! enddo ! all i processes
         !print*,'element number=',n_elemente,sum(ne)
         !print*,'read_mesh_nc_sc: using hydraulic event: ',trim(adjustl(modellverzeichnis))//'outputs_schism'
         !systemaufruf='ls -thora '//trim(adjustl(modellverzeichnis))//'outputs_schism'
         !call system(trim(systemaufruf),istat)
	 
      end if !! prozess 0 only

      call mpi_barrier (mpi_komm_welt, ierror)
      !if(meinrang.ne.0)allocate( np(0:proz_anz-1),ns(0:proz_anz-1),ne(0:proz_anz-1),stat=istat)
	  if(rkind.ne.8)call qerror('read_mesh_nc_sc: rkind.ne.8') ! Default real datatype from schism_glbl needs to be 8=DOUBLE_PRECISION
      call MPI_Bcast(dt,1,MPI_DOUBLE_PRECISION,0,mpi_komm_welt,ierror)
	  ! print* ,meinrang,"MPI_Bcast(dt",ierror
	  call MPI_Bcast(RNDAY,1,MPI_DOUBLE_PRECISION,0,mpi_komm_welt,ierror)
	  ! MPI_FLOAT, mpi_real, MPI_DOUBLE, MPI_LONG_DOUBLE,MPI_REAL2, MPI_REAL4 and MPI_REAL8
	  call MPI_Bcast(deltat,1,MPI_INT,0,mpi_komm_welt,ierror)
	  ! print* ,meinrang,"MPI_Bcast(deltat",ierror
      call MPI_Bcast(knotenanzahl3D,1,MPI_INT,0,mpi_komm_welt,ierror)
      call MPI_Bcast(knotenanzahl2D,1,MPI_INT,0,mpi_komm_welt,ierror)
      call MPI_Bcast(n_elemente,1,MPI_INT,0,mpi_komm_welt,ierror)
      call MPI_Bcast(kantenanzahl,1,MPI_INT,0,mpi_komm_welt,ierror)
      call MPI_Bcast(ns_global,1,MPI_INT,0,mpi_komm_welt,ierror)
      call MPI_Bcast(ne_global,1,MPI_INT,0,mpi_komm_welt,ierror)
      call MPI_Bcast(np_global,1,MPI_INT,0,mpi_komm_welt,ierror)
      call MPI_Bcast(nproc,1,MPI_INT,0,mpi_komm_welt,ierror)
      call MPI_Bcast(nvrt,1,MPI_INT,0,mpi_komm_welt,ierror)
      call mpi_barrier (mpi_komm_welt, ierror)
	  ! print*,meinrang,"read_mesh_nc_sc: dt,deltat,RNDAY=",dt,deltat,RNDAY

	  !!!! read first part: local_to_global_meinrang !!!!
      !print*,meinrang,'-',adjustl(trim(modellverzeichnis)),'outputs_schism','/','local_to_global_0000'
      if(longname .lt. len(adjustl(trim(modellverzeichnis)))+36)call qerror('read_mesh_nc_sc: longname too short')
      write(dateiname,'(4A)')adjustl(trim(modellverzeichnis)),'outputs_schism','/','local_to_global_0000'
      lfdb=len_trim(dateiname)
      write(dateiname(lfdb-3:lfdb),'(i4.4)') meinrang
      open(10+meinrang, file=trim(dateiname), status='old', iostat = istat)
	  !print*,"read_mesh_nc_sc read:  dateiname ",trim(dateiname)
      if(istat/=0) then
         write(fehler,*)'read_mesh_nc_sc open local_to_global_* failed, rank=',meinrang
         call qerror(trim(fehler))
      end if
      if(.not.zeile(10+meinrang))call qerror('get_local_to_global erste zeile')!print*,'1 ',trim(adjustl(dateiname)),' : ',trim(adjustl(ctext)), ' rang=',meinrang
      if(.not.zeile(10+meinrang))call qerror('get_local_to_global zweite zeile')!print*,'2 ',trim(adjustl(dateiname)),' : ',trim(adjustl(ctext)), ' rang=',meinrang

      read(10+meinrang,*, iostat = istat)nea
      if(istat/=0) call qerror('read_mesh_nc_sc nea')
      !call mpi_barrier (mpi_komm_welt, ierror)
      allocate( ielg(nea))
	  allocate(i34(nea),elnode(4,nea))
      do i=1,nea
        read(10+meinrang,*, iostat = istat)j,ielg(i)
        if(istat/=0) call qerror('read_mesh_nc_sc j,ielg(i)') 
      enddo !i
      !print*, trim(adjustl(dateiname)),' local number elements=',nea,j,ielg(j)

      read(10+meinrang,*, iostat = istat)npa
      if(istat/=0) call qerror('read_mesh_nc_sc npa')
	  allocate(eta2(npa))
	  allocate(xnd(npa),ynd(npa),dp00(npa),kbp00(npa))
      !call mpi_barrier (mpi_komm_welt, ierror)
      allocate( iplg(npa) )
      do i=1,npa
        read(10+meinrang,*, iostat = istat)j,iplg(i)
        if(istat/=0) call qerror('read_mesh_nc_sc j,iplg(i)')
      enddo !i
      ! print*, trim(adjustl(dateiname)),' local number points=',npa,j,iplg(j)
	  
      read(10+meinrang,*, iostat = istat)nsa
      if(istat/=0) call qerror('read_mesh_nc_sc nsa')
	  allocate( su2(nvrt,nsa),sv2(nvrt,nsa) )
	  allocate( isidenode(2,nsa), snx(nsa), sny(nsa), distj(nsa) )
      !call mpi_barrier (mpi_komm_welt, ierror)
      allocate( islg(nsa) )
      do i=1,nsa
         read(10+meinrang,*, iostat = istat)j,islg(i)
         if(istat/=0) then
            write(fehler,*)'read error get_local_to_global islg rank=',meinrang
            call qerror('fehler')
         end if
      enddo
	  
	  !!!! read second part:  !!!!
      !write(10,*)'Header:'
      !write(10,*)start_year,start_month,start_day,start_hour,utc_start
      !write(10,*)nrec,real(dt*nspool),nspool,nvrt,kz,real(h0),real(h_s),real(h_c),real(theta_b),real(theta_f),ics
      !write(10,*)(real(ztot(k)),k=1,kz-1),(real(sigma(k)),k=1,nvrt-kz+1)
	  read(10+meinrang,*)textline !; print*,meinrang,textline
	  read(10+meinrang,*)textline !; print*,textline
	  read(10+meinrang,*)textline !; print*,textline
	  read(10+meinrang,*)textline !; print*,textline
      read(10+meinrang,*)np,ne,ns !; print*,np,ne,ns
	  if((np.ne.npa).or.(ne.ne.nea).or.(ns.ne.nsa))then
	     call qerror('local_to_global np.ne.npa')
	  else
	     print*,trim(dateiname),": npa,nea,nsa=",npa,nea,nsa
	  endif !numbers wrong
      do i=1,np
        read(10+meinrang,*, iostat = istat)xnd(i),ynd(i),dp00(i),kbp00(i)
        if(istat/=0) then;print*,meinrang,i;call qerror('read_mesh_nc_sc xnd');endif
      enddo !all nodes i
      do m=1,ne
        read(10+meinrang,*, iostat = istat)i34(m),(elnode(mm,m),mm=1,i34(m))
        if(istat/=0) then;print*,meinrang,i;call qerror('read_mesh_nc_sc elnode');endif
      enddo !m
      do m=1,ns
	    read(10+meinrang,'(A)')textline
        read(textline,*, iostat = istat)i,(isidenode(mm,m),mm=1,2),snx(m),sny(m),distj(m)
		if(i/=m)call qerror("read_mesh_nc_sc: i .ne. m")
        if(istat/=0) then
		   print*,meinrang,m
		   print*,m,isidenode(1,m),isidenode(2,m),snx(m),sny(m),distj(m)
		   call qerror('read_mesh_nc_sc isidenode')
		endif
      enddo !m
      rewind(10+meinrang)
      close(10+meinrang)
      call mpi_barrier (mpi_komm_welt, ierror)

      if(meinrang.eq.0)then !! nur prozessor 0
         allocate(ne_sc(proz_anz), np_sc(proz_anz), ns_sc(proz_anz),stat=istat)
         if(istat/=0) call qerror('Allocation error: np')
         allocate(ztot(nvrt),sigma(nvrt) ,stat=istat)
         if(istat/=0) call qerror('Allocation error: ztot')
         allocate( kbp00_global(knotenanzahl2D) ,stat=istat)
         if(istat/=0) call qerror('Allocation error: kbp00_global')
	     ! print*,"read_mesh_nc_sc: allocate 0 1"

!         if(istat/=0) stop 'Allocation error: kbe'
         allocate( ielg_sc(proz_anz,n_elemente) ) !! too large, who cares?
         allocate( iplg_sc(proz_anz,knotenanzahl2D) )
         allocate( islg_sc(proz_anz,kantenanzahl) )
         allocate( i34_global(n_elemente), nm2(4,n_elemente), stat = istat)
		 allocate( top_node(kantenanzahl), bottom_node(kantenanzahl), stat = istat)
		 allocate( edge_normal_x(kantenanzahl), edge_normal_y(kantenanzahl), &
     &             cell_bound_length(kantenanzahl), stat = istat)
	     ! print*,"read_mesh_nc_sc: allocate 0 2"
		 top_node(:)=-1; bottom_node(:)=-1
		 edge_normal_x(:)=-77.7; edge_normal_y(:)=-77.7; cell_bound_length(:)=-77.7
		 
         if(istat/=0) call qerror('Allocation error: i34_global')
		 allocate( ed_flux(kantenanzahl), ed_area(kantenanzahl), stat = istat )
         if(istat/=0) call qerror('Allocation error: ed_flux')
		 allocate( ed_vel_x(kantenanzahl), ed_vel_y(kantenanzahl), stat = istat )
         if(istat/=0) call qerror('Allocation error: ed_vel_x')
		 ! init
		 ed_flux(:)=-1; ed_area(:)=-1; ed_vel_x(:)=-1; ed_vel_y(:)=-1
	     ! print*,"read_mesh_nc_sc: allocate 0 3"

         allocate (elementnodes(n_elemente,4), stat=istat)
         if(istat.ne. 0) call qerror('allocate elementnodes failed')
         allocate (cornernumber(n_elemente), stat=istat )
         if(istat/=0) call qerror('Allocation error: cornernumber')
         allocate (element_zone(n_elemente), stat = istat )
         if(istat.ne. 0) call qerror('allocate element_zone failed')
         allocate (element_rand(n_elemente), stat = istat )
         if(istat.ne. 0) call qerror('allocate element_zone failed')
         do n=1,n_elemente !initialize zones and boundaries
            element_zone(n) = 0
            element_rand(n) = 0
         end do ! alle Elemente!
	     ! print*,"read_mesh_nc_sc: allocate 0 4"

         ! reread on process 0
         write(dateiname,'(4A)')trim(modellverzeichnis),'outputs_schism','/','local_to_global_0000'
		 ! print*,"read_mesh_nc_sc reread:  dateiname ",trim(dateiname)
         lfdb=len_trim(dateiname)
         maxstack=0
         do irank=1,proz_anz,1 ! all ranks
            write(dateiname(lfdb-3:lfdb),'(i4.4)') irank-1
            open(10, file=dateiname, status='old', iostat = istat)
			! print*,"do irank dateiname=",trim(adjustl(dateiname))
            if(istat/=0)then
               call qerror('open 10 failed')
            !else
               !print*,'success open 10 ',trim(adjustl(dateiname))
            endif ! open failed
            if(.not.zeile(10))call qerror('Lesefehler 1')
            if(.not.zeile(10))call qerror('Lesefehler 2')
            read(10,*)ne_l
            do k=1,ne_l
               read(10,*)j,ielg_sc(irank,k)
            enddo !k
            read(10,*)np_l
            !print*,'irank,np_l=',irank,np_l,trim(adjustl(dateiname))
            do k=1,np_l
               read(10,*)j,iplg_sc(irank,k)
               knoten_rang(iplg_sc(irank,k))=irank
            enddo !k
            read(10,*)ns_l
            do k=1,ns_l
               read(10,*)j,islg_sc(irank,k)
            enddo !k
			! print*,irank,"read_mesh_nc_sc: np_l,ne_l,ns_l=",np_l,ne_l,ns_l
			! take care that maxstack is large enough for everything
            if(maxstack.lt.ne_l)maxstack=ne_l
            if(maxstack.lt.np_l)maxstack=np_l
            if(maxstack.lt.ns_l)maxstack=ns_l 

            if(.not.zeile(10))call qerror('get_local_to_global Header missing')!read(10+meinrang,*) !'Header:'
            read(10,*)start_year,start_month,start_day,start_hour,utc_start 
            !if(irank.eq.1)print*,'Start Date=',start_year,start_month,start_day,start_hour,utc_start
            jahr=start_year
            monat=start_month
            tag=start_day
            stunde=int(start_hour)
            minute=int( start_hour*60.0 - int(start_hour)*int(60) )
            sekunde=int( start_hour*3600.0 - int(start_hour)*int(3600) )
            ! version='v10'
            read(10,*)nrec,dtout,nspool,nvrt,kz,h0,h_s,h_c,theta_b,theta_f,ics
            !print*,'read_mesh_nc_sc: nrec,dtout,nspool=',nrec,dtout,nspool
            read(10,*)(ztot(k),k=1,kz-1),(sigma(k),k=1,nvrt-kz+1)
            !print*,'read_mesh_nc_sc: ztot(k),kz,nvrt=',ztot(1),kz,nvrt
            read(10,*)np_sc(irank),ne_sc(irank),ns_sc(irank)
			!print*,irank,'read_mesh_nc_sc: np_sc,ne_sc,ns_sc=',np_sc(irank),ne_sc(irank),ns_sc(irank)
            if(np_sc(irank).ne. np_l) call qerror('np_sc(irank).ne. np_l')
			do m=1,np_sc(irank)
               read(10,*, iostat = istat) xg, yg, knoten_z(iplg_sc(irank,m)), kbp00_global(iplg_sc(irank,m))
               if(istat/=0)call qerror('read(10,*) ( knoten_x... failed')
			   if(abs(knoten_x(iplg_sc(irank,m))-xg).gt.0.1)call qerror("knoten_x missmatch local_to_global zone.gr3 ")
			   if(abs(knoten_y(iplg_sc(irank,m))-yg).gt.0.1)call qerror("knoten_y missmatch local_to_global zone.gr3 ")
			   knoten_rang(iplg_sc(irank,m))=irank
			enddo !m
            read(10,*, iostat = istat) ( i34_global(ielg_sc(irank,m)), &
			                           (nm2(mm,m),mm=1,i34_global(ielg_sc(irank,m))), m=1,ne_sc(irank) )
            if(istat/=0)call qerror('read(10,*)( i34_global(ielg(irank,m))... failed')
			do m=1,ns_sc(irank)
			   read(10,*,iostat = istat)i,tn,bn,  &
     &            edge_normal_x(islg_sc(irank,m)),edge_normal_y(islg_sc(irank,m)),cell_bound_length(islg_sc(irank,m))
			      !snx_l(m),sny_l(m),distj_l(m)
               if(istat/=0)call qerror('read(10,*) local_to_global (top +bottom node)... failed')
			   if( ((tn.le.0).or.(bn.le.0)) .or. ((tn.gt.np_sc(irank)).or.(bn.gt.np_sc(irank))) )then
			      print*, irank, tn, bn, np_sc(irank)
			      call qerror('isidenode not o.k.')
			   endif ! node numbers ok?
			   if((tn.le.0).or.(tn.gt.np_sc(irank)))print*,irank,tn,np_sc(irank)," tn wrong"
			   if((bn.le.0).or.(bn.gt.np_sc(irank)))print*,irank,bn,np_sc(irank)," bn wrong"
			   top_node(   islg_sc(irank,m))=iplg_sc(irank,tn)
			   bottom_node(islg_sc(irank,m))=iplg_sc(irank,bn)
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
            do m=1,ne_sc(irank)  ! Elements
               j=ielg_sc(irank,m) ! global element number
               if(j>n_elemente) call qerror('element number error in read_mesh_nc_sc')
               cornernumber(j)=i34_global(j)
               if((cornernumber(j).lt. 3).or.(cornernumber(j).gt. 4)) call qerror('cornernumber neither 3 nor 4 in read_mesh_nc_sc')
               do mm=1,cornernumber(j)
                  i=nm2(mm,m) ! local node number
                  if(i>np_sc(irank).or.i<=0) then
                     print*,',nm2(1....4,',m,'))=',nm2(1,m),nm2(2,m),nm2(3,m),nm2(4,m)
                     print*,',i,np(irank),irank,nm2(mm,m),mm,m,j,cornernumber(j)=',  &
     &                        i,np_sc(irank),irank,nm2(mm,m),mm,m,j,cornernumber(j)
                     call qerror('cornernumber error in read_mesh_nc_sc')
                  endif
                  elementnodes(j,mm)=iplg_sc(irank,i)
               enddo !mm
            enddo !m	! Elements
         enddo ! all irank
         deallocate( nm2 )
         do n=1,kantenanzahl !! check integrity of side-node connectivity
		 	if((top_node(n).le.0).or.(top_node(n).gt.knotenanzahl2D))print*,"top_node(",n,")wrong",top_node(n)
			if((bottom_node(n).le.0).or.(bottom_node(n).gt.knotenanzahl2D))print*,"bottom_node(",n,")wrong",bottom_node(n)
         end do ! all n sides=edges

         summ_ne=0 ! needed by vtk output
         do n=1,n_elemente
            summ_ne=summ_ne+cornernumber(n)+1
         end do ! all Elements
		 
	  	 distmax=-999999999999.9
		 distmin=999999999999.9
         do n=1,kantenanzahl ! all edges
               if(distmax.le.cell_bound_length(n))distmax=cell_bound_length(n)
               if(distmin.ge.cell_bound_length(n))distmin=cell_bound_length(n) 
         end do ! all n edges/sides
         element_vorhanden=.true.
         xmax=-999999999999.9 
         xmin=999999999999.9  
         ymax=-999999999999.9  
         ymin=999999999999.9  
         zmax=-999999999999.9  
         zmin=999999999999.9 
         do n=1,knotenanzahl2D
            if(xmax.le.knoten_x(n))xmax=knoten_x(n)
            if(xmin.ge.knoten_x(n))xmin=knoten_x(n)  
            if(ymax.le.knoten_y(n))ymax=knoten_y(n)  
            if(ymin.ge.knoten_y(n))ymin=knoten_y(n) 
            knoten_z(n)=knoten_z(n)*(-1.0) ! bathymety elevation upwards
            if(zmax.le.knoten_z(n))zmax=knoten_z(n)
            if(zmin.ge.knoten_z(n))zmin=knoten_z(n)
         end do ! alle Knoten
         print*,'local_to_global_0000: x-koordinate max+min', xmax, xmin
         print*,'local_to_global_0000: y-koordinate max+min', ymax, ymin
         print*,'local_to_global_0000: Sohlhöhe max+min', zmax, zmin 
         !print*,'maxstack=',maxstack
         print*,'local_to_global_0000: edge length distmax+distmin', distmax, distmin 

      end if !! prozess 0 only
      call mpi_barrier (mpi_komm_welt, ierror)
      call MPI_Bcast(maxstack,1,MPI_INT,0,mpi_komm_welt,ierror)
      nst_prev=-333 ! initialize
      ncid=-333
      call mpi_barrier (mpi_komm_welt, ierror)

      ! get boundary numbers from zone.gr3
      if(meinrang.eq.0)then !! prozess 0 only
         do j=1,knotenanzahl2D ! initialize all j nodes with  boundary number 0 = inside node
            knoten_rand(j)=0
         end do ! alle j node
         rewind(14)
         read(14,*); read(14,*) !header+numbers
         do i=1,knotenanzahl2D; read(14,*); enddo;
         do i=1,n_elemente; read(14,*); enddo;
         read(14,*) ianz_rb
         read(14,*) neta_global
         ! print*,'read_mesh_nc_sc: open+closed boundaries',ianz_rb,neta_global
         do j=1,ianz_rb ! all j open boundaries
            read(14,*) nr
            do i=1,nr; read(14,*) n; knoten_rand(n)=j; enddo;
            print*,'read_mesh_nc_sc: zone.gr3 open boundary ',j, ' has ',nr,' nodes'
         end do ! all j open boundaries
         close(14)
      end if !! prozess 0 only
      call mpi_barrier (mpi_komm_welt, ierror)

      !if(meinrang.eq.0) print*,'read_mesh_nc_sc finished' 
      RETURN
      END subroutine read_mesh_nc_sc
