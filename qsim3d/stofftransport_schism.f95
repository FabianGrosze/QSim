
!> \page Transport_SCHISM Transportinformationen von SCHISM
!! dargestellt im Bericht ...
!! \n\n aus Datei stofftransport_schism.f95; zurück zu \ref Datentechnik oder \ref Transportinformationen


      SUBROUTINE stofftransport_schism()
      use netcdf
      use modell                                                   
      implicit none
      include 'netcdf.inc'
      integer nti, nt, n,j,k, subtim, diff, diffprev, alloc_status
      real :: laeng, cu_max, cu_min, dt_sub, sumwicht
      real , allocatable , dimension (:,:) :: zwischen
      integer , parameter :: num_sub=1

if(meinrang.eq.0)then !! prozessor 0 only
      print*,'stofftransport_schism: startzeitpunkt, zeitpunkt, dt, endzeitpunkt' &
     &        ,startzeitpunkt, zeitpunkt, dt, endzeitpunkt
      do nt=1,num_sub ! alle Transport (zwischen) Zeitschritte
         subtim=startzeitpunkt + int( real((2*nt-1)*dt)/real(num_sub*2) )
         print*,'stofftransport_schism: substep-time=',subtim, nt
         if(subtim.lt.transinfo_zeit(transinfo_zuord(1)))call qerror('subzeitpunkt vor SCHISM Zeitraum')
         if(subtim.gt.transinfo_zeit(transinfo_zuord(transinfo_anzahl)))call qerror('subzeitpunkt nach SCHISM Zeitraum')
         nti=0
         diffprev=(subtim-transinfo_zeit(transinfo_zuord(1)) )
         do n=2,transinfo_anzahl
            diff= (subtim-transinfo_zeit(transinfo_zuord(n)) )
            if( (real(diffprev)*real(diff)) .le. 0.0 )then !in between ## Wertebereichsüberschreitung bei integermultiplikation
               if(abs(diff).gt.abs(diffprev))then !closer to first
                  nti=n-1
               else
                  nti=n
               end if !closer to first
               print*,'stofftransport_schism: n,diff,diffprev,diffprev*diff,abs='  &
     &               ,n,diff,diffprev,(real(diffprev)*real(diff)),abs(diff),abs(diffprev)
            end if ! !in between
            diffprev=diff
         end do ! all n transport sub steps
         if(nti.lt. 1) then ! schould not happen
            call qerror('detecting closest hydro-timestep failed')
         end if
         print*,'stofftransport_schism: nti=',nti   &
     &         ,' transportiert mit SCHISM-Strömungsfeld zuord,zeit,stack,instack,datei='  &
     &         ,transinfo_zuord(nti), transinfo_zeit(transinfo_zuord(nti)),transinfo_stack(transinfo_zuord(nti)) &
     &         ,transinfo_instack(transinfo_zuord(nti)), transinfo_datei(transinfo_zuord(nti))
      end do ! all nt Substeps
end if !! prozess 0 only
      call mpi_barrier (mpi_komm_welt, ierr)
      call MPI_Bcast(nti,1,MPI_INT,0,mpi_komm_welt,ierr)
      !do nt=1,num_sub ! alle Transport (zwischen) Zeitschritte
         call holen_trans_schism(nti)
      !end do ! all nt Substeps
      call mpi_barrier (mpi_komm_welt, ierr)

! die anderen beiden treiber machen :
         call gather_planktkon()
         call scatter_planktkon()
! weil stofftransport nicht parallel ???? braucht schism das ????

      RETURN
      END subroutine stofftransport_schism
!----+-----+-----+-----+-----+-----+-----+-----+-----+-----+

      SUBROUTINE read_mesh_nc_sc() !meinrang.eq.0
      use netcdf
      use modell                                                   
      implicit none
      include 'netcdf.inc'
      integer :: i,j,k,n,m,mm,ne,np, neta_global,nr
      integer ns_global,ne_global,np_global,nvrt,nproc
      integer lfdb, istat
      character (len=longname) :: dateiname,systemaufruf
      integer :: start_year,start_month,start_day
      real*8 :: start_hour,utc_start
      character(len=72) :: fgb,fgb2,fdb  ! Processor specific global output file name
      integer :: ne_p, np_p, ns_p ! numbers on each process
      integer :: ne_l, np_l, ns_l
      integer,allocatable :: ielg_p(:),iplg_p(:),islg_p(:) ! global numbers on each process
      integer ::irank
      integer :: nrec, nspool, kz, ics, nmax, nmin
      real :: dtout, h0, h_s, h_c, theta_b, theta_f
      real,allocatable , dimension (:) :: ztot, sigma
      integer,allocatable , dimension (:)   :: kbp00, i34
      integer,allocatable , dimension (:,:) :: nm2
      real :: xmax, xmin, ymax, ymin, zmax, zmin, rzone

      !--- zone.gr3 contains mesh too
      if(meinrang.eq.0)then !! nur prozessor 0
         open(14,file='zone.gr3',status='old',iostat=istat)
         if(istat/=0) call qerror('ead_mesh_nc_sc: zone.gr3 open failure')
         read(14,*); read(14,*) ne,np
         print*,'zone.gr3: ne,np=',ne,np
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
         print*,'zone.gr3:'
         print*,'x-koordinate max+min', xmax, xmin
         print*,'y-koordinate max+min', ymax, ymin
         print*,'Zone max+min', nmax, nmin 

         !Read local_to_global_0000 for global info
         write(dateiname,'(4A)')trim(modellverzeichnis),'outputs_schism','/','local_to_global_0000'
         print*,dateiname
         open(10, file=dateiname, status='old', iostat = istat)
         if(istat/=0) then
            write(fehler,*)'read_mesh_nc_sc local_to_global_0000 failed, rank=',meinrang
            call qerror(trim(fehler))
         end if
         read(10,*)ns_global,ne_global,np_global,nvrt,nproc !,ntracers
         print*,'read_mesh_nc_sc: local_to_global_0000 ',trim(adjustl(dateiname)), ' points, elements, sides levels, processes'  &
     &         ,np_global,ne_global,ns_global,nvrt,nproc
         close(10)
         if((ne.ne.ne_global).or.(np.ne.np_global))then
            write(fehler,*)'element + nodenumber dissence zone.gr3:',ne,np ; call qerror(fehler)
         endif
         if(nproc.ne.proz_anz)then
            write(fehler,*)'read_mesh_nc_sc: given number of processes=', proz_anz, ', needed number of processes=', nproc
            call qerror(fehler)
         endif !! wrong process number
         knotenanzahl3D=nvrt*np_global
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
      call MPI_Bcast(knotenanzahl3D,1,MPI_INT,0,mpi_komm_welt,ierr)
      call MPI_Bcast(knotenanzahl2D,1,MPI_INT,0,mpi_komm_welt,ierr)
      call MPI_Bcast(n_elemente,1,MPI_INT,0,mpi_komm_welt,ierr)
      call MPI_Bcast(ns_global,1,MPI_INT,0,mpi_komm_welt,ierr)

      !print*,meinrang,'-',adjustl(trim(modellverzeichnis)),'outputs_schism','/','local_to_global_0000'
      if(longname .lt. len(adjustl(trim(modellverzeichnis)))+36)call qerror('read_mesh_nc_sc: longname too short')
      write(dateiname,'(4A)')adjustl(trim(modellverzeichnis)),'outputs_schism','/','local_to_global_0000'
      lfdb=len_trim(dateiname)
      write(dateiname(lfdb-3:lfdb),'(i4.4)') meinrang
      !print*, adjustl(trim(dateiname))
      open(10+meinrang, file=trim(dateiname), status='old', iostat = istat)
      if(istat/=0) then
         write(fehler,*)'read_mesh_nc_sc open local_to_global_* failed, rank=',meinrang
         call qerror(trim(fehler))
      end if
      if(.not.zeile(10+meinrang))call qerror('get_local_to_global erste zeile')!print*,'1 ',trim(adjustl(dateiname)),' : ',trim(adjustl(ctext)), ' rang=',meinrang
      if(.not.zeile(10+meinrang))call qerror('get_local_to_global zweite zeile')!print*,'2 ',trim(adjustl(dateiname)),' : ',trim(adjustl(ctext)), ' rang=',meinrang
      read(10+meinrang,*)ne_p
      allocate( ielg_p(ne_p))
      do i=1,ne_p
        read(10+meinrang,*)j,ielg_p(i)
      enddo !i
      !print*, trim(adjustl(dateiname)),' local number elements=',ne_p,j,ielg(j)
      read(10+meinrang,*)np_p
      allocate( iplg_p(np_p) )
      do i=1,np_p
        read(10+meinrang,*)j,iplg_p(i)
      enddo !i
      !print*, trim(adjustl(dateiname)),' local number points=',np_p,j,iplg(j)
      read(10+meinrang,*)ns_p
      allocate( islg_p(ns_p) )
      do i=1,ns_p
         read(10+meinrang,*, iostat = istat)j,islg_p(i)
         if(istat/=0) then
            write(fehler,*)'read error get_local_to_global islg rank=',meinrang
            call qerror('fehler')
         end if
      enddo
      !print*, trim(adjustl(dateiname)),' local number sides=',ns_p,j,islg_p(j)
      close(10+meinrang)
      call mpi_barrier (mpi_komm_welt, ierr)

      if(meinrang.eq.0)then !! nur prozessor 0
         allocate(ne_sc(proz_anz), np_sc(proz_anz), ns_sc(proz_anz),stat=istat)
         if(istat/=0) call qerror('Allocation error: np')
         allocate(ztot(nvrt),sigma(nvrt) ,stat=istat)
         if(istat/=0) call qerror('Allocation error: ztot')
         allocate( kbp00(knotenanzahl2D) ,stat=istat)
         if(istat/=0) call qerror('Allocation error: kbp00')

!         if(istat/=0) stop 'Allocation error: kbe'
         allocate( ielg_sc(proz_anz,n_elemente) ) !! too large, who cares?
         allocate( iplg_sc(proz_anz,knotenanzahl2D) )
         allocate( islg_sc(proz_anz,ns_global) )
         allocate( i34(n_elemente), nm2(4,n_elemente), stat = istat)
         if(istat/=0) call qerror('Allocation error: i34')

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

         ! reread on process 0
         write(dateiname,'(4A)')trim(modellverzeichnis),'outputs_schism','/','local_to_global_0000'
         lfdb=len_trim(dateiname)
         maxstack=0
         do irank=1,proz_anz ! all ranks
            write(dateiname(lfdb-3:lfdb),'(i4.4)') irank-1
            open(10, file=dateiname, status='old', iostat = istat)
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
            if(maxstack.lt.np_l)maxstack=np_l
            print*,'irank,np_l=',irank,np_l,trim(adjustl(dateiname))
            do k=1,np_l
               read(10,*)j,iplg_sc(irank,k)
               knoten_rang(iplg_sc(irank,k))=irank
            enddo !k
            read(10,*)ns_l
            do k=1,ns_l
               read(10,*)j,islg_sc(irank,k)
            enddo !k
            if(.not.zeile(10))call qerror('get_local_to_global Header missing')!read(10+meinrang,*) !'Header:'
            read(10,*)start_year,start_month,start_day,start_hour,utc_start 
            if(irank.eq. 1) print*,'Start Date=',start_year,start_month,start_day,start_hour,utc_start
            jahr=start_year
            monat=start_month
            tag=start_day
            stunde=int(start_hour)
            minute=int( start_hour*60.0 - int(start_hour)*int(60) )
            sekunde=int( start_hour*3600.0 - int(start_hour)*int(3600) )
            ! version='v10'
            read(10,*)nrec,dtout,nspool,nvrt,kz,h0,h_s,h_c,theta_b,theta_f,ics
            !print*,'nvrt,kz,irank=',nvrt,kz,irank
            read(10,*)(ztot(k),k=1,kz-1),(sigma(k),k=1,nvrt-kz+1)
            read(10,*)np_sc(irank),ne_sc(irank)
            if(np_sc(irank).ne. np_l) call qerror('np_sc(irank).ne. np_l')
            !print*,'np(irank),ne(irank),irank=',np(irank), np_l, ne(irank), ne_l, irank
            read(10,*, iostat = istat) ( knoten_x(iplg_sc(irank,m)), knoten_y(iplg_sc(irank,m)), knoten_z(iplg_sc(irank,m)),  &
     &                   kbp00(iplg_sc(irank,m)),m=1,np_sc(irank) )
            if(istat/=0)call qerror('read(10,*) ( knoten_x... failed')
            read(10,*, iostat = istat) ( i34(ielg_sc(irank,m)), (nm2(mm,m),mm=1,i34(ielg_sc(irank,m))), m=1,ne_sc(irank) )
            if(istat/=0)call qerror('read(10,*)( i34(ielg(irank,m))... failed')
            close(10)
            ! Reconstruct connectivity table
            do m=1,ne_sc(irank)
               j=ielg_sc(irank,m) ! global element number
               if(j>n_elemente) call qerror('element number error in read_mesh_nc_sc')
               cornernumber(j)=i34(j)
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
            enddo !m
         enddo ! all irank
         deallocate( i34, nm2, kbp00)
         summ_ne=0 ! needed by vtk output
         do n=1,n_elemente
            summ_ne=summ_ne+cornernumber(n)+1
         end do ! alle Knoten

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
         print*,'local_to_global_0000:'
         print*,'x-koordinate max+min', xmax, xmin
         print*,'y-koordinate max+min', ymax, ymin
         print*,'Sohlhöhe max+min', zmax, zmin 
         print*,'maxstack=',maxstack

      end if !! prozess 0 only
      call mpi_barrier (mpi_komm_welt, ierr)
      call MPI_Bcast(maxstack,1,MPI_INT,0,mpi_komm_welt,ierr)
      if(meinrang.eq.0)then
         allocate(var_g(proz_anz*maxstack),stat = istat)
         if(istat.ne.0) call qerror("allocate var_g( failed")
      endif
      allocate(var_p(maxstack),stat = istat)
      if(istat.ne.0) call qerror("allocate var_p( failed")
      nst_prev=-333 ! initialize
      ncid=-333
      call mpi_barrier (mpi_komm_welt, ierr)

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
         print*,'read_mesh_nc_sc: open+closed boundaries',ianz_rb,neta_global
         do j=1,ianz_rb ! all j open boundaries
            read(14,*) nr
            do i=1,nr; read(14,*) n; knoten_rand(n)=j; enddo;
            print*,'read_mesh_nc_sc: open boundary ',j, ' has ',nr,' nodes'
         end do ! all j open boundaries
         close(14)
      end if !! prozess 0 only
      call mpi_barrier (mpi_komm_welt, ierr)

      RETURN
      END subroutine read_mesh_nc_sc
!----+-----+-----+-----+-----+-----+-----+-----+-----+-----+

      subroutine nc_sc_sichten()
      use netcdf
      use modell                                                   
      implicit none
      include 'netcdf.inc'
      integer :: i,j,k,l,m,n, istat
      character (len=400) :: dateiname,systemaufruf, chari
      INTEGER :: iret, ndims, nVars, nGlobalAtts, unlimdimid, nAtts
      integer , allocatable , dimension (:) :: dlength
      character(256) , allocatable , dimension (:) :: dname
      integer, dimension(nf90_max_var_dims) :: dimids
      integer , allocatable , dimension (:) :: vxtype, vndims
      CHARACTER(256) , allocatable , dimension (:) :: vname
      integer attnum, alen, varid
      logical weiter
      real , allocatable , dimension (:) :: zeiten
      real zeit_min, zeit_max
      real zeit_delta
      integer :: nnd, nnv, sumtra, nnt

      if(meinrang.eq.0) print*,'nc_sc_sichten starts' 

      !--- netcdf-files parallel
      !print*,nf90_max_var_dims,'nf90_max_var_dims',meinrang
      weiter=.true.
      i=1
      n_stacks=0
      ndims=0
      nVars=0
      do while (weiter)
         write(chari,*),i
         write(dateiname,'(2A,I4.4,3A)')trim(modellverzeichnis),'outputs_schism/schout_',meinrang,'_',trim(adjustl(chari)),'.nc' !schout_0001_1.nc
         systemaufruf='stat '//trim(adjustl(dateiname))//' >/dev/null 2>/dev/null'
         call system(trim(systemaufruf),istat)
         if(istat.eq. 0)then
            n_stacks=i
            i=i+1
         else
            weiter=.false.
            !if(meinrang.eq.0)print*,'nc_sc_sichten: systemaufruf',trim(adjustl(systemaufruf))
         endif
      end do
      if(meinrang.eq.0)print*,"nc_sc_sichten,n_stacks=",n_stacks
      call mpi_barrier (mpi_komm_welt, ierr)

      transinfo_anzahl=0
      zeit_min=3153600000.0
      zeit_max=-3153600000.0

      do i=1,n_stacks
      !do i=1,10
         write(chari,*),i
         write(dateiname,'(2A,I4.4,3A)')trim(modellverzeichnis),'outputs_schism/schout_',meinrang,'_',trim(adjustl(chari)),'.nc' !schout_0001_1.nc
         iret = nf_open(dateiname, NF_NOWRITE, ncid)
         if(iret.ne. 0) then
            call check_err(iret)
            write(fehler,*)meinrang,i,' nc_sc_sichten: nf_open failed ',dateiname,iret
            call qerror(fehler)
         end if ! open failed
         call check_err( nf90_inquire(ncid, ndims, nVars, nGlobalAtts, unlimdimid) )!--- overview
         !! dimensions
         if(i.eq. 1)allocate (dlength(ndims),dname(ndims), stat = istat)
         if(i.eq. 1)allocate (vxtype(nVars),vndims(nVars),vname(nVars),  stat = istat )
         do j=1,ndims
            iret = nf90_Inquire_Dimension(ncid, j, dname(j), dlength(j))
            call check_err(iret)
            if((meinrang.eq. 2).and.(i.eq. 1))  &
     &         print*,meinrang,i,j,' nc_sc_sichten: Dimension  ' ,trim(adjustl(dname(j))),' wert=', dlength(j)
         end do !all j dimension
         !! Variables
         do j=1,nVars
            iret = nf90_inquire_variable(ncid,j,vname(j),vxtype(j),vndims(j),dimids, nAtts)
            call check_err(iret)
            if((meinrang.eq. 0).and.(i.eq. 1))  &
     &         print*, j,'-th variable; name=' ,trim(adjustl(vname(j))),' , vxtype=',vxtype(j)
             do k=1,vndims(j)
               if((meinrang.eq. 0).and.(i.eq. 1))  &
     &            print*, k,'-th dimension; name=',trim(adjustl(dname(dimids(k)))),' dim.length='  &
     &                  ,dlength(dimids(k)),' dimid=' ,dimids(k)
            end do !k Dimensionen von Variable j
            !print*,"Attribute : "
            !call print_attributes(j, nAtts)
         end do ! Variable j
         !! time-steps
         iret =  nf_inq_varid(ncid,'time', varid)
         !print*,meinrang,i,' read_mesh_nc_sc: nf_inq_varid(time   iret=',iret,ncid
         call check_err(iret)
         iret = nf90_inquire_variable(ncid,varid,vname(varid),vxtype(varid),vndims(varid),dimids, nAtts)
         call check_err(iret)
         !print*,meinrang,i,' read_mesh_nc_sc: nf90_inquire_variable   varid=',varid,iret,ncid
         n=dlength(dimids(1))
         if(n.gt. 0)then
            transinfo_anzahl=transinfo_anzahl+n
            !print*,i,'read_mesh_nc_sc: transinfo_anzahl=',transinfo_anzahl,n
            allocate (zeiten(n), stat = istat )
            iret = nf90_get_var(ncid, varid, zeiten)
            call check_err(iret)
            if(iret.ne. 0)then
               print*,meinrang,i,' nc_sc_sichten: nf90_get_var  varid=',varid,iret,ncid
               call qerror('nc_sc_sichten: nf90_get_var failed')
            endif !
            if( zeit_min.ge.zeiten(1) ) zeit_min=zeiten(1)
            if( zeit_max.le.zeiten(n) ) zeit_max=zeiten(n)
            if(n.gt. 1)then
               zeit_delta=zeiten(2)-zeiten(1)
            end if ! more than one timestep
            !print*,meinrang,' nc_sc_sichten Zeit stack ',i,' zeiten 1,2=',zeiten(1), zeiten(2),zeit_delta
            deallocate(zeiten)
         end if ! dlength ok
         iret = nf_close(ncid)
         call check_err(iret)
         !if(meinrang.eq. 0)print*,i,' nc_sc_sichten firstread ',n, transinfo_anzahl, varid, iret
      end do !all i stacks
      if(transinfo_anzahl.lt. 1)call qerror('No transport info')
      !print*,meinrang,' nc_sc_sichten Zeit  von:',zeit_min,' bis: ',zeit_max,' zeit_delta=',zeit_delta

      call MPI_Allreduce(zeit_delta, dttrans, 1, MPI_FLOAT, MPI_SUM, mpi_komm_welt, iret)
      call check_err(iret)
      !print*,meinrang,' nc_sc_sichten: zeit_delta, dttrans= ',zeit_delta, dttrans
      dttrans=dttrans/real(proz_anz)
      if(int(zeit_delta).ne.int(dttrans))call qerror('timestep length unclear nc_sc_sichten')
      if(meinrang.eq.0)print*,meinrang,'nc_sc_sichten time delta= ',zeit_delta, dttrans
      call MPI_Allreduce(transinfo_anzahl, sumtra, 1, MPI_INT, MPI_SUM, mpi_komm_welt, iret)
      call check_err(iret)
      sumtra=sumtra/proz_anz
      !print*,meinrang,'nc_sc_sichten timestep number=',transinfo_anzahl, sumtra, n_stacks ! if(meinrang.eq.0) 
      if(transinfo_anzahl.ne.sumtra)call qerror('timestep number unclear nc_sc_sichten')
      call mpi_barrier (mpi_komm_welt, ierr)

if(meinrang.eq. 0)then ! prozess 0 only
      allocate (transinfo_zeit(transinfo_anzahl), transinfo_zuord(transinfo_anzahl), stat = istat )
      allocate (transinfo_datei(transinfo_anzahl), stat = istat )
      allocate (transinfo_stack(transinfo_anzahl), transinfo_instack(transinfo_anzahl), stat = istat )
      if(istat.ne.0) call qerror("allocate (transinfo_zeit(transinfo_anzahl) failed")
      nnt=0
      do i=1,n_stacks ! reread all stacks
         write(chari,*),i
         write(dateiname,'(2A,I4.4,3A)')trim(modellverzeichnis),'outputs_schism/schout_',meinrang,'_',trim(adjustl(chari)),'.nc' !schout_0001_1.nc
         iret = nf_open(dateiname, NF_NOWRITE, ncid)
         if(iret.ne. 0) print*,meinrang,i,' nc_sc_sichten reread 0: nf_open failed   iret=',iret
         do j=1,ndims
            iret = nf90_Inquire_Dimension(ncid, j, dname(j), dlength(j))
            call check_err(iret)
         end do ! all j Dimensions
         iret =  nf_inq_varid(ncid,'time', varid)
         call check_err(iret)
         iret = nf90_inquire_variable(ncid,varid,vname(varid),vxtype(varid),vndims(varid),dimids, nAtts)
         call check_err(iret)
         n=dlength(dimids(1))
         allocate (zeiten(n), stat = istat )
         iret = nf90_get_var(ncid, varid, zeiten)
         call check_err(iret)
         !print*,i,' nc_sc_sichten reread ',n, nnt, dimids(1), varid, iret
         do j=1,n
            transinfo_zeit(nnt+j)=zeiten(j)
            transinfo_zuord(nnt+j)=nnt+j
            transinfo_stack(nnt+j)=i
            transinfo_instack(nnt+j)=j
            write(transinfo_datei(nnt+j),'(A)')trim(adjustl(dateiname))
         end do ! all j timesteps of this stack i
         nnt=nnt+n
         deallocate(zeiten)
         iret = nf_close(ncid)
         call check_err(iret)
      end do !all i stacks
      print*,'nc_sc_sichten reread 0 timestep number=',transinfo_anzahl, nnt, n_stacks
      if(nnt.ne.transinfo_anzahl)call qerror('nc_sc_sichten reread 0 timestep number unclear ')
      !write(time_offset_string,'(A)')'2011 01 01 00 00 00' !#################
      write(time_offset_string,'(I4,x,I2,x,I2,x,I2,x,I2,x,I2)')jahr, monat, tag , stunde, minute, sekunde
      print*,'nc_sc_sichten: ', transinfo_anzahl,' Transport-Zeitschritte ab ',trim(adjustl(time_offset_string)) ! &
     !&      ,' ######## WARNING ###### start time hard coded ########'
      !read(time_offset_string,*,iostat=istat) jahr, monat, tag, stunde, minute, sekunde
      !if(istat.ne.0)call qerror('nc_sc_sichten: time_offset-Lesefehler')
      print*,"nc_sc_sichten time_offset=",tag, monat, jahr, stunde, minute, sekunde
      print*,"nc_sc_sichten time_offset_string=",trim(time_offset_string)
      call sekundenzeit(1)
      write(*,227)'nc_sichten time-offset='  &
                  ,tag,monat,jahr,stunde,minute,sekunde,zeitpunkt,referenzjahr
      time_offset=zeitpunkt !! Offset vom Referenzjahr zum netcdf-Zeitursprung

      zeitpunkt=transinfo_zeit(transinfo_zuord(1))
      call zeitsekunde()
      write(*,228)'von: ',tag,monat,jahr,stunde,minute,sekunde, zeitpunkt, trim(time_offset_string)
      zeitpunkt=transinfo_zeit(transinfo_zuord(transinfo_anzahl))
      call zeitsekunde()
      write(*,228)'bis: ',tag,monat,jahr,stunde,minute,sekunde, zeitpunkt, trim(time_offset_string)
      !print*,' transinfo_sichten rechenzeit=', rechenzeit, ' startzeitpunkt=',startzeitpunkt
      print*,'in regelmäßigen Schritten von  ',dttrans, ' Sekunden'

end if ! only prozessor 0
      call mpi_barrier (mpi_komm_welt, ierr)
      deallocate (dlength,dname, stat = istat)
      deallocate (vxtype,vndims,vname,  stat = istat )
      ncid=-333
      call mpi_barrier (mpi_komm_welt, ierr)
      !call qerror('SCHISM momentan nur bis ende nc_sc_sichten ')

  227 FORMAT (A,2x,I2.2,".",I2.2,".",I4,2x,I2.2,":",I2.2,":",I2.2," Uhr  = ",I9," sek. seit Jahresanfang ",I4)
  228 FORMAT (A,2x,I2.2,".",I2.2,".",I4,2x,I2.2,":",I2.2,":",I2.2," Uhr  = ",I9," sek. seit ",A)
      END subroutine nc_sc_sichten
!----+-----+-----+-----+-----+-----+-----+-----+-----+-----+

      subroutine holen_trans_schism(nt)
      use netcdf
      use modell                                                   
      implicit none
      include 'netcdf.inc'
      integer :: nt, nst, nin, np_p ,iret, varid
      integer :: i,j,k,l,m,n, istat
      character (len=400) :: dateiname, chari
      integer :: start3(3), count3(3)
      integer :: start2(2), count2(2)
      integer :: ndims, nVars, nGlobalAtts, unlimdimid, nAtts
      integer , allocatable , dimension (:) :: dlength
      character(256) , allocatable , dimension (:) :: dname
      integer, dimension(nf90_max_var_dims) :: dimids
      integer , allocatable , dimension (:) :: vxtype, vndims
      CHARACTER(256) , allocatable , dimension (:) :: vname
      real vel_norm, vel_dir, minwert, maxwert, tempi

      if(meinrang.eq. 0)then
         print*,'holen_trans_schism: nt,zuord=',nt,transinfo_zuord(nt)
         print*,'zeitpunkt=',transinfo_zeit(transinfo_zuord(nt)),' Zeitschritt',izeit
         print*,'datei=',trim(adjustl(transinfo_datei(transinfo_zuord(nt))))
         nst=transinfo_stack(transinfo_zuord(nt))
         nin=transinfo_instack(transinfo_zuord(nt))
         print*,'stack,instack=',nst,nin
         print*,'number_plankt_point=',number_plankt_point,' proz_anz*maxstack=',proz_anz*maxstack
      end if ! proc. 0 only
      call mpi_barrier (mpi_komm_welt, ierr)
      call MPI_Bcast(nst,1,MPI_INT,0,mpi_komm_welt,ierr)
      call MPI_Bcast(nin,1,MPI_INT,0,mpi_komm_welt,ierr)
      !print*,meinrang,' - holen_trans_schism holt: ',nst,nin,n_stacks,maxstack
      call mpi_barrier (mpi_komm_welt, ierr)

      if(nst.ne.nst_prev)then ! proceed to next stack
         if(meinrang.eq.0)print*,'holen_trans_schism proceeds to next stack ',nst,ncid
         if(ncid.ne. -333) call check_err( nf_close(ncid) ) ! close previous
         !! open nc files
         write(chari,*),nst
         write(dateiname,'(2A,I4.4,3A)')trim(modellverzeichnis),'outputs_schism/schout_',meinrang,'_',trim(adjustl(chari)),'.nc' !schout_0001_1.nc
         iret = nf_open(dateiname, NF_NOWRITE, ncid)
         if(iret.ne. 0)then ! open error
            call check_err( iret )
            write(fehler,*)' holen_trans_schism: nf_open failed, iret=',iret, " rank=",meinrang
            call qerror(fehler)
         else ! no open error
            if(meinrang.eq.0)print*,'holen_trans_schism opens: ', trim(adjustl(dateiname)), ncid
         end if ! open error
         nst_prev=nst
      end if !next stack

      !! get Dimensions
      call check_err( nf90_inquire(ncid, ndims, nVars, nGlobalAtts, unlimdimid) ) !--- overview
      allocate (dlength(ndims),dname(ndims), stat = istat)
      allocate (vxtype(nVars),vndims(nVars),vname(nVars),  stat = istat )
      do j=1,ndims
         iret= nf90_Inquire_Dimension(ncid, j, dname(j), dlength(j)) 
         call check_err(iret)
         if(iret.ne. 0) print*,meinrang,j,' holen_trans_schism nf90_Inquire_Dimension failed iret=',iret
      end do ! all dimensions

     !!!!! elev -> p
      iret = nf_inq_varid(ncid,'elev', varid)
      if(iret.ne. 0)then
         call check_err( iret )
         write(fehler,*)' holen_trans_schism: nf_inq_varid(ncid, >> elev <<  failed, iret=',iret, " rank=",meinrang
         call qerror(fehler)
      end if
      iret= nf90_inquire_variable(ncid,varid,vname(varid),vxtype(varid),vndims(varid),dimids, nAtts)
      call check_err( iret )
      if(iret.ne. 0) print*,meinrang,' holen_trans_schism nf90_inquire_variable elev failed iret=',iret
      np_p=dlength(dimids(1))
      !if(meinrang.eq. 0)print*,'holen_trans_schism elev  np_p,varid=',np_p,varid,trim(adjustl(vname(varid)))  &
      print*,'holen_trans_schism elev  np_p,varid=',np_p,varid,trim(adjustl(vname(varid)))  &
     &                        ,' dimids,dlength,maxstack=',dimids(1),trim(adjustl(dname(dimids(1)))),maxstack
      if(np_p.gt.maxstack)call qerror('dlength(dimids(1)).gt.maxstack')
      !! initialize
      do j=1,maxstack 
         var_p(j)=666.666
      end do ! all j
      if(meinrang.eq. 0)then
         do j=1,proz_anz*maxstack
            var_g(j)=777.777
         end do ! all j
      end if ! proc. 0 only
      call mpi_barrier (mpi_komm_welt, ierr)
      !! get data
      start2 = (/ 1, nin /)
      count2 = (/ np_p, 1 /) ! nodenumber first dimension
      iret=  nf90_get_var(ncid, varid, var_p(1:np_p), start2, count2 )
      call check_err(iret)
      if(iret.ne. 0) print*,meinrang,' holen_trans_schism nf90_get_var elev failed iret=',iret
      call mpi_barrier (mpi_komm_welt, ierr)
      ! gather var_p into var_g
      call MPI_Gather(var_p, maxstack, MPI_FLOAT, var_g, maxstack, MPI_FLOAT, 0, mpi_komm_welt, ierr)
      if(ierr.ne.0)then
         write(fehler,*)'holen_trans_schism MPI_Gather(var_p elev failed : ', ierr
         call qerror(fehler)
      end if 
      call mpi_barrier (mpi_komm_welt, ierr)
      !! recombine into global numbers
      if(meinrang.eq. 0)then
         do j=1,proz_anz ! all processes/ranks
            do k=1,np_sc(j) ! all nodes at this rank
               p(iplg_sc(j,k))=var_g((j-1)*maxstack+k)
            end do
         end do
         minwert=99999.9
         maxwert=-99999.9
         do j=1,knotenanzahl2D
            if(p(j).gt.maxwert)maxwert=p(j)
            if(p(j).lt.minwert)minwert=p(j)
         end do ! all j
         print*,' holen_trans_schism p minwert, maxwert=',minwert, maxwert
      end if ! proc. 0 only
      call mpi_barrier (mpi_komm_welt, ierr)

      !!!!! dahv -> u,dir
      iret= nf_inq_varid(ncid,'dahv', varid)
      if(iret.ne. 0)then
         call check_err( iret )
         write(fehler,*)' holen_trans_schism: nf_inq_varid(ncid, >> dahv <<  failed, iret=',iret, " rank=",meinrang
         call qerror(fehler)
      end if
      iret= nf90_inquire_variable(ncid,varid,vname(varid),vxtype(varid),vndims(varid),dimids, nAtts)
      call check_err( iret )
      if(iret.ne. 0)call qerror("holen_trans_schism nf90_inquire_variable dahv failed")
      np_p=dlength(dimids(2))
      if(meinrang.eq. 0)print*,'holen_trans_schism dahv varid=',varid,trim(adjustl(vname(varid)))  &
     &                        ,' dimids,dlength=',dimids(2),np_p,trim(adjustl(dname(dimids(2))))
      if(np_p.gt.maxstack)call qerror('dlength(dimids(2)).gt.maxstack')
      !! initialize
      do j=1,maxstack 
         var_p(j)=666.666
      end do ! all j
      if(meinrang.eq. 0)then
         do j=1,proz_anz*maxstack
            var_g(j)=777.777
         end do ! all j
      end if ! proc. 0 only
      call mpi_barrier (mpi_komm_welt, ierr)
      !! get data vel-x
      start3 = (/1, 1, nin /)
      count3 = (/1, np_p, 1 /) ! nodenumber second dimension
      iret=  nf90_get_var(ncid, varid, var_p(1:np_p), start3, count3 )
      call check_err(iret)
      if(iret.ne. 0) print*,meinrang,' holen_trans_schism nf90_get_var dahv1 failed iret=',iret
      call mpi_barrier (mpi_komm_welt, ierr)
      ! gather var_p into var_g
      call MPI_Gather(var_p, maxstack, MPI_FLOAT, var_g, maxstack, MPI_FLOAT, 0, mpi_komm_welt, ierr)
      if(ierr.ne.0)then
         write(fehler,*)'holen_trans_schism MPI_Gather(var_p dahv1 failed : ', ierr
         call qerror(fehler)
      end if 
      call mpi_barrier (mpi_komm_welt, ierr)
      !! recombine into global numbers
      if(meinrang.eq. 0)then
         do j=1,proz_anz ! all processes/ranks
            do k=1,np_sc(j) ! all nodes at this rank
               u(iplg_sc(j,k))=var_g((j-1)*maxstack+k)
            end do
         end do
      end if ! proc. 0 only
      call mpi_barrier (mpi_komm_welt, ierr)
      !! get data vel-y
      start3 = (/2, 1, nin /)
      count3 = (/1, np_p, 1 /) ! nodenumber second dimension
      iret=  nf90_get_var(ncid, varid, var_p(1:np_p), start3, count3 )
      call check_err(iret)
      if(iret.ne. 0) print*,meinrang,' holen_trans_schism nf90_get_var dahv2 failed iret=',iret
      call mpi_barrier (mpi_komm_welt, ierr)
      ! gather var_p into var_g
      call MPI_Gather(var_p, maxstack, MPI_FLOAT, var_g, maxstack, MPI_FLOAT, 0, mpi_komm_welt, ierr)
      if(ierr.ne.0)then
         write(fehler,*)'holen_trans_schism MPI_Gather(var_p dahv2 failed : ', ierr
         call qerror(fehler)
      end if 
      call mpi_barrier (mpi_komm_welt, ierr)
      !! recombine into global numbers
      if(meinrang.eq. 0)then
         do j=1,proz_anz ! all processes/ranks
            do k=1,np_sc(j) ! all nodes at this rank
               dir(iplg_sc(j,k))=var_g((j-1)*maxstack+k)
           end do
         end do
         !! split vel in norm and direction
         do j=1,number_plankt_point
            vel_x(j)=u(j)
            vel_y(j)=dir(j) 
            vel_norm=u(j)**2.0
            vel_dir=0.0
            if(vel_norm.gt. 0.0)vel_dir=atan(dir(j)/u(j))
            vel_norm=(vel_norm+dir(j)**2.0)**0.5
            u(j)=vel_norm
            dir(j)=vel_dir
         end do         
         minwert=99999.9
         maxwert=-99999.9
         do j=1,knotenanzahl2D
            if(u(j).gt.maxwert)maxwert=u(j)
            if(u(j).lt.minwert)minwert=u(j)
         end do ! all j
         print*,' holen_trans_schism u minwert, maxwert=',minwert, maxwert
      end if ! proc. 0 only
      call mpi_barrier (mpi_komm_welt, ierr)

      !!!!! temp -> planktonic_variable_name(1)
      if(meinrang.eq. 0)then
         do j=1,proz_anz*maxstack
            var_g(j)=777.777
         end do ! all j
      end if ! proc. 0 only
      call mpi_barrier (mpi_komm_welt, ierr)
      iret=nf_inq_varid(ncid,'temp', varid)
      if(iret.ne. 0)then !no temp?
         if(meinrang.eq. 0)then ! message only once
            call check_err( iret )
            print*,'holen_trans_schism: nf_inq_varid(ncid, >> temp <<  failed, iret=',iret, " rank=",meinrang
            print*,'initialize temp to zero '
         end if ! message only once
         do j=1,maxstack 
            var_p(j)=0.0
         end do ! all j
      else ! with temp
         iret= nf90_inquire_variable(ncid,varid,vname(varid),vxtype(varid),vndims(varid),dimids, nAtts)
         call check_err( iret )
         if(iret.ne. 0) print*,meinrang,' holen_trans_schism nf90_inquire_variable temp failed iret=',iret
         np_p=dlength(dimids(2))
         if(meinrang.eq. 0)print*,'holen_trans_schism temp varid=',varid,trim(adjustl(vname(varid)))  &
     &                           ,' dimids,dlength=',dimids(2),np_p,trim(adjustl(dname(dimids(2))))
         if(np_p.gt.maxstack)call qerror('dlength(dimids(2)).gt.maxstack')
         !! initialize
         do j=1,maxstack 
            var_p(j)=666.666
         end do ! all j
         !! get data vel-x
         start3 = (/1, 1, nin /)
         count3 = (/1, np_p, 1 /) ! nodenumber second dimension
         iret=  nf90_get_var(ncid, varid, var_p(1:np_p), start3, count3 )
         call check_err(iret)
         if(iret.ne. 0)then
            write(fehler,*)meinrang,' holen_trans_schism nf90_get_var temp failed iret=',iret
            call qerror(fehler)
         end if
      end if ! with salt
      call mpi_barrier (mpi_komm_welt, ierr)
      ! gather var_p into var_g
      call MPI_Gather(var_p, maxstack, MPI_FLOAT, var_g, maxstack, MPI_FLOAT, 0, mpi_komm_welt, ierr)
      if(ierr.ne.0)then
         write(fehler,*)'holen_trans_schism MPI_Gather(var_p temp failed : ', ierr
         call qerror(fehler)
      end if 
      call mpi_barrier (mpi_komm_welt, ierr)
      !! recombine into global numbers
      if(meinrang.eq. 0)then
         do j=1,proz_anz ! all processes/ranks
            do k=1,np_sc(j) ! all nodes at this rank
               planktonic_variable(1+(iplg_sc(j,k)-1)*number_plankt_vari)=var_g((j-1)*maxstack+k)
            end do
         end do
         minwert=999999999.9
         maxwert=-999999999.9
         do j=1,knotenanzahl2D
            tempi=planktonic_variable(1+(j-1)*number_plankt_vari)
            if(tempi.gt.maxwert)maxwert=tempi
            if(tempi.lt.minwert)minwert=tempi
         end do ! all j
         print*,' holen_trans_schism temp minwert, maxwert=',minwert, maxwert
         !!### if(minwert.lt. -1.0)call qerror('This is no ice simulation')
         !!### if(maxwert.gt. 90.0)call qerror('This is no steam simulation')
      end if ! proc. 0 only
      call mpi_barrier (mpi_komm_welt, ierr)

      !!!!! salt -> planktonic_variable_name(72)
      if(meinrang.eq. 0)then
         do j=1,proz_anz*maxstack
            var_g(j)=777.777
         end do ! all j
      end if ! proc. 0 only
      call mpi_barrier (mpi_komm_welt, ierr)
      iret=nf_inq_varid(ncid,'salt', varid)
      if(iret.ne. 0)then ! salt?
         if(meinrang.eq. 0)then ! message only once
            call check_err( iret )
            print*,'holen_trans_schism: nf_inq_varid(ncid, >> salt <<  failed, iret=',iret, " rank=",meinrang
            print*,'initialize salt to zero '
         end if ! message only once
         do j=1,maxstack 
            var_p(j)=0.0
         end do ! all j
      else ! no salt
         iret= nf90_inquire_variable(ncid,varid,vname(varid),vxtype(varid),vndims(varid),dimids, nAtts)
         call check_err( iret )
         if(iret.ne. 0) print*,meinrang,' holen_trans_schism nf90_inquire_variable salt failed iret=',iret
         np_p=dlength(dimids(2))
         if(meinrang.eq. 0)print*,'holen_trans_schism salt varid=',varid,trim(adjustl(vname(varid)))  &
     &                           ,' dimids,dlength=',dimids(2),np_p,trim(adjustl(dname(dimids(2))))
         if(np_p.gt.maxstack)call qerror('dlength(dimids(2)).gt.maxstack')
         !! initialize
         do j=1,maxstack 
            var_p(j)=666.666
         end do ! all j
         !! get data vel-x
         start3 = (/1, 1, nin /)
         count3 = (/1, np_p, 1 /) ! nodenumber second dimension
         iret=  nf90_get_var(ncid, varid, var_p(1:np_p), start3, count3 )
         call check_err(iret)
         if(iret.ne. 0)then
            write(fehler,*)meinrang,' holen_trans_schism nf90_get_var salt failed iret=',iret
            call qerror(fehler)
         end if
      end if ! with salt
      call mpi_barrier (mpi_komm_welt, ierr)
      ! gather var_p into var_g
      call MPI_Gather(var_p, maxstack, MPI_FLOAT, var_g, maxstack, MPI_FLOAT, 0, mpi_komm_welt, ierr)
      if(ierr.ne.0)then
         write(fehler,*)'holen_trans_schism MPI_Gather(var_p salt failed : ', ierr
         call qerror(fehler)
      end if 
      call mpi_barrier (mpi_komm_welt, ierr)
      !! recombine into global numbers
      if(meinrang.eq. 0)then
         do j=1,proz_anz ! all processes/ranks
            do k=1,np_sc(j) ! all nodes at this rank
               planktonic_variable(72+(iplg_sc(j,k)-1)*number_plankt_vari)=var_g((j-1)*maxstack+k)
            end do
         end do
      end if ! proc. 0 only
      call mpi_barrier (mpi_komm_welt, ierr)

      !!!!! AGE_1 -> planktonic_variable_name(74)= "       alter_arith"
      if(meinrang.eq. 0)then
         do j=1,proz_anz*maxstack
            var_g(j)=777.777
         end do ! all j
      end if ! proc. 0 only
      call mpi_barrier (mpi_komm_welt, ierr)
      iret= nf_inq_varid(ncid,'AGE_1', varid)
      if(iret.ne. 0)then ! with age1 ?
         if(meinrang.eq. 0)then ! message only once
            call check_err( iret )
            print*,'holen_trans_schism: nf_inq_varid(ncid, >> AGE_1 <<  failed, iret=',iret, " rank=",meinrang
            print*,'initialize age_1 to zero '
         end if ! message only once
         do j=1,maxstack 
            var_p(j)=0.0
         end do ! all j
      else ! no age
         iret= nf90_inquire_variable(ncid,varid,vname(varid),vxtype(varid),vndims(varid),dimids, nAtts)
         call check_err( iret )
         if(iret.ne. 0) print*,meinrang,' holen_trans_schism nf90_inquire_variable AGE_1 failed iret=',iret
         np_p=dlength(dimids(2))
         if(meinrang.eq. 0)print*,'holen_trans_schism AGE_1 varid=',varid,trim(adjustl(vname(varid)))  &
     &                           ,' dimids,dlength=',dimids(2),np_p,trim(adjustl(dname(dimids(2))))
         if(np_p.gt.maxstack)call qerror('dlength(dimids(2)).gt.maxstack')
         !! initialize
         do j=1,maxstack 
            var_p(j)=666.666
         end do ! all j
         !! get data vel-x
         start3 = (/1, 1, nin /)
         count3 = (/1, np_p, 1 /) ! nodenumber second dimension
         iret=  nf90_get_var(ncid, varid, var_p(1:np_p), start3, count3 )
         call check_err(iret)
         if(iret.ne. 0) print*,meinrang,' holen_trans_schism nf90_get_var AGE_1 failed iret=',iret
      end if ! with age
      call mpi_barrier (mpi_komm_welt, ierr)
      ! gather var_p into var_g
      call MPI_Gather(var_p, maxstack, MPI_FLOAT, var_g, maxstack, MPI_FLOAT, 0, mpi_komm_welt, ierr)
      if(ierr.ne.0)then
         write(fehler,*)'holen_trans_schism MPI_Gather(var_p AGE_1 failed : ', ierr
         call qerror(fehler)
      end if 
      call mpi_barrier (mpi_komm_welt, ierr)
      !! recombine into global numbers
      if(meinrang.eq. 0)then
         do j=1,proz_anz ! all processes/ranks
            do k=1,np_sc(j) ! all nodes at this rank
               planktonic_variable(74+(iplg_sc(j,k)-1)*number_plankt_vari)=var_g((j-1)*maxstack+k)
            end do
         end do
      end if ! proc. 0 only
      call mpi_barrier (mpi_komm_welt, ierr)


      if(izeit==1)print*,'Kantenmitten beim ersten Zeitschritt mitnehmen',meinrang
!##         allocate (ur_x(number_plankt_point),ur_y(number_plankt_point),ur_z(number_plankt_point), stat = alloc_status )
!##         if(alloc_status.ne.0)call qerror('allocate (ur_ failed')
!##
!##      !!!!! backtracked edge-mid stream-lines
!##      iret= nf_inq_varid(ncid,'btrk', varid)
!##      if(iret.ne. 0)then
!##         call check_err( iret )
!##         write(fehler,*)' holen_trans_schism: nf_inq_varid(ncid, >> btrk <<  failed, iret=',iret, " rank=",meinrang
!##         call qerror(fehler)
!##      end if
!##      iret= nf90_inquire_variable(ncid,varid,vname(varid),vxtype(varid),vndims(varid),dimids, nAtts)
!##      call check_err( iret )
!##      if(iret.ne. 0)call qerror("holen_trans_schism nf90_inquire_variable dahv failed")
!##      np_p=dlength(dimids(2))
!##      if(meinrang.eq. 0)print*,'holen_trans_schism dahv varid=',varid,trim(adjustl(vname(varid)))  &
!##     &                        ,' dimids,dlength=',dimids(2),np_p,trim(adjustl(dname(dimids(2))))
!##      if(np_p.gt.maxstack)call qerror('dlength(dimids(2)).gt.maxstack')
!##
!##      !! recombine into global numbers
!##      if(meinrang.eq. 0)then
!#####
!##      end if ! proc. 0 only
!##      call mpi_barrier (mpi_komm_welt, ierr)
!####      real , allocatable , dimension (:) :: edge_mid_x,edge_mid_y ! mid-side location of edges



      !! close, clean, return
      deallocate (dlength,dname, stat = istat)
      deallocate (vxtype,vndims,vname,  stat = istat )
      call mpi_barrier (mpi_komm_welt, ierr)

      if(meinrang.eq. 0)then
         do j=1,number_plankt_point ! all j nodes
            rb_hydraul(1+(j-1)*number_rb_hydraul)    = u(j)
            rb_hydraul(2+(j-1)*number_rb_hydraul)    = p(j)-knoten_z(j) ! Tiefe
            rb_hydraul(3+(j-1)*number_rb_hydraul)    = p(j)
            benthic_distribution(44+(j-1)*number_benth_distr)= 0.1      ! ks #### jetzt anders gelöst mit zone()%reib
            benthic_distribution(45+(j-1)*number_benth_distr)= 0.1*u(j) ! utau
         end do ! all j nodes
         print*,'### stofftransport_schism: ks and utau, only first guess ###'
      end if ! proc. 0 only
      call mpi_barrier (mpi_komm_welt, ierr)

      if(meinrang.eq. 0)then !! all boundary nodes inflow ####
         do j=1,number_plankt_point ! all j nodes
            if(knoten_rand(j).gt. 0 ) inflow(j)=.true.
         end do ! all j nodes
      end if ! proc. 0 only
      call mpi_barrier (mpi_komm_welt, ierr)

      END subroutine holen_trans_schism
!----+-----+-----+-----+-----+-----+-----+-----+-----+-----+

      subroutine sc_read_rough()
      use netcdf
      use modell                                                   
      implicit none
      integer :: n
      do n=1,number_benthic_points
         benthic_distribution(5+(n-1)*number_benth_distr)= 70.0 !! Strickler Reibungsbeiwert Kst_rau (Mannings n, here: Kst=1/n)
      end do ! alle n Knoten
      print*,'##### preliminary ##### modellg: call sc_read_rough: Strickler=70 ######'
      RETURN
      END subroutine sc_read_rough
!----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
