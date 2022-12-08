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
!> screen_schism()
!! inspects SCHISM output:\n
!! checks for time steps\n
!! checks for necessary variables
!! \n\n
!! aus Datei screen_schism_nc.f95; zurück zu \ref lnk_transport_schism
subroutine screen_schism()
   !subroutine nc_sc_sichten()
   use netcdf
   use modell
   implicit none
   include 'netcdf.inc'
   integer :: i,j,k,l,m,n, istat, ierr

   character (len = 400) :: dateiname,systemaufruf, chari
   integer :: iret, ndims, nVars, nGlobalAtts, unlimdimid, nAtts
   integer , allocatable , dimension (:) :: dlength
   character(256) , allocatable , dimension (:) :: dname
   integer, dimension(nf90_max_var_dims) :: dimids
   integer , allocatable , dimension (:) :: vxtype, vndims
   character(256) , allocatable , dimension (:) :: vname
   integer attnum, alen, varid
   logical weiter
   real , allocatable , dimension (:) :: zeiten
   real zeit_min, zeit_max
   real zeit_delta
   integer :: nnd, nnv, sumtra, nnt, sumstack
   
   if (meinrang == 0) print*,'screen_schism starts'
   
   !--- netcdf-files parallel
   !print*,nf90_max_var_dims,'nf90_max_var_dims',meinrang
   weiter = .true.
   i = 1
   n_stacks = 0
   ndims = 0
   nVars = 0
   do while (weiter)
      write(chari,*),i
      write(dateiname,'(2A,I6.6,3A)')trim(modellverzeichnis),'outputs_schism/schout_',meinrang,'_',trim(adjustl(chari)),'.nc' !schout_0001_1.nc
      systemaufruf = 'stat '//trim(adjustl(dateiname))//' >/dev/null 2>/dev/null'
      call system(trim(systemaufruf),istat)
      if (istat == 0) then
         n_stacks = i
         i = i+1
         !print*,'screen_schism found:',trim(adjustl(dateiname))
      else
         weiter = .false.
         !print*,'screen_schism not found:',trim(adjustl(dateiname))
         !if(meinrang.eq.0)print*,'screen_schism: systemaufruf',trim(adjustl(systemaufruf))
      endif
   end do
   !print*,meinrang," screen_schism,n_stacks = ",n_stacks
   
   ! check stack number
   call mpi_barrier (mpi_komm_welt, ierr)
   call mpi_reduce(n_stacks,sumstack,1,MPI_INT,mpi_sum,0,mpi_komm_welt,ierr)
   !print*,meinrang,'sumstack,proz_anz,n_stacks=',sumstack,proz_anz,n_stacks
   if (meinrang==0)then
      if((sumstack/proz_anz)/=n_stacks)then
         call qerror('screen_schism: something gone wrong with the number of schout**.nc files')
      else
         print*,'screen_schism: got ',n_stacks,' schout**.nc files for all',proz_anz,' parallel processes'
      endif
   endif
   call mpi_barrier (mpi_komm_welt, ierr)

   ! screen available files ...
   transinfo_anzahl = 0
   zeit_min = 3153600000.0
   zeit_max = -3153600000.0
   do i = 1,n_stacks
      !do i=1,10
      write(chari,*),i
      write(dateiname,'(2A,I6.6,3A)')trim(modellverzeichnis),'outputs_schism/schout_',meinrang,'_',trim(adjustl(chari)),'.nc' !schout_0001_1.nc
      !print*,"screen_schism: nf_open(dateiname,NF_NOWRITE, ncid,meinrang ",adjustl(trim(dateiname)),NF_NOWRITE, ncid,meinrang
      iret = nf_open(dateiname, NF_NOWRITE, ncid)
      if (iret /= 0) then
         call check_err(iret)
         write(fehler,*)meinrang,i,' screen_schism: nf_open failed ',dateiname,iret
         call qerror(fehler)
      !else
         !print*,"screen_schism: nf_open(ncid = ",ncid,adjustl(trim(dateiname))
      end if ! open failed
      call mpi_barrier (mpi_komm_welt, ierr)
      
      call check_err( nf90_inquire(ncid, ndims, nVars, nGlobalAtts, unlimdimid) )!--- overview
      if((meinrang == 0).and.(i==1))print*,i,'screen_schism: ',ndims,' dimensions ',nVars,' variables '
      !! get dimensions
      if (i == 1)allocate (dlength(ndims),dname(ndims), stat = istat)
      if (i == 1)allocate (vxtype(nVars),vndims(nVars),vname(nVars),  stat = istat )
      do j = 1,ndims
         iret = nf90_Inquire_Dimension(ncid, j, dname(j), dlength(j))
         call check_err(iret)
         if((meinrang ==0).and.(i==1).and.(kontrollknoten>0))then
            print*,'screen_schism: dimension  ',trim(dname(j)), dlength(j),j
         endif
         !print*,meinrang,i,j,' screen_schism: dimension  ' ,trim(adjustl(dname(j))),' wert = ', dlength(j)
      end do !all j dimension
      call mpi_barrier (mpi_komm_welt, ierr)
      
      !! get Variables
      do j = 1,nVars
         iret = nf90_inquire_variable(ncid,j,vname(j),vxtype(j),vndims(j),dimids, nAtts)
         call check_err(iret)
         if((meinrang ==0).and.(i==1).and.(kontrollknoten>0))then
            print*,'screen_schism: variable  ' ,trim(vname(j)),vxtype(j),j
         endif
      end do ! Variable j
      call mpi_barrier (mpi_komm_welt, ierr)

      !! time-steps
      iret = nf_inq_varid(ncid,'time', varid)
      if (iret /= 0) then
         call check_err( iret )
         write(fehler,*)'screen_schism: nf_inq_varid(ncid, > > time <  < failed, iret = ',iret, " rank = ",meinrang
         call qerror(fehler)
      end if
      iret = nf90_inquire_variable(ncid,varid,vname(varid),vxtype(varid),vndims(varid),dimids, nAtts)
      call check_err(iret)
      n = dlength(dimids(1))
      if((meinrang ==0).and.(i==1).and.(kontrollknoten>0))then
         print*,meinrang,'screen_schism: time varid=',varid,' dlength=',n,iret,ncid
      end if
      call mpi_barrier (mpi_komm_welt, ierr)

      if (n > 0) then
         transinfo_anzahl = transinfo_anzahl+n ! total number of timesteps
         allocate (zeiten(n), stat = istat )
         iret = nf90_get_var(ncid, varid, zeiten)
         call check_err(iret)
         if (iret /= 0) then
            print*,meinrang,i,' screen_schism: nf90_get_var  varid = ',varid,iret,ncid
            call qerror('screen_schism: nf90_get_var failed')
         endif !
         if ( zeit_min >= zeiten(1) ) zeit_min = zeiten(1)
         if ( zeit_max <= zeiten(n) ) zeit_max = zeiten(n)
         if (n > 1) then
            zeit_delta = zeiten(2)-zeiten(1)
         end if ! more than one timestep
         deallocate(zeiten)
      end if ! dlength ok
      call mpi_barrier (mpi_komm_welt, ierr)
      
      !! checking necessary variables
      iret = nf_inq_varid(ncid,'elev', varid)
      if (iret /= 0) then
         if (meinrang == 0) print*,'screen_schism: water level elevation needed by QSim , param.nml: iof_hydro(1) = 1'
         call check_err( iret )
         write(fehler,*)'screen_schism: nf_inq_varid(ncid, > > elev <  < failed, iret = ',iret, " rank = ",meinrang
         call qerror(fehler)
      end if
      call check_err( nf90_inquire_variable(ncid,varid,vname(varid),vxtype(varid),vndims(varid),dimids, nAtts) )
      n = dlength(dimids(1))
      !print*,'screen_schism: node number in stack',meinrang,' =',n
      call mpi_barrier (mpi_komm_welt, ierr)!#!
      if(meinrang==0)then
         print*,i,'-th stack; screen_schism Zeiten,delta',zeit_min, zeit_max,zeit_delta  &
               ,' transinfo_anzahl=',transinfo_anzahl
      endif

      !iret = nf_inq_varid(ncid,'dahv', varid)
      !if (iret /= 0) then
      !   if (meinrang == 0) print*,'screen_schism: node veocities needed by QSim, param.nml: iof_hydro(16) = 1'
      !   call check_err( iret )
      !   write(fehler,*)'screen_schism: nf_inq_varid(ncid, > > dahv <  < failed, iret = ',iret, " rank = ",meinrang
      !   call qerror(fehler)
      !end if
      !iret = nf_inq_varid(ncid,'hvel_side', varid)
      !if (iret /= 0) then
      !   if (meinrang == 0) print*,'screen_schism: side velocities needed by QSim, param.nml: iof_hydro(26) = 1'
      !   call check_err( iret )
      !   write(fehler,*)'screen_schism: nf_inq_varid(ncid, > > hvel_side <  < failed, iret = ',iret, " rank = ",meinrang
      !   call qerror(fehler)
      !end if
      call check_err( nf_close(ncid) )
   end do !all i stacks
   call mpi_barrier (mpi_komm_welt, ierr)
   
   ! get number of timesteps
   if (transinfo_anzahl < 1)call qerror('screen_schism: No transport info')
   !print*,meinrang,' screen_schism Zeit  von:',zeit_min,' bis: ',zeit_max,' zeit_delta=',zeit_delta
   call MPI_Allreduce(zeit_delta, dttrans, 1, MPI_FLOAT, MPI_SUM, mpi_komm_welt, iret)
   call check_err(iret)
   !print*,meinrang,' screen_schism: zeit_delta, dttrans= ',zeit_delta, dttrans
   dttrans = dttrans/real(proz_anz)
   if (int(zeit_delta) /= int(dttrans))call qerror('timestep length unclear screen_schism')
   if (meinrang == 0)print*,meinrang,'screen_schism time delta = ',zeit_delta, dttrans
   call MPI_Allreduce(transinfo_anzahl, sumtra, 1, MPI_INT, MPI_SUM, mpi_komm_welt, iret)
   call check_err(iret)
   sumtra = sumtra/proz_anz
   !if(meinrang==0)print*,'screen_schism timestep number=',transinfo_anzahl, sumtra, n_stacks ! if(meinrang.eq.0)
   if (transinfo_anzahl /= sumtra)call qerror('timestep number unclear screen_schism')
   call mpi_barrier (mpi_komm_welt, ierr)
   allocate (transinfo_zeit(transinfo_anzahl))
   allocate (transinfo_zuord(transinfo_anzahl), stat = istat )
   if (istat /= 0) call qerror("allocate (transinfo_zuord(transinfo_anzahl) failed")
   allocate (transinfo_datei(transinfo_anzahl), stat = istat )
   allocate (transinfo_stack(transinfo_anzahl), transinfo_instack(transinfo_anzahl), stat = istat )
   if (istat /= 0) call qerror("allocate (transinfo_zeit(transinfo_anzahl) failed")

   ! get time steps
   if (meinrang == 0) then ! prozess 0 only
      nnt = 0
      do i = 1,n_stacks ! reread all stacks
         write(chari,*),i
         write(dateiname,'(2A,I6.6,3A)')trim(modellverzeichnis),'outputs_schism/schout_',meinrang,'_',trim(adjustl(chari)),'.nc' !schout_0001_1.nc
         iret = nf_open(dateiname, NF_NOWRITE, ncid)
         if (iret /= 0) print*,meinrang,i,' screen_schism reread 0: nf_open failed   iret = ',iret
         do j = 1,ndims
            iret = nf90_Inquire_Dimension(ncid, j, dname(j), dlength(j))
            call check_err(iret)
         end do ! all j Dimensions
         iret = nf_inq_varid(ncid,'time', varid)
         call check_err(iret)
         iret = nf90_inquire_variable(ncid,varid,vname(varid),vxtype(varid),vndims(varid),dimids, nAtts)
         call check_err(iret)
         n = dlength(dimids(1))
         allocate (zeiten(n), stat = istat )
         iret = nf90_get_var(ncid, varid, zeiten)
         call check_err(iret)
         !print*,i,' screen_schism reread ',n, nnt, dimids(1), varid, iret
         do j = 1,n
            transinfo_zeit(nnt+j) = zeiten(j)
            transinfo_zuord(nnt+j) = nnt+j
            transinfo_stack(nnt+j) = i
            transinfo_instack(nnt+j) = j
            write(transinfo_datei(nnt+j),'(A)')trim(adjustl(dateiname))
         end do ! all j timesteps of this stack i
         nnt = nnt+n
         deallocate(zeiten)
         iret = nf_close(ncid)
         call check_err(iret)
      end do !all i stacks
      print*,'screen_schism reread 0 timestep number = ',transinfo_anzahl, nnt, n_stacks
      if (nnt /= transinfo_anzahl)call qerror('screen_schism reread 0 timestep number unclear ')
      
      ! check timesteps
      do j = 2,transinfo_anzahl
         if(transinfo_zeit(transinfo_zuord(j))-transinfo_zeit(transinfo_zuord(j-1))/=dttrans)then
            call qerror('screen_schism uneven timesteping')
         endif
      end do

      ! starting time obtained from local_to_global_000000 Header: section by read_mesh_schism
      write(time_offset_string,'(I4,x,I2,x,I2,x,I2,x,I2,x,I2)')jahr, monat, tag , stunde, minute, sekunde
      call sekundenzeit(1)
      write(*,227)"screen_schism: time-offset = ",tag,monat,jahr,stunde,minute,sekunde,zeitpunkt,referenzjahr
      time_offset = zeitpunkt
  227 format (A,2x,I2.2,".",I2.2,".",I4,2x,I2.2,":",I2.2,":",I2.2," Uhr = ",I9," sek. seit Jahresanfang ",I4)
      
      ! time span output
      zeitpunkt = transinfo_zeit(transinfo_zuord(1))
      call zeitsekunde()
      write(*,228)'von: ',tag,monat,jahr,stunde,minute,sekunde, zeitpunkt, trim(time_offset_string)
      zeitpunkt = transinfo_zeit(transinfo_zuord(transinfo_anzahl))
      call zeitsekunde()
      write(*,228)'bis: ',tag,monat,jahr,stunde,minute,sekunde, zeitpunkt, trim(time_offset_string)
      !print*,' transinfo_sichten rechenzeit=', rechenzeit, ' startzeitpunkt=',startzeitpunkt
      print*,'in regelmäßigen Schritten von  ',dttrans, ' Sekunden'
  228 format (A,2x,I2.2,".",I2.2,".",I4,2x,I2.2,":",I2.2,":",I2.2," Uhr = ",I9," sek. seit ",A)
  
   end if ! only prozessor 0
   
   call MPI_Bcast(transinfo_zeit,transinfo_anzahl,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(transinfo_zuord,transinfo_anzahl,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(transinfo_stack,transinfo_anzahl,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(transinfo_instack,transinfo_anzahl,MPI_INT,0,mpi_komm_welt,ierr)
   call mpi_barrier (mpi_komm_welt, ierr)
   deallocate (dlength,dname, stat = istat)
   deallocate (vxtype,vndims,vname,  stat = istat )
   ncid = -333
   call mpi_barrier (mpi_komm_welt, ierr)
   if (meinrang == 0) print*,"screen_schism finished"
   return
end subroutine screen_schism
