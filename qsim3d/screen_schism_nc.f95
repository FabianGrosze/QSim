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
subroutine screen_schism_nc()
   ! TODO (Schönung, August 2023)
   ! Schism routines must be adapted to unixtime
   ! Will be done when merging schism-branch
   call qerror("schism is not implemented")

end subroutine screen_schism_nc()

!> screen_schism_nc()
!! inspects SCHISM output:\n
!! checks for time steps\n
!! checks for necessary variables
!! \n\n
!! aus Datei screen_schism_nc.f95; zurück zu \ref lnk_transport_schism
!subroutine screen_schism_nc()
!   use module_datetime
!   use netcdf
!   use modell
!   include 'netcdf.inc'
!   
!   implicit none
!   
!   integer :: i,j,k,l,m,n, istat
!   character (len = 400) :: dateiname,systemaufruf, chari
!   integer :: iret, ndims, nVars, nGlobalAtts, unlimdimid, nAtts
!   integer , allocatable , dimension (:) :: dlength
!   character(256) , allocatable , dimension (:) :: dname
!   integer, dimension(nf90_max_var_dims) :: dimids
!   integer , allocatable , dimension (:) :: vxtype, vndims
!   character(256) , allocatable , dimension (:) :: vname
!   integer attnum, alen, varid
!   logical weiter
!   real , allocatable , dimension (:) :: zeiten
!   real    :: zeit_min, zeit_max, zeit_delta
!   integer :: nnd, nnv, sumtra, nnt
!   type(datetime) :: datetime_output
!   
!   
!   if (meinrang == 0) print*,'screen_schism_nc starts'
!   
!   !--- netcdf-files parallel
!   !print*,nf90_max_var_dims,'nf90_max_var_dims',meinrang
!   weiter = .true.
!   i = 1
!   n_stacks = 0
!   ndims = 0
!   nVars = 0
!   do while (weiter)
!      write(chari,*),i
!      write(dateiname,'(2A,I4.4,3A)')trim(modellverzeichnis),'outputs_schism/schout_',meinrang,'_',trim(adjustl(chari)),'.nc' !schout_0001_1.nc
!      systemaufruf = 'stat '//trim(adjustl(dateiname))//' >/dev/null 2>/dev/null'
!      call system(trim(systemaufruf),istat)
!      if (istat == 0) then
!         n_stacks = i
!         i = i+1
!      else
!         weiter = .false.
!         !if(meinrang.eq.0)print*,'screen_schism_nc: systemaufruf',trim(adjustl(systemaufruf))
!      endif
!   enddo
!   if (meinrang == 0)print*,"screen_schism_nc,n_stacks = ",n_stacks
!   call mpi_barrier (mpi_komm_welt, ierr)
!   transinfo_anzahl = 0
!   zeit_min = 3153600000.0
!   zeit_max = -3153600000.0
!   do i = 1,n_stacks
!      !do i=1,10
!      write(chari,*),i
!      write(dateiname,'(2A,I4.4,3A)')trim(modellverzeichnis),'outputs_schism/schout_',meinrang,'_',trim(adjustl(chari)),'.nc' !schout_0001_1.nc
!      print*,"screen_schism_nc: nf_open(dateiname,NF_NOWRITE, ncid,meinrang ",adjustl(trim(dateiname)),NF_NOWRITE, ncid,meinrang
!      iret = nf_open(dateiname, NF_NOWRITE, ncid)
!      if (iret /= 0) then
!         call check_err(iret)
!         write(fehler,*)meinrang,i,' screen_schism_nc: nf_open failed ',dateiname,iret
!         call qerror(fehler)
!      else
!         print*,"screen_schism_nc: nf_open(ncid = ",ncid
!      endif ! open failed
!      call check_err( nf90_inquire(ncid, ndims, nVars, nGlobalAtts, unlimdimid) )!--- overview
!      !! dimensions
!      if (i == 1)allocate (dlength(ndims),dname(ndims), stat = istat)
!      if (i == 1)allocate (vxtype(nVars),vndims(nVars),vname(nVars),  stat = istat )
!      do j = 1,ndims
!         iret = nf90_Inquire_Dimension(ncid, j, dname(j), dlength(j))
!         call check_err(iret)
!         if ((meinrang == 2) .and. (i == 1))  &
!             print*,meinrang,i,j,' screen_schism_nc: dimension  ' ,trim(adjustl(dname(j))),' wert = ', dlength(j)
!      enddo !all j dimension
!      !! Variables
!      do j = 1,nVars
!         iret = nf90_inquire_variable(ncid,j,vname(j),vxtype(j),vndims(j),dimids, nAtts)
!         call check_err(iret)
!         if ((meinrang == 0) .and. (i == 1))  &
!             print*, j,'-th variable; name = ' ,trim(adjustl(vname(j))),' , vxtype = ',vxtype(j)
!         do k = 1,vndims(j)
!            if ((meinrang == 0) .and. (i == 1))  &
!                print*, k,'-th dimension; name = ',trim(adjustl(dname(dimids(k)))),' dim.length = '  &
!                ,dlength(dimids(k)),' dimid = ' ,dimids(k)
!         enddo !k Dimensionen von Variable j
!         !print*,"Attribute : "
!         !call print_attributes(j, nAtts)
!      enddo ! Variable j
!      !! time-steps
!      iret = nf_inq_varid(ncid,'time', varid)
!      if (iret /= 0) then
!         call check_err( iret )
!         write(fehler,*)'screen_schism_nc: nf_inq_varid(ncid, > > time <  < failed, iret = ',iret, " rank = ",meinrang
!         call qerror(fehler)
!      endif
!      iret = nf90_inquire_variable(ncid,varid,vname(varid),vxtype(varid),vndims(varid),dimids, nAtts)
!      call check_err(iret)
!      print*,meinrang,i,' read_mesh_nc_sc: nf90_inquire_variable   varid = ',varid,iret,ncid
!      n = dlength(dimids(1))
!      if (n > 0) then
!         transinfo_anzahl = transinfo_anzahl+n
!         print*,i,'read_mesh_nc_sc: transinfo_anzahl = ',transinfo_anzahl,n
!         allocate (zeiten(n), stat = istat )
!         iret = nf90_get_var(ncid, varid, zeiten)
!         call check_err(iret)
!         if (iret /= 0) then
!            print*,meinrang,i,' screen_schism_nc: nf90_get_var  varid = ',varid,iret,ncid
!            call qerror('screen_schism_nc: nf90_get_var failed')
!         endif !
!         if ( zeit_min >= zeiten(1) ) zeit_min = zeiten(1)
!         if ( zeit_max <= zeiten(n) ) zeit_max = zeiten(n)
!         if (n > 1) then
!            zeit_delta = zeiten(2)-zeiten(1)
!         endif ! more than one timestep
!         print*,meinrang,' screen_schism_nc Zeit stack ',i,' zeiten 1,2 = ',zeiten(1), zeiten(2),zeit_delta
!         deallocate(zeiten)
!      endif ! dlength ok
!      !! checking necessary variables
!      iret = nf_inq_varid(ncid,'elev', varid)
!      if (iret /= 0) then
!         if (meinrang == 0) print*,'screen_schism_nc: water level elevation needed by QSim , param.nml: iof_hydro(1) = 1'
!         call check_err( iret )
!         write(fehler,*)'screen_schism_nc: nf_inq_varid(ncid, > > elev <  < failed, iret = ',iret, " rank = ",meinrang
!         call qerror(fehler)
!      endif
!      iret = nf_inq_varid(ncid,'dahv', varid)
!      if (iret /= 0) then
!         if (meinrang == 0) print*,'screen_schism_nc: node veocities needed by QSim, param.nml: iof_hydro(16) = 1'
!         call check_err( iret )
!         write(fehler,*)'screen_schism_nc: nf_inq_varid(ncid, > > dahv <  < failed, iret = ',iret, " rank = ",meinrang
!         call qerror(fehler)
!      endif
!      iret = nf_inq_varid(ncid,'hvel_side', varid)
!      if (iret /= 0) then
!         if (meinrang == 0) print*,'screen_schism_nc: side velocities needed by QSim, param.nml: iof_hydro(26) = 1'
!         call check_err( iret )
!         write(fehler,*)'screen_schism_nc: nf_inq_varid(ncid, > > hvel_side <  < failed, iret = ',iret, " rank = ",meinrang
!         call qerror(fehler)
!      endif
!      call check_err( nf_close(ncid) )
!   enddo !all i stacks
!   if (transinfo_anzahl < 1)call qerror('No transport info')
!   !print*,meinrang,' screen_schism_nc Zeit  von:',zeit_min,' bis: ',zeit_max,' zeit_delta=',zeit_delta
!   call MPI_Allreduce(zeit_delta, dttrans, 1, MPI_FLOAT, MPI_SUM, mpi_komm_welt, iret)
!   call check_err(iret)
!   !print*,meinrang,' screen_schism_nc: zeit_delta, dttrans= ',zeit_delta, dttrans
!   dttrans = dttrans/real(proz_anz)
!   if (int(zeit_delta) /= int(dttrans))call qerror('timestep length unclear screen_schism_nc')
!   if (meinrang == 0)print*,meinrang,'screen_schism_nc time delta = ',zeit_delta, dttrans
!   call MPI_Allreduce(transinfo_anzahl, sumtra, 1, MPI_INT, MPI_SUM, mpi_komm_welt, iret)
!   call check_err(iret)
!   sumtra = sumtra/proz_anz
!   !print*,meinrang,'screen_schism_nc timestep number=',transinfo_anzahl, sumtra, n_stacks ! if(meinrang.eq.0)
!   if (transinfo_anzahl /= sumtra)call qerror('timestep number unclear screen_schism_nc')
!   call mpi_barrier (mpi_komm_welt, ierr)
!   if (meinrang == 0) then ! prozess 0 only
!      allocate (transinfo_zeit(transinfo_anzahl), transinfo_zuord(transinfo_anzahl), stat = istat )
!      allocate (transinfo_datei(transinfo_anzahl), stat = istat )
!      allocate (transinfo_stack(transinfo_anzahl), transinfo_instack(transinfo_anzahl), stat = istat )
!      if (istat /= 0) call qerror("allocate (transinfo_zeit(transinfo_anzahl) failed")
!      nnt = 0
!      do i = 1,n_stacks ! reread all stacks
!         write(chari,*),i
!         write(dateiname,'(2A,I4.4,3A)')trim(modellverzeichnis),'outputs_schism/schout_',meinrang,'_',trim(adjustl(chari)),'.nc' !schout_0001_1.nc
!         iret = nf_open(dateiname, NF_NOWRITE, ncid)
!         if (iret /= 0) print*,meinrang,i,' screen_schism_nc reread 0: nf_open failed   iret = ',iret
!         do j = 1,ndims
!            iret = nf90_Inquire_Dimension(ncid, j, dname(j), dlength(j))
!            call check_err(iret)
!         enddo ! all j Dimensions
!         iret = nf_inq_varid(ncid,'time', varid)
!         call check_err(iret)
!         iret = nf90_inquire_variable(ncid,varid,vname(varid),vxtype(varid),vndims(varid),dimids, nAtts)
!         call check_err(iret)
!         n = dlength(dimids(1))
!         allocate (zeiten(n), stat = istat )
!         iret = nf90_get_var(ncid, varid, zeiten)
!         call check_err(iret)
!         !print*,i,' screen_schism_nc reread ',n, nnt, dimids(1), varid, iret
!         do j = 1,n
!            transinfo_zeit(nnt+j) = zeiten(j)
!            transinfo_zuord(nnt+j) = nnt+j
!            transinfo_stack(nnt+j) = i
!            transinfo_instack(nnt+j) = j
!            write(transinfo_datei(nnt+j),'(A)')trim(adjustl(dateiname))
!         enddo ! all j timesteps of this stack i
!         nnt = nnt+n
!         deallocate(zeiten)
!         iret = nf_close(ncid)
!         call check_err(iret)
!      enddo !all i stacks
!      
!      if (nnt /= transinfo_anzahl)call qerror('screen_schism_nc reread 0 timestep number unclear ')
!      
!      
!      
!      
!      call sekundenzeit(1)
!      write(*,227)"screen_schism_nc: time-offset = "  &
!                                                   ,tag,monat,jahr,stunde,minute,sekunde,zeitpunkt,referenzjahr
!      time_offset = zeitpunkt !! Offset vom Referenzjahr zum netcdf-Zeitursprung
!      zeitpunkt = transinfo_zeit(transinfo_zuord(1))
!      call zeitsekunde()
!      write(*,228)'von: ',tag,monat,jahr,stunde,minute,sekunde, zeitpunkt
!      zeitpunkt = transinfo_zeit(transinfo_zuord(transinfo_anzahl))
!      call zeitsekunde()
!      write(*,228)'bis: ',tag,monat,jahr,stunde,minute,sekunde, zeitpunkt
!      !print*,' transinfo_sichten rechenzeit=', rechenzeit, ' startzeitpunkt=',startzeitpunkt
!      print*,'in regelmäßigen Schritten von  ',dttrans, ' Sekunden'
!   endif ! only prozessor 0
!   call mpi_barrier (mpi_komm_welt, ierr)
!   deallocate (dlength,dname, stat = istat)
!   deallocate (vxtype,vndims,vname,  stat = istat )
!   ncid = -333
!   call mpi_barrier (mpi_komm_welt, ierr)
!   227 format (A,2x,I2.2,".",I2.2,".",I4,2x,I2.2,":",I2.2,":",I2.2," Uhr = ",I9," sek. seit Jahresanfang ",I4)
!   228 format (A,2x,I2.2,".",I2.2,".",I4,2x,I2.2,":",I2.2,":",I2.2," Uhr = ",I9," sek. seit ",A)
!   if (meinrang == 0) print*,"screen_schism_nc finished"
!   return
!end subroutine screen_schism_nc
