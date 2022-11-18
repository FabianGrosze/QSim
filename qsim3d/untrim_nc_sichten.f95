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

subroutine untrim_nc_sichten()
   use netcdf
   use modell
   implicit none
   include 'netcdf.inc'
   integer, parameter :: attstrlen = 2000
   character(attstrlen) :: attstring
   character(256) :: aname
   integer i,j,k,n, alloc_status, didi, io_error, nnn, timesteps,ierr
   real delt, parttime
   real , allocatable , dimension (:) :: zeitstunde, secuz
   if (meinrang == 0) then ! prozess 0 only
      !! nMesh2_time ist leer
      ! call check_err( nf90_inq_dimid(ncid, "nMesh2_time", didi) )
      ! call check_err( nf90_Inquire_Dimension(ncid, didi, aname, timesteps) )
      ! print*,"read_mesh_nc: nMesh2_time zeitschritte=",timesteps," ",trim(aname)
      ! call check_err(  nf90_inq_varid(ncid,"nMesh2_time", didi) )
      ! allocate (secuz(timesteps), stat = alloc_status )
      ! call check_err(  nf90_get_var(ncid, didi, secuz) )
      ! print*,"netcdf nMesh2_time",timesteps," Zeitschritte von: ",secuz(1)," bis: " &
      !&      ,secuz(timesteps)," Sekunden"
      ! transinfo_anzahl bereits bekannt
      if (transinfo_anzahl < 1)call qerror('No transport info')
      allocate (zeitstunde(transinfo_anzahl), stat = alloc_status )
      if (alloc_status /= 0) call qerror("allocate (zeitstunde(transinfo_anzahl) failed")
      allocate (transinfo_zeit(transinfo_anzahl),transinfo_zuord(transinfo_anzahl), stat = alloc_status )
      if (alloc_status /= 0) call qerror("allocate (transinfo_zeit(transinfo_anzahl) failed")
      call check_err( nf90_inq_dimid(ncid,"nMesh2_data_time", didi) )
      call check_err( nf90_inq_varid(ncid,"nMesh2_data_time", didi) )
      call check_err( nf90_get_var(ncid, didi, zeitstunde) )
      print*,'netcdf nMesh2_data_time ',transinfo_anzahl,' timesteps starting: ',zeitstunde(1),' until: ' &
      ,zeitstunde(transinfo_anzahl),' h'
      !! es wird jetzt einfach mal angenommen, dass die Zeitschritte gleichmäßig sind !!
      delt = (zeitstunde(transinfo_anzahl)-zeitstunde(1))/(transinfo_anzahl-1)
      !print*,'nc_sichten: delt=',delt,(delt*3600.0),int(delt*3600.0)
      do n = 1,transinfo_anzahl ! timestep exactly equal
         transinfo_zeit(n) = (n-1)*int(delt*3600.0) + int(zeitstunde(1)*3600.0)
         transinfo_zuord(n) = n
         !nnn=n-1-(((n-1)/3)*3)
         !transinfo_zeit(n)= transinfo_zeit(n)+ 1200.0*real(nnn) !Elbe
         !transinfo_zeit(n)= transinfo_zeit(n)+ 600.0*real(nnn) !Weser
      end do ! alle Transportzeitschritte
      do i = 1,attstrlen
         attstring(i:i) = ' '
      end do
      call check_err( nf_get_att_text(ncid, didi, 'units', attstring) )
      print*,'nc_sichten, nMesh2_data_time units = ',trim(attstring)
      do i = 13,len(time_offset_string)+12
         time_offset_string(i-12:i-12) = attstring(i:i)
      end do
      !write(time_offset_string,'(A)')attstring(13 : len(trim(attstring)))
      time_offset = 0
      print*,'time_offset_string = ',trim(time_offset_string)
      !time_offset=2010-01-01 00:30:00 01:00
      time_offset_string(5:5) = ' '
      time_offset_string(8:8) = ' '
      time_offset_string(14:14) = ' '
      time_offset_string(17:17) = ' '
      read(time_offset_string,*,iostat = io_error) jahr, monat, tag, stunde, minute, sekunde
      print*," nMesh2_data_time units time_offset = ",tag, monat, jahr, stunde, minute, sekunde
      if (io_error /= 0)call qerror('nc_sichten: time_offset-Lesefehler')
      call sekundenzeit(1)
      write(*,227)'nc_sichten time-offset = '  &
                                            ,tag,monat,jahr,stunde,minute,sekunde,zeitpunkt,referenzjahr
      time_offset = zeitpunkt !! Offset vom Referenzjahr zum netcdf-Zeitursprung
      !do n=1,transinfo_anzahl ! Stunden in Sekunden
      !  transinfo_zeit(n)= transinfo_zeit(n)+time_offset
      !end do ! alle Transportzeitschritte
      do n = 1,transinfo_anzahl
         if (iabs(transinfo_zeit(n)-int(zeitstunde(n)*3600.0)) > 5) then ! wrong times
            write(fehler,*)'nc_sichten: ERROR nMesh2_data_time does not fit'
            print*,n,' times do not fit = ',zeitstunde(n),' h, ', int(zeitstunde(n)*3600.0),transinfo_zeit(n),' s'
            call qerror(fehler)
         endif
      end do ! alle Transportzeitschritte ab 2
      dttrans = transinfo_zeit(transinfo_zuord(2))-transinfo_zeit(transinfo_zuord(1))
      do n = 3,transinfo_anzahl,1
         delt = transinfo_zeit(transinfo_zuord(n))-transinfo_zeit(transinfo_zuord(n-1))
         if ((delt /= dttrans) .or. (delt < 1.0)) then
            print*,n,' = n dttrans = ',dttrans," transinfos_zeit (n) und (n-1) = "  &
                  ,transinfo_zeit(transinfo_zuord(n)),transinfo_zeit(transinfo_zuord(n-1))  &
                  ,"transinfo_zuord(n)und(n-1) = ",transinfo_zuord(n),transinfo_zuord(n-1)
            print*,'transinfo_zuord(n),transinfo_zuord(n-1) = ',transinfo_zuord(n),transinfo_zuord(n-1)
            do nnn = 1,15,1
               print*,nnn,' = n transinfo_zeit = ',transinfo_zeit(transinfo_zuord(nnn)),transinfo_zuord(nnn)
            end do
            write(fehler,*)'nc_sichten: ERROR unequal timestep = ',delt, ' should be: ', dttrans
            print*,'nMesh2_data_time = ',zeitstunde(n-1),zeitstunde(n),' h'
            call qerror(fehler)
         end if ! wrong timestep
      end do ! alle Transportzeitschritte ab 2
      print*,'all netcdf timesteps = ',dttrans,' seconds, checked.'
      print*,'nc_sichten ', transinfo_anzahl,' transport-timesteps'
      zeitpunkt = transinfo_zeit(transinfo_zuord(1))
      call zeitsekunde()
      write(*,228)'from: ',tag,monat,jahr,stunde,minute,sekunde, zeitpunkt, trim(time_offset_string)
      zeitpunkt = transinfo_zeit(transinfo_zuord(transinfo_anzahl))
      call zeitsekunde()
      write(*,228)'until: ',tag,monat,jahr,stunde,minute,sekunde, zeitpunkt, trim(time_offset_string)
      !print*,' transinfo_sichten rechenzeit=', rechenzeit, ' startzeitpunkt=',startzeitpunkt
      deallocate(zeitstunde) !,secuz)
   end if ! only prozessor 0
   call mpi_barrier (mpi_komm_welt, ierr)
   return
   227 format (A,2x,I2.2,".",I2.2,".",I4,2x,I2.2,":",I2.2,":",I2.2," o'clock = ",I9," sec. since start of year ",I4)
   228 format (A,2x,I2.2,".",I2.2,".",I4,2x,I2.2,":",I2.2,":",I2.2," o'clock = ",I9," sec. since ",A)
end subroutine untrim_nc_sichten