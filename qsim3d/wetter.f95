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

!> Wetter Randbedingungen auf allen Prozessen allocieren und verteilen.
subroutine wetter_parallel()  ! called from all processes randbedingungen_parallel()
   use modell
   implicit none

   integer :: alloc_status


   call MPI_Bcast(IWETTs_T,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(IMET_T,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(mwettmax_T,1,MPI_INT,0,mpi_komm_welt,ierr)
   if (meinrang == 0)print*,'meinrang, IWETTs_T, IMET_T, mwettmax_T'
   print*, meinrang, IWETTs_T, IMET_T, mwettmax_T
   call mpi_barrier (mpi_komm_welt, ierr)


   if (meinrang /= 0) then ! alle Prozesse ausser 0
      allocate (Wetterstationskennung_T(IWETTs_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' allocate failed in wetter_parallel Wetterstationskennung_T(IWETTs_T) :'  &
                        , meinrang, alloc_status
         call qerror(fehler)
      endif
      
      allocate (iWSta_T(IWETTs_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' allocate failed in wetter_parallel iWSta :', alloc_status
         call qerror(fehler)
      endif
      
      allocate (mwetts_T(IWETTs_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' allocate failed in wetter_parallel mwetts :', alloc_status
         call qerror(fehler)
      endif
      
      allocate (itagw_T(IWETTs_T,mwettmax_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' allocate failed in wetter_parallel itagw :', alloc_status
         call qerror(fehler)
      endif
      
      allocate (monatw_T(IWETTs_T,mwettmax_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' allocate failed in wetter_parallel monatw :', alloc_status
         call qerror(fehler)
      endif
      
      allocate (jahrw_T(IWETTs_T,mwettmax_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' allocate failed in wetter_parallel jahrw :', alloc_status
         call qerror(fehler)
      endif
      
      allocate (uhrzw_T(IWETTs_T,mwettmax_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' allocate failed in wetter_parallel uhrzw_T:', alloc_status
         call qerror(fehler)
      endif
      
      allocate (zeitpunktw(IWETTs_T,mwettmax_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' allocate failed in wetter_parallel zeitpunktw:', alloc_status
         call qerror(fehler)
      endif
      
      allocate (wertw_T(IWETTs_T,7,mwettmax_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' allocate failed in wetter_parallel wertw :', alloc_status
         call qerror(fehler)
      endif
      
      
      ! allocate and initialize arrays for time-values
      allocate (glob_T(IWETTs_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' allocate failed in wetter_parallel glob_T :', alloc_status
         call qerror(fehler)
      endif
      glob_t(:) = 0.0
      
      allocate (tlmax_T(IWETTs_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' allocate failed in wetter_parallel tlmax_T :', alloc_status
         call qerror(fehler)
      endif
      tlmax_t(:) = 0.0
      
      allocate (tlmin_T(IWETTs_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' allocate failed in wetter_parallel tlmin_T :', alloc_status
         call qerror(fehler)
      endif
      tlmin_t(:) = 0.0
      
      allocate (tlmed_T(IWETTs_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' allocate failed in wetter_parallel tlmin_T :', alloc_status
         call qerror(fehler)
      endif
      tlmed_t(:) = 0.0
      
      allocate (ro_T(IWETTs_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' allocate failed in wetter_parallel ro_T :', alloc_status
         call qerror(fehler)
      endif
      ro_t(:) = 0.0
      
      allocate (wge_T(IWETTs_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' allocate failed in wetter_parallel wge_T :', alloc_status
         call qerror(fehler)
      endif
      wge_t(:) = 0.0
      
      allocate (cloud_T(IWETTs_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' allocate failed in wetter_parallel cloud_T :', alloc_status
         call qerror(fehler)
      endif
      cloud_t(:) = 0.0
      
      allocate (wtyp_T(IWETTs_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' allocate failed in wetter_parallel wtyp_T :', alloc_status
         call qerror(fehler)
      endif
      wtyp_t(:) = 0.0
      
      allocate (schwi_T(IWETTs_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' allocate failed in wetter_parallel strahlung :', alloc_status
         call qerror(fehler)
      endif
      schwi_t(:) = 0.0
   endif
   
   call MPI_Bcast(Wetterstationskennung_T,IWETTs_T, MPI_INTEGER,  0, mpi_komm_welt, ierr)
   call MPI_Bcast(iWSta_T,    IWETTs_T,             MPI_INTEGER,  0, mpi_komm_welt, ierr)
   call MPI_Bcast(mwetts_T,   IWETTs_T,             MPI_INTEGER,  0, mpi_komm_welt, ierr)
   call MPI_Bcast(itagw_T,    IWETTs_T*mwettmax_T,  MPI_INTEGER,  0, mpi_komm_welt, ierr)
   call MPI_Bcast(monatw_T,   IWETTs_T*mwettmax_T,  MPI_INTEGER,  0, mpi_komm_welt, ierr)
   call MPI_Bcast(jahrw_T,    IWETTs_T*mwettmax_T,  MPI_INTEGER,  0, mpi_komm_welt, ierr)
   call MPI_Bcast(uhrzw_T,    IWETTs_T*mwettmax_T,  MPI_FLOAT,    0, mpi_komm_welt, ierr)
   call MPI_Bcast(zeitpunktw, IWETTs_T*mwettmax_T,  MPI_INTEGER8, 0, mpi_komm_welt, ierr)
   call MPI_Bcast(wertw_T,    IWETTs_T*7*mwettmax_T,MPI_FLOAT,    0, mpi_komm_welt, ierr)
   
   return
end subroutine wetter_parallel


!> Dient der eingabe() von  Wetterdaten aus <a href="./exp/WETTER.txt" target="_blank">WETTER.txt</a>.
!!
!! In QSim-3D können die selben Dateien verwendet werden wie in QSim-1D.
!! Die Wetterdaten sind die wesentlichen Randbedingungen für die Berechnung der Wärmebilanz mittels water_temperature_wrapper_3d(),
!! deren Resultat die Temperaturverteilung im Wasserkörper ist.
subroutine wetter_readallo0()
   use modell
   use module_datetime
   implicit none

   character(300) :: file_name, dummy
   character(100) :: version_t, modname_t, erename_t
   logical        :: existing_station
   integer        :: alloc_status , open_error, io_error ,i, j
   integer        :: mwett_t, iwett_t, ixw_t
   integer        :: year, month, day, hour
   type(datetime), dimension(:,:), allocatable :: datetime_weather
   
   if (meinrang /= 0) return
   
   
   file_name = trim(modellverzeichnis) // 'WETTER.txt'
   open(unit = 86, file = file_name, status = 'old', action = 'read ', iostat = open_error)
   if (open_error /= 0) call qerror("Could not open " // trim(file_name))
   rewind (86)
   
   ! read file header
   read(86,'(a40)', iostat = io_error) version_t
   if (io_error /= 0) call qerror("Error while reading " // trim(file_name))

   read(86,'(a40)', iostat = io_error) modname_t
   if (io_error /= 0) call qerror("Error while reading " // trim(file_name))

   read(86,'(a40)', iostat = io_error) erename_t
   if (io_error /= 0) call qerror("Error while reading " // trim(file_name))

   ! iwetts_t: number of weather stations
   ! imet_t:   switch to indicate daily means
   read(86,*,iostat = io_error) iwetts_t, imet_t
   if (io_error /= 0) call qerror("Error while reading " // trim(file_name))

   
   
   allocate (wetterstationskennung_t(iwetts_t), stat = alloc_status )
   if (alloc_status /= 0) call qerror("error while allocating `wetterstationskennung_t`")
   
   allocate (iwsta_t(iwetts_t), stat = alloc_status )
   if (alloc_status /= 0) call qerror("error while allocating `iwsta_t`")
   
   allocate (mwetts_t(iwetts_t), stat = alloc_status )
   if (alloc_status /= 0) call qerror("error while allocating `mwetts_t`")
   
   ! -----------------------------------------------------------------------
   ! determine dimensions
   ! -----------------------------------------------------------------------
   do iwett_t = 1,iwetts_t 
      read(86, * ,iostat = io_error ) wetterstationskennung_t(iwett_t), mwetts_t(iwett_t)
      
      if (io_error /= 0) call qerror("error while reading " // trim(file_name))
      iwsta_t(iwett_t) = iwett_t
      
      if (mwetts_t(iwett_t) <= 0) then
         write(fehler, "(a,i0,a)") "Error in wetter.txt: Timeseries for station ", &
                                    wetterstationskennung_t(iwett_t), " is missing."
         call qerror(fehler)
      endif
      
      ! skip timeseries elements
      do mwett_t = 1,mwetts_t(iwett_t)
         read(86,*,iostat = io_error ) dummy
      enddo
   enddo
   
   
   ! --- allocate arrays ---
   mwettmax_t = maxval(mwetts_t)
   
   allocate(itagw_t(iwetts_t,mwettmax_t), stat = alloc_status)
   if (alloc_status /= 0) call qerror("error while allocating `itagw_t`")
   
   allocate (monatw_t(iwetts_t,mwettmax_t), stat = alloc_status )
   if (alloc_status /= 0) call qerror("error while allocating `monatw_t`")
   
   allocate (jahrw_t(iwetts_t,mwettmax_t), stat = alloc_status )
   if (alloc_status /= 0) call qerror("error while allocating `jahrw_t`")
   
   allocate (uhrzw_t(iwetts_t,mwettmax_t), stat = alloc_status )
   if (alloc_status /= 0) call qerror("error while allocating `uhrzw_t`")
   
   allocate (zeitpunktw(iwetts_t,mwettmax_t), stat = alloc_status )
   if (alloc_status /= 0) call qerror("error while allocating `zeitpunktw`")
   
   allocate (datetime_weather(iwetts_t,mwettmax_t), stat = alloc_status )
   if (alloc_status /= 0) call qerror("error while allocating `datetime_weather`")
   
   allocate (wertw_t(iwetts_t,7,mwettmax_t), stat = alloc_status )
   if (alloc_status /= 0) call qerror("error while allocating `wertw_t`")
   
   allocate(glob_t(iwetts_t), stat = alloc_status)
   if (alloc_status /= 0) call qerror("Error while allocating `glob_t`")
   
   allocate(tlmax_t(iwetts_t), stat = alloc_status)
   if (alloc_status /= 0) call qerror("Error while allocating `tlmax_t`")
   
   allocate(tlmin_t(iwetts_t), stat = alloc_status)
   if (alloc_status /= 0) call qerror("Error while allocating `tlmin_t`")
   
   allocate(tlmed_t(iwetts_t), stat = alloc_status)
   if (alloc_status /= 0) call qerror("Error while allocating `tlmed_t`")
   
   allocate(ro_t(iwetts_t), stat = alloc_status)
   if (alloc_status /= 0) call qerror("Error while allocating `ro_t`")
   
   allocate(wge_t(iwetts_t), stat = alloc_status)
   if (alloc_status /= 0) call qerror("Error while allocating `wge_t`")
   
   allocate(cloud_t(iwetts_t), stat = alloc_status)
   if (alloc_status /= 0) call qerror("Error while allocating `cloud_t`")
   
   allocate(wtyp_t(iwetts_t), stat = alloc_status)
   if (alloc_status /= 0) call qerror("Error while allocating `wtyp_t`")
   
   allocate(schwi_t(iwetts_t), stat = alloc_status)
   if (alloc_status /= 0) call qerror("Error while allocating `schwi_t`")
   
   
   
   ! -----------------------------------------------------------------------
   ! read timeseries
   ! -----------------------------------------------------------------------
   rewind (86)
   
   ! skip header
   read(86,*) dummy
   read(86,*) dummy
   read(86,*) dummy
   read(86,*) dummy
   
   
   do iwett_t = 1,iwetts_t
      ! block header
      read(86, *, iostat = io_error) dummy
      
      ! actual data
      do mWett_T = 1,mWetts_T(iwett_T) 
      
         ! 1: GStrahl
         ! 2: MaxTemp
         ! 3: MinTemp
         ! 4: Feuchte
         ! 5: Wind
         ! 6: Wdichte
         ! 7: Wtyp" 
         read(86,*,iostat = io_error) itagw_t(iwett_t,mwett_t),  &
                                      monatw_t(iwett_t,mwett_t), &
                                      jahrw_t(iwett_t,mwett_t),  &
                                      uhrzw_t(iwett_t,mwett_t),  &
                                     (wertw_t(iwett_t,ixw_t,mwett_t),ixw_t = 1,7)
        
         if (io_error /= 0) call qerror("Error while reading // ", trim(file_name))
      
      enddo
      
      ! check timeseries
      if (imet_t /= 1) then
         ! missing values are indicated by -99.99
         if (all(wertw_T(iwett_T,2,:) < -99.)) then
            call qerror("Error in wetter.txt: timeseries for maximum air temperature is missing.")
         endif
         
         if (all(wertw_T(iwett_T,3,:) < -99.)) then
            call qerror("Error in wetter.txt: timeseries for minimum air temperature is missing.")
         endif
      endif
      
      
      ! convert datetimes to unixtime
      do mwett_t = 1,mwetts_t(iwett_t) 
         
         day = itagw_t(iwett_t,mwett_t)
         month = monatw_t(iwett_t,mwett_t)
         year = jahrw_t(iwett_t,mwett_t)
         
         if (imet_t == 0) then
            ! if weather is given as daily means all datetimes are assumed 
            ! to be 12 o'clock
            uhrzw_t(iwett_t,mwett_t) = 12.0
            hour = 12
            minute = 0
         else
            hour = int(uhrzw_t(iwett_t,mwett_t))
            minute = int((uhrzw_t(iwett_t,mwett_t) - floor(uhrzw_t(iwett_t,mwett_t))) * 100.)
         endif
         
         
         datetime_weather(iwett_t,mwett_t) = datetime(year, month, day, hour, minute, tz = tz_qsim)
         zeitpunktw(iwett_t,mwett_t) = datetime_weather(iwett_t,mwett_t) % seconds_since_epoch()
      enddo
      
      
      
      ! check order of timeseries
      do mWett_T = 2,mWetts_T(iwett_T) 
         if (zeitpunktw(iwett_T,mwett_T) <= zeitpunktw(iwett_T,mwett_T-1)) then
            write(fehler, "(a,a,i0,a,i0,a,a)")                             & 
                     "Error in wetter.txt: timeseries are not in order.",  &
                     " line: ", mWett_T,                                   &
                     ", station: ", iwett_T,                               &  
                     ", date: ", datetime_weather(iwett_t,mwett_t)%date_string()
            call qerror(fehler)
         endif
      enddo
      
   enddo
   
   close(86) ! WETTER.txt
   
  
   ! check zone assignment
   do i = 1,zonen_anzahl
      existing_station = .false.
      do j = 1,iwetts_t
         if (zone(i)%wettstat%wetterstations_nummer == wetterstationskennung_t(j)) then
            existing_station = .true.
            zone(i)%wettstat%wetterstations_nummer = j
         endif
      enddo
      
      if (.not. existing_station) then
         write(fehler,"(2(a,i0))") "Weatherstation ", zone(i)%wettstat%wetterstations_nummer, &
                                   " is missing, but needed for zone ", zone(i)%zonen_nummer
         call qerror(fehler)
      else 
      
         print*, "der",i,"-ten zone mit der (kenn)-nummer = ",zone(i)%zonen_nummer," wurde die "      &
               , zone(i)%wettstat%wetterstations_nummer,"-te wetterstation mit der (kenn)-nummer = "  &
               , wetterstationskennung_t(zone(i)%wettstat%wetterstations_nummer), "zugeordnet."
      endif 
   enddo 
   
   ! --------------------------------------------------------------------------
   ! print summary to console
   ! --------------------------------------------------------------------------
   print*
   print*, repeat("-", 80)
   print*, "wetter.txt"
   print*, repeat("-", 80)
   
   print*, "version:  ", version_t
   print*, "model:    ", modname_t
   print*, "instance: ", erename_t
   print*
   
   print "(2x,a,i0)", "iwetts_t = ", iwetts_t
   print "(2x,a,i0)", "imet_t   = ", imet_t
   
   print*
   print "(2x,a)", "timeseries:"
   print "(4x,a)", "station   id  values  time range"
   print "(4x,a)", "-------  ---  ------  -----------------------------------------------------"
   
   do iwett_t = 1,iwetts_t
      print "(4x,i7,2x,i3,2x,i6,2x,a25,a,a25)",                         &
               iwsta_t(iwett_t),  wetterstationskennung_t(iwett_t),    &
               mwetts_t(iwett_t),                                      &
               datetime_weather(iwett_t,1) % date_string(), " - ",     &
               datetime_weather(iwett_t,mwetts_t(iwett_t)) % date_string()
   enddo
   
   
   ! zones
   print*
   print "(2x,a)", "zones assignment:"
   print "(4x,a)", "zone  zone_id  station  station_id"
   print "(4x,a)", "----  -------  -------  ----------"
   
   do i = 1,zonen_anzahl
      print "(4x,i4,2x,i7,2x,i7,2x,i10)",                                         &
               i, zone(i)%zonen_nummer, zone(i)%wettstat%wetterstations_nummer,&
               Wetterstationskennung_T(zone(i)%wettstat%wetterstations_nummer)
   enddo
   
end subroutine wetter_readallo0


!> Wetterdaten für Waermebilanz in diesem Zeitschritt  
!! runs at all processes parallel
subroutine update_weather()
   use modell
   implicit none
   integer i, nuzo, i2
   ! update weather station values *_T values
   call interpolate_weather()  ! ersetzt wettles(), interpoliert Wetterdaten für den aktuellen Zeitpunkt
   call temperl_wetter()  ! ersetzt Temperl(), berechnet Lufttemperatur und legt sie in tlmax_T ab.
   call strahlg_wetter()  ! berechnet aus der Globalstrahlung den Strahlungsanteil, der im Gewässer ankommt.
   do i2 = 1,IWETTs_T   !! Schleife über alle Wetterstationen
      if ((kontrollknoten > 0) .and. (meinrang == 0))      &
          print*,i2,meinrang," update_weather: tlmed_T,ro_T,schwi_T,wge_T,cloud_T,wtyp_T = ",  &
          tlmed_T(i2),ro_T(i2),schwi_T(i2),wge_T(i2),cloud_T(i2),wtyp_T(i2)
   enddo ! i Schleife über alle Wetterstationen
   ! transfer to nodes via transfer_quantity_p array
   do i = 1,part ! Alle Knoten auf diesem Prozessor
      iglob = i + meinrang * part
      if (iglob <= number_plankt_point) then ! Knotennummer existiert (letzter Prozess)
         i2 = zone(point_zone(iglob))%wettstat%wetterstations_nummer !! ist parallel !!!
         transfer_quantity_p(62+(i-1)*number_trans_quant) = tlmed_T(i2) ! air temp.
         transfer_quantity_p(63+(i-1)*number_trans_quant) = ro_T(i2)    ! humidity at node from weather station
         transfer_quantity_p(64+(i-1)*number_trans_quant) = schwi_T(i2) ! radiation received by water
         transfer_quantity_p(65+(i-1)*number_trans_quant) = wge_T(i2)   ! wind speed in m/s
         transfer_quantity_p(66+(i-1)*number_trans_quant) = cloud_T(i2) ! cloud cover in 1/8
         transfer_quantity_p(67+(i-1)*number_trans_quant) = wtyp_T(i2)  ! cloud reflectance(?)
         if (iglob == kontrollknoten)print*,'update_weather: ',tlmed_T(i2),schwi_T(i2),i2,iglob,i,' auf Prozess #',meinrang
      endif ! Knotennummer existiert(letzter Prozess)
   enddo ! all i nodes at this processor
   return
end subroutine update_weather



!> Dient der Ermittlung der momentanen Wetterwerte an allen Stationen.
!! Ersetzt die QSim1D Subroutine Wettles()  
!! wird von allen Prozessen aufgerufen  
!! all processes do all weather-stations
subroutine interpolate_weather()
   use modell
   implicit none
   integer     :: i, j, ipw, z1, z2
   real        :: b, ywert, w1, w2
   logical     :: found1, found2, wert_gueltig
   
   if (meinrang == 0) then 
      print '(a,i0,a)',   "* interpolate_weather(): Interpolation Weather Boundaries [time: ", zeitpunkt, "]"
      print '(*(a9,1x))', "station","glob_T","tlmax_T2","tlmin_t","ro_T","wge_T","cloud_T","wtyp_T"
   endif
   
   ! Schleife über alle Wetterstationen
   do i = 1, iwetts_t
      ! Schleife über alle 7 Wetterwerte
      do ipw = 1,7
         
         if (zeitpunkt < (zeitpunktw(i,1)-43200) .or. zeitpunkt > (zeitpunktw(i,mwetts_T(i))+43200)) then
            print*,'Zum Berechnungszeitpunkt liegen keine Daten an Wetterstation ',i,' vor.'
            print*,'zeitpunkt = ', zeitpunkt
            write(fehler,*)'zeitpunktw(i,1) = ',zeitpunktw(i,1),' zeitpunktw(i,mwetts_T(i)) = ',zeitpunktw(i,mwetts_T(i))
            call qerror(fehler)
         endif
         
         
         ywert = 0.0
         found1 = .false.
         found2 = .false.
         w1 = 0.0
         w2 = 0.0
         z1 = 0
         z2 = 0
         
         ! find closest valid datapoint before current time
         do j = 1,mwetts_T(i),1 ! alle j zeitintervalle vorwärts
            if (zeitpunktw(i,j) <= zeitpunkt) then ! bis zum aktuellen Zeitpunkt
               if (wert_gueltig(ipw,wertw_T(i,ipw,j),imet_t) ) then
                  found1 = .true.
                  w1 = wertw_T(i,ipw,j)
                  z1 = zeitpunktw(i,j)
               else
                  ! ungültige Werte merken bis gültiger gefunden
                  if (.not. found1) w1 = wertw_T(i,ipw,j)
               endif 
            endif
         enddo
         
         ! find closest valid datapoint after current time
         do j = mwetts_T(i),1,-1 ! alle j zeitintervalle rückwärts
            if (zeitpunktw(i,j) > zeitpunkt) then ! bis zum aktuellen Zeitpunkt
               if (wert_gueltig(ipw,wertw_T(i,ipw,j),imet_t)) then
                  found2 = .true.
                  w2 = wertw_T(i,ipw,j)
                  z2 = zeitpunktw(i,j)
               else
                  ! ungültige Werte merken bis gültiger gefunden
                  if (.not. found2) w2 = wertw_T(i,ipw,j)
               endif
            endif
         enddo
         
         ! in case of cloud type (0-9) convert to cloud reflectance(?)
         if (ipw == 7) then
            if (found1) call set_cloud_reflectance(nint(w1), w1)
            if (found2) call set_cloud_reflectance(nint(w2), w2)
         endif
         
         ! Interpolation
         if (found1 .and. found2) then
            b = real(zeitpunkt-z1)/real(z2-z1)
            ywert = (1.0 - b) * w1 + b * w2
         
         else if (found1 .and. .not.found2) then
            ywert = w1
         
         else if (.not.found1 .and. found2) then
            ywert = w2
         
         else if (.not.found1 .and. .not.found2) then
            ! if no valid values are found before and after and error is thrown.
            ! ipw == 7 (Wolkentyp) is an exception. For some station no such data
            ! is available, so it is accepted for timeseries to have no values at all.
            if (ipw == 7) then
               call set_cloud_reflectance(-1, Ywert)
            else
               write(fehler,*)'interpolate_weather: no valid data at weather station ',i,' for value ',ipw,"  ",w1
               call qerror(fehler)
            endif
         endif
         
         ! set interpolated values to their variable
         select case(ipw)
            case(1); glob_T(i) = ywert
            case(2); tlmax_T(i) = ywert
            case(3); tlmin_T(i) = ywert
            case(4); ro_T(i) = ywert
            case(5); wge_T(i) = ywert
            case(6); cloud_T(i) = ywert
            case(7); wtyp_T(i) = ywert
            case default
               write(fehler,*)'interpolate_weather: wrong number in variable ipw', ipw
               call qerror(fehler)
         end select
      enddo
      
      if (meinrang == 0) then
         print "(i9,1x,*(f9.2,1x))", i, glob_T(i), tlmax_T(i), tlmin_T(i), ro_T(i), wge_T(i), cloud_T(i), wtyp_T(i)
      endif
      
   enddo 
   
   if (meinrang == 0) print*, ""
end subroutine interpolate_weather


logical function wert_gueltig(ipw,wert,imet)
   implicit none
   character (300) :: fehler
   integer ipw,imet
   real wert
   wert_gueltig = .false.
   select case (ipw)
      case(1) ! glob_T(i) = ywert
         if (wert >= 0.0)wert_gueltig = .true.
      case(2) ! tlmax_T(i) = ywert
         if ((wert >= -20.0) .and. (wert <= 50.0))wert_gueltig = .true.
      case(3) ! tlmin_T(i) = ywert
         if ((wert >= -20.0) .and. (wert <= 50.0))wert_gueltig = .true.
         if (imet == 1)wert_gueltig = .true. !! not in use with timeseries data (needed for daily averages imet=0)
      case(4) ! ro_T(i) = ywert
         if (wert >= 0.0)wert_gueltig = .true.
      case(5) ! wge_T(i) = ywert
         if (wert >= 0.0)wert_gueltig = .true.
      case(6) ! cloud_T(i) = ywert
         if ((wert >= 0.0) .and. (wert <= 8.0))wert_gueltig = .true.
      case(7) ! wtyp_T(i) = ywert
         if (wert >= 0.0) wert_gueltig = .true.
         case default
         write(fehler,*)'wert_gueltig: wrong number in variable ipw',ipw
         call qerror(fehler)
   end select
end function wert_gueltig
!----+-----+----
!> berechnet Lufttemperatur uhrzeitabhängig (cosinus-Verlauf) wenn die Wetterstationsdaten,
!! als Tagesmittelwerte mit Min. und Max.-Temperatur angegeben werden.
!! Bei Stundenwerten wird die Maxtemperatur genommen. Alles andere ist ein Fehler.\n
!! Hüllroutine für Temperl() \n\n
!! wird von allen Prozessen aufgerufen\n
!! all processes do all weather-stations\n
!! \n
subroutine temperl_wetter()
   use modell
   use QSimDatenfelder
   implicit none
   integer i
   real dk, sa, su, zg, zlk, geol, geob
   integer tdj, imet
   real, dimension(20)              :: TLMAX, TLMIN
   do i = 1,IWETTs_T   !! Schleife über alle Wetterstationen
      geol = modell_geol
      geob = modell_geob
      uhrz = uhrzeit_stunde
      tdj = tagdesjahres !! wird von sasu verändert (wozu ist unklar)
      call SASU(tag, monat, geob, geol, sa, su, zg, zlk, dk, tdj)
      sonnenaufgang = sa
      sonnenuntergang = su
      IDWe(1,1) = 1
      IDWe(1,2) = 1
      anze = 1
      imet = IMET_T
      mstr = 1
      TLMAX(1) = tlmax_T(i)
      TLMIN(1) = tlmin_T(i)
      call temperl(sa,su,uhrz,templ,mstr,idwe,tlmax,tlmin,anze,imet)
      tlmed_T(i) = TEMPL(1)
      if ((kontrollknoten > 0) .and. (meinrang == 0))      &
          print*,'temperl_wetter: Station ',i,' Uhrz,TLMAX,TLMIN,TEMPL',Uhrz,TLMAX(1),TLMIN(1),TEMPL(1)
   enddo ! alle Wetterstationen i
end subroutine temperl_wetter
!----+-----+----


!> Diese Hüllroutine dient dazu, die QSim-Subroutine strahlg() punktweise aufzurufen.  
!! all processes do all weather-stations  
!!
!! Umstellung von profilweise auf wetterstationsweise (Zuordnung über Zonen)
subroutine strahlg_wetter()
   
   use module_alloc_dimensions
   use modell
   use qsimdatenfelder
   
   implicit none
   
   integer, dimension(8)      :: NRV
   real                       :: maxi,lt
   real, dimension(4)         :: ar, br
   real, dimension(1000)      :: breite
   real, dimension(14)        :: EVALT, EKRBRT, EDUFER, EVDICH, ESLEN, ESLENS
   real, dimension(1000,14)   :: VTYP
   real, dimension(1000)      :: VALTBL, EDUFBL, VALTBR, EDUFBR
   real, dimension(50)        :: SHtest
   real                       :: dk, sa, su, schwia, zg, zlk, geol, geob
   logical                    :: printi
   integer                    :: i, tdj, azStr
   
   ! loop all weather stations
   do i = 1,IWETTs_T
      ! Eingangsdaten
      glob(1) = glob_T(i)     
      uhrz = uhrzeit_stunde
      ! call zeitsekunde()    ! Rückrechnung der Uhrzeit aus dem sekunden zeitpunkt # wird schon in Zeitschleife Qsim3D gemacht
      
      ! wird von sasu verändert (wozu ist unklar)
      tdj = tagdesjahres 
      
      geol = modell_geol
      geob = modell_geob
      call sasu(tag, monat, geob, geol, sa, su, zg, zlk, dk, tdj)
      if (sa > su) then
         print*,"strahlg_wetter: tag, monat, modell_geob, modell_geol, sa, su, zg, zlk, dk, tdj" &
         ,tag, monat, modell_geob, modell_geol, sa, su, zg, zlk, dk, tdj
         call qerror("strahlg_wetter: computing daylight hours went wrong")
      endif
      
      
      
      ! --- calculate global radiation at weather station ---
      ! strahlg(...) is a subroutine written for QSim1D.
      ! To use it in QSim3D the following variables are adapted to this subroutine
      
      ! transform timestep from seconds (integer) to hours (real)
      tflie = real(deltat)/3600.0 
      
      ! nur ein Strang
      mstr = 1
      anze = 1
      azStr = 1 
      IDWe(1,1) = 1  ! für strahlg() hat strang 1, profil 1 die 1. wetterstation
      IDWe(1,2) = 1  ! für strahlg() hat strang 1, profil 1 die 1. wetterstation
      
      ! cloudiness
      cloud(1) = cloud_T(i)
      
      ! Kein Uferbewuchs
      VTYP = 0
      VALTBL(1) = 0
      EDUFBL(1) = 0
      VALTBR(1) = 0
      EDUFBR(1) = 0
      
      breite(1) = 100.0
      dk = 0.0
      
      ! Zeitschritt-Nummer während eines Tages (unbenutzt in 3D)
      ij = 1 
      
      call strahlg(glob, uhrz, sa, su, schwi, tflie, geol, tdj, geob, dk,      &
                   cloud, schwia, IMET_T, mstr, IDWe, tag, monat, VTYP, VALTBL,&
                   EDUFBL, VALTBR, EDUFBR, breite, anze, it_h,                 &
                   ij, jahrs, itage, monate, jahre, uhren, isim_end, azStr)
      schwi_T(i) = schwi(1)    ! global radiation at weather station
      
      !transfer to nodes in water_temperature_wrapper_3d: transfer_quantity_p(64+(i-1)*number_trans_quant) = schwi(1)
      if (isNaN(schwi_T(i))) then
         write(fehler,*)'strahlg_wetter station',Wetterstationskennung_T(i),' IMET_T = ',IMET_T,' isNaN(schwi_T(i))'
         call qerror(fehler)
      endif
      transfer_value_p(10) = it_h(1,1) ! Anzahl der Zeitschritte während der Hellphase (unbenutzt in 3D)
      
   enddo
   return
end subroutine strahlg_wetter

