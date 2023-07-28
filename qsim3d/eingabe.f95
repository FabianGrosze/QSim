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
!> subroutine eingabe()
!! bewerkstelligt das Einlesen vom \ref lnk_datenmodell.
!!
!! aus Datei eingabe.f95 ; zurück zu \ref lnk_modellerstellung
subroutine eingabe() ! arbeite nur auf Prozessor 0 
   
   use modell
   use QSimDatenfelder
   use module_aparam
   
   implicit none
   
   integer                             :: i, j, n, n_cal
   integer                             :: mtag, mmonat ,mjahr
   integer, allocatable , dimension(:) :: randzaehl
   logical                             :: vorhanden, querschnitt_lesen
   real                                :: muhrzeit_stunde
   logical, allocatable , dimension(:) :: randda
   
   
   select case (hydro_trieb)
      case(1) ! casu-transinfo
         if (meinrang == 0) then
            call netz_lesen() ! Lage der Knoten, Zonen, Randnummern und Vermaschung einlesen
            ! Konzentrationen anlegen und initialisieren:
            n_cal = knotenanzahl2D
         endif ! only prozessor 0
         call mpi_barrier (mpi_komm_welt, ierr)
         call MPI_Bcast(n_cal,1,MPI_INT,0,mpi_komm_welt,ierr)
      
      case(2) ! Untrim² netCDF
         if (meinrang == 0) then 
            call read_mesh_nc()  ! Lage der Knoten und Vermaschung aus der netcdf-hydraulik-Datei einlesen
            call read_elemente_gerris()  ! Zonen und Randnummern von ELEMENTE.txt einlesen, die von Gerris erzeugt wurde
            n_cal = n_elemente
            print*,'Untrim netCDF read mesh'
         endif 
         call mpi_barrier (mpi_komm_welt, ierr)
         call MPI_Bcast(n_cal,1,MPI_INT,0,mpi_komm_welt,ierr)
      
      case(3) ! SCHISM netCDF
         ! call read_mesh_nc_sc()
         n_cal = n_elemente !!??
         n_cal = knotenanzahl2D
         if (meinrang == 0) print*,'got SCHISM netCDF mesh ##### but n_cal = knotenanzahl2D ?????????########'
      
      case default
         call qerror('Hydraulischer Antrieb unbekannt netz_lesen')
   end select
   
   ! partitioning of variable arrays
   part = n_cal / proz_anz
   n = part * proz_anz
   if (n < n_cal) part = part+1
   
   print "(*(a,i0))", 'part = ', part, ' part*proz_anz = ',part*proz_anz," meinrang = ",meinrang  &
                  ," modell_parallel() n_cal = ", n_cal
   print*, ""
   
   call mpi_barrier (mpi_komm_welt, ierr)
   call ini_planktkon0(n_cal)
   call ini_benthic0(n_cal)
   call ini_ueber(n_cal)
   
   if (meinrang == 0) then ! only prozessor 0
      call ausgabekonzentrationen_beispiel()
      if (kontrollknoten == 0) then
         print*,"special option: only writing output variable list ausgabekonzentrationen_beispiel.txt"
         call qerror('modeverz: control node = 0 - special option (error is regular exit)')
      endif
   endif ! only prozessor 0
   
   call mpi_barrier (mpi_komm_welt, ierr)
   call show_mesh()
   call ini_zeit() ! initialise time preliminary to reference-year
   call mpi_barrier (mpi_komm_welt, ierr)
   select case (hydro_trieb)
      case(1) ! casu-transinfo
         if (meinrang == 0) then ! prozess 0 only
            call transinfo_sichten()      ! Transportinformationen sichten:
         endif ! only prozessor 0
         call mpi_barrier (mpi_komm_welt, ierr)
      
      case(2) ! Untrim² netCDF
         call nc_sichten()
      
      case(3) ! SCHISM netCDF
         !!call screen_schism_nc()
      
      case default
         call qerror('Unknown hydrodynamical driver.')
   end select
   
   !#FG: reading model settings here to ensure iEros is known (required for SS from file)
   if (meinrang == 0) call read_ereigg_model() ! read time-stepping information at first
   call mpi_barrier (mpi_komm_welt, ierr)
   call MPI_Bcast(iEros,1,MPI_INT,0,mpi_komm_welt,ierr)
   call allo_trans() ! Felder für Transportinformationen und Strömungsfeld allocieren
   
   if (meinrang == 0) then ! only prozessor 0
      call modellg() ! read zone-information aus from MODELLG.3D.txt
      call modella() ! read lat. lon. at first ( zunächst nur Geographische Breiten- und Längenkoordinaten )
      call read_ereigg_model() ! read time-stepping information at first
      call ereigg_Randbedingungen_lesen() ! next read BC-development
     
      ! read global model-parameters now in module ::uebergabe_werte
      cpfad = trim(adjustl(modellverzeichnis))
      
      call read_aparam(cpfad, iwsim, icoli, ischwer)
      call extnct_lesen()
      call ausgabezeitpunkte()      ! reading points in time for output
      call ausgabekonzentrationen() ! reading output-values
      call transinfo_schritte(startzeitpunkt, startzeitpunkt+deltat) !! sollte eigentlich für beide Antriebe gleichermaßen funktionieren
      call wetter_readallo0()
      call ganglinien_lesen()
      querschneiden = querschnitt_lesen()
      if (querschneiden) then
         print*,'querschneiden'
      else
         print*,'keine Querschnitte'
      endif
      !! nachschauen, ob und zu welchen Zeitpunkten
      !! Verteilungen der Trübung/Schwebstoff und des Salzgehalts offline bereitliegen.
      call schwebstoff_salz_sichten()
      !! Daten für die Aufenthaltszeitberrechnung von Datei alter.txt lesen
      if (nur_alter) call alter_lesen()
   endif ! only prozessor 0
   
   call aparam_parallel()
   call mpi_barrier (mpi_komm_welt, ierr)
   return
   
end subroutine eingabe

!----+-----+----
!> Read model settings from file `EreigG.txt`
subroutine read_ereigg_model()
   use modell
   use qsimdatenfelder
   implicit none
   
   character(500) :: dateiname, version, model, instance
   integer        :: open_error, ion, read_error
   real           :: dt_min, tictac
   
   dateiname = trim(modellverzeichnis) // '/EREIGG.txt'
   ion = 92
   open(unit = ion, file = dateiname, status = 'old', action = 'read ', iostat = open_error)
   if (open_error /= 0) call qerror("Could not open EreigG.txt")
   rewind (ion)
   
   
   !---------------------------------------------------------------------------
   ! file header
   !---------------------------------------------------------------------------
   if ( .not. zeile(ion)) call qerror('ereigg_modell 1 read_error /= 0')
   version = trim(ctext)
   if ( .not. zeile(ion)) call qerror('ereigg_modell 2 read_error /= 0')
   model = trim(ctext)
   if ( .not. zeile(ion)) call qerror('ereigg_modell 3 read_error /= 0')
   instance = trim(ctext)
   
   
   ! --------------------------------------------------------------------------
   ! time setttings
   ! --------------------------------------------------------------------------
   
   ! --- read simulation start time ---
   ! date format: "01  01  2010  03.00"
   if ( .not. zeile(ion)) call qerror('Zeile 3 von EREIGG.txt nicht da')
   
   read(ctext, *, iostat = read_error) tag, monat, jahr, uhrzeit_stunde 
   call sekundenzeit(2)
   
   startzeitpunkt = zeitpunkt
   itags = tag
   monats = monat
   jahrs = jahr
   uhrs = uhrzeit_stunde
   rechenzeit = startzeitpunkt
   
   ! --- read simulation end time and timestep ---
   ! date format: "30  01  2010  01.00  20"
   if ( .not. zeile(ion)) call qerror('Zeile 4 von EREIGG.txt nicht da')
   read(ctext, *, iostat = read_error) tag, monat, jahr, uhrzeit_stunde, dt_min ! itage,monate,jahre,uhren,izdt
   if (read_error /= 0) call qerror('read_error in Zeile 4 von EREIGG.txt; Endzeitpunkt der Berechnung')
   call sekundenzeit(2)
   
   endzeitpunkt = zeitpunkt
   itage = tag
   monate = monat
   jahre = jahr
   uhren = uhrzeit_stunde
   
   ! --- check start and end time ----
   if (startzeitpunkt >= endzeitpunkt) then
      call qerror("Error in EreigG.txt: start time is after end time.")
   endif
   
   if (startzeitpunkt < transinfo_zeit(transinfo_zuord(1))) then
      call qerror("Start time given in EreigG.txt is before start time of hydrodynamic driver.")
   endif
   
   if (endzeitpunkt > transinfo_zeit(transinfo_zuord(transinfo_anzahl))) then
      call qerror("End time given in EreigG.txt is after end time of hydrodynamic driver.")
   endif
   
   
   !--- convert timestep into seconds ---
   deltat = int(dt_min*60)
   if (deltat <= 0) call qerror("Timestep given in EreigG.txt is negativ.")
   if (modulo(deltat, 2) == 1) call qerror("Timestep given in EreiG must be an even number.")
   
   zeitschrittanzahl = (endzeitpunkt - startzeitpunkt) / deltat
   
   ! -------------------------------------------------------------------------
   ! model settings
   ! -------------------------------------------------------------------------
   ! TODO (Schönung, august 2023):
   ! After reading these values it should be wether they are valid   
   if ( .not. zeile(ion)) call qerror('Zeile 5 von EREIGG.txt nicht da')
   read(ctext, *, iostat = read_error) imitt, ipH, idl, itemp, itracer, ieros, &
                                       ischwa, iverfahren, ilongDis, FlongDis, &
                                       iColi, ikonsS, iSchwer, iphy, iformVert,&
                                       iform_verdr
                                       
   if (read_error /= 0) call qerror("Error while reading model settings from EreigG.txt")
   close(ion)
   
   nur_temp = itemp == 1
   
   ! Schönung, November 2022
   ! Schwermetalle sind noch nicht ausreichend getestet unter QSim3D
   ! Im Codecafe am 17.November 2022 wurde beschlossen, die Schwermetalle vorläufig
   ! zu deaktivieren. 
   ! Aktuell finden Entwicklungsarbeiten zur Verknüpfung zwischen Schwermetalle, 
   ! Schwebstoffe und Erosion statt. Nach Abschluss dieser Arbeiten und erfolgreichen
   ! Tests für QSim1D und QSim3D sollen die Schwermetalle auch in QSim3D wieder
   ! aktiviert werden können.
   if (iSchwer == 1) then
      print*, 'You are trying to run a simulation with heavy metals.'
      print*, 'This is not supported by this version of QSim3D'
      call qerror('Heavy metals not supported by this version of QSim3D')
   endif
   
   ! TODO (Große)
   ! check that suspended matter ('SS') is only read from hydrodynamic 
   ! forcing ('iEros<0') when UnTRIM2 is used ('hydro_trieb=2')
   ! dirty temporary hack to use 'iEros' for this
   if (iEros < 0 .and. hydro_trieb /= 2) then
      call qerror("eingabe.f95: Can only read 'SS' (iEros < 0) from UnTRIM &
                 & hydrodynamics (hydro_trieb = 2)")
   endif
   
   if (iphy < 1 .or. iphy > 4) call qerror("Aeration flag given in EreigG.txt is invalid.")
   
  
   ! --------------------------------------------------------------------------
   ! write summary to console
   ! --------------------------------------------------------------------------
   print*
   print '(a)', repeat("-", 80)
   print '(a)', "EreigG.txt"
   print '(a)', repeat("-", 80)
   
   print '(a)','version:  ' // trim(version)
   print '(a)','model:    ' // trim(model)
   print '(a)','instance: ' // trim(instance)
   print*
   
   print '(a,i0)',   "  start       = ", startzeitpunkt
   print '(a,i0)',   "  end         = ", endzeitpunkt
   print '(a,i0)',   "  delta t     = ", deltat
   print '(a,i0)',   "  timesteps   = ", zeitschrittanzahl
   print '(a,i1)',   "  iMitt       = ", imitt
   print '(a,i1)',   "  ipH         = ", iph
   print '(a,i1)',   "  idl         = ", idl
   print '(a,i1)',   "  iTemp       = ", itemp
   print '(a,i1)',   "  iTracer     = ", itracer
   print '(a,i1)',   "  iEros       = ", ieros
   print '(a,i1)',   "  iSchwa      = ", ischwa
   print '(a,i1)',   "  iVerfahren  = ", iverfahren
   print '(a,i1)',   "  iLongDis    = ", ilongDis
   print '(a,f0.2)', "  FlongDis    = ", FlongDis
   print '(a,i1)',   "  iColi       = ", iColi
   print '(a,i1)',   "  iKonsS      = ", ikonsS
   print '(a,i1)',   "  iSchwer     = ", iSchwer
   print '(a,i1)',   "  iPhy        = ", iphy
   print '(a,i1)',   "  iFormVert   = ", iformVert
   print '(a,i1)',   "  iForm_VerdR = ", iform_VerdR
   print *
   print '(a)', "Warnings:"
   
   ! TODO (Schönung):
   ! Wouldn't it be more sensible to call qerror() in these cases?
   if (imitt == 1)   print*, "  Output of daily means is not implemented."
   if (idl == 0)     print*, "  Reading despersion coefficient from external sources is not implemented."
   if (idl == 1)     print*, "  Calculation of despersion coefficient is not implemented."
   if (itracer == 1) print*, "  Simulation of tracers is not implemented."

end subroutine read_ereigg_model
