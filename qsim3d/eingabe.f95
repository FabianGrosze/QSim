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
!! bewerkstelligt das Einlesen vom \ref lnk_datenmodell. \n
!! aus Datei eingabe.f95 ; zurück zu \ref lnk_modellerstellung

subroutine eingabe()   !!!! arbeite nur auf Prozessor 0 !!!!
   !
   use modell
   use QSimDatenfelder
   use aparam
   
   implicit none
   integer :: i, j, n, n_cal
   logical :: vorhanden, only, querschnitt_lesen
   integer mtag, mmonat ,mjahr
   real :: muhrzeit_stunde
   integer , allocatable , dimension (:) :: randzaehl
   logical , allocatable , dimension (:) :: randda
   !print*,'eingabe() startet'
   only = .false.
   select case (hydro_trieb)
      case(1) ! casu-transinfo
         if (meinrang == 0) then ! prozess 0 only
            call netz_lesen() ! Lage der Knoten, Zonen, Randnummern und Vermaschung einlesen
            ! Konzentrationen anlegen und initialisieren:
            n_cal = knotenanzahl2D
         end if ! only prozessor 0
         call mpi_barrier (mpi_komm_welt, ierr)
         call MPI_Bcast(n_cal,1,MPI_INT,0,mpi_komm_welt,ierr)
      case(2) ! Untrim² netCDF
         if (meinrang == 0) then ! prozess 0 only
            call read_mesh_nc()  ! Lage der Knoten und Vermaschung aus der netcdf-hydraulik-Datei einlesen
            call read_elemente_gerris()  ! Zonen und Randnummern von ELEMENTE.txt einlesen, die von Gerris erzeugt wurde
            n_cal = n_elemente
            print*,'Untrim netCDF read mesh'
         end if ! only prozessor 0
         call mpi_barrier (mpi_komm_welt, ierr)
         call MPI_Bcast(n_cal,1,MPI_INT,0,mpi_komm_welt,ierr)
      case(3) ! SCHISM netCDF
         !!!### call read_mesh_nc_sc()
         n_cal = n_elemente !!??
         n_cal = knotenanzahl2D
         if (meinrang == 0)print*,'got SCHISM netCDF mesh ##### but n_cal = knotenanzahl2D ?????????########'
         case default
         call qerror('Hydraulischer Antrieb unbekannt netz_lesen')
   end select
   ! partitioning of variable arrays
   part = n_cal/proz_anz
   n = part*proz_anz
   !print*,'ini_par knotenanzahl=', nk,' proz_anz=', proz_anz, ' part=', part, ' part*proz_anz=', n
   if (n < n_cal)part = part+1
   print*,'part = ', part, ' part*proz_anz = ',part*proz_anz," meinrang = ",meinrang  &
                  ," modell_parallel() n_cal = ", n_cal
   call mpi_barrier (mpi_komm_welt, ierr)
   call ini_planktkon0(n_cal)
   call ini_benthic0(n_cal)
   call ini_ueber(n_cal)
   
   if (meinrang == 0) then ! only prozessor 0
      call ausgabekonzentrationen_beispiel()
      if (kontrollknoten == 0) then
         print*,"### special option #### only writing output variable list ausgabekonzentrationen_beispiel.txt"
         call qerror('modeverz: control node = 0  ### special option #### (error is regular exit)')
      end if
   end if ! only prozessor 0
   call mpi_barrier (mpi_komm_welt, ierr)
   call show_mesh()
   call ini_zeit() ! initialise time preliminary to reference-year
   call mpi_barrier (mpi_komm_welt, ierr)
   select case (hydro_trieb)
      case(1) ! casu-transinfo
         if (meinrang == 0) then ! prozess 0 only
            call transinfo_sichten()      ! Transportinformationen sichten:
         end if ! only prozessor 0
         call mpi_barrier (mpi_komm_welt, ierr)
      case(2) ! Untrim² netCDF
         call nc_sichten()
      case(3) ! SCHISM netCDF
         !!call screen_schism_nc()
         case default
         call qerror('Hydraulischer Antrieb unbekannt; sichten')
   end select
   
   !#FG: reading model settings here to ensure iEros is known (required for SS from file)
   if (meinrang == 0) call ereigg_modell() ! read time-stepping information at first
   call mpi_barrier (mpi_komm_welt, ierr)
   call MPI_Bcast(iEros,1,MPI_INT,0,mpi_komm_welt,ierr)
   call allo_trans() ! Felder für Transportinformationen und Strömungsfeld allocieren
   
   if (meinrang == 0) then ! only prozessor 0
      call modellg() ! read zone-information aus from MODELLG.3D.txt
      call modella() ! read lat. lon. at first ( zunächst nur Geographische Breiten- und Längenkoordinaten )
      call ereigg_modell() ! read time-stepping information at first
      call ereigg_Randbedingungen_lesen() ! next read BC-development
      !     read global model-parameters now in module ::uebergabe_werte
      write(cpfad,*,iostat = ifehl)trim(adjustl(modellverzeichnis))
      if (ifehl /= 0)call qerror('eingabe: write(cpfad went wrong')
      call aparam_lesen(cpfad,iwsim,icoli,ieros,ischwer,ifehl)
      if (ifehl /= 0) then
         print*,'cpfad,iwsim,icoli,ieros = ',trim(cpfad),iwsim,icoli,ieros
         write(fehler,*)'eingabe: aparam_lesen went wrong, ifehl = ',ifehl
         call qerror(fehler)
      endif
      call extnct_lesen()
      call ausgabezeitpunkte() !! reading points in time for output
      call ausgabekonzentrationen() !! reading output-values
      call transinfo_schritte(startzeitpunkt, startzeitpunkt+deltat) !! sollte eigentlich für beide Antriebe gleichermaßen funktionieren
      call wetter_readallo0()
      print*,"wetter_readallo0() gemacht"
      call ganglinien_lesen()
      querschneiden = querschnitt_lesen()
      if (querschneiden) then
         print*,'querschneiden'
      else
         print*,'keine Querschnitte'
      end if
      !! nachschauen, ob und zu welchen Zeitpunkten
      !! Verteilungen der Trübung/Schwebstoff und des Salzgehalts offline bereitliegen.
      call schwebstoff_salz_sichten()
      !! Daten für die Aufenthaltszeitberrechnung von Datei alter.txt lesen
      if (nur_alter) call alter_lesen()
   end if ! only prozessor 0
   call mpi_barrier (mpi_komm_welt, ierr)
   return
   222 format (A,'rechenzeit = ',I15,' Temperatur_Wasser = ',F8.3,' Temperatur_Sediment = ',F8.3)
end subroutine eingabe
!----+-----+----
!> die Subroutine ereigg_modell()\n
!! Die <a href="./exp/EREIGG.txt" target="_blank">EREIGG.txt</a> Dateien für QSim sind weiterverwendbar,\n
!! hier wird zunächst nur die Zeitsteuerung (Anfang, Ende, Zeitschrittweite) daraus gelesen. \n
!! Die SUBROUTINE ereigg_Randbedingungen_lesen() entnimmt dann die Rand-Werte aus der Datei \n
!! \n\n
!! aus Datei eingabe.f95 ; zurück zu \ref lnk_modellerstellung
subroutine ereigg_modell()
   use modell
   use QSimDatenfelder
   implicit none
   character (len = 500) :: dateiname
   integer :: open_error, ion, read_error
   real :: dt_min, tictac
   !      real :: lesezeit
   write(dateiname,'(2A)')trim(modellverzeichnis),'/EREIGG.txt'
   ion = 92
   open ( unit = ion , file = dateiname, status = 'old', action = 'read ', iostat = open_error )
   if (open_error /= 0) then
      write(fehler,*)'open_error EREIGG.txt ... Datei vorhanden?'
      call qerror(fehler)
   end if ! open_error.ne.0
   rewind (ion)
   !
   if ( .not. zeile(ion)) call qerror('ereigg_modell 1 read_error /= 0')
   print*,'EREIGG Version: ', trim(ctext)
   if ( .not. zeile(ion)) call qerror('ereigg_modell 2 read_error /= 0')
   print*,'EREIGG Modell: ', trim(ctext)
   if ( .not. zeile(ion)) call qerror('ereigg_modell 3 read_error /= 0')
   print*,'EREIGG Ereignis: ', trim(ctext)
   !
   if ( .not. zeile(ion)) call qerror('Zeile 3 von EREIGG.txt nicht da')
   read(ctext, *, iostat = read_error) tag, monat, jahr, uhrzeit_stunde ! itags,monats,jahrs,uhrs
   !if(read_error.ne.0) call qerror('read_error in Zeile 3 von EREIGG.txt; Anfangszeitpunkt der Berechnung')
   !tictac=int(uhrzeit_stunde)+((uhrzeit_stunde-int(uhrzeit_stunde))/0.6) ! Umrechnung stunde.minute in dezimal-stunden
   !print*,"gelesen start:", tag, monat, jahr, uhrzeit_stunde,int(uhrzeit_stunde),uhrzeit_stunde-int(uhrzeit_stunde),tictac
   !uhrzeit_stunde = tictac
   call sekundenzeit(2)
   startzeitpunkt = zeitpunkt
   itags = tag
   monats = monat
   jahrs = jahr
   uhrs = uhrzeit_stunde
   print*,'EREIGG.txt, Berechnungsbeginn: tag,monat,jahr, Uhrzeit, Startzeitpunkt' &
   , itags, monats, jahrs, uhrs, startzeitpunkt
   !
   if ( .not. zeile(ion)) call qerror('Zeile 4 von EREIGG.txt nicht da')
   read(ctext, *, iostat = read_error) tag, monat, jahr, uhrzeit_stunde, dt_min ! itage,monate,jahre,uhren,izdt
   if (read_error /= 0) call qerror('read_error in Zeile 4 von EREIGG.txt; Endzeitpunkt der Berechnung')
   !print*,"gelesen ende:", tag, monat, jahr, uhrzeit_stunde,int(uhrzeit_stunde),uhrzeit_stunde-int(uhrzeit_stunde)
   !uhrzeit_stunde = int(uhrzeit_stunde)+((uhrzeit_stunde-int(uhrzeit_stunde))/0.6) ! Umrechnung stunde.minute in dezimal-stunden
   call sekundenzeit(2)
   endzeitpunkt = zeitpunkt
   itage = tag
   monate = monat
   jahre = jahr
   uhren = uhrzeit_stunde
   print*,'EREIGG.txt,   Berechnungsende: tag,monat,jahr, Uhrzeit, Endzeitpunkt' &
   , itage, monate, jahre, uhren, endzeitpunkt
   deltat = int(dt_min*60)
   if (deltat <= 0) then
      write(fehler,*)'ereigg_modell: zeitschrittweite = ',deltat,' , und das ist falsch!'
      call qerror(fehler)
   end if ! zeitschrittweite deltat ist falsch
   if (abs(real(deltat)-(dt_min*60)) > 0.01) then
      write(fehler,*)'ereigg_modell: angegebene zeitschrittweite = ',dt_min,' minuten d.h.',(dt_min*60)  &
                    ,' sekunden ist falsch weil sekundenzeitschritt nicht ganzzahlig'
      call qerror(fehler)
   end if ! Zeitschritt als ganze sekunden
   zeitschrittanzahl = (endzeitpunkt-startzeitpunkt)/(deltat)
   print*,'zeitschrittanzahl, startzeitpunkt, endzeitpunkt, deltat = ',zeitschrittanzahl, startzeitpunkt, endzeitpunkt, deltat
   if (zeitschrittanzahl <= 0) then
      print*,'WARNUNG zeitschrittanzahl = ',zeitschrittanzahl,' , wollen Sie das wirklich ????'
      !! zeitschrittanzahl=null durchlaufen lassen, um Initialisierung ausgeben zu können
      if (zeitschrittanzahl < 0) then !! nur abbrechen wenn unter null.
         call qerror('zeitschrittanzahl < 0')
      end if ! zeitschrittanzahl.lt.0
   end if ! zeitschrittanzahl.le.0
   !if(hydro_trieb.eq. 3)then !## preliminary SCHISM all hydro steps
   !   deltat=dttrans
   !   startzeitpunkt=transinfo_zeit(transinfo_zuord(1))
   !   endzeitpunkt  =transinfo_zeit(transinfo_zuord(transinfo_anzahl))
   !   zeitschrittanzahl=(endzeitpunkt-startzeitpunkt)/deltat
   !   print*,'##preliminary## all ',zeitschrittanzahl,' SCHISM steps ',startzeitpunkt,' until ',endzeitpunkt,' deltat=',deltat
   !end if !SCHISM
   print*,"hydro_trieb = ",hydro_trieb      !case(2) ! Untrim² netCDF
   
   print*,'transinfo_zeit,Anfang+Ende = ',transinfo_zeit(transinfo_zuord(1)), transinfo_zeit(transinfo_zuord(transinfo_anzahl))
   if (startzeitpunkt < transinfo_zeit(transinfo_zuord(1))) then
      print*,"startzeitpunkt, transinfo_zeit(transinfo_zuord(1)), transinfo_zuord(1) = "
      print*,startzeitpunkt, transinfo_zeit(transinfo_zuord(1)), transinfo_zuord(1)
      call qerror('### Abbruch ### zum startzeitpunkt liegen noch keine Transportinformationen vor')
   end if !wrong start time
   if (endzeitpunkt > transinfo_zeit(transinfo_zuord(transinfo_anzahl)))  &
       call qerror('### Abbruch ### zum endzeitpunkt liegen keine Transportinformationen mehr vor')
   print*,'EREIGG.txt, Berechnungs-Zeitraum von ',startzeitpunkt,' bis ', endzeitpunkt  &
   ,' mit zeitschrittweite ',deltat  &
   ,' liegt innerhalb des Zeitraums ',transinfo_zeit(transinfo_zuord(1)),' bis '  &
   ,transinfo_zeit(transinfo_zuord(transinfo_anzahl)),', in dem Transportinformationen vorliegen.'
   rechenzeit = startzeitpunkt
   if ( .not. zeile(ion)) call qerror('Zeile 5 von EREIGG.txt nicht da')
   read(ctext, *, iostat = read_error) imitt,ipH,idl,itemp,itracer,ieros,ischwa,iverfahren  &
        ,ilongDis,FlongDis,iColi,ikonsS,iSchwer,iphy,iformVert,iform_verdr
   
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
   
   print*,'Zeile 5 von EREIGG.txt:'
   print*,'imitt,ipH,idl,itemp,itracer,ieros,ischwa,iverfahren,ilongDis,FlongDis,iColi,ikonsS,iSchwer,iphy,iformVert,iform_verdr'
   print*, imitt,ipH,idl,itemp,itracer,ieros,ischwa,iverfahren,ilongDis,FlongDis,iColi,ikonsS,iSchwer,iphy,iformVert,iform_verdr
   if (read_error /= 0) then
      print*,'EREIGG.txt Zeilentext: ',trim(ctext)
      write(fehler,*)'read_error in Zeile 5 von EREIGG.txt; Berechnungs-Flags'
      call qerror(fehler)
   end if ! open_error.ne.0
   
   !#FG: check that suspended matter ('SS') is only read from hydrodynamic forcing ('iEros<0') when UnTRIM2 is used ('hydro_trieb=2')
   !#FG: dirty(!!!) temporary hack to use 'iEros' for this
   if (iEros < 0 .and. hydro_trieb /= 2) then
      write(fehler,'(a)') "eingabe.f95: Can only read 'SS' (iEros < 0) from UnTRIM hydrodynamics (hydro_trieb = 2)"
      call qerror(fehler)
   end if
   
   if (imitt == 1) &
       print*,'### Warnung ###, die in EREIGG.txt mittels imitt = 1 angeforderte Ausgabe von Tagesmittelwerten ', &
       'ist in QSim3D nicht implementiert.'
   if (ipH == 0) &
       print*,'### Warnung ###, in EREIGG.txt wird mittels ipH = 0 eine Sim. ohne ph-Wert Berechnung angefordert !!!'
   if (idl == 0) &
       print*,'### Warnung ###, die in EREIGG.txt mittels idl = 0 angeforderte Einlesen von Disp.Koeff.', &
       'ist in QSim3D nicht implementiert.'
   if (idl == 1) &
       print*,'### Warnung ###, die in EREIGG.txt mittels idl = 1 angeforderte Berechnen von Disp.Koeff.', &
       'ist in QSim3D nicht implementiert.'
   if (itemp == 1) then
      print*,'### Warning ### Temperature-simulation only. Asked for by itemp = 1 in EREIGG.txt'
      nur_temp = .true.
   else
      nur_temp = .false.
   endif
   if (itracer == 1) &
       print*,'### Warnung ###, die in EREIGG.txt mittels itracer = 1 angeforderte Tracer-Sim.', &
       'ist in QSim3D nicht implementiert.'
   if ((iphy < 1) .or. (iphy > 4)) then
      write(fehler,*)'ereigg_modell: aeration flag iphy out of bounds'
      call qerror(fehler)
   endif
   !qsim_201314:
   !             read(92,'(a50)')modell
   !             read(92,'(a50)')cEreig
   !             read(92,9200)itags,monats,jahrs,uhrs
   !             read(92,9210)itage,monate,jahre,uhren,izdt
   !             read(92,9220)imitt,ipH,idl,itemp,itracer,ieros                    &
   !             ,ischwa,iverfahren,ilongDis,FlongDis
   ! 9220 format(I1,2x,I1,2x,I1,2x,I1,2x,I1,2x,i1,2x,I1,2x,I1,2x,I1,2x,f4.2)
   close (ion)
   rewind (ion)
end subroutine ereigg_modell
