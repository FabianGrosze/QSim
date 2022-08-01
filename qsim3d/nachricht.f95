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

!> Ausgabe von Fehlermeldungen
subroutine qerror(fehlermeldung)
   use modell
   implicit none
   character fehlermeldung*(*)
   character (len = longname) :: systemaufruf
   integer errcode,sysa
   print*,'qsim3d-error: ',trim(fehlermeldung)
   write(*,*)meinrang,' exiting '
   write(systemaufruf,'(3A)',iostat = errcode)'rm -rf ',trim(modellverzeichnis),'fortschritt'
   if (errcode /= 0) then
      !print*,systemaufruf
      print*,'qerror system call rm -rf fortschritt failed'
      call MPI_Abort(mpi_komm_welt, errcode, ierr)
   end if !
   call system(systemaufruf,sysa)
   if (sysa /= 0) then
      print*,'deleting file fortschritt when error exit failed'
   end if !
   write(systemaufruf,'(5A)',iostat = errcode)'echo "',trim(fehlermeldung),'" > ',trim(modellverzeichnis),'abbruch'
   if (errcode /= 0) then
      print*,'qerror system call echo abbruch failed'
      call MPI_Abort(mpi_komm_welt, errcode, ierr)
   end if !
   call system(systemaufruf,sysa)
   if (sysa /= 0) then
      print*,'writing error message into file abbruch failed'
   end if !
   write(errcode,*)' ### controlled error exit ### QSim3D ### '
   call MPI_Abort(mpi_komm_welt, errcode, ierr)
   stop
end subroutine qerror
!-----+-----+-----+-----+
!> Die suboutine fortschritt()
!! legt beim Start n=1 die Datei forschritt im Modell-Verzeichnis an
!! und blockiert damit weitere Modellläufe im diesem Verzeichnis
!! In der Zeitschleife n=0 wird das Verhältnis der bereits bearbeiteten zur Gesamtzahl der zu bearbeitenden Zeitschritte
!! in der Datei fortschritt abgelgt.
!! Nach Beendigung der Berechnung n=-1 wird die Datei fortschritt gelöscht und
!! das Modellverzeichnis für weitere Berechnungen wieder freigegeben
!! \n\n
subroutine fortschritt(n,f)
   use modell
   implicit none
   integer n
   real f, f_old
   integer ion, sysa, system_error,errcode
   character (len = longname) :: systemaufruf,progressfile
   character (len = 8)                     :: versionstext
   if (meinrang == 0) then !! alles nur auf Prozessor 0
      ion = 202
      select case (n)
         case (1) ! start
            call version_string(versionstext)
            print*,'--------------- > QSim-3D ',trim(versionstext),' < --------------- Start-- '
            !   call ausgabekonzentrationen_beispiel()
            !   call AParamParam(cpfad1,j1)
            !   call EreigGParam(cpfad1,j1)
            !   call ModellGParam(cpfad1,j1)
            !   call E_extnctParam(cpfad1,j1)
            !   call EreigHParam(cpfad1,j1)
            !   call Ergeb2DParam(cpfad1,j1)
            !   call ErgebMParam(cpfad1,j1)
            !   call ErgebTParam(cpfad1,j1)
            !   call WetterParam(cpfad1,j1)
            call versionsdatum()
            !        Pfadnamen des Modell Verzeichnisses ermitteln und auf Vollständigkeit prüfen:
            call modeverz()
            if ( .not. modell_vollstaendig()) then
               write(fehler,*)'Modell leider unvollständig'
               call qerror(fehler)
            endif
            !        Laufzeitermittlung starten
            write(systemaufruf,'(3A)',iostat = errcode)'date > ',trim(modellverzeichnis),'start'
            if (errcode /= 0)call qerror('fortschritt systemaufruf date start')
            call system(systemaufruf,system_error)
            if (system_error /= 0) then
               write(fehler,*)'date > start fehlgeschlagen system_error = ', system_error
               call qerror(fehler)
            end if !
            write(progressfile,'(2A)',iostat = errcode)adjustl(trim(modellverzeichnis)),'fortschritt' !! create filestring
            if (errcode /= 0) then
               print*,'|',modellverzeichnis,'|'
               print*,'|',progressfile,'|'
               write(fehler,*)'fortschritt progressfile 1 error = ', errcode
               call qerror(fehler)
            end if !
            open ( ion , file = adjustl(trim(progressfile)), status = 'new', action = 'write', iostat = sysa )
            if (sysa /= 0) then
               print*,'open error file fortschritt, directory blocked by another run?'
               print*,'### uncontrolled error exit ### QSim3D ### '
               call MPI_Abort(mpi_komm_welt, errcode, ierr)
            else
               rewind (ion)
               write(ion,'(f9.6)') 0.0
            end if ! Datei lässt sich anlegen
            close(ion)
            !        source-code Verweis übernehmen:
            ! z.B. write(codesource,*) "/home/jwyrwa/QSim3D" (wird vom Makefile geschrieben)
            include "code_source.h"
            print*,"starting; progress file = ",adjustl(trim(progressfile))," ; codesource = ",adjustl(trim(codesource))
         case (0) ! making progress
            write(progressfile,'(2A)',iostat = errcode)adjustl(trim(modellverzeichnis)),'fortschritt' !! create filestring
            if (errcode /= 0) then
               print*,'|',modellverzeichnis,'|'
               print*,'|',progressfile,'|'
               write(fehler,*)'fortschritt progressfile 0 error = ', errcode
               call qerror(fehler)
            end if !
            open ( ion , file = adjustl(trim(progressfile)), status = 'old', action = 'readwrite', iostat = sysa )
            if (sysa /= 0) then
               print*,' |',adjustl(trim(progressfile)),'| ',sysa, ion
               call qerror('Datei fortschritt lässt sich nicht überschreiben')
            else
               rewind (ion)
               read(ion,*) f_old
               if (f > f_old) then
                  rewind (ion)
                  write(ion,'(f9.6)') f
                  !print*,"Fortschritt bis",f
               else
                  call qerror('kein fortschritt ???')
               endif
            end if ! Datei lässt sich anlegen
            close(ion)
         case (-1) ! end, finalizing
            print*,'mittelflaech = ',mittelflaech,' mittelvolumen = ',mittelvolumen
            !call versionsdatum()
            write(systemaufruf,'(3A)',iostat = errcode)'date > ',trim(modellverzeichnis),'ende'
            if (errcode /= 0)call qerror('fortschritt systemaufruf date ende')
            call system(systemaufruf,system_error)
            if (system_error /= 0) then
               write(fehler,*)'date > ende fehlgeschlagen system_error = ', system_error
               call qerror(fehler)
            end if !
            !call system('cat start ende > lauf')
            write(systemaufruf,'(7A)',iostat = errcode)'cat ',trim(modellverzeichnis),'start '  &
                                               ,trim(modellverzeichnis),'ende > ',trim(modellverzeichnis),'lauf'
            if (errcode /= 0)call qerror('fortschritt systemaufruf cat lauf')
            call system(systemaufruf,system_error)
            if (system_error /= 0) then
               write(fehler,*)'cat start ende > lauf fehlgeschlagen system_error = ', system_error
               call qerror(fehler)
            end if !
            print*,'Laufzeit von ... bis:'
            !call system('tail lauf')
            write(systemaufruf,'(3A)',iostat = errcode)'tail ',trim(modellverzeichnis),'lauf'
            if (errcode /= 0)call qerror('fortschritt systemaufruf cat lauf')
            call system(systemaufruf,system_error)
            if (system_error /= 0) then
               write(fehler,*)'tail lauf fehlgeschlagen system_error = ', system_error
               call qerror(fehler)
            end if !
            if (send_email) then
               write(systemaufruf,'(7A)',iostat = errcode)'mail ',trim(email),' -s "qsim3d ',trim(modellverzeichnis)  &
                                                  ,' fertig" < ',trim(modellverzeichnis),'lauf'
               if (errcode /= 0)call qerror('fortschritt systemaufruf mail fertig')
               !write(systemaufruf,*)trim(email),' -s " qsim3d_notrans hpc01 ',trim(modellverzeichnis),' fertig" < lauf'
               call system (trim(systemaufruf),system_error)
               if (system_error /= 0) then
                  print*,'Email versenden fehlgeschlagen',trim(systemaufruf)
               else
                  print*,trim(systemaufruf), ' erfoglreich verschickt'
               endif
            else
               print*,'keine Beenden-Email verschickt'
            endif
            write(systemaufruf,'(3A)',iostat = errcode) 'stat ',trim(adjustl(codesource)),'/*source*.taz > /dev/null 2 > /dev/null'
            if (errcode /= 0)call qerror('fortschritt systemaufruf stat codesource')
            call system(trim(systemaufruf),system_error)
            if (system_error /= 0) then
               print*,'keine source-code Sicherung (*source*.taz) verfügbar in ',trim(adjustl(codesource))
            else
               write(systemaufruf,'(5A)',iostat = errcode)'cp ',trim(adjustl(codesource)),'/*source*.taz '  &
                                                  ,adjustl(trim(modellverzeichnis)),' > /dev/null 2 > /dev/null'
               if (errcode /= 0)call qerror('fortschritt systemaufruf cp  codesource')
               call system(trim(systemaufruf),system_error)
               if (system_error == 0) then
                  print*,"folgende Quellcode Sicherung wird archiviert:"
                  systemaufruf = 'ls -thora '//trim(adjustl(codesource))//'/*source*.taz'
                  call system(trim(systemaufruf),system_error)
               else
                  print*,"### Archivierung der Quellcode Sicherung (*source*.taz) aus "  &
                  ,trim(adjustl(codesource))," schlug fehl."
               end if
            end if ! system_error.ne.0
            ! Ereignis sichern:
            write(systemaufruf,'(3A)',iostat = errcode)'qusave ',trim(modellverzeichnis),' > /dev/null 2 > /dev/null'
            if (errcode /= 0)call qerror('fortschritt: systemaufruf qusave modellverzeichnis fehlgeschlagen')
            call system(trim(systemaufruf),system_error) !qusave löscht Quellcode-Sicherung im Modellverzeichnis
            if (system_error == 0) then
               print*,"Eingabedaten + Quellcode in qsim3d_modell_ < Modellname > _ < Datum > .taz archiviert."
            else
               print*,"### Archivierung der Eingabedaten schlug fehl."
            end if
            ! vtk-Dateien (d.h. Variablenfelder zu den Ausgabezeitpunkten) archivieren:
            write(systemaufruf,'(3A)',iostat = errcode)'quzip ',trim(modellverzeichnis),' > /dev/null 2 > /dev/null'
            if (errcode /= 0)call qerror('fortschritt systemaufruf ')
            call system(trim(systemaufruf),system_error)
            if (system_error == 0) then
               print*,'zipped vtk-output'
            else
               print*,'no archive for vtk-output'
            end if
            if (stationaer)print*,'stationaer'
            if (nur_temp)print*,'##### nur Temperatur-Simulation (itemp == 1 in EREIGG.txt) #####'
            if (nur_alter)print*,'##### nur Aufenthaltzeit-Simulation  (Datei alter.txt) #####'
            !print*,"#### stoffumsatz ausgeschaltet 14apr15 ####"
            call versionsdatum()
            call version_string(versionstext)
            !print*,'---------------> QSim-3D (muqu) ### tiefe 7 m Kreisgerinne mitt ### <--------------- Schluß -------'
            !print*,'---------------> QSim-3D (qsim3d) <--------------- endet regulaer -------'
            print*,'--------------- > QSim-3D < --------------- ends normally ------- Version = ',trim(versionstext)
            write(systemaufruf,'(3A)',iostat = errcode)'rm -rf ',adjustl(trim(modellverzeichnis)),'fortschritt'
            if (errcode /= 0) then
               print*,'|',modellverzeichnis,'|'
               print*,'|',systemaufruf,'|'
               write(fehler,*)'systemaufruf fortschritt -1 error = ', errcode
               call qerror(fehler)
            end if !
            call system(systemaufruf,sysa)
            if (sysa /= 0) then
               call qerror('Löschen der Datei fortschritt fehlgeschlagen')
            end if !
            case default
            call qerror ('fortschritt, Auswahlparameter n falsch')
      end select
   endif !! nur auf Prozessor 0
   call MPI_Bcast(hydro_trieb,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_BCAST(modellverzeichnis, longname, MPI_CHARACTER,0,mpi_komm_welt,ierr)
   call MPI_Bcast(kontrollknoten,1,MPI_INT,0,mpi_komm_welt,ierr)
   !print*,meinrang," ",adjustl(trim(modellverzeichnis))," ",kontrollknoten
   if ((meinrang == (proz_anz-1)) .and. (n == 0)) then  !! timestep in between ; controll values at non zero (central) process
      print 223, zeitpunkt, izeit,  &
      tag,monat,jahr,stunde,minute,sekunde, startzeitpunkt, endzeitpunkt !, meinrang, tagdesjahres
   endif
   223 format ("============= zeitpunkt:",I10,"s, Zeitschritt: ",I7,'  ',I2.2,'.',I2.2,'.',I4,'  ',I2.2,':',I2.2,':',I2.2, &
               " von ",I10," bis ",I10," =============")
   call mpi_barrier (mpi_komm_welt, ierr)
   return
end subroutine fortschritt
