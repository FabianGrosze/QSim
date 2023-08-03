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
   endif !
   call system(systemaufruf,sysa)
   if (sysa /= 0) then
      print*,'deleting file fortschritt when error exit failed'
   endif !
   write(systemaufruf,'(5A)',iostat = errcode)'echo "',trim(fehlermeldung),'" > ',trim(modellverzeichnis),'abbruch'
   if (errcode /= 0) then
      print*,'qerror system call echo abbruch failed'
      call MPI_Abort(mpi_komm_welt, errcode, ierr)
   endif !
   call system(systemaufruf,sysa)
   if (sysa /= 0) then
      print*,'writing error message into file abbruch failed'
   endif !
   write(errcode,*)' ### controlled error exit ### QSim3D ### '
   call MPI_Abort(mpi_komm_welt, errcode, ierr)
   stop
end subroutine qerror

!> Update Model Progress
!!
!! When a simulation is started the file `fortschritt` is created. It contains
!! the percentage of model completion. The file is deleted at the end of the
!! simulation.
!! In addition `fortschritt` serves as a lock file, i.e. no other simulation
!! can be executed as long as this file exists.
subroutine fortschritt(n, f)
   use modell
   use module_datetime
   implicit none
   
   integer, intent(in) :: n !< modus
   real,    intent(in) :: f !< current progress
   
   integer              :: open_error, system_error
   integer              :: u_end, u_run, u_start, u_progress
   integer              :: runtime, runtime_hours, runtime_minutes, runtime_seconds
   logical              :: file_exists
   real                 :: f_old
   character(longname)  :: sys_call, progress_file, file_name
   character(8)         :: qsim_version
   type(datetime)       :: datetime_execution_end, datetime_timestep
   type(datetime)       :: datetime_timestep_start, datetime_timestep_end
   type(datetime), save :: datetime_execution_start
 

   if (meinrang == 0) then
      
      select case (n)
         case (1) ! start
            call version_string(qsim_version)
            print "(a)", repeat("=", 80)
            print "(a)", repeat(" ", 32) // "QSim3D " // qsim_version
            print "(a)", repeat("=", 80)
            
            call versionsdatum()
            
            ! check mnodel directory for completeness
            call modeverz()
            if (.not. modell_vollstaendig()) call qerror("Model directory does not contain all input files.")
            
            ! --- file `start` ---
            datetime_execution_start = datetime()
            datetime_execution_start = datetime_execution_start%now()
            
            file_name = trim(modellverzeichnis) // "start"
            open(file = file_name, newunit = u_start, status = "replace", action = "write")
               write(u_start, "(i0)") datetime_execution_start % seconds_since_epoch()
               write(u_start, "(a)")  datetime_execution_start % date_string()
            close(u_start)
           
            ! --- file `fortschritt` ---   
            progress_file = adjustl(trim(modellverzeichnis) // 'fortschritt')
            inquire(file = trim(progress_file), exist = file_exists)
            if (file_exists) call qerror("File `fortschritt` exists already: Model directory seems to be blocked by another simulation")
            
            open(file = trim(progress_file), newunit = u_progress, status = 'new', action = 'write', iostat = open_error)
            if (open_error /= 0) call qerror("Could not open " // progress_file)
            
            rewind(u_progress)
            write(u_progress,'(f9.6)') 0.0
            close(u_progress)
            
            
            ! --- source code ---
            ! TODO (Schönung, August 2023): Should we really keep this? This information
            ! makes sense while developing, but when distributed this may contain outdated
            ! and potentially private data
            
            ! z.B. write(codesource,*) "/home/jwyrwa/QSim3D" (wird vom Makefile geschrieben)
            include "code_source.h"
            print*, "progress_file = ", adjustl(trim(progress_file))
            print*, "codesource    = ", adjustl(trim(codesource))
            
            
         case (0) ! making progress
            progress_file = adjustl(trim(modellverzeichnis) // 'fortschritt')
            open(file = progress_file, newunit = u_progress, status = 'old', action = 'readwrite', iostat = open_error)
            if (open_error /= 0) call qerror("Could not overwrite file " // progress_file)
            
            read(u_progress,*) f_old
            if (f <= f_old) call qerror("File `fortschritt` contains invalid value.")
            
            rewind(u_progress)
            write(u_progress,'(f9.6)') f
            
            close(u_progress)
         
         case (-1) ! end, finalizing
            if (hydro_trieb == 1) then
               print*, 'mittelflaech  = ', mittelflaech
               print*, 'mittelvolumen = ', mittelvolumen
            endif
            
            ! determine endtime and runtime
            datetime_execution_end = datetime()
            datetime_execution_end = datetime_execution_end%now()
            
            runtime = datetime_execution_end%seconds_since_epoch() - datetime_execution_start%seconds_since_epoch()
            runtime_hours = runtime / 3600
            runtime = modulo(runtime, 3600)
            runtime_minutes = runtime / 60
            runtime_seconds = modulo(runtime, 60)
            
            ! --- file `ende` ---
            file_name = trim(modellverzeichnis) // "ende"
            open(file = file_name, newunit = u_end, status = "replace", action = "write", iostat = open_error)
            if (open_error /= 0) call qerror("Could not open " // trim(file_name))
               write(u_end, "(i0)") datetime_execution_end % seconds_since_epoch()
               write(u_end, "(a)")  datetime_execution_end % date_string()
            close(u_end)
            
            ! --- file `lauf` ---
            file_name = trim(modellverzeichnis) // "lauf"
            open(file = file_name, newunit = u_run, status = "replace", action = "write", iostat = open_error)
            if (open_error /= 0) call qerror("Could not open " // trim(file_name))
               write(u_run, "(a)")  datetime_execution_start % date_string()
               write(u_run, "(a)")  datetime_execution_end % date_string()
            close(u_run)
            
            
            
            ! --- source code ---
            sys_call =  'stat ' // trim(adjustl(codesource)) // '/*source*.taz >/dev/null 2>/dev/null'
            call system(trim(sys_call),system_error)
            if (system_error == 0) then
               sys_call = 'cp ' // trim(adjustl(codesource)) // '/*source*.taz ' // &
                              adjustl(trim(modellverzeichnis)) // ' >/dev/null 2>/dev/null'
               
               call system(trim(sys_call),system_error)
               if (system_error == 0) then
                  print*, "Saving source code:"
                  sys_call = 'ls -thora '// trim(adjustl(codesource)) // '/*source*.taz'
                  call system(trim(sys_call),system_error)
               else
                  print*,"Could not save source code (source*.taz) from " // trim(codesource)
               endif
            endif
            
            ! --- qusave ---
            sys_call = 'qusave ' // trim(modellverzeichnis) // ' >/dev/null 2>/dev/null'
            ! qusave löscht Quellcode-Sicherung im Modellverzeichnis
            call system(trim(sys_call),system_error) 
            if (system_error == 0) print*, "qusave: All input data archived in qsim3d_modell_<model>_<date>.taz"
            
            ! --- quzip ---
            ! vtk-Dateien (d.h. Variablenfelder zu den Ausgabezeitpunkten) archivieren:
            sys_call = 'quzip ' // trim(modellverzeichnis)  //' >/dev/null 2>/dev/null'
            call system(trim(sys_call),system_error)
            if (system_error == 0) print*, "quzip: All .vtk-files archived."

           
            ! --- delete `fortschritt` ---
            progress_file = trim(modellverzeichnis) // 'fortschritt'
            open(file = progress_file, newunit = u_progress, status = 'old', iostat = open_error)
            if (open_error /= 0) call qerror("Could not open file " // progress_file)
            
            close(u_progress, status = "delete", iostat = open_error)
            if (open_error /= 0) call qerror("Could not delete file " // progress_file)
            
            call version_string(qsim_version)
            
            print*
            print "(a)", repeat("=", 80)
            print "(a)", "End of Simulation"
            print "(a)", repeat("=", 80)
            print "(a,a)",       "QSim3D version:  ", qsim_version
            print "(a,a)",       "execution start: ", datetime_execution_start % date_string()
            print "(a,a)",       "execution end:   ", datetime_execution_end % date_string()
            print "(a,*(i0,a))", "runtime:         ", runtime_hours, "h ", runtime_minutes, "min ", runtime_seconds, "sec"
            
            print*
            call versionsdatum()
            
         case default
            call qerror ('fortschritt, Auswahlparameter n falsch')
      end select
   endif !! nur auf Prozessor 0
   
   call mpi_bcast(hydro_trieb,       1,        mpi_integer,   0, mpi_komm_welt, ierr)
   call mpi_bcast(modellverzeichnis, longname, mpi_character, 0, mpi_komm_welt, ierr)
   call mpi_bcast(kontrollknoten,    1,        mpi_integer,   0, mpi_komm_welt, ierr)
   
   ! timestep in between ; controll values at non zero (central) process
   if (meinrang == (proz_anz-1) .and. n == 0) then  
      ! print current timestep to console
      datetime_timestep = gmtime(rechenzeit)
      datetime_timestep_start = gmtime(startzeitpunkt)
      datetime_timestep_end = gmtime(endzeitpunkt)
      
      print*
      print "(a)", repeat("*", 80)
      print "(a,i0,2a)",  "timestep ", izeit, ": ", datetime_timestep%date_string()
      print*
      print "(3x,3a,i0,a)", "from   ",   datetime_timestep_start%date_string(), " [", datetime_timestep_start%seconds_since_epoch(), "]"
      print "(3x,3a,i0,a)", "to     ",   datetime_timestep_end  %date_string(), " [", datetime_timestep_end  %seconds_since_epoch(), "]"
      print "(a)", repeat("*", 80)
      
      
   endif
   
   call mpi_barrier (mpi_komm_welt, ierr)
   
end subroutine fortschritt
