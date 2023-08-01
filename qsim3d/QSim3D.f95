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
!> Hauptprogramm QSim3D
!! Beschreibung siehe:\ref index \n
program QSim3D
   
   use module_alloc_dimensions
   use netcdf
   use modell
   use QSimDatenfelder
   use mod_suspendedMatter, only: step_suspendedMatter
   use mod_salinity, only: step_salinity
   !use netcdf
   !use mpi
   
   implicit none
   
   include 'netcdf.inc'
   
   integer :: i,ni,j,k,n, system_error
   character(300) systemaufruf
   logical :: raus, jetzt_ausgeben
   
   !----initialize parallel computing:
   call parallel_ini()
   ! ----- write start message, find model directory, start progess display (file "fortschritt") which blocks concurrent runs in the same directory
   call fortschritt(1,0.0) ! also gets type of hydraulic driver
   ! ----- set number of stretches
   call set_azstrs(1)    
   !------- reading input, allocation, initialisation etc.
   call eingabe()        ! --- input
   call initialisieren() ! --- initialize
   !----preparing parallel computing:
   call parallel_vorbereiten()
   !! mirror inital values
   call ausgeben()
   call ganglinien_zeitschritt(1)
   
   
   !==== Start of time-loop =================================================================
   do izeit = 1,zeitschrittanzahl !------------------------------------------------- proceed in time
      call zeitschritt_halb(.true.) ! --- increment time and compute boundary-values in the middle of the timestep
      call MPI_Bcast(zeitpunkt, 1, MPI_INTEGER8, 0, mpi_komm_welt, ierr)
      call MPI_Bcast(izeit,1,MPI_INT,0,mpi_komm_welt,ierr)
      call zeitsekunde()
      call fortschritt(0,real(izeit)/real(zeitschrittanzahl)) ! update progess display
      call mpi_barrier (mpi_komm_welt, ierr)
      
      ! set Boundary-Conditions (incl. Weather and Flow)
      call randbedingungen_setzen()
      
      ! salinity module
      call step_salinity
	   
      ! suspended matter module
      if (iEros>=0) then
         call schwebstoff_salz()    ! currently only reading distribuions from input
         call mpi_barrier (mpi_komm_welt, ierr)
      else
         call step_suspendedMatter
      endif
      !------------------------------------------------- all metabolic processes
      call stoffumsatz()     !!             <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      !------------------------------------------------- transport all concentrations (advection-diffusion) ...
      call stofftransport()  !!             <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< if (hydro_trieb.ne. 3) für SCHISM erstmal nicht #########
      !------------------------------------------------- finish time step
      call zeitschritt_halb(.false.)
      call mpi_barrier (mpi_komm_welt, ierr)
      !------------------------------------------------- output ...
      if (jetzt_ausgeben()) call ausgeben() !! output concentration fields if required
      call ganglinien_zeitschritt(izeit+1) !! store values for time series
      call mpi_barrier (mpi_komm_welt, ierr)
   enddo
   !==== End of time-loop   =================================================================
   write(*,*)meinrang,' end of time-loop'
   !-------------------------------------------------
   !call mpi_barrier (mpi_komm_welt, ierr)
   !call gather_planktkon()
   !call gather_benthic()
   !call gather_ueber()
   !call mpi_barrier (mpi_komm_welt, ierr)
   !-------------------------------------------------
   call ganglinien_schliessen() !! write and close time series files
   call ausgeben() !! output at the end
   if (hydro_trieb == 3)call check_err( nf_close(ncid) ) ! close SCHISM netCDF files
   call mpi_barrier (mpi_komm_welt, ierr)
   call fortschritt(-1,0.0) !! write closing message, delete file "fortschritt"
   call mpi_finalize(ierr)
   if (ierr /= 0) then
      print*,'mpi_finalize(ierr) /= 0'
      call exit(7)
      !else
      !   print*,'mpi_finalized'
   endif
   call exit(0)
end program QSim3D
