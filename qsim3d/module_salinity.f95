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
!> This module defines variables and subroutines for the use of Salinity read from hydrodynamic
!! files within QSim3D. At the moment, this only works for UnTRIM2/SediMorph data.
!!
module mod_salinity
   use netcdf
   use modell
   
   implicit none
   
   private ! default: all is private
   public  init_salinity, step_salinity
   
   ! Module variables
   
   !#FG:
   ! On one fine day, when QSim has a better data structure ...
   !  - These parameters shall not be imprisoned parameters anymore!
   !  - They shall roam the prosperous lands of QSim as free variables!
   !  - Defineth arm in arm with their fellow index tracer variables in an initialisation
   !    routine that checketh the modell options and seteth all tracer variable indices,
   !    then a dimension of the glorious new tracer array.
   !    QSim is dead, long live QSim!
   integer, parameter                    :: hydro_UnTRIM2 = 2                    !< identifier of UnTRIM2 hydrodynamics
   integer, parameter                    :: i_salinity    = 72                   !< tracer index of salinity
   integer                               :: varid                                !< netCDF variable ID
 
   ! the following quantities are averaged over hydrodynamics output time step
   real, allocatable, dimension(:  )     :: salinity_element, salinity_element_p !< Salinity (psu) in water column
   
   logical, parameter                    :: debug       = .true.                 !< turn debugging on/off
   
contains
   
   ! =================================================================================
   ! ========================= SUBROUTINES and FUNCTIONS =============================
   ! =================================================================================
   
   subroutine init_salinity
      ! initialise data arrays for reading Salinity from file
      
      integer              :: alloc_status   ! success status of array allocation
      
      character(len = 200) :: text_string    ! self-explanatory
      
      ! data fields for NetCDF access
      if (meinrang == 0) then         
         select case (hydro_trieb)
         case (hydro_UnTRIM2) ! UnTRIM2 hydrodynamics/SPM
            call nc_check_err(nf90_inq_dimid(ncid, 'Mesh2_face_Salzgehalt_2d', varid))
            call nc_check_err(nf90_inquire_dimension(ncid, varid, text_string))
         case default         ! any other hydrodynamics/SPM
            call qerror('init_salinity: Reading salinity from file only implemented for UnTRIM2 hydrodynamics')
         end select
         
         ! initialize data arrays for entire domain
         allocate (salinity_element(part * proz_anz), stat = alloc_status)
         if (alloc_status /= 0) call qerror('init_salinity: Error allocating salinity for NetCDF reading.')
         salinity_element = 0
      endif
      
      ! initialize data field used on MPI processes
      allocate (salinity_element_p(part), stat = alloc_status)
      if (alloc_status /= 0) call qerror('init_salinity: Error allocating Salinity partial fields for MPI processes.')
      salinity_element_p = 0.
      
      ! read first time step for initialisation
      call step_salinity
      ! gather information from parallel processes to have correct salinity available for writing of initial state
      call gather_planktkon()
      
      ! synchronize all parallel processes
      call mpi_barrier(mpi_komm_welt, ierr)
      
   end subroutine init_salinity
   
   ! =====================================================================
   subroutine step_salinity
      ! do time step for Salinity
      
      integer             :: i, j, k          ! indices
      integer             :: i_time           ! ID of time record to be read from file
      character(len = 200):: error_message    ! self-explanatory
      
      ! get Salinity from file and distribute it over all processes
      if (meinrang == 0) then
         ! get Salinity data for current time step
         if (rechenzeit < transinfo_zeit(transinfo_zuord(1)) .or. &
             rechenzeit > transinfo_zeit(transinfo_zuord(transinfo_anzahl))) then
            call qerror('step_salinity: Time outside of available time period')
         endif
         ! read data closest to center of current time step
         i_time = minloc(abs(transinfo_zeit - rechenzeit), 1)
         select case (hydro_trieb)
            case (hydro_UnTRIM2) ! UnTRIM2 hydrodynamics/Salinity
               call get_salinity_UnTRIM2(i_time)
            case default         ! any other hydrodynamics/Salinity
               call qerror('step_salinity: Reading salinity from file only implemented for UnTRIM2 hydrodynamics')
         end select
         ! write salinity min/max to log file
         write(*,'(a,i8,a,F6.2,a,F6.2,a,F6.2,a)')                                                                 &
               'step_salinity: ', i_time, '-th record read from file - min = ', minval(salinity_element),          &
               ', max = ', maxval(salinity_element), ', mean = ', sum(salinity_element)/max(1,size(salinity_element)), ' (psu)'
      endif
      
      ! synchronize all parallel processes
      call mpi_barrier(mpi_komm_welt, ierr)
      
      ! distribute Salinity across parallel processes
      call mpi_scatter(salinity_element, part, MPI_FLOAT, salinity_element_p, part, MPI_FLOAT, 0, mpi_komm_welt, ierr)
      if (ierr /= 0) then
         write(error_message,'(a,i3)') 'step_salinity: mpi_scatter(salinity_element) failed - ', ierr
         call qerror(trim(error_message))
      endif
      
      ! Copy salinity variable to parallel transfer variable
      do i = 1,part
         iGlob = i + meinrang * part
         if (iGlob > number_plankt_point) exit
         j = (i - 1) * number_plankt_vari
         planktonic_variable_p(i_salinity + j) = salinity_element_p(i)
      enddo
      
      ! synchronize all parallel processes
      call mpi_barrier(mpi_komm_welt, ierr)
      
   end subroutine step_salinity
   
   ! =====================================================================
   subroutine get_salinity_UnTRIM2(i_time)
      ! read Salinity from from UnTRIM netCDF file
      
      integer, intent(in)   :: i_time                 ! ID of time record to be read
      
      real, parameter       :: one = 1.
      
      integer               :: i                      ! loop index
      integer               :: start3(3), count3(3)   ! netCDF read start/count for 3D variable
      integer               :: i_fill                 ! is fill value used (0) or not (1) in .nc file?
      
      real                  :: fill_value             ! fill value of netCDF variables
      
      character(len = 200)  :: text_string            ! self-explanatory
      
      ! read data from netCDF file
      start3 = (/                   1, 1, i_time /)
      count3 = (/ number_plankt_point, 1,      1 /)
      call nc_check_err(nf90_inq_varid    (ncid, 'Mesh2_face_Salzgehalt_2d', varid))
      call nc_check_err(nf90_get_var      (ncid, varid, salinity_element, start3, count3))
      call nc_check_err(nf90_inq_var_fill (ncid, varid, i_fill, fill_value))
      
      do i = 1,number_plankt_point
         if (i_fill == 0 .and. abs(salinity_element(i) - fill_value) <= one) then
            ! set land values to 0
            salinity_element(i) = 0.
         else
            salinity_element(i) = max(0., salinity_element(i))
         endif
      enddo

   end subroutine get_salinity_UnTRIM2
   
   ! =====================================================================
   subroutine nc_check_err(iNetCDFerror)
      ! netCDF error checker
      !#FG: This is a duplicate from stofftransport_untrim.f95.
      !     Standard utilities like this should all be placed in a single
      !     'utilities' module in the future.
      
      integer, intent(in) :: iNetCDFerror
      
      include 'netcdf.inc'
      
      if (iNetCDFerror /= NF_NOERR) write(*,'(a)') nf_strerror(iNetCDFerror)
      
   end subroutine nc_check_err
   
end module mod_salinity
