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
module module_salinity
   use netcdf
   use modell
   
   implicit none
   
   private 
   public :: init_salinity
   public :: step_salinity
   
   ! Module variables
   
   !#FG:
   ! On one fine day, when QSim has a better data structure ...
   !  - These parameters shall not be imprisoned parameters anymore!
   !  - They shall roam the prosperous lands of QSim as free variables!
   !  - Defineth arm in arm with their fellow index tracer variables in an initialisation
   !    routine that checketh the model options and seteth all tracer variable indices,
   !    then a dimension of the glorious new tracer array.
   !    QSim is dead, long live QSim!
   integer                         :: varid                                !< netCDF variable ID
   real, allocatable, dimension(:) :: salinity_element, salinity_element_p !< Salinity (psu) in water column
   
   integer, parameter              :: hydro_UnTRIM2 = 2                    !< identifier of UnTRIM2 hydrodynamics
   integer, parameter              :: i_salinity    = 72                   !< tracer index of salinity
   logical, parameter              :: debug       = .true.                 !< turn debugging on/off
   
   external :: check_err
   
contains
  
   !> Initialise data arrays for reading salinity from file
   subroutine init_salinity
      integer              :: alloc_status   ! success status of array allocation
      character(len = 200) :: text_string 
      
      if (meinrang == 0) then
         ! initialize data arrays for entire domain
         allocate(salinity_element(part * proz_anz), source = 0., stat = alloc_status)
         if (alloc_status /= 0) call qerror('init_salinity: Error allocating salinity for NetCDF reading.')
      endif
      
      ! initialize data field used on MPI processes
      allocate(salinity_element_p(part), source = 0., stat = alloc_status)
      if (alloc_status /= 0) call qerror('init_salinity: Error allocating Salinity partial fields for MPI processes.')
      
      ! read first time step for initialisation
      call step_salinity
      
      ! gather information from parallel processes to have correct salinity available for writing of initial state
      call gather_planktkon()
      
      ! synchronize all parallel processes
      call mpi_barrier(mpi_komm_welt, ierr)
      
   end subroutine init_salinity
   
   ! =====================================================================
   !> Do time step for salinity.
   subroutine step_salinity   
      integer              :: i, j, ks, i_time
      character(len = 200) :: error_message    
      
      ! get Salinity from file and distribute it over all processes
      if (meinrang == 0) then
         
         ! get Salinity data for current time step
         if (rechenzeit < transinfo_zeit(transinfo_zuord(1)) .or. &
             rechenzeit > transinfo_zeit(transinfo_zuord(transinfo_anzahl))) then
            call qerror('step_salinity: Time outside of available time period')
         endif
         
         ! read data closest to center of current time step
         i_time = minloc(abs(int(transinfo_zeit - rechenzeit, 4)), 1)
         
         select case (hydro_trieb)
            case (hydro_UnTRIM2) ! UnTRIM2 hydrodynamics/Salinity
               call get_salinity_UnTRIM2(i_time)
            
            case default
               call qerror('step_salinity: Reading salinity from file only implemented for UnTRIM2 hydrodynamics')
         end select
         
         ! write salinity min/max to console
         write(*,'(a,i0,a,f0.2,a,f0.2,a,f0.2,a)')                                                                  &
               'step_salinity: ', i_time, '-th record read from file - min = ', minval(salinity_element),          &
               ', max = ',  maxval(salinity_element), &
               ', mean = ', sum(salinity_element)/real(max(1,size(salinity_element))), ' (psu)'
      endif
      
      ! synchronize all parallel processes
      call mpi_barrier(mpi_komm_welt, ierr)
      
      ! distribute salinity across parallel processes
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
   !> Read salinity from UnTRIM NetCDF file.
   subroutine get_salinity_UnTRIM2(i_time)
      
      integer, intent(in)   :: i_time    !< ID of time record to be read
      
      integer               :: i                     
      integer, dimension(3) :: start3, count3
      real                  :: fill_value
      
      real,           parameter :: one = 1.
      character(100), parameter :: nc_error_prefix = 'get_salinity_UnTRIM2 - Mesh2_face_Salzgehalt_2d'
      
      ! read data from netCDF file
      start3 = [                  1, 1, i_time]
      count3 = [number_plankt_point, 1,      1]
      call check_err(trim(nc_error_prefix), nf90_inq_varid(ncid, 'Mesh2_face_Salzgehalt_2d', varid))
      call check_err(trim(nc_error_prefix), nf90_get_var(ncid, varid, salinity_element, start3, count3))
      call check_err(trim(nc_error_prefix), nf90_get_att(ncid, varid, "_FillValue", fill_value))
      
      do i = 1,number_plankt_point
         if (abs(salinity_element(i) - fill_value) <= one) then
            ! set land values to 0
            salinity_element(i) = 0.
         else
            salinity_element(i) = max(0., salinity_element(i))
         endif
      enddo
   end subroutine get_salinity_UnTRIM2
   
end module module_salinity