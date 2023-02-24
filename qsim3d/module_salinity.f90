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
!> This module defines variables and subroutines for the use of SPM concentrations read from hydrodynamic
!! files within QSim3D. At the moment, this only works for UnTRIM2/SediMorph data.
!!
!! In the long-term it could be expanded to include all SPM related variables and routines.
module mod_salinity
   use netcdf
   use modell
!   use aparam, only: GRot
   
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
   integer, parameter                    :: hydro_UnTRIM2 = 2           !< identifier of UnTRIM2 hydrodynamics
   !!integer, parameter                    :: iSSalg = 52                 !< tracer index of SSalg
   !!integer, parameter                    :: iSS = 53                    !< tracer index of SS
   !!integer, parameter                    :: iZoo = 50                   !< tracer index of zooind
   !!integer, parameter                    :: nPhyto = 3                  !< number of phytoplankton groups
   !!integer, parameter, dimension(nPhyto) :: iPhyto = (/ 8, 9, 10 /)     !< tracer indices of phytoplankton groups
   integer, parameter                    :: i_salinity = 72             !< tracer index of salt
   integer                               :: varid                       !< netCDF variable ID
   !!integer                               :: nClasses                    !< number of SPM fractions to be read from file (starting with finest)
   !!integer                               :: nClassesFile                !< number of SPM fractions available in file (incl. total SPM)
   
   ! the following quantities are averaged over hydrodynamics output time step
   real, allocatable, dimension(:  )     :: salinity_element, salinity_element_p 
   !!real, allocatable, dimension(:  )     :: spm_element, spm_element_p  !< SPM concentration (mg L-1) in water column
   !!real, allocatable, dimension(:  )     :: vol_element                 !< water column volume (m3)
   !!real, allocatable, dimension(:,:)     :: spm_classes_element         !< mass of SPM size classes (kg)
   
   logical, parameter                    :: debug = .true.              !<s turn debugging on/off
   
contains
   
   ! =================================================================================
   ! ========================= SUBROUTINES and FUNCTIONS =============================
   ! =================================================================================
   
   subroutine init_salinity
      ! initialise data arrays for reading Salt from file
      
      integer            :: allocStatus   ! success status of array allocation
      
      character(len = 200) :: textString    ! self-explanatory
      
      ! data fields for NetCDF access
      if (meinrang == 0) then
         
   !      if (iEros>=0) call qerror('init_salinity: iEros >= 0 invalid for reading salt from file.')
   !      nClasses = abs(iEros)
         
         select case (hydro_trieb)
            case (hydro_UnTRIM2) ! UnTRIM2 hydrodynamics/SPM
               ! read number of SPM classes and check against input 'nClasses'
               call nc_check_err( nf90_inq_dimid(ncid, 'Mesh2_face_Salzgehalt_2d', varid) )
               call nc_check_err( nf90_inquire_dimension(ncid, varid, textString) )
   !            if (nClassesFile-1 < nClasses) then
   !               write(textString, '(a,i2,a,i2,a)') 'init_suspendedMatter_UnTRIM2: Number of selected SPM classes (',       &
   !                                                 nClasses, ') exceeds number of available classes (', nClassesFile-1, ').'
   !               call qerror(trim(textString))
   !            end if
               case default         ! any other hydrodynamics/SPM
               call qerror('init_salinity: Reading salt from file only implemented for UnTRIM2 hydrodynamics')
         end select
         
         ! initialize data arrays for entire domain
   !      allocate ( spm_classes_element(number_plankt_point, nClasses),   &
   !      vol_element(number_plankt_point)                  ,   &
   !      spm_element(part * proz_anz), stat = allocStatus )
         allocate (salinity_element(part * proz_anz), stat = allocStatus )
         if (allocStatus /= 0) call qerror('init_salinity: Error allocating salt for NetCDF reading.')
         
   !      spm_classes_element = 0.
   !      spm_element = 0.
   !      vol_element = 0.
          salinity_element = 0
      end if
      
      ! initialize data field used on MPI processes
      allocate ( salinity_element_p(part), stat = allocStatus )
      if (allocStatus /= 0) call qerror('init_salinity: Error allocating Salt partial fields for MPI processes.')
      salinity_element_p = 0.
      
      ! read first time step for initialisation
      call step_salinity
      ! gather information from parallel processes to have correct salinity available for writing of initial state
 !     call gather_planktkon()
      
      ! synchronize all parallel processes
      call mpi_barrier (mpi_komm_welt, ierr)
      
   end subroutine init_salinity
   
   ! =====================================================================
   subroutine step_salinity
      ! do time step for SPM, i.e., read data from file and update SS and SSalg
      
      integer             :: i, j, k         ! indices
      integer             :: iTime           ! ID of time record to be read from file
 !     real                :: livingMatter    ! combined phyto- and zooplankton biomass (mg L-1)
      character(len = 200):: errorMessage    ! self-explanatory
      
      ! get SPM concentrations from file and distribute them over all processes
      if (meinrang == 0) then
         ! get SPM data for current time step
         if (rechenzeit < transinfo_zeit(transinfo_zuord(1)) .or. &
             rechenzeit > transinfo_zeit(transinfo_zuord(transinfo_anzahl))) then
            call qerror('step_salinity: Time outside of available time period')
         end if
         ! read data closest to center of current time step
         iTime = minloc(abs(transinfo_zeit - rechenzeit), 1)
         select case (hydro_trieb)
            case (hydro_UnTRIM2) ! UnTRIM2 hydrodynamics/SPM
               call get_salinity_UnTRIM2(iTime)
            case default         ! any other hydrodynamics/SPM
               call qerror('step_salinity: Reading salinity from file only implemented for UnTRIM2 hydrodynamics')
         end select
         ! write salinity min/max to log file
         write(*,'(a,i8,a,F6.2,a,F6.2,a,F6.2,a)')                                                                 &
               'step_salinity: ', iTime, '-th record read from file - min = ', minval(salinity_element),        &
               ', max = ', maxval(salinity_element), ', mean = ', sum(salinity_element)/max(1,size(salinity_element)), ' (mg/L)'
      end if
      
      ! synchronize all parallel processes
      call mpi_barrier(mpi_komm_welt, ierr)
      
      ! distribute SPM concentrations across parallel processes
      call mpi_scatter(salinity_element, part, MPI_FLOAT,  salinity_element_p, part, MPI_FLOAT, 0, mpi_komm_welt, ierr)
      if (ierr /= 0) then
         write(errorMessage,'(a,i3)') 'step_salinity: mpi_scatter(salinity_element) failed - ', ierr
         call qerror(trim(errorMessage))
      end if
      
      ! set SPM variables (SS and SSalg) and copy to parallel transfer variables
      do i = 1,part
         iGlob = i + meinrang * part
         if (iGlob > number_plankt_point) exit
         j = (i - 1) * number_plankt_vari
 !        ! sum up zooplankton (from zooind) and phytoplankton biomasses (aki, agr and abl)
 !        livingMatter = 0.001 * planktonic_variable_p(iZoo + j) * GRot
 !        do k = 1,nPhyto
 !           livingMatter = livingMatter + planktonic_variable_p(iPhyto(k) + j)
 !        end do
         ! assign SS and SSalg to MPI process fields
 !        planktonic_variable_p(iSS    + j) = spm_element_p(i)
 !        planktonic_variable_p(iSSalg + j) = spm_element_p(i) + livingMatter
          planktonic_variable_p(i_salinity + j) = salinity_element_p(i)

      end do
      
      ! synchronize all parallel processes
      call mpi_barrier(mpi_komm_welt, ierr)
      
   end subroutine step_salinity
   
   ! =====================================================================
   subroutine get_salinity_UnTRIM2(iTime)
      ! read SPM and volume from from UnTRIM netCDF file and convert kg to mg L-1
      
      integer, intent(in) :: iTime                  ! ID of time record to be read
      
      real   , parameter  :: one = 1.
      
      integer             :: i                      ! loop index
      integer nk
      integer             :: start3(3), count3(3)   ! netCDF read start/count for 3D variable
 !     integer             :: start4(4), count4(4)   ! netCDF read start/count for 4D variable
      integer             :: iFill               ! is fill value used (1) or not (0) in .nc file?
      
      real                :: fillValue           ! fill value of netCDF variables
      
      character(len = 200)  :: textString    ! self-explanatory
      
      ! read data from netCDF file
      ! read mean SPM mass
 !     start4 = (/                   1, 1, nClassesFile - nClasses + 1, iTime /)
 !     count4 = (/ number_plankt_point, 1,                nClasses    ,     1 /)
 !     call nc_check_err( nf90_inq_varid(ncid,'Mesh2_face_Schwebstoffmenge_2d', varid) )
 !     call nc_check_err( nf90_get_var(ncid, varid, spm_classes_element, start4, count4 ) )
 !     call nc_check_err( nf90_inq_var_fill(ncid, varid, iFill(1), fillValue(1)) )
      ! read mean water volume
      start3 = (/                   1, 1, iTime /)
      count3 = (/ number_plankt_point, 1,     1 /)
      call nc_check_err( nf90_inq_varid(ncid,'Mesh2_face_Salzgehalt_2d', varid) )
      call nc_check_err( nf90_get_var(ncid, varid, salinity_element, start3, count3 ) )
      call nc_check_err( nf90_inq_var_fill(ncid, varid, iFill, fillValue) )
      
      ! sum up SPM masses and calculate concentration
      do i = 1,number_plankt_point
         !nk = (i-1)*number_plankt_vari
 !        spm_element(i) = sum(spm_classes_element(i,:))
         !planktonic_variable(72+nk)=salinity_element(i)
         
         salinity_element(i) = max(0., salinity_element(i))
      end do

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
