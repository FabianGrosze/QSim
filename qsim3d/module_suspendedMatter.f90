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
module mod_suspendedMatter
   use netcdf
   use modell
   use QSimDatenfelder, only: Grote
   
   implicit none
   
   private ! default: all is private
   public  init_suspendedMatter, step_suspendedMatter
   
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
   integer, parameter                    :: iSSalg = 52                 !< tracer index of SSalg
   integer, parameter                    :: iSS = 53                    !< tracer index of SS
   integer, parameter                    :: iZoo = 50                   !< tracer index of zooind
   integer, parameter                    :: nPhyto = 3                  !< number of phytoplankton groups
   integer, parameter, dimension(nPhyto) :: iPhyto = (/ 8, 9, 10 /)     !< tracer indices of phytoplankton groups
   
   integer                               :: varid                       !< netCDF variable ID
   integer                               :: nClasses                    !< number of SPM fractions to be read from file (starting with finest)
   integer                               :: nClassesFile                !< number of SPM fractions available in file (incl. total SPM)
   
   ! the following quantities are averaged over hydrodynamics output time step
   real, allocatable, dimension(:  )     :: spm_element, spm_element_p  !< SPM concentration (mg L-1) in water column
   real, allocatable, dimension(:  )     :: vol_element                 !< water column volume (m3)
   real, allocatable, dimension(:,:)     :: spm_classes_element         !< mass of SPM size classes (kg)
   
   logical, parameter                    :: debug = .true.              !<s turn debugging on/off
   
contains
   
   ! =================================================================================
   ! ========================= SUBROUTINES and FUNCTIONS =============================
   ! =================================================================================
   
   subroutine init_suspendedMatter
      ! initialise data arrays for reading SPM from file
      
      integer            :: allocStatus   ! success status of array allocation
      
      character(len = 200) :: textString    ! self-explanatory
      
      ! data fields for NetCDF access
      if (meinrang == 0) then
         
         if (iEros>=0) call qerror('init_suspendedMatter: iEros >= 0 invalid for reading SPM from file.')
         nClasses = abs(iEros)
         
         select case (hydro_trieb)
            case (hydro_UnTRIM2) ! UnTRIM2 hydrodynamics/SPM
               ! read number of SPM classes and check against input 'nClasses'
               call nc_check_err( nf90_inq_dimid(ncid, 'nMesh2_suspension_classes', varid) )
               call nc_check_err( nf90_inquire_dimension(ncid, varid, textString, nClassesFile) )
               if (nClassesFile-1 < nClasses) then
                  write(textString, '(a,i2,a,i2,a)') 'init_suspendedMatter_UnTRIM2: Number of selected SPM classes (',       &
                                                    nClasses, ') exceeds number of available classes (', nClassesFile-1, ').'
                  call qerror(trim(textString))
               end if
               case default         ! any other hydrodynamics/SPM
               call qerror('init_suspendedMatter: Reading SPM from file only implemented for UnTRIM2 hydrodynamics')
         end select
         
         ! initialize data arrays for entire domain
         allocate ( spm_classes_element(number_plankt_point, nClasses),   &
         vol_element(number_plankt_point)                  ,   &
         spm_element(part * proz_anz), stat = allocStatus )
         if (allocStatus /= 0) call qerror('init_suspendedMatter: Error allocating SPM fields for NetCDF reading.')
         
         spm_classes_element = 0.
         spm_element = 0.
         vol_element = 0.
         
      end if
      
      ! initialize data field used on MPI processes
      allocate ( spm_element_p(part), stat = allocStatus )
      if (allocStatus /= 0) call qerror('init_suspendedMatter: Error allocating SPM partial fields for MPI processes.')
      spm_element_p = 0.
      
      ! read first time step for initialisation
      call step_suspendedMatter
      ! gather information from parallel processes to have correct SS and SSalg available for writing of initial state
      call gather_planktkon()
      
      ! synchronize all parallel processes
      call mpi_barrier (mpi_komm_welt, ierr)
      
   end subroutine init_suspendedMatter
   
   ! =====================================================================
   subroutine step_suspendedMatter
      ! do time step for SPM, i.e., read data from file and update SS and SSalg
      
      integer             :: i, j, k         ! indices
      integer             :: iTime           ! ID of time record to be read from file
      real                :: livingMatter    ! combined phyto- and zooplankton biomass (mg L-1)
      character(len = 200)  :: errorMessage    ! self-explanatory
      
      ! get SPM concentrations from file and distribute them over all processes
      if (meinrang == 0) then
         ! get SPM data for current time step
         if (rechenzeit < transinfo_zeit(transinfo_zuord(1)) .or. &
             rechenzeit > transinfo_zeit(transinfo_zuord(transinfo_anzahl))) then
            call qerror('step_suspendedMatter: Time outside of available time period')
         end if
         ! read data closest to center of current time step
         iTime = minloc(abs(transinfo_zeit - rechenzeit), 1)
         select case (hydro_trieb)
            case (hydro_UnTRIM2) ! UnTRIM2 hydrodynamics/SPM
               call get_suspendedMatter_UnTRIM2(iTime)
               case default         ! any other hydrodynamics/SPM
               call qerror('step_suspendedMatter: Reading SPM from file only implemented for UnTRIM2 hydrodynamics')
         end select
         ! write SPM min/max to log file
         write(*,'(a,i8,a,F6.2,a,F6.2,a,F6.2,a)')                                                                 &
               'step_suspendedMatter: ', iTime, '-th record read from file - min = ', minval(spm_element),        &
               ', max = ', maxval(spm_element), ', mean = ', sum(spm_element)/max(1,size(spm_element)), ' (mg/L)'
      end if
      
      ! synchronize all parallel processes
      call mpi_barrier(mpi_komm_welt, ierr)
      
      ! distribute SPM concentrations across parallel processes
      call mpi_scatter(spm_element, part, MPI_FLOAT,  spm_element_p, part, MPI_FLOAT, 0, mpi_komm_welt, ierr)
      if (ierr /= 0) then
         write(errorMessage,'(a,i3)') 'step_suspendedMatter: mpi_scatter(spm_element) failed - ', ierr
         call qerror(trim(errorMessage))
      end if
      
      ! set SPM variables (SS and SSalg) and copy to parallel transfer variables
      do i = 1,part
         iGlob = i + meinrang * part
         if (iGlob > number_plankt_point) exit
         j = (i - 1) * number_plankt_vari
         ! sum up zooplankton (from zooind) and phytoplankton biomasses (aki, agr and abl)
         livingMatter = 0.001 * planktonic_variable_p(iZoo + j) * Grote
         do k = 1,nPhyto
            livingMatter = livingMatter + planktonic_variable_p(iPhyto(k) + j)
         end do
         ! assign SS and SSalg to MPI process fields
         planktonic_variable_p(iSS    + j) = spm_element_p(i)
         planktonic_variable_p(iSSalg + j) = spm_element_p(i) + livingMatter
      end do
      
      ! synchronize all parallel processes
      call mpi_barrier(mpi_komm_welt, ierr)
      
   end subroutine step_suspendedMatter
   
   ! =====================================================================
   subroutine get_suspendedMatter_UnTRIM2(iTime)
      ! read SPM and volume from from UnTRIM netCDF file and convert kg to mg L-1
      
      integer, intent(in) :: iTime                  ! ID of time record to be read
      
      real   , parameter  :: one = 1.
      
      integer             :: i                      ! loop index
      integer             :: start3(3), count3(3)   ! netCDF read start/count for 3D variable
      integer             :: start4(4), count4(4)   ! netCDF read start/count for 4D variable
      integer             :: iFill(2)               ! is fill value used (1) or not (0) in .nc file?
      
      real                :: fillValue(2)           ! fill value of netCDF variables
      
      character(len = 200)  :: textString    ! self-explanatory
      
      ! read data from netCDF file
      ! read mean SPM mass
      start4 = (/                   1, 1, nClassesFile - nClasses + 1, iTime /)
      count4 = (/ number_plankt_point, 1,                nClasses    ,     1 /)
      call nc_check_err( nf90_inq_varid(ncid,'Mesh2_face_Schwebstoffmenge_2d', varid) )
      call nc_check_err( nf90_get_var(ncid, varid, spm_classes_element, start4, count4 ) )
      call nc_check_err( nf90_inq_var_fill(ncid, varid, iFill(1), fillValue(1)) )
      ! read mean water volume
      start3 = (/                   1, 1, iTime /)
      count3 = (/ number_plankt_point, 1,     1 /)
      call nc_check_err( nf90_inq_varid(ncid,'Mesh2_face_mittleres_Wasservolumen_2d', varid) )
      call nc_check_err( nf90_get_var(ncid, varid, vol_element, start3, count3 ) )
      call nc_check_err( nf90_inq_var_fill(ncid, varid, iFill(2), fillValue(2)) )
      
      ! sum up SPM masses and calculate concentration
      do i = 1,number_plankt_point
         spm_element(i) = sum(spm_classes_element(i,:))
         if (min(spm_element(i), vol_element(i)) <= 0. .or. &
             (iFill(1) == 1 .and. abs(spm_element(i)/nClasses - fillValue(1)) <= one) .or. &
             (iFill(2) == 1 .and. abs(vol_element(i)          - fillValue(2)) <= one)      ) then
            ! set land values to 0
            spm_element(i) = 0.
         else
            ! [kg] -> [g/m3] = [mg/L]
            spm_element(i) = 1.e3 * spm_element(i) / vol_element(i)
         end if
      end do
      
   end subroutine get_suspendedMatter_UnTRIM2
   
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
   
end module mod_suspendedMatter
