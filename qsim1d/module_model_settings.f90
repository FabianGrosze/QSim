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

!> Contains settings as defined in EreigG.txt
!! @date 20.06.2022
module module_model_settings
   implicit none
   save
   
   character(len=255), protected  :: cpfad       !< QSim directory
   character(len=255), protected  :: cpfad1      !< Hydrax directory
   character(len=50),  protected  :: modell      !< modell name (Gerris)
   character(len=255), protected  :: cEreig      !< meta data (Gerris)
   integer, protected             :: itag_start  !< day of simulation
   integer, protected             :: monat_start !< month of simulation
   integer, protected             :: jahr_start  !< year of simulation
   real,    protected             :: uhr_start   !< time of simulation
   integer, protected             :: itage       !< day of simulation end
   integer, protected             :: monate      !< month of simulation end
   integer, protected             :: jahre       !< year of simulation end
   real   , protected             :: uhren       !< time of simulation end 
   integer, protected             :: izdt        !< timestep [min]
   integer, protected             :: imitt       !< controlls output for daily menas
   integer, protected             :: ipH         !< controlls calculation for pH
   integer, protected             :: idl         !< dispertionkoefficient
   integer, protected             :: itemp       !< controlls calculation for temperature
   integer, protected             :: itracer     !< controlls calculation for tracer
   integer, protected             :: ieros       !< controlls calculation for erosion
   integer, protected             :: ischwa      !< not used
   integer, protected             :: iverfahren  !< number of transportation equation
   integer, protected             :: ilongDis    !< number for dispersion equation
   real,    protected             :: FlongDis    !< factor for disperation equation
   integer, protected             :: iColi       !< controlls calculation for coliform
   integer, protected             :: ikonsS      !< controlls calculation for conservative substances
   integer, protected             :: iSchwer     !< controlls calculation for heavy metals
   integer, protected             :: iphy        !< number for equation of aeration
   integer, protected             :: iformVert   !< number for equation of evaporation
   integer, protected             :: iform_VerdR !< unknown
   integer, protected             :: iWSim       !< defines kind of simulation
   
   real, protected                :: tflie       !< timestep [d]
   
   public :: read_ereigg_settings, get_paths

contains

   !> Read paths as given to the program
   subroutine get_paths(linux)
      logical, intent(in)  :: linux    !< Should paths adapt to linux?
      
      character(len=1)     :: sep 
      character(2)         :: bckslsh = '\\'
      
      ! read program arguments
      call GETARG(1, cpfad)
      call GETARG(2, cpfad1)
      if (cpfad1 == ' ') cpfad1 = cpfad
      
      
      ! add file seperator
      if (linux) then 
         sep = '/'
      else
         sep = bckslsh(1:1)
      endif
      
      if (cpfad /= '/F') cpfad = trim(cpfad) // sep
      cpfad1 = trim(cpfad1) // sep
      
   end subroutine get_paths
   
   
   
   !> Read modell settings as defined in EreigG.txt
   subroutine read_ereigg_settings()
      implicit none
      
      character(len=275)   :: pfadstring
      character(len=2)     :: cKenn_vers1
      integer              :: open_error, read_error
      
      
      pfadstring = trim(adjustl(cpfad)) // 'EREIGG.txt'
      open(unit = 92, file = pfadstring, iostat = open_error)
      
      
      rewind (92)
      read(92,'(A2)')ckenn_vers1
      if (ckenn_vers1 /= '*V') then
         read(92,'(a255)')cEreig
         read(92,9200)itag_start,monat_start,jahr_start,uhr_start
         read(92,9210)itage,monate,jahre,uhren,izdt
         read(92,9220)imitt,ipH,idl,itemp,itracer,ieros,ischwa,iverfahren,ilongDis,FlongDis
      else
         read(92,'(a50)')modell
         read(92,'(a255)')cEreig
         read(92,9200)itag_start,monat_start,jahr_start,uhr_start
         read(92,9210)itage,monate,jahre,uhren,izdt
         read(92,*,iostat = read_error)imitt,ipH,idl,itemp,itracer,ieros,   &
                                       ischwa,iverfahren,ilongDis,FlongDis, &
                                       iColi,ikonsS,iSchwer, iphy,iformVert,&
                                       iform_VerdR
      endif
      
      if (read_error /= 0) then
         print *, 'Error while reading EreigG.txt.'
         print *, 'Fileformat may be wrong.'
         stop 33
      endif
      
      close(92)
      9200 format(I2,2x,I2,2x,I4,2x,f5.2)
      9210 format(I2,2x,I2,2x,I4,2x,f5.2,2x,I3)
      9220 format(I1,2x,I1,2x,I1,2x,I1,2x,I1,2x,i1,2x,I1,2x,I1,2x,I1,2x,f4.2,2x,I1,2x,I1,2x,I1,2x,I1,2x,I1,2x,I1)
      
      
      ! if not specified defaults are used
      if (iverfahren == 0) iverfahren = 1
      if (ilongDis == 0)   ilongDis = 1
      if (FlongDis == 0.0) FlongDis = 1.
      
      if (iphy < 1 .or. iphy > 4) then
         print*, 'Error in EreigG.txt:'
         print*, 'iPhy (number for equation of aeration) is defined incorretly'
         print '("iphy = ", I0)', iPhy
         stop 34
      endif
      
      ! determine iWSim
      if (itemp == 0 .and. itracer == 0) iwsim = 3
      if (itemp == 1)   iwsim = 2
      if (itracer == 1) iwsim = 4
      if (icoli == 1)   iwsim = 2
      if (ikonsS == 1)  iwsim = 5
      if (iSchwer == 1) iwsim = 3
      
      ! adapt settings
      if (iwsim == 2 .or. iwsim == 5) then
         if (ieros == 1) ieros = 0
         if (iph == 1) iph = 0
      endif
      if (iSchwer == 1 .and. iFormVert == 1) ipH = 1
      
      ! Umrechnen von Pseudoformat in Dezimalformat
      uhren = int(uhren)+((uhren-int(uhren))/0.6)
      
      tflie = izdt/1440.
      
      ! ---  print values to screen ---
      print *, ''
      print *, repeat('=', 78)
      print *, repeat(' ', 34), 'EreigG.txt'
      print *, repeat('=', 78)
      
      print '("  simulation start: ", I0.4,"-",I0.2,"-",I0.2," ",f5.2," UTC+1")', jahr_start,monat_start,itag_start,uhr_start
      print '("  simulation end:   ", I0.4,"-",I0.2,"-",I0.2," ",f5.2," UTC+1")', jahre,monate,itage,uhren
      print '("  timestep        = ", I0, " minutes (", f5.4," days)")', izdt, tflie
      print *, ''
      
      print '(a,i1)',   '  iMitt       = ', imitt
      print '(a,i1)',   '  ipH         = ', iph
      print '(a,i1)',   '  idl         = ', idl
      print '(a,i1)',   '  iTemp       = ', itemp
      print '(a,i1)',   '  iTracer     = ', itracer
      print '(a,i1)',   '  iEros       = ', ieros
      print '(a,i1)',   '  iSchwa      = ', ischwa
      print '(a,i1)',   '  iVerfahren  = ', iverfahren
      print '(a,i1)',   '  iLongDis    = ', ilongDis
      print '(a,f0.2)', '  FlongDis    = ', FlongDis
      print '(a,i1)',   '  iColi       = ', iColi
      print '(a,i1)',   '  iKonsS      = ', ikonsS
      print '(a,i1)',   '  iSchwer     = ', iSchwer
      print '(a,i1)',   '  iPhy        = ', iphy
      print '(a,i1)',   '  iFormVert   = ', iformVert
      print '(a,i1)',   '  iForm_VerdR = ', iform_VerdR
      print '(a,i1)',   '  iWSim       = ', iwsim
      
   end subroutine read_ereigg_settings

end module module_model_settings