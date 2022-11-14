module module_oxygen
   implicit none
   
   public :: oxygen, oxygen_saturation_concentration
   
contains
   include 'oxygen.f90'
   include 'oxygen_saturation_concentration.f90'

end module module_oxygen

