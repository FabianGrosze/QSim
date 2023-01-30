module module_nitrogen
   implicit none
   private
   
   real, parameter   :: KNH3_X1 = 35.
   real, parameter   :: KHNO2_X1 = 5.e-5
   real, parameter   :: KNH3_X2 = 0.75
   real, parameter   :: KHNO2_X2 = 0.18
   real, parameter   :: rhymo = 0.00875
   real, parameter   :: KMO_NO3 = 0.26
   real, parameter   :: KM_NO3  = 0.4
   
   public :: nitrifiers, nitrogen
   
contains
   
   include 'nitrifiers.f90'
   include 'nitrogen.f90'

end module module_nitrogen
