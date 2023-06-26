!> All Metabolism Modules
!!
!! This module is designed to make it easy to use all metabolism 
!! modules in a single step. 
module module_metabolism
   
   use module_coliform_bacteria
   use module_nitrogen
   use module_organic_carbon
   use module_oxygen
   use module_ph
   use module_phosphate
   use module_silicate
   use module_water_temperature
   use module_zooplankton
   
   ! These modules are currently turned off:
   ! use module_corophium
   ! use module_hnf
   ! use module_macrophytes
   
   implicit none

end module module_metabolism