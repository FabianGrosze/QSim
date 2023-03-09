subroutine set_cloud_reflectance(cloud_type, cloud_reflectance)
   
   ! set cloud's reflectance based on its type (0-9)
   
   implicit none
   
   integer, intent(in)  :: cloud_type
   real   , intent(out) :: cloud_reflectance
   
   character(len=100)   :: message
   
   external             :: qerror
   
   if (cloud_type > 9) then
      write(message, '(a,i3)') "set_cloud_reflectance.f90: Invalid cloud type - ", cloud_type
      call qerror(trim(message))
   endif
   
   if (cloud_type == 0) then
      cloud_reflectance = 0.0
   elseif (cloud_type == 1) then
      cloud_reflectance = 0.04
   elseif (cloud_type == 2) then
      cloud_reflectance = 0.08
   elseif (cloud_type == 3 .or. cloud_type <  0) then
      cloud_reflectance = 0.17
   elseif (cloud_type == 4 .or. cloud_type == 5) then
      cloud_reflectance = 0.2
   elseif (cloud_type >  5) then
      cloud_reflectance = 0.25
   endif
   
end subroutine set_cloud_reflectance