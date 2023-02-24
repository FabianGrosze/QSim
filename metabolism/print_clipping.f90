!> Print information about clipping
!!
!! In metabolism routines some values might become negative within the timestep
!! and are then clipped to a positive value. This subroutine provides a convient
!! way to inform the user about such clipping.
subroutine print_clipping(origin, var_name, value_old, value_new, values_unit)
   implicit none
   
   character(len=*), intent(in)  :: origin      !< name of the subroutine where clipping occured
   character(len=*), intent(in)  :: var_name    !< name of the clipped variable
   real, intent(in)              :: value_old   !< value before clipping
   real, intent(in)              :: value_new   !< value after clipping
   character(len=*), intent(in)  :: values_unit !< unit of the clipped variable
   
   print "(*(g0,1x))",   "Clipping occured in", origin
   print "(3x,*(g0,1x))", var_name, "old =", value_old, values_unit
   print "(3x,*(g0,1x))", var_name, "new =", value_new, values_unit
   print "(3x,*(g0,1x))", "delta =", value_new - value_old, values_unit
   print *, ""
   
end subroutine print_clipping