
!> Sauerstoffsättigungskonzentration in Abhänigkeit der Wassertemperatur
pure real function oxygen_saturation_concentration(tempw)
   implicit none
   real, intent(in) :: tempw !< Wassertemperatur [°C]
   
   oxygen_saturation_concentration = 14.603               &
                                   - 0.40215   * tempw    &
                                   + 0.007687  * tempw**2 &
                                   - 0.0000693 * tempw**3
end function oxygen_saturation_concentration