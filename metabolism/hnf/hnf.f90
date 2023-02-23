!> Heterotroph Nanoflagelates
!! @author Volker Kirchesch
!! @date 15.06.2001
subroutine hnf(chnf_s, bac_s, vo2_s, tempw_s, drhnf_s, &
               tflie,                                  &
               hnfbac_s, hnfupa_s, hnfrea_s, hnfexa_s, & 
               hnfmoa_s, hnfmua_s, ro2hnf_s, bsbhnf_s, &
               kontroll, jjj)
   
   use aparam,        only: uphnf, backs
   use module_oxygen, only: oxygen_saturation_concentration
   implicit none
   
   ! --- dummy arguments ---
   real,    intent(inout) :: chnf_s   !< HNF [mgC]
   real,    intent(in)    :: bac_s    !< heterotroph bacteria [mgC]
   real,    intent(in)    :: vo2_s    !< oxygen [mg/l]
   real,    intent(in)    :: tempw_s  !< water temperature [°C]
   real,    intent(in)    :: drhnf_s  !< consumption of hnf by Dreissena
   real,    intent(in)    :: tflie    !< timestep [d]
   real,    intent(out)   :: hnfbac_s !< consumption of bacteria by hnf
   real,    intent(out)   :: hnfupa_s !<
   real,    intent(out)   :: hnfrea_s !<
   real,    intent(out)   :: hnfexa_s !<
   real,    intent(out)   :: hnfmoa_s !<
   real,    intent(out)   :: hnfmua_s !<
   real,    intent(out)   :: ro2hnf_s !< consumption of oxygen by hnf (respiration)
   real,    intent(out)   :: bsbhnf_s !< increment of bsb due to dead hnf and excretion 
   logical, intent(in)    :: kontroll !< debugging
   integer, intent(in)    :: jjj      !< debugging
   
   ! --- local variables ---
   real            :: ftemp, fo2, saett, chnft, chnf_old
   real, parameter :: q10 = 2.
   real, parameter :: rg_hnf  = 0.05
   real, parameter :: hnf_ass = 0.33
   real, parameter :: hnf_ex  = 0.5
   real, parameter :: mt_hnf  = 0.1
   real, parameter :: mo_hnf  = 0.25
   
   external :: print_clipping
   
   ! influence of temperature
   ftemp = q10**((tempw_s - 20.) / 10.)
   
   ! specific uptake rate
   hnfupa_s = uphnf * (bac_s / (bac_s + backs)) * ftemp
   
   ! respiration rate
   hnfrea_s = rg_hnf * ftemp + hnfupa_s * (1. - hnf_ass) * (1. - hnf_ex)
   
   ! excretion rate
   hnfexa_s = hnfupa_s * (1. - hnf_ass) * hnf_ex
   
   ! mortality rate
   saett = oxygen_saturation_concentration(tempw_s)
   fo2 = min(1., vo2_s / saett)
   hnfmoa_s = (1.-fo2) * mo_hnf + mt_hnf * ftemp
   
   ! growth rate
   hnfmua_s = hnfupa_s - hnfrea_s - hnfexa_s - hnfmoa_s
   
   
   ! --------------------------------------------------------------------------
   ! timestep
   ! --------------------------------------------------------------------------
   chnft = chnf_s * exp(hnfmua_s * tflie) & ! net growth
         - drhnf_s                          ! grazing dreissena
   
   if (chnft < 0.0) then
      chnf_old = chnft
      chnft = (chnf_s / (chnf_s + abs(chnft - chnf_s))) * chnf_s
      call print_clipping("hnf", "chnft", chnf_old, chnft, "")
   endif
   
   ! --------------------------------------------------------------------------
   ! update return values
   ! --------------------------------------------------------------------------
   ! consumed bacteria
   hnfbac_s = hnfupa_s * chnf_s * tflie
   
   ! consumed oxygen during respiration
   ! assumption: In order to respirate 1mgC biomass it needs 3.2 mg oxygen
   ro2hnf_s = hnfrea_s * chnf_s * tflie
   
   ! increment of bsb due to dead hnf and excretion 
   bsbhnf_s = (hnfmoa_s + hnfexa_s) * chnf_s
   
   chnf_s = chnft
   return
end
