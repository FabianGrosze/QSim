!> Berechnung des Einflusses von Corophium auf das Phytoplankton
!! @author Volker Kirchesch
!! @date 10.10.2011
subroutine corophium(coro_s, coros_s, aki_s, agr_s, abl_s,  &
                     flae_s, elen_s, bsohlm_s, lboem_s,     &
                     itags, monats, uhrz, tflie,            &
                     coroi_s, corois_s,                     &
                     algcok_s, algcog_s, algcob_s,          &
                     control, jjj)
   
   use module_alloc_dimensions
     
   implicit none
   
   ! --- dummy arguments ---
   real,    intent(inout), dimension(5) :: coro_s   !< Corophium on channcel bed by generation [Ind/m2]
   real,    intent(inout), dimension(5) :: coros_s  !< Corophium on embankment by generation [Ind/m2]
   real,    intent(in)                  :: aki_s    !< Diatoms [mg/l]
   real,    intent(in)                  :: agr_s    !< Green algae[mg/l]
   real,    intent(in)                  :: abl_s    !< Cyanobacteria [mg/l]
   real,    intent(in)                  :: flae_s   !<
   real,    intent(in)                  :: elen_s   !<
   real,    intent(in)                  :: bsohlm_s !<
   real,    intent(in)                  :: lboem_s  !<
   integer, intent(in)                  :: itags    !< current day of qsim simulation
   integer, intent(in)                  :: monats   !< current month of qsim simulation
   real,    intent(in)                  :: uhrz     !< current time of qsim simulation in hours since midnight
   real,    intent(in)                  :: tflie    !< timestep [d]
   real,    intent(out)                 :: coroi_s  !< Corophium on channcel bed [Ind/m2]
   real,    intent(out)                 :: corois_s !< Corophium on embankment [Ind/m2]
   real,    intent(out)                 :: algcok_s !< consumption of diatoms by corophium [mg/l]
   real,    intent(out)                 :: algcog_s !< consumption of green algae by corophium [mg/l]
   real,    intent(out)                 :: algcob_s !< consumption of cyanobacteria by corophium [mg/l]
   logical, intent(in)                  :: control !< debugging
   integer, intent(in)                  :: jjj      !< debugging
   
   ! --- local variables ---
   integer :: date_qsim
   real    :: volfm3, hconf, corsg, corg, n_eggs, n_eggss, time_qsim
   
   real, parameter :: date_g1 = 105                 ! april 15th as day of year
   real, parameter :: date_g2 = 166                 ! june 15th as day of year
   real, parameter :: date_g3 = 227                 ! august 15th as day of year
   real, parameter :: filtration_rate = 0.005 * 24. ! filtration rate in l/(Ind*d)
   real, parameter :: lw1 = 5.5                     !
   real, parameter :: lw2 = 4.5                     !
   real, parameter :: alpha_hatching = 0.7          ! fraction of successfull hatching
   real, parameter :: alpha_female1 = 0.75          ! fraction of female individuals in generation 1
   real, parameter :: alpha_female2 = 0.65          ! fraction of female individuals in generation 2
   real, parameter :: mortality_rate0 = 0.01        ! mortality rate before date3
   real, parameter :: mortality_rate3 = 0.115       ! mortality rate for generation 3 after date3
   real, parameter :: mortality_rate4 = 0.23        ! mortality rate for generation 4 after date3
   real, parameter :: mortality_rate5 = 0.011       ! mortality rate for generation 5 after date3
   
   intrinsic :: epsilon

   
   ! current date in QSim simulation as day of year
   if (monats <= 2) then
      date_qsim = itags + 31 * (monats - 1)
   else
      date_qsim = (itags + 31 * (monats - 1) - int(0.4 * monats + 2.3))
   endif
   
   ! current time of QSim simulation in days since midnight
   time_qsim = uhrz / 24.
   
   
   ! population density [Ind/m2]
   coroi_s  = sum(coro_s)
   corois_s = sum(coros_s)
   
   ! absolute population in current stretch segment [Ind]
   corg  = coroi_s  * lboem_s  * elen_s * 2.
   corsg = corois_s * bsohlm_s * elen_s
   
   
   ! --------------------------------------------------------------------
   ! consumption of algae
   ! --------------------------------------------------------------------
   ! water volume filtered by population
   volfm3  = filtration_rate * (corg + corsg)* tflie / 1000.
   
   hconf = min(1., volfm3 / flae_s * elen_s)
   algcok_s = aki_s * hconf
   algcog_s = agr_s * hconf
   algcob_s = abl_s * hconf
   
   ! --------------------------------------------------------------------
   ! population dynamics
   ! --------------------------------------------------------------------
   if (date_qsim < date_g1) return
   
   if (date_qsim == date_g1 .and. time_qsim <= tflie + epsilon(tflie)) then
      ! (only at first timestep of day)
      
      ! generation 2 hatches
      n_eggs  = (-13.4 + 6.95 * lw1) * coro_s(1)  * alpha_female1
      n_eggss = (-13.4 + 6.95 * lw1) * coros_s(1) * alpha_female1
         
      coro_s(2)  = n_eggs  * alpha_hatching
      coros_s(2) = n_eggss * alpha_hatching
      
      ! generation 1 dies
      coro_s(1)  = 0.0
      coros_s(1) = 0.0
   
   
   else if (date_qsim >= date_g1 .and. date_qsim < date_g2) then
      ! generation 2 decreases
      coro_s(2)  = coro_s(2)  * exp(-mortality_rate0 * tflie)
      coros_s(2) = coros_s(2) * exp(-mortality_rate0 * tflie)
      
   else if (date_qsim == date_g2 .and. time_qsim <= tflie + epsilon(tflie)) then
      ! (only at first timestep of day)
      
      ! generation 3 hatches
      n_eggs  = (-13.4+6.95 * lw2) * coro_s(2)  * alpha_female2
      n_eggss = (-13.4+6.95 * lw2) * coros_s(2) * alpha_female2
      
      coro_s(3)  = n_eggs  * alpha_hatching
      coros_s(3) = n_eggss * alpha_hatching
      
      ! generation 2 decreases
      coro_s(2)  = coro_s(2)  * 0.3
      coros_s(2) = coros_s(2) * 0.3
   
   else if (date_qsim >= date_g2 .and. date_qsim < date_g3) then
      ! generation 2 decreases
      coro_s(2)  = coro_s(2)  * exp(-mortality_rate0 * tflie)
      coros_s(2) = coros_s(2) * exp(-mortality_rate0 * tflie)
      
      ! generation 3 decreases
      coro_s(3)  = coro_s(3)  * exp(-mortality_rate0 * tflie)
      coros_s(3) = coros_s(3) * exp(-mortality_rate0 * tflie)
   
   else if (date_qsim == date_g3 .and. time_qsim <= tflie + epsilon(tflie)) then
      ! (only at first timestep of day)
      
      ! generation 4 hatches
      n_eggs  = (-13.4+6.95 * lw1) * coro_s(2)  * alpha_female1
      n_eggss = (-13.4+6.95 * lw1) * coros_s(2) * alpha_female1
      
      coro_s(4)  = n_eggs  * alpha_hatching
      coros_s(4) = n_eggss * alpha_hatching
      
      ! generation 5 hatches
      n_eggs  = (-13.4+6.95 * lw2) * coro_s(3)  * alpha_female2
      n_eggss = (-13.4+6.95 * lw2) * coros_s(3) * alpha_female2
      
      coro_s(5)  = n_eggs  * alpha_hatching
      coros_s(5) = n_eggss * alpha_hatching
      
      ! generation 2 dies
      coro_s(2)  = 0.0
      coros_s(2) = 0.0
      
   else if (date_qsim >= date_g3) then
      ! generation 3 dies off
      coro_s(3)  = coro_s(3)  * exp(-mortality_rate3 * tflie)
      coros_s(3) = coros_s(3) * exp(-mortality_rate3 * tflie)
      if (coro_s(3)  < 1.) coro_s(3)  = 0.0
      if (coros_s(3) < 1.) coros_s(3) = 0.0
      
      ! generation 4 dies off
      coro_s(4)  = coro_s(4)  * exp(-mortality_rate4 * tflie)
      coros_s(4) = coros_s(4) * exp(-mortality_rate4 * tflie)
      if (coro_s(4)  < 1.) coro_s(4)  = 0.0
      if (coros_s(4) < 1.) coros_s(4) = 0.0
      
      ! generation 5 decreases
      coro_s(5)  = coro_s(5)  * exp(-mortality_rate5 * tflie)
      coros_s(5) = coros_s(5) * exp(-mortality_rate5 * tflie)
   endif
      
   return

end subroutine corophium
