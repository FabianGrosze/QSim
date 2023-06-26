subroutine water_temperature(tempw_s, tsed_s, templ_s, ro_s,  &
                        schwi_s, wtyp_s, cloud_s, extk_s,     &
                        extks_s, wlage_s, wge_s, hws_s,       &
                        tiefe_s, wuebk_s, spewks_s, psrefs_s, &
                        iform_verdr, dh2d, tflie,             &
                        control, jjj)
   implicit none
   ! TODO (Schönung, June 2023):
   ! This subroutine must not alter extk_s
   ! --- dummy arguments ---                       
   real,    intent(inout) :: tempw_s     !< water temperature [°C]
   real,    intent(inout) :: tsed_s      !< sediment temperature[°C]
   real,    intent(in)    :: templ_s     !< air temperature [°C]
   real,    intent(in)    :: ro_s        !< relative humidity [%]
   real,    intent(in)    :: schwi_s     !< global radiation [cal/(cm2*h)]
   real,    intent(in)    :: wtyp_s      !< cloud reflectance(?) derived from cloud type [-]
   real,    intent(in)    :: cloud_s     !< cloud cover [okta]
   real,    intent(inout) :: extk_s      !< light absorbance [1/m]
   real,    intent(in)    :: extks_s     !< light absorbance as given in ModellG [1/m]
   real,    intent(in)    :: wlage_s     !< height of weather station [m above sealevel]
   real,    intent(in)    :: wge_s       !< Windgeschwindigkeit [m/s]
   real,    intent(in)    :: hws_s       !< water level [m above sea level]   
   real,    intent(in)    :: tiefe_s     !< water depth [m]
   real,    intent(in)    :: wuebk_s     !< heat transfer coefficient water/sediment [kJ/(K*m2*h)]
   real,    intent(in)    :: spewks_s    !< specific heat capacity of sediment [kJ/(kg*K)]
   real,    intent(in)    :: psrefs_s    !< reflectance of sediment [-]
   real,    intent(in)    :: dh2d
   integer, intent(in)    :: iform_verdr !< switch to select evaporation equation
   real,    intent(in)    :: tflie       !< timestep [d]
   logical, intent(in)    :: control     !< debugging
   integer, intent(in)    :: jjj         !< debugging
   
   ! --- local variables ---
   integer           :: itime, itimes, jiter, ilauf, iouter
   real              :: counterradiation, radiant_exitance, cloudiness, tempw1, tempw2, absorbed_par
   real              :: atkor, saturated_vapor_pressure, partial_vapor_pressure, pdltt, f_wind, q_radiation, evaporation_rate
   real              :: evaporation_heat, q_evaporation, q_convection, delta_temp, dt2d, dt1, heat_capacity_sediment
   real              :: sedhw,  dtw, depth, cv_t, p, gamma_t
   real              :: heat_flux_sediment, delta_temp_sediment, absorbed_counter_par, dt1d_it, dtemp1
   real              :: slope_t, qn, b1, a3, b3, heat_flux_max
   real              :: tempw_in, f_extk, wind_speed, heat_capacity_water, dtime
   real              :: wuebk, spewks, psrefs
   double precision  :: par_at_sediment
   character(1000)   :: message
   
   ! TODO (Schönung, June 2023):
   ! This constant is wrong. According to Wikipedia it is
   ! sigma = 5.670374419e-08 W/(m2 K4) =  2.041335e-07 kJ/(m2 K4 h)
   ! hence, the sigma given here is wrong both in value and unit!
   real, parameter :: sigma_sb = 2.0411e-7 ! Stefan-Boltzman-constant [kJ /(m2*K4)]
   real, parameter :: rho_water = 1000.    ! density water [kg/m3]
   real, parameter :: alpha_par = 0.45     ! proportion of PAR in global radiation [-]
   real, parameter :: sedpv = 0.5
   real, parameter :: cp_water = 4.187     ! specific heat capacity of water [kJ/(kg*K)]
   real, parameter :: rho_sediment = 1825. ! density sediment [kg/m3]
   real, parameter :: cp_air   = 1.005     ! specific heat capacity of air  [kJ/(kg*K)]
   real, parameter :: nu = 0.622           ! molar mass ratio water:air
   real, parameter :: p0 = 6.1078          ! vapor pressure at 0°C [mbar]
   
   external :: qerror
   
   ! -------------------------------------------------------------------------
   ! setup
   ! -------------------------------------------------------------------------
   ! TODO (Schönung, June 2023)
   ! These values should not be altered here, but already when reading ModellG
   if (extk_s <= 0.0 .and. extks_s  > 0.0) extk_s = extks_s
   if (extk_s <= 0.0 .and. extks_s <= 0.0) extk_s = 1.5 
   
   if (wuebk_s > 0.0) then
      wuebk = wuebk_s
   else 
      wuebk = 350.
   endif
   
   if (spewks_s > 0.0) then
      spewks = spewks_s
   else
      spewks = 0.8
   endif
      
   if (psrefs_s > 0.0) then
      psrefs = psrefs_s
   else
      psrefs = 0.8
   endif
      
   depth = max(0.01, tiefe_s) 
   
   dtemp1 = 0.0
   itimes = 1
   ilauf = 0
   
   tempw_in = tempw_s
   tempw1 = tempw_s
   tempw2 = tempw_s

   ! correction of wind speed
   if (wlage_s - hws_s <= 0.0) then
      wind_speed = wge_s
   else
      wind_speed = wge_s * (2./(wlage_s-hws_s))**0.11
   endif
   
   ! TODO (Schönung, June 2023)
   ! What the heck is going on with all these loops? Are they really needed?
   outerloop: do iouter = 1,100
      timeloop: do itime = 1,itimes
      
         absorbed_counter_par = 0.0
         heat_flux_sediment = 0.0
         
         ! start iteration loop
         iterloop: do jiter = 1,50 
            
            dtime = tflie * 24. / itimes
            ! ----------------------------------------------------------------
            ! radiation
            ! ----------------------------------------------------------------
            cloudiness = 1. + wtyp_s * (cloud_s / 8.)**2.6
            counterradiation = 9.37e-6 * sigma_sb * (templ_s+273.15)**6
            counterradiation = counterradiation * cloudiness * 0.97 /42.
            
            ! factor for light extinction
            ! TODO (Schönung, June 2023)
            ! Why is this limited to -25? This causes two issues:
            ! a) This means at a certain point it does not
            !    get darker even if turbidtiy is still increasing.
            ! b) In ModellG the user can set `extk_s` explicitly. But this value 
            !    is overwritten here and calculations differ from the users
            !    expectation
            f_extk = exp(max(-25., -extk_s * depth))
            
            ! convert kj/m2/h in cal/cm2/h
            radiant_exitance = 0.97 * sigma_sb * ((tempw1+273.15)**4)
            radiant_exitance = radiant_exitance / 42.
            absorbed_par = schwi_s * alpha_par * (1. - f_extk)
            q_radiation = schwi_s * (1 - alpha_par) &
                        + absorbed_par              &
                        + counterradiation          &
                        - radiant_exitance 
            
            ! --------------------------------------------------------------
            ! evaporation
            ! --------------------------------------------------------------
            ! TODO (Schönung, June 2023)
            ! Why is there a lower boundary for evaporation heat?
            ! This means evaporation  occurs all the time, even if current weather
            ! conditions would prevent it
            evaporation_heat = max(2400., 2501. - 2.361 * tempw1)
            atkor = exp(-9.81*hws_s/(287.*(templ_s + 273.15)))
            saturated_vapor_pressure = p0 * exp(17.08085 * tempw1  / (234.175 + tempw1))
            partial_vapor_pressure   = p0 * exp(17.08085 * templ_s / (234.175 + templ_s))
            pdltt = ro_s * partial_vapor_pressure / 100.
            
            select case (iform_verdr)
               ! evaporation rate [m/h]
               case(1) ! WMO (FGSM-Handbuch)
                  f_wind = 0.13+0.0936* wind_speed
                  evaporation_rate = (f_wind*(saturated_vapor_pressure-pdltt)*atkor)/24000.
               
               case(2) ! Sweers (1976) over Land
                  f_wind = 0.153+0.063* wind_speed
                  evaporation_rate = (f_wind*(saturated_vapor_pressure-pdltt)*atkor)/24000.
               
               case(3) ! Rimsha & Donschenko
                  f_wind = 0.211+0.103* wind_speed
                  evaporation_rate = (f_wind*(saturated_vapor_pressure-pdltt)*atkor)/24000.
               
               case(4) ! nach Priestley-Taylor (1972)
                  cv_t = evaporation_heat
                  p = 1013. * ((293.-0.0065*hws_s)/293.)**5.26
                  gamma_t = cp_air * p / (cv_t * nu)
                  slope_t = (0.04145*exp(0.06088*templ_s))*10.
                  qn = q_radiation*42.+absorbed_counter_par-heat_flux_sediment
                  evaporation_rate = slope_t/(slope_t+gamma_t)*(abs(qn)/(cv_t*rho_water))
                  b1 = 2.805
                  evaporation_rate = evaporation_rate*((saturated_vapor_pressure-pdltt)/saturated_vapor_pressure) * b1
               
               case(5) ! Delclaux et al. (2007)
                  a3 = 0.04
                  b3 = 27.375
                  cv_t = evaporation_heat
                  qn = q_radiation*42.+absorbed_counter_par-heat_flux_sediment
                  evaporation_rate = (templ_s+b3)*abs(qn)/(cv_t*rho_water)
                  evaporation_rate = evaporation_rate * ((saturated_vapor_pressure-pdltt)/saturated_vapor_pressure)*a3
               
               case default
                  write(message, "(a,i0)") "subroutine water_temperature: The &
                     &given value for 'iform_verdr' is invalid: ", iform_verdr
                  call qerror(message)
            end select
            
            ! [cal/(cm2*h)]
            q_evaporation = rho_water * evaporation_heat * evaporation_rate / 42.
            
            ! -------------------------------------------------------------
            !  convection
            ! -------------------------------------------------------------
            if (saturated_vapor_pressure - pdltt == 0.0) saturated_vapor_pressure = saturated_vapor_pressure + 0.001
            q_convection = ((tempw1-templ_s) / (1.53 * (saturated_vapor_pressure-pdltt))) * q_evaporation
            
            ! -------------------------------------------------------------
            !  sediment
            ! -------------------------------------------------------------
            ! PAR at riverbed [kJ/(m2 h)]
            par_at_sediment = schwi_s * alpha_par * f_extk * 42.
            absorbed_counter_par = par_at_sediment * psrefs  * (1.- f_extk)
            
            ! volumetric heat capacity [kJ/(m3 K)]
            heat_capacity_sediment = sedpv * rho_water * cp_water + (1-sedpv) * rho_sediment * spewks
            heat_capacity_water = rho_water * cp_water
            sedhw = depth * heat_capacity_sediment / heat_capacity_water
            if (sedhw < 0.6) sedhw = 0.6
            
            ! calculation of heatflux from difference in temperature
            ! between water and sediment
            heat_flux_sediment = wuebk * (tempw1 - tsed_s) 
            delta_temp_sediment = (par_at_sediment * (1. - psrefs) + heat_flux_sediment) / (heat_capacity_sediment * sedhw) * dtime
            
            ! neu eingeführt damit die Änderung dTW durch Wärmeübergang "heat_flux_sediment"
            ! nicht grösser ist als tempw1-tsed_s. Kann bei sehr kleinen Tiefen
            ! zum Programmabsturz führen
            heat_flux_max = abs((tempw1 - tsed_s) * wuebk * heat_capacity_water * depth)
            heat_flux_sediment = min(heat_flux_max, abs(heat_flux_sediment))
            if (tempw1 < tsed_s) heat_flux_sediment = -1. * abs(heat_flux_sediment)
            
            ! -------------------------------------------------------------
            ! timestep  
            ! -------------------------------------------------------------
            ! Change in water temperature due to interaction with sediment [K]
            dtw = (absorbed_counter_par-heat_flux_sediment) / (heat_capacity_water*depth) * dtime
            
            ! Change in water temperature  [K]
            ! TODO (Schönung, June 2023):
            ! Why is this multiplied by 100? It must be multiplied by `heat_capacity_water`
            delta_temp = (q_radiation-q_evaporation-q_convection) * dtime / (depth * 100.)
            dt2d = (q_radiation-q_evaporation-q_convection) * dtime / (dh2d * 100.)
            dt1d_it = max(abs(delta_temp), abs(dtw), abs(delta_temp+dtw))
            
            delta_temp = delta_temp + dtw
            tempw1 = tempw2 + delta_temp
            dt1 = abs(delta_temp - dtemp1)
            
            
            if (dt1d_it > 1.5) exit timeloop
            if (dt1 <= 0.001) exit iterloop
            
            ! initialize next step in iterloop
            dtemp1 = delta_temp
            tempw1 = (tempw2 + tempw1) / 2.
            
         enddo iterloop
         
         
         if (jiter > 50 .and. dt1 > 0.001) exit timeloop
         
         ! initialize next step in timeloop
         tempw2 = max(0.001, tempw2 + dt2d)
         tempw1 = tempw2
         tsed_s = tsed_s + delta_temp_sediment
      
      enddo timeloop
      
      ! TODO (Schönung, June 2023)
      ! Is this a very weird way of programming a recursive function? If so, it 
      ! must be rewritten in a proper way.
      if (dt1d_it <=  1.5) exit outerloop
      
      ! restart timeloop with new loop counter
      if (ilauf == 0) then
         itimes = int(2. * dt1d_it) + 1
         ilauf = 1
      else
         ! TODO (Schönung, June 2023)
         ! `itimes` is an integer! Does it make sense to multiply with 1.25?
         itimes = itimes * 1.25
      endif
      
      tempw1 = tempw_in
      tempw2 = tempw_in
      
   enddo outerloop
  
   
   ! TODO (Schönung, June 2023)
   ! This is not the actual change in temperature during the timestep `tflie`
   ! In the calculations above QSim checks if change in temperature is greater 
   ! than 1.5K - if so, it subdivides the timestep and calculates again. So
   ! `delta_temp` is the change of temperature within one of these 
   ! subtimesteps. All the substep-changes must be added up in order to get the
   ! total change - but this step is missing in this subroutine
   tempw_s = tempw_s + delta_temp 
   if (tempw_s < 0.0) tempw_s = 0.001
   
   
   ! TODO (Schönung, June 2023):
   ! Why is this reset to a missing value? This way it gets recalculated again
   ! in other subroutines. That means other subroutines may use a different
   ! extinction coefficient, even though it is the same point and date.
   extk_s = -1.

end subroutine water_temperature
