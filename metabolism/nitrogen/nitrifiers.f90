subroutine nitrifiers(vx0_s, vx02_s, pfl_s, vph_s, tempw_s, vo2_s, vNH4_s, &
                      vNO2_s, rhyd_s, rau_s, tiefe_s, vmitt_s, hJNH4_s,    &
                      tflie, susn_s, susn2_s, pfln1_s, pfln2_s, sedx0_s,   &
                      bettn_s, go2n_s, susno_s, kontroll, jjj)
   
   use aparam,    only: ynmax1, ynmax2, stks2, stks1, bnmx1, bnks1, bnmx2, &
                        bnks2, anitr1, anitr2
   implicit none
   ! --- dummy arguments ---
   real, intent(inout)     :: vx0_s    !< nitrosomonas
   real, intent(inout)     :: vx02_s   !< nitrobacter
   real, intent(in)        :: pfl_s    !< macrophytes
   real, intent(in)        :: vph_s    !< pH
   real, intent(in)        :: tempw_s  !< water temperature [°C]
   real, intent(in)        :: vo2_s    !< oxygen
   real, intent(in)        :: vNH4_s   !< ammonium
   real, intent(in)        :: vNO2_s   !< nitrite
   real, intent(in)        :: rhyd_s   !<
   real, intent(in)        :: rau_s    !< friction
   real, intent(in)        :: tiefe_s  !< water depth
   real, intent(in)        :: vmitt_s  !< flow velocity
   real, intent(in)        :: hJNH4_s  !< sediment flux ammonium
   real, intent(in)        :: tflie    !< timestep [d]
   real, intent(out)       :: susn_s   !< ammonium, which is oxidised to nitrite
   real, intent(out)       :: susn2_s  !< nitrite , which is oxidised to nitrate
   real, intent(out)       :: pfln1_s  !< ammonium, which is oxidised to nitrite (on macrophytes)
   real, intent(out)       :: pfln2_s  !< nitrite , which is oxidised to nitrate (on macrophytes)
   real, intent(out)       :: sedx0_s  !<
   real, intent(out)       :: bettn_s  !<
   real, intent(out)       :: go2n_s   !< O2-Verbrauch durch Nitrifikation (NH4N -> NO3N)
   real, intent(out)       :: susno_s  !<
   logical, intent(in)     :: kontroll !< debugging
   integer, intent(in)     :: jjj      !< debugging
   
   ! --- local variables ---
   real              :: alphat, alphao, fph2n2, fph2n3, fph1n2, fph1n3, fvel
   real              :: pka, vNH3, kdn2, vhNO2, vmod, kd_n2
   real              :: ekx0, ekx02, yn, vx0t, vx02t, u3, bettf, betn2f, anitri 
   real              :: ust, csedn, csedn2, ceq, ceq2, sednit, sednt2
   real              :: zellv, qsgr, oc, oc0, wst
   real              :: delx0, delx2
   integer           :: ised, jsed
   
   real, parameter   :: khNO2_x1 = 5.e-5
   real, parameter   :: khNO2_x2 = 0.18
   real, parameter   :: khH3_x1  = 35.0
   real, parameter   :: kNH3_x2  = 0.75
   real, parameter   :: rhymo    = 0.00875
   real, parameter   :: g = 9.81             !@TODO (Schönung): Define `g` globally
   
   
   ! influence of temperature (Wolf)
   if (tempw_s < 15.) then
      alphat = 0.75 * 1.108**(tempw_s - 15.)
   else
      alphat = 1.5 / (((tempw_s - 32.) / 17.)**2 + 1)
   endif
   
   ! influence of oxygen (Hajek, Neumann, Bischoffsberger)
   alphaO = max(0.0, 1. - exp(-1.39 * (vo2_s - 0.5)))
   
   ! influence of pH
   if (vph_s < 0.0) then
      fph1n2 = 1.
      fph2n2 = 1.
      fph1n3 = 1.
      fph2n3 = 1.
   else
      ! TODO (schoenung): absolute zero is -273.15°C
      pka = 0.09018 + (2729.92/(273.16 + tempw_s))
      vNH3 = vNH4_s / (1. + 10**(pka - vph_s))
      fph1n3 = 1. / (1. + vNH3 / kNH3_X1)
      fph2n3 = 1. + vNH3 / kNH3_x2
      
      
      if (vx02_s > 0.0) then
         ! TODO (schoenung): absolute zero is -273.15°C
         KD_N2 = exp(-2300. / (273.16 + tempw_s))
         vhNO2 = vNO2_s / (1. + KD_N2 * 10**vph_s)
      
         fph1n2 = 1. + vhNO2 / khNO2_x1
         fph2n2 = 1. / (1. + vhNO2 / khNO2_x2)
      else
         fph1n2 = 1.
         fph2n2 = 1.
      endif
   endif
   
   ! influence of velocity
   vmod = abs(vmitt_s) * (rhymo / rhyd_s)**0.5
   fvel = 1. + (9.5 * (vmod - 0.045))
   
   ! -----------------------------------------------------------------------
   ! nitrosomonas
   ! ammonium -> nitrite (NH4N -> NO2N)
   ! -----------------------------------------------------------------------
   ekx0 = 0.06
   
   ! growth rate
   yn = ynmax1 * fph1n3  *(vNH4_s / (stks1 * fph1n2 + vNH4_s))
   yn = yn * alphaO * alphaT
      
   vx0t = vx0_s * exp(yn * tflie)
   susn_s = (vx0t - vx0_s) / ekx0

   if (susn_s > vNH4_s) then
      susn_s = vNH4_s
      vx0t = vx0_s + susn_s * ekx0
      if (vx0t <= 0.0 .or. vx0_s <= 0.0) then
         yn = 0.0
      else
         yn = (log(vx0t) - log(vx0_s)) / tflie
      endif
   endif
   
   ! mortality [1/d]
   anitri = anitr1 * (stks1 / (stks1 + vNH4_s)) * alphaT
   
   ! timestep
   vx0t = vx0_s * exp((yn-anitri) * tflie)
   if (vx0t < 0.0) then
      delx0 = vx0t  - vx0_s
      vx0t = (vx0_s/(vx0_s + abs(delx0))) * vx0_s
   endif
   
   ! --- Ammoniumoxidation auf Makrophyten ---
   U3 = 300.
   bettf = bnmx1 * fph1n3 * (vNH4_s/(vNH4_s + bnks1 * fph1n2)) * fvel * alphaO * alphaT
   pfln1_s = ((bettf * pfl_s)/(U3 * tiefe_s)) * tflie
   
   
   ! -----------------------------------------------------------------------
   ! nitrobacter
   ! nitrite -> nitrate (NO2N -> NO3N)
   ! -----------------------------------------------------------------------
   if (vx02_s <= 0.0) then
      susn2_s = 0.0
      vx02_s  = 0.0
      pfln2_s = 0.0
   else
      ekx02 = 0.02
      
      ! growth rate
      yn = ynmax2 * fph2n2 * (vno2_s / (STKS2 * fph2n3 + vNO2_s))
      yn = yn * alphaO * alphaT
      
      ! timestep
      vx02t = vx02_s * exp(yn * tflie)
      susn2_s = (vx02t - vx02_s) / ekx02
      if (susn2_s > vNO2_s) then
         susn2_s = vNO2_s
         vx02t = vx02_s + susn2_s * ekx0
         yn = (log(vx02t) - log(vx02_s)) / tflie
      endif
      
      ! mortality
      anitri = anitr2 *(stks2 / (stks2 + vNO2_s)) * alphaT
      
      ! timestep
      vx02t = vx02_s * exp((yn-anitri) * tflie)
      if (vx02t < 0.0) then
         delx2 = vx02t - vx02_s
         vx02t = (vx02_s/(vx02_s + abs(delx2))) * vx02_s
      endif
      
      ! --- Makrophythen ---
      betn2f = bnmx2 * fph2n2 * (vNO2_s/(vNO2_s + bnks2 * fph2n3)) * fvel * alphaO * alphaT
      pfln2_s = ((betn2f * pfl_s) / (U3 * tiefe_s)) * tflie
   endif
   
   ! -----------------------------------------------------------------------
   ! sedimentation
   ! -----------------------------------------------------------------------
   ust = (((1./rau_s)*sqrt(g)) / (tiefe_s**0.16667)) * abs(vmitt_s)
   ised = 4
   jsed = 1
   ZellV = 0.0
   call sedimentation(tiefe_s, ised, ust, qsgr, oc, Oc0, tflie, wst, jsed, ZellV, &
                      kontroll, jjj)
   
   ! --- Nitrosomonas ---
   csedn  = vx0_s  * 0.69
   ceq  = csedn  * qsgr
   sednit = max(0.0, (csedn  - ceq )) * oc
   
   vx0t  = vx0t  - sednit
   
   ! sedimentierte Nitrosomonasbiomasse [µg/l] (Ausgabe)
   sedx0_s = sednit * 1000.
   
   ! --- Nitrobacter ---
   csedn2 = vx02_s * 0.69
   ceq2 = csedn2 * qsgr
   sednt2 = max(0.0, (CSEDN2 - ceq2)) * oc
   
   vx02t = vx02t - sednt2
   
   ! -----------------------------------------------------------------------
   ! return values
   ! -----------------------------------------------------------------------
   ! O2-Verbrauch durch Nitrifikation
   if (vx02_s <= 0.0) then
      go2n_s  = (susn_s + pfln1_s) * 4.33
      susno_s = go2n_s
   else
      go2n_s  = 3.22 * (susn_s  + pfln1_s)  &
              + 1.11 * (susn2_s + pfln2_s)
      susno_s = 3.22 * susn_s + 1.11 * susn2_s 
   endif
   
   ! Ausgabewerte
   bettn_s = hJNH4_s * tflie * (1./(24. * tflie)) / tiefe_s
   
   vx0_s  = vx0t
   vx02_s = vx02t
end subroutine nitrifiers