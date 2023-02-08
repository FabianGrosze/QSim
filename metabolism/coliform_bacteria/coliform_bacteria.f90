subroutine coliform_bacteria(coli_s, doscf_s, extks_s, tempw_s, &
                             rau_s, tiefe_s, vmitt_s, schwi_s,  &
                             tflie,                             &
                             kontroll, jjj)

   use aparam, only: rateci, xnuec, ratecd, etacd, ratecg, ratecs
   implicit none

   ! --- dummy arguments ---
   real, intent(inout)  :: coli_s      !< coliform bacteria
   real, intent(inout)  :: doscf_s     !<
   real, intent(in)     :: extks_s     !< 
   real, intent(in)     :: tempw_s     !< temperature [°C]
   real, intent(in)     :: rau_s       !< friction
   real, intent(in)     :: tiefe_s     !< waterdepth
   real, intent(in)     :: vmitt_s     !< velocity
   real, intent(in)     :: schwi_s     !< global radiation [cal/(cm2*h)]
   real, intent(in)     :: tflie       !< timestep [d]
   logical, intent(in)  :: kontroll    !< debugging
   integer, intent(in)  :: jjj         !< debuggin

   ! --- local variables ---
   real  :: fn, ust
   real  :: a, xmuet, pars, tlip, vlicht, vges, parsw_j, parsw_mj, doscft
   real  :: vrc, colit, decoli, extk

   real, parameter   :: g = 9.81 
   real, parameter   :: apara = 0.45   ! Anteil des PARS Strahlung an der Globalstrahlung


   ! TODO (Schönung, November 2022) This subroutine must not alter extk_s!
   ! If extks_s is defined in ModellG this value will be used, otherwise it uses
   ! a default value
   if (extks_s >  0.0) then   
      extk = extkS_s
   else
      ! 0.17 reines Wasser; 0.13 Schwebstoffe; 0.094 Ki; 0.0145 Gr
      extk  = 1.5  
   endif

   ! Berechnung der Schubspannungsgeschwindigkeit
   ! TODO (Schönung, December 2022) 
   ! This is calculated over and over again in different subroutines.
   ! It should be replaced with a globally available function.
   fn = 1./rau_s
   ust = ((fn*g**0.5) / tiefe_s**0.166667) * abs(vmitt_s)

   ! Berechnung des mittleren vertikalen Dispersionskoeffizient
   ! nach Fischer im ein-dimensionalen Fall (gute Näherung)
   ! TODO (Schönung, December 2022)
   ! Light must not be calculated within this subroutine. Light is needed elsewhere
   ! in the programm as well, so it should be calculated in a central place and
   ! available to all subroutines dealing with light (i.e. macrophytes and algae)
   xmuet = 0.4 * ust * tiefe_s / 6.
   pars = max(0.0001, schwi_s * 4.2 * apara)   ! J/cm2/h
   tlip = log(PARS) / extk
   if (tlip < 0.001) tlip = 0.001
   if (tlip > tiefe_s) tlip = tiefe_s

   if (xmuet > 0.0) then
     vlicht = tlip**2    / xmuet
     vges   = tiefe_s**2 / xmuet
   else
     vlicht = 0.0
     vges   = 0.0
   endif

   if (pars <= 0.0001) then
      doscft = 0.0
   else if (vges > 0.0) then
      parsw_j = pars*(1./(tlip * extk)) * (1.-exp(-extk * tlip))
      parsw_mj = parsw_j * 0.01 * tflie * 24.       ! MJ*m-2
      doscft = doscf_s + parsw_mj * vlicht/vges
   else
     doscft = doscf_s
   endif

   ! sinks
   vrc = ratecd * etacd**(tempw_s - 20.) & ! mortality
       + ratecg                          & ! grazing
       + ratecs                            ! sedimentation

   colit = coli_s * (1.-(1.-exp(-rateci*doscft))**xnuec)*exp(-vrc * tflie)

   if (colit < 0.0) then
     decoli = colit - coli_s
     colit = (coli_s/(coli_s+abs(decoli)))*coli_s
   endif

   ! --------------------------------------------------------------------------
   ! update return values
   ! --------------------------------------------------------------------------
   coli_s  = colit
   doscf_s = doscft

end subroutine coliform_bacteria