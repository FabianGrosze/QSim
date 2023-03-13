! --------------------------------------------------------------------------- !
!  QSim - Programm zur Simulation der Wasserqualität                          !
!                                                                             !
!  Copyright (C) 2022                                                         !
!  Bundesanstalt für Gewässerkunde                                            !
!  Koblenz (Deutschland)                                                      !
!  http://www.bafg.de                                                         !
!                                                                             !
!  Dieses Programm ist freie Software. Sie können es unter den Bedingungen    !
!  der GNU General Public License, Version 3, wie von der Free Software       !
!  Foundation veröffentlicht, weitergeben und/oder modifizieren.              !
!                                                                             !
!  Die Veröffentlichung dieses Programms erfolgt in der Hoffnung, dass es     !
!  Ihnen von Nutzen sein wird, aber ohne irgendeine Garantie, sogar ohne die  !
!  implizite Garantie der Makrtreife oder der Verwendbarkeit für einen        !
!  bestimmten Zweck.                                                          !
!                                                                             !
!  Details finden Sie in der GNU General Public License.                      !
!  Sie sollten ein Exemplar der GNU General Public License zusammen mit       !
!  diesem Programm erhalten haben.                                            !
!  Falls nicht, siehe http://www.gnu.org/licenses/.                           !
!                                                                             !
!  Programmiert von                                                           !
!  1979 bis 2018   Volker Kirchesch                                           !
!  seit 2011       Jens Wyrwa, Wyrwa@bafg.de                                  !
! --------------------------------------------------------------------------- !

!> Entwicklung von Dreissena polymorph und deren Einfluss auf das Phytoplankton
!! @author Volker Kirchesch
!! @date 15.01.1996
subroutine dreissen(zdrei,zdreis,tempw,flae,elen,anze,                 &
                    ior,volfdr,akbcm,agbcm,aki,agr,algdrk,algdrg,      &
                    tflie,ro2dr,lboem,bsohlm,ss,vo2,ssdr,drfaek,       &
                    drfaeg,drfaes,gewdr,dlarvn,itags,monats,jahrs,     &
                    lait1,laim1,laid1,ilang,                           &
                    resdr,exdrvg,exdrvk,ssalg,drpfec,                  &
                    abl,exdrvb,abbcm,algdrb,drfaeb,                    &
                    idras,drmas,drakr,drbar,drmor,ffood,coroI,coroIs,  &
                    CHNF,drHNF,HNFdra,dlmax,dlmaxs,gwdmax,             &
                    sgwmue,fkm,FoptDe,mstr,azStr,                      &
                    kontroll,jjj)
                    
   implicit none
   
   integer     :: nrla1e, nrs, nrla1a, ndr, ilang
   integer     :: mstr, monats, lait1, laim1, laid1
   integer     :: jahrs, itags, ior
   real        :: x, w, water_volume
   real        :: uptm3, tflie
   real        :: stdpla, rres
   real        :: respbio
   real        :: rescm3, f_fecal
   real        :: hconvk, hconvg
   real        :: hconvb, hcont, hconf, hcond
   real        :: hconc2, hconc1, gewdts
   real        :: filtration_rate, foptd, foptde, food
   real        :: fki, filtered_volume, fgr
   real        :: fco, fcos, fcom, fbl
   real        :: exdrvz, excm3
   real        :: drrt3, drrt33, drrt2, drrt, drft
   real        :: drrt22, drrt1, drrt11
   real        :: dreisn, dreinm, dreing, draup
   real        :: dmorg, dlamor, dlafes, dgwmue
   real        :: dfmue, dfmues, dfemue
   real        :: ddrein, ddlarn
   logical     :: increment_time
   logical     :: kontroll !< debugging
   integer     :: jjj      !< debugging
   real        :: elen(1000),tempw(1000),flae(1000),zdrei(1000,4)
   real        :: zdreis(1000,4),volfdr(1000)
   real        :: akbcm(1000),agbcm(1000),fkm(1000)
   real        :: aki(1000),agr(1000),algdrk(1000),algdrg(1000)
   real        :: ro2dr(1000),vo2(1000),ssdr(1000),idras(1000,2)
   real        :: drmas(1000,2),drakr(1000,2),drbar(1000,2),drmor(1000,2)
   real        :: lboem(1000),bsohlm(1000),ss(1000)
   real        :: drfaek(1000),drfaeg(1000),drfaes(1000)
   real        :: gewdr(1000,4),ingestion_rate,dlarvn(1000),resdr(1000),exdrvg(1000)
   real        :: dlmax(1000),dlmaxs(1000),gwdmax(1000),exdrvk(1000)
   real        :: adrg(4),adrk(4),drss(4),drfeck(4),drfecg(4),drfecs(4)
   real        :: filaki(4),filagr(4)
   real        :: adrb(4),drfecb(4),filabl(4)
   real        :: filss(4),vofkop(4),filHNF(4),HNFdra(1000)
   real        :: ssalg(1000),drpfec(1000),sgwmue(1000),ffood(1000)
   real        :: coroI(1000),coroIs(1000)
   real        :: klmor,hcondb(4),hconds(4)
   real        :: CHNF(1000),drHNF(1000),drfaeb(1000)
   real        :: abl(1000),exdrvb(1000),abbcm(1000),algdrb(1000)
   integer     :: anze, azStr, i
   
   ! parameters
   real   , parameter :: zqz10        = 3.1       ! Q10 of T dependence of respiration
   real   , parameter :: ztmax        = 31.       ! T above which Dreissena has no additional respiration losses?
   real   , parameter :: ztopt        = 28.       ! optimal T for respiration?
   real   , parameter :: pgr          = 1.0       ! feeding preference for green algae
   real   , parameter :: pki          = 1.0       ! feeding preference for diatoms
   real   , parameter :: pbl          = 0.5       ! feeding preference for cyanobacteria
   real   , parameter :: qres         = 0.29      ! active respiration rate (assimilation dependent) (1 / d)
   real   , parameter :: rres0        = 0.0015    ! basal respiration rate (1 / d)
   real   , parameter :: f_excrete    = 0.064     ! excreted fraction of assimilated biomass
   real   , parameter :: r_o2_resp    = 5.59      ! oxygen consumption per biomass (mg o2 / mg) after Schneider
   real   , parameter :: C_adult      = 0.38      ! C:Biomass of Dreissena adults (gC / g)
   real   , parameter :: C_egg        = 3.35e-9   ! C:Biomass of Dreissena eggs   (gC / g)
   real   , parameter :: Cagr         = 0.48      ! C:Biomass of green algae      (gC / g)
   real   , parameter :: Caki         = 0.48      ! C:Biomass of diatoms          (gC / g)
   real   , parameter :: Cabl         = 0.48      ! C:Biomass of cyanobacteria    (gC / g)
   real   , parameter :: f_female     = 0.5       ! fraction of female Dreissena in population
   real   , parameter :: f_fit        = 0.25      ! fraction of fit/healthy Dreissena in population
   real   , parameter :: f_survival   = 0.75      ! egg/larval survivability?
   real   , parameter :: C_lim        = 0.01      ! C content at which Dreissena cease feeding
   real   , parameter :: klmorg       = 8.26      ! mortalilty of larvae over 22 days (mg) ?
   real   , parameter :: tdpla        = 22.       ! development period from egg to larva (d)?
   real   , parameter :: flai         = 0.52      ! fraction of weight loss attributed to egg production?
   integer, parameter :: nndr         = 2         ! number of Dreissena cohorts (0th, 1st)
   ! derived parameters
   real   , parameter :: f_production = f_survival * f_fit * f_female
   
   ! newly introduced internal variables (F. Grosse)
   real                  :: dt, dt1, dt2, dt3     ! times describing phase within reproduction period (days)
   real                  :: f_spawn, f_spawn_max  ! fraction(s) of biomass invested into reproduction
   real, dimension(nndr) :: delta_weight          ! weight loss of single Dreissena
   
   ! internal variables for Dreissena on slope/embankment and river bed, respectively
   ! index 1 - slope/embankment
   ! index 2 - river bed
   real, dimension(2)    :: habitat_size          ! area of slope/embankment and area of river bed (m2)
   real, dimension(2)    :: biomass_adult         ! areal integral of biomass of adult Dreissena (g)
   real, dimension(2)    :: uptake_rate           ! (mgC / d)
   real, dimension(2)    :: assimilation_rate     ! (mgC / d)
   real, dimension(2)    :: excretion_rate        ! (mgC / d)
   real, dimension(2)    :: respiration_rate      ! (mgC / d)
   real, dimension(2)    :: net_growth            ! (mgC)
   
   save drrt, drft, stdpla, increment_time
   
   ! Some useful information (units based on Gerris menu for stretch options)
   ! zdrei  ... Dreissena biomass on embankment (0th/1st cohort; g / m2)
   ! zdreis ... Dreissena biomass on river bed  (0th/1st cohort; g / m2)
   ! gewdr  ... weight of a single Dreissena individual (mgC)
   !
   ! drrt   ... time since start of simulation
   ! stdpla ... time since start of reproduction period
   ! drft   ... time since start of reproduction period + egg development period (tdpla)
   !
   ! all *s quantities refer to river bed values, while those w/o 's' refer to embankment
   
   ! simulation forerun (no calculations)
   if (ilang == 0) then
      dlarvn(anze+1) = dlarvn(anze)
      return
   endif
   
   ! optimal food concentration (mg C / L)
   FoptD = FoptDe
   if (FoptD == 0.0) FoptD = 1.2
   
   ! mortality rate of larvae (1 / d)
   klmor  = klmorg / tdpla
   
   ! drrt: time since start of simulation - only increment when handling 1st model stretch
   if (azStr == 1) then
      drrt = drrt + tflie
      ! switch for incrementing time
      increment_time = .true.
   endif
   
   ! calculate current day of year
   NRS = ITAGS + 31 * (MONATS - 1)
   if (monats > 2) NRS = NRS - INT(0.4 * real(MONATS) + 2.3)
   
   ! calculate start and end day of reproduction period (day of year)
   NRla1a = lait1 + 31 * (laim1 - 1)
   if (laim1 > 2) NRla1a = NRla1a - INT(0.4 * real(laim1) + 2.3)
   nrla1e = nrla1a + laid1
   
   do ior = 1,anze
      
      ! initialise output variables
      exdrvz      = 0.0
      exdrvg(ior) = 0.0
      exdrvk(ior) = 0.0
      exdrvb(ior) = 0.0
      resdr(ior)  = 0.0
      ro2dr(ior)  = 0.0
      
      if (elen(ior) == 0.0) then
         algdrg(ior) = 0.0
         algdrk(ior) = 0.0
         algdrb(ior) = 0.0
         volfdr(ior) = 0.0
         cycle
      endif
      
      ! calculate areas of slopes (hence, factor 2) and river bed
      habitat_size(1) = 2. *  lboem(ior) * elen(ior)
      habitat_size(2) =      bsohlm(ior) * elen(ior)
      
      ! calculate water volume
      water_volume = flae(ior) * elen(ior)
      
      ! calculate influence of Corophium on ingestion and filtration rates
      if (coroI(ior) > 0.0 .or. coroIs(ior) > 0.0) then
         fco  = max(0., min(1., 1. - (coroI(ior)  - 10000.) / 90000.))
         fcos = max(0., min(1., 1. - (coroIs(ior) - 10000.) / 90000.))
         hconc1 = habitat_size(1) * coroI(ior)
         hconc2 = habitat_size(2) * coroIs(ior)
         fcom   = (hconc1 * fco + hconc2 * fcos) / (hconc1 + hconc2)
      else
         fco  = 1.
         fcos = 1.
         fcom = 1.
      endif
      
      ! calculate available amount of food and food dependence of ingestion
      fgr  = agr(ior) * pgr * Cagr
      fki  = aki(ior) * pki * Caki
      fbl  = abl(ior) * pbl * Cabl
      food = fgr + fki + fbl
      
      ! food-related Michaelis-Menten kinetics
      if (food > C_lim) then
         hconf = min(1., food / FoptD)
      else
         hconf = 0.0
      endif
      ffood(ior) = hconf
      
      ! Michaelis-Menten food term for algae-specific uptake (mg / mgC)
      if (aki(ior) + agr(ior) + abl(ior) > 0.0) then
         hconvk = aki(ior) * pki / (Caki * (aki(ior) * pki + agr(ior) * pgr + abl(ior) * pbl))
         hconvg = agr(ior) * pgr / (Cagr * (aki(ior) * pki + agr(ior) * pgr + abl(ior) * pbl))
         hconvb = abl(ior) * pbl / (Cabl * (aki(ior) * pki + agr(ior) * pgr + abl(ior) * pbl))
      else
         hconvk = 0.0
         hconvg = 0.0
         hconvb = 0.0
      endif
         
      ! T dependence of respiration
      ! TODO FG: This does not seem to make sense: No additional respiratory losses if T >= ztmax?
      if (tempw(ior) < ztmax) then
         w = log(zqz10) * (ztmax - ztopt)
         x = (w / 20. * (1. + sqrt(1. + 40. / w)))**2
         hcont = ((ztmax - tempw(ior)) / (ztmax - ztopt) * exp(1. - (ztmax - tempw(ior)) / (ztmax - ztopt)))**x
      else
         hcont = 0.0
      endif
      
      ! loop over Dreissena cohorts
      do ndr = 1,nndr
         
         ! weight of one Dreissena (mgC according to Gerris menu)
         gewdr(ior,ndr)  = max(0., gewdr(ior,ndr))
         
         ! respiration of a single Dreissena (mgC / (g d))
         if (gewdr(ior,ndr) > 0.0) then
            rres = rres0 * gewdr(ior,ndr)**(-0.25)
         else
            rres = 0.0
         endif
         
         ! conversion of Dreissena biomass from g / m2 to g
         biomass_adult(1) = max(0., zdrei(ior,ndr)  * habitat_size(1))
         biomass_adult(2) = max(0., zdreis(ior,ndr) * habitat_size(2))
         
         ! calculate ingestion and uptake rates (mgC / d)
         if (gewdr(ior,ndr) > 0.0) then
            !Walz
            !      ingestion_rate = (0.1105*gewdr(ior,ndr)**(-0.213)) *
            !                       exp(-0.00605*(20.0-tempw(ior))**2)
            !modifizierte Walz
            !      ingestion_rate = (0.295*gewdr(ior,ndr)**(-0.636)) *
            !                       exp(-0.00605*(20.0-tempw(ior))**2)
            !nach Schneider
            !      ingestion_rate = (0.1271*gewdr(ior,ndr)**(-0.39)) *
            !                       exp(-0.00605*(20.0-tempw(ior))**2)
            !
            ingestion_rate = 0.249 * gewdr(ior,ndr)**(-0.615) * exp(-0.00605 * (20.0 - tempw(ior))**2) * hconf
            uptake_rate(1) = ingestion_rate * fco  * biomass_adult(1)
            uptake_rate(2) = ingestion_rate * fcos * biomass_adult(2)
            ingestion_rate = ingestion_rate * fcom
         else
            ingestion_rate = 0.0
            uptake_rate(:) = 0.0
         endif
         
         ! output ingestion rate
         idras(ior,ndr) = ingestion_rate
         
         ! fraction of uptake going into fecal pellet production
         f_fecal  = 0.315 * exp(0.88 * hconf)
         
         ! calculate rates (mgC / d) and net growth (mgC) for Dreissena on slopes and river bed, respectively
         do i = 1,2
            assimilation_rate(i) = (1. - f_fecal) * uptake_rate(i)
            excretion_rate(i)    = f_excrete * assimilation_rate(i)
            respiration_rate(i)  = rres * hcont * biomass_adult(i) + qres * assimilation_rate(i)
            net_growth(i)        = (assimilation_rate(i) - respiration_rate(i) - excretion_rate(i)) * tflie
            
            ! change of Dreissena biomass due to net growth
            ! TODO FG: If Gerris units are correct, this implies a summation of units g and mgC
            biomass_adult(i) = biomass_adult(i) + net_growth(i)
            if (i == 1) then
               zdrei(ior,ndr)  = biomass_adult(i)
            else
               zdreis(ior,ndr) = biomass_adult(i)
            endif
         enddo
         
         ! calculate filtration rate (m3 / (g d)) and filtrated volume (m3)
         if (gewdr(ior,ndr) > 0.0) then
            filtration_rate  = 9.24  * gewdr(ior,ndr)**(-0.392)                                   *   &
         &                     3.267 * exp(-0.037 * SSalg(ior) - 0.00605 * (20. - tempw(ior))**2) *   &
         &                     24. / 1000. * fcom
            filtered_volume  = filtration_rate * sum(biomass_adult) * tflie
         else
            ! no filtration
            filtered_volume = 0.0
         endif
         
         ! ingestion/assimilation-dependent respiration rate (1 / d)
         drakr(ior,ndr)    = (1. - f_fecal) * qres * ingestion_rate
         
         ! basal repiration rate (1 / d)
         drbar(ior,ndr)    = rres * hcont
         
         ! net growth rate ( 1 / d)
         drmas(ior,ndr)    = (1. - f_fecal) * (1. - f_excrete) * ingestion_rate - drakr(ior,ndr) - drbar(ior,ndr)
         
         ! overall weight change (mgC / d) of one Dreissena
         delta_weight(ndr) = gewdr(ior,ndr) * drmas(ior,ndr) * tflie
         gewdr(ior,ndr)    = gewdr(ior,ndr) + delta_weight(ndr)
         
         if (water_volume == 0.0) then
            dlarvn(anze+1) = dlarvn(anze)
            return
         endif
         
         ! respirated and excreted biomass (mgC / L)
         rescm3 = sum(respiration_rate) / water_volume
         excm3  = sum(excretion_rate)   / water_volume
         
         ! respirated biomass (mg / L)
         respbio    = rescm3 / C_adult * tflie
         resdr(ior) = resdr(ior) + respbio
         
         ! oxygen consumption due to respiration (mg O2 / L)
         ro2dr(ior) = ro2dr(ior) + respbio * r_o2_resp
         
         ! total excreted biomass (mgC / L)
         exdrvz      = exdrvz + excm3 * tflie
         
         ! excreted biomass due to grazing on the different algae (mg / L)
         exdrvg(ior) = exdrvz * hconvg
         exdrvk(ior) = exdrvz * hconvk
         exdrvb(ior) = exdrvz * hconvb
         
         ! uptake of the different algae and suspended matter (mg / L)
         uptm3 = sum(uptake_rate) / water_volume * tflie
         adrg(ndr) = uptm3 * hconvg
         adrk(ndr) = uptm3 * hconvk
         adrb(ndr) = uptm3 * hconvb
         drss(ndr) = 0.
         
         ! fecal pellet production (mg / L)
         drfecg(ndr) = f_fecal * adrg(ndr)
         drfeck(ndr) = f_fecal * adrk(ndr)
         drfecb(ndr) = f_fecal * adrb(ndr)
         drfecs(ndr) = f_fecal * drss(ndr)
         
         ! volume fraction (%) filtered by 'ndr-1'-th cohort
         filtered_volume = filtered_volume / water_volume
         vofkop(ndr)     = filtered_volume * 100.
         
         ! amount of filtered algae and suspended matter (in mg / L)
         filaki(ndr) = aki(ior) * pki * filtered_volume
         filagr(ndr) = agr(ior) * pgr * filtered_volume
         filabl(ndr) = abl(ior) * pbl * filtered_volume
         filss(ndr)  = ss(ior)  *       filtered_volume
         
         if (adrk(ndr) > 0.0 .and. adrk(ndr) > Filaki(ndr)) then
            filaki(ndr) = adrk(ndr)
            filagr(ndr) = adrg(ndr)
            filabl(ndr) = adrb(ndr)
            vofkop(ndr) = filaki(ndr) / (aki(ior) * pki) * 100.
         elseif (adrg(ndr) > 0.0 .and. adrg(ndr) > Filagr(ndr)) then
            filaki(ndr) = adrk(ndr)
            filagr(ndr) = adrg(ndr)
            filabl(ndr) = adrb(ndr)
            vofkop(ndr) = filagr(ndr) / (agr(ior) * pgr) * 100.
         elseif (adrb(ndr) > 0.0 .and. adrb(ndr) > Filabl(ndr)) then
            filaki(ndr) = adrk(ndr)
            filagr(ndr) = adrg(ndr)
            filabl(ndr) = adrb(ndr)
            vofkop(ndr) = filabl(ndr) / (abl(ior) * pbl) * 100.
         endif
         
         ! amount of filtered HNF
         filHNF(ndr) = CHNF(ior) * filtered_volume
         
      enddo
      
      ! set output variables
      algdrg(ior) = sum(filagr)
      algdrk(ior) = sum(filaki)
      algdrb(ior) = sum(filabl)
      ssdr(ior)   = sum(filss)
      drfaeg(ior) = sum(drfecg)
      drfaek(ior) = sum(drfeck)
      drfaeb(ior) = sum(drfecb)
      drfaes(ior) = sum(drfecs)
      volfdr(ior) = sum(vofkop)
      draup       = sum(adrg) + sum(adrk) + sum(adrb)
      drHNF(ior)  = sum(filHNF)
      
      if (CHNF(ior) > 0.0) then
         HNFdra(ior) = drHNF(ior) / CHNF(ior) * 24.
      else
         HNFdra(ior) = 0.0
      endif
      
      ! suspended matter uptake currently not implemented: reset to zero
      ssdr(ior) = 0.0
      
      ! Pseudo faeces fraction of Dreissena
      ! TODO FG: Whatever pseudo-faeces are meant to be ...
      if (algdrg(ior) + algdrk(ior) + algdrb(ior) > 0.0 .and. draup > 0.0) then
         drpfec(ior) = 1. - draup / (algdrg(ior) + algdrk(ior) + algdrb(ior))
         drpfec(ior) = max(0., drpfec(ior) * 100.)
      else
         drpfec(ior) = 0.0
      endif
      if (drpfec(ior) == 0.) volfdr(ior) = 0.0
      
      ! initialise values for larval production and losses
      ddlarn = 0.0
      dlamor = 0.0
      dlafes = 0.0
      
      ! start date of reproduction period not set, i.e. no egg and larvae production
      if (lait1 == 0 .and. laim1 == 0) cycle
         
      ! set maximum larvae production on slopes and bed (g)
      dlmax(ior)  = dlmax(ior)  * habitat_size(1)
      dlmaxs(ior) = dlmaxs(ior) * habitat_size(2)
      
      if (drft < laid1) then
         if (ilang == 0) then
            drrt = 0.0
            dlmax(ior)  = dlmax(ior)  / habitat_size(1)
            dlmaxs(ior) = dlmaxs(ior) / habitat_size(2)
            cycle
         endif
            
         ! set points in time describing production curve during reproduction period
         ! 1st part of curve (first 30 days of reproduction period, with subdivision halfway)
         drrt1 = 0.0
         drrt3 = 30.
         drrt2 = 0.5 * drrt3
         ! 2nd part of curve (30 days till end of reproduction period, with subdivision halfway)
         drrt11 = 0.0
         drrt33 = laid1 - drrt3
         drrt22 = 0.5 * drrt33
         
         if (nrs < nrla1a .or. nrs >= nrla1e) then
            ! outside of production period
            drrt = 0.0
         else
            ! set maximum weight loss of adults due to egg production to 1st cohorts adults' weight
            if (dlmax(ior) == 0.0 .and. dlmaxs(ior) == 0.0) then
               dlmax(ior)  = zdrei(ior,2)
               dlmaxs(ior) = zdreis(ior,2)
               gwdmax(ior) = gewdr(ior,2)
               sgwmue(ior) = 0.0
            endif
            
            ! set reference times depending on current phase within reproduction period
            if (drrt <= drrt3) then
               ! 1st part of production curve: time since start of reproduction period <= 30 days
               f_spawn_max = 0.6  ! maximum fraction of body weight invested into reproducton?
               dt  = drrt
               dt1 = drrt1
               dt2 = drrt2
               dt3 = drrt3
            else
               ! 2nd part of production curve: time since start of reproduction period > 30 days
               ! production is reduced relative to early reproduction phase (cf. 'spwmx')
               f_spawn_max = 0.4  ! maximum fraction of body weight invested into reproducton?
               dt  = drrt - drrt3
               dt1 = drrt11
               dt2 = drrt22
               dt3 = drrt33
            endif
            if (dt > dt2) dt1 = dt3
            ! calculate reproduction factor (i.e. fraction of biomass invested in egg production)
            f_spawn = 2. * flai / dt3 * f_spawn_max * (dt - dt1)**2 / ((dt - dt2)**2 + (dt - dt1)**2)
         
            ! NOTE: 0th cohort (ndr == 1) does not reproduce
            do ndr = 2,nndr
               gewdr(ior,ndr) = gewdr(ior,ndr) - gwdmax(ior) * tflie * f_spawn
            
               ! larval production over time step from weight loss of females
               ddlarn = (dlmax(ior) + dlmaxs(ior)) * tflie * f_spawn / C_egg * f_production
               
               ! additional larval production due to growth over time step
               ddlarn = ddlarn + max(0., sum(net_growth) * flai / C_egg * f_production)
               
               ! convert to mg / L
               ddlarn = ddlarn / (water_volume * 1000.)
               
               ! new weight after loss due to spawn production
               gewdr(ior,ndr) = gewdr(ior,ndr) - delta_weight(ndr) * flai
               
               ! biomass change due to net growth (g / m2)
               dgwmue = (net_growth(1) / habitat_size(1) + net_growth(2) / habitat_size(2)) * flai
               sgwmue(ior) = sgwmue(ior) + dgwmue
               
               ! biomass loss due to reproduction (g)
               zdrei(ior,ndr)  = zdrei(ior,ndr)  - dlmax(ior)  * tflie * f_spawn - net_growth(1) * flai
               zdreis(ior,ndr) = zdreis(ior,ndr) - dlmaxs(ior) * tflie * f_spawn - net_growth(2) * flai
            enddo
         endif
         
         ! calculate larval biomass loss due to mortality
         dlamor = dlarvn(ior) * (1. - exp(-klmor * tflie))
         
         ! calculate time since start of reproduction period
         if (nrs >= nrla1a .and. increment_time) stdpla = stdpla + tflie
         
         ! compare time since start of reproduction period to development period of larvae (tdpla)
         if (stdpla < tdpla) then
            ! no larvae yet
            drft = 0.0
            increment_time = .false.
         else
            ! increment time since first larvae hatched
            if (increment_time) then
               drft = drft + tflie
               increment_time = .false.
            endif
            
            ! set reference times depending on current phase within reproduction period
            if (drft <= drrt3) then
               ! 1st part of production curve: time since start of reproduction period <= 30 days
               f_spawn_max = 0.6  ! maximum fraction of body weight invested into reproducton?
               dt  = drft
               dt1 = drrt1
               dt2 = drrt2
               dt3 = drrt3
            else
               ! 2nd part of production curve: time since start of reproduction period > 30 days
               ! production is reduced relative to early reproduction phase (cf. 'spwmx')
               f_spawn_max = 0.4  ! maximum fraction of body weight invested into reproducton?
               dt  = drft - drrt3
               dt1 = drrt11
               dt2 = drrt22
               dt3 = drrt33
            endif
            if (dt > dt2) dt1 = dt3
            ! calculate hatching factor (i.e. fraction of larvae successfully developed from eggs)
            f_spawn = 2. * flai / dt3 * f_spawn_max * (dt - dt1)**2 / ((dt - dt2)**2 + (dt - dt1)**2)
            
            ! Larval weight when settling: 8.6e-8 gC; 8.6e-5 mgC <= not used
            dlafes = (dlmax(ior) + dlmaxs(ior)) * tflie * f_spawn / C_egg * f_production
         endif
      endif
      
      ! change in larval biomass due to maturation?
      if (drft >= laid1 .or. stdpla >= tdpla) then
         dfemue = sgwmue(ior) / tdpla * tflie
         if (zdreis(ior,2) > 0.0 .or. zdrei(ior,2) > 0.0) then
            sgwmue(ior) = sgwmue(ior) - dfemue
            dfmue       = dfemue * habitat_size(1) * (zdrei(ior,2)  / (zdrei(ior,2) + zdreis(ior,2)))
            dfmues      = dfemue * habitat_size(2) * (zdreis(ior,2) / (zdrei(ior,2) + zdreis(ior,2)))
            dfemue      = (dfmue + dfmues) / C_egg * f_production
         else
            dfemue = 0.0
         endif
         dlafes = dlafes + dfemue
         dlafes = dlafes * exp(-klmorg) / (water_volume * 1000.)
      endif
      dlarvn(ior) = max(0., dlarvn(ior) + ddlarn - dlamor - dlafes)
      
      do ndr = 1,nndr
         ddrein = 0.0
         
         hconds(ndr) = zdreis(ior,ndr)
         hcondb(ndr) = zdrei(ior,ndr)
         
         if (gewdr(ior,ndr) > 0.0) then
            dreisn = (zdrei(ior,ndr) + zdreis(ior,ndr)) * 1000. / gewdr(ior,ndr)
         else
            dreisn = 0.0
         endif
         dreing = dreisn
         
         if (ndr == 1 .and. dlafes > 0.0) then
            dreing = dreisn + dlafes * water_volume * 1000.
            gewdts = (dreisn * gewdr(ior,1) + dlafes * water_volume * 1000.* 8.6e-5) / dreing
            if (gewdts > 0.0246) then
               dlafes = dlafes * exp(-2. * tflie)
               dreing = dreisn + dlafes * water_volume * 1000.
               gewdts = (dreisn * gewdr(ior,1) + dlafes * water_volume * 1000.* 8.6e-5) / dreing
            endif
            gewdr(ior,1) = gewdts
         elseif (ndr == 2 .and. gewdr(ior,1) > 1.6) then
            ddrein        = (zdrei(ior,1) + zdreis(ior,1)) * 1000. / gewdr(ior,1)
            dreing        = dreisn + ddrein
            gewdr(ior,2)  = (dreisn * gewdr(ior,2) + ddrein * gewdr(ior,1)) / dreing
            gewdr(ior,1)  = 0.0
            zdrei(ior,1)  = 0.0
            zdreis(ior,1) = 0.0
         endif
         
         ! natural mortality rate (1 / d)
         if (gewdr(ior,ndr) < 0.0246) then
            dmorg = 0.1
         else
            dmorg = 0.0157 * gewdr(ior,ndr)**(-0.502)
         endif
         
         if (gewdr(ior,ndr) > 0.0) then
            drmor(ior,ndr) = dmorg
         else
            drmor(ior,ndr) = 0.0
         endif
         
         dreinm = dreing * (1. - exp(-dmorg * tflie))
         dreing = max(0., dreing - dreinm)
         
         if (ndr == 1 .and. zdrei(ior,ndr) + zdreis(ior,ndr) == 0.) then
            if (dlafes > 0.0) then
               hcond = zdrei(ior,2) / (zdrei(ior,2) + zdreis(ior,2))
            else
               zdrei(ior,ndr)  = 0.0
               zdreis(ior,ndr) = 0.0
               cycle
            endif
         elseif (ndr == 2 .and. zdrei(ior,ndr) + zdreis(ior,ndr) == 0.0) then
            if (ddrein > 0.0) then
               hcond = hcondb(1) / (hcondb(1) + hconds(1))
            else
               zdrei(ior,ndr)  = 0.0
               zdreis(ior,ndr) = 0.0
               cycle
            endif
         else
            hcond = hcondb(ndr) / (hcondb(ndr) + hconds(ndr))
         endif
         
         zdrei(ior,ndr)  = (dreing * gewdr(ior,ndr) / 1000.) *       hcond
         zdreis(ior,ndr) = (dreing * gewdr(ior,ndr) / 1000.) * (1. - hcond)
         
      enddo
      
      ! convert Dreissena biomass back from g to g / m2
      do ndr = 1,nndr
         if (habitat_size(1) > 0.) then
            zdrei(ior,ndr) = zdrei(ior,ndr) / habitat_size(1)
         else
            zdrei(ior,ndr) = 0.
         endif
         if (habitat_size(2) > 0.) then
            zdreis(ior,ndr) = zdreis(ior,ndr) / habitat_size(2)
         else
            zdreis(ior,ndr) = 0.
         endif
      enddo
      
      dlmax(ior)  = dlmax(ior)  / habitat_size(1)
      dlmaxs(ior) = dlmaxs(ior) / habitat_size(2)
      
   enddo
   
   dlarvn(anze+1) = dlarvn(anze)
   
   return
end
