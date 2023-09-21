! --------------------------------------------------------------------------- !
!  qsim - programm zur simulation der wasserqualität                          !
!                                                                             !
!  copyright (c) 2022                                                         !
!  bundesanstalt für gewässerkunde                                            !
!  koblenz (deutschland)                                                      !
!  http://www.bafg.de                                                         !
!                                                                             !
!  dieses programm ist freie software. sie können es unter den bedingungen    !
!  der gnu general public license, version 3, wie von der free software       !
!  foundation veröffentlicht, weitergeben und/oder modifizieren.              !
!                                                                             !
!  die veröffentlichung dieses programms erfolgt in der hoffnung, dass es     !
!  ihnen von nutzen sein wird, aber ohne irgendeine garantie, sogar ohne die  !
!  implizite garantie der makrtreife oder der verwendbarkeit für einen        !
!  bestimmten zweck.                                                          !
!                                                                             !
!  details finden sie in der gnu general public license.                      !
!  sie sollten ein exemplar der gnu general public license zusammen mit       !
!  diesem programm erhalten haben.                                            !
!  falls nicht, siehe http://www.gnu.org/licenses/.                           !
!                                                                             !
!  programmiert von                                                           !
!  1979 bis 2018   volker kirchesch                                           !
!  seit 2011       jens wyrwa, wyrwa@bafg.de                                  !
! --------------------------------------------------------------------------- !
subroutine zooplankton(zooind_s,                                 &
                       aki_s, agr_s, abl_s,                      &
                       tempw_s, vo2_s,                           &
                       tflie,                                    &
                       abszo_s, ir_s, zexki_s, zexgr_s, zexbl_s, &
                       dzres1_s, dzres2_s,                       &
                       algzob_s, algzog_s, algzok_s,             &
                       rmuas_s, rakr_s, rbar_s, iras_s,          &
                       kontroll, jjj)
   
   use aparam
   
   implicit none
   
   ! --- dummy variables ---
   real, intent(inout) :: zooind_s   !< rotifers [ind/l]
   real, intent(in)    :: aki_s      !< diatoms [mg/l]
   real, intent(in)    :: agr_s      !< green algae [mg/l]
   real, intent(in)    :: abl_s      !< cyanobacteria [mg/l]
   real, intent(in)    :: tempw_s    !< water temperature [°c]
   real, intent(in)    :: vo2_s      !< oxygen [mg/l]
   real, intent(in)    :: tflie      !< timestep [d]
   real, intent(out)   :: abszo_s    !< dead rotifers [mg/l]
   real, intent(out)   :: ir_s       !< ingestion rate [mg/l]
   real, intent(out)   :: zexki_s    !< excretion of diatoms [mg/l]
   real, intent(out)   :: zexgr_s    !< excretion of green algae [mg/l]
   real, intent(out)   :: zexbl_s    !< excretion of cyanobacteria [mg/l]
   real, intent(out)   :: dzres1_s   !< basal respiration [mg/l]
   real, intent(out)   :: dzres2_s   !< aktive respiration [mg/l]
   real, intent(out)   :: algzok_s   !< ingested diatoms [mg/l]
   real, intent(out)   :: algzog_s   !< ingested green algae [mg/l]
   real, intent(out)   :: algzob_s   !< ingested cyanobacteria [mg/l]
   real, intent(out)   :: rmuas_s    !< net growth rate [1/d]
   real, intent(out)   :: rakr_s     !< active respiration rate [1/d]
   real, intent(out)   :: rbar_s     !< basal respiration rate [1/d]
   real, intent(out)   :: iras_s     !< ingestion rate [1/d]
   logical, intent(in) :: kontroll   !< debugging
   integer, intent(in) :: jjj        !< debugging
      
   ! --- locale variables ---
   real :: abl_filterable, agr_filterable, aki_filterable, algae_filterable_total
   real :: alpha_abl, alpha_agr, alpha_aki
   real :: f_food, f_temp, f_oxy
   real ::  ingestion_rate, mortality_rate, respiration_rate_basal
   real :: ingested_algae, assimilated_biomass, excreted_biomass, respirated_biomass_basal
   real :: dead_rotifers, loss_rotifers, respirated_biomass
   real :: alpha_assimilation, rotifers, rotifers_new
   
   real, parameter ::  q10_ingestion = 2.16          ! q10-value ingestion [-]
   real, parameter ::  alpha_respiration = 0.203     ! fraction active respiration [-]
   real, parameter ::  alpha_assimilation_max = 0.84 ! max. fraction assimilation [-] (verschoor et al., 2007)
   real, parameter ::  exp_assimilation = 0.705      ! exponent fraction assimilation [-] (verschoor et al., 2007)
   real, parameter ::  q10_respiration_basal = 3.11  ! q10-value basal respiration [-]
   real, parameter ::  oxygen_critical = 2.5         ! critical oxygen concentration for rotifer growth [mg/l]
   real, parameter ::  mortality_max = 0.15          ! max. mortality rate rate at 20°c [1/d]
   real, parameter ::  exp_mortality = 2.0           ! exponent mortality rate [-]
   real, parameter ::  q10_mortality = 2.10          ! q10-value mortality rate [-]
   
   ! =========================================================================
   ! convert ind/l to mg/l
   rotifers = zooind_s * grot / 1000.
   
   ! concentrationen of filterable algae  [mg/l]
   abl_filterable = abl_s * zabl
   agr_filterable = agr_s * zagr
   aki_filterable = aki_s * zaki
   algae_filterable_total = abl_filterable + agr_filterable + aki_filterable
   
   
   if (algae_filterable_total <= epsilon(algae_filterable_total)) then
      alpha_abl = 0.0
      alpha_agr = 0.0
      alpha_aki = 0.0
   else
      alpha_abl = abl_filterable / algae_filterable_total
      alpha_agr = agr_filterable / algae_filterable_total
      alpha_aki = aki_filterable / algae_filterable_total 
   endif
   
   ! -------------------------------------------------------------------------
   ! turnover
   ! -------------------------------------------------------------------------
   ! --- ingestion ----
   ! influence of food [-]
   f_food = algae_filterable_total / (km_rot + algae_filterable_total)
   
   ! temperature dependency of ingestion [-]
   f_temp = q10_ingestion**((tempw_s - 20.) / 10.)
   
   ! ingestion rate [1/d]
   ingestion_rate = imax_rot * f_food * f_temp
   
   ! ingested algae [mg/l]
   ingested_algae = rotifers * exp(ingestion_rate * tflie) - rotifers
   ingested_algae = min(ingested_algae, algae_filterable_total)
   
   ! --- assimilation and excretion ---
   ! fraction of assimilation [-]
   alpha_assimilation = alpha_assimilation_max * exp(-1. * exp_assimilation * f_food)
   
   ! excreted_biomass [mg/l]
   excreted_biomass = (1. - alpha_assimilation) * ingested_algae
   
   ! assimiliated biomass [mg/l]
   assimilated_biomass = alpha_assimilation * ingested_algae
   
   ! --- respiration ----
   ! temperature dependency [-]
   f_temp = q10_respiration_basal**((tempw_s - 20.) / 10.)
   
   ! basal respiration rate [1/d]
   respiration_rate_basal = resp0_rot * f_temp
   
   ! basally respirated biomass [mg/l]
   respirated_biomass_basal = rotifers * exp(respiration_rate_basal * tflie) - rotifers
   
   ! actively respirated biomass [mg/l]
   respirated_biomass = alpha_respiration * assimilated_biomass
   
   ! --- mortality ----
   ! influence of oxygen [-]
   f_oxy = max(0., min(vo2_s / oxygen_critical, 1.))
   
   ! temperature dependency [-]
   f_temp = q10_mortality**((tempw_s - 20.) / 10.)
   
   ! mortality_rate rate [1/d]
   mortality_rate = mortality_max * f_temp * exp(-1. * exp_mortality * f_oxy)
   
   ! dead rotifers [mg/l]
   dead_rotifers = rotifers * exp(mortality_rate * tflie) - rotifers
   
   ! -------------------------------------------------------------------------
   ! timestep
   ! -------------------------------------------------------------------------
   ! If the loss in zooplankton biomass is higher that the actuall zooplankton
   ! biomass, basal respiration and mortality will be recalculted.
   ! Otherwise zooplankton would become negative durintg the timestep
   loss_rotifers = respirated_biomass_basal + dead_rotifers 
   if (loss_rotifers > rotifers) then
      respirated_biomass_basal = (respirated_biomass_basal / loss_rotifers) * rotifers
      dead_rotifers = (dead_rotifers / loss_rotifers) * rotifers
   endif
    
   ! total balance [mg/l]
   rotifers_new = rotifers                  &
                + assimilated_biomass       &
                - respirated_biomass_basal  &
                - respirated_biomass
   
   ! reconvert to ind/l
   zooind_s = rotifers_new / grot * 1000.
   
   ! -------------------------------------------------------------------------
   ! return values
   ! -------------------------------------------------------------------------
   ! ingested algae during timestep [mg/l]
   ir_s = ingested_algae
   
   ! ingestion separated into algae groups [mg/l]
   algzob_s = alpha_abl * ingested_algae
   algzog_s = alpha_agr * ingested_algae
   algzok_s = alpha_aki * ingested_algae
   
   ! excretion separated into algae groups [mg/l]
   zexbl_s = alpha_abl * excreted_biomass
   zexgr_s = alpha_agr * excreted_biomass
   zexki_s = alpha_aki * excreted_biomass
   
   ! basal respiration during timestep [mg/l]
   dzres1_s = respirated_biomass_basal
   
   ! aktive respiration during timestep [mg/l]
   dzres2_s = respirated_biomass
   
   ! mortaility within timestep [mg/l]
   abszo_s = dead_rotifers
   
   !--------------------------------------------------------------------------
   ! output values
   !--------------------------------------------------------------------------
   ! net growth rate [1/d]
   rmuas_s = (assimilated_biomass - respirated_biomass - respirated_biomass_basal) / (rotifers * tflie)
   
   ! active respiration rate [1/d]
   rakr_s = respirated_biomass / (rotifers * tflie)
   
   ! basal respiration rate [1/d]
   rbar_s = respirated_biomass_basal / (rotifers * tflie)
   
   ! ingestion rate [1/d]
   iras_s = ingested_algae / (rotifers * tflie)

end subroutine zooplankton
