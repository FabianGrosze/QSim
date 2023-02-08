subroutine oxygen(vO2_s, zooind_s,                                  &
                  agrNH4_s, akiNH4_s, ablNH4_s,                     &
                  agrNO3_s, akiNO3_s, ablNO3_s,                     &
                  dalggr_s, dalgki_s, dalgbl_s, albewg_s, albewk_s, &
                  dalgag_s, dalgak_s, dalgab_s, alberg_s, alberk_s, &
                  hJO2_s, bsbt_s, dC_DenW_s, TOC_CSB, gO2n_s,       & 
                  pO2p_s, pO2r_s, rO2dr_s, rO2hnf_s,                &
                  rau_s, tiefe_s, rhyd_s, vmitt_s, flae_s,          &
                  wlage_s, hws_s, wge_s, tempw_s,                   &
                  iPhy, tflie,                                      &
                  dalgo_s, dalgao_s, algo_s, abeowg_s, abeowk_s,    &
                  abeorg_s, abeork_s, zooro2_s, hSchlr_s,           &
                  o2ein_s, o2ein1_s,  saett_s,                      &
                  kontroll, jjj)
      
   use aparam, only : Caki, Cagr, Cabl, GRot, &
                      opkimi, opgrmi, opblmi, &
                      opkima, opgrma, opblma
   implicit none
   ! --- dummy arguments ---           
   real, intent(inout)  :: vO2_s       !< Sauerstoffgehalt
   real, intent(in)     :: zooind_s    !< Rotatorien
   real, intent(in)     :: agrNH4_s    !< Ammoniumaufnahme der Grünalgen    agrnh4z(nkz,ior)
   real, intent(in)     :: akiNH4_s    !< Ammoniumaufnahme der Kieselalgen  akinh4z(nkz,ior)
   real, intent(in)     :: ablNH4_s    !< Ammoniumaufnahme der Blaualgen    ablnh4z(nkz,ior)
   real, intent(in)     :: agrNO3_s    !< Nitrataufnahme der Grünalgen      agrNO3z(nkz,ior)
   real, intent(in)     :: akiNO3_s    !< Nitrataufnahme der Kieselalgen    akiNO3z(nkz,ior)
   real, intent(in)     :: ablNO3_s    !< Nitrataufnahme der Blaualgen      ablNO3z(nkz,ior)
   real, intent(in)     :: dalggr_s    !< Zuwachs Grünalgen
   real, intent(in)     :: dalgki_s    !< Zuwachs Kieselalgen
   real, intent(in)     :: dalgbl_s    !< Zuwachs Blaualgen
   real, intent(in)     :: albewg_s    !< Wachstum benthischer Grünalgen
   real, intent(in)     :: albewk_s    !< Wachstum benthischer Kieselalgen
   real, intent(in)     :: dalgag_s    !< Respiration Grünalgen  algagz(nkz,ior)
   real, intent(in)     :: dalgak_s    !< Respiration Kieselagen algakz(nkz,ior)
   real, intent(in)     :: dalgab_s    !< Respiration Blaualgen  algabz(nkz,ior)
   real, intent(in)     :: alberg_s    !< Respiration benthischer Grünalgen
   real, intent(in)     :: alberk_s    !< Respiration bentischer Kieselagen
   real, intent(in)     :: tempw_s     !< Wassertemperatur
   real, intent(in)     :: hJO2_s      !< Sauerstoffzehrung des Sediments je Zeitschritt [gO2/m2]
   real, intent(in)     :: bsbt_s      !< Sauerstoffverbrauch durch Kohlenstoffabbau
   real, intent(in)     :: dC_DenW_s   !< C-Abbau durch Denitrifikation in der Wassersäule
   real, intent(in)     :: TOC_CSB     !< Berechnung der BSB-Komponenten am oberen Rand
   real, intent(in)     :: gO2n_s      !< für die Stickstoffoxidation verbrauchte Sauerstoffmenge
   real, intent(in)     :: pO2p_s      !< Sauerstoffproduktion durch Makrophyten je Zeitschritt [mgO2/l]
   real, intent(in)     :: pO2r_s      !< Sauerstoffverbrauch durch Makrophyten je Zeitschritt[mgO2/l]
   real, intent(in)     :: rO2dr_s     !< Respiration Dreissena-Muscheln je Zeitschritt [mgO2/9]
   real, intent(in)     :: rO2hnf_s    !< Respiration HNF
   real, intent(in)     :: rau_s       !< Strickler Reibungsbeiwert
   real, intent(in)     :: tiefe_s     !< Wassertiefe
   real, intent(in)     :: rhyd_s      !< hydraulischer Radius
   real, intent(in)     :: vmitt_s     !< Geschwindigkeitsbetrag
   real, intent(in)     :: flae_s      !<
   real, intent(in)     :: wlage_s     !< Höhenlage der zugehörigen Wetterstation [mNHN]   !< WLage(mstr,ior)
   real, intent(in)     :: hws_s       !< Wasserspiegellage hws(mstr,ior)
   real, intent(in)     :: wge_s       !< Windgeschwindigkeit   !< wge(IDWe(mstr,ior))
   integer, intent(in)  :: iPhy        !< Nummer der Belüftungsformel
   real, intent(in)     :: tflie       !< Zeitschritt [d]
   real, intent(out)    :: dalgo_s     !< Sauerstoffproduktion der Grün-, Kiesel-, und Blaualgen  dalgoz(nkz,ior)
   real, intent(out)    :: dalgao_s    !< Respiration (Sauerstoffverbrauch) der Grün-, Kiesel-, und Blaualgen
   real, intent(out)    :: algo_s      !< Netto-Sauerstoffproduktion der Algen
   real, intent(out)    :: abeowg_s    !< Sauerstoffproduktion benthischer Grünalgen
   real, intent(out)    :: abeowk_s    !< Sauerstoffproduktion benthischer Kieselalgen 
   real, intent(out)    :: abeorg_s    !< Sauerstoffverbrauch benthischer Grünalgen
   real, intent(out)    :: abeork_s    !< Sauerstoffverbrauch benthishcer Kieselalagen 
   real, intent(out)    :: zooro2_s    !< Sauerstoffverbrauch durch Zooplanktonrespiration (Rückgabewert)
   real, intent(out)    :: hSchlr_s    !< Sauerstoffzehrung durch das Sediment [mgO2/(l*h)] hSchlr(mstr,ior)
   real, intent(out)    :: o2ein_s     !< potentieller Sauerstoffeintrag aus der Luft
   real, intent(out)    :: o2ein1_s    !< Sauerstoffeintrag aus der Luft
   real, intent(out)    :: saett_s     !< Sauerstoffstättigungskonzentration [mgO2/l]
   logical              :: kontroll    !< debugging
   integer, intent(in)  :: jjj         !< debugging
   
   ! --- local variables ---
   real            :: opkimix, opkimax, opgrmix, opgrmax, opblmix, opblmax
   real            :: falgog, falgok, falgob
   real            :: ft, v, abeor, abeow, sed_o2
   real            :: bbei, defiz, delta_oxygen
   
   real, parameter :: mol_weight_o2 = 32.
   real, parameter :: mol_weight_c  = 12.
   real, parameter :: mol_o2_mol_c  = mol_weight_o2 / mol_weight_c
   
   ! --------------------------------------------------------------------------
   ! Influence of Algae
   ! --------------------------------------------------------------------------
   ! Umrechnung von RQ [molC/molO2] und PQ [molO2/molC] in mg O2/mg Bio
   if (opkimi > 0.) then
      opkimix = mol_o2_mol_C / opkimi * Caki
   else
      opkimix = 0.
   endif
   if (opgrmi > 0.) then
      opgrmix = mol_o2_mol_C / opgrmi * Cagr
   else
      opgrmix = 0.
   endif
   if (opblmi > 0.) then
      opblmix = mol_o2_mol_C / opblmi * Cabl
   else
      opblmix = 0.
   endif
   opkimax = opkima * mol_o2_mol_C * Caki
   opgrmax = opgrma * mol_o2_mol_C * Cagr
   opblmax = opblma * mol_o2_mol_C * Cabl
   
   ! Grünagen
   if (agrNH4_s > 0.0) then 
      falgog = agrno3_s / agrNH4_s
   else
      falgog = agrno3_s / 0.00001
   endif
   falgog = (opgrmix + falgog * opgrmax) / (1. + falgog)

   ! Benthische Grünalgen
   abeowg_s = albewg_s * falgog

   ! Kieselalgen
   if (akiNH4_s > 0.0) then
      falgok = akiNO3_s / akiNH4_s
   else
      falgok = akiNO3_s / 0.00001
   endif
   falgok = (opkimix + falgok * opkimax) / (1. + falgok)

   ! Benthische Kieselalgen
   abeowk_s = albewk_s * falgok

   ! Blaualgen
   if (ablNH4_s > 0.0) then
      falgob = ablNO3_s / ablNH4_s
   else
      falgob = ablNO3_s / 0.00001
   endif
   falgob = (opblmix + falgob * opblmax) / (1. + falgob)

   
   ! --- Sauerstoffproduktion --- 
   ! Grün-, Kiesel-, und Blaualgen
   dalgo_s = dalggr_s * falgog   &
           + dalgki_s * falgok   &
           + dalgbl_s * falgob
   
   ! benthische Grün- und Kieselalgen
   abeow = abeowg_s + abeowk_s
   
   ! --- Sauerstoffrespiration ---
   ! Grün-, Kiesel-, und Blaualgen
   dalgao_s = dalgag_s * opgrmix &
            + dalgak_s * opkimix &
            + dalgab_s * opblmix

   ! benthische Grün- und Kieselalgen
   abeorg_s = alberg_s * opgrmix
   abeork_s = alberk_s * opkimix
   abeor    = abeorg_s + abeork_s
   
   ! Netto Sauerstoffproduktion der Algen
   algo_s = dalgo_s - dalgao_s
   
   ! --------------------------------------------------------------------------
   ! Influence of Zooplankton
   ! --------------------------------------------------------------------------
   ft = 1.047**(tempw_s - 20.)
   zooro2_s = 13.08 * GRot**0.716 * 24. * 1.e-6  ! mgO2/Ind/d (Galkovskaya 1995)
   zooro2_s = zooro2_s * zooind_s * ft * tflie
   
   ! --------------------------------------------------------------------------
   ! oxygen exchange at water surface
   ! --------------------------------------------------------------------------
   ! Belüftungsbeiwert
   ! TODO (Schoenung): Berechnung erfolgt ebenfalls in pH und ist somit redundant.
   ! Ticket #51
   if (rau_s > 0.0 .and. tiefe_s > 0.0 .and. rhyd_s > 0.0) then
      call belueftung_k2(rau_s, tiefe_s, vmitt_s, rhyd_s , flae_s, tempw_s,   &
                         WLage_s, hWS_s, wge_s, iphy, bbei)
   else
      bbei = 0.0
   endif
   
   ! Sauerstoffsättigungskonzentration
   saett_s = oxygen_saturation_concentration(tempw_s)
   defiz   = saett_s - vO2_s
   
   ! potentieller Sauerstoffaustausch
   o2ein_s = saett_s * (1 - exp(-bbei * tflie))
   
   ! tatsächlicher Sauerstoffaustausch
   ! TODO (Schoenung, august 2022): Ticket #53
   ! Hier wird ein implizites Eulerverfahren benutzt. In der Sauerstoffbilanz weiter unten
   ! wird hingegen ein semi-implizites Verfahren benutzt. Damit untescheiden sich der Ausgabewert
   ! `o2ein1_s` und der tatsächlich in der Bilanz verwendete Wert voneinander.
   o2ein1_s = defiz * (1 - exp(-bbei * tflie))
   
   if (kontroll) then
      write(*,'(A)')  'oxygen Oberflaechenbelueftung:'
      write(*,'(A,F0.6)') '  vO2    = ', vO2_s
      write(*,'(A,F0.6)') '  saett  = ', saett_s
      write(*,'(A,F0.6)') '  defiz  = ', defiz
      write(*,'(A,F0.6)') '  O2ein  = ', o2ein_s
      write(*,'(A,F0.6)') '  O2ein1 = ', o2ein1_s
      write(*,'(A,F0.6)') '  tempw  = ', tempw_s
      write(*,'(A,F0.6)') '  rau    = ', rau_s
      write(*,'(A,F0.6)') '  tiefe  = ', tiefe_s
      write(*,'(A,F0.6)') '  vmitt  = ', vmitt_s
      write(*,'(A,F0.6)') '  rhyd   = ', rhyd_s
      write(*,'(A,F0.6)') '  flae   = ', flae_s
      write(*,'(A,F0.6)') '  WLage  = ', WLage_s
      write(*,'(A,F0.6)') '  hws    = ', hws_s
      write(*,'(A,F0.6)') '  wge    = ', wge_s
      write(*,'(A,F0.6)') '  bbei   = ', bbei
      write(*,'(A,I0)')   '  iPhy   = ', iphy
   endif
   
   ! Sauerstoffzehrung des Sediments
   if (tiefe_s > 0.) then
      sed_o2 = hJO2_s * tflie / tiefe_s
   else
      sed_o2 = 0.
   endif
   
   ! --------------------------------------------------------------------------
   ! oxygen balance
   ! --------------------------------------------------------------------------
   
   v = dalgo_s                         &  ! Sauerstoffproduktion der Algen
     - dalgao_s                        &  ! Sauerstoffrespiration der Algen
     - go2n_s                          &  ! Sauerstoffverbrauch bei Stickstoffoxidation
     - (bsbt_s - dC_DenW_s * TOC_CSB)  &  ! Sauerstoffverbrauch durch Kohlenstoffabbau
     - zooro2_s                        &  ! Sauerstoffverbrauch der Rotatorien
     - sed_o2                          &  ! Sauerstoffzehrung des Sediments
     + po2p_s                          &  ! Sauerstoffproduktion der Makrophyten
     - po2r_s                          &  ! Sauerstoffrespiration der Makrophyten
     + abeow                           &  ! Sauerstoffproduktion der benthischen Algen
     - abeor                           &  ! Sauerstoffrespiration der benthischen Algen
     - ro2dr_s                         &  ! Sauerstoffrespiration der Dreissena
     - rO2HNF_s                           ! Sauerstoffrespiration der HNF
   
   ! Weil die Belüftungsrate vom Sauerstoffgehalt selbst abhängig ist, wird hier 
   ! eine semi-implizite Diskretisierung zur Berechnung des Sauerstoffgehaltes 
   ! zum Zeitpunkt t+Δt verwendet:
   delta_oxygen = (v + bbei * tflie * defiz) / (1. + bbei * tflie * 0.5)
   
   ! --------------------------------------------------------------------------
   ! update return values
   ! --------------------------------------------------------------------------
   ! Ausgabe Sedimentflux [mgO2/l/h]
   if (tiefe_s > 0.) then
      hSchlr_s = hJO2_s / (tiefe_s * 24.)
   else
      hSchlr_s = 0.
   endif
   
   vo2_s = max(vo2_s + delta_oxygen, 0.01)
   
end subroutine oxygen
