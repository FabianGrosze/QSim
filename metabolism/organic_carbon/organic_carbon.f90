!> Berechnung des biochemischen Sauerstoffbedarfs (BSB)
!! @author Volker Kirchesch
!! @date 22.05.2015
subroutine organic_carbon(ocsb_s, obsb_s, CD1_s, CD2_s, CP1_s, CP2_s,   &
                          CM_s, bac_s, fbsgr_s, frfgr_s,  nl0_s, pl0_s, &
                          cHNF_s, bvHNF_s,                              &
                          tempw_s, tiefe_s, pfl_s, jdoc1_s, jdoc2_s,    &
                          rau_s, vmitt_s, bsbHNF_s,                     &
                          dKiMor_s, dGrMor_s, dBlMor_s, abszo_s,        &
                          Q_PK_s, Q_PG_s, Q_PB_s,                       &
                          Q_NK_s, Q_NG_s, Q_NB_s,                       &
                          zexKi_s, zexGr_s, zexbl_s,                    &
                          drfaek_s, drfaeg_s, drfaeb_s,                 &
                          ssdr_s, hnfbac_s, zBAC_s,                     &
                          abl_s, agr_s, aki_s, zooind_s,                &
                          bsbzoo, toc_csb, tflie,                       &
                          BAcmua_s, bsbct_s, BSBctP_s, doN_s, bsbt_s,   &
                          BSBbet_s, orgCsd0_s, orgCsd_s, orgCsd_abb_s,  &
                          dorgSS_s, vBSB_s, vCSB_s,                     &
                          kontroll, jjj)
  
   use aparam,    only: Cagr, Caki, Cabl, CZoo, PZoo, NZoo,  ksm, yBAC, rsGBAC, &
                        upBAC, hymxD, hyP1, ksd1, ksd2, GRot,                   &
                        bsbbl, bsbgr, bsbki, csbbl, csbgr, csbki
   implicit none
   
   ! ksM    - Halbsaettigungskons. fuer die Aufnahme monomolekularer C-Verbindungen [mgC/l]
   ! YBAC   - Ertragskoeffizient
   ! upBACm - maximale Aufnahmerate 1/d
   ! --- dummy arguments ---
   ! TODO (schoenung, june 2022): descriptions need to be completed.
   real, intent(inout)  :: ocsb_s       !< chemical oxgen demand
   real, intent(inout)  :: obsb_s       !< biochemical oxygen demand
   real, intent(inout)  :: cd1_s        !< leicht abbaubare gelöste organische C-Verbindungen
   real, intent(inout)  :: cd2_s        !< schwer abbaubare gelöste organische C-Verbindungen
   real, intent(inout)  :: cp1_s        !< leicht abbaubare partikuläre organische C-Verbindungen
   real, intent(inout)  :: cp2_s        !< schwer abbaubare partikuläre organische C-Verbindungen
   real, intent(inout)  :: CM_s         !< monomolekularen organischen C-Verbindungen
   real, intent(inout)  :: bac_s        !< C-biomass of bacteria
   real, intent(inout)  :: fbsgr_s      !< ablagerungsfreien Grenzkonzentration zehrungsfähig/refraktär
   real, intent(inout)  :: frfgr_s      !< ablagerungsfreien Grenzkonzentration zehrungsfähig/refraktär
   real, intent(inout)  :: nl0_s        !< N:C ratio
   real, intent(inout)  :: pl0_s        !< P:C ratio
   real, intent(in)     :: cHNF_s       !< C-biomass of HNF
   real, intent(in)     :: bvHNF_s      !< biovolume HNF
   real, intent(in)     :: tempw_s      !< water temperature [°C]
   real, intent(in)     :: tiefe_s      !< water depth [m]
   real, intent(in)     :: pfl_s        !< macrophytes [gTG/m2]
   real, intent(in)     :: jdoc1_s      !< Flux gelöster org. Kohlenstoffe aus dem Sediment, leicht abbaubar
   real, intent(in)     :: jdoc2_s      !< Flux gelöster org. Kohlenstoffe aus dem Sediment, schwer abbaubar
   real, intent(in)     :: rau_s        !< friction
   real, intent(in)     :: vmitt_s      !< flow velocity
   real, intent(in)     :: BSBhnf_s     !< Absterben und Exkretion Heterotropher Naloflagelaten
   real, intent(in)     :: dKiMor_s     !< mortalityrate algae
   real, intent(in)     :: dGrMor_s     !< mortalityrate algae
   real, intent(in)     :: dBlMor_s     !< mortalityrate algae
   real, intent(in)     :: absZo_s      !<
   real, intent(in)     :: Q_PK_s       !< Phosphoranteil der Kieselalgenbiomasse
   real, intent(in)     :: Q_PG_s       !< Phosphoranteil der Grünalgenbiomasse
   real, intent(in)     :: Q_PB_s       !< Phosphoranteil der Blaualgenbiomasse
   real, intent(in)     :: Q_NK_s       !< Stickstoffanteil der Kieselalgenbiomasse
   real, intent(in)     :: Q_NG_s       !< Stickstoffanteil der Grünalgenbiomasse
   real, intent(in)     :: Q_NB_s       !< Stickstoffanteil der Blaualgenbiomasse
   real, intent(in)     :: zexKi_s      !< excretion of algae [mg/l]
   real, intent(in)     :: zexGr_s      !< excretion of algae [mg/l]
   real, intent(in)     :: zexbl_s      !< excretion of algae [mg/l]
   real, intent(in)     :: drfaek_s     !< Ausscheidungen der Muscheln infolge Konsums von Algen
   real, intent(in)     :: drfaeg_s     !< Ausscheidungen der Muscheln infolge Konsums von Algen
   real, intent(in)     :: drfaeb_s     !< Ausscheidungen der Muscheln infolge Konsums von Algen
   real, intent(in)     :: ssdr_s       !< Schwebstoffaufnahme der Dreissena
   real, intent(in)     :: HNFbac_s     !< Verlust der Bakterien durch HNF-Grazing
   real, intent(in)     :: zBAC_s       !< Konsum von BAC durch Zoopankton
   real, intent(in)     :: abl_s        !< Blaualgen [mg/l]
   real, intent(in)     :: agr_s        !< Grünalgen [mg/l]
   real, intent(in)     :: aki_s        !< Kieselalgen [mg/l]
   real, intent(in)     :: zooind_s     !< rotatoria [Ind/l]
   real, intent(in)     :: bsbzoo       !<
   real, intent(in)     :: toc_csb      !<
   real, intent(in)     :: tflie        !< timestep [d]
   real, intent(out)    :: BACmua_s     !<
   real, intent(out)    :: bsbct_s      !< mineralisierter Kohlenstoffgehalt in der Wassersäule | Rückgabewert
   real, intent(out)    :: BSBctP_s     !< Phosphorfreisetzung beim Abbau organischer Kohlenstoffverbidungen
   real, intent(out)    :: doN_s        !< mineralisierter N-Gehalt der Wassersäule; Ammoniumfreisetzung beim Abbau org. C-Verbindungen
   real, intent(out)    :: BSBt_s       !< Kohlenstoffbürtige Sauerstoffzehrung je Zeitschritt
   real, intent(out)    :: BSBbet_s     !< Sauerstoffverbrauch durch Organismen auf Makrophyten; Ausgabevariable für bsbtb
   real, intent(out)    :: orgCsd0_s    !< sedimentiertes organisches Material
   real, intent(out)    :: orgCsd_s     !< Gesamtmasse Kohlenstoff, die je Zeitschritt sedimentiert
   real, intent(out)    :: orgCsd_abb_s !< sedimentiertes biologisch abbaubares organisches Material
   real, intent(out)    :: dorgSS_s     !< Veraenderung der org SS beim C-Abbau
   real, intent(out)    :: vBSB_s       !<
   real, intent(out)    :: vCSB_s       !<
   logical, intent(in)  :: kontroll     !< debugging
   integer, intent(in)  :: jjj          !< debugging
      
   ! --- local variables ---
   real     :: hnf_p_c, hnf_n_c
   real     :: vcb, cref, orgN, orgP
   real     :: topt, dti, ftemp, hymx
   real     :: dCD1, dCD2, CD1_t, CD2_t, CP1_t, CP2_t, CMt, Creft, TOC
   real     :: hupBAC, dCM, resBAC, BACt, bsbts, dC
   real     :: fluxd1, fluxd2, fvcb, hconpf
   real     :: fluxO2, BSBtb, hc1, hc2, fbsgrt, frfgrt
   real     :: ust, aSedC, bSedC, CP1sd, CP2sd, BACsd, Crfsd, zellv, qsgr, oc, oc0, wst
   real     :: sedCP1, sedCP2, sedBAC, sedCrf
   real     :: dorgP, dorgN, orgPn, orgNn
   real     :: sumC, fakCref, bl01t, bl02t, bl01, bl02, bl0t, bl0
   real     :: hc_wert, obsbt, bact_old, ocsbt_old, obsbt_old
   real     :: cd1_t_old, cd2_t_old, cp1_t_old, cp2_t_old, cmt_old, cref_old
   real     :: algb51, algb52, algb53, algb5, algcs1, algcs2, algcs3, algcs
   real     :: zoobsb, zoocsb, ocsbt
   integer  :: ised, jsed
   
   real, parameter :: f_cd1  = 0.3          ! fraction of easily biodegradable dissolved organic C in TOC
   real, parameter :: f_cd2  = 0.2          ! fraction of hardly biodegradable dissolved organic C in TOC
   real, parameter :: f_cp1  = 0.1          ! fraction of easily biodegradable particulate organic C in TOC
   real, parameter :: f_cp2  = 0.3          ! fraction of hardly biodegradable particulate organic C in TOC
   real, parameter :: f_cref = 0.1          ! fraction of refractory particulate organic C in TOC
   real, parameter :: f_org  = 0.3          ! organic fraction of suspended matter (30%)
   real, parameter :: f_orgC = 0.4          ! C:mass ratio of organic matter (40%)
   real, parameter :: c_min  = 0.00001      ! minimum concentration of organic C components (mg C / L)
   real, parameter :: g      = 9.81         ! gravitational acceleration on earth (m / s2)
   
   external :: sedimentation, print_clipping
   
   ! =======================================================================
   ! start
   ! =======================================================================
   
   ! calculate amount of refractory organic C at start of time step
   if (TOC_CSB > 0.0 .and. ocsb_s > 0.) then
      Cref = ocsb_s / TOC_CSB - CM_s          -   &
             CD1_s - CD2_s - CP1_s - CP2_s    -   &
             (1. - f_cref) * (bac_s + chnf_s)
      ! TODO FG: Testing/Debugging showed that Cref frequently becomes negative as the
      !          sum of all other organic C compounds exceeds total organic C (TOC)
      cref = max(0., cref)
   else
      Cref = 0.0
   endif
   Creft = Cref
   
   ! recalculate TOC and CSB
   TOC = cref  + CM_s  +                   &
         CD1_s + CD2_s +                   &
         CP1_s + CP2_s +                   &
         (1. - f_cref) * (bac_s + chnf_s)
   
   ocsb_s = TOC * TOC_CSB
   
   ! calculate organic N and P at start of time step
   orgN = TOC * nl0_s
   orgP = TOC * pl0_s
   
   ! Temperaturabhaengigkeit
   Topt = 25.
   dti  = 15.
   ftemp = exp(-((tempw_s - Topt) / dti)**2)
   
   if (ocsb_s > 0.0) then 
      vcb = obsb_s / ocsb_s
   else
      vcb = 0.0
   endif
   
   ! Änderung der gelösten organischen C-Verbindungen aus partikulaeren C-Verbindungen
   dC    = hyP1 * ftemp * CP1_s * tflie
   CD1_t = CD1_s + dC
   CP1_t = CP1_s - dC
   
   dC    = 1.51 * vcb**2.31 * ftemp * CP2_s * tflie
   CD2_t = CD2_s + dC
   CP2_t = CP2_s - dC
   
   ! Änderung der Konz. an monomolekularen C-Verbindungen
   CMt = CM_s
   
   if (CD1_t + ksD1 > 0.0) then
      hymx = hymxD * CD1_t / (CD1_t + ksD1)
      dC   = max(0., min(CD1_t - c_min, hymx * ftemp * bac_s * tflie))
      CD1_t = CD1_t - dC
      CMt   = CMt   + dC
   endif
   
   if (CD2_t + ksD2 > 0.0) then
      hymx = min(6., 0.474 * vcb**(-1.346)) * CD2_t / (CD2_t + ksD2)
      dC   = max(0., min(CD2_t - c_min, hymx * ftemp * bac_s * tflie))
      CD2_t = CD2_t - dC
      CMt   = CMt   + dC
   endif
   
   ! --- Bakterien ---
   ! Wachstum
   hupBAC = 0.
   dC     = 0.
   if (CMt + ksM > 0. .and. upBAC * ftemp * CMt > 0.) then 
      hupBAC = upBAC * ftemp * CMt / (CMt + ksM)
      dC     = bac_s * (exp(hupBAC * tflie) - 1.)
      if (dC > CMt - c_min .and. bac_s > 0. .and. c_min - CMt > bac_s .and. tflie > 0.) then
         hupBAC = log(1. + (CMt - c_min) / bac_s) / tflie
         dC     = bac_s * (exp(hupBAC * tflie) - 1.)
      else
         hupBAC = 0.
         dC     = 0.
      endif
   endif
   
   ! Respiration
   resBAC   = max(0., rsGBAC * ftemp + hupBAC * (1. - yBAC))
   BACt     = bac_s * exp((hupBAC - resBAC) * tflie)
   BACmua_s = hupBAC - resBAC     ! Ausgabewert
   
   CMt     = CMt - dC
   bsbct_s = bac_s * (1. - exp(-resBAC * tflie))
   bsbts   = bsbct_s * TOC_CSB
   
   ! --- Einfluss der sessilen Organismen ---
   ! FLUX in g/(m2*d)
   ! Änderung der gel. C-Verbindungen durch Organismen auf Makrophyten
   ! wird überarbeitet
   
   FluxD1 = 0.62 * (CD1_s + CD2_s)**0.817
   if (vcb > 0.) then
      fvcb = 0.62 * log(vcb) + 2.2
   else
      fvcb = 0.0
   endif
   FluxD1 = FluxD1 * fvcb
   
   FluxD2 = 0.56 * (CD1_s + CD2_s)**0.916
   fvcb = -3.11 * vcb +1.407
   FluxD2 = FluxD2 * fvcb
   
   ! Umrechnung auf g/(m3*d)
   hconPf = 0.
   if (tiefe_s > 0.0) then 
      hconPf = pfl_s * ftemp * tflie / (300. * tiefe_s)
      if (hConPf > 0.) then
         ! ensure that CD1 and CD2 > c_min by correcting flux values if necessary
         if (FluxD1 * hconPf > CD1_t - c_min) FluxD1 = (CD1_t - c_min) / hconPf
         CD1_t = CD1_t - FluxD1 * hconPf
         
         if (FluxD2 * hconPf > CD2_t - c_min) FluxD2 = (CD2_t - c_min) / hconPf
         CD2_t = CD2_t - FluxD2 * hconPf
         
         bsbct_s = bsbct_s + (FluxD1 + FluxD2) * hconPf
      endif
   endif
   
   ! --- Einfluss des Sediments ---
   ! TODO FG: if max took effect, mass conservation were violated
   !          as jDOC1_s and jDOC2_s required recalculation
   if (tiefe_s > 0.0) then
      CD1_t = max(c_min, CD1_t + jDOC1_s * tflie / tiefe_s)
      CD2_t = max(c_min, CD2_t + jDOC2_s * tflie / tiefe_s)
   endif
   
   ! bsbct_s - mineralisierter Kohlenstoffgehalt in der Wassersäule(einschli
   ! doN_s   - mineralisierter N-Gehalt in der Wassersäule (einschließlich
   bsbctP_s = bsbct_s * pl0_s
   doN_s    = bsbct_s * nl0_s
   
   ! Sauerstoffverbrauch durch Organismen auf Makrophyten
   FluxO2 =  0.758 * (CD1_s + CD2_s) + 0.21
   fvcb   = -5.476 * vcb**2 + 2.256 * vcb + 0.789
   FluxO2 = FluxO2 * fvcb
 
   bsbtb    = FluxO2 * hconPf
   bsbt_s   = bsbts + bsbtb
   BSBbet_s = bsbtb        !  Ausgabewert
 
   ! --- Einfluss der Sedimentation ---
   if (rau_s * tiefe_s > 0.) then
      ust = (sqrt(g) * abs(vmitt_s)) / (rau_s * tiefe_s**0.16667)
   else
      ust = 0.0
   endif
 
   ASEDC = 1.44E-6
   BSEDC = 3.13
   CP1sd = fbsgr_s * CP1_s
   CP2sd = fbsgr_s * CP2_s
   BACsd = fbsgr_s * bac_s
   Crfsd = frfgr_s * Cref
 
   ised  = 2
   jsed  = 1
   ZellV = 0.0
   call sedimentation(tiefe_s, ised, ust, qsgr, oc, oc0, tflie, wst, jsed, ZellV, kontroll, jjj)
 
   sedCP1 = (1. - qsgr) * CP1sd * oc
   sedCP2 = (1. - qsgr) * CP2sd * oc
   sedBAC = (1. - qsgr) * BACsd * oc
   sedCrf = (1. - qsgr) * Crfsd * oc
   
   ! sedimented organic matter
   orgCsd0_s = (CP1sd  + CP2sd  + BaCsd  + Crfsd ) * oc0
   orgCsd_s  =  sedCP1 + sedCP2 + sedBAC + sedCrf
   
   ! sedimented, biologically available organic matter
   orgCsd_abb_s = sedCP1 + sedCP2 + sedBAC
   
   ! Änderung der Fraktionen durch abgstorbene HNF, Zooplankton und Algen
   dC = bsbHNF_s        +     &
        dKimor_s * Caki +     &
        dGrmor_s * Cagr +     &
        dBlmor_s * Cabl +     &
        abszo_s  * Czoo
   
   CD1_t = CD1_t + f_cd1  * dC
   CD2_t = CD2_t + f_cd2  * dC
   CP1_t = CP1_t + f_cp1  * dC
   CP2_t = CP2_t + f_cp2  * dC
   Creft = Creft + f_cref * dC
       
   ! Änderung des orgN und orgP-Gehalt
   HNF_P_C = 0.004 * bvHNF_s**0.367
   dorgP = bsbHNF_s * HNF_P_C +      &
           dKimor_s * Q_PK_s  +      &
           dGrmor_s * Q_PG_s  +      &
           dBlmor_s * Q_PB_s  +      &
           abszo_s  * Pzoo
   
   HNF_N_C = 0.183 * bvHNF_s**0.0361
   dorgN = bsbHNF_s * HNF_N_C +      &
           dKimor_s * Q_NK_s  +      &
           dGrmor_s * Q_NG_s  +      &
           dBlmor_s * Q_NB_s  +      &
           abszo_s  * Nzoo
   
   orgPn = orgP + dorgP - bsbctP_s
   orgNn = orgN + dorgN - doN_s
   
   ! Erhöhung durch Faeces-Bildung von Zooplankton und Dreissena
   dC =  Caki * (zexKi_s + drfaek_s) +    &
         Cagr * (zexGr_s + drfaeg_s) +    &
         Cabl * (zexBl_s + drfaeb_s)
   
   CD1_t = CD1_t + f_cd1  * dC
   CD2_t = CD2_t + f_cd2  * dC
   CP1_t = CP1_t + f_cp1  * dC
   CP2_t = CP2_t + f_cp2  * dC
   Creft = Creft + f_cref * dC
   
   ! Änderung des orgP und orgN-Gehaltes
   dorgP = Q_PK_s * (zexKi_s + drfaek_s) +   &
           Q_PG_s * (zexGr_s + drfaeg_s) +   &
           Q_PB_s * (zexBl_s + drfaeb_s)
   orgPn = orgPn + dorgP
   
   dorgN = Q_NK_s * (zexKi_s + drfaek_s) +   &
           Q_NG_s * (zexGr_s + drfaeg_s) +   &
           Q_NB_s * (zexBl_s + drfaeb_s)
   orgNn = orgNn + dorgN
   
   ! Änderung der partik. C-Verbindungen durch Schwebstoffaufnahme der Dreissena
   ! Annahmen:
   !  Anteil partikuläres refraktäres C ergibt sich aus Verhältnis CPges / (CDges+CPges)
   !  TODO FG: previous comment not clear - why is that so?
   sumc = CP1_s + CP2_s + CD1_s + CD2_s
   if (sumc > 0.0) then 
      fakCref = (CP1_s + CP2_s) / sumc
   else
      fakCref = 0.0
   endif
   
   sumc = CP1_s + CP2_s + fakCref * Cref
   if (sumc > 0.0) then 
      CP1_t = CP1_t - ssdr_s * f_org * f_orgC * CP1_s / sumc
      CP2_t = CP2_t - ssdr_s * f_org * f_orgC * CP2_s / sumc
      Creft = Creft - ssdr_s * f_org * f_orgC * cref  / sumc
   endif
   
   ! Beruecksichtigung der Sedimentation
   CP1_t = CP1_t - sedCP1
   CP2_t = CP2_t - sedCP2
   BACt  = BACt  - sedBAC
   Creft = Creft - sedCrf
   ! TODO FG: Why is sedBAC not included below?
   orgNn = orgNn - nl0_s * (sedCP1 + sedCP2 + sedCrf)
   orgPn = orgPn - pl0_s * (sedCP1 + sedCP2 + sedCrf)
   
   ! Verlust der Bakterien durch HNF-Grazing
   BACt = max(c_min, BACt - HNFbac_s - zbac_s)
   
   ! --- Neuberechnung des BSB5 ---
   BL01t = ( CD1_t + CP1_t + CMt +                         &
            (f_cd1 + f_cp1) * (BACt  + chnf_s)) * TOC_CSB
   BL01  = ( CD1_s + CP1_s + CM_s +                        &
            (f_cd1 + f_cp1) * (bac_s + chnf_s)) * TOC_CSB
   
   BL02t = ( CD2_t + CP2_t +                               &
            (f_cd2 + f_cp2) * (BACt  + chnf_s)) * TOC_CSB
   BL02  = ( CD2_s + CP2_s +                               &
            (f_cd2 + f_cp2) * (bac_s + chnf_s)) * TOC_CSB
        
   BL0t = BL01t + BL02t
   BL0  = BL01  + Bl02
   
   ! Abbaurate 1. Ordnung berechnet aus dem alten Zeitschritt
   hc_wert = min(0.95, obsb_s / BL0)
   ! TODO FG: old calculation
   ! deltat = 5.
   ! k_BSB = -log(1. - hc_wert) / deltat
   ! obsbt = BL0t * (1. - exp(-k_BSB * deltat))
   ! TODO FG: simplified calculation
   obsbt = BL0t * hc_wert
   
   ! Neuberechnung des Faktors zur Berechnung der ablagerungsfreien Grenzkonzentration
   sumc = CP1_s + CP2_s + fakCref * Cref
   
   dC = bsbHNF_s                               +   &
        Caki * (dKimor_s + zexKi_s + drfaek_s) +   &
        Cagr * (dGrmor_s + zexGr_s + drfaeg_s) +   &
        Cabl * (dBlmor_s + zexBl_s + drfaeb_s) +   &
        Czoo * abszo_s
   
   hc1 = CP2_s - sedCP2 + f_cp2 * dC
   hc2 = CP2sd - sedCP2 + f_cp2 * dC
   if (sumc > 0.0) then
      hc1 = hc1 - ssdr_s * f_org * f_orgC * CP2_s / sumc
      hc2 = hc2 - ssdr_s * f_org * f_orgC * CP2_s / sumc
   endif
   fbsgrt = 0.0
   if (hc1 > 0.0) fbsgrt = max(0.0, min(0.9, hc2 / hc1))
   
   hc1 = Cref  - sedCrf + f_cref * dC
   hc2 = Crfsd - sedCrf + f_cref * dC
   if (sumc > 0.0) then
      hc1 = hc1 - ssdr_s * f_org * f_orgC * cref / sumc
      hc2 = hc2 - ssdr_s * f_org * f_orgC * cref / sumc
   endif
   frfgrt = 0.0
   if (hc1 > 0.0) frfgrt = max(0.0, min(0.9, hc2 / hc1))
   
   
   ! TODO (Schönung, february 2023): Add units in print_clipping
   if (obsbt < 0.0) then 
      obsbt_old = obsbt
      obsbt     = max(0.0, (obsb_s/(obsb_s + abs(obsbt - obsb_s))) * obsb_s)
      call print_clipping("organic_carbon", "obsbt", obsbt_old, obsbt, "")
   endif
   
   if (cd1_t < 0.0) then
      cd1_t_old = cd1_t
      cd1_t     = max(0.0, (cd1_s /(cd1_s + abs(cd1_t - cd1_s))) * cd1_s)
      call print_clipping("organic_carbon", "cd1_t", cd1_t_old, cd1_t, "")
   endif   
      
   if (cd2_t < 0.0) then 
      cd2_t_old = cd2_t
      cd2_t     = max(0.0, (cd2_s /(cd2_s + abs(cd2_t - cd2_s))) * cd2_s)
      call print_clipping("organic_carbon", "cd2_t", cd2_t_old, cd2_t, "")
   endif
   
   if (cp1_t < 0.0) then
      cp1_t_old = cp1_t
      cp1_t     = max(0.0, (cp1_s /(cp1_s + abs(cp1_t - cp1_s))) * cp1_s)
      call print_clipping("organic_carbon", "cp1_t", cp1_t_old, cp1_t, "")
   endif
   
   if (cp2_t < 0.0) then
      cp2_t_old = cp2_t
      cp2_t     = max(0.0, (cp2_s /(cp2_s + abs(cp2_t - cp2_s))) * cp2_s)
      call print_clipping("organic_carbon", "cp2_t", cp2_t_old, cp2_t, "")
   endif
   
   if (cmt   < 0.0) then
      cmt_old = cmt
      cmt     = max(0.0, (cm_s  /(cm_s + abs(cmt - cm_s))) * cm_s)
      call print_clipping("organic_carbon", "cmt", cmt_old, cmt, "")
   endif
   
   if (bact  < 0.0) then
      bact_old = bact
      bact     = max(0.0, (bac_s /(bac_s + abs(bact - bac_s))) * bac_s)
      call print_clipping("organic_carbon", "bact", bact_old, bact, "")
   endif
   
   if (creft < 0.0) then
      cref_old = creft
      creft    = max(0.0, (cref /(cref + abs(creft - cref))) * cref)
      call print_clipping("organic_carbon", "creft", cref_old, creft, "")
   endif
   
   ocsbt = Creft                          &
         + CD1_t + CD2_t                  &
         + CP1_t + CP2_t                  &
         + CMt                            &
         + (1. - f_cref) * (BACt + chnf_s)
   ocsbt = ocsbt * TOC_CSB
   if (ocsbt < 0.0) then
      ocsbt_old = ocsbt
      ocsbt = max(0.0, ocsb_s / (ocsb_s + abs(ocsbt - ocsb_s)) *ocsb_s)
      ! TODO (Schönung, february 2023): Add unit
      call print_clipping("organic_carbon", "ocsbt", ocsbt_old, ocsbt, "")
   endif
   
   ! Neuberechnung von pl0 und nl0
   if (ocsbt > 0.0) then
      nl0_s = max(0., min(0.02, orgNn * TOC_CSB / ocsbt))
      pl0_s = max(0., min(0.2 , orgPn * TOC_CSB / ocsbt))
   else
      nl0_s = 0.0
      pl0_s = 0.0
   endif
   
   ! --- Einfluss der lebenden Organismus auf BSB5 und CSB ---
   ! Algen
   algb51 = aki_s * Caki * bsbki
   algb52 = agr_s * Cagr * bsbgr
   algb53 = abl_s * Cabl * bsbbl
   algb5  = algb51 + algb52 + algb53
   
   algcs1 = aki_s * Caki * csbKi
   algcs2 = agr_s * Cagr * csbgr
   algcs3 = abl_s * Cabl * csbbl
   algcs  = algcs1 + algcs2 + algcs3
   
   ! --- Rotatorien ---
   ! Annahme
   ! nur die vorhandenen Zooplankter tragen zum BSB5 bei.
   ! moegliches Hinzuwachsen der Zooplankter in der BSB-Flasch
   ! wird dadurch beruecksichtigt, dass die gesamte Algenbioma
   ! zum BSB5 beitraegt.
   ! In 5d verbraucht 1 mg Zooplanktonbiomasse 1.6 mg O2
   zoobsb = zooind_s * GRot / 1000.        * bsbZoo
   zooCSB = zooind_s * GRot / 1000. * Czoo * TOC_CSB

   vbsb_s = obsbt + algb5 + zoobsb
   vCSB_s = ocsbt + algcs + zooCSB
   
   ! --- Veraenderung der org SS beim C-Abbau (dorgSS) ---
   dorgSS_s = (CP1_t + sedCP1 - CP1_s)           &
            + (CP2_t + sedCP2 - CP2_s)           &
            + (BACt  + sedBAC - bac_s)           &
            + (Creft + sedCrf - Cref ) * fakCref
   ! Umrechnung in TG
   dorgSS_s = dorgSS_s / 0.45
   
   ! -----------------------------------------------------------------------
   ! update values
   ! -----------------------------------------------------------------------
   ocsb_s  = ocsbt
   obsb_s  = obsbt
   CD1_s   = CD1_t
   CD2_s   = CD2_t
   CP1_s   = CP1_t
   CP2_s   = CP2_t
   CM_s    = CMt
   BAC_s   = BACt
   fbsgr_s = fbsgrt
   frfgr_s = frfgrt
   
   return
   
end subroutine organic_carbon