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
   real, intent(out)    :: doN_s        !< mineralisierter N-Gehalt in der Wassersäule ; Ammoniumfreisetzung beim Abbau org. C-Verbindungen
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
   real     :: topt, dti, ftemp
   real     :: hymxD1, hymxD2, hhyp1, hhyp2
   real     :: dCD1, dCD2, CD1_t, CD2_t, CP1_t, CP2_t, CMt
   real     :: ddCM1, ddCM2, hyD1, hyD2
   real     :: hupBAC, dCM, resBAC, dBAC, BACt, bsbts
   real     :: fluxd1, fluxd2, fvcb, hconpf
   real     :: fluxO2, BSBtb
   real     :: g, ust, aSedC, bSedC, CP1sd, CP2sd, BACsd, Crfsd, zellv, qsgr, oc, oc0, wst
   real     :: ceq1, ceq2, ceq3, ceq4, sedCP1, sedCP2, sedBAC, sedCrf
   integer  :: ised, jsed
   real     :: dCD1t, dCD2t, dCP1t, dCP2t, Creft
   real     :: dorgP, dorgN, orgPn, orgNn
   real     :: sumC, fakCref
   real     :: bl01t, bl02t, bl01, bl02, bl0t, bl0 
   real     :: deltat, hc_wert, k_bsb, obsbt
   real     :: hc1, hc2, fbsgrt, frfgrt
   real     :: delbsg, delfrg, delbs, delcs, delCD1, delCD2, delCP1, delCP2, delCM, delBAC
   real     :: algb51, algb52, algb53, algb5, algcs1, algcs2, algcs3, algcs
   real     :: zoobsb, zoocsb, ocsbt
  
   real, parameter :: bk1 = 0.51
   real, parameter :: bk2 = 0.02
   real, parameter :: famD1 = 0.3
   real, parameter :: famD2 = 0.2
   real, parameter :: famP1 = 0.1
   real, parameter :: famP2 = 0.3
   real, parameter :: famR  = 0.1
   
   ! =======================================================================
   ! start
   ! =======================================================================
   if (bvHNF_s > 0.0) then
     HNF_P_C = 0.004 * bvHNF_s**0.367
     HNF_N_C = 0.183 * bvHNF_s**0.0361
   else
     HNF_P_C = 0.0
     HNF_N_C = 0.0
   endif
   
   if (ocsb_s > 0.0) then 
     vcb = obsb_s/ocsb_s
   else
     vcb = 0.0
   endif
   
   if (TOC_CSB > 0.0) then
     Cref = (ocsb_s / TOC_CSB)  &
          - CD1_s - CD2_s       &
          - CP1_s - CP2_s       &
          - CM_s                &
          - (1.-famR) * bac_s   &
          - (1.-famR) * chnf_s
   else
     Cref = 0.0
   endif
   
   orgN = ( Cref                 &
           + CD1_s + CD2_s       &
           + CP1_s + CP2_s       &
           + CM_s                &
           + (1.-famR) * bac_s   &
           + (1.-famR) * chnf_s  &
          ) * nl0_s
        
   orgP = ( Cref                  &
           + CD1_s + CD2_s        &
           + CP1_s + CP2_s        &
           + CM_s                 &
           + (1.-famR) * bac_s    &
           + (1.-famR) * chnf_s   &
          ) * pl0_s
   
   ! Temperaturabhaengigkeit
   Topt = 25.
   dti  = 15.
   ftemp = exp(-((tempw_s-Topt)**2) / (dti**2))
   
   hymxD1 = hymxD
   hymxD2 = 0.474*vcb**(-1.346)
   if (hymxD2 > 6.) hymxD2 = 6.
   
   hhyP1 = hyP1
   hhyP2 = 1.51 * vcb**2.31
   
   ! Änderung der gelösten organischen C-Verbindungen aus partikulaeren C-Verbindungen
   dCD1 = hhyP1 * ftemp * CP1_s * tflie
   dCD2 = hhyP2 * ftemp * CP2_s * tflie
   
   ! Änderung der partik. und geloesten org. C-Verbindungen
   CD1_t = CD1_s + dCD1
   CD2_t = CD2_s + dCD2
   CP1_t = CP1_s - dCD1
   CP2_t = CP2_s - dCD2
       
   ! Änderung der Konz. an monomolekularen C-Verbindungen
   ddCM1 = 0.0
   ddCM2 = 0.0
   
   if ((CD1_t+ksD1) > 0.0) then 
     hyD1 = hymxD1*(CD1_t/(CD1_t + ksD1))
   else
     hyD1 = 0.0
   endif
   
   ddCM1 = hyD1 * ftemp * bac_s * tflie
   if (ddCM1 > CD1_t) ddCM1 = CD1_t - 0.00001
   
   if ((CD2_t + ksD2) > 0.0) then
     hyD2 = hymxD2 * (CD2_t /(CD2_t + ksD2))
   else
     hyD2 = 0.0
   endif
   
   ddCM2 = hyD2 * ftemp * bac_s * tflie
   if (ddCM2 > CD2_t) ddCM2 = CD2_t - 0.00001
   
   
   CD1_t = CD1_t - ddCM1
   CD2_t = CD2_t - ddCM2
   if (CD1_t < 0.00001) CD1_t = 0.00001
   if (CD2_t < 0.00001) CD2_t = 0.00001
   
   CMt = CM_s + ddCM1 + ddCM2
   
   ! --- Bakterien ---
   ! Wachstum
   if ((CMt+ksM) > 0.0) then 
     hupBAC = upBAC * ftemp * (CMt/(CMt+ksM))
   else
     hupBAC = 0.0
   endif
   
   dCM = bac_s * (exp(hupBAC * tflie) - 1.)
   if (dCM > CMt .and. bac_s > 0. .and. bac_s + CMt - 0.00001 > 0.) then
     hupBAC = 0.0
     if (tflie > 0.0) hupBAC = (log(bac_s+CMt-0.00001)-log(bac_s))/tflie
     dCM = bac_s*(exp(hupBAC*tflie)-1.)
   endif
   
   ! Respiration
   resBAC = rsGBAC * ftemp + hupBAC * (1. - yBAC)
   if (resBAC < 0.0) resBAC = 0.0
   
   dBAC = bac_s * (exp((hupBAC-resBAC)*tflie)-1.)
   BACt = bac_s + dBAC
   BACmua_s = hupBAC - resBAC     ! Ausgabewert
   
   CMt = CMt - dCM
   if (CMt < 0.00001)CMt = 0.00001
   bsbct_s = bac_s * (1.-exp(-resBAC*tflie))
   bsbts  = bsbct_s * TOC_CSB
   
   ! --- Einfluss der sessilen Organismen ---
   ! FLUX in g/(m2*d)
   ! Änderung der gel. C-Verbindungen durch Organismen auf Makrophyten
   ! wird überarbeitet
   
   FluxD1 = 0.62 * (CD1_s+CD2_s)**0.817
   if (vcb > 0.) then
      fvcb = 0.62 * log(vcb) + 2.2
   else
      fvcb = 0.0
   endif
  
   FluxD1 = FluxD1 * fvcb
   
   FluxD2 = 0.56*(CD1_s+CD2_s)**0.916
   fvcb = -3.11*vcb+1.407
   FluxD2 = FluxD2*fvcb
   
   ! Umrechnung auf g/(m3*d)
   if (tiefe_s > 0.0) then 
     hconPf = pfl_s/(300.*tiefe_s)
   else
     hconPf = 0.0
   endif
   
   CD1_t = CD1_t - FluxD1 * hconPf  *ftemp * tflie
   if (CD1_t < 0.00001) CD1_t = 0.00001
   
   CD2_t = CD2_t - FluxD2 * hconPf * ftemp * tflie
   if (CD2_t < 0.00001) CD2_t = 0.00001
   
   bsbct_s = bsbct_s + (FLuxD1+FluxD2) * hconPf * ftemp * tflie
   
   ! --- Einfluss des Sediments ---
   if (tiefe_s > 0.0) then
     CD1_t = CD1_t + jDOC1_s * tflie / tiefe_s
     CD2_t = CD2_t + jDOC2_s * tflie / tiefe_s
   endif
   if (CD1_t < 0.00001) CD1_t = 0.00001
   if (CD2_t < 0.00001) CD2_t = 0.00001
   
   ! bsbct_s - mineralisierter Kohlenstoffgehalt in der Wassersäule(einschli
   ! doN_s   - mineralisierter N-Gehalt in der Wassersäule (einschließlich
   bsbctP_s = bsbct_s * pl0_s
   doN_s    = bsbct_s * nl0_s
   
   ! Sauerstoffverbrauch durch Organismen auf Makrophyten
   FluxO2 = 0.758*(CD1_s+CD2_s) + 0.21
   fvcb = -5.476*(vcb**2) + 2.256*vcb + 0.789
   FluxO2 = FluxO2 * fvcb
 
   bsbtb = FluxO2 * hconPf * ftemp * tflie
   bsbt_s = bsbts + bsbtb
   BSBbet_s = bsbtb        !  Ausgabewert
 
   ! --- Einfluss der Sedimentation ---
   g = sqrt(9.81)
   ust = 0.0
   if (rau_s > 0.0 .and. tiefe_s > 0.0) then
     ust = (((1/rau_s)*g)/(tiefe_s**0.16667))*abs(vmitt_s)
   endif
 
   ASEDC = 1.44E-6
   BSEDC = 3.13
   CP1sd = fbsgr_s * CP1_s
   CP2sd = fbsgr_s * CP2_s
   BACsd = fbsgr_s * bac_s
   Crfsd = frfgr_s * Cref
 
   ised = 2
   jsed = 1
   ZellV = 0.0
   call sedimentation(tiefe_s,ised,ust,qsgr,oc,Oc0,tflie,wst,jsed,ZellV, &
                 kontroll,jjj)
   Ceq1 = CP1sd * qsgr
   Ceq2 = CP2sd * qsgr
   Ceq3 = BACsd * qsgr
   Ceq4 = Crfsd * qsgr
 
   sedCP1 = (CP1sd-Ceq1) * oc
   sedCP2 = (CP2sd-Ceq2) * oc
   sedBAC = (BACsd-Ceq3) * oc
   sedCrf = (Crfsd-Ceq4) * oc
   
   orgCsd0_s = CP1sd * oc0    &
             + CP2sd * oc0    &
             + BaCsd * oc0    &
             + Crfsd * oc0
   
   Creft = Cref
   
   orgCsd_s = sedCP1 + sedCP2 &
            + sedBAC          &
            + sedCrf
   
   ! sedimentiertes biologisch abbaubares organ.Material
   orgCsd_abb_s = sedCP1 + sedCP2 + sedBAC 
   
   ! Änderung der Fraktionen durch abgstorbene HNF,Zooplankter und Algen
   dCD1t = famD1 * bsbHNF_s         &
         + famD1 * dKimor_s * Caki  &
         + famD1 * dGrmor_s * Cagr  &
         + famD1 * dBlmor_s * Cabl  &
         + famD1 * abszo_s  * Czoo
   CD1_t = CD1_t + dCD1t
   
   dCD2t = famD2 * bsbHNF_s         &
         + famD2 * dKimor_s * Caki  &
         + famD2 * dGrmor_s * Cagr  &
         + famD2 * dBlmor_s * Cabl  &
         + famD2 * abszo_s  * Czoo
   CD2_t = CD2_t + dCD2t
   
   dCP1t = famP1 * bsbHNF_s       &
         + famP1 * dKimor_s * Caki  &
         + famP1 * dGrmor_s * Cagr  &
         + famP1 * dBlmor_s * Cabl  &
         + famP1 * abszo_s  * Czoo
   CP1_t = CP1_t + dCP1t
   
   dCP2t = famP2 * bsbHNF_s       &
         + famP2 * dKimor_s * Caki  &
         + famP2 * dGrmor_s * Cagr  &
         + famP2 * dBlmor_s * Cabl  &
         + famP2 * abszo_s  * Czoo
   CP2_t = CP2_t + dCP2t
   
   Creft = Creft                  &
         + famR * bsbHNF_s          &
         + famR * dKimor_s * Caki   &
         + famR * dGrmor_s * Cagr   &
         + famR * dBlmor_s * Cabl   &
         + famR * abszo_s  * Czoo
       
   ! Änderung des orgN und orgP-Gehalt
   dorgP = bsbHNF_s * HNF_P_C       &
         + dKimor_s * Q_PK_s        &
         + dGrmor_s * Q_PG_s        &
         + dBlmor_s * Q_PB_s        &
         + abszo_s  * Pzoo
   
   dorgN = bsbHNF_s * HNF_N_C       &
         + dKimor_s * Q_NK_s        &
         + dGrmor_s * Q_NG_s        &
         + dBlmor_s * Q_NB_s        &
         + abszo_s  * Nzoo
   
   orgPn = orgP + dorgP - bsbctP_s
   orgNn = orgN + dorgN - doN_s
   
   ! Erhöhung durch Faces-Bildung von Zooplankter und Dreissena
   CD1_t = CD1_t                    &
         + famD1 * zexKi_s  * Caki  &
         + famD1 * zexGr_s  * Cagr  &
         + famD1 * drfaek_s * Caki  &
         + famD1 * drfaeg_s * Cagr  &
         + famD1 * zexBl_s  * Cabl  &
         + famD1 * drfaeb_s * Cabl
   
   CD2_t = CD2_t                    &
         + famD2 * zexKi_s  * Caki  &
         + famD2 * zexGr_s  * Cagr  &
         + famD2 * drfaek_s * Caki  &
         + famD2 * drfaeg_s * Cagr  &
         + famD2 * zexBl_s  * Cabl  &
         + famD2 * drfaeb_s * Cabl
   
   CP2_t = CP2_t                    &
         + famP1 * zexKi_s  * Caki  &
         + famP1 * zexGr_s  * Cagr  &
         + famP1 * drfaek_s * Caki  &
         + famP1 * drfaeg_s * Cagr  &
         + famP1 * zexBl_s  * Cabl  &
         + famP1 * drfaeb_s * Cabl
   
   CP2_t = CP2_t                    &
         + famP2 * zexKi_s  * Caki  &
         + famP2 * zexGr_s  * Cagr  &
         + famP2 * drfaek_s * Caki  &
         + famP2 * drfaeg_s * Cagr  &
         + famP2 * zexBl_s  * Cabl  &
         + famP2 * drfaeb_s * Cabl
   
   Creft = Creft                    &
         + famR * zexKi_s  * Caki   &
         + famR * zexGr_s  * Cagr   &
         + famR * drfaek_s * Caki   &
         + famR * drfaeg_s * Cagr   &
         + famR * zexBl_s  * Cabl   &
         + famR * drfaeb_s * Cabl
   
   ! Änderung ses orgP und orgN-Gehaltes
   dorgP = zexKi_s  * Q_PK_s        &
         + zexGr_s  * Q_PG_s        &
         + drfaek_s * Q_PK_s        &
         + drfaeg_s * Q_PG_s        &
         + zexBl_s  * Q_PB_s        &
         + drfaeb_s * Q_PB_s
   
   dorgN = zexKi_s  * Q_NK_s        &
         + zexGr_s  * Q_NG_s        &
         + drfaek_s * Q_NK_s        &
         + drfaeg_s * Q_NG_s        &
         + zexBl_s  * Q_NB_s        &
         + drfaeb_s * Q_NB_s
   
   orgPn = orgPn + dorgP
   orgNn = orgNn + dorgN
   
   ! Änderung der partik. C-Verbindungen durch Schwebstoffaufnahme der Dreissena
   ! Annahmen:
   !  Anteil organisches Material 30%
   !  Anteil orgC 40%
   !  Anteil partikuläres refakteres C ergibt sich aus Verhältnis CPges/CDges+CPges)
   sumc = CP1_s + CP2_s + CD1_s + CD2_s
   if (sumc > 0.0) then 
     fakCref = (CP1_s+CP2_s)/sumc
   else
     fakCref = 0.0
   endif
   
   sumc = CP1_s + CP2_s + fakCref * Cref
   if (sumc > 0.0) then 
     CP1_t = CP1_t - ssdr_s*0.3*0.4*(CP1_s/ sumc )
     CP2_t = CP2_t - ssdr_s*0.3*0.4*(CP2_s/ sumc )
     Creft = Creft - ssdr_s*0.3*0.4*(cref/ sumc )
   endif
   
   ! Beruecksichtigung der Sedimentation
   CP1_t = CP1_t - sedCP1
   CP2_t = CP2_t - sedCP2
   BACt  = BACt  - sedBAC
   Creft = Cref  - sedCrf
   orgNn = orgNn        &
         - sedCP1*nl0_s &
         - sedCP2*nl0_s &
         - sedCrf*nl0_s 
   orgPn = orgPn        &
         - sedCP1*pl0_s &
         - sedCP2*pl0_s &
         - sedCrf*pl0_s
   
   ! Verlust der Bakterien durch HNF-Grazing
   BACt = BACt - HNFbac_s - zbac_s
   if (BACt < 0.00001) BACt = 0.00001
   
   ! --- Neuberechnung des BSB5 ---
   BL01t = ( CD1_t                  &
           + CP1_t                  &
           + (famD1+famP1) * BACt   &
           + CMt                    &
           + (famD1+famP1)*chnf_s   &
         ) * TOC_CSB
         
   BL02t = ( CD2_t                  &
           + CP2_t                  &
           + (famD2+famP2) * BACt   &
           + (famD2+famP2) * chnf_s &
         ) * TOC_CSB
         
   BL01 = ( CD1_s                   &
          + CP1_s                   &
          + (famD1+famP1) * bac_s   &
          + CM_s                    &
          + (famD1+famP1) * chnf_s  &
        ) * TOC_CSB
        
   BL02 = ( CD2_s                   &
          + CP2_s                   &
          + (famD2+famP2) * bac_s   &
          + (famD2+famP2) * chnf_s  &
        ) * TOC_CSB
        
   BL0t = BL01t + BL02t
   BL0  = BL01  + Bl02
   
   deltat = 5.
   hc_wert = min(0.95,obsb_s/Bl0)
   
   ! Abbaurate 1. Ordnung berechnet aus dem alten Zeitschritt
   k_BSB = (-1.*log(1.-hc_wert))/deltat
   obsbt = BL0t*(1.-exp(-k_BSB*deltat))
   
   
   ! Neuberechnung des Faktors zur Berechnung der ablagerungsfreien Grenzkonzentration
   sumc = CP1_s                     &
        + CP2_s                     &
        + fakCref * Cref
   
   hc1 = CP2_s - sedCP2             &
       + famP2 * bsbHNF_s           &
       + famP2 * dKimor_s * Caki    &
       + famP2 * dGrmor_s * Cagr    &
       + famP2 * dBlmor_s * Cabl 
   
   hc1 = hc1                        &
       + famP2 * abszo_s * Czoo
      
   hc1 = hc1                        &
       + famP2 * zexKi_s  * Caki    &
       + famP2 * zexGr_s  * Cagr    &
       + famP2 * drfaek_s * Caki    &
       + famP2 * drfaeg_s * Cagr
      
   hc1 = hc1                        &
       + famP2 * zexBl_s  * Cabl    &
       + famP2 * drfaeb_s * Cabl
   
   if (sumc > 0.0) hc1 = hc1 - (ssdr_s * 0.3 * 0.4 * (CP2_s/ sumc ))
   
   hc2 = CP2sd                      &
       - sedCP2                     &
       + famP2 * bsbHNF_s           &
       + famP2 * dKimor_s * Caki    &
       + famP2 * dGrmor_s * Cagr    &
       + famP2 * dBlmor_s * Cabl
   hc2 = hc2                        &
       + famP2 * abszo_s  * Czoo
   hc2 = hc2                        &
       + famP2 * zexKi_s  * Caki    &
       + famP2 * zexGr_s  * Cagr    &
       + famP2 * drfaek_s * Caki    &
       + famP2 * drfaeg_s * Cagr
   hc2 = hc2                        &
       + famP2 * zexBl_s  * Cabl    &
       + famP2 * drfaeb_s * Cabl
   if (sumc > 0.0) hc2 = hc2 - (ssdr_s * 0.3 * 0.4 * (CP2_s/ sumc ))
   
   fbsgrt = 0.0
   if (hc1 > 0.0) fbsgrt = max(0.0,min(0.9,(hc2/hc1)))
   
   hc1 = Cref                       &
       - sedCrf                     &
       + famR * bsbHNF_s            &
       + famR * dKimor_s * Caki     &
       + famR * dGrmor_s * Cagr     &
       + famR * dBlmor_s * Cabl
   hc1 = hc1                        &
       + famR * abszo_s  * Czoo
   hc1 = hc1                        &
       + famR * zexKi_s  * Caki     &
       + famR * zexGr_s  * Cagr     &
       + famR * drfaek_s * Caki     &
       + famR * drfaeg_s * Cagr
   hc1 = hc1                        &
       + famR * zexBl_s  * Cabl     &
       + famR * drfaeb_s * Cabl
   if (sumc > 0.0) hc1 = hc1 - (ssdr_s * 0.3 * 0.4 * (cref/ sumc ))
   
   hc2 = Crfsd                      &
       - sedCrf                     &
       + famR * bsbHNF_s            &
       + famR * dKimor_s * Caki     &
       + famR * dGrmor_s * Cagr     &
       + famR * dBlmor_s * Cabl
   hc2 = hc2                        &
       + famR * abszo_s  * Czoo
   hc2 = hc2                        &
       + famR * zexKi_s  * Caki     &
       + famR * zexGr_s  * Cagr     &
       + famR * drfaek_s * Caki     &
       + famR * drfaeg_s * Cagr
   hc2 = hc2                        &
       + famR * zexBl_s  * Cabl     &
       + famR * drfaeb_s * Cabl
   if (sumc > 0.0) hc2 = hc2 - (ssdr_s * 0.3 * 0.4 * (cref/ sumc ))
       
   frfgrt = 0.0
   if (hc1 > 0.0) frfgrt = max(0.0,min(0.9,(hc2/hc1)))
   
   delbsg = fbsgr_s - fbsgrt
   delfrg = frfgr_s - frfgrt
   delbs  = obsbt   - obsb_s
   delCD1 = CD1_t   - CD1_s
   delCD2 = CD2_t   - CD2_s
   delCP1 = CP1_t   - CP1_s
   delCP2 = CP2_t   - CP2_s
   delCM  = CMt     - CM_s
   delBAC = BACt    - bac_s
   if (obsbt < 0.0) obsbt = max(0.0, (obsb_s/(obsb_s + abs(delbs )))*obsb_s )
   if (CD1_t < 0.0) CD1_t = max(0.0, (CD1_s /(CD1_s  + abs(delCD1)))*CD1_s  )
   if (CD2_t < 0.0) CD2_t = max(0.0, (CD2_s /(CD2_s  + abs(delCD2)))*CD2_s  )
   if (CP1_t < 0.0) CP1_t = max(0.0, (CP1_s /(CP1_s  + abs(delCP1)))*CP1_s  )
   if (CP2_t < 0.0) CP2_t = max(0.0, (CP2_s /(CP2_s  + abs(delCP2)))*CP2_s  )
   if (CMt   < 0.0) CMt   = max(0.0, (CM_s  /(CM_s   + abs(delCM )))*CM_s   )
   if (BACt  < 0.0) BACt  = max(0.0, (bac_s /(bac_s  + abs(delBAC)))*bac_s  )
   
   ocsbt = Creft              &
         + CD1_t + CD2_t      &
         + CP1_t + CP2_t      &
         + CMt                &
         + (1. - famR) * (BACt + chnf_s)
   ocsbt = ocsbt * TOC_CSB
   if (ocsbt < 0.0) then
      delcs = ocsbt - ocsb_s
      ocsbt = max(0.0, ocsb_s / (ocsb_s + abs(delcs)) *ocsb_s)
   endif
   
   ! Neuberechnung von pl0 und nl0
   if (ocsbt > 0.0) then
     pl0_s = orgPn * TOC_CSB / ocsbt
     nl0_s = orgNn * TOC_CSB / ocsbt
   else
     nl0_s = 0.0
     pl0_s = 0.0
   endif
   nl0_s = max(0.0, min(0.2 ,nl0_s))
   pl0_s = max(0.0, min(0.02,pl0_s))
   
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
   zoobsb = (zooind_s * GRot/1000.)        * bsbZoo
   zooCSB = (zooind_s * GRot/1000. * Czoo) * TOC_CSB
   
   vbsb_s = obsbt + algb5 + zoobsb
   vCSB_s = ocsbt + algcs + zooCSB
   
   ! --- Veraenderung der org SS beim C-Abbau (dorgSS) ---
   dorgSS_s = (CP1_t + sedCP1 - CP1_s)          &
            + (CP2_t + sedCP2 - CP2_s)          &
            + (BACt  + sedBAC - bac_s)          &
            + (Creft + sedCrf - Cref) * fakCref
   ! Umrechnung in TG
   dorgSS_s = dorgSS_s/0.45
   
   
   
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