!> Berechnung des o-PO4-Gehalts
!! @author Volker Kirchesch
!! @date 10.01.2014
subroutine phosphate(gelP_s, gesP_s, bsbctP_s,                  &
                     aki_s, agr_s, abl_s, dzres1_s, dzres2_s,   &
                     Q_PK_s, Q_PG_s, Q_PB_s,                    &
                     resdr_s, exdrvk_s, exdrvg_s, exdrvb_s,     &
                     up_PG_s, up_PK_s, up_PB_s,                 &
                     agrbr_s, akibr_s, ablbr_s,                 &
                     algag_s, algak_s, algab_s,                 &
                     albewg_s, alberg_s, albewk_s, alberk_s,    &
                     tiefe_s, hJPO4_s, orgCsd_s, pl0_s,         &
                     sedalk_s, sedalb_s, sedalg_s,              &
                     algdrk_s, algdrb_s, algdrg_s,              &
                     tflie,                                     &
                     kontroll, jjj)
      
   use aparam, only :  Qmx_PG, Qmx_PK
   implicit none
   
   ! --- dummy arguments ---
   real, intent(inout)  :: gelP_s   !< gelöster ortho-Phosphat-Phosphor
   real, intent(inout)  :: gesP_s   !< gesamter ortho-Phosphat-Phosphor
   real, intent(in)     :: bsbctP_s !< Phosphorfreisetzung beim Abbau organischer Kohlenstoffverbidungen
   real, intent(in)     :: aki_s    !< Kieselalgen
   real, intent(in)     :: agr_s    !< Grünalgen
   real, intent(in)     :: abl_s    !< Blaualgen
   real, intent(in)     :: dzres1_s !< durch Zooplankton respirierte Algen
   real, intent(in)     :: dzres2_s !< durch Zooplankton respirierte Algen
   real, intent(in)     :: Q_PK_s   !< Phosphoranteil der Kieselalgenbiomasse
   real, intent(in)     :: Q_PG_s   !< Phosphoranteil der Grünalgenbiomasse
   real, intent(in)     :: Q_PB_s   !< Phosphoranteil der Blaualgenbiomasse
   real, intent(in)     :: resdr_s  !< Respirationsrate benthischer Filtrierer (Dreissena-Muscheln)
   real, intent(in)     :: exdrvk_s !< exkretierte Biomasse der Muscheln beim Konsum von Kieselalgen
   real, intent(in)     :: exdrvg_s !< exkretierte Biomasse der Muscheln beim Konsum von Grünalgen
   real, intent(in)     :: exdrvb_s !< exkretierte Biomasse der Muscheln beim Konsum von Blaualgen
   real, intent(in)     :: tiefe_s  !< water depth [m]
   real, intent(in)     :: hJPO4_s  !< sediment flux
   real, intent(in)     :: up_PG_s  !< P-Aufnahmerate der Grünalgen
   real, intent(in)     :: up_PK_s  !< P-Aufnahmerate der Kieselalgen
   real, intent(in)     :: up_PB_s  !< P-Aufnahmerate der Blaualgen
   real, intent(in)     :: agrbr_s  !< Brutto-Wachstum der Grünalgen
   real, intent(in)     :: akibr_s  !< Brutto-Wachstum der Kiesealgen
   real, intent(in)     :: ablbr_s  !< Brutto-Wachstum der Blaualgen
   real, intent(in)     :: algag_s  !< Respirierte Biomasse der Grünalgen
   real, intent(in)     :: algak_s  !< Respirierte Biomasse der Kieselalgen
   real, intent(in)     :: algab_s  !< Respirierte Biomasse der Blaualgen
   real, intent(in)     :: albewg_s !< Verbrauch durch Wachstum bentischer Algen
   real, intent(in)     :: alberg_s !< Freisetzung durch Respiration von bentischen Algen
   real, intent(in)     :: albewk_s !< Wachstum benthischer Kieselalgen
   real, intent(in)     :: alberk_s !< Respiration benthischer Kieselalgen
   real, intent(in)     :: orgCsd_s !< Gesamtmasse Kohlenstoff, die je Zeitschritt sedimentiert
   real, intent(in)     :: pl0_s    !< P:C Verhältnis in organischem Material
   real, intent(in)     :: sedalk_s !< Sedimentierte Biomasse Kieselalgen
   real, intent(in)     :: sedalb_s !< Sedimentierte Biomasse Blaualgen
   real, intent(in)     :: sedalg_s !< Sedimentierte Biomasse Grünalgen
   real, intent(in)     :: algdrk_s !< durch Dreissena konsumierte Kieselalgen [mg/l]
   real, intent(in)     :: algdrg_s !< durch Dreissena konsumierte Grünalgen [mg/l]
   real, intent(in)     :: algdrb_s !< durch Dreissena konsumierte Blaualgen [mg/l]
   real, intent(in)     :: tflie    !< timestep [d]
   logical, intent(in)  :: kontroll !< debugging
   integer, intent(in)  :: jjj      !< debugging

   ! --- local variables ---
   real     :: gelpt, gespt, gelP_dr, delp, PSed, hconKi, hconGr, hconBl
   real     :: gelP_zoo, agrP, akiP, ablP
   
   !========================================================================
   ! --- Einfluss des Zooplanktons ---
   if (aki_s+agr_s+abl_s > 0.0) then
      hconKi = aki_s/(aki_s+agr_s+abl_s)
      hcongr = agr_s/(aki_s+agr_s+abl_s)
      hconbl = abl_s/(aki_s+agr_s+abl_s)
   else
      hconKi = 0.0
      hconGr = 0.0
      hconBl = 0.0
   endif
   
   gelP_zoo = dzres1_s * 0.01              &
            + dzres2_s * hconKi * Q_PK_s   &
            + dzres2_s * hcongr * Q_PG_s   &
            + dzres2_s * hconbl * Q_PB_s
   
   ! --- Einfluss von Dreissena ---
   gelP_dr = resdr_s  * 0.01    &
           + exdrvk_s * Q_PK_s  &
           + exdrvg_s * Q_PG_s  &
           + exdrvb_s * Q_PB_s
   
   ! --- Einfluss der Algen
   agrP = -up_PG_s * (agrbr_s - algag_s) - (albewg_s - alberg_s) * Qmx_PG
   akiP = -up_PK_s * (akibr_s - algak_s) - (albewk_s - alberk_s) * Qmx_PK
   ablP = -up_PB_s * (ablbr_s - algab_s)
   
   ! --- sediment flux ---
   if (tiefe_s > 0.0) then 
      Psed = hJPO4_s * tflie / tiefe_s
   else
      PSed = 0.0
   endif
   
   ! -----------------------------------------------------------------------
   ! timestep gelP
   ! -----------------------------------------------------------------------
   
   gelPt = gelP_s              &
         + agrP + akiP + ablP  & ! Änderung durch Algen
         + bsbctP_s            & ! Änderung durch C-Abbau
         + Psed                & ! Änderung durch Sedimentfluss
         + gelP_zoo            & ! Änderung durch Zooplankton
         + gelP_dr               ! Änderung durch Dreissena
   
   if (gelPt < 0.0) then
      delp = gelPt - gelP_s
      gelPt = 0.0
      if (gelP_s + abs(delP) > 0.0) gelPt = (gelP_s / (gelP_s + abs(delp))) * gelP_s
   endif
   
   ! -----------------------------------------------------------------------
   ! timestep gesP
   ! -----------------------------------------------------------------------
   ! Veränderung des gesP durch Sedimentation
   if (gesP_s <= 0.0) then
      gesPt = gesP_s
   else
      gesPt = gesP_s                         &
            - orgCsd_s * pl0_s               &
            - sedalk_s * Q_PK_s              &
            - sedalb_s * Q_PB_s              &
            - sedalg_s * Q_PG_s              &
            + Psed                           &
            - algdrk_s * Q_PK_s              &
            - algdrg_s * Q_PG_s              &
            - algdrb_s * Q_PB_s              &
            - (albewg_s-alberg_s) * Qmx_PG   &
            - (albewk_s-alberk_s) * Qmx_PK
   endif
   if (gesPt < 0.001) gesPt = 0.001
   
   ! --------------------------------------------------------------------------
   ! update return values
   ! --------------------------------------------------------------------------
   gelP_s = gelPt
   gesP_s = gesPt
   
   return
end subroutine phosphate