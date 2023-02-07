vno3tsubroutine nitrogen(vNH4_s, vNO3_s, vNO2_s, gesN_s, vO2_s, vx02_s, &
                    aki_s, agr_s, abl_s,                           &
                    Q_NK_s, Q_NG_s, Q_NB_s,                        &
                    up_NK_s, up_NG_s, up_NB_s,                     &
                    akibr_s, agrbr_s, ablbr_s,                     &
                    algak_s, algag_s, algab_s,                     &
                    sedalk_s, sedalg_s, sedalb_s,                  &
                    algdrk_s, algdrg_s, algdrb_s,                  &
                    abltbr_s,                                      &
                    albewk_s, albewg_s,                            &
                    alberk_s, alberg_s,                            &
                    resdr_s, dzres1_s, dzres2_s,                   &
                    exdrvk_s, exdrvg_s, exdrvb_s,                  &
                    up_N2_s, orgCsd_s, nl0_s, bsbct_s,             &
                    susn_s, susn2_s, pfln1_s, pfln2_s, don_s,      &
                    hJNH4_s, hJNO3_s, hJN2_s,                      &
                    tiefe_s, tflie,                                &
                    akiNH4_s, agrNH4_s, ablNH4_s,                  &
                    akiNO3_s, agrNO3_s, ablNO3_s,                  &
                    hFluN3_s, dC_DenW_s,                           &
                    kontroll, jjj)
   
   use aparam, only: Nzoo, Qmx_NG, Qmx_NK, akksN, agksN, abksN
   implicit none
   
   ! --- dummy arguments ---
   real, intent(inout)  :: vNH4_s      !< Ammonium
   real, intent(inout)  :: vNO3_s      !< Nitrat
   real, intent(inout)  :: vNO2_s      !< Nitrit
   real, intent(inout)  :: gesN_s      !< Gesamtstickstoff
   real, intent(in)     :: vO2_s       !< Sauerstoff
   real, intent(in)     :: vx02_s      !< Nitrobacter
   real, intent(in)     :: aki_s       !< Kieselalgen
   real, intent(in)     :: agr_s       !< Grünalgen
   real, intent(in)     :: abl_s       !< Blaualgen
   real, intent(in)     :: Q_NK_s      !< Stickstoffanteil in Kieselangen
   real, intent(in)     :: Q_NG_s      !< Stickstoffanteil in Grünalgen
   real, intent(in)     :: Q_NB_s      !< Stickstoffanteil in Blaualgen
   real, intent(in)     :: up_NK_s     !< Stickstoffaufnahmerate der Kieselagen
   real, intent(in)     :: up_NG_s     !< Stickstoffaufnahmerate der Grünalgen
   real, intent(in)     :: up_NB_s     !< Stickstoffaufnahmerate der Blaualgen
   real, intent(in)     :: akibr_s     !< TODO
   real, intent(in)     :: agrbr_s     !< TODO
   real, intent(in)     :: ablbr_s     !< TODO
   real, intent(in)     :: algak_s     !< TODO
   real, intent(in)     :: algag_s     !< TODO
   real, intent(in)     :: algab_s     !< TODO
   real, intent(in)     :: sedalk_s    !< sedimentierte Kieselalgen
   real, intent(in)     :: sedalg_s    !< sedimentierte Grünalgen
   real, intent(in)     :: sedalb_s    !< sedimentierte Blaualgen
   real, intent(in)     :: algdrk_s    !< Kiesealgenkonsum der Dreissena
   real, intent(in)     :: algdrg_s    !< Grünalgenkonsum der Dreissena
   real, intent(in)     :: algdrb_s    !< Blaualgenkonsum der Dreissena
   real, intent(in)     :: abltbr_s    !< TODO
   real, intent(in)     :: albewk_s    !< Bruttowachstum bentischer Kieselalgen
   real, intent(in)     :: albewg_s    !< Bruttowachstum benthischer Grünalgen
   real, intent(in)     :: alberk_s    !< Respiration der Kieselalgen
   real, intent(in)     :: alberg_s    !< Respiration der Grünalgen
   real, intent(in)     :: resdr_s     !< Respiration Dreissena
   real, intent(in)     :: exdrvk_s    !< Exkretion Dreissena
   real, intent(in)     :: exdrvg_s    !< Exkretion Dreissena
   real, intent(in)     :: exdrvb_s    !< Exkretion Dreissena
   real, intent(in)     :: dzres1_s    !< Respiration Zooplankton
   real, intent(in)     :: dzres2_s    !< Respiration Zooplankton
   real, intent(in)     :: up_N2_s     !< Aufnahmerate von Luftstickstoff durch Blaualgen
   real, intent(in)     :: orgCsd_s    !< Sedimentierte Menge an organischem Kohlenstoff (ohne lebende Algen und Rotatorien)
   real, intent(in)     :: nl0_s       !< Verhältnis von Stickstoff zu Kohlenstoff in organischem Material 
   real, intent(in)     :: bsbct_s     !< mineralisierter Kohlenstoffgehalt in der Wassersäule
   real, intent(in)     :: susn_s      !< zu Nitrit oxidiertes Ammonium
   real, intent(in)     :: susn2_s     !< zu Nitrat oxidiertes Nitrit
   real, intent(in)     :: pfln1_s     !< zu Nitrit oxidiertes Ammonium (Makrophyten)
   real, intent(in)     :: pfln2_s     !< zu Nitrat oxidiertes Nitrit (Makrophyten)
   real, intent(in)     :: don_s       !< Ammoniumfreisetzung beim Abbau org. Kohlenstoffverbidungen
   real, intent(in)     :: hJNH4_s     !< Ammoniumfreisetzung aus dem Sediment
   real, intent(in)     :: hJNO3_s     !< Nitratfreisetzung aus dem Sediment
   real, intent(in)     :: hJN2_s      !< Stickstofffreisetzung aus dem Sediment
   real, intent(in)     :: tiefe_s     !< Wassertiefe
   real, intent(in)     :: tflie       !< Zeitschritt
   real, intent(out)    :: akiNH4_s    !< Ammoniumaufnahme der Kieselagen
   real, intent(out)    :: agrNH4_s    !< Ammoniumaufnahme der Grünalgen
   real, intent(out)    :: ablNH4_s    !< Ammoniumaufnahme der Blaualalgen
   real, intent(out)    :: akiNO3_s    !< Nitrataufnahme der Kiesealgen
   real, intent(out)    :: agrNO3_s    !< Nitrataufnahme der Grünalgen
   real, intent(out)    :: ablNO3_s    !< Nitrataufnahme der Blaualgen
   real, intent(out)    :: hFluN3_s    !< Nitratflux Sediment (Ausgabe)
   real, intent(out)    :: dC_DenW_s   !< Kohlenstoffabbau in Wassersäule durch Nitrifikation (Ausgabe)
   logical, intent(in)  :: kontroll    !< debugging
   integer, intent(in)  :: jjj         !< debugging
   
   ! --- local variables ---
   real          :: suma, hconki, hcongr, hconbl, dzn
   real          :: ndr, ddrn
   real          :: nwgr, nwki, nwbl, alpha_upN4, a_up, b_up, hc_upN3, hc_upN4
   real          :: alpha_upN4k, alpha_upN4g, alpha_upN4b
   real          :: vNH4t, delNH4
   real          :: vNO2t, delNO2
   real          :: vNO3t, delNO3
   real          :: gesNt
   real          :: denWatz, dNO3Den
   integer       :: j_up
   character(200):: message
   
   real, parameter :: KMO_NO3 = 0.26
   real, parameter :: KM_NO3  = 0.4
   
   ! =======================================================================
   ! Wahl der Formel zur Beschreibung der Interaktion zwischen NH4-N und 
   ! NO3-N-Aufnahme durch Algen
   ! 0) unklar
   ! 1) nach Yajnik & Sharada (2003)
   j_up = 0
   
   ! --------------------------------------------------------------------------
   ! zooplankton
   ! --------------------------------------------------------------------------
   ! TODO (schoenung, june 2022):
   ! Diese Berechnung ist falsch. Für die Algenaufnahme des Zooplanktons wird
   ! die jeweilige Filtrierbarkeit der Algen berücksichgt. Die Zusammensetzung
   ! der Zooplanktonnahrung entspricht also nicht der Zusammensetzung der Algen
   ! im Wasser. Dementsprechend müssen für die Respirationsanteile ebenfalls die
   ! Filtrierbarkeiten berücksichtigt werden.
   ! Ich halte es für sinnvoller 'dzn (Ammoniumfreisetzung durch Zooplankton)'
   ! direkt im Zooplanktonbaustein zu berechnen und diesen Wert dann als Übergabe-
   ! variable im Stickstoffbaustein verfügbar zu machen.
   suma = aki_s + agr_s + abl_s
   if (suma > 0.0 .and. suma < huge(suma)) then
      hconKi = aki_s / suma
      hconGr = agr_s / suma
      hconBl = abl_s / suma
   else
      hconKi = 1./3.
      hconGr = 1./3.
      hconBl = 1./3.
   endif
   ! Ammoniumfreisetzung durch Zooplankton (Rotatorienrespiration)
   dzn = dzres1_s * NZoo              &
       + dzres2_s * hconki * Q_NK_s   &
       + dzres2_s * hcongr * Q_NG_s   &
       + dzres2_s * hconbl * Q_NB_s
       
   if (isnan(dzn)) then
      print*, "isNaN(dzn) in subroutine nitrogen."
      print*, "   dzres1 = ", dzres1_s
      print*, "   dzres2 = ", dzres2_s
      print*, "   hconki = ", hconki
      print*, "   hcongr = ", hcongr
      print*, "   hconbl = ", hconbl
      print*, "   NZoo   = ", Nzoo
      print*, "   Q_NK   = ", Q_NK_s
      print*, "   Q_NG   = ", Q_NG_s
      print*, "   Q_NB   = ", Q_NB_s
      print*, "   aki    = ", aki_s
      print*, "   agr    = ", agr_s
      print*, "   abl    = ", abl_s
      
      call qerror("Variable 'dzn' became NaN in subroutine nitrogen.")
   end if

   
   ! --------------------------------------------------------------------------
   ! benthische Filtrierer (Dreissena)
   ! --------------------------------------------------------------------------
   ! Stickstoffanteil Dreissena
   NDR = 0.1
   
   ! Ammoniumfreisetzung der Dreissena
   ddrn = resdr_s  * NDR     &
        + exdrvk_s * Q_NK_s  &
        + exdrvg_s * Q_NG_s  &
        + exdrvb_s * Q_NB_s
   
   ! --------------------------------------------------------------------------
   ! Algen
   ! --------------------------------------------------------------------------
   nwgr = up_NG_s * (agrbr_s - algag_s) + (albewg_s - alberg_s) * Qmx_NG  ! Grünalgen
   nwki = up_NK_s * (akibr_s - algak_s) + (albewk_s - alberk_s) * Qmx_NK  ! Kieselalgen
   nwbl = up_NB_s * (ablbr_s - algab_s)                                   ! Blaualgen
   
   ! Ammoniumaufnahme der Algen
   alpha_upN4k = 0.
   alpha_upN4g = 0.
   alpha_upN4b = 0.
   
   akiNH4_s = 0.
   agrNH4_s = 0.
   ablNH4_s = 0.
   
   if (nwgr /= 0.0 .or. nwki /= 0.0 .or. nwbl /= 0.0) then
      select case (j_up)
         case(0) ! Quelle unklar
            ! Kieselalgen
            alpha_upN4k = vNH4_s * vNO3_s / (( akksN + vNH4_s) * (akksN + vNO3_s))  &
                        + vNH4_s * akksN  / ((vNH4_s + vNO3_s) * (akksN + vNO3_s))
            
            ! Grünalgen
            alpha_upN4g = vNH4_s * vNO3_s / (( agksN + vNH4_s) * (agksN + vNO3_s))  &
                        + vNH4_s * agksN  / ((vNH4_s + vNO3_s) * (agksN + vNO3_s))
            
            ! Blaualgen
            alpha_upN4b = vNH4_s * vNO3_s / (( abksN + vNH4_s) * (abksN + vNO3_s))  &
                        + vNH4_s * abksN  / ((vNH4_s + vNO3_s) * (abksN + vNO3_s))
            
          case(1) ! nach Yajnik & Sharada (2003)
            a_up = 1.
            b_up = 3.
            
            ! Kieselalgen
            hc_upN3     = vNO3_s * (1. + a_up * vNH4_s) / ((akksN + vNO3_s) * (1. + b_up * vNH4_s))
            hc_upN4     = vNH4_s /  (akksN + vNH4_s)
            alpha_upN4k = hc_upN4 / (hc_upN3 + hc_upN4)
            
            ! Grünalgen
            hc_upN3     = vNO3_s * (1. + a_up * vNH4_s) / ((agksN + vNO3_s) * (1. + b_up * vNH4_s))
            hc_upN4     = vNH4_s /  (agksN + vNH4_s)
            alpha_upN4g = hc_upN4 / (hc_upN3 + hc_upN4)
            
            ! Blaualgen
            hc_upN3     = vNO3_s * (1. + a_up * vNH4_s) / ((abksN + vNO3_s) * (1.+ b_up * vNH4_s))
            hc_upN4     = vNH4_s / (abksN + vNH4_s)
            alpha_upN4b = hc_upN4 / (hc_upN3 + hc_upN4)
         
         case default
            write(message,"(a,i0)") "Subroutine nitrogen: Variable 'j_up' set to unknown option:" // &
                                    new_line('a') //                                                 &
                                    "j_up = ", j_up
            call qerror(message)
      end select
      
      alpha_upN4k = min(1.,alpha_upN4k)
      alpha_upN4g = min(1.,alpha_upN4g)
      alpha_upN4b = min(1.,alpha_upN4b)
      
      akiNH4_s = nwki * alpha_upN4k
      agrNH4_s = nwgr * alpha_upN4g
      ablNH4_s = nwbl * alpha_upN4b
   endif
  
  ! Nitrataufnahme der Algen
  agrNO3_s = (1. - alpha_upN4g) * nwgr
  akiNO3_s = (1. - alpha_upN4k) * nwki
  ablNO3_s = (1. - alpha_upN4b) * nwbl
  
   ! --------------------------------------------------------------------------
   ! ammonium
   ! --------------------------------------------------------------------------
   vNH4t = vNH4_s                    & !
         - susn_s                    & ! zu Nitrit oxidiertes Ammonium (Nitrosomonas)
         - PflN1_s                   & ! zu Nitrit oxidiertes Ammonium (Makrophyten)
         + doN_s                     & ! Ammoniumfreisetzung beim C-Abbau (wird im Baustein <orgC> berechnet)
         + dzN                       & ! Ammoniumfreisetzung durch Zooplankton 
         + ddrN                      & ! Ammoniumfreisetzung durch Dreissena
         - agrNH4_s                  & ! Ammoniumaufnahme der Grünalgen
         - akiNH4_s                  & ! Ammoniumaufnahme der Kieselalgen
         - ablNH4_s                  & ! Ammoniumaufnahme der Blaualgen
         + hJNH4_s * tflie / tiefe_s   ! Ammoniumflux aus dem Sediment
   
   if (isnan(vnh4t)) then
      print*,"isNaN(vnh4t) in subroutine nitrogen."
      print*, "   vNH4   = ", vNH4_S
      print*, "   susn   = ", susn_s
      print*, "   hJNH4  = ", hJNH4_s
      print*, "   tiefe  = ", tiefe_s
      print*, "   pfln1  = ", pfln1_s
      print*, "   dzN    = ", dzN
      print*, "   ddrN   = ", ddrN
      print*, "   agrnh4 = ", agrnh4_s
      print*, "   akinh4 = ", akinh4_s
      print*, "   ablnh4 = ", ablnh4_s
      print*, "   doN    = ", doN_s
      
      call qerror("Variable 'vNH4t' became NaN in subroutine nitrogen.")
   end if
   
   if (vNH4t < 0.0) then
      delNH4 = vNH4t - vNH4_s
      vNH4t  = (vNH4_s / (vNH4_s + abs(delNH4))) * vNH4_s
   endif
   if (vNH4t < 0.0001) vNH4t = 0.0001
   
   ! --------------------------------------------------------------------------
   ! nitrite (NO2)
   ! --------------------------------------------------------------------------
   vNO2t = 0.
   if (vx02_s > 0.0) then
      vNO2t = vNO2_s  &
            + susn_s  & ! zu Nitrit oxidiertes Ammonium (Nitrosomonas)
            + PflN1_s & ! zu Nitrit oxidiertes Ammonium (Makrophyten)
            - susn2_s & ! zu Nitrat oxidiertes Nitrit (Nitrosomonas)
            - PflN2_s   ! zu Nitrat oxidiertes Nitrit (Makrophyten)
      
      if (vNO2t < 0.0) then
         delNO2 = vNO2t - vNO2_s
         vNO2t = (vNO2_s / (vNO2_s + abs(delNO2))) * vNO2_s
      endif
      if (vNO2t < 0.0001) vNO2t = 0.0001
   endif
   
   ! --------------------------------------------------------------------------
   ! nitrate (NO3)
   ! --------------------------------------------------------------------------
   DenWatz = bsbCt_s * (KMO_NO3 / (KMO_NO3 + vO2_s)) * (vNO3_s /(vNO3_s + KM_NO3))
   dNO3Den = 0.93 * DenWatz
   
   
   if (vx02_s > 0.0) then
      vno3t = vno3_s                      &
            + susn2_s                     &
            + pfln2_s                     &
            + hJNO3_s * tflie / tiefe_s
   else
      vno3t = vno3_s                      &
            + susn_s                      &
            + pfln1_s                     &
            + hJNO3_s * tflie / tiefe_s
   endif
   
   vno3t = vno3t     &
         - agrNO3_s  &
         - akiNO3_s  &
         - ablNO3_s  &
         - dNO3Den
   
   if (isnan(vno3t)) then
      print*,"isNaN(vNO3t) in subroutine nitrogen."
      print*, "   vNO3    = ", vno3_s
      print*, "   hJNO3   = ", hjno3_s
      print*, "   tiefe   = ", tiefe_s
      print*, "   agrNO3  = ", agrno3_s
      print*, "   akiNO3  = ", akino3_s
      print*, "   ablNO3  = ", ablno3_s
      print*, "   dNO3den = ", dno3den
      
      if (vx02_s > 0.0) then
         print*, "   susn2   = ", susn2_s
         print*, "   pfln2   = ", pfln2_s
      else
         print*, "   susn    = ", susn_s
         print*, "   pfln1   = ", pfln1_s
      endif
      
      call qerror("Variable 'vNO3t' became NaN in subroutine nitrogen.")
   end if
   
   if (vNO3t < 0.0) then
      delNO3 = vNO3t - vNO3_s
      vNO3t = (vNO3_s / (vNO3_s + abs(delNO3))) * vNO3_s
   endif
   if (vNO3t < 0.000002) vNO3t = 0.000002
   
   ! Ausgabewert des NitratFluxes Wasser/Sediment in mgN/(l*h)
   hFluN3_s = hFluN3_s + hJN2_s * tflie /tiefe_s
   
   ! C-Abbau durch Denitrifikation in der Wassersäule
   dC_DenW_s = dNO3Den / 0.93
   
   ! -------------------------------------------------------------------------
   ! total N
   ! -------------------------------------------------------------------------
   if (gesN_s > 0.0) then
      gesNt = gesN_s                         & !
            - sedalk_s * Q_NK_s              & ! sedimentierte Kieselalgen
            - sedalg_s * Q_NG_s              & ! sedimentierte Grünalgen
            - sedalb_s * Q_NB_s              & ! sedimentierte Blaualgen
            - algdrk_s * Q_NK_s              & ! durch Dreissena konsumierte Kieselalgen
            - algdrg_s * Q_NG_s              & ! durch Dreissena konsumierte Grünalgen
            - algdrb_s * Q_NB_s              & ! durch Dreissena konsumierte Blaualgen
            - (albewk_s - alberk_s) * Qmx_NK & ! Nettowachstum benthischer Kieselagen
            - (albewg_s - alberg_s) * Qmx_NG & ! Nettowachstum benthischer Grünalgen
            - orgCsd_s * nl0_s               & ! 
            - dNO3Den                        & !
            + hJNH4_s * tflie / tiefe_s      & ! Ammoniumflux aus dem Sediment
            + hJNO3_s * tflie / tiefe_s      & ! Nitratflux aus dem Sediment
            + up_N2_s * (abltbr_s - algab_s)   ! Stickstoffaufnahme aus der Luft durch Blaualgen up_N2z(nkz,ior)
      
      if (gesNt < 0.001) gesNt = 0.001
   else
      gesNt = gesN_s
   endif
   
   ! -----------------------------------------------------------------------
   ! update return values
   ! -----------------------------------------------------------------------
   vNH4_s = vNH4t
   vNO2_s = vNO2t
   vNO3_s = vNO3t
   gesN_s = gesNt
   
   return
end subroutine nitrogen