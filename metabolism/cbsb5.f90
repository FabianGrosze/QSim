  subroutine CBSB5(BL0, BL0t, obsb, obsbt, ior, mstr)



 real                                 ::  k_BSB
 real, Dimension(1000)                ::  obsb

 save deltat

      deltat = 5.
                                                       

!########################
!    BSB5-Berechnung
!########################

      hc_wert = min(0.95,obsb(ior)/Bl0)  
      k_BSB = (-1.*log(1.-hc_wert))/deltat        ! Abbaurate 1. Ordnung berechnet aus dem alten Zeitschritt

      obsbt = BL0t*(1.-exp(-k_BSB*deltat))

  end subroutine CBSB5
