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

!> Berechnung des pH-Wertes aus dem m-Wert und der Kohlensäuresumme
!! @author Volker Kirchesch
!! @date 11.06.1993
subroutine ph(mw_s, pw_s, ca_s, lf_s, tempw_s, vph_s, vco2_s,              &
              tflie, rau_s, vmitt_s, tiefe_s, rhyd_s, flae_s,              &
              wge_s, WLage_s, hWS_s, iphy,                                 &
              bsbct_s, resdr_s, dzres1_s, dzres2_s,                        &
              dalgki_s, dalggr_s, dalgbl_s, dalgak_s, dalgag_s, dalgab_s,  &
              alberg_s, alberk_s, albewg_s, albewk_s,                      &
              susn_s, po2p_s, po2r_s, ssalg_s, stind_s,                    &
              kontroll, jjj)
   
   use aparam, only: Caki, Cabl, Cagr, CRot, CDr
   implicit none
   ! --- dummy arguments ---
   real, intent(inout)     :: mw_s      !< m-Wert
   real, intent(inout)     :: pw_s      !< p-Wert
   real, intent(inout)     :: ca_s      !< Calcium
   real, intent(inout)     :: lf_s      !< Leitfähigkeit
   real, intent(in)        :: tempw_s   !< Wassertemperatur [°C]
   real, intent(inout)     :: vph_s     !< pH-Wert [-]
   real, intent(inout)     :: vco2_s    !< CO2
   real, intent(in)        :: tflie     !< Zeitschritt [d]
   real, intent(in)        :: rau_s     !< Kst
   real, intent(in)        :: vmitt_s   !< Geschwindigkeit
   real, intent(in)        :: tiefe_s   !< Tiefe
   real, intent(in)        :: rhyd_s    !< hydraulischer Radius
   real, intent(in)        :: flae_s    !< Oberfläche
   real, intent(in)        :: wge_s     !< Windgeschwindigkeit
   real, intent(in)        :: WLage_s   !< Höhenlage
   real, intent(in)        :: hWS_s     !< WSP
   integer, intent(in)     :: iphy      !< Steuerung Belüftungsverfahren
   real, intent(in)        :: bsbct_s   !< CO2-Produktion der Bakterien
   real, intent(in)        :: resdr_s   !< CO2-Produktion der Muscheln
   real, intent(in)        :: dzres1_s  !< CO2-Produktion des Zooplanktons
   real, intent(in)        :: dzres2_s  !< CO2-Produktion des Zooplanktons
   real, intent(in)        :: dalgki_s  !< CO2 Verbrauch und Produktion von planktischen Algen
   real, intent(in)        :: dalggr_s  !< CO2 Verbrauch und Produktion von planktischen Algen
   real, intent(in)        :: dalgbl_s  !< CO2 Verbrauch und Produktion von planktischen Algen
   real, intent(in)        :: dalgak_s  !< CO2 Verbrauch und Produktion von planktischen Algen
   real, intent(in)        :: dalgag_s  !< CO2 Verbrauch und Produktion von planktischen Algen
   real, intent(in)        :: dalgab_s  !< CO2 Verbrauch und Produktion von planktischen Algen
   real, intent(in)        :: alberg_s  !< CO2 Verbrauch und Produktion bentischer Algen
   real, intent(in)        :: alberk_s  !< CO2 Verbrauch und Produktion bentischer Algen
   real, intent(in)        :: albewg_s  !< CO2 Verbrauch und Produktion bentischer Algen
   real, intent(in)        :: albewk_s  !< CO2 Verbrauch und Produktion bentischer Algen
   real, intent(in)        :: susn_s    !< oxidiertes Ammonium
   real, intent(in)        :: po2p_s    !< CO2 Verbrauch der Makrophyten
   real, intent(in)        :: po2r_s    !< CO2 Produktion der Makrophyten
   real, intent(in)        :: ssalg_s   !<
   real, intent(inout)     :: stind_s   !<
   logical, intent(in)     :: kontroll  !< debugging
   integer, intent(in)     :: jjj       !< debugging
   
   ! --- local variables ---
   real     :: mwt,pwt,cat,lft
   real     :: bbeis,abst,pk1,pk2,pkw,pkca,mue,lgk1,lgk2,lgkca,hk,k1,k2,kca
   real     :: lgh,h,poh,lgoh,oh,beta,fco2,fhco3,fco3,moco2,mohco3,moco3
   real     :: mgco2,mghco3,mgco3,fca
   real     :: MOCA,c,SAETCO2,DEFCO2,MOLGCO,HCON,DCO2O,BKCO2,CO2BSB,CO2DR
   real     :: CO2ZOO,ALCO2M,PFCO2R,ALHCO3,CO2ALW,CO2PFW,FPFL,FALG,DELTAH
   real     :: GHCO31,GCO21,DCA,DCAH,DLF,RHCO3,RCO3,HCONTI
   real     :: UEBCA,TIND,CUEBCA,R1,R2,DCA1,DCA2,GCO31,CAV1,PH1,PH2
   integer  :: IITER
   real     :: PH0,Y1,ETA,Y2,Y,DELPH,DELCA,DELLF,DELMW
   
   ! Berechnung der Kohlensäuresumme [mol/l]
   mw_s = mw_s*1.e-3
   pw_s = pw_s*1.e-3
   moca = ca_s/(1000.*40.08)
   c = mw_s - pw_s
   
   ! Berechnung der absoluten Temperatur
   ! TODO (schoenung, 2022): must be 273.15
   abst = tempw_s + 273.16
   
   ! Berechnung der negativen Logarithmen der Dissoziationskonstantenl
   ! bei einer Ionenstaerke von 0 mol/l
   ! in Abhängigkeit der absoluten Temperatur
   pk1 = (17052./abst) + 215.21 * log10(abst) - 0.12675*abst - 545.56
   pk2 = (2902.39/abst) + 0.02379 * abst - 6.498
   pkw = (4471.33/abst) + 0.017053 * abst - 6.085
   kca = 9.41e-9                 &
       - 2.52e-10 * tempw_s       &
       + 2.76e-12 * tempw_s**2    &
       - 1.14e-14 * tempw_s**3
   
   pkca = alog10(kca) * (-1.)
   
   ! Einfluss der Ionenstärke
   if (lf_s < 0.0) lf_s = 0.0
   mue = 1.7e-5*lf_s
   lgk1 = sqrt(mue) / (1.+1.4*sqrt(mue))
   lgk2 = (2.*sqrt(mue)) / (1.+1.4*sqrt(mue))
   lgkca = (4.*sqrt(mue)) / (1.+3.9*sqrt(mue))
   hk = (0.5*sqrt(mue)) / (1.+1.4*sqrt(mue))
   
   ! Berechnung der von Temperatur und Ionenstärke abhängigen Konstanten
   k1 = pk1-lgk1
   k2 = pk2-lgk2
   kca = pkca-lgkca
   k1 = 10**(-k1)
   k2 = 10**(-k2)
   kca = 10**(-kca)
   
   ! Berechnung der Konzentrationen an H+ und OH-
   lgh = vph_s-hk
   h = 10**(-lgh)
   poh = pkw-vph_s
   lgoh = poh-hk
   oh = 10**(-lgoh)
   
   ! Berechnung der Kohlenssäureformen(nur einmal)
   beta = 1+(k1/h)+(k1*k2/h**2)
   fco2 = 1./beta
   fhco3 = k1/(beta*h)
   fco3 = (k1*k2)/(beta*h**2)
   
   moco2  = c * fco2
   mohco3 = c * fhco3
   moco3  = c * fco3
   
   mgco2  = moco2  * 44.000 * 1000.
   mghco3 = mohco3 * 61.020 * 1000.
   mgco3  = moco3  * 60.009 * 1000.
   
   
   fca = 1+(h/k2)+(h**2/(k1*k2))
   
   ! phsikalischer CO2 Ein- und Austrag über die Gewässeroberfläche
   saetco2 = 1.27                    &
           - 0.04657    * tempw_s    &
           + 0.0009662  * tempw_s**2 &
           - 0.00000907 * tempw_s**3
           
           
   DefCO2 = saetco2 - MGCO2
   molgco = 44.
   hcon = sqrt(32./molgco)
   dco2o = 0.0
   
   ! Berechnung des Belüftungsbeiwerts [1/d]
   call belueftung_k2(rau_s,tiefe_s,vmitt_s,rhyd_s,flae_s,tempw_s,WLage_s, &
                      hws_s,wge_s,iphy,bbeis)
   bkco2 = hcon * bbeis
   DCO2o = defco2*(1.-exp(-bkco2*tflie))
   
   if (kontroll) then
      print*, 'ph Oberflaechenbelueftung:'
      print*, '  DCO2o   = ', DCO2o
      print*, '  defco2  = ', defco2
      print*, '  saetco2 = ', saetco2
      print*, '  MGCO2   = ', MGCO2
      print*, '  rau     = ', rau_s
      print*, '  tiefe   = ', tiefe_s
      print*, '  vmitt   = ', vmitt_s
      print*, '  rhyd    = ', rhyd_s
      print*, '  flae    = ', flae_s
      print*, '  tempw   = ', tempw_s
      print*, '  WLage   = ', WLage_s
      print*, '  hws     = ', hws_s
      print*, '  wges    = ', wge_s
      print*, '  iPhy    = ', iphy
      print*, '  bbei    = ', bbeis   
   endif
   
   
   ! CO2 Lieferung durch C-Abbau
   co2bsb = bsbct_s
   
   ! CO2 Lieferung bei der Atmung von Dreissen und Zooplankter
   CO2dr = resdr_s * CDR
   
   co2zoo = (dzres1_s + dzres2_s) * CRot
   alco2m = dalgak_s * Caki &
          + dalgag_s * Cagr &
          + dalgab_s * Cabl &
          + alberg_s * Cagr &
          + alberk_s * Caki
   
   ! Umrechnung Molmasse CO2/ O2 !war: pfco2r = po2rs*1.3
   pfco2r = po2r_s * (44.0/32.0)
   
   mgco2 = mgco2     &
         + alco2m    &
         + pfco2r    &
         + co2bsb    &
         + dco2o     &
         + co2dr     &
         + co2zoo
   
   !  CO2 Verbrauch durch Algenwachstum und Pflanzen
   alhco3 = 0.0
   co2alw = dalgki_s * Caki    &
          + dalggr_s * Cagr    &
          + dalgbl_s * Cabl    &
          + albewg_s * Cagr    &
          + albewk_s * Caki
   ! Umrechnung Molmasse CO2/ O2 ! War fälsclicherwiese: co2pfw = po2ps/1.3
   co2pfw = po2p_s*(44.0/32.0) 
   
   if (co2alw + co2pfw > mgco2) then
      if (co2pfw + co2alw == 0.0) then
         fpfl = 0.0
         falg = 0.0
      else
         fpfl = co2pfw/(co2pfw+co2alw)
         falg = co2alw/(co2pfw+co2alw)
      endif
      alhco3 = ((co2pfw+co2alw)-mgco2)
      co2pfw = mgco2*fpfl
      co2alw = mgco2*falg
   endif
   
   
   ! Einfluss der Nitrifikation
   deltah = 2.*(susn_s/14.)/1000.
   h = h + deltah
   
   ghco31 = mghco3
   gco21  = mgco2
   mghco3 = mghco3 - alhco3
   mgco2  = mgco2 - co2alw - co2pfw
   
   if (mghco3 < 0.0) mghco3 = (ghco31/(ghco31+alhco3))*ghco31
   if (mgco2  < 0.0) mgco2 = (gco21/(gco21+co2alw+co2pfw))*gco21
   
   vco2_s = mgCo2
   
   DCA = alhco3*0.328
   DCAH = DCA
   cat = ca_s - dca
   
   if (cat < 0.0) then
      cat = (ca_s/(ca_s+dca)) * ca_s
      DCA = Cat - ca_s
   endif
   
   moca = cat/(1000.*40.08)
   dLf = DCA * 2.2
   LFt = lf_s - dLf
   
   mohco3 = mghco3/61.02/1000.
   moco2 = mgco2/44.0/1000.
   moco3 = mgco3/60.009/1000.
   
   
   ! Berechnung der Calcitbildung
   dca = 0.0
   rhco3 = 0.0
   rco3 = 0.0
   hconti = tflie*1440.
   
   ! Berechnung der Calciumübersättigung
   uebca = moca*moco3/kca
   tind = (1577.*exp(-0.0496*uebca))*10./ssalg_s
   
   if (kontroll) then 
      if (tind > stind_s) then
         print*,'pH: keine Calcium Aenderung'
      else
         print*,'pH: Calcium Aenderung'
      endif
      
      print*, '  tind    = ', tind
      print*, '  stinds  = ', stind_s
      print*, '  uebca   = ', uebca
      print*, '  Ca      = ', cat
   endif
   
   if (tind > stind_s) goto 55
   72 continue
   
   if (lf_s < 0.0)lf_s = 0.0
   mue = 1.7e-5*lf_s
   lgkca = (4.*sqrt(mue))/(1.+3.9*sqrt(mue))
   kca = pkca-lgkca
   kca = 10**(-kca)
   
   uebca = moca*moco3/kca
   dca = 0.0
   rhco3 = 0.0
   rco3 = 0.0
   if (uebca < 10.0)goto 55
   
   dca = 0.0406*exp(0.0362*uebca)
   dca = dca*0.001*40.08*ssalg_s
   cuebca = (((uebca-10.)*kca)/moco3)*40.08*1000.
   if (dca > cuebca)dca = cuebca
   
   r1 = -37.47 * vph_s + 360.67
   r1 = r1/100.
   r2 = 1. - r1
   rhco3 = r1 * dca * 1.52
   rco3 = r2 * dca * 1.5
   dca1 = r1 * dca
   dca2 = r2 * dca
   if (rhco3 > mghco3)dca1 = mghco3/(r1*1.52)
   if (rco3 > mgco3)dca2 = mgco3/(r2*1.5)
   dca = dca1+dca2
   
   if (alhco3 > rhco3) then
      rhco3 = 0.0
      dca = dca2
   endif
   
   if (alhco3 <= rhco3) then
      rhco3 = rhco3-alhco3
      dca = dca-dcaH
   endif
   
   ghco31 = mghco3
   gco31 = mgco3
   mghco3 = mghco3-rhco3
   mgco3 = mgco3-rco3
   
   if (mghco3 < 0.0)mghco3 = (ghco31/(ghco31+rhco3))*ghco31
   
   if (mgco3 < 0.0)mgco3 = (gco31/(gco31+rco3))*gco31
   
   mohco3 = mghco3/61.02/1000.
   moco3 = mgco3/60.009/1000.
   
   cav1 = cat
   cat = cat-dca
   
   if (cat < 0.0) cat = (cav1/(cav1+dca))*cav1
   
   moca = cat/(1000.*40.08)
   DLF = DCA*2.2
   LFt = LFt-DLF
   
   55 continue
   !      poh = (-1.*(log10(h)))-pkw+2*hk
   !      oh = 10**poh
   mwt = mohco3+2*moco3+oh-h
   pwt = moco2*(-1.)+moco3+oh-h
   
   c = moco2 + mohco3 + moco3
   
   ! --- Berechnung der Konzentrationen an H+ und OH- ---
   ! Schätzer
   ph1 = 0.
   ph2 = 14.
   
   iiter = 0
   do while (.true.) 
      ph0 = (ph1+ph2)/2.
      poh = pkw-ph0
      lgh = ph0 -hk
      lgoh = poh-hk
      h = 10**(-lgh)
      oh = 10**(-lgoh)
      y1 = oh-h
   
      ! Berechnung des Äquivalenzfaktors eta
      eta = (k1*h+2*k1*k2)/((h**2)+k1*h+k1*k2)
      y2 = mwt-c*eta
      y = y2-y1
   
      delph = ph2-ph1
      if (delph < 0.001 .or. iiter == 50) exit
      
      if (y < 0.0) then
         ph2 = ph0
         iiter = iiter+1
      else
         ph1 = ph0
         iiter = iiter+1
      endif
   enddo
   
   vph_s = ph1
   hconti = hconti-1.
   if (hconti < 1 .or. uebca < 20) goto 56
   goto 72
   
   ! ------------------------------------------------------------------------
   ! update return values
   ! ------------------------------------------------------------------------
   56 continue
   mwt = mwt*1000.
   pwt = pwt*1000.
   stind_s = stind_s + tflie * 1440.
   
   delca = cat - ca_s
   dellf = lft - lf_s
   delmw = mwt - mw_s * 1000.
   mw_s = mw_s * 1000.
   pw_s = pw_s * 1000.
   
   if (cat < 0.0) cat = (ca_s / (ca_s + abs(delca))) * ca_s
   if (Lft < 0.0) Lft = (lf_s / (lf_s + abs(delLf))) * lf_s
   if (mwt < 0.0) mwt = (mw_s / (mw_s + abs(delmw))) * mw_s
   ca_s = cat
   mw_s = mwt
   pw_s = pwt
   lf_s = lft
   
end subroutine ph
