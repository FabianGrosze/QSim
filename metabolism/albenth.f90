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

!> Berechnung des Einflusses benthischer Algen.
!! @author Volker Kirchesch
!! @date 02.02.1996
subroutine albenth(abegm2_s, abekm2_s, vnh4_s, vno3_s, gelp_s, &
                   si_s, vmitt_s, tempw_s, schwi_s, extk_s,    &
                   tiefe_s , tflie,                            &
                   albewg_s, alberg_s, albewk_s, alberk_s,     &
                   cmatgr_s, cmatki_s,                         & 
                   control, jjj)
   use module_aparam
   use module_alloc_dimensions
   implicit none
   
   ! --- dummy arguments ---
   real,    intent(inout) :: abegm2_s !< TODO
   real,    intent(inout) :: abekm2_s !< TODO
   real,    intent(in)    :: vnh4_s   !< ammonium
   real,    intent(in)    :: vno3_s   !< nitrate
   real,    intent(in)    :: gelp_s   !< dissolved phosphor
   real,    intent(in)    :: si_s     !< silicate
   real,    intent(in)    :: vmitt_s  !< flow velocity
   real,    intent(in)    :: tempw_s  !< water temperature
   real,    intent(in)    :: schwi_s  !< global radiation [cal/(cm2 h)]
   real,    intent(in)    :: extk_s   !< coefficient for light extinction
   real,    intent(in)    :: tiefe_s  !< water depth
   real,    intent(in)    :: tflie    !< timestep [d]
   real,    intent(out)   :: albewg_s !< growth benthic green algae
   real,    intent(out)   :: alberg_s !< respiration benthic green algea
   real,    intent(out)   :: albewk_s !< growth benthic diatoms
   real,    intent(out)   :: alberk_s !< respiration benthic diatoms
   real,    intent(out)   :: cmatgr_s !< TODO
   real,    intent(out)   :: cmatki_s !< TODO
   logical, intent(in)    :: control !< debugging
   integer, intent(in)    :: jjj      !< debugging
   
   ! --- local variables ---
   real    :: w, zperi, zperii, x, vlim, veloci, topt, tmax
   real    :: respg, obfli, fta, ftak, ftag, frespl, fnk, fnk3, fnk2, fnk1
   real    :: fng, fng2, fng1, flicht, fik, fig, b2, b1, algip1, akgrow
   real    :: aggrow, abkre, abgre, abeki, abekit, abegr, abegrt
   real    :: kx, igrenz, kxi
   
   ! Werte wurden dem Modell AQUATOX entnommen, Dim.: mueE/(m2*s)
   real, parameter :: saetbk = 147.     ! Sättigungslichtstärke für Kieselalgen
   real, parameter :: saetbg = 176.     ! Sättigungslichtstärke für Grünalgen
   real, parameter :: vkoff1 = 0.2
   real, parameter :: vkoff2 = 0.057
   real, parameter :: roPeri = 1030000. ! Dichte des Periphytons in g/m3 (s. Uhlmann)
   
   real, parameter :: lnq = 0.61519
   
   
   ! schwi*4.2 - umrechnung von cal/(cm2*h) in j/(cm2*h)
   ! obfli = 8.2051 * schwi_s * 4.2
   ! vorläufig bis zur endgültigen klärung
   obfli = 5.846 * schwi_s * 4.2
   if (obfli == 0.0) obfli = 0.0001
   
   cmatgr_s = 0.0
   cmatki_s = 0.0
   
   alberg_s = 0.0
   alberk_s = 0.0
   albewg_s = 0.0
   albewk_s = 0.0
   if (abegm2_s == 0.0 .and. abekm2_s == 0.0) return
   
   abegr = abegm2_s
   abeki = abekm2_s
   
   
   ! Kx    - Berücksichtigt, dass mit zunehmender Schichtdicke
   !         die unteren Schichten weniger Licht bekommen
   ! zPeri - Dicke des Periphytonrasens [m]
   zperi = (abegr + abeki) / roperi
   kx = 0.015 * roperi * zperi / 2.
   
   ! Temperaturabhaengigkeit der Wachstumsrate fuer Gruenalgen
   tmax = 45.0
   topt = 27.0
       
   w = lnq * (tmax - topt)
   x = (w**2 * (1 + sqrt(1. + 40./w))**2) / 400.
   fta = ((tmax - tempw_s) / (tmax - topt))**x
   ftag = fta * exp(x * (1. - ((tmax - tempw_s) / (tmax - topt))))
   
   ! Temperaturabhaengigkeit der Wachstumsrate fuer Kieselalgen
   tmax = 31.
   topt = 20.
   if (tempw_s >= tmax) then
      ftak = 0.0
   else
      w = lnq * (tmax - topt)
      x = (w**2 * (1. + sqrt(1. + 40./w))**2) / 400.
      fta = ((tmax - tempw_s) / (tmax - topt))**x
      ftak = fta * exp(x * (1. - ((tmax - tempw_s) / (tmax-topt))))
   endif
  
   ! Berechnung des vertikalen Lichtklimas
   if (obfli <= 0.0001) then
      algip1 = 0.0
      fik = 0.0
      fig = 0.0
      flicht = 1.
   else
      algip1 = obfli * exp(-extk_s * tiefe_s)
      igrenz = algip1 * 0.01
      kxi = log(algip1) - log(igrenz)
      zperii = kxi / (0.015 * roperi)
      zperi = (abegr + abeki) / roperi
      
      if (zperi > zperii) then
         kx = 0.015 * roperi * zperii / 2.
         flicht = zperii / zperi
      else
         kx = 0.015 * roperi * zperi / 2.
         flicht = 1.
      endif
      
      algip1 = algip1 * exp(-kx)
      if (algip1 > saetbk) then
         fik = 1.
      else
         fik = (algip1 / saetbk) * exp(1. - (algip1 / saetbk))
      endif
      
      if (algip1 > saetbg) then
         fig = 1.
      else
         fig = (algip1 / saetbg) * exp(1. - (algip1 / saetbg))
      endif
      
      ! Abhängigkeit der Wachstumsrate vom Nährstoffangebot
      fng1 = (vno3_s + vnh4_s) / (agksn + vno3_s + vnh4_s)
      fng2 = gelp_s / (agksp + gelp_s)
      fng = fng1
      if (fng2 < fng) fng = fng2
      
      fnk1 = (vno3_s + vnh4_s) / (akksn + vno3_s + vnh4_s)
      fnk2 = gelp_s / (akksp  + gelp_s)
      fnk3 = si_s   / (akkssi + si_s)
      fnk = fnk1
      if (fnk2 < fnk) fnk = fnk2
      if (fnk3 < fnk) fnk = fnk3
      
      ! Limitation des Wachstums durch die Fliessgeschwindigkeit
      ! (s. Modell AQUATOX)
      veloci = abs(vmitt_s) * 100.
      vlim = vkoff1 + ((vkoff2 * veloci) / (1. + (vkoff2 * veloci)))
      if (vlim > 1.) vlim = 1.
   endif
   
   
   ! Periphytonmat
   
   ! Vol = flae_s * elen_s
   ! CCPeri = 80.
   ! hconC = (CCPeri - (abegr + abeki)) / CCPeri
   ! cmatgr_s = hconC * (abs(vabfl_s)/Vol) * abegr
   ! cmatki_s = hconC * (vabfl_s / Vol) * abeki
   ! cmatgr_s = cmatgr_s * tflie * 86400.
   ! cmatki_s = cmatki_s * tflie * 86400.
   aggrow = aggmax * fng * ftag * fig * vlim
   akgrow = akgmax * fnk * ftak * fik * vlim
   
   abegrt = abegr * flicht * exp(aggrow * tflie)
   abekit = abeki * flicht * exp(akgrow * tflie)
   
   albewg_s = abegrt - abegr
   albewk_s = abekit - abeki
   
   
   ! Berechnung der Respiration
   b1 = 1.7
   b2 = 0.187
   frespL = 0.2
   respG = 0.085
   
   ! Wachstumsrate ohne Temperaturberücksichtigung
   aggrow = aggmax * fng * fig * vlim
   akgrow = akgmax * fnk * fik * vlim
   abgre = respG + aggrow * frespL
   abkre = respG + akgrow * frespL
            
   alberg_s = abegr * (1. - (exp(-abgre * tflie)))
   alberk_s = abeki * (1. - (exp(-abkre * tflie)))
   alberg_s = alberg_s / (1. + exp(b1 - b2 * tempw_s))
   alberk_s = alberk_s / (1. + exp(b1 - b2 * tempw_s))
   
   abegm2_s = abegrt - alberg_s - (cmatgr_s * tiefe_s)
   abekm2_s = abekit - alberk_s - (cmatki_s * tiefe_s)
   
   ! Umrechnung auf mg/l
   albewg_s = albewg_s / tiefe_s
   albewk_s = albewk_s / tiefe_s
   alberg_s = alberg_s / tiefe_s
   alberk_s = alberk_s / tiefe_s
   cmatgr_s = albewg_s
   cmatki_s = albewk_s
      
   return
   
end subroutine albenth
