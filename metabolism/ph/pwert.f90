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

!> Berechnung des p-Wertes aus dem m-Wert und dem ph-Wert
!! @author Volker Kirchesch
!! @date 05.07.1993
subroutine pwert(mw_s, vph_s, lf_s, tempw_s, pw_s)
   implicit none
   
   real, intent(in)  :: mw_s         !< m-Wert
   real, intent(in)  :: vph_s        !< pH-Wert [-]
   real, intent(in)  :: lf_s         !< Leitfähigkeit
   real, intent(in)  :: tempw_s      !< Wassertemperatur [°C]
   real, intent(out) :: pw_s         !< p-Wert
   
   ! --- local varibles ---
   real     :: K1,K2,MUE,lgk1,lgk2,lgh,lgoh, abst, pk1, pk2, pkw, hk
   real     :: c, ph0, ph1, ph2, delph1, delph2
   real     :: poh, h, oh, y, y1, y2, eta
   integer  :: itera
   
   ! Berechnung der absoluten Temperatur
   abst = tempw_s + 273.16
   
   ! Berechnung der negativen Logarithmen der Dissoziationskonstanten b
   ! in Abhängigkeit von der absoluten Temperatur für Konzentrationen in mo
   pk1 = (17052./abst) + 215.21 * log10(abst) - 0.12675 * abst - 545.56
   pk2 = (2902.39/abst) + 0.02379 * abst - 6.498
   pkw = (4471.33/abst) + 0.017053 * abst - 6.085
   
   ! Einfluss der Ionenstärke
   if (lf_s < 0.0) then 
      mue = 0.0
   else
      mue = 1.7e-5 * lf_s
   endif
   lgk1 = sqrt(mue)/(1.+1.4*sqrt(mue))
   lgk2 = (2.*sqrt(mue))/(1.+1.4*sqrt(mue))
   hk = (0.5*sqrt(mue))/(1.+1.4*sqrt(mue))
   
   ! Berechnung der von Temperatur und Ionenstärke abhängigen Konstanten
   k1 = 10**(-pk1 + lgk1)
   k2 = 10**(-pk2 + lgk2)
   
   pw_s = 0.0
   do itera = 1, 500
      c = (mw_s * 1.e-3) - pw_s
   
      ! --- Berechnung der Konzentrationen an H+ und OH- ---
      ! Schaetzer
      ph1 = 0.
      ph2 = 14.
      
      do while (.true.)
         ph0 = (ph1+ph2)/2.
         poh = pkw-ph0
         lgh = ph0 -hk
         lgoh = poh-hk
         h = 10**(-lgh)
         oh = 10**(-lgoh)
         y1 = oh-h
   
         ! Berechnung des Aequivalenzfaktors eta
         eta = (k1*h+2*k1*k2)/((h**2)+k1*h+k1*k2)
         y2 = (mw_s * 1.e-3) - c * eta
         y = y2-y1
   
         if (ph2-ph1 < 0.001) exit
         if (y < 0.0) then
            ph2 = ph0
         else
            ph1 = ph0
         endif
      enddo
   
      delph1 = vph_s -ph1
   
      if (abs(delph1) < 0.02) exit
      
      if (delph1 < 0.0) then
         pw_s = pw_s - 0.000005
      else
         pw_s = pw_s + 0.000005
      endif
   enddo
   
   pw_s = pw_s *1000.
   return
end subroutine pwert
