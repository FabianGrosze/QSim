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

!> Berechnung des geloesten Silikats
!! @author Volker Kirchesch
!! @date 24.08. 2011
!> Berechnung des gelösten Silikats
subroutine silicate(si_s, hJSi_s, up_Si_s, akibr_s,  &
                    algak_s, albewk_s,               &
                    tiefe_s, tflie,                  &
                    control, jjj)
   use module_aparam
   implicit none
   
   ! --- dummy arguments ---
   real, intent(inout)  :: si_s     !< Silikat-Silizium-Konzentration  
   real, intent(in)     :: hJSi_s   !< Silizium-Flux aus dem Sediment
   real, intent(in)     :: up_Si_s  !< Si-Aufnahmerate der Kieselalgen
   real, intent(in)     :: akibr_s  !< Bruttowachstum Biomasse Kieselalgen
   real, intent(in)     :: algak_s  !< Respirierte Biomasse Kieselalgen
   real, intent(in)     :: albewk_s !< Wachstum benthischer Kieselalgen
   real, intent(in)     :: tiefe_s  !< Wassertiefe [m]
   real, intent(in)     :: tflie    !< Zeitschritt [d]
   logical, intent(in)  :: control !< debugging
   integer, intent(in)  :: jjj      !< debugging
   
   ! --- local variables ---
   real :: dSiSed, akisi, sit, sit_old
   
   external :: print_clipping
   
   ! Neuberechnung der Silikatmenge an der Gewässersohle nach Rücklösung
   ! Änderungsrate durch Silikatfreisetzung aus dem Sediment
   dSised = hJSi_s * tflie/ tiefe_s 
   
   ! Einfluss der Kieselalgen
   akisi = - up_Si_s * (akibr_s - algak_s) &
           - albewk_s * Qmx_SK
   
   ! timestep
   sit = si_s + akisi + dSised
   
   if (sit < 0.0) then 
      sit_old = sit
      sit = (si_s / (si_s + abs(sit - si_s))) * si_s
      call print_clipping("silicate", "sit", sit_old, sit, "mg/l")
   endif
   
   ! update return value
   si_s = sit
end subroutine silicate
