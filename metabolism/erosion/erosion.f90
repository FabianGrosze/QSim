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

!> Bestimmung der Erosionsrate
subroutine erosion(ss_s, ssalg_s, vmitt_s, tiefe_s, rau_s, tausc_s,        &
                   sedroh_s, m_eros_s, n_eros_s, tflie, sseros_s, dsedh_s, &
                   control, jjj)
   implicit none
   
   ! --- dummy arguments ---
   real, intent(inout) :: ss_s      !< Schwebstoffe
   real, intent(inout) :: ssalg_s   !< 
   real, intent(in)    :: vmitt_s   !< Fließgeschwindigkeit
   real, intent(in)    :: tiefe_s   !< Wassertiefe
   real, intent(in)    :: rau_s     !< 
   real, intent(in)    :: tausc_s   !< 
   real, intent(in)    :: sedroh_s  !< Rohdichte des Sediments [kg/m3]
   real, intent(in)    :: m_eros_s  !< 
   real, intent(in)    :: n_eros_s  !< 
   real, intent(in)    :: tflie     !< Zeitschritt [d]
   real, intent(out)   :: sseros_s  !< 
   real, intent(out)   :: dsedh_s   !< Sohlhöhenänderung im aktuellen Zeitschritt [mm]
   logical, intent(in) :: control   !< debugging
   integer, intent(in) :: jjj       !< debugging
   
   ! --- local variables ---
   real                :: ust, tau, drero
   
   real, parameter     :: roh2o = 1000.
   real, parameter     :: g = 9.81
   
   
   ! Berechnung der Sohlschubspannung
   ust   = (g / rau_s)**0.5 / (tiefe_s**0.166667) * abs(vmitt_s)
   tau = ust**2 * roh2o
   
   if (tau > tausc_s .and. tausc_s > 0.0 .and. sedroh_s > 0.0 .and. tiefe_s > 0.0) then
      drero  = m_eros_s * (tau / tausc_s - 1.)**n_eros_s
      drero  = drero * tflie * 86400.
      dsedh_s  = 1000.0 * drero / sedroh_s
      sseros_s = drero / tiefe_s
      ss_s     = ss_s    + sseros_s * 1000.
      ssalg_s  = ssalg_s + sseros_s * 1000.
   else
      dsedh_s  = 0.
      sseros_s = 0.
   endif
   
   if (control) then 
      print*, 'erosion_kern tau,ss,ssalg,sseros,dsedh,jjj = ',  &
               tau,ss_s,ssalg_s,sseros_s,dsedh_s,jjj
   endif
   
end subroutine erosion
