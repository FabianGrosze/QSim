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
!! @author Volker Kirchesch Jens Wyrwa
!! @date 01.11.2021
subroutine erosion_kern(tflie, tiefe_s, rau_s, vmitt_s, SSeros_s, ss_s,      &
                        ssalg_s, dsedh_s, tausc_s, m_eros_s, n_eros_s,       &
                        sedroh_s,                                            &
                        control,jjj)
   implicit none
   
   ! dummy arguments
   real, intent(in)    :: tflie     !< Zeitschritt [d]
   real, intent(in)    :: tiefe_s   !< Wassertiefe
   real, intent(in)    :: rau_s     !< <i>Dokumentation fehlt noch</i>
   real, intent(in)    :: vmitt_s   !< Fließgeschwindigkeit
   real                :: SSeros_s  !< <i>Dokumentation fehlt noch</i>
   real                :: ss_s      !< Schwebstoffe
   real                :: ssalg_s   !< <i>Dokumentation fehlt noch</i>
   real                :: dsedh_s   !< Sohlhöhenänderung im aktuellen Zeitschritt [mm]
   real                :: tausc_s   !< <i>Dokumentation fehlt noch</i>
   real                :: m_eros_s  !< <i>Dokumentation fehlt noch</i>
   real                :: n_eros_s  !< <i>Dokumentation fehlt noch</i>
   real                :: sedroh_s  !< Rohdichte des Sediments [kg/m3]
   real                :: dRero_s   !< Erosionsrate je Zeitschritt [kg/m2]
   logical, intent(in) :: control  !< debugging
   integer, intent(in) :: jjj       !< debugging
   
   ! local variables
   real                :: UST, tau_s
   
   real, parameter     :: roh2o = 1000.
   real, parameter     :: g = 9.81
   ! --------------------------------------------------------------------------
   
   if (control) then
      print*,'erosion_kern tiefe, rau, vmitt,', tiefe_s, rau_s, vmitt_s
      print*,'tausc,M_eros,n_eros,sedroh = '  , tausc_s, m_eros_s,n_eros_s,sedroh_s
   endif
   
   
   
   ! Berechnung der Sohlschubspannung
   ust   = (g / rau_s)**0.5 / (tiefe_s**0.166667) * abs(vmitt_s)
   tau_s = ust**2 * roh2o
   
   if (tau_s > tausc_s .and. tausc_s > 0.0 .and. sedroh_s > 0.0 .and. tiefe_s > 0.0) then
      dRero_s  = m_eros_s * (tau_s / tausc_s - 1.)**n_eros_s
      dRero_s  = dRero_s * tflie * 86400.
      dsedh_s  = 1000.0 * dRero_s / sedroh_s
      SSeros_s = dRero_s / tiefe_s
      ss_s     = ss_s    + SSeros_s * 1000.
      ssalg_s  = ssalg_s + SSeros_s * 1000.
   else
      dsedh_s  = 0.
      SSeros_s = 0.
   endif
   
   if (control) then 
      print*, 'erosion_kern tau_s,SS,SSalg,SSeros,dsedH,jjj = ',  &
               tau_s,ss_s,ssalg_s,SSeros_s,dsedh_s,jjj
   endif
   
end subroutine erosion_kern
