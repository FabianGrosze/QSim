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
   ! Sedimentparameter ehedem:
   ! M_eros(mstr,:) = 7.5e-4      !! Eingebbar machen!     ###
   ! n_eros(mstr,:) = 3.2         !! Eingebbar machen!     ###
   ! tausc(mstr,:)  = 1.25        !! Eingebbar machen!     ###
   ! sedroh(mstr,:) = 1450.       !! Eingebbar machen!     ###
   

!> Bestimmung der Erosionsrate
!! @author Volker Kirchesch Jens Wyrwa
!! @date 01.11.2021
subroutine erosion_kern(tflie, tiefe_s, rau_s, vmitt_s, tau_s, SSeros_s, ss_s,      &
                        ssalg_s, dsedh_s, tausc_s, m_eros_s, n_eros_s,       &
                        sedroh_s,                                            &
                        kontroll,ior,mstr)
   implicit none
   
   ! exchange variables
   real, intent(in)    :: tflie     !< Zeitschritt [d]
   real, intent(in)    :: tiefe_s   !< mittlere Wassertiefe [m]
   real, intent(in)    :: rau_s     !< Reibungsbeiwert nach Gauckler/Manning/Strickler "Rauhigkeit" [m^1/3/s]
   real, intent(in)    :: vmitt_s   !< Fließgeschwindigkeit [m/s]
   real                :: tau_s,tau     !< aktuelle Sohlschubspannung [kg/m*s²]
   real                :: SSeros_s  !< Erosionsmassenstrom [kg/(m²*s)]
   real                :: ss_s      !< Schwebstoff ohne Lebewesen [mg/l]
   real                :: ssalg_s   !< Schwebstoff mit Algen+Konsumenten [mg/l]
   real                :: dsedh_s   !< Sohlhöhenänderung im aktuellen Zeitschritt [mm]
   real                :: tausc_s   !< kritische Sohlschubspannung[kg/m*s²]
   real                :: m_eros_s  !< Erodibilitätsparameter [kg/m²*s] 
   real                :: n_eros_s  !< empirischer Erosionsexponent [-]
   real                :: sedroh_s  !< Sedimentmasse im liegenden Sedimentvolumen [kg/m3]
   real                :: dRero_s   !< Zwischengröße
   logical, intent(in) :: kontroll  !< debugging
   integer, intent(in) :: ior,mstr  !< debugging
   
   ! local variables
   real                :: UST       !< Sohlschubspannungsgeschwindigkeit [m/s]
   
   real, parameter     :: roh2o = 1000      !< Wasserdichte [kg/m³]
   real, parameter     :: g = 9.81          !< Erdbeschleunigung [m/s²] 

   ! --------------------------------------------------------------------------
   
   ! Berechnung der Sohlschubspannung
   call bottom_friction_strickler(tau,ust,rau_s,tiefe_s,vmitt_s)
   !ust = (((g/rau_s)**0.5) / (tiefe_s**0.166667)) * abs(vmitt_s)
   !tau_s = (ust**2) * roh2o
      ! 2 316  ! 979-663   ! Elbe-Km 474,5
      !if((mstr==2).and.(ior==316))print*,'erosion_kern Elbe-Km 474,5 htau2,tau=', tau_s,tau
      ! 2 512  ! 1175-663  ! Elbe-Km 585,05
      !if((mstr==2).and.(ior==512))print*,'erosion_kern Elbe-Km 585,05 htau2,tau=', tau_s,tau
   tau_s = tau
   
   if (tau_s > tausc_s .and. tausc_s > 0.0 .and. sedroh_s > 0.0 .and. tiefe_s > 0.0) then
      SSeros_s = m_eros_s*((tau_s-tausc_s)/tausc_s)**n_eros_s ! Erosionsmassenstrom [kg/(m²*s)]
      dRero_s = 1000.0 * SSeros_s * tflie * 86400.            ! erodierte Masse je m² und Zeitschritt [g/m²]
      dsedh_s = dRero_s/sedroh_s                              ! Sohlhöhenänderung im aktuellen Zeitschritt [mm]
      dRero_s = dRero_s/tiefe_s                               ! Schwebstoffkonzentrationszunahme je Zeitschritt [mg/l]
      ss_s = ss_s + dRero_s                                   ! aktualisierte Schwebstoffkonzentration [mg/l]
      ssalg_s = ssalg_s + dRero_s
   else
      dsedh_s = 0.0
      SSeros_s = 0.0
   endif
   
!   if ((kontroll).or.(ior==5)) then 
!      print*,ior,mstr,' erosion_kern SS,SSalg,SSeros,dsedH = ',ss_s,ssalg_s,SSeros_s,dsedh_s
!      print*, 'tau_s,ust,vmitt_s, tflie = ',tau_s,ust,vmitt_s, tflie
!      print*, 'rau_s,tausc_s,m_eros_s,n_eros_s = ',rau_s,tausc_s,m_eros_s,n_eros_s
!   endif
end subroutine erosion_kern

!> bottom friction and friction velocity (Strickler-Formula, kst=1/n  n=Mannings-n)
subroutine bottom_friction_strickler(tau,ust,kst,tiefe,vmitt)
   implicit none
   real                :: tau    !< bottom friction [N/m²]
   real                :: ust    !< friction velocity[m/s]
   real                :: kst    !< Strickler coefficient [(m^1/3)/s]
   real                :: tiefe  !< depth (hydraulic radius) [m]
   real                :: vmitt  !< median flow velocity [m/s]
   
   real, parameter     :: roh = 1000 !< water density [kg/m³]
   real, parameter     :: g = 9.81   !< gravity [m/s²] 
   
   ust=0.0
   if((kst >= 0.0).and.(tiefe >= 0.0))then
      ust = (abs(vmitt)*(g**0.5)) / (kst*(tiefe**(1/6)))
   endif
   tau=(ust**2.0)*roh
end subroutine bottom_friction_strickler

