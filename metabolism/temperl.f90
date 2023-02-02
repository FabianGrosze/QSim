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
!> Berechnung der Lufttemperatur für eine gegebene Uhrzeit aus der Tagesmaximums-
!! und minimumstemperatur
!!
!! Literatur: W.J. Parton and J.A. Logan: A Model for Diurnal variation in Soil and Air
!!            Temperature. - Agricultural Meteorology, 23, S.205-216 (1981)
subroutine temperl(sa, su, uhrz, templ, mstr, idwe, tlmax, tlmin, anze, imet)

   use allodim
   implicit none
   
   ! --- dummy arguments ---
   real,    intent(in)                            :: sa     !< sunrise
   real,    intent(in)                            :: su     !< sunset
   real,    intent(in)                            :: uhrz   !< current time of simulation
   real,    intent(out), dimension(ialloc2)       :: templ  !< air temperature at given time
   integer, intent(in)                            :: mstr   !< current stretch
   integer, intent(in), dimension(azStrs,ialloc2) :: IDWe   !<  
   real,    intent(in), dimension(20)             :: tlMax  !< daily maximum of air temperature
   real,    intent(in), dimension(20)             :: tlMin  !< daily minimum of air temperature
   integer, intent(in)                            :: anze   !< number of segments in current stretch
   integer, intent(in)                            :: imet   !<
 
 
   ! --- local variables ---
   real, parameter :: a = 2.0 ! zeiliche Verschiebung des Temperaturmaximums [h]
   real, parameter :: b = 3.0 ! Koeffizient, der die Temperaturabnahme während der Nacht beschreibt [-]
   
   real :: a_day, a_night, t_max, t_min, bbd, t_sn
   integer :: ior
   
   
   do ior = 1,anze+1
      ! Werte der Lufttemperatur liegen im Berechnungszeittakt vor
      if (imet == 1) then 
         templ(ior) = tlmax(idwe(mstr,ior))
      
      else
         t_max = tlmax(idwe(mstr,ior))
         t_min = tlmin(idwe(mstr,ior))
         
         ! duration of brigthness [h]
         a_day = su - sa
         ! duration of darkness [h]
         a_night = 24. - a_day
         
         if (uhrz <= su .and. uhrz>=sa) then ! Festlegung ob Tag oder Nacht
            ! time since sunrise
            bbd = uhrz-sa
            templ(ior) = (t_max-t_min)*sin((3.14*bbd)/(a_day + 2.*a)) +t_min
         else
            ! time since sunset
            if (uhrz > sa) then
               bbd = uhrz-su
            else
               bbd = 24.-su+uhrz
            endif
            t_sn = (t_max - t_min) * sin((3.14*a_day) / (a_day + 2.*a)) + t_min
            templ(ior) = t_min + (t_sn-t_min)*exp(-b*bbd/a_night)
         endif
      endif
   enddo

end subroutine temperl
