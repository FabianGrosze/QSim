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

!> Lösung der Diff-Gleichung zur Beschreibung der Lichthemmung  mit dem 
!! Runge-Kutta-Verfahren 4. Ordnung
!!
!! Gleichung
!! dH/dt = Kd*I*H+Tau*(1-H)
!! Kd in m2/µE

subroutine lichthemmung(dt, ic, yk, cchl_stern, chlacz)

   implicit none
   ! --- dummy arguments ---
   real, intent(in)    :: dt
   real, intent(in)    :: ic
   real, intent(inout) :: yk
   real, intent(in)    :: cchl_stern
   real, intent(in)    :: chlacz
   
   ! --- local variables ---
   integer :: it
   real    :: tau, psii, h, fclose, chl_c
   real    :: k1, k2, k3, k4, kd
   
   real, parameter :: psii0 = 1.5    ! Absortionsfläche dunkeladaptierter Algen in [m2/E]
   real, parameter :: kd0 = 1.04e-8  ! Fitingwert, Messungen nach Anning (2000)
   real, parameter :: taui0 = 4.5e-5 ! Parameter zur Beschreibung der Erholung (Ross)
   
   
   ! ki0 Zuwachsrate der Hemmung
   if (chlacz > 0.0) then
      chl_c = 1./chlacz
      psii = psii0 * (chl_c * cchl_stern)**0.22
      psii = min(psii0, psii)
   else
      psii = psii0
   endif

   ! TODO: psii kürzt sich hier aus der Gleichung. Warum wird es dann überhaupt berechnet?
   fclose = psii0/psii
   kd = psii * kd0 * fclose * dt * 86400.
   tau = taui0 * dt * 86400.
   
   h = 0.5
   do it = 1,2
      k1 = -kd*ic*yk+tau*(1.-yk)
      k2 = (yk+0.5*h*k1)*(-kd*ic)+tau*(1.-yk+0.5*h*k1)
      k3 = (yk+0.5*h*k2)*(-kd*ic)+tau*(1.-yk+0.5*h*k2)
      k4 = (yk+h*k3)*(-kd*ic)+tau*(1.-yk+h*k3)
      yk = yk+((h/6.)*(k1+2.*k2+2.*k3+k4))
   enddo
end subroutine lichthemmung


