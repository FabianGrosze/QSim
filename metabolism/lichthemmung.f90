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

subroutine lichthemmung(dt,Ic,yK,CChl_Stern,ChlaCz,j)
   
   real                  :: KI, k1, k2, k3, k4
   real                  :: Ic,Kd0,Kd
   real, dimension(50)   :: ChlaCz
   
   real, parameter         :: PSII0 = 1.5  ! Absortionsfläche dunkeladaptierter Algen in m2/E
   
   ! KI0 Zuwachsrate der Hemmung
   ChlC_Stern = 1./CChl_Stern
   if (ChlaCz(j) > 0.0) then
      Chl_C = 1./ChlaCz(j)
      PSII = min(PSII0,PSII0 * (Chl_C/ChlC_Stern)**0.22)
   else
      PSII = PSII0
   endif
   fclose = PSII0/PSII
   ! Fiting-Wert, Messungen nach Anning (2000)
   Kd0 = 1.04e-8
   Kd = PSII*kd0*fclose*dt*86400.
   
   ! Parameter zur Beschreibung der Erholung (Ross)
   TauI0 = 4.5e-5
   Tau = TauI0*dt*86400.
   
   h = 0.5
   do it = 1,2
      k1 = -Kd*Ic*yk+Tau*(1.-yk)
      k2 = (yk+0.5*h*k1)*(-Kd*Ic)+Tau*(1.-yk+0.5*h*k1)
      k3 = (yk+0.5*h*k2)*(-Kd*Ic)+Tau*(1.-yk+0.5*h*k2)
      k4 = (yk+h*k3)*(-Kd*Ic)+Tau*(1.-yk+h*k3)
      yk = yk+((h/6.)*(k1+2.*k2+2.*k3+k4))
   enddo
end subroutine lichthemmung

