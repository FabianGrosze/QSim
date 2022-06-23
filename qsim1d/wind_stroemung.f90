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

!> Berechnung des Windeinflusses auf die Strömungsgeschwindigkeit
!!
!! @author Volker Kirchesch
!! @date 14.02.2007
subroutine wind_stroemung(tiefe,nkzs,ior,dH2D,hconus,uwind)
   
   integer,dimension(1000) :: nkzs
   real,dimension(50)      :: uwind
   real, dimension(1000)   :: tiefe
   
   zwS = tiefe(ior)
   
   gamW = 0.35
   zsh = 2.2e-4
   zbh = 6.e-5
   zb = zbh*tiefe(ior)
   zs = zsh*tiefe(ior)
   
   p1 = gamW*zsh
   p2 = gamW*zsh/zbh
   q1 = ((1.+zsh)*log(1.+(1./zsh)))-1.
   q2 = (zsh*log(1.+(1./zbh)))-1.
   
   A = q2/((p1*q2)-(q1*p2))
   B = -q1/((p1*q2)-(q1*p2))
   C = 0.0
   
   do nkz = 1,nkzs(ior)
      uwind(nkz) = A*hconus*(-1.)*log(1+(zwS/zs))+B*hconus*(-1.)*log(1.-(zwS/(zb+tiefe(ior))))
      zWs = ZWs-dH2D
      if (zWs < 0.0)zWs = 0.0
   enddo
   
   return
end subroutine wind_stroemung
