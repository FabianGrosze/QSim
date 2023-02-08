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

!> Bestimmug des Reflektionsanteils der direkten Sonnenstrahlung an der Gewässeroberfläche
!! @author Volker Kirchesch
!! @date 22.09.2010
subroutine albedo(SH,REFL)
   ! TODO: Literaturangabe
   implicit none
   
   real, dimension(15) :: alb
   real                :: sh, shgr, refl, pi, dshgr, dalb, albint
   integer             :: n
   
   pi = 22./7.
   alb(1)  = 100.0
   alb(2)  = 70.5
   alb(3)  = 46.0
   alb(4)  = 32.5
   alb(5)  = 25.0
   alb(6)  = 20.0
   alb(7)  = 15.4
   alb(8)  = 12.0
   alb(9)  = 9.5
   alb(10) = 8.5
   alb(11) = 7.5
   alb(12) = 7.0
   alb(13) = 6.5
   
   shgr = sh*180/pi
   if (shgr > 60.0) then
      refl = 6.5
   else
      n = int(shgr/5)+1
      dalb = alb(n)-alb(n+1)
      dshgr = shgr-(n-1)*5
      albint = dalb/5*dshgr
      refl = alb(n)-albint
   endif
   refl = refl/100
   refl = 1-refl
   return
end subroutine albedo
