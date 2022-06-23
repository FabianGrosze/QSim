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
   ! Literaturangabe !!
   
   real, dimension(15) :: ALB
   
   PI = 22./7.
   ALB(1) = 100.0
   ALB(2) = 70.5
   ALB(3) = 46.0
   ALB(4) = 32.5
   ALB(5) = 25.0
   ALB(6) = 20.0
   ALB(7) = 15.4
   ALB(8) = 12.0
   ALB(9) = 9.5
   ALB(10) = 8.5
   ALB(11) = 7.5
   ALB(12) = 7.0
   ALB(13) = 6.5
   
   SHGR = SH*180/PI
   if (SHGR > 60.0) then
      REFL = 6.5
   else
      N = INT(SHGR/5)+1
      DALB = ALB(N)-ALB(N+1)
      DSHGR = SHGR-(N-1)*5
      ALBINT = DALB/5*DSHGR
      REFL = ALB(N)-ALBINT
   endif
   REFL = REFL/100
   REFL = 1-REFL
   return
end subroutine albedo
