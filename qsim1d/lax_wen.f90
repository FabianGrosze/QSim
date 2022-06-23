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

subroutine lax_wen(ior,nkzs,U,xws,hcdt,dH2D)
   integer, dimension (1000)  :: nkzs
   real, dimension(50)        :: U, U_neu
   save U_neu
   
   do nkz = 1, nkzs(ior)
      if (nkz == 1) then
         U_neu(nkz) = U(nkz)-(hcdt*xws/(2.*dH2D))*(U(nkz+1)-U(nkz))                  &
                      +(hcdt**2*xws**2/(2.*dH2D**2))*(U(nkz)-2.*U(nkz)+U(nkz+1))
      endif
      if (nkz == nkzs(ior)) then
         U_neu(nkz) = U(nkz)-(hcdt*xws/(2.*dH2D))*(U(nkz)-U(nkz-1))                  &
                      +(hcdt**2*xws**2/(2.*dH2D**2))*(U(nkz-1)-2.*U(nkz)+U(nkz))
      endif
      
      if (nkz > 1 .and. nkz < nkzs(ior)) then
         U_neu(nkz) = U(nkz)-(hcdt*xws/(2.*dH2D))*(U(nkz+1)-U(nkz-1))                  &
                      +(hcdt**2*xws**2/(2.*dH2D**2))*(U(nkz-1)-2.*U(nkz)+U(nkz+1))
      endif
   enddo
   U(1:nkzs(ior)) = U_neu(1:nkzs(ior))
   
end subroutine lax_wen