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

subroutine lax_wen(ior, nkzs, u, xws, hcdt, dh2d)
   implicit none
   
   ! --- dummy arguments ---
   integer, intent(in)                      :: ior
   integer, intent(in),    dimension (1000) :: nkzs
   real,    intent(inout), dimension(50)    :: u
   real,    intent(in)                      :: xws
   real,    intent(in)                      :: hcdt
   real,    intent(in)                      :: dh2d
   
   ! --- local variables ---
   real, dimension(50) :: u_new
   integer             :: nkz
   save u_new
   
   do nkz = 1, nkzs(ior)
      if (nkz == 1) then
         u_new(nkz) = u(nkz)-(hcdt*xws/(2.*dh2d))*(u(nkz+1)-u(nkz))                  &
                      +(hcdt**2*xws**2/(2.*dh2d**2))*(u(nkz)-2.*u(nkz)+u(nkz+1))
      endif
      
      if (nkz == nkzs(ior)) then
         u_new(nkz) = u(nkz)-(hcdt*xws/(2.*dh2d))*(u(nkz)-u(nkz-1))                  &
                      +(hcdt**2*xws**2/(2.*dh2d**2))*(u(nkz-1)-2.*u(nkz)+u(nkz))
      endif
      
      if (nkz > 1 .and. nkz < nkzs(ior)) then
         u_new(nkz) = u(nkz)-(hcdt*xws/(2.*dh2d))*(u(nkz+1)-u(nkz-1))                  &
                      +(hcdt**2*xws**2/(2.*dh2d**2))*(u(nkz-1)-2.*u(nkz)+u(nkz+1))
      endif
   enddo
   
   u(1:nkzs(ior)) = u_new(1:nkzs(ior))

end subroutine lax_wen