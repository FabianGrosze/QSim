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
subroutine dichte(tempwz, nkzs, ior, D)
   use allodim
   implicit none
   real,    intent(in),  dimension(ialloc5,ialloc2) :: tempwz
   integer, intent(in),  dimension(ialloc2)         :: nkzs
   integer, intent(in)                              :: ior
   real,    intent(out), dimension(ialloc5)         :: D
   
   integer         :: j, nkz
   
   real, parameter :: a0 =  999.842594
   real, parameter :: a1 =  6.793952e-2
   real, parameter :: a2 = -9.095290e-3
   real, parameter :: a3 =  1.001685e-4
   real, parameter :: a4 = -1.120083e-6
   real, parameter :: a5 =  6.536332e-9
   
   
   ! TODO (Schoenung, august 2022): This is wrong. This way the deepest layers use
   ! the temperature form the highest layers and vice versa. They must use their 
   ! corresponding layers.
   j = nkzs(ior)
   do nkz = 1,nkzs(ior)
      d(nkz) = a0                      &
             + a1 * tempwz(j,ior)      &
             + a2 * tempwz(j,ior)**2   &
             + a3 * tempwz(j,ior)**3   &
             + a4 * tempwz(j,ior)**4   &
             + a5 * tempwz(j,ior)**5
      j = j-1
   enddo
end subroutine dichte
