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
subroutine schiff(vmitt1, tiefe1, ischif, v6)

   implicit none
   
   ! --- dummy arguments ---
   real,    intent(in)  :: vmitt1
   real,    intent(in)  :: tiefe1
   integer, intent(in)  :: ischif
   real,    intent(out) :: v6
   
   ! --- local variables ---
   integer :: it
   real    :: vs, vschiff, v3, v4, v5, uprop, hp, h2
   
   real, parameter :: dprop = 1.55
   real, parameter :: kt = 0.35
   real, parameter :: w = 0.6
   real, parameter :: e = 0.25
      
   if (ischif == 2) then
      vschiff = 1.5
      uprop = 1.2
   else
      vschiff = 3.2
      uprop = 3.33
   endif
   
   v3 = vschiff - vmitt1
   if (v3 < 0.0) v3 = 0.0
   v4 = v3 * (1. - w)

   v5 = 2.
   do it = 1,1000
      vs = v5
      v5 = 1.6 * uprop * dprop * sqrt(kt) * (1. + 2. * (v4/vs))**(-0.5)
      if (abs(vs-v5) <= 0.0001) exit
   enddo
   
   h2 = tiefe1 - 3.0
   h2 = min(h2, 0.2)
   hp = h2 + dprop/2.
   v6 = e*(hp/dprop)**(-1) * v5 * (1. - (v3/(uprop*dprop)))
   
   return
end subroutine schiff
