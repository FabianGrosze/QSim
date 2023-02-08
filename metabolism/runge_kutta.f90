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

!> Numerische Lösung gewöhnlicher Differenzial-Gleichungen mit dem Runge-Kutta-Verfahren
!! @author Volker Kirchesch
!! @date 12.08.2007
subroutine runge_kutta(yk,xk,h,n,Qmxi,Qmni,CNaehr,Halbi,upmxi,tflie,up_Ci,    &
                       up_N2i,abr,jcyano,ifix)

   implicit none
   
   integer :: n, j_intern, jcyano, ifix
   real    :: k1,k2,k3,k4
   real    :: yk, yk0, xk, up_n2i, up_ci
   real    :: upmxi, tflie, qmxi, qmni, h
   real    :: halbi, cnaehr, abr
   
   j_intern = 1
   up_N2i = 0.0
   up_Ci = 0.0
   xk = xk*tflie
   if (j_intern == 0) then
      up_Ci = ((Qmxi-yk)/(Qmxi-Qmni))*upmxi*(CNaehr/(CNaehr+Halbi))
      if (jcyano == 1 .and. ifix == 1)up_N2i = ((Qmxi-yk)/(Qmxi-Qmni))*upmxi*(Halbi/(CNaehr+Halbi))
   else if (j_intern == 1) then
      up_Ci = upmxi*((1.-yk/Qmxi)/(1.-yk/Qmxi+0.01))*(CNaehr/(CNaehr+Halbi))
      if (jcyano == 1 .and. ifix == 1)up_N2i = upmxi*((1.-yk/Qmxi)/(1.-yk/Qmxi+0.01))*(Halbi/(CNaehr+Halbi))
   endif
   
   up_Ci = up_Ci*tflie
   up_N2i = up_N2i*tflie
   
   yk0 = yk*exp(-xk)
   yk = yk0 + up_Ci + up_N2i
   if (yk > Qmxi) then
      up_Ci = (Qmxi - yk0)*(up_Ci/(up_Ci + up_N2i))
      up_N2i = (Qmxi - yk0)*(up_N2i/(up_Ci + up_N2i))
      yk = Qmxi
   endif
   
end subroutine runge_kutta
