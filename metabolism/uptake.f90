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
!> Berechnung der Nährstoffaufnahme durch Algen, sowie des zellinternen Nährstoffgehalts
!!
!! Funktion: dQ/dt = upX - µ*Q
!! @author Volker Kirchesch
!! @date 27.11.2015
subroutine uptake(yk,xk,Qmxi,Qmni,CNaehr,Halbi,upmxi,tflie,up_Ci,up_N2i,abr,jcyano,ifix,j_aus)
   
   ! Wahl der Funktion für die Abhängigkeit der Aufnahmerate von der zellinternen Konzentration
   j_intern = 1
   ms = 10
   up_N2i = 0.0
   up_Ci = 0.0
   sumup_C = 0.0
   sumup_N2 = 0.0
   deltat = tflie/ms
   ! Wachstumsrate µ
   xk = max(0.0,xk*deltat) 
   
   do m = 1, ms
      if (j_intern == 0) then
         up_Ci = ((Qmxi-yk)/(Qmxi-Qmni))*upmxi*(CNaehr/(CNaehr+Halbi))
         if (jcyano == 1 .and. ifix == 1)up_N2i = ((Qmxi-yk)/(Qmxi-Qmni))*upmxi*(Halbi/(CNaehr+Halbi))
      else if (j_intern == 1) then
         up_Ci = upmxi*((1.-yk/Qmxi)/(1.-yk/Qmxi+0.01))*(CNaehr/(CNaehr+Halbi))
         if (jcyano == 1 .and. ifix == 1)up_N2i = upmxi*((1.-yk/Qmxi)/(1.-yk/Qmxi+0.01))*(Halbi/(CNaehr+Halbi))
      endif
      up_Ci = up_Ci*deltat
      up_N2i = up_N2i*deltat
      sup_C = up_Ci + up_N2i
      ykaus = yk
      yk = yk + (sup_C - xk*yk)
      if (yk > Qmxi) then
         dyk = yk - Qmxi
         sup_C = sup_C - dyk
         if ((up_Ci + up_N2i) == 0.0) then
            up_Ci = 0.0
            up_N2i = 0.0
         else
            up_Ci = sup_C * (up_Ci/(up_Ci + up_N2i))
            up_N2i = sup_C * (up_N2i/(up_Ci + up_N2i))
         endif
         yk = Qmxi
      endif
      sumup_C = sumup_C + up_Ci
      sumup_N2 = sumup_N2 + up_N2i
   enddo
   up_Ci = sumup_C
   up_N2i = sumup_N2
end subroutine uptake
