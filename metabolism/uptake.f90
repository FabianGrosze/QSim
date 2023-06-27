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
subroutine uptake(yk, xk, qmxi, qmni, cnaehr, halbi, upmxi, & 
                  tflie, up_ci, up_n2i, jcyano, ifix)
   
   implicit none
   
   real,    intent(inout) :: yk 
   real,    intent(inout) :: xk
   real,    intent(in)    :: qmxi
   real,    intent(in)    :: qmni
   real,    intent(in)    :: cnaehr
   real,    intent(in)    :: halbi
   real,    intent(in)    :: upmxi
   real,    intent(in)    :: tflie
   real,    intent(out)   :: up_ci
   real,    intent(out)   :: up_n2i
   integer, intent(in)    :: jcyano
   integer, intent(in)    :: ifix
   
   integer :: m, ms, j_intern
   real    :: ykaus, sup_c, sumup_n2, sumup_c, dyk, deltat
   
   ! Wahl der Funktion für die Abhängigkeit der Aufnahmerate von der zellinternen Konzentration
   j_intern = 1
   ms = 10
   up_n2i = 0.0
   up_ci = 0.0
   sumup_c = 0.0
   sumup_n2 = 0.0
   deltat = tflie/ms
   ! growth rate µ
   xk = max(0.0, xk * deltat) 
   
   do m = 1, ms
      select case(j_intern)
         case(0)
            up_ci = ((qmxi-yk)/(qmxi-qmni))*upmxi*(cnaehr/(cnaehr+halbi))
            if (jcyano == 1 .and. ifix == 1)up_n2i = ((qmxi-yk)/(qmxi-qmni))*upmxi*(halbi/(cnaehr+halbi))
      
         case(1)
            up_ci = upmxi*((1.-yk/qmxi)/(1.-yk/qmxi+0.01))*(cnaehr/(cnaehr+halbi))
            if (jcyano == 1 .and. ifix == 1)up_n2i = upmxi*((1.-yk/qmxi)/(1.-yk/qmxi+0.01))*(halbi/(cnaehr+halbi))
      
      end select
      
      up_ci = up_ci*deltat
      up_n2i = up_n2i*deltat
      sup_c = up_ci + up_n2i
      ykaus = yk
      yk = yk + sup_c - xk * yk
      if (yk > qmxi) then
         dyk = yk - qmxi
         sup_c = sup_c - dyk
         
         if (up_ci + up_n2i == 0.0) then
            up_ci = 0.0
            up_n2i = 0.0
         else
            up_ci = sup_c * (up_ci/(up_ci + up_n2i))
            up_n2i = sup_c * (up_n2i/(up_ci + up_n2i))
         endif
         yk = qmxi
      endif
      sumup_c = sumup_c + up_ci
      sumup_n2 = sumup_n2 + up_n2i
   enddo
   up_ci = sumup_c
   up_n2i = sumup_n2
end subroutine uptake
