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

subroutine einleiter_misch(ns,i,C,Cpart,hcQ,hcQE,CE,rohE,D,dH2D)

   implicit none
   integer, intent(in),  dimension(1000)    :: ns
   integer, intent(in)                      :: i
   real,    intent(in),  dimension(50,1000) :: C
   real,    intent(out), dimension(50)      :: cpart
   real,    intent(in)                      :: hcq
   real,    intent(in)                      :: hcqe
   real,    intent(in)                      :: ce
   real,    intent(in)                      :: rohe
   real,    intent(in), dimension(50)       :: d
   real,    intent(in)                      :: dh2d
   
   
   integer             :: n, j, jneu
   real                :: qp, qep, Masseg, Massep
   real, dimension(50) :: Cg
   
   Masseg = 0.0
   Massep = 0.0
   j = 0
   do n = 1,ns(i)
      j = j+1
      if (rohE < D(n)) exit    ! D wird von der Sohle aus berechnet D1 -> Sohle, Dns -> Oberfläche
   enddo
   
   ! Anzahl der Schichten von der Oberfläche aus
   jneu = (ns(i)-j)+1 
   j = jneu
   if (j == (ns(i)-1))j = ns(i)
   Qp = hcQ/ns(i)
   QEp = hcQE/j
   
   ! gleichverteilte Einleitung ueber die Vertikale
   do n = 1,ns(i)
      Cg(n) = (hcQ*C(n,i)+hcQE*CE)/(hcQ+hcQE)
   enddo
   
   ! Einleitung wird nur in den Teil der Vertikalen eingemischt fuer den Gilt DEin<=DVorfluter
   do n = 1,j
      Cpart(n) = (Qp*C(n,i)+QEp*CE)/(Qp+QEp)  
   enddo
   
   ! für die übrigen vertikalen Schichten erfolgt keine Temperaturänderung
   do n = j+1,ns(i)
      Cpart(n) = C(n,i)
   enddo
   
end subroutine einleiter_misch
