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

subroutine coroph_huelle(i)
   use modell
   use QSimDatenfelder
   implicit none
   integer :: i,j
   iglob = (i+meinrang*part)
   
   ! This module is currently turned off
   
   ! if (kontroll) then
   !   print*,'coroph vorher: coro,coros = ',coro,coros
   ! end if 
   
   ! call coroph(coro,coros,tempw,flae,elen,anze,ior                             &
   !             ,volfco,aki,agr,algcok,algcog,tflie,bsohlm,lboem,coroI,coroIs   &
   !             ,abl,algcob,mstr,itags,monats,jahrs,ilang,nbuhn,ilbuhn          &
   !             ,kontroll ,iglob ) !!wy
   return
end subroutine coroph_huelle
