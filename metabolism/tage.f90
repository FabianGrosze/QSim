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

!> Berechnung der vergangenen Tage seit dem 21. März
!! @author Volker Kirchesch
!! @date 21.08.1984
subroutine tage(tag, monat, anzt)
   implicit none
   
   integer, intent(in)  :: tag   !< Tag des Monats
   integer, intent(in)  :: monat !< Monat
   integer, intent(out) :: anzt  !< Tage seit 21. März
   
   ! TODO (schoenung, june 2022)
   ! This calculation does not work for leap years
   if (monat <= 2) then
      anzt = tag + 31 * (monat-1) + 285
   else if (monat == 3 .and. tag <= 21) then
      anzt = tag + 31 * (monat-1) - int(0.4*monat + 2.3) + 285
   else
      anzt = tag + 31 * (monat-1) - int(0.4*monat+2.3) - 80
   endif
   return
end subroutine tage
