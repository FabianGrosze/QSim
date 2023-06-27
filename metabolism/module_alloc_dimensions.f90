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

!> array dimenions
!!
!! Felddimensionierungs-Parameter
module module_alloc_dimensions

   implicit none
     
   public

   integer , protected :: azstrs         !< \anchor azstrs Stranganzahl
   integer , parameter :: ialloc1 = 100  !> \anchor ialloc1 Einleiter pro Strang
   integer , parameter :: ialloc2 = 1000 !> \anchor ialloc2 Querprofile im Strang
   integer , parameter :: ialloc3 = 20   !> \anchor ialloc3 Abschnitte im Strang?
   integer , parameter :: ialloc4 = 250  !> \anchor ialloc4 maximal Anzahl Ausgabegrößen
   integer , parameter :: ialloc5 = 50   !> \anchor ialloc5 Tiefenschichtenanzahl
   
contains

   subroutine set_azstrs(azstrs_in)
      integer, intent(in) :: azstrs_in
      azstrs = azstrs_in
   end subroutine set_azstrs
   
end module module_alloc_dimensions