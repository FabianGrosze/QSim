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

!> Sunset and Sunrise
!! @author Volker Kirchesch
!! @date 05.11.1987
subroutine sasu(itags, monats, geob, geol, sa, su, zg, zlk, dk, tdj)
   use aparam
   implicit none
   
   ! --- dummy arguments ---
   integer, intent(in)  :: itags   !< current day of simulation
   integer, intent(in)  :: monats  !< current month of simulation
   real,    intent(in)  :: geob    !< latitude
   real,    intent(in)  :: geol    !< longitude
   real,    intent(out) :: sa      !< sunrise
   real,    intent(out) :: su      !< sunset
   real,    intent(out) :: zg      !<
   real,    intent(out) :: zlk     !<
   real,    intent(out) :: dk      !<
   integer, intent(out) :: tdj     !<
   
   ! --- local variables ---
   integer :: anzt, ineg
   real    :: ph, fk, t0, t1, d0, geobn
      
   external :: qerror, tage
   
   if (geoB < -90.0  .or. geoB > 90.0 .or. &
       geoL < -180.0 .or. geoL > 180.0) then
      print "(a,f0.2)", "latitude  = ", geob
      print "(a,f0.2)", "longidude = ", geol
      call qerror("The given longitude or latitude is wrong.")
   endif
   
   ! Berchnung der vergangenen Tage seit dem 21. März
   call tage(itags, monats, anzt)
   
   ineg = 0
   tdj = anzt + 80
   ph = 0.9876 * anzt * pi/180.
   zg = -7.683 * sin(ph + 1.3788) + 9.867 * sin(2. * ph)
   fk = 1.921 * sin(ph + 1.3788) - 1.8855
   ph = (ph*180./pi)+fk
   dk = 0.39875*sin(ph*pi/180.)
   dk = atan(dk/sqrt(1-dk**2))
   geobn = geob*pi/180.
   t0 = (-1.)*tan(geobn)*tan(dk)
   if (t0 < 0.0) then
      t0 = t0*(-1.)
      ineg = 1
   endif
   t0 = atan(sqrt(1.-t0**2)/t0)
   if (ineg > 0) then
      t1 = pi-t0
      t0 = t1
   endif
   d0 = 0.833/(cos(geobn)*cos(dk)*sin(t0))
   t0 = ((t0*180./pi)+d0)*4.
   zlk = (15.-geol)*4.
   sa = (720. - t0 - zg + zlk) / 60.
   su = (720. + t0 - zg + zlk) / 60.
   
   return
end subroutine sasu
