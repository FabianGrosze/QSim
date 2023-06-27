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
subroutine sed_pom(tiefe1, ust, n, sedom_s, dkorn_s, sedomb_s,  &
                   dkornb_s, fsch, jsed, w2_s, w2b_s,           &
                   control, jjj)
   
   use module_alloc_dimensions
   implicit none
   
   ! --- dummy arguments ---
   real,    intent(in)    :: tiefe1
   real,    intent(in)    :: ust
   integer, intent(in)    :: n
   real,    intent(inout) :: sedom_s
   real,    intent(inout) :: dkorn_s
   real,    intent(inout) :: sedomb_s
   real,    intent(inout) :: dkornb_s
   real,    intent(in)    :: fsch
   integer, intent(in)    :: jsed
   real,    intent(inout) :: w2_s
   real,    intent(inout) :: w2b_s
   logical, intent(in)    :: control  !< debugging
   integer, intent(in)    :: jjj       !< debugging
   
   ! --- local variables ---
   integer          :: ised, ior
   real             :: zellv, xtflie, xsedom, wst
   real             :: w2z, sedomz, sdflus
   real             :: sdflub, sdflua, qsgr, oc
   real             :: oc0,  dkornz
   real             :: dichte, ceq
   double precision :: sedoc
   
   real, parameter :: bsbc = 5.
   real, parameter :: phytoc = 3.4
   real, parameter :: gesss = 55.
   real, parameter :: dichta = 2.6
   real, parameter :: dichto = 1.2
   
   external :: sedimentation
   
   do ised = 1,3
      
      xtflie = 1.
      ior = 1
      zellv = 500.
      call sedimentation(tiefe1, ised, ust, qsgr, oc, oc0,      &
                         xtflie, wst, jsed, zellv, control, jjj)
      ceq = qsgr
      
      if (ised == 1) then
         sdflua = (1. - ceq) * oc
      else if (ised == 2) then
         sdflub = (1. - ceq) * oc
      else
         sdflus = (1. - ceq) * oc
      endif
   enddo
   
   if (n == 2) then
      sedomz = sedom_s
      dkornz = dkorn_s
      w2z = w2_s
   endif
   
   sedoc = (bsbc*sdflub+phytoc*sdflua)/(gesss*max(1.e-10,sdflus))
   
   if (sedom_s > 0.0 .and. n == 1) then
      sedom_s = sedom_s/100.
   else if (sedomb_s > 0.0 .and. n == 2) then
      sedom_s = sedomb_s/100.
   else
      sedom_s = sedoc
      xsedom = sedom_s
      sedom_s = sedom_s * fsch
      if (sedom_s < 0.001) sedom_s = 0.005
   endif
   
   
   dichte = dichta * (1.-sedom_s) + sedom_s * dichto
   w2_s = 0.74e-5
   
   ! berechnung des mittleren korndurchmessers [m]
   dkorn_s = min(100.,0.0047*exp(64.89*ust))
   dkorn_s = dkorn_s / 1000.
   
   if (n == 2) then
      sedomb_s = sedom_s
      dkornb_s = dkorn_s
      w2b_s = w2_s
      sedom_s = sedomz
      dkorn_s = dkornz
      w2_s = w2z
   endif
   
   return
end subroutine sed_pom
