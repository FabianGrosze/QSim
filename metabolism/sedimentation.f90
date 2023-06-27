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
subroutine sedimentation(tiefe, ised, ust, qsgr, oc, oc0, xtflie, &
                         wst, jsed, zellv, control, jjj)
   implicit none
   
   real,    intent(in)  :: tiefe
   integer, intent(in)  :: ised
   real,    intent(in)  :: ust
   real,    intent(out) :: qsgr
   real,    intent(out) :: oc
   real,    intent(out) :: oc0
   real,    intent(in)  :: xtflie
   real,    intent(out) :: wst
   integer, intent(in)  :: jsed
   real,    intent(in)  :: zellv
   logical, intent(in)  :: control  !< debugging
   integer, intent(in)  :: jjj       !< debugging
   
   real           :: ased, bsed, prop, wsalg, wsgr, fwst, qssed, ws0, ws
   character(100) :: message
   
   external :: qerror
   
   if (ised < 1 .or. ised > 4) then
      write(message, '("sedimentation.f90: invalid sedimentation option: ",i2)') ised
      call qerror(message)
   
   elseif (ised == 1 .and. jsed /= 0 .and. zellv <= 0.) then
      call qerror('sedimentation.f90: zellv = 0.')
   endif
   
   if (jsed == 0) then
      select case(ised)
         case(1) ! algen
            ased = 6.67e-7
            bsed = 2.78
            prop = 0.53
         
         case(2) ! bsb
            ased = 1.44e-6
            bsed = 3.13
            prop = 0.6
         
         case(3) ! gesamt-ss
            ased = 1.74e-4
            bsed = 1.63
            prop = 1.36
            
         case(4) ! nitrifikanten
            ased = 1.91e-7
            bsed = 3.00
            prop = 0.56
      end select
   
   else
      select case(ised)
         case(1) ! algen
            bsed  = 2.7
            prop  = 0.5
            wsalg = 2.0155 * log10(zellv) - 11.512
            wsalg = 10**wsalg
            ased  = 1. / exp(-bsed * log10(wsalg))
      
         case(2) ! bsb
            ased = 2.43e-7
            bsed = 2.5
            prop = 0.7
         
         case(3) ! gesamt-ss
            ased = 1.55e-7
            bsed = 2.8
            prop = 0.75
         
         case(4) ! nitrifikanten
            ased = 2.43e-7
            bsed = 2.5
            prop = 0.7
      end select
   endif
   
   wsgr = 0.14 * ust**2 + 0.0054 * ust + 1.25e-6
   qsgr = 1. / (1.+ ased * exp(-bsed * log10(wsgr)))
   qssed = 0.5 * (1. + qsgr)
   
   if (qssed < 1. .and. qssed > 0.) then
      ws = (log(ased) - log(1. / qssed - 1.)) / bsed
   else
      ws = 0.
   endif
   ws  = 10**ws
   
   fwst = min(1., exp(-604.2 * ust))
   if (ised == 3) fwst = max(1.91e-19, fwst)
   
   wst = ws * fwst
   oc  = 1. - 1. / exp(prop * wst * xtflie * 86400. / tiefe)
   
   ! sinkgeschwindigkeit in ruhendem medium
   ws0 = log(ased) / bsed
   ws0 = 10**ws0
   
   ! sedimentierter anteil in ruhendem medium
   oc0 = 1. - 1. / exp(prop * ws0 * xtflie * 86400. / tiefe)

end subroutine sedimentation
