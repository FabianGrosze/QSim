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
subroutine sedimentation(tiefe,ised,ust,qsgr,oc,Oc0,ytflie,wst,jsed,ZellV, &
                         kontroll,jjj)
   implicit none
   real                 :: tiefe,ust,qsgr,oc,Oc0,ytflie,wst,ZellV
   real                 :: ased,bsed,prop,  WsAlg,wsgr,fwst,qssed,ws0, ws
   integer              :: ised,jsed
   logical, intent(in)  :: kontroll  !< debugging
   integer, intent(in)  :: jjj       !< debugging
   
   character(100)       :: message
   
   if (ised < 1 .or. ised > 4) then
      write(message, '("sedimentation.f90: Invalid sedimentation option: ",i2)') ised
      call qerror(trim(message))
   endif
   
   if (jsed == 0) then
      !   Algen
      if (ised == 1) then
         ased = 6.67e-7
         bsed = 2.78
         prop = 0.53
         !   BSB
      else if (ised == 2) then
         ased = 1.44E-6
         bsed = 3.13
         prop = 0.6
         !   Gesamt-SS
      else if (ised == 3) then
         ased = 1.74E-4
         bsed = 1.63
         prop = 1.36
         !   Nitrifikanten
      else if (ised == 4) then
         ASED = 1.91E-7
         BSED = 3.00
         prop = 0.56
      endif
   else
      if (ised == 1) then
         !   Algen
         bsed = 2.7
         prop = 0.5
         if (ZellV > 0.) then
            WsAlg = 2.0155 * log10(ZellV) - 11.512
            WsAlg = 10**WsAlg
            Ased = 1. / exp(-bsed * log10(WsAlg))
         else
            call qerror('sedimentation.f90: ZellV = 0.')
         endif
      else if (ised == 2) then
         !   BSB
         ased = 2.43E-7
         bsed = 2.5
         prop = 0.7
      else if (ised == 3) then
         !   Gesamt-SS
         ased = 1.55e-7
         bsed = 2.8
         prop = 0.75
      else if (ised == 4) then
         !   Nitrifikanten
         ASED = 2.43E-7
         BSED = 2.5
         prop = 0.7
      endif
   endif
   
   ! wsgr = 0.625*ust**2.1
   wsgr = 0.14 * ust**2 + 0.0054 * ust + 1.25e-6
   qsgr = 1. / (1.+ ased * exp(-bsed * alog10(wsgr)))
   qssed = 0.5 * (1. + qsgr)
   
   if (qssed < 1. .and. qssed > 0. .and. ased > 0. .and. abs(bsed) > 0.) then
      ws = (log(ased) - log(1. / qssed - 1.)) / bsed
   else
      ws = 0.
   endif
   ws  = 10**ws
   
   if (ased > 0. .and. abs(bsed) > 0.) then
      ws0 = log(ased) / bsed   !ws0 - Sinkgeschwindigkeit in ruhendem Medium
   else
      ws0 = 0.
   endif
   ws0 = 10**ws0
   
   !      fwst = 1.14*exp(-188.2*ust)
   fwst = min(1., exp(-604.2 * ust))
   if (ised == 3) fwst = max(1.91e-19,fwst)
   
   wst = ws * fwst
   Oc  = 1. / exp(prop * wst * yTFLIE * 86400. / tiefe)
   oc  = 1. - Oc
   Oc0 = 1. / exp(prop * ws0 * yTFLIE * 86400. / tiefe) ! sedimentierter Anteil in ruhendem Medium
   OC0 = 1. - Oc0
   
   if (kontroll) then
      print*,'Sedimentation: tiefe,ised,ust,qsgr,oc,Oc0,ytflie,wst,jsed,ZellV = '    &
            ,tiefe,ised,ust,qsgr,oc,Oc0,ytflie,wst,jsed,ZellV
   endif
   
end subroutine sedimentation
