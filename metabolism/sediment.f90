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

!> Berechnung des Schlammzehrungspotentials
!! @author Volker Kirchesch
!! @date 30.08.1994
subroutine sediment(abfr, mStra, Stakm, mStas, mSs, aschif, eschif,           &
                    SedOM, SedOMb, dKorn, dKornb, raua, vmq, Hmq, nbuhn, bvmq,&
                    bHmq, jsed, w2, w2b,                                      &
                    kontroll, jjj)
   
   use allodim
   implicit none
   
   integer                         :: n, ns, nschif, ms, mstr
   integer                         :: msta, kbuhn, jsed, ised, ischif
   real                            :: vmitt1, v6, ust, tiefe1, raun
   real                            :: phytoc, hcon, g, gesss, fsch
   real                            :: fom_oc, bsbc
   integer                         :: azStr
   integer, dimension(azStrs)      :: mStas, mSs, abfr, mStra, nbuhn
   real                            :: oc, wst
   real, dimension(azStrs,20)      :: aschif, eschif
   real, dimension(azStrs,1000)    :: dKorn, SedOM, raua, Stakm, vmq, Hmq, bvmq, bHmq, SedOMb, dKornb, w2, w2b
   logical, intent(in)             :: kontroll  !< debugging
   integer, intent(in)             :: jjj       !< debugging
   character(1000)                 :: message
   
   ! fOM_OC Verhältnis organisches Material zu organischem Kohlenstoff
   fOM_OC = 1./0.378
   
   do 30 azStr = 1,azStrs
      mS = 1
      mstr = mStra(azStr)
      do 35 mSta = 1,mStas(mstr)
         ischif = 0
         if (mSs(mstr) == 0)goto 700
         if (abfr(mstr) == 1)goto 879
         if (Stakm(mstr,mSta) <= aschif(mstr,mS) .and. Stakm(mstr,mSta) >= eschif(mstr,mS)) then
            ischif = 1
            goto 700
         endif
         
         if ((mS+1) > mSs(mstr))goto 700
         if (Stakm(mstr,mSta) <= eschif(mstr,mS) .and. Stakm(mstr,mSta) <= aschif(mstr,mS+1)) then
            ischif = 1
            mS = mS+1
            goto 700
         endif
         
         ! Kilometrierung wird zur Muendung hin groesser
         879 if (Stakm(mstr,mSta) >= aschif(mstr,mS) .and. Stakm(mstr,mSta) <= eschif(mstr,mS)) then
            ischif = 1
            goto 700
         endif
         if ((mS+1) > mSs(mstr))goto 700
         if (Stakm(mstr,mSta) >= eschif(mstr,mS) .and. Stakm(mstr,mSta) >= aschif(mstr,mS+1)) then
            ischif = 1
            mS = mS+1
         endif
         
         
         700 raun = 1./raua(mstr,mSta)
         g = sqrt(9.81)
         
         ! Fehlermeldung
         if (Hmq(mstr,mSta) <= 0.0) then
            write(message, "(a,f8.3)") "Missing MQ values at profile ", Stakm(mstr,mSta)
            call qerror(message)
         endif
         
         ns = 2
         if (nbuhn(mstr) == 0)ns = 1
         do 50 n = 1,ns
            
            ! Sedimentation
            ! sdFlu...  in g/(m3*d)
            ised = 1
            kbuhn = 1
            
            ! Schiffseinfluss
            vmq(mstr,mSta) = max(0.001,vmq(mstr,mSta))
            if (n == 1) then
               vmitt1 = vmq(mstr,mSta)
               tiefe1 = Hmq(mstr,mSta)
            else
               hcon = 0.21*vmq(mstr,mSta)**2.97
               if (hcon < 0.21)hcon = 0.21
               vmitt1 = vmq(mstr,mSta)*hcon
               vmitt1 = bvmq(mstr,mSta)
               if (bvmq(mstr,mSta) < 0.0) then
                  vmitt1 = vmq(mstr,mSta)
                  kbuhn = 0
               endif
               tiefe1 = bHmq(mstr,mSta)
               if (tiefe1 < 0.0) then
                  tiefe1 = Hmq(mstr,mSta)
                  vmitt1 = vmq(mstr,mSta)
               endif
            endif
            
            if (vmitt1 == 0.0)vmitt1 = 0.00001
            if (tiefe1 == 0.0)tiefe1 = 0.00001
            vmitt1 = abs(vmitt1)
            ust = ((raun*g)/(tiefe1**0.16667))*vmitt1
            
            ! ischif = 0 -> kein Schiffsverkehr
            !        = 1 -> Schiffsverkehr
            ! v6 - Schiffsgeschwindigkeit
            !
            if (n == 1 .or. kbuhn == 0) then
               v6 = 0.0
               fsch = 1.
               if (ischif == 1) then
                  call schiff(vmitt1,tiefe1,v6,nschif)
                  fsch = -5.88*v6+1.76
                  if (fsch < 0.0)fsch = 0.0
                  if (fsch > 1.)fsch = 1.
               else
               endif
            endif
            !
            if (n == 2 .and. kbuhn == 1) then
               fsch = 1.
               if (ischif == 1) then
                  vmitt1 = vmitt1*2.5
                  ust = ((raun*g)/(tiefe1**0.16667))*vmitt1
               else
               endif
            endif
            
            
            BSBC = 5.  !1.9
            PhytoC = 3.4   !1.3
            GesSS = 55.            ! 16.
            call Sed_POM(tiefe1,ust,n,BSBC,PhytoC,GesSS,SedOM,dKorn,SedOMb,&
                        dKornb,fsch,fOM_OC,mstr,mSta,jsed,w2,w2b,   &
                        kontroll,jjj)
            
         50 continue
      35 continue
   30 continue
   
   
   999 return
end
