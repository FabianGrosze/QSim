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
                    control, jjj)
   
   use module_alloc_dimensions
   implicit none
   
   integer, intent(in),    dimension(azStrs)         :: abfr
   integer, intent(in),    dimension(azStrs)         :: mStra
   real,    intent(in),    dimension(azStrs,ialloc2) :: Stakm
   integer, intent(in),    dimension(azStrs)         :: mStas
   integer, intent(in),    dimension(azStrs)         :: mSs
   real,    intent(in),    dimension(azStrs,ialloc3) :: aschif, eschif
   real,    intent(inout), dimension(azStrs,ialloc2) :: sedOM
   real,    intent(inout), dimension(azStrs,ialloc2) :: SedOMb
   real,    intent(inout), dimension(azStrs,ialloc2) :: dKorn
   real,    intent(inout), dimension(azStrs,ialloc2) :: dKornb
   real,    intent(in),    dimension(azStrs,ialloc2) :: raua
   real,    intent(inout), dimension(azStrs,ialloc2) :: vmq
   real,    intent(in),    dimension(azStrs,ialloc2) :: Hmq
   integer, intent(in),    dimension(azStrs)         :: nbuhn
   real,    intent(in),    dimension(azStrs,ialloc2) :: bvmq
   real,    intent(in),    dimension(azStrs,ialloc2) :: bHmq
   integer, intent(in)                               :: jsed
   real,    intent(inout), dimension(azStrs,ialloc2) :: w2
   real,    intent(inout), dimension(azStrs,ialloc2) :: w2b
   logical, intent(in)                               :: control  !< debugging
   integer, intent(in)                               :: jjj       !< debugging
   
   
   integer         :: n, ns, nschif, ms, mstr, azStr
   integer         :: msta, kbuhn, ised, ischif
   real            :: vmitt1, v6, ust, tiefe1, raun, hcon, fsch
   character(1000) :: message
   
   real, parameter :: g = 9.81
   
   external  :: qerror, schiff, sed_pom
   
 
   
   do azStr = 1,azStrs
      mS = 1
      mstr = mStra(azStr)
      do mSta = 1,mStas(mstr)
         ischif = 0
         if (mSs(mstr) /= 0) then
            if (abfr(mstr) /= 1) then
               
               if (Stakm(mstr,mSta) <= aschif(mstr,mS) .and. Stakm(mstr,mSta) >= eschif(mstr,mS)) then
                  ischif = 1
                  
               else if ((mS+1) > mSs(mstr)) then
               
               else if (Stakm(mstr,mSta) <= eschif(mstr,mS) .and. Stakm(mstr,mSta) <= aschif(mstr,mS+1)) then
                  ischif = 1
                  mS = mS+1
               endif
            
            else
               if (Stakm(mstr,mSta) >= aschif(mstr,mS) .and. Stakm(mstr,mSta) <= eschif(mstr,mS)) then
                  ischif = 1
               
               else if ((mS+1) > mSs(mstr)) then
               
               else if (Stakm(mstr,mSta) >= eschif(mstr,mS) .and. Stakm(mstr,mSta) >= aschif(mstr,mS+1)) then
                  ischif = 1
                  mS = mS+1
               endif
               
            endif
         endif
         
         raun = 1./raua(mstr,mSta)
         
         ! Fehlermeldung
         if (Hmq(mstr,mSta) <= 0.0) then
            write(message, "(a,f8.3)") "Missing MQ values at profile ", Stakm(mstr,mSta)
            call qerror(message)
         endif
         
         ns = 2
         if (nbuhn(mstr) == 0)ns = 1
         do n = 1,ns
            
            ! Sedimentation
            ! sdFlu [g/(m3*d)]
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
            
            if (vmitt1 == 0.0) vmitt1 = 0.00001
            if (tiefe1 == 0.0) tiefe1 = 0.00001
            vmitt1 = abs(vmitt1)
            ust = ((raun*sqrt(g))/(tiefe1**0.16667))*vmitt1
            
            if (n == 1 .or. kbuhn == 0) then
               v6 = 0.0
               fsch = 1.
               if (ischif == 1) then
                  call schiff(vmitt1, tiefe1, nschif, v6)
                  fsch = -5.88 * v6 + 1.76
                  if (fsch < 0.0)fsch = 0.0
                  if (fsch > 1.)fsch = 1.
               endif
            endif
            
            if (n == 2 .and. kbuhn == 1) then
               fsch = 1.
               if (ischif == 1) then
                  vmitt1 = vmitt1*2.5
                  ust = ((raun*g)/(tiefe1**0.16667))*vmitt1
               else
               endif
            endif
            
            call sed_pom(tiefe1, ust, n, sedom(mstr,mSta), dkorn(mstr,mSta),  &
                         sedomb(mstr,mSta), dkornb(mstr,mSta), fsch, jsed,    &
                         w2(mstr,mSta), w2b(mstr,mSta),                       &
                         control, jjj)
            
         enddo
      enddo
   enddo
   
   return
end subroutine sediment
