!---------------------------------------------------------------------------------------
!
!   QSim - Programm zur Simulation der Wasserqualität
!
!   Copyright (C) 2020 Bundesanstalt für Gewässerkunde, Koblenz, Deutschland, http://www.bafg.de
!
!   Dieses Programm ist freie Software. Sie können es unter den Bedingungen der 
!   GNU General Public License, Version 3,
!   wie von der Free Software Foundation veröffentlicht, weitergeben und/oder modifizieren. 
!   Die Veröffentlichung dieses Programms erfolgt in der Hoffnung, daß es Ihnen von Nutzen sein wird, 
!   aber OHNE IRGENDEINE GARANTIE, sogar ohne die implizite Garantie der MARKTREIFE oder der VERWENDBARKEIT FÜR EINEN BESTIMMTEN ZWECK. 
!   Details finden Sie in der GNU General Public License.
!   Sie sollten ein Exemplar der GNU General Public License zusammen mit diesem Programm erhalten haben. 
!   Falls nicht, siehe http://www.gnu.org/licenses/.  
!   
!	Programmiert von:
!	1979 bis 2018 Volker Kirchesch
!	seit 2011 Jens Wyrwa, Wyrwa@bafg.de
!
!---------------------------------------------------------------------------------------

      subroutine Sediment(abfr,azStrs,mStra,Stakm,mStas,mSs,aschif,eschif,SedOM             &
                         ,SedOMb,dKorn,dKornb,raua,vmq,Hmq,nbuhn,bvmq,bHmq,jsed,w2,w2b,ifehl)                                    
!                                                                       
!                                                                       
!                                                                       
!     Autor: V. Kirchesch                                               
!                                                                       
!                                                                       
!     Stand: 30.08.94                                                   
!                                                                       
!     Berechnung des schlammzehrungspotentials                          
!                                                                       

      integer                               :: azStr, azStrs
      integer, Dimension(azStrs)            :: mStas, mSs, abfr, mStra, nbuhn 

      real                                  :: oc, wst 
      real, Dimension(azStrs,20)            :: aschif, eschif
      real, Dimension(azStrs,1000)          :: dKorn, SedOM, raua, Stakm, vmq, Hmq, bvmq, bHmq, SedOMb, dKornb, w2, w2b  

                                                                       
!      open(unit=461,file='schlamm.tst')                                
                                                                       
                                                                       
!     fOM_OC Verhältnis organisches Material zu organischem Kohlenstoff 
      fOM_OC = 1./0.378 
                                                                       
      do 30 azStr = 1,azStrs 
      mS = 1 
      mstr = mStra(azStr) 
      do 35 mSta = 1,mStas(mstr)

      ischif = 0 
      if(mSs(mstr).eq.0)goto 700 
      if(abfr(mstr).eq.1)goto 879 
      if(Stakm(mstr,mSta).le.aschif(mstr,mS).and.Stakm(mstr,mSta).ge.eschif(mstr,mS))then                     
        ischif = 1 
        goto 700 
      endif
 
      if((mS+1).gt.mSs(mstr))goto 700 
      if(Stakm(mstr,mSta).le.eschif(mstr,mS).and.Stakm(mstr,mSta).le.aschif(mstr,mS+1))then                   
        ischif = 1 
        mS = mS+1 
        goto 700 
      endif 
                                                                       
! #### Kilometrierung wird zur Muendung hin groesser ####                    
                                                                       
  879 if(Stakm(mstr,mSta).ge.aschif(mstr,mS).and.Stakm(mstr,mSta).le.eschif(mstr,mS))then                     
        ischif = 1 
        goto 700 
      endif 

      if((mS+1).gt.mSs(mstr))goto 700 
      if(Stakm(mstr,mSta).ge.eschif(mstr,mS).and.Stakm(mstr,mSta).ge.aschif(mstr,mS+1))then                   
        ischif = 1 
        mS = mS+1 
      endif 
                                                                       
                                                                       
  700 raun = 1./raua(mstr,mSta) 
      g = sqrt(9.81) 
                                                                       
! #### Fehlermeldung ####
                                                      
      if(Hmq(mstr,mSta)<=0.0)then 
        write(199,1999)Stakm(mstr,mSta) 
   1999 format(2x,'Es fehlt die Zuordnung von MQ-Werten am Profil: ',F8.3)
        ifehl = 26                                                            
        goto 999
      endif 
                                                                       
      ns = 2 
      if(nbuhn(mstr).eq.0)ns = 1 
      do 50 n = 1,ns 
!                                                                       
!     SEDIMENTATION                                                     
!...sdFlu...  in g/(m3*d)                                               
!                                                                       
      ised = 1 
      kbuhn = 1 
!                                                                       
! ....Schiffseinfluss                                                   
!                                                                       

      vmq(mstr,mSta) = max(0.001,vmq(mstr,mSta))

      if(n.eq.1)then 
      vmitt1 = vmq(mstr,mSta) 
      tiefe1 = Hmq(mstr,mSta)
         else 
      hcon = 0.21*vmq(mstr,mSta)**2.97 
      if(hcon.lt.0.21)hcon = 0.21 
      vmitt1 = vmq(mstr,mSta)*hcon 
      vmitt1 = bvmq(mstr,mSta)

      if(bvmq(mstr,mSta).lt.0.0)then 
      vmitt1 = vmq(mstr,mSta)
         kbuhn = 0 
       endif 
      tiefe1 = bHmq(mstr,mSta) 
      if(tiefe1.lt.0.0)then
        tiefe1 = Hmq(mstr,mSta) 
        vmitt1 = vmq(mstr,mSta)
      endif
    endif 
                                                                       
      if(vmitt1==0.0)vmitt1 = 0.00001
      if(tiefe1==0.0)tiefe1 = 0.00001 
      vmitt1 = abs(vmitt1) 

      ust = ((raun*g)/(tiefe1**0.16667))*vmitt1 
!                                                                       
!..ischif = 0 -> kein Schiffsverkehr                                    
!         = 1 -> Schiffsverkehr                                         
!...v6 - Schiffsgeschwindigkeit                                         
!                                                                       
      if(n.eq.1.or.kbuhn.eq.0)then 
      v6 = 0.0 
      fsch = 1. 
      if(ischif.eq.1)then 
      call schiff(vmitt1,tiefe1,v6,nschif) 
      fsch = -5.88*v6+1.76 
        if(fsch.lt.0.0)fsch = 0.0 
        if(fsch.gt.1.)fsch = 1. 
            else 
          endif 
      endif 
!                                                                       
      if(n.eq.2.and.kbuhn.eq.1)then 
      fsch = 1. 
      if(ischif.eq.1)then 
      vmitt1 = vmitt1*2.5 
      ust = ((raun*g)/(tiefe1**0.16667))*vmitt1 
            else 
          endif 
      endif 
                                                                       
                                                                       
      BSBC = 5.  !1.9
      PhytoC = 3.4   !1.3 
      GesSS = 55.            ! 16. 

      call Sed_POM(tiefe1,ust,n,BSBC,PhytoC,GesSS,SedOM,dKorn,SedOMb,dKornb,fsch,fOM_OC,mstr,mSta,azStrs,jsed,w2,w2b)                                
                                                                       
   50 continue 
   35 continue 
   30 continue 
                                                                       
                                                                       
  999 return 
      END                                           
