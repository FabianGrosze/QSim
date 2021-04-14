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
  
  subroutine ph_kern(mws,pws,cas,lfs,tempws,vphs,vco2s                 &
                    ,tflie,raus,vmitts,tiefes,rhyds,flaes              &
                    ,wges,WLages,hWSs,iphys                            &
					,bsbcts,resdrs,dzres1s,dzres2s                     &
					,dalgkis,dalggrs,dalgbls,dalgaks,dalgags,dalgabs   &
					,Caki,Cagr,Cabl                                    &
					,albergs,alberks,albewgs,albewks                   &
					,susns,po2ps,po2rs,ssalgs,stinds                   &
                    ,kontroll ,jjj )
					
!!!!! in der Kernroutine ist allen Übergabe-Variablen ein "s" im Namen angehängt, \n
!!!!! um zu kennzeichen, dass es sich um lokale (Einzel-)Variablen in dieser Subroutine handelt \n
!!!!! und nicht um die globalen Felder des Hauptprogramms. \n
	  
      implicit none
 
      ! m-Wert,p-Wert,Calcium,Leitf.,T°C,pH, CO2
	  real mws,pws,cas,lfs,tempws,vphs,vco2s
	  !dt,Kst,Geschw.,Tiefe,hydr.Rad.,Oberfläche
	  real tflie,raus,vmitts,tiefes,rhyds,flaes
	  !Windgeschw.,Höhenlage,WSP
	  real wges,WLages,hWSs
	  ! Steuerung Belüftungsverfahren
      integer iphys
	  ! CO2 Produktion von: Bakterien, Muscheln und Zooplankton
	  real bsbcts, resdrs,dzres1s,dzres2s
	  ! CO2 Verbrauch und Produktion von planktischen Algen
	  real dalgkis,dalggrs,dalgbls,dalgaks,dalgags,dalgabs
	  ! Kohlenstoffgehalte der Algenbiomassen
	  real Caki, Cagr, Cabl
	  ! CO2 Verbrauch und Produktion von bentischen Algen
	  real albergs,alberks,albewgs,albewks
	  !oxidiertes Amonium, CO2 Verbrauch und Produktion von Makrophyten, ssalgs,stinds
	  real susns,  po2ps,po2rs,   ssalgs,stinds	  
      logical kontroll
      integer jjj                                                                    
 
	  real mwt,pwt,cat,lft
      real bbeis,abst,pk1,pk2,pkw,pkca,mue,lgk1,lgk2,lgkca,hk,k1,k2,kca
	  real lgh,h,poh,lgoh,oh,beta,fco2,fhco3,fco3,moco2,mohco3,moco3
      real mgco2,mghco3,mgco3,fca
	  real MOCA,c,SAETCO2,DEFCO2,MOLGCO,HCON,DCO2O,BKCO2,CO2BSB,CO2DR
	  real CDR,CO2ZOO,CROT,ALCO2M,PFCO2R,ALHCO3,CO2ALW,CO2PFW,FPFL,FALG,DELTAH
	  real GHCO31,GCO21,DCA,DCAH,DLF,RHCO3,RCO3,HCONTI
	  real UEBCA,TIND,CUEBCA,R1,R2,DCA1,DCA2,GCO31,CAV1,PH1,PH2
	  integer IITER
	  real PH0,Y1,ETA,Y2,Y,DELPH,DELCA,DELLF,DELMW
	  
	  cat = cas
      mwt = mws
      pwt = pws
      lft = lfs
!                                                                       
!     Berechnung der Kohlensaeuresumme in mol/l                         
!                                                                       
      mws = mws*1.e-3 
      pws = pws*1.e-3 
      moca = cas/(1000.*40.08) 
      c = mws-pws 
!                                                                       
!                                                                       
!     Berechnung der absoluten Temperatur                               
!                                                                       
      abst = tempws+273.16 
!                                                                       
!     Berechnung der negativen Logarithmen der Dissoziationskonstantenl 
!     bei einer Ionenstaerke von 0 mol/l                                
!     in Abhaengigkeit von der abs. Temperatur                          
!                                                                       
      pk1 = (17052./abst)+215.21*log10(abst)-0.12675*abst-545.56 
!                                                                       
      pk2 = (2902.39/abst)+0.02379*abst-6.498 
!                                                                       
      pkw = (4471.33/abst)+0.017053*abst-6.085 
!                                                                       
      kca = 9.41e-9-2.52e-10*tempws+2.76e-12*tempws**2          &
     &-1.14e-14*tempws**3                                           
!                                                                       
      pkca = (alog10(kca))*(-1.) 
!                                                                       
!     Einfluss der Ionenstaerke                                         
!                                                                       
      if(lfs.lt.0.0)lfs = 0.0 
      mue = 1.7e-5*lfs 
      lgk1 = sqrt(mue)/(1.+1.4*sqrt(mue)) 
      lgk2 = (2.*sqrt(mue))/(1.+1.4*sqrt(mue)) 
      lgkca = (4.*sqrt(mue))/(1.+3.9*sqrt(mue)) 
      hk = (0.5*sqrt(mue))/(1.+1.4*sqrt(mue)) 
!                                                                       
!     Berechnung der von Temperatur und Ionenstaerke abhaengigen        
!     Konstanten                                                        
!                                                                       
      k1 = pk1-lgk1 
      k2 = pk2-lgk2 
      kca = pkca-lgkca 
      k1 = 10**(-k1) 
      k2 = 10**(-k2) 
      kca = 10**(-kca) 
!                                                                       
!     Berechnung der Konzentrationen an H+ und OH-                      
!                                                                       
      lgh = vphs-hk 
      h = 10**(-lgh) 
      poh = pkw-vphs 
      lgoh = poh-hk 
      oh = 10**(-lgoh) 
!                                                                       
!                                                                       
!                                                                       
!     Berechnung der Kohlenssaeureformen(nur einmal)                    
!                                                                       
      beta = 1+(k1/h)+(k1*k2/h**2) 
      fco2 = 1./beta 
      fhco3 = k1/(beta*h) 
      fco3 = (k1*k2)/(beta*h**2) 
!                                                                       
      moco2 = c*fco2 
      mohco3 = c*fhco3 
      moco3 = c*fco3 
!                                                                       
      mgco2 = (moco2*44.)*1000. 
      mghco3 = (mohco3*61.02)*1000. 
      mgco3 = (moco3*60.009)*1000. 
                                                                       
                                                                       
      fca = 1+(h/k2)+(h**2/(k1*k2)) 
!                                                                       
!                                                                       
!     phsikalischer CO2 Ein-Austrag ueber die Gewaesseroberflaeche      
!                                                                       
      saetco2 = -0.00000907*tempws**3+0.0009662*tempws**2-0.04657*tempws+1.27                                        

      DefCO2 = saetco2-MGCO2 
      molgco = 44. 
      hcon = sqrt(32./molgco) 
      dco2o = 0.0 
                                                                       

!.....Berechnung des BELUEFTUNGSBEIWERT in 1/d       
                                                                        
  call Belueftung_K2(raus,tiefes,vmitts,rhyds,flaes,tempws,WLages,hwss,wges,iphys,bbeis)

      bkco2 = hcon*bbeis 

      DCO2o = defco2*(1.-exp(-bkco2*tflie)) 
                                                                       
!##### CO2 Lieferung durch C-abbau #####                                       
                                                                       
      co2bsb = bsbcts 
                                                                       
!##### CO2 Lieferung bei der Atmung von Dreissen und Zooplankter ######        
                                                                       
      CO2dr = resdrs * CDR 
                                                                       
      co2zoo = (dzres1s + dzres2s) * CRot         

      alco2m = dalgaks*Caki + dalgags*Cagr + dalgabs*Cabl + albergs*Cagr + alberks*Caki                                 
      pfco2r = po2rs*(44.0/32.0) ! Umrechnung Molmasse CO2/ O2 !war: pfco2r = po2rs*1.3 
                                                                       
      mgco2 = mgco2+alco2m+pfco2r+co2bsb+dco2o+co2dr+co2zoo                                                     
                                                                      
                                                                       
!##### CO2 Verbrauch durch Algenwachstum und Pflanzen #####                    

      alhco3 = 0.0 
      co2alw = dalgkis*Caki + dalggrs*Cagr + dalgbls*Cabl + albewgs*Cagr + albewks*Caki                                  

      co2pfw = po2ps*(44.0/32.0) ! Umrechnung Molmasse CO2/ O2 ! War fälsclicherwiese: co2pfw = po2ps/1.3 

      if((co2alw+co2pfw).gt.mgco2)then 
      if((co2pfw+co2alw).eq.0.0)then 
      fpfl = 0.0 
      falg = 0.0 
      goto 123 
      endif 
      fpfl = co2pfw/(co2pfw+co2alw) 
      falg = co2alw/(co2pfw+co2alw) 
  123 alhco3 = ((co2pfw+co2alw)-mgco2) 
      co2pfw = mgco2*fpfl 
      co2alw = mgco2*falg 
      endif 
                                                                       
                                                                       
! ##### Einfluss der Nitrifikation #####                                        
                                                                       
      deltah = 2.*(susns/14.)/1000. 
      h = h+deltah 
                                                                       
      ghco31 =mghco3 
      gco21 = mgco2 
      mghco3 = mghco3-alhco3 
      mgco2 = mgco2-co2alw-co2pfw 
                                                                       
      if(mghco3.lt.0.0)mghco3  = (ghco31/(ghco31+alhco3))*ghco31                         
                                                                       
      if(mgco2.lt.0.0)mgco2  = (gco21/(gco21+co2alw+co2pfw))*gco21                      
                                                                       
      vco2s = mgCo2 
                                                                       
      DCA = alhco3*0.328 
      DCAH = DCA 
      cat = cas-dca 
                                                                       
      if(cat.lt.0.0)then 
      cat = (cas/(cas+dca))*cas 
      DCA = Cat-cas 
      endif 
                                                                       
      moca = cat/(1000.*40.08) 
      DLF = DCA*2.2 
      LFt = lfs-DLF 
                                                                       
      mohco3 = mghco3/61.02/1000. 
      moco2 = mgco2/44.0/1000. 
      moco3 = mgco3/60.009/1000. 
                                                                       
                                                                       
! ##### Berechnung der Calcitbildung ####                                      
                                                                       
      dca = 0.0 
      rhco3 = 0.0 
      rco3 = 0.0 
      hconti = tflie*1440. 
                                                                       
! #### Berechnung der Calciumuebersaettigung ####                             
                                                                       
      uebca = moca*moco3/kca 
      tind = (1577.*exp(-0.0496*uebca))*10./ssalgs
      if(tind.gt.stinds)then
	     if(kontroll)print*,'pH keine Calcium Änderung tind,stinds,uebca,Ca=',tind,stinds,uebca,cat
	     goto 55
	  else
	     if(kontroll)print*,'pH Calcium Änderung tind,stinds,uebca,Ca=',tind,stinds,uebca,cat
	  endif
   72 continue 
                                                                       
      if(lfs.lt.0.0)lfs = 0.0 
      mue = 1.7e-5*lfs 
      lgkca = (4.*sqrt(mue))/(1.+3.9*sqrt(mue)) 
      kca = pkca-lgkca 
      kca = 10**(-kca) 
                                                                       
      uebca = moca*moco3/kca 
      dca = 0.0 
      rhco3 = 0.0 
      rco3 = 0.0 
      if(uebca.lt.10.0)goto 55 
                                                                       
      dca = 0.0406*exp(0.0362*uebca) 
      dca = dca*0.001*40.08*ssalgs 
      cuebca = (((uebca-10.)*kca)/moco3)*40.08*1000. 
      if(dca.gt.cuebca)dca = cuebca 
                                                                       
      r1 = -37.47*vphs+360.67 
      r1 = r1/100. 
      r2 = 1.-r1 
      rhco3 = r1*dca*1.52 
      rco3 = r2*dca*1.5 
      dca1 = r1*dca 
      dca2 = r2*dca 
      if(rhco3.gt.mghco3)dca1 = mghco3/(r1*1.52) 
      if(rco3.gt.mgco3)dca2 = mgco3/(r2*1.5) 
      dca = dca1+dca2 
                                                                       
      if(alhco3.gt.rhco3)then 
      rhco3 = 0.0 
      dca = dca2 
      endif 
                                                                       
      if(alhco3.le.rhco3)then 
      rhco3 = rhco3-alhco3 
      dca = dca-dcaH 
      endif 
                                                                       
      ghco31 =mghco3 
      gco31 = mgco3 
      mghco3 = mghco3-rhco3 
      mgco3 = mgco3-rco3 
                                                                       
      if(mghco3.lt.0.0)mghco3  = (ghco31/(ghco31+rhco3))*ghco31                          
                                                                       
      if(mgco3.lt.0.0)mgco3  = (gco31/(gco31+rco3))*gco31                               
                                                                       
      mohco3 = mghco3/61.02/1000. 
      moco3 = mgco3/60.009/1000. 
                                                                       
      cav1 = cat 
      cat = cat-dca 
                                                                       
      if(cat.lt.0.0)cat = (cav1/(cav1+dca))*cav1                                      
                                                                       
      moca = cat/(1000.*40.08) 
      DLF = DCA*2.2 
      LFt = LFt-DLF 
                                                                       
   55 continue 

!      poh = (-1.*(log10(h)))-pkw+2*hk                                  
!      oh = 10**poh                                                     
      mwt = mohco3+2*moco3+oh-h 
      pwt = moco2*(-1.)+moco3+oh-h 
                                                                       
      c = moco2+mohco3+moco3 
                                                                       
! #### Schaetzer ####                                                         
!                                                                       
      ph1 = 0. 
      ph2 = 14. 
                                                                       
                                                                       
! ##### Berechnung der Konzentrationen an H+ und OH- ####                      
                                                                       
      iiter = 0 
    1 ph0 = (ph1+ph2)/2. 
      poh = pkw-ph0 
      lgh = ph0 -hk 
      lgoh = poh-hk 
      h = 10**(-lgh) 
      oh = 10**(-lgoh) 
      y1 = oh-h 
                                                                       
! #### Berechnung des Aequivalenzfaktors eta #####                            
!                                                                       
      eta = (k1*h+2*k1*k2)/((h**2)+k1*h+k1*k2) 
      y2 = mwt-c*eta 
      y = y2-y1 
                                                                       
      delph = ph2-ph1 
      if(delph<0.001.or.iiter==50)goto 10 
      if(y.lt.0.0)then 
      ph2 = ph0 
      iiter = iiter+1 
      goto 1 
      endif 
      ph1 = ph0 
      iiter = iiter+1 
      goto 1 
                                                                       
   10 vphs = ph1 
      hconti = hconti-1. 
      if(hconti.lt.1.or.uebca.lt.20)goto 56 
      goto 72 
                                                                       
   56 mwt = mwt*1000. 
      pwt = pwt*1000. 
      stinds = stinds+tflie*1440. 
                                                                       
      delca = cat-cas 
      dellf = lft-lfs 
      delmw = mwt-mws*1000. 
      mws = mws*1000. 
      pws = pws*1000. 
                                                                       
      if(cat.lt.0.0)cat = (cas/(cas+abs(delca)))*cas                      
      if(Lft.lt.0.0)Lft = (lfs/(lfs+abs(delLf)))*lfs                      
      if(mwt.lt.0.0)mwt = (mws/(mws+abs(delmw)))*mws                      

	  cas = cat
      mws = mwt
      pws = pwt
      lfs = lft
  
  end subroutine ph_kern
