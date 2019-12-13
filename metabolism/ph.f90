      subroutine ph(mw,pw,ca,lf,tempw,tflie,susn,bsbct,dalgki,dalggr,dalgak,dalgag,po2p,po2r,rau,vmitt,tiefe                  &
                    ,flae,vabfl,flag,elen,ior,anze,vph,elfL,CaL,qeinlL,iorLa,iorLe,ieinLs,ssalg,stind,albewg                  &
                    ,alberg,albewk,alberk,wge,abl,dalgbl,dalgab,IDWe,iwied,fkm,ij,resdr,dzres1,dzres2,aki,agr                 &
                    ,ilbuhn,eph,emw,elf,eca,vco2,qeinl,jiein,mstr,cpfad,rhyd,WLage,hWS,itags,monats,uhrz,azStrs,iphy          &                                                   
                    ,kontroll ,jjj ) !!wy  
                                                                       
                                                                       
!##### EIN PROGRAMM ZUR BERECHNUNG DES PH-WERTES EINES GEWAESSERS        
!      aus dem m-Wert und der Kohlensaeuresumme ########                         
                                                                       
                                                                       
                                                                       
                                                                       
                                                                       
                                                                       
!     STAND:11.06.1993                                                  
                                                                       
                                                                       
!     AUTOR:VOLKER KIRCHESCH                                            
                                                                       
                                                                       
      logical kontroll !!wy
      integer jjj !!wy                                                                       
      character (len=2)                          :: cwert 
      character (len=255)                        :: cpfad 
      character (len=275)                        :: pfadstring 

      integer                                    :: anze, azStrs
      integer, Dimension(azStrs)                 :: ieinLs
      integer, Dimension(100)                    :: iorLa, iorLe 
      integer, Dimension(1000)                   :: flag, jiein
      integer, Dimension(azStrs,1000)            :: IDWe 

      real                                       :: MUE, lgk1, lgk2, lgkca, lgh, lgoh, moco2, mohco3, moco3, mwt,lft 
      real                                       :: oh, h, k1, k2, mgco2, mghco3, mgco3, kca, moca   
      real, Dimension(20)                        :: wge
      real, Dimension(100)                       :: eph, emw, elf, elfL, caL, eca, qeinl, qeinlL  
      real, Dimension(1000)                      :: mw, pw, tempw, ca, lf, vph, flae, vmitt, tiefe, rau, bsbct, dalgki 
      real, Dimension(1000)                      :: dalggr, dalgak, dalgag, susn, po2p, po2r, elen, ssalg, vabfl
      real, Dimension(1000)                      :: stind, albewg, alberg, albewk, alberk, fkm, resdr, dzres1, dzres2 
      real, Dimension(1000)                      :: aki, agr, abl, dalgbl, dalgab, vco2, rhyd, bbei 
      real, Dimension(azStrs,1000)               :: WLage, hWS
                                                                       
!      open(unit=18,file='ph.tst')                                      
                                                                    
      Crot = 0.45
      CDR = 0.38

      Caki = 0.48
      Cabl = 0.48
      Cagr = 0.48

!      write(pfadstring,'(2A)')trim(adjustl(cpfad)),'funk.dat' !!wy
!      open(unit=555, file=pfadstring)
!      read(555,'(a2)',end=66)cwert 
!      read(555,5555,end=66)iphy,iphyw,zwgmes 
! 5555 format(i1,2x,i1,2x,f5.1) 
!   66 close (555) 
                                                                       
      iein = 1 
                                                                       
! #### Berücksichtigung der Linienquelle ####                                  
    
      do ieinL = 1, ieinLs(mstr)
        do ior = 1,anze+1
          if(iorLe(ieinL)<ior)cycle
          if(iorLa(ieinL)<=ior.and.iorLe(ieinL)>=ior)then
            if(qeinlL(ieinL)<=0.0)then
              elfL(ieinL) = 0.0
              caL(ieinL) = 0.0
            endif
              if(elfL(ieinL)>=0.0)                        &
                Lf(ior) = Lf(ior)+((elfL(ieinL)-Lf(ior))*qeinlL(ieinL)/flae(ior))*tflie*86400.  ! 1D
              if(CaL(ieinL)>=0.0)                         &  
                Ca(ior) = Ca(ior)+((CaL(ieinL)-Ca(ior))*qeinlL(ieinL)/flae(ior))*tflie*86400.   ! 1D
            else
         endif
       enddo ! Ende Knotenschleife 
     enddo   ! Ende Schleife Linienquellen


      do j=1,anze+1  ! Beginn Schleife Ortspunkte

      ior = j
  
      ior_flag = 0
      if(flag(ior)==6.and.vabfl(ior)<0.0.and.vabfl(ior+1)>0.0)then
        ior = ior+1
        ior_flag = 1
      endif    

      if(ilbuhn==1)then
          else if(flag(ior)/=4)then
            else                        ! Berücksichtigung der Einleitungen
              m = 1
              ihcQ = 0
              if(vabfl(ior-1)<0.0.and.vabfl(ior)<0.0)m = -1
              if(vabfl(ior-1)<0.0.and.vabfl(ior)>0.0)ihcQ = 1 ! Konzentration an der Einleitstelle 
                                                              ! ist gleich der Konzentration der Einleitung 

              
              hcmw = mw(ior-m)     ! Umbenennen der benötigten Variablen; 1D
              hcca = ca(ior-m) 
              hcvph = vph(ior-m) 
              hclf = lf(ior-m)

              mue = 1.7e-5*hclf 
              hk = (0.5*sqrt(mue))/(1.+1.4*sqrt(mue)) 
              lgh = hcvph-hk 
              hcvh = 10**(-lgh) 

              hcQ = vabfl(ior-m)
              if(hcQ<0.0)hcQ = abs(hcQ)
              if(hcQ==0.0.or.ihcQ==1)hcQ = 1.e-10
              
              do ji=1,jiein(ior)   ! Beginn Einleitungsschleife  
              hcQE = max(0.0,qeinl(iein))

              hcphE = eph(iein)
              if(hcphE<0.0)hcphE = hcvph

              hclfE = elf(iein)
              if(hclfE<0.0)hclfE = hclf

              mue = 1.7e-5*hclfE 
              hk = (0.5*sqrt(mue))/(1.+1.4*sqrt(mue)) 
              lgh = hcphE-hk 
              hchE = 10**(-lgh) 

              hcmwE = emw(iein)
              if(hcmwE<0.0)hcmwE = hcmw

              hccaE = eca(iein)
              if(hccaE<0.0)hccaE = hcca

             vhneu = (hcQ*hcvh+hcQE*hchE)/(hcQ+hcQE) 
             mw(ior) = (hcQ*hcmw+hcQE*hcmwE)/(hcQ+hcQE) 
             lf(ior) = (hcQ*hclf+hcQE*hclfE)/(hcQ+hcQE) 
             ca(ior) = (hcQ*hcca+hcQE*hccaE)/(hcQ+hcQE) 

             mue = 1.7e-5*lf(ior) 
             hk = (0.5*sqrt(mue))/(1.+1.4*sqrt(mue)) 
             vph(ior) = log10(vhneu) 
             vph(ior) = (-1.*vph(ior))+hk 
!                                                                       
             hcQ = hcQ+qeinl(iein) 
             iein = iein+1 
             hcmw = mw(ior) 
             hcca = ca(ior) 
             hcvph = vph(ior) 
             hclf = lf(ior) 

             call pweinl(mw,vph,lf,tempw,pw,ior) 
           enddo                        ! Ende Einleitungsschleife
           
           if(ior_flag==1)then
             iein = iein - jiein(ior) 
             ior = ior-1
             vph(ior) = vph(ior+1)
             mw(ior) = mw(ior+1)
             lf(ior) = lf(ior+1)
             ca(ior) = ca(ior+1)
           endif
    endif                               ! Ende Einleitungs-flag                                                                  

      if(ior.gt.1)then 
      ca(ior-1) = cat 
      mw(ior-1) = mwt 
      pw(ior-1) = pwt 
      lf(ior-1) = lft 
      vph(ior-1) = vpht 
      endif 
      vphv = vph(ior) 
!                                                                       
!     Berechnung der Kohlensaeuresumme in mol/l                         
!                                                                       
      mw(ior) = mw(ior)*1.e-3 
      pw(ior) = pw(ior)*1.e-3 
      moca = ca(ior)/(1000.*40.08) 
      c = mw(ior)-pw(ior) 
!                                                                       
!                                                                       
!     Berechnung der absoluten Temperatur                               
!                                                                       
      abst = tempw(ior)+273.16 
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
      kca = 9.41e-9-2.52e-10*tempw(ior)+2.76e-12*tempw(ior)**2          &
     &-1.14e-14*tempw(ior)**3                                           
!                                                                       
      pkca = (alog10(kca))*(-1.) 
!                                                                       
!     Einfluss der Ionenstaerke                                         
!                                                                       
      if(lf(ior).lt.0.0)lf(ior) = 0.0 
      mue = 1.7e-5*lf(ior) 
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
      lgh = vph(ior)-hk 
      h = 10**(-lgh) 
      poh = pkw-vph(ior) 
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
      saetco2 = -0.00000907*tempw(ior)**3+0.0009662*tempw(ior)**2-0.04657*tempw(ior)+1.27                                        

      DefCO2 = saetco2-MGCO2 
      molgco = 44. 
      hcon = sqrt(32./molgco) 
      dco2o = 0.0 
                                                                       

!.....Berechnung des BELUEFTUNGSBEIWERT in 1/d       
                                                                        
  call Belueftung_K2(rau,tiefe,vmitt,rhyd,flae,tempw,WLage,hws,wge,IDWe,iphy,bbei,mstr,ior,azStrs)

      bkco2 = hcon*BBei(ior) 

      DCO2o = defco2*(1.-exp(-bkco2*tflie)) 
                                                                       
!##### CO2 Lieferung durch C-abbau #####                                       
                                                                       
      co2bsb = bsbct(ior) 
                                                                       
!##### CO2 Lieferung bei der Atmung von Dreissen und Zooplankter ######        
                                                                       
      CO2dr = resdr(ior) * CDR 
                                                                       
      co2zoo = (dzres1(ior) + dzres2(ior)) * CRot         

      alco2m = dalgak(ior)*Caki + dalgag(ior)*Cagr + dalgab(ior)*Cabl + alberg(ior)*Cagr + alberk(ior)*Caki                                 
      pfco2r = po2r(ior)*1.3 
                                                                       
      mgco2 = mgco2+alco2m+pfco2r+co2bsb+dco2o+co2dr+co2zoo                                                     
                                                                      
                                                                       
!##### CO2 Verbrauch durch Algenwachstum und Pflanzen #####                    

      alhco3 = 0.0 
      co2alw = dalgki(ior)*Caki + dalggr(ior)*Cagr + dalgbl(ior)*Cabl + albewg(ior)*Cagr+albewk(ior)*Caki                                  

      co2pfw = po2p(ior)/1.3 

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
                                                                       
      deltah = 2.*(susn(ior)/14.)/1000. 
      h = h+deltah 
                                                                       
      ghco31 =mghco3 
      gco21 = mgco2 
      mghco3 = mghco3-alhco3 
      mgco2 = mgco2-co2alw-co2pfw 
                                                                       
      if(mghco3.lt.0.0)mghco3  = (ghco31/(ghco31+alhco3))*ghco31                         
                                                                       
      if(mgco2.lt.0.0)mgco2  = (gco21/(gco21+co2alw+co2pfw))*gco21                      
                                                                       
      vco2(ior) = mgCo2 
                                                                       
      DCA = alhco3*0.328 
      DCAH = DCA 
      cat = ca(ior)-dca 
                                                                       
      if(cat.lt.0.0)then 
      cat = (ca(ior)/(ca(ior)+dca))*ca(ior) 
      DCA = Cat-Ca(ior) 
      endif 
                                                                       
      moca = cat/(1000.*40.08) 
      DLF = DCA*2.2 
      LFt = LF(ior)-DLF 
                                                                       
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
      tind = (1577.*exp(-0.0496*uebca))*10./ssalg(ior)
      if(tind.gt.stind(ior))goto 55 
   72 continue 
                                                                       
      if(lf(ior).lt.0.0)lf(ior) = 0.0 
      mue = 1.7e-5*lf(ior) 
      lgkca = (4.*sqrt(mue))/(1.+3.9*sqrt(mue)) 
      kca = pkca-lgkca 
      kca = 10**(-kca) 
                                                                       
      uebca = moca*moco3/kca 
      dca = 0.0 
      rhco3 = 0.0 
      rco3 = 0.0 
      if(uebca.lt.10.0)goto 55 
                                                                       
      dca = 0.0406*exp(0.0362*uebca) 
      dca = dca*0.001*40.08*ssalg(ior) 
      cuebca = (((uebca-10.)*kca)/moco3)*40.08*1000. 
      if(dca.gt.cuebca)dca = cuebca 
                                                                       
      r1 = -37.47*vph(ior)+360.67 
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
                                                                       
   10 vph(ior) = ph1 
      hconti = hconti-1. 
      if(hconti.lt.1.or.uebca.lt.20)goto 56 
      goto 72 
                                                                       
   56 mwt = mwt*1000. 
      pwt = pwt*1000. 
      stind(ior) = stind(ior)+tflie*1440. 
                                                                       
      delca = cat-ca(ior) 
      dellf = lft-lf(ior) 
      delmw = mwt-mw(ior)*1000. 
      mw(ior) = mw(ior)*1000. 
      pw(ior) = pw(ior)*1000. 
                                                                       
      if(cat.lt.0.0)cat = (ca(ior)/(ca(ior)+abs(delca)))*ca(ior)                      
      if(Lft.lt.0.0)Lft = (Lf(ior)/(Lf(ior)+abs(delLf)))*Lf(ior)                      
      if(mwt.lt.0.0)mwt = (mw(ior)/(mw(ior)+abs(delmw)))*mw(ior)                      

      vpht = vph(ior) 
      vph(ior) = vphv 
                                                                       
   enddo   ! Ende Schleife Ortspunkte 

      ca(anze+1) = cat 
      mw(anze+1) = mwt 
      pw(anze+1) = pwt 
      lf(anze+1) = lft 
      vph(anze+1) = vpht 
                                                                       
  end subroutine ph 
