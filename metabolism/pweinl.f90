      subroutine pweinl(mw,vph,lf,tempw,pw,ior) 
!                                                                       
!                                                                       
!     EIN PROGRAMM ZUR BERECHNUNG DES P-WERTES aus dem m-Wert und       
!     dem ph-Wert                                                       
!                                                                       
!                                                                       
!                                                                       
!                                                                       
!                                                                       
!     STAND:05.07.1993                                                  
!                                                                       
!                                                                       
!     AUTOR:VOLKER KIRCHESCH                                            
!                                                                       
!                                                                       
      REAL K1,K2,MUE,mw(1000),lgk1,lgk2,lgh,lgoh,lf(1000) 
      real vph(1000),tempw(1000),pw(1000) 
!                                                                       
!                                                                       
!                                                                       
!     Berechnung der absoluten Temperatur                               
!                                                                       
      itera = 0 
      abst = tempw(ior)+273.16 
!                                                                       
!     Berechnung der negativen Logarithmen der Dissoziationskonstanten b
!     in Abhaengigkeit von der abs. Temperatur f}r Konzentrationen in mo
!                                                                       
      pk1 = (17052./abst)+215.21*log10(abst)-0.12675*abst-545.56 
!                                                                       
      pk2 = (2902.39/abst)+0.02379*abst-6.498 
!                                                                       
      pkw = (4471.33/abst)+0.017053*abst-6.085 
!                                                                       
!     Einfluss der Ionenstaerke                                         
!                                                                       
      if(lf(ior).lt.0.0)lf(ior) = 0.0 
      mue = 1.7e-5*lf(ior) 
      lgk1 = sqrt(mue)/(1.+1.4*sqrt(mue)) 
      lgk2 = (2.*sqrt(mue))/(1.+1.4*sqrt(mue)) 
      hk = (0.5*sqrt(mue))/(1.+1.4*sqrt(mue)) 
!                                                                       
!     Berechnung der von Temperatur und Ionenstaerke abhaengigen Konstan
!                                                                       
      k1 = pk1-lgk1 
      k2 = pk2-lgk2 
      k1 = 10**(-k1) 
      k2 = 10**(-k2) 
!                                                                       
!                                                                       
      pw(ior) = 0.0 
      mw(ior) = mw(ior)*1.e-3 
      pw(ior) = pw(ior)*1.e-3 
    5 c = mw(ior)-pw(ior) 
!                                                                       
!     Schaetzer                                                         
!                                                                       
      ph1 = 0. 
      ph2 = 14. 
!                                                                       
!     Berechnung der Konzentrationen an H+ und OH-                      
!                                                                       
    1 ph0 = (ph1+ph2)/2. 
      poh = pkw-ph0 
      lgh = ph0 -hk 
      lgoh = poh-hk 
      h = 10**(-lgh) 
      oh = 10**(-lgoh) 
      y1 = oh-h 
!                                                                       
!     Berechnung des Aequivalenzfaktors eta                             
!                                                                       
      eta = (k1*h+2*k1*k2)/((h**2)+k1*h+k1*k2) 
      y2 = mw(ior)-c*eta 
      y = y2-y1 
!                                                                       
      delph = ph2-ph1 
      if(delph.lt.0.001)goto 10 
      if(y.lt.0.0)then 
      ph2 = ph0 
      goto 1 
      endif 
      ph1 = ph0 
      goto 1 
!                                                                       
!                                                                       
   10 continue 
      delph1 = vph(ior)-ph1 
      itera = itera+1 
      if(abs(delph1).lt.0.02.or.itera.ge.500)goto 20 
      if(delph1.lt.0.0)then 
      pw(ior) = pw(ior)-0.000005 
      goto 5 
      endif 
      if(delph1.gt.0.0)then 
      pw(ior) = pw(ior)+0.000005 
      goto 5 
      endif 
!                                                                       
   20 pw(ior) = pw(ior)*1000. 
      mw(ior) = mw(ior)*1000. 
!                                                                       
      return 
      END                                           
