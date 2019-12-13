
      subroutine pwert(mws,vphs,lfs,tempws,pws,mRB,mstr) !!wy azStrs nicht mehr benötigt
!                                                                       
!     EIN PROGRAMM ZUR BERECHNUNG DES P-WERTES aus dem m-Wert und       
!     dem ph-Wert                                                       
!                                                                       
!     STAND:05.07.1993                                                  
!                                                                       
!     AUTOR:VOLKER KIRCHESCH                                            
!                                                                       
!             
      implicit none !!wy
      real :: mws, vphs, lfs, tempws, pws  !!wy Übergabe Einzelwerte, nicht Felder
      REAL :: K1,K2,MUE,lgk1,lgk2,lgh,lgoh, abst, pk1, pk2, pkw, hk, c, ph0, ph1, ph2, delph, delph1, delph2
      REAL :: poh, h, oh, y, y1, y2, eta
      integer :: mRB, mstr, itera
!                                                                       
!     Berechnung der absoluten Temperatur                               
!                                                                       
      abst = tempws +273.16 
      itera = 0 
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
      mue = 1.7e-5*lfs 
      if(lfs <0.0)mue = 0.0 

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
      pws  = 0.0 
      mws  = mws *1.e-3 
      pws  = pws *1.e-3 
    5 c = mws -pws  
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
      y2 = mws -c*eta 
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
      delph1 = vphs -ph1 
      itera = itera+1 
      if(abs(delph1).lt.0.02.or.itera.gt.500) goto 20 
      if(delph1.lt.0.0)then 
      pws  = pws -0.000005 
      goto 5 
      endif 
      if(delph1.gt.0.0)then 
      pws  = pws +0.000005 
      goto 5 
      endif 
!                                                                       
   20 pws  = pws *1000. 
      mws  = mws *1000. 
!                                                                       
      return 
      END                                           
