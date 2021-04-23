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

      subroutine phstart(mws,pws,lfs,chlas,vkigrs,agbcms,akbcms,vphs,tempws,abbcms,antbls,mstr,mRB,azStrs)                                   


                                                                       
                                                                       
!#### EIN PROGRAMM ZUR BERECHNUNG DES PH-WERTES EINES GEWAESSERS        
!     aus dem m-Wert und der Kohlensaeuresumme #####                           
                                                                       
                                                                       
                                                                       
                                                                       
                                                                       
!     STAND:24.06.1987                                                  

                                                                       
                                                                       
!     AUTOR:VOLKER KIRCHESCH                                            
                                                                       
      integer                                        :: azStrs
                                                                       
      real                                           :: K1, K2, MUE, lgk1, lgk2, lgh, lgoh, moco2, mohco3, moco3 
      real                                           :: mgco2, mghco3, mgco3
      real, Dimension(azStrs,100)                    :: akbcms, agbcms, mws, pws, lfs, chlas, vkigrs, vphs, tempws
      real, Dimension(azStrs,100)                    :: antbls, abbcms


      Cagr = 0.48 
      Caki = 0.48 
      Cabl = 0.48 
!                                                                       
!     Berechnung der Kohlensaeuresumme in mol/l                         
!                                                                       
      mws(mstr,mRB) = mws(mstr,mRB)*1.e-3 
      pws(mstr,mRB) = pws(mstr,mRB)*1.e-3 
      c = mws(mstr,mRB)-pws(mstr,mRB) 
      chlaph = chlas(mstr,mRB) 
!                                                                       
!                                                                       
    5 continue 
!                                                                       
!     Schaetzer                                                         
!                                                                       
      ph1 = 0. 
      ph2 = 14. 
!                                                                       
!     Berechnung der absoluten Temperatur                               
!                                                                       
      abst = tempws(mstr,mRB)+273.16 
!                                                                       
!     Berechnung der negativen Logarithmen der Dissoziationskonstanten  
!     bei Ionenstaerke 0 mol/l                                          
!     in Abhaengigkeit von der abs. Temperatur                          
!                                                                       
      pk1 = (17052./abst)+215.21*log10(abst)-0.12675*abst-545.56 
!                                                                       
      pk2 = (2902.39/abst)+0.02379*abst-6.498 
!                                                                       
      pkw = (4471.33/abst)+0.017053*abst-6.085 
!                                                                       
!     Einfluss der Ionenstaerke                                         
!                                                                       
      mue = 1.7e-5*lfs(mstr,mRB) 
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
      y2 = mws(mstr,mRB)-c*eta 
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
!     Berechnung der Kohlenssaeureformen(nur einmal)                    
!                                                                       
   10 continue 
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
!                                                                       
!                                                                       
!                                                                       
!     CO2 Verbrauch durch Algenwachstum                                 
!                                                                       
!     Annahme: fuer die Bildung von 1 mg Algenbiomasse wird 1.77 mg CO2 
!              benoetigt                                                
!                                                                       
      if(chlaph.lt.0.001)goto 999 
      dchla = 2. 
      if(chlaph.lt.1.)dchla = chlaph 
      dalgki = dchla*vkigrs(mstr,mRB)/(1000.*akbcms(mstr,mRB)*Caki) 
      dalggr = dchla*(1.-vkigrs(mstr,mRB)-antbls(mstr,mRB))             &
     &/(1000.*agbcms(mstr,mRB)*Cagr)                                    
      dalgbl = dchla*antbls(mstr,mRB)/(1000.*abbcms(mstr,mRB)*Cabl) 
      co2alg = dalgki*1.30+dalggr*1.77+dalgbl*1.77 
      alhco3 = 0.0 
      alco2 = co2alg 
      if(alco2.gt.mgco2)alco2 = mgco2 
      alhco3 = (co2alg-alco2)*2.77 
!                                                                       
      chlaph = chlaph-dchla 
!                                                                       
!                                                                       
      mgco2 = mgco2-alco2 
      mghco3 = mghco3-alhco3 
      if(mgco2.lt.0.0)mgco2 = 0.0 
      if(mghco3.lt.0.0)mghco3 = 0.0 
!                                                                       
      mohco3 = mghco3/61.02/1000. 
      moco3 = mgco3/(60.009*1000.) 
      moco2 = mgco2/44.0/1000. 
!                                                                       
!                                                                       
      mws(mstr,mRB) = mohco3+2*moco3+oh-h 
      pws(mstr,mRB) = moco2*(-1.)+moco3+oh-h 
!                                                                       
      c = mws(mstr,mRB)-pws(mstr,mRB) 
      goto 5 
!                                                                       
  999 mws(mstr,mRB) = mws(mstr,mRB)*1000. 
      pws(mstr,mRB) = pws(mstr,mRB)*1000. 
      vphs(mstr,mRB) = ph1 
!                                                                       
!                                                                       
      return 
      END                                           
