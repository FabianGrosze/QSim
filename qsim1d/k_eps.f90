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

      subroutine k_eps(tempwz,tiefe,hvmitt,rau,dH2D,nkzs,tflie,dtemp,IDWe,WGe,mstr,Dz2D,ior,vo2z,hJO2      &                           
                       ,dO2o2D,vz1,vNH4z,vNO2z,vNO3z,hJPO4,hJSi,hJNH4,hJNO3,vx02,gelPz,Siz,iwied,uhrz      &
                       ,FluxT1,akiz,agrz,ablz,chlaz,hchlkz,hchlgz,hchlbz,hgesPz,hgesNz,orgCsd0,pl0,nl0     &
                       ,gesP,gesN,sedalk0,sedalb0,sedalg0,aki,abl,agr,Q_PK,Q_PB,Q_PG,hQ_NKz,hQ_NBz,hQ_NGz  &
                       ,hCChlkz,hCChlbz,hCChlgz,Q_NK,Q_NB,Q_NG,Qmx_NK,Qmx_NB,Qmx_NG,akbcm,abbcm,agbcm,fkm  &
                       ,Wlage,hWS,itags,monats,azStrs)      
                                                                       
!     Rahmenprogramm zur Berechnung des vertikalen                      
!     turbulenten Austauschkoeffizienten mittels k-Epsilon-Modell.      
                                                                       
                                                                       
!---------------------------------------------------------------------- 
      integer                             :: azStrs

      integer, Dimension(1000)            :: nkzs
      integer, Dimension(azStrs,1000)     :: IDWe
      real, Dimension(20)                 :: WGe
      real, Dimension(50)                 :: D, xU, dvdz 
      real, Dimension(1000)               :: tiefe, rau, Dz2D, dO2o2D, vx02, FluxT1, fkm
      real, Dimension(1000)               :: orgCsd0, pl0, nl0, gesP, gesN, sedalk0, sedalb0, sedalg0
      real, Dimension(1000)               :: aki, abl, agr, Q_PK, Q_PB,Q_PG, Q_NK, Q_NB, Q_NG,akbcm,abbcm,agbcm  
      real, Dimension(azStrs,1000)        :: Wlage, hWS, hvmitt, hJO2, hJPO4, hJNH4, hJNO3, hJSi
      real, Dimension(50,1000)            :: dtemp, tempwz, vo2z, vz1, vNH4z, vNO2z, vNO3z
      real, Dimension(50,1000)            :: gelPz, Siz, akiz, agrz, ablz, chlaz, Uvert  
      real, Dimension(azStrs,50,1000)     :: hchlkz, hchlgz, hchlbz, hgesPz, hgesNz,hQ_NKz, hQ_NBz, hQ_NGz 
      real, Dimension(azStrs,50,1000)     :: hCChlkz, hCChlbz, hCChlgz 
                                                                       
      DOUBLE PRECISION  Mu(50) 
      DOUBLE PRECISION kappa 
!     U(1..Janz<150) ist die Fließgeschwindigkeit                       
!     D(1..Janz<150) ist die Dichte                                     
!---------------------------------------------------------------------- 
      i_windP = 1
      call van_Veen(rau,tiefe,hvmitt,nkzs,dH2D,zf,xU,dvdz                                     &
                    ,WGe,IDWe,mstr,ior,hconus,hconub,Uvert,Wlage,hWS,i_windP,azStrs)                                                       
                                                                       
!...Berechnung der Dichte zu Beginn der Zeitschleife                    
      call Dichte(tempwz,nkzs,D,ior,itags,uhrz,fkm) 
!                                                                       
!                                                                       
      Jsteps = 1 
!      dt = 20.                                                         
      dt = 300. 
      n      = nkzs(ior) 
!      tief   = tiefe(ior)                                              
      h      = dH2D 
!      zf     = 0.1                                                     
      ub     = hconub 
      us     = hconus 
      kappa  = 0.399D00 
!                                                                       
!                                                                       
!                                                                       
!     Eigentliche Rechnung absolvieren:                                 


      call       Mixing (n, Jsteps, xU, D, DT, zf, ub, us, h, tiefe, Mu,tflie,dtemp,dH2D,nkzs,                &                         
                         tempwz,dvdz,Dz2D,ior,vo2z,hJO2,dO2o2D,vz1,vNH4z,vNO2z,vNO3z,hJPO4,hJSi,hJNH4,        &                                         
                         hJNO3,vx02,gelPz,Siz,akiz,agrz,ablz,chlaz,hchlkz,hchlgz,hchlbz,hgesPz,               & 
                         hgesNz,orgCsd0,pl0,nl0,gesP,gesN,sedalk0,sedalb0,sedalg0,aki,abl,agr,Q_PK,Q_PB,Q_PG, &
                         hQ_NKz,hQ_NBz,hQ_NGz,hCChlkz,hCChlbz,hCChlgz,Q_NK,Q_NB,Q_NG,Qmx_NK,Qmx_NB,Qmx_NG,    &
                         akbcm,abbcm,agbcm,mstr,iwied,uhrz,FluxT1,fkm,itags,monats,azStrs)                                    
                                                                       
      return 
      END                                           
! --- Ende keMain                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 
!---------------------------------------------------------------------- 
      SUBROUTINE Mixing (Janz, Jstp, xU, xD, xDT, xzf, xub, xus, xh, tiefe, xMu,tflie,dtemp,dH2D,nkzs,        &                          
                         tempwz,dvdz,Dz2D,ior,vo2z,hJO2,dO2o2D,vz1,vNH4z,vNO2z,vNO3z,hJPO4,hJSi,hJNH4,        &                                         
                         hJNO3,vx02,gelPz,Siz,akiz,agrz,ablz,chlaz,hchlkz,hchlgz,hchlbz,hgesPz,               & 
                         hgesNz,orgCsd0,pl0,nl0,gesP,gesN,sedalk0,sedalb0,sedalg0,aki,abl,agr,Q_PK,Q_PB,Q_PG, &
                         hQ_NKz,hQ_NBz,hQ_NGz,hCChlkz,hCChlbz,hCChlgz,Q_NK,Q_NB,Q_NG,Qmx_NK,Qmx_NB,Qmx_NG,    & 
                         akbcm,abbcm,agbcm,mstr,iwied,uhrz,FluxT1,fkm,itags, monats,azStrs)                                    
                                                                      


      integer                             :: azStrs 
      integer, Dimension(1000)            :: nkzs
      real, Dimension(50)                 :: xU, xD, Dz, hcdif1, hcdif2, dvdz
      real, Dimension(1000)               :: tiefe, Dz2D, betO2N, dO2o2D, vx02, FluxT1, fkm  
      real, Dimension(1000)               :: orgCsd0, pl0, nl0, gesP, gesN, sedalk0, sedalb0, sedalg0
      real, Dimension(1000)               :: aki, abl, agr, Q_PK, Q_PB,Q_PG, akbcm,abbcm,agbcm,Q_NK,Q_NB,Q_NG
      real, Dimension(50,1000)            :: dtemp, tempwz, vo2z, vz1, vNH4z, vNO2z, vNO3z 
      real, Dimension(azStrs,1000)        :: hJPO4, hJNH4, hJSi, hJO2, hJNO3    
      real, Dimension(50,1000)            :: gelPz, Siz, akiz, agrz, ablz, chlaz 
      real, Dimension(azStrs,50,1000)     :: hchlkz, hchlgz, hchlbz, hgesPz, hgesNz, hQ_NKz, hQ_NBz, hQ_NGz 
      real, Dimension(azStrs,50,1000)     :: hCChlkz, hCChlbz, hCChlgz 
                                                                       

     DOUBLE PRECISION  xMu(50) 
                                                                       
      DOUBLE PRECISION                                                  &
     & P(50),B(50),eps(50),avh(50),TKE(50),                             &
     & Ri(50),dUdz2(50),phim(50),phip(50),XN2(50),                      &
     & sigma(50),XKEmin(50),Xk2eps(50),epska(50)                        
!---------------------------------------------------------------------- 
      DATA appa    /0.399/ 
      DATA cmue    /0.101/ 
      DATA av_iw   /0.5E-10/ 
      DATA ce1     /1.5/ 
      DATA ce2     /2.0/ 
      DATA ce3     /-1.4/ 
      DATA g       /9.81/ 
      DATA tinny   /1.0E-20/ 
      DATA r0      /1000.0/ 
      DATA Sig_k   /1.0/ 
      DATA Sig_e   /1.31/ 
      DATA Ri_c    /0.50/ 
      DATA tkemin  /1.0E-07/ 
      DATA epsmin  /0.50E-10/ 
!                                                                       
      Janz1  = Janz-1 
      h1     = 1.0 / xh 
      h2     = h1*h1 
      dth2   = xDT*h2 
      cmu12  = sqrt(cmue) 
!                                                                       
!                                                                       
      fTKE =  0.50*cmue/sig_k 
      fEps =  0.50*cmue/sig_e 
!                                                                       
!      if(jini.eq.1.and.iwied.eq.1)goto 61                              
!     Felder initialisieren:                                            
!                                                                       
      DO 1   i = 2, Janz 
         B(i)      = 0.0 
         TKE(i)    = tkemin+3.1415*(xub**2)*(1.1-i*1.0/Janz) 
         eps(i)    = epsmin+40.0*((xub**3)/(tiefe(ior))/(1.0*i)) 
         Xk2eps(i) = TKE(i)**2/eps(i) 
         epska(i)  = eps(i)/TKE(i) 
         Xkemin(i) = (tkemin+TKE(i))**2/(epsmin+eps(i)) 
         avh(i)    = 0.5*cmue*(XKEmin(i-1) + XKEmin(i)) 
         sigma(i) = 1.0 
    1 END DO 
!         jini = 1                                                      
          ibegin = 0 
          ilauf = 0
!                                                                       
!====================================================================== 
!                                                                       
!     Beginn der Zeitschleife:                                          
   61 tflies = tflie*24. 
  666 continue 
!                                                                       
!     Innerhalb dieses Zuklus' können ggf. zusätzlich andere            
!     Subroutinen aufgerufen werden, die z.B. Durchfluß und             
!     Wasserstand berechnen oder auch die Wassergüte.                   
!                                                                       
      CALL HiFlds(Janz, B, xD, dUdz2,P, Ri, sigma, xU, Xk2eps, XN2,     &
     & cmue, ce1, ce2, ce3, xh, tinny, g, r0, tiefe, ior, mstr, xzf, xMu,&
     & fkm,uhrz,itags)                                                  
!      goto 171                                                         
!                                                                       
      CALL TKEnrg(Janz, avh, B, dUdz2, eps,                             &
     & xKEmin, P, Ri, TKE, Xk2eps,                                      &
     & fTKE, dth2, av_iw, Ri_c, xub, xus, cmu12, xDT, tkemin)           
!                                                                       
      CALL Dissi(Janz, avh, B, eps, xKEmin, P,Ri, TKE, Xk2eps,Epska,    &
     & fTKE, fEps, xzf, appa, ce1, ce2, ce3, dth2, av_iw,               &
     & Ri_c, xub, xus, cmu12, xDT, epsmin, xh, tiefe, ior)              
!                                                                       
      CALL Umspch(Janz, avh, eps,Sigma,                                 &
     & xKEmin, P, TKE, Xk2eps, Epska,                                   &
     & Xmu, epsmin, tkemin, cmue)                                       
!5                                                                      
      if(ibegin.ge.10)goto 171 
      ibegin = ibegin+1 
      hconb = sqrt(xMu(1)*dvdz(1)) 
      hcons = sqrt(xMu(janz)*dvdz(janz)) 
      xus = hcons 
      xub = hconb 
      if(ibegin.eq.10)xdt = 30. 
      goto 666 
!                                                                       
!..Berechnung vo DZ                                                     
  171 mn = janz 
      Dzmax = 0.0 
      do 33 i=1,janz 
      Dz(mn) = xMu(i)
      Dz(mn) = Dz(mn)*3600.  
      if(Dz(mn).gt.Dzmax)Dzmax = Dz(mn) 
      mn = mn-1 
!      hcdudz = (xU(2)-xU(1))/xh                                        
      hconb = sqrt(xMu(1)*dvdz(1)) 
      hcons = sqrt(xMu(janz)*dvdz(janz)) 
      xus = hcons 
      xub = hconb 
!                                                                       
   33 continue 
!                                                                       
! ###Ermittlung der Zeitschrittweite####

      hcdt_Dif = 1.00*(dH2D**2)/Dzmax 

      hcdt1 = 0.45*dH2D/orgCsd0(ior)
      hcdt2 = 0.45*dH2D/sedalk0(ior)
      hcdt3 = 0.45*dH2D/sedalb0(ior)
      hcdt4 = 0.45*dH2D/sedalg0(ior)

      hcdt = min(hcdt_Dif,hcdt1,hcdt2,hcdt3,hcdt4)     

      ilend = int((xdt/3600.)/hcdt) 
!      hcdt = xdt/3600.                                                 
  402 if(hcdt.gt.tflies)hcdt = tflies 
      tflies = tflies-hcdt 
!                                                                       
      do 34 nkz = 1,nkzs(ior) 
!      hcdif1(nkz) = ((Dz(nkz-1)*Dz(nkz))/(Dz(nkz-1)+Dz(nkz)))          
!     **hcdt/(dH2D**2)                                                  
!      hcdif2(nkz) = ((Dz(nkz)*Dz(nkz+1))/(Dz(nkz)+Dz(nkz+1)))          
!     **hcdt/(dH2D**2)                                                  
      hcdif1(nkz) = Dz(nkz)*hcdt/(dH2D**2) 
   34 continue 
                                                                       

      call       v_konz(tempwz,hcdif1,hcdt,dtemp,nkzs,xD,ior,vo2z,hJO2,dO2o2D,vz1,vNH4z,vNO2z,vNO3z                    &                                      
                       ,hJPO4,hJSi,hJNH4,hJNO3,vx02,itags,gelPz,Siz,akiz,agrz,ablz,chlaz,hchlkz                        &
                       ,hchlgz,hchlbz,hCChlkz,hCChlbz,hCChlgz,hgesPz,hgesNz,orgCsd0,pl0,nl0,gesP,gesN,sedalk0          &
                       ,sedalb0,sedalg0,tiefe,aki,abl,agr,Q_PK,Q_PB,Q_PG,hQ_NKz,hQ_NBz,hQ_NGz,Q_NK,Q_NB,Q_NG           &
                       ,Qmx_NK,Qmx_NB,Qmx_NG,akbcm,abbcm,agbcm,dH2D,tflie,mstr,uhrz,monats,azStrs)             


      if(tflies.eq.0.0)goto 999 
!      call Dichte(tempwz,nkzs,xD,dtemp)                                
      ilauf = ilauf+1 
      if(ilauf.ge.ilend)then 
      ilauf = 0 
      goto 666 
      endif 
      goto 402 
!                                                                       
!                                                                       
!     Ende der Zeitschleife                                             
!====================================================================== 
                                                                        
  999 continue 
!...Mittelwertbildung von Dz                                            
                                                                        
      sumDz = 0.0 
      sumH = 0.0 
      zzz = tiefe(ior) 
      i = janz 
      do 71 mn = 1,janz-1 
      Dz(mn) = Dz(mn)/3600. 
      zzz = zzz-0.25 
      Dz(mn+1) = Dz(mn+1)/3600. 
      sumDz = sumDz+((DZ(mn)+Dz(mn+1))/2.)*dH2D 
      sumH = sumH+dH2D 
      Dz(mn+1) = Dz(mn+1)*3600. 
      i = i-1 
   71 continue 
      Dz2D(ior) = sumDz/sumH 
                                                                       
!                                                                       
      RETURN 
!---------------------------------------------------------------------- 
      END                                           
!     Ende Mixing                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 
!---------------------------------------------------------------------- 
      SUBROUTINE HiFlds                                                 &
     & (nn, xB, xD, xdUdz2,xP, xRi, xsigma,xU, xXk2ep, xXN2,            &
     &  ccmue, cce1, cce2, cce3, hh, tinn, gg, rr, tiefe, ior, mstr, xzf,     &
     & xMu,fkm,uhrz,itags)                                              
!                                                                       
!     Routine zur Berechnung von Hilfsfeldern                           
!                                                                       
      real xD(50),xU(50),tiefe(1000),fkm(1000) 
      DOUBLE PRECISION                                                  &
     & xB(50), xdUdz2(50), xP(50),xRi(50), xMu(50),                     &
     & xsigma(50),xXk2ep(50),xXN2(50)                                   
!---------------------------------------------------------------------- 
      Bmax  = 0.0 
      DGMax = 0.0 
!---------------------------------------------------------------------- 
      DO 1   i = 1, nn - 1 
         xXN2(i) = (gg/rr)*(xD(i+1)-xD(i))/hh 
         Bmax    = ABS (xXN2(i)) 
         IF (Bmax.GT.DGMax) DGMax  =  Bmax 
         xdUdz2(i) = (abs((xU(i+1)-xU(i)))/hh)**2 
         If (xdUdz2(i).LT.tinn) xdUdz2(i) = tinn 
    1 END DO 
!---------------------------------------------------------------------- 
         xdUdz2(nn) =  xdUdz2(nn-1) 
         xXN2(nn) = 0.0 
!---------------------------------------------------------------------- 
      DO 2   i = 1, nn 
         xRi(i) =  - xXN2(i) / xdUdz2(i) 
         IF (xRi(i).LT.0) xsigma(i) = 1.0 
!..loeschen                                                             
!         IF (xRi(i).LT.0) xRi(i) = 0.0                                 
!                                                                       
         IF (xRi(i).GE.0) THEN 
             xsigma(i) = ( 1.0 + 3.333*xRi(i) )**1.5 
             xsigma(i) = xsigma(i)/sqrt(1.0+10.0*xRi(i)) 
         ENDIF 
!                                                                       
         xP(i)   =  ccmue * xXK2ep(i) * xdUdz2(i) 
         xB(i)   =  ccmue * xXK2ep(i) * xXN2(i) / xsigma(i) 
!                                                                       
         IF (xB(i).GT.0) THEN 
             Bmax = -(cce1-1.0)/(cce1*cce3-1.0)*xP(i) 
             xB(i) = Bmax*xB(i)/(Bmax+xB(i)) 
         ENDIF 
    2 END DO 
!---------------------------------------------------------------------- 
!...Mischungswegansatz                                                  
!      call verti_Dz(nn,ior,tiefe,hh,xdUdz2,xRi,xzf,xMu)                
!---------------------------------------------------------------------- 
      RETURN 
      END                                           
! --- Ende von HiFlds                                                   
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 
!---------------------------------------------------------------------- 
      SUBROUTINE TKEnrg                                                 &
     & (nn, xavh, xB, xdUdz2, xeps, xXKEmi,                             &
     &  xP, xRi, xTKE, xXk2ep,                                          &
     &  ffTKE, ddth2, aav_iw, RRi_c, uub, uus, ccmu12, DDT, ttkemi)     
!                                                                       
!     Berechnung der turbulenten kinetischen Energie (TKE)              
!                                                                       
      DOUBLE PRECISION                                                  &
     & xavh(50),xB(50), xdUdz2(50),xeps(50),                            &
     & xXKEmi(50),xP(50),xRi(50), xTKE(50),                             &
     & xXk2ep(50)                                                       
                                                                        
      DOUBLE PRECISION xphim(50), xphip(50) 
      DOUBLE PRECISION au(50), bu(50), cu(50), fu(50), wu(50) 
!---------------------------------------------------------------------- 
      Fact = ffTKE*ddth2 
      Ri_m = 0.0 
      cc   = 0.0 
      pp   = 0.0 
!---------------------------------------------------------------------- 
      DO 1  i = 2, nn 
         Ri_m    = 0.5 * (xRi(i-1)+xRi(i)) 
         xavh(i) = Fact*(xXKEmi(i-1)+xXKEmi(i))                         &
     &           + ddth2*aav_iw*(Ri_m**2)/((Ri_m**2)+(RRi_c**2))        

    1 END DO 
         xavh(1) = 2.0 * Fact * xXKEmi(1)                               &
     &           +ddth2*aav_iw*(xRi(1)**2)/((xRi(1)**2)+(RRi_c**2))     
!---------------------------------------------------------------------- 
      DO 2  i = 2, nn 
         IF (xP(i) + xB(i).GT.0) THEN 
             xphip(i) = xP(i)+xB(i) 
             xphim(i) = xeps(i) 
         ELSE 
             xphip(i) = xP(i) 
             xphim(i) = xeps(i)-xB(i) 
         ENDIF 
    2 END DO 
!---------------------------------------------------------------------- 
      DO 3  i = 2, nn - 1 
         au(i) = -xavh(i+1) 
         bu(i) = -xavh(i) 
         cu(i) =  1.0 - au(i) - bu(i) + xphim(i)*DDT/xTKE(i) 
         fu(i) =  xTKE(i)             + xphip(i)*DDT 
    3 END DO 
!---------------------------------------------------------------------- 
      au(1) = 0.0 
      cu(1) = 1.0 
      fu(1) = (uub**2) / ccmu12 
!                                                                       
      cc    = (uus**2) / ccmu12 
      pp    = cc/(xTKE(nn)+cc) 
      cu(nn) = 1.0 
      bu(nn) = pp - 1.0 
      fu(nn) = pp * cc 
!---------------------------------------------------------------------- 
      CALL Tridi(nn, au, bu, cu, fu, xTKE) 
!---------------------------------------------------------------------- 
      Do 4 i = 1, nn 
      IF (xTKE(i).LT.ttkemi) xTKE(i) = ttkemi 
    4 CONTINUE 
!---------------------------------------------------------------------- 
      RETURN 
      END                                           
!---  Ende von TKEnrg                                                   
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 
!---------------------------------------------------------------------- 
      SUBROUTINE Dissi                                                  &
     & (nn, xavh,xB,xeps,xXKEmi,xP, xRi,xTKE, xXk2ep, xEpska,           &
     &  ffTKE, ffEps, xxzf, pappa, cce1, cce2, cce3,                    &
     &  ddth2, aav_iw, RRi_c, uub, uus, ccmu12, DDT, eepsmi, hh,        &
     &  tiefe, ior)                                                     
!                                                                       
!     Berechnung der lokalen Dissipationsrate                           
!                                                                       
      DOUBLE PRECISION                                                  &
     & xavh(50),xB(50), xeps(50),                                       &
     & xXKEmi(50),xP(50),xRi(50),xTKE(50),                              &
     & xXk2ep(50),xEpska(50)                                            
      real tiefe(1000) 
!                                                                       
      DOUBLE PRECISION au(50), bu(50), cu(50), fu(50), wu(50) 
      DOUBLE PRECISION xphim(50), xphip(50), pe 
!---------------------------------------------------------------------- 
      Fact   = ffEps/ffTKE 
      h1     = 1.0 / hh 
      pe     = xxzf*0.5*h1 
      appape = pappa*xxzf/(pe*log(1.0+1.0/pe)) 
!---------------------------------------------------------------------- 
      DO 1   i = 2, nn 
         xavh(i) = Fact*xavh(i) 
    1 END DO 
!---------------------------------------------------------------------- 
      DO 2  i = 2, nn - 1 
         IF (xP(i)+cce3*xB(i).GT.0) THEN 
            xphip(i) = cce1*xepska(i) *(xP(i)+cce3*xB(i)) 
            xphim(i) = cce2*xeps(i)*xepska(i) 
         ELSE 
            xphip(i) = cce1*xepska(i)*xP(i) 
            xphim(i) = xepska(i)*(cce2*xeps(i)-cce1*cce3*xB(i)) 
         ENDIF 
    2 END DO 
!---------------------------------------------------------------------- 
      DO 3  i = 2, nn - 1 
         au(i) = -xavh(i+1) 
         bu(i) = -xavh(i) 
         cu(i) = 1.0 - au(i) - bu(i)+xphim(i)*DDT/xeps(i) 
         fu(i) = xeps(i)+xphip(i)*DDT 
    3 END DO 
!---------------------------------------------------------------------- 
      au(1) = 0.0 
      cu(1) = 1.0 
      fu(1)  = (uub**3)/appape 
!                                                                       
      bu(nn) = 0 
      cu(nn) = 1.0 
      cc     = xTKE(nn)*ccmu12 
      pp     = uus**2 
      Xkcmue = sqrt(cc**2 + pp**2)
      fu(nn) = 1.0 / pappa * Xkcmue * sqrt (Xkcmue)                     &
     &/(hh/10.0 + 0.07 * (nn-1) * hh * (1.0 - uus**2/Xkcmue))           
!---------------------------------------------------------------------- 
      CALL Tridi(nn, au, bu, cu, fu, xEps) 
!---------------------------------------------------------------------- 
      DO 4 i = 1, nn 
      IF (xeps(i).LT.eepsmi) xeps(i) = eepsmi 
    4 END DO 
!      xeps(nn) = (xTKE(nn)**(3./2.))/(0.18*tiefe(ior))                 
    5 continue 
!---------------------------------------------------------------------- 
      RETURN 
      END                                           
!---  Ende von Dissi                                                    
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 
!---------------------------------------------------------------------- 
      SUBROUTINE Umspch                                                 &
     & (nn, xavh, xeps, xSigma,xxKEmi, xP, xTKE, xXk2ep, xEpska, xXmu,  &
     &  eepsmi, ttkemi, ccmue)                                          
!                                                                       
      DOUBLE PRECISION                                                  &
     & xavh(50),xeps(50),xSigma(50),xxKEmi(50), xP(50), xTKE(50),       &
     & xXk2ep(50), xEpska(50), xXmu(50)                                 
!---------------------------------------------------------------------- 
      DO 1  i = 1, nn 
         xXk2ep(i) = xTKE(i)**2 / xeps(i) 
         xepska(i) = xeps(i) / xTKE(i) 
         xXkemi(i) = ((xTKE(i)+ttkemi)**2) / (xeps(i)+eepsmi) 
         xXmu(i)   = ccmue * xXkemi(i)/xSigma(i) 

    1 END DO 
!---------------------------------------------------------------------- 
      RETURN 
      END                                           
!---------------------------------------------------------------------- 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 
!---------------------------------------------------------------------- 
      SUBROUTINE Tridi(n, ax, bx, cx, fx, wx) 
!                                                                       
!     Lösung tri-diagonaler Gleichungsssyteme                           
!     mit dem Progonka-Verfahren (double sweep).                        
!                                                                       
!---------------------------------------------------------------------- 
      DOUBLE PRECISION ax(50), bx(50), cx(50), fx(50), wx(50) 
      DOUBLE PRECISION alfa(50), beta(50), ddd 
!---------------------------------------------------------------------- 
      alfa(n) = bx(n)/cx(n) 
      beta(n) = fx(n)/cx(n) 
      ddd     = 0.0 
!---------------------------------------------------------------------- 
      DO 1  i = 2, n-1 
         j = n + 1 - i 
         ddd     = 1/(cx(j)-ax(j)*alfa(j+1)) 
         alfa(j) = bx(j)*ddd 
         beta(j) = (fx(j)-ax(j)*beta(j+1))*ddd 
    1 END DO 
!---------------------------------------------------------------------- 
      beta(1)=(fx(1)-ax(1)*beta(2))/(cx(1)-ax(1)*alfa(2)) 
      wx(1)=beta(1) 
!---------------------------------------------------------------------- 
      DO 2   i = 2, n 
      wx(i)=beta(i)-alfa(i)*wx(i-1) 
    2 END DO 
!---------------------------------------------------------------------- 
      RETURN 
      END                                           
