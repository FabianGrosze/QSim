!---------------------------------------------------------------------------------------
!
!   QSim - Programm zur Simulation der Wasserqualit�t
!
!   Copyright (C) 2020 Bundesanstalt f�r Gew�sserkunde, Koblenz, Deutschland, http://www.bafg.de
!
!   Dieses Programm ist freie Software. Sie k�nnen es unter den Bedingungen der 
!   GNU General Public License, Version 3,
!   wie von der Free Software Foundation ver�ffentlicht, weitergeben und/oder modifizieren. 
!   Die Ver�ffentlichung dieses Programms erfolgt in der Hoffnung, da� es Ihnen von Nutzen sein wird, 
!   aber OHNE IRGENDEINE GARANTIE, sogar ohne die implizite Garantie der MARKTREIFE oder der VERWENDBARKEIT F�R EINEN BESTIMMTEN ZWECK. 
!   Details finden Sie in der GNU General Public License.
!   Sie sollten ein Exemplar der GNU General Public License zusammen mit diesem Programm erhalten haben. 
!   Falls nicht, siehe http://www.gnu.org/licenses/.  
!   
!	Programmiert von:
!	1979 bis 2018 Volker Kirchesch
!	seit 2011 Jens Wyrwa, Wyrwa@bafg.de
!
!---------------------------------------------------------------------------------------

      SUBROUTINE albenth(SCHWI,TFLIE,TEMPW,TIEFE,VMITT,VNO3,VNH4,GELP,albewg,alberg,elen,flae,ior,anze         &
                         ,aggmax,agksn,agksp,si,akksn,akksp,akkssi,akgmax,albewk,alberk,abegm2,abekm2          &
                         ,vabfl,cmatgr,cmatki,akchl,agchl,extk,ilang,mstr                                      &                                                   
                         ,kontroll ,jjj ) !!wy  
!                                                                       
!                                                                       
!     UNTERPROGRAMM ZUR BERECHNUNG DES DES EINFLUSSES BENTHISCHER       
!     ALGEN                                                             
!                                                                       
!                                                                       
!                                                                       
!     AUTOR :VOLKER KIRCHESCH                                           
!                                                                       
!     STAND :20.02.1996                                                 
!                                                                       
!                                                                       
!                                                                       
      logical kontroll !!wy
      integer jjj !!wy                                                                       
      integer                   :: anze

      real                      :: LNQ, kx, Igrenz, KxI
      real, Dimension(1000)     :: tempw, vno3, vnh4, gelp, albewg, alberg, vmitt, tiefe, elen, si, albewk, alberk
      real, Dimension(1000)     :: abegm2, abekm2, vabfl, flae, extk, schwi, cmatgr, cmatki   
                                                                       
!      open(unit=33,file='albenth.tst')                                 
                                                                       
      if(ilang.eq.0)then
        else                                                                
!                                                                       
!     Kx        - Beruecksichtigt, dass mit zunehmender Schichtdicke    
!                 die unteren Schichten weniger Licht bekommen          
!                                                                       
!     saetbk    - Saettigungslichtst�rke f�r Kieselalgen                
!     saetbg    - Saettigungslichtst�rke f�r Gr�nalgen                  
!     (Werte wurden dem Modell AQUATOX entnommen, Dim.: mueE/(m2*s))    
!     roPeri    - Dichte des Periphytons in g/m3 (s. Uhlmann)           
!                                                                       
      Cagr = 0.48 
      Caki = 0.48 
!                                                                       
!...Temperaturabh�ngigkeit des Chla:C-Verhaeltnisses                    
!...ak(g)chl gilt fuer 20�C                                             
      fTChlC = 0.398*exp(0.0465*Tempw(ior)) 
!                                                                       
      ChlCK = akchl/(1000.*Caki) 
      ChlCK = ChlCK*fTChlC 
      ChlCG = agchl/(1000.*Cagr) 
      ChlCG = ChlCG*fTChlC 
!....Umrechnung von muegC*mgChla-1*s-1 in mgC*mgChla-1*d-1 (*3.6*24)    
!      akgmax = akPmax*3.6*24.*ChlCK                                    
!      aggmax = agPmax*3.6*24.*ChlCG                                    
!                                                                       
      saetbk = 147. 
      saetbg = 176. 
      roPeri = 1030000. 
                                                                       
      do ior=1,anze+1 ! Beginn Knotenschleife 
!......schwi*4.2 - Umrechnung von cal/(cm2*h) in J/(cm2*h)              
!      OBFLI = 8.2051*(schwi(ior)*4.2)                                  
!.vorl�ufig bis zur endg�ltigen Kl�rung                                 
      OBFLI = 5.846*(schwi(ior)*4.2) 
      IF(OBFLI.eq.0.0)obfli = 0.0001 
!                                                                       
      cmatgr(ior) = 0.0 
      cmatki(ior) = 0.0 
!                                                                       
      alberg(ior) = 0.0 
      alberk(ior) = 0.0 
      albewg(ior) = 0.0 
      albewk(ior) = 0.0 
      if(abegm2(ior)==0.0.and.abekm2(ior)==0.0)cycle 
!                                                                       
      abegr = abegm2(ior) 
      abeki = abekm2(ior) 
!                                                                       
!                                                                       
!     Kx        - Beruecksichtigt, dass mit zunehmender Schichtdicke    
!                 die unteren Schichten weniger Licht bekommen          
!     zPeri     - Dicke des Periphytonrasens [m]                        
!                                                                       
      zPeri = (abegr+abeki)/roPeri 
      Kx = 0.015*roPeri*zPeri/2. 
!                                                                       
!                                                                       
!     Temperaturabhaengigkeit der Wachstumsrate fuer Gruenalgen         
!                                                                       
      TMAX = 45.0 
      TOPT = 27.0 
!                                                                       
      LNQ = 0.61519 
      W = LNQ*(TMAX-TOPT) 
      X = (W**2*(1+SQRT(1+40/W))**2)/400. 
      FTA = ((TMAX-TEMPW(ior))/(TMAX-TOPT))**X 
      FTAg = FTA*EXP(X*(1-((TMAX-TEMPW(ior))/(TMAX-TOPT)))) 
!                                                                       
!     Temperaturabhaengigkeit der Wachstumsrate fuer Kieselalgen        
!                                                                       
      TMAX = 31. 
      TOPT = 20. 
      if(tempw(ior).ge.tmax)then 
      ftak = 0.0 
      goto 501 
      endif 
      W = LNQ*(TMAX-TOPT) 
      X = (W**2*(1+SQRT(1+40/W))**2)/400. 
      FTA = ((TMAX-TEMPW(ior))/(TMAX-TOPT))**X 
      FTAk = FTA*EXP(X*(1-((TMAX-TEMPW(ior))/(TMAX-TOPT)))) 

!                                                                       
!     Berechnung des vertikalen Lichtklimas                             
!                                                                       
  501 IF(OBFLI.le.0.0001)THEN 
      ALGIP1 = 0.0 
      fik = 0.0 
      fig = 0.0 
      flicht = 1. 
      GOTO 13 
      ENDIF 
!                                                                       
!                                                                       
      flicht = 1. 
      algip1 = obfli*exp(-extk(ior)*tiefe(ior)) 
!                                                                       
      Igrenz = algip1*0.01 
      KxI = log(algip1)-log(Igrenz) 
      zPeriI = KxI/(0.015*roPeri) 
      zPeri = (abegr+abeki)/roPeri 
      if(zPeri.gt.zPeriI)goto 288 
      Kx = 0.015*roPeri*zPeri/2. 
      goto 289 
  288 continue 
      Kx = 0.015*roPeri*zPeriI/2. 
      flicht = zPeriI/zPeri 
!                                                                       
!                                                                       
  289 algip1 = algip1*exp(-Kx) 
      fik = (algip1/saetbk)*exp(1.-(algip1/saetbk)) 
      if(algip1.gt.saetbk)fik = 1. 
      fig = (algip1/saetbg)*exp(1.-(algip1/saetbg)) 
      if(algip1.gt.saetbg)fig = 1. 
!                                                                       
!                                                                       
!     Abhaengigkeit der Wachstumsrate vom Naehrstoffangebot             
!                                                                       
      FNG1 = (VNO3(ior)+VNH4(ior))/(agksn+VNO3(ior)+VNH4(ior)) 
      FNG2 = gelp(ior)/(agksp+gelp(ior)) 
      FNG = FNG1 
      if(FNG2.lt.FNG)FNG = FNG2 
!                                                                       
      FNK1 = (VNO3(ior)+VNH4(ior))/(akksn+VNO3(ior)+VNH4(ior)) 
      FNK2 = gelp(ior)/(akksp+gelp(ior)) 
      FNK3 = si(ior)/(akkssi+si(ior)) 
      FNK = FNK1 
      if(FNK2.lt.FNK)FNK = FNK2 
      if(FNK3.lt.FNK)FNK = FNK3 
!                                                                       
!     Limitation des Wachstums durch die Fliessgeschwindigkeit          
!     (s. Modell AQUATOX)                                               
                                                                        
      vkoff1 = 0.2 
      vkoff2 = 0.057 
      veloci = abs(vmitt(ior))*100. 
      vlim = vkoff1+((vkoff2*veloci)/(1.+(vkoff2*veloci))) 
      if(vlim.gt.1.)vlim = 1. 
!                                                                       
!     Periphytonmat                                                     
!                                                                       
   13 continue 
!      Vol = flae(ior)*elen(ior)                                        
!      CCPeri = 80.                                                     
!      hconC = (CCPeri-(abegr+abeki))/CCPeri                            
!      cmatgr(ior) = hconC*(abs(vabfl(ior))/Vol)*abegr                  
!      cmatki(ior) = hconC*(vabfl(ior)/Vol)*abeki                       
!      cmatgr(ior) = cmatgr(ior)*tflie*86400.                           
!      cmatki(ior) = cmatki(ior)*tflie*86400.                           
!                                                                       
      aggrow = aggmax*fng*ftag*fig*vlim 
      akgrow = akgmax*fnk*ftak*fik*vlim 
!                                                                       
      abegrt = abegr*flicht*exp(aggrow*tflie) 
      abekit = abeki*flicht*exp(akgrow*tflie) 
!                                                                       
      albewg(ior) = abegrt-abegr 
      albewk(ior) = abekit-abeki 
!                                                                       
!                                                                       
!     BERECHNUNG DER RESPIRATION                                        
!                                                                       
      b1 = 1.7 
      b2 = 0.187 
      frespL = 0.2 
      respG = 0.085 
!....Wachstumsrate ohne Temperaturber�cksichtigung                      
      aggrow = aggmax*fng*fig*vlim 
      akgrow = akgmax*fnk*fik*vlim 
      abgre = respG+aggrow*frespL 
      abkre = respG+akgrow*frespL 
!                                                                       
!                                                                       
      alberg(ior) = abegr*(1.-(exp(-abgre*tflie))) 
      alberk(ior) = abeki*(1.-(exp(-abkre*tflie))) 
      alberg(ior) = alberg(ior)/(1.+exp(b1-b2*tempw(ior))) 
      alberk(ior) = alberk(ior)/(1.+exp(b1-b2*tempw(ior))) 
!                                                                       
!                                                                       
      abegm2(ior) = abegrt-alberg(ior)-(cmatgr(ior)*tiefe(ior)) 
      abekm2(ior) = abekit-alberk(ior)-(cmatki(ior)*tiefe(ior)) 
!                                                                       
!     Umrechnung auf mg/l                                               
!                                                                       
      albewg(ior) = albewg(ior)/tiefe(ior) 
      albewk(ior) = albewk(ior)/tiefe(ior) 
      alberg(ior) = alberg(ior)/tiefe(ior) 
      alberk(ior) = alberk(ior)/tiefe(ior) 
      cmatgr(ior) = albewg(ior)
      cmatki(ior) = albewk(ior)



!                                                                       
      enddo    ! Ende Knotenschleife 
        endif

  RETURN 
  END                                           
