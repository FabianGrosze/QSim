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

      SUBROUTINE dreissen(zdrei,zdreis,tempw,flae,elen,anze             &
     &,ior,volfdr,akbcm,agbcm,aki,agr,algdrk,algdrg                     &
     &,tflie,ro2dr,lboem,bsohlm,ss,vo2,ssdr,drfaek                      &
     &,drfaeg,drfaes,gewdr,dlarvn,itags,monats,jahrs                    &
     &,lait1,laim1,laid1,ilang                                          &
     &,resdr,exdrvg,exdrvk,ssalg,drpfec                                 &
     &,abl,exdrvb,abbcm,algdrb,drfaeb                                   &
     &,idras,drmas,drakr,drbar,drmor,ffood,coroI,coroIs                 &
     &,CHNF,drHNF,HNFdra,dlmax,dlmaxs,gwdmax                            &
     &,sgwmue,fkm,FoptDe,mstr,azStr             &                                                   
     &,kontroll ,jjj ) !!wy
!                                                                       
!                                                                       
!     EIN PROGRAMM zu Berechnung der Entwicklung von Dreissena polymorph
!     und deren Einfluss auf das Phytoplankton                          
!                                                                       
!                                                                       
!                                                                       
!     AUTOR : VOLKER KIRCHESCH                                          
!                                                                       
!                                                                       
!                                                                       
!                                                                       
!     STAND : 15.01.1996                                                
!                                                                       
!                                                                       
      logical kontroll !!wy
      integer jjj !!wy                                                                       
      real elen(1000),tempw(1000),flae(1000),zdrei(1000,4) 
      real zdreis(1000,4),volfdr(1000) 
      real akbcm(1000),agbcm(1000),fkmR(1000),fkm(1000) 
      real aki(1000),agr(1000),algdrk(1000),algdrg(1000) 
      real ro2dr(1000),vo2(1000),ssdr(1000),idras(1000,2) 
      real drmas(1000,2),drakr(1000,2),drbar(1000,2),drmor(1000,2) 
      real lboem(1000),bsohlm(1000),ss(1000),dlarvd(1000) 
      real drfaek(1000),drfaeg(1000),drfaes(1000) 
      real gewdr(1000,4),idr,dlarvn(1000),resdr(1000),exdrvg(1000) 
      real dlmax(1000),dlmaxs(1000),gwdmax(1000),exdrvk(1000) 
      real adrg(4),adrk(4),drss(4),drfeck(4),drfecg(4),drfecs(4) 
      real dchlg(4),dchlk(4),filaki(4),filagr(4) 
      real adrb(4),drfecb(4),filabl(4),dchlb(4) 
      real filss(4),vofkop(4),filHNF(4),HNFdra(1000) 
      real ssalg(1000),drpfec(1000),sgwmue(1000),ffood(1000) 
      real coroI(1000),coroIs(1000) 
      real klmor,klmorg,hcondb(4),hconds(4) 
      real CHNF(1000),drHNF(1000),drfaeb(1000) 
      real abl(1000),exdrvb(1000),abbcm(1000),algdrb(1000), drrt, drft 
      integer anze, azStr
      save jahr_tst1, drrt, drft, stdpla, itime_hoch 
!                                                                       
!                                                                       
!      open(unit=78,file='dreissen.tst')                                
!                                                                       
                                                                        
      if(ilang.eq.0)goto 999 
!                                                                       
!....     exdrvg(k,b)	-	Anteil der Algenbiomasse die exkretiert wird    
!         wrid nur bei den N�hrstoffen ber�cksichtigt.                  
!                                                                       
      zqz10 = 3.1 
      ztmax = 31. 
      ztopt = 28. 
      do2krit = 2.0 
!      drmax = 1000.                                                    

      dmorG = 0.0
!                                                                       
      drCgeh = 0.38 
      Cagr = 0.48 
      Caki = 0.48 
      Cabl = 0.48 
      fweib = 0.5 
      fgesund = 0.25 
      FoptD = FoptDe 
      if(FoptD.eq.0.0)FoptD = 1.2 
      F_lim = 0.01 ! Kohlenstoffgehalt, bei dem Dreissena die Futteraufnahme einstellt
!                                                                       
      tdpla = 22. 
!      tdpla = 2.                                                       
      klmorg = 8.26 
      klmor = klmorg/tdpla 
!                                                                       
      flai = 0.52 
      nndr = 2 
                                                                       
                                                                        
                                                                       
!     Parameter                                                         
!                                                                       
!     pgr   - Preferenzfaktor fuer Gruenalgen                           
!     pki   - Preferenzfaktor fuer Kieselalgen                          
!     pbl   - Preferenzfaktor fuer Blaualgen                            
!     qfec  - Anteil des Futters, dass als Faeces wieder ausgeschieden  
!             wird                                                      
!     rres  - Grundrespirationsrate 1/d                                 
!     qres  - aktive Respirationsrate (abhaengig von der Assimilation) 1
!     dmorg - natuerliche Mortalitaetsrate                              
!     dmorma- maximale Mortalitaetsrate bei Sauerstoffschwund           
!                                                                       
      pgr = 1.0 
      pki = 1.0 
      pbl = 0.5     ! ist 0.2
      pss = 0.1 
      qfec = 0.25 
      rres0 = 0.0015 
      qres = 0.29 

      
      if(azStr==1)then
        drrt = drrt+tflie 
        itime_hoch = 1          ! Schalter f�r Hochz�hlen der div. Zeiten
      endif      
                                                                
      do 111 ior=1,anze 
                                                                        
      do ndr=1,nndr 
        if(zdrei(ior,ndr).lt.0.0)zdrei(ior,ndr) = 0.0 
        if(zdreis(ior,ndr).lt.0.0)zdreis(ior,ndr) = 0.0 
        if(gewdr(ior,ndr).lt.0.0)gewdr(ior,ndr) = 0.0 
      enddo 
!                                                                       
!Einfluss von Corophium auf die Ingest.- und Filtrierrate               
!                                                                       
      if(coroI(ior).eq.0.0.and.coroIs(ior).eq.0.0)then 
        fco = 1. 
        fcos = 1. 
        fcom = 1. 
          else
           fco = (90000.-(coroI(ior)-10000.))/90000. 
           if(fco.gt.1.)fco = 1. 
           if(fco.lt.0.0)fco = 0.0 
                                                                       
           fcos = (90000.-(coroIs(ior)-10000.))/90000. 
           if(fcos.gt.1.)fcos = 1. 
           if(fcos.lt.0.0)fcos = 0.0 
                                                                       
           hconc1 = 2.*lboem(ior)*elen(ior)*coroI(ior) 
           hconc2 = bsohlm(ior)*elen(ior)*coroIs(ior) 
           fcom = (hconc1*fco+hconc2*fcos)/(hconc1+hconc2) 
       endif

      if(elen(ior).eq.0.0)then 
        ro2dr(ior) = 0.0 
        algdrg(ior) = 0.0 
        algdrk(ior) = 0.0 
        volfdr(ior) = 0.0 
        resdr(ior) = 0.0 
        exdrvz = 0.0 
        exdrvg(ior) = 0.0 
        exdrvk(ior) = 0.0 
        goto 111 
      endif 
!                                                                       
      exdrvz = 0.0 
      exdrvg(ior) = 0.0 
      exdrvk(ior) = 0.0 
      exdrvb(ior) = 0.0 
      resdr(ior) = 0.0 
      ro2dr(ior) = 0.0 
!                                                                       
!     Berechnung der filtrierbaren Futterkonzentration                  
!                                                                       
      ssorg = SS(ior)*0.1 
      ssc = ssorg*0.4 
      ssorg = 0.0 
!                                                                       
      agrc = agr(ior)*0.48 
      akic = aki(ior)*0.48 
      ablc = abl(ior)*0.48 
!.....max. Dreissena-Dichte                                             
!      drpr = zdreis(ior)/(bsohlm(ior)*elen(ior))                       
!                                                                       
      do 214 ndr=1,nndr 
      zdrei(ior,ndr) = zdrei(ior,ndr)*(2.*lboem(ior)*elen(ior)) 
      zdreis(ior,ndr) = zdreis(ior,ndr)*(bsohlm(ior)*elen(ior)) 
      Yc = zdrei(ior,ndr) 
      Ycs = zdreis(ior,ndr) 

                                                                       
      Fgr = pgr*agrc 
      Fki = pki*akic 
      Fbl = pbl*ablc 
      Fss = 0.0 
!                                                                       
      if((aki(ior)+agr(ior)+abl(ior)).eq.0.0)then 
      hconvk = 0.0 
      hconvg = 0.0 
      hconvb = 0.0 
      goto 568 
      endif 
      hconvk = aki(ior)*pki/(aki(ior)*pki+agr(ior)*pgr+abl(ior)*pbl) 
      hconvg = agr(ior)*pgr/(aki(ior)*pki+agr(ior)*pgr+abl(ior)*pbl) 
      hconvb = abl(ior)*pgr/(aki(ior)*pki+agr(ior)*pgr+abl(ior)*pbl) 
  568 hconvs = 0.0 
!                                                                       
!     Berechnung der Aufnahmerate gC/m2                                 
!                                                                       
      food = fgr+fki+fbl+fss 
      hconf = food/FoptD 
      if(food.gt.FoptD)hconf = 1. 
      if(food<=F_lim)hconf = 0.0 
      ffood(ior) = hconf 
      if(gewdr(ior,ndr).eq.0.0)then 
      idr = 0.0 
      goto 555 
      endif 
!Walz                                                                   
!      idr = (0.1105*gewdr(ior,ndr)**(-0.213))                          
!     **exp(-0.00605*(20.0-tempw(ior))**2)                              
!modifizierte Walz                                                      
!      idr = (0.295*gewdr(ior,ndr)**(-0.636))                           
!     **exp(-0.00605*(20.0-tempw(ior))**2)                              
!nach Schneider                                                         
!      idr = (0.1271*gewdr(ior,ndr)**(-0.39))                           
!     **exp(-0.00605*(20.0-tempw(ior))**2)                              
!                                                                       
      idr = (0.249*gewdr(ior,ndr)**(-0.615))                            &
     &*exp(-0.00605*(20.0-tempw(ior))**2)                               
!                                                                       
  555 continue 
!                                                                       
!Filtrierrate                                                           
      if(gewdr(ior,ndr).eq.0.0)then 
      FR = 0.0 
      goto 560 
      endif 
      FR = 9.24*gewdr(ior,ndr)**(-0.392) 
      FR = FR*3.267*exp(-0.037*SSalg(ior)) 
      FR = FR*exp(-0.00605*(20.-tempw(ior))**2) 
!Umrechnung in m3/g*d                                                   
      FR = (FR*24./1000.)*fcom 
                                                                        
!                                                                       
  560 continue 
      up = idr*hconf*fco*Yc 
      ups = idr*hconf*fcos*Ycs 
      idr = idr*hconf*fcom 
!                                                                       
      idras(ior,ndr) = idr 
!                                                                       
!                                                                       
!     Berechnung der Assimilationsrate                                  
!                                                                       
      qfec = 0.315*exp(0.88*hconf) 
      assr = (1.-qfec)*up 
      assrs = (1.-qfec)*ups 
!                                                                       
!     Berechnung der Exkretionsrate                                     
!                                                                       
      exdr = 0.064*(1.-qfec)*up 
      exdrs = 0.064*(1.-qfec)*ups 
!                                                                       
!     Temperaturabhaengigkeit                                           
!                                                                       
      if(tempw(ior).ge.ztmax)then 
      hcont = 0.0 
      goto 77 
      endif 
      w = log(zqz10)*(ztmax-ztopt) 
      x = ((w/20.)*(1.+sqrt(1.+40./w)))**2 
      hcont = (((ztmax-tempw(ior))/(ztmax-ztopt))*exp(1.-               &
     &(ztmax-tempw(ior))/(ztmax-ztopt)))**x                             
!                                                                       
!     Berechnung der Respirationsrate in gC/(m2Gewaesserbod.*d)         
!                                                                       
   77 if(gewdr(ior,ndr).eq.0.0)then 
      rres = 0.0 
      goto 556 
      endif 
      rres = rres0*gewdr(ior,ndr)**(-0.25) 
  556 respc = rres*hcont*Yc+qres*assr 
      respcs = rres*hcont*Ycs+qres*assrs 
                                                                       
                                                                       
!     zeitl. Aenderung der Dreissena-Biomasse in gC                     
!                                                                       
      dYc = (assr-respc-exdr)*tflie 
      Yc = Yc+dYc 
      dYcs = (assrs-respcs-exdrs)*tflie 
      Ycs = Ycs+dYcs 
      zdrei(ior,ndr) = Yc 
      zdreis(ior,ndr) = Ycs 
                                                                       
      FH2oVol = FR*Yc+FR*Ycs 
      FH2oVOL = FH2oVOL*tflie 
!                                                                       
!     Gewichts�nderung einer Muschel                                    
!                                                                       
      drmue = idr-(idr*qfec)-(idr*(1.-qfec)*0.064)-((1.-qfec)*qres*idr)-rres*hcont                                              
      dgewdr = gewdr(ior,ndr)*drmue*tflie 
      gewdr(ior,ndr) = gewdr(ior,ndr)+dgewdr 
      drmas(ior,ndr) = drmue 
      drakr(ior,ndr) = (1.-qfec)*qres*idr 
      drbar(ior,ndr) = rres*hcont 
!                                                                       
!                                                                       
!     Umrechnung in mg/l                                                
!.....Annahme: 1 mg respirierte Biomasse verbraucht 5.59 mg O2          
!     (Schneider)                                                       
!                                                                       
      vol = flae(ior)*elen(ior) 
      if(vol.eq.0.0)goto 999 
      rescm3 = (respc+respcs)/vol 
      excm3 =(exdr+exdrs)/vol 
!                                                                       
      respbio = rescm3/drCgeh ! Respiration Umrechnung von C in Biomasse 
      resdr(ior) = resdr(ior)+respbio*tflie 
                                                                        
      exdrvz = exdrvz+excm3*tflie 
      exdrvg(ior) = exdrvz*hconvg/0.48 
      exdrvk(ior) = exdrvz*hconvk/0.48 
      exdrvb(ior) = exdrvz*hconvb/0.48 
!                                                                       
      respo2 = respbio*5.59 
      ro2dr(ior) = ro2dr(ior)+respo2*tflie 
!                                                                       
      upt = up*tflie 
      upst = ups*tflie 
      uptm3 = (upt+upst)/vol 
!                                                                       
!                                                                       
      adrg(ndr) = uptm3*hconvg/0.48 
      adrk(ndr) = uptm3*hconvk/0.48 
      adrb(ndr) = uptm3*hconvb/0.48 
!      drss(ndr) = uptm3*hconvs/0.4                                     
      drfecg(ndr) = qfec*adrg(ndr) 
      drfeck(ndr) = qfec*adrk(ndr) 
      drfecb(ndr) = qfec*adrb(ndr) 
      drfecs(ndr) = qfec*drss(ndr) 
      vofkop(ndr) = (fh2ovol/vol)*100. 
!                                                                       
      Filaki(ndr) = aki(ior)*pki*fh2ovol/vol 
      Filagr(ndr) = agr(ior)*pgr*fh2ovol/vol 
      Filabl(ndr) = abl(ior)*pbl*fh2ovol/vol 
      filss(ndr) = ss(ior)*fh2ovol/vol 
!                                                                       
      if(adrk(ndr).gt.0.0.and.adrk(ndr).gt.Filaki(ndr))goto 501 
      if(adrg(ndr).gt.0.0.and.adrg(ndr).gt.Filagr(ndr))goto 502 
      if(adrb(ndr).gt.0.0.and.adrb(ndr).gt.Filabl(ndr))goto 503 
      goto 515 
!                                                                       
  501 Filaki(ndr) = adrk(ndr) 
      filagr(ndr) = adrg(ndr) 
      filabl(ndr) = adrb(ndr) 
      vofkop(ndr) = (Filaki(ndr)/(aki(ior)*pki))*100. 
      goto 515 
!                                                                       
  502 Filaki(ndr) = adrk(ndr) 
      filagr(ndr) = adrg(ndr) 
      filabl(ndr) = adrb(ndr) 
      vofkop(ndr) = (Filagr(ndr)/(agr(ior)*pgr))*100. 
      goto 515 
!                                                                       
  503 Filaki(ndr) = adrk(ndr) 
      filagr(ndr) = adrg(ndr) 
      filabl(ndr) = adrb(ndr) 
      vofkop(ndr) = (Filabl(ndr)/(abl(ior)*pbl))*100. 
!                                                                       
  515 dchlg(ndr) = 0.0
      dchlk(ndr) = 0.0
      dchlb(ndr) = 0.0

      if(agbcm(ior)>0.0)dchlg(ndr) = filagr(ndr)*1000.*Cagr/agbcm(ior) 
      if(akbcm(ior)>0.0)dchlk(ndr) = filaki(ndr)*1000.*Caki/akbcm(ior) 
      if(abbcm(ior)>0.0)dchlb(ndr) = filabl(ndr)*1000.*Cabl/abbcm(ior) 
!                                                                       
      filHNF(ndr) = CHNF(ior)*fh2ovol/vol 
!                                                                       
!                                                                       
  214 continue 
!                                                                       
      algdrg(ior) = 0.0 
      algdrk(ior) = 0.0 
      algdrb(ior) = 0.0 
      ssdr(ior) = 0.0 
      drfaek(ior) = 0.0 
      drfaeg(ior) = 0.0 
      drfaeb(ior) = 0.0 
      drfaes(ior) = 0.0 
      draup = 0.0 
      volfdr(ior) = 0.0 
!                                                                       
      drHNF(ior) = 0.0 
!                                                                       
!                                                                       
      do ndr=1,nndr 
!                                                                       
        algdrg(ior) = algdrg(ior)+filagr(ndr) 
        algdrk(ior) = algdrk(ior)+filaki(ndr) 
        algdrb(ior) = algdrb(ior)+filabl(ndr) 
        ssdr(ior) = ssdr(ior)+filss(ndr) 
        drfaeg(ior) = drfaeg(ior)+drfecg(ndr) 
        drfaek(ior) = drfaek(ior)+drfeck(ndr) 
        drfaeb(ior) = drfaeb(ior)+drfecb(ndr) 
        drfaes(ior) = drfaes(ior)+drfecs(ndr) 
                                                                       
        volfdr(ior) = volfdr(ior)+vofkop(ndr) 
                                                                       
        draup = draup+adrg(ndr)+adrk(ndr)+adrb(ndr) 
        drHNF(ior) = drHNF(ior)+filHNF(ndr) 
      enddo
      
!....Ausgabe                                                            
      HNFdra(ior) = 0.0 
      if(CHNF(ior)>0.0)HNFdra(ior) = (drHNF(ior)/CHNF(ior))*24. 
                                                                       
!.....Schwebstoffaufnahme durch Dreissena wird vorl�ufig auf Null gesetz
      ssdr(ior) = 0.0 
!                                                                       
      if((algdrg(ior)+algdrk(ior)+algdrb(ior)).eq.0.0)then 
        drpfec(ior) = 0.0 
          else         
           drpfec(ior) = 1.-(draup/(algdrg(ior)+algdrk(ior)+algdrb(ior))) 
      endif

      drpfec(ior) = drpfec(ior)*100. 
      if(draup.eq.0.0)drpfec(ior) = 0.0 
      if(drpfec(ior).eq.0.0)volfdr(ior) = 0.0 
      if(drpfec(ior).lt.0.0)drpfec(ior) = 0.0 
!                                                                       
!                                                                       
!     Berechnung der Larvenbildung                                      
!                                                                       
      ddlarn = 0.0 
      dlamor = 0.0 
      dlafes = 0.0 
!                                                                       
      dlmax(ior) = dlmax(ior)*(2.*lboem(ior)*elen(ior)) 
      dlmaxs(ior) = dlmaxs(ior)*(bsohlm(ior)*elen(ior)) 

      if(lait1.eq.0.and.laim1.eq.0)goto 211 
      if(drft.ge.laid1)goto 116 
      if(ilang.eq.0.or.jahr_tst1.lt.jahrs)then 
      drrt = 0.0 
      goto 211 
      endif 
      if(monats.gt.2)goto 23 
      NRS = ITAGS+31*(MONATS-1) 
      goto 29 
   23 NRS = (ITAGS+31*(MONATS-1)-INT(0.4*MONATS+2.3)) 
!                                                                       
   29 if(laim1.gt.2)goto 25 
      NRla1a = lait1+31*(laim1-1) 
      goto 26 
   25 NRla1a = (lait1+31*(laim1-1)-INT(0.4*laim1+2.3)) 
!                                                                       
   26 nrla1e = nrla1a+laid1 

      if(nrs.lt.nrla1a.or.nrs.ge.nrla1e)then 
      drrt = 0.0 
      goto 113 
      endif 
!                                                                       
!                                                                       
      drrt1 = 0.0 
      drrt3 = 30. 
      drrt2 = drrt3/2. 
                                                                        
                                                                        
      drrt11 = 0.0 
      drrt33 = laid1-drrt3 
      drrt22 = drrt33/2. 
!                                                                       
!                                                                       
!.....Annahme Gewichtverlust der Adulten durch Reproduktion             
!                                                                       
!                                                                       
      if(drrt.le.drrt3)spwmx = flai*0.6/(0.5*drrt3) 
      if(drrt.gt.drrt3)spwmx = flai*0.4/(0.5*(laid1-drrt3)) 
!                                                                       
      if(dlmax(ior).eq.0.0.and.dlmaxs(ior).eq.0.0)then 
      dlmax(ior) = zdrei(ior,2) 
      dlmaxs(ior) = zdreis(ior,2) 
      gwdmax(ior) = gewdr(ior,2) 
      sgwmue(ior) = 0.0 
      endif 
!                                                                       
      if(drrt.gt.drrt3)goto 221 
!                                                                       
      if(drrt.gt.drrt2)goto 220 
      fdrrt = ((drrt-drrt1)**2)/((drrt-drrt2)**2+(drrt-drrt1)**2) 
      fdrrt = fdrrt*spwmx 
      goto 250 
  220 fdrrt = ((drrt-drrt3)**2)/((drrt-drrt2)**2+(drrt-drrt3)**2) 
      fdrrt = fdrrt*spwmx 
      goto 250 
!                                                                       
!   zweite Kurve                                                        
!                                                                       
  221 drrtt = drrt-drrt3 
      if(drrtt.gt.drrt22)goto 225 
      fdrrt = ((drrtt-drrt11)**2)/((drrtt-drrt22)**2+(drrtt-drrt11)**2) 
      fdrrt = fdrrt*spwmx 
      goto 250 
  225 fdrrt = ((drrtt-drrt33)**2)/((drrtt-drrt22)**2+(drrtt-drrt33)**2) 
      fdrrt = fdrrt*spwmx 
!                                                                       
  250 continue 
      do 216 ndr=2,nndr 
      gewdr(ior,ndr) = gewdr(ior,ndr)-(gwdmax(ior)*tflie*fdrrt) 
!                                                                       
!+++Berechnung der gebildeten Larven im Zeitschritt                     
!   aus dem Gewichtsverlust der Weibchen+++++                           
!   C-Gehalt einer Eizelle: 3.35e-9 g                                   
!                                                                       
      dEi = (dlmax(ior)+dlmaxs(ior))*tflie*fdrrt/3.35e-9 
      dEi = dEi*0.75 
      ddlarn = dEi*fgesund*fweib 
                                                                       
! Larvenbildung aus Zuwachs im Zeitschritt                              
!                                                                       
      dEimue = ((dyc*flai+dycs*flai)/3.35e-9)*0.75*fgesund*fweib 
      if(dEimue.lt.0.0)dEimue = 0.0 
      ddlarn = ddlarn+dEimue 
      gewdr(ior,ndr) = gewdr(ior,ndr)-dgewdr*0.52 
      dgwmue = (dyc/(2.*lboem(ior)*elen(ior)))*0.52                     &
     &+(dycs/(bsohlm(ior)*elen(ior)))*0.52                              
      sgwmue(ior) = sgwmue(ior)+dgwmue 
                                                                        
!                                                                       
      ddlarn = ddlarn/(vol*1000.) 
      zdrei(ior,ndr) = zdrei(ior,ndr)-(dlmax(ior)*tflie*fdrrt) 
      zdrei(ior,ndr) = zdrei(ior,ndr)-dyc*0.52 
      zdreis(ior,ndr) = zdreis(ior,ndr)-(dlmaxs(ior)*tflie*fdrrt) 
      zdreis(ior,ndr) = zdreis(ior,ndr)-dycs*0.52 

  216 continue 
!                                                                       
  113 continue 

      if(nrs>=nrla1a.and.itime_hoch==1)stdpla = stdpla+tflie 
      dlamor = dlarvn(ior)*(1.-exp(-klmor*tflie)) 

      if(stdpla.lt.tdpla)then 
      drft = 0.0
      itime_hoch = 0 
      goto 114 
      endif 
!                                                                       
!                                                                       
      if(itime_hoch==1)then
        drft = drft+tflie
        itime_hoch = 0
      endif 
!                                                                       
!                                                                       
      if(drft.le.drrt3)spwmx = flai*0.6/(0.5*drrt3) 
      if(drft.gt.drrt3)spwmx = flai*0.4/(0.5*(laid1-drrt3)) 
!                                                                       
!                                                                       
!                                                                       
      if(drft.gt.drrt3)goto 325 
!                                                                       
      if(drft.gt.drrt2)goto 322 
      fdrrt = ((drft-drrt1)**2)/((drft-drrt2)**2+(drft-drrt1)**2) 
      fdrrt = fdrrt*spwmx 
      goto 350 
  322 fdrrt = ((drft-drrt3)**2)/((drft-drrt2)**2+(drft-drrt3)**2) 
      fdrrt = fdrrt*spwmx 
      goto 350 
!                                                                       
!   zweite Kurve                                                        
!                                                                       
  325 drftt = drft-drrt3 
      if(drftt.gt.drrt22)goto 320 
      fdrrt = ((drftt-drrt11)**2)/((drftt-drrt22)**2+(drftt-drrt11)**2) 
      fdrrt = fdrrt*spwmx 
      goto 350 
  320 fdrrt = ((drftt-drrt33)**2)/((drftt-drrt22)**2+(drftt-drrt33)**2) 
      fdrrt = fdrrt*spwmx 
!                                                                       
!                                                                       
  350 continue 
!                                                                       
!....Larvengewicht beim Festsetzen: 8.6e-8 gC; 8.6e-5 mgC               
      dlafes = (dlmax(ior)+dlmaxs(ior))*tflie*fdrrt/3.35e-9 
      dlafes = dlafes*0.75*fgesund*fweib 
  116 dfemue = sgwmue(ior)/(tdpla*1./tflie)

      if(zdreis(ior,2).eq.0.0.and.zdrei(ior,2).eq.0.0)then 
      dfemue = 0.0 
      dfmue = 0.0 
      dfmues = 0.0 
      sgwmue(ior) = sgwmue(ior)-dfemue 
      goto 316 
      endif 
!                                                                       
      sgwmue(ior) = sgwmue(ior)-dfemue 
      dfmue = dfemue*(2.*lboem(ior)*elen(ior))*                         &
     &(zdrei(ior,2)/(zdrei(ior,2)+zdreis(ior,2)))                       
      dfmues = dfemue*(bsohlm(ior)*elen(ior))*                          &
     &(zdreis(ior,2)/(zdrei(ior,2)+zdreis(ior,2)))                      
  316 dfemue = ((dfmue+dfmues)/3.35e-9)*0.75*fgesund*fweib 
      dlafes = dlafes+dfemue 
      dlafes = (dlafes*exp(-klmorg))/(vol*1000.) 
  114 dlarvn(ior) = dlarvn(ior)+ddlarn-dlamor-dlafes 
      if(dlarvn(ior).lt.0.0)dlarvn(ior) = 0.0 

      do 217 ndr=1,nndr 
      ddrein = 0.0 
!                                                                       
      hconds(ndr) = zdreis(ior,ndr) 
      hcondb(ndr) = zdrei(ior,ndr) 
!                                                                       
      if(gewdr(ior,ndr).eq.0.0)then 
      dreing = 0.0 
      dreisn = 0.0 
      goto 219 
      endif 

      dreisn = ((zdrei(ior,ndr)+zdreis(ior,ndr))*1000.)/gewdr(ior,ndr) 
      dreing = dreisn 
  219 if(ndr.eq.1.and.dlafes.gt.0.0)then 

      gewdts = (dreisn*gewdr(ior,1)+dlafes*vol*1000.*8.6e-5)            &
     &/(dreisn+dlafes*vol*1000.)                                        
      if(gewdts.gt.0.0246)then 
      dlafes = dlafes*exp(-0.1*20.*tflie) 
      gewdts = (dreisn*gewdr(ior,1)+dlafes*vol*1000.*8.6e-5)            &
     &/(dreisn+dlafes*vol*1000.)                                        
      endif 
!                                                                       
      gewdr(ior,1) = gewdts 
!                                                                       
      dreing = dreisn+dlafes*vol*1000.
      goto 218 
      endif
 
      if(ndr.eq.2.and.gewdr(ior,1).gt.1.6)then 
!                                                                       
      ddrein = ((zdrei(ior,1)+zdreis(ior,1))*1000.)/gewdr(ior,1) 
      dreing = dreisn+ddrein 

!                                                                       
      gewdr(ior,2) = (dreisn*gewdr(ior,2)+ddrein*gewdr(ior,1))          &
     &/(dreisn+ddrein)                                                  
!                                                                       
      gewdr(ior,1) = 0.0 
      zdrei(ior,1) = 0.0 
      zdreis(ior,1) = 0.0 
      endif 
  218 continue 
      if(gewdr(ior,ndr).lt.0.0246)dmorg = 0.1 
      if(gewdr(ior,ndr).gt.0.0246)                                      &
     &dmorg = 0.0157*gewdr(ior,ndr)**(-0.502)                           
!nur ausschreiben                                                       
      drmor(ior,ndr) = dmorg 
      if(gewdr(ior,ndr).eq.0.0)drmor(ior,ndr) = 0.0 
                                                                       
      dreinm = dreing*(1.-exp(-dmorg*tflie)) 
      dreing = dreing-dreinm 
      if(dreing.lt.0.0)dreing = 0.0 
                                                                       
      if(ndr.eq.1.and.dlafes.gt.0.0.and.                                &
     &(zdrei(ior,ndr)+zdreis(ior,ndr)).eq.0.0)then                      
      hcond1 = zdrei(ior,2)/(zdrei(ior,2)+zdreis(ior,2)) 
      hcond2 = zdreis(ior,2)/(zdrei(ior,2)+zdreis(ior,2)) 
      goto 351 
      endif 
!                                                                       
      if((zdrei(ior,ndr)+zdreis(ior,ndr)).eq.0.0.and.ndr.eq.1)then 
      zdrei(ior,ndr) = 0.0 
      zdreis(ior,ndr) = 0.0 
      goto 217 
      endif 
!                                                                       
      if((zdrei(ior,ndr)+zdreis(ior,ndr)).eq.0.0.and.ndr.eq.2           &
     &.and.ddrein.eq.0.0)then                                           
      zdrei(ior,ndr) = 0.0 
      zdreis(ior,ndr) = 0.0 
      goto 217 
      endif 
!                                                                       
      if((zdrei(ior,ndr)+zdreis(ior,ndr)).eq.0.0.and.ndr.eq.2           &
     &.and.ddrein.gt.0.0)then                                           
      hcond1 = hcondb(1)/(hcondb(1)+hconds(1)) 
      hcond2 = hconds(1)/(hcondb(1)+hconds(1)) 
      goto 351 
      endif 
!                                                                       
      hcond1 = hcondb(ndr)/(hcondb(ndr)+hconds(ndr)) 
      hcond2 = hconds(ndr)/(hcondb(ndr)+hconds(ndr)) 
!                                                                       
  351 zdrei(ior,ndr) = (dreing*gewdr(ior,ndr)/1000.)*hcond1 
      zdreis(ior,ndr) = (dreing*gewdr(ior,ndr)/1000.)*hcond2 
                                                                       
  217 continue 
                                                                        
      do 872 ndr = 1,nndr 
      zdrei(ior,ndr) = zdrei(ior,ndr)/(2.*lboem(ior)*elen(ior)) 
      if(bsohlm(ior).le.0.0)then 
      zdreis(ior,ndr) = 0.0 
      goto 872 
      endif 
      zdreis(ior,ndr) = zdreis(ior,ndr)/(bsohlm(ior)*elen(ior))
  872 continue 
!                                                                       
  211 dlmax(ior) = dlmax(ior)/(2.*lboem(ior)*elen(ior)) 
      dlmaxs(ior) = dlmaxs(ior)/(bsohlm(ior)*elen(ior)) 
!                                                                       
  111 continue 
  999 dlarvn(anze+1) = dlarvn(anze)

      jahr_tst1 = jahrs 
!                                                                       
!                                                                       
      RETURN 
      END                                           
