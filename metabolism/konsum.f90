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

  SUBROUTINE konsum(vkigr,TEMPW,VO2,TFLIE                            &
  &,ezind,ZOOIND,abszo,ir,flag,elen,ior,anze,qeinl,vabfl             &
  &,jiein,FopIRe,GRote,dzres1,dzres2,zresge                          &
  &,irmaxe,zexki,zexgr,zexbl                                         &
  &,aki,agr,abl,iwied,rmuas,iras,TGZoo,BAC,zBAC                      &
  &,rakr,rbar,CHNF,zHNF,ilbuhn,zakie,zagre,zable,HNFza,algzok        &
  &,algzog,algzob,akiz,agrz,ablz,algzkz,algzgz,algzbz,nkzs,monats    &
  &,itags,uhrz,mstr,azStrs                                           &                                                   
  &,kontroll ,jjj ) !!wy  
                                                                       
                                                                       
!   UNTERPROGRAMM ZUR BERECHNUNG DES ZOOPLANKTONEINFLUSSES            
!   AUF DEN STOFF-UND SAUERSTOFFHAUSHALT EINES FLIEGEWAESSERS         
                                                                       
                                                                       
!   AUTOR :      VOLKER KIRCHESCH                                     
                                                                       
!   STAND :      21.5.2015                                             
                                                                       
                                                                       
!   VARIABLENLISTE:                                                   
                                                                       
!   irmax  - max. Filtrierate in mueg Chl-a/(100 Ind*d)               
                                                                       
      logical kontroll !!wy
      integer jjj !!wy                                                                       
    integer                        :: anze, azStrs
    integer, Dimension(1000)       :: jiein, nkzs, flag
    real                           :: irmaxK, irmaxG, irmaxB, irmaxn, irmax, irmaxe, mueRot, mormax, mormin, morRot
    real                           :: irmax_Ind, ir_F 
    real, Dimension(50)            :: irz 
    real, Dimension(100)           :: ezind, qeinl 
    real, Dimension(1000)          :: tempw, vo2, zooind, abszo, zexki, zexgr, zexbl, aki, agr, abl, vkigr 
    real, Dimension(1000)          :: elen, ir, dzres1, dzres2, vabfl, rmuas, iras, rakr, rbar 
    real, Dimension(1000)          :: CHNF, zHNF, BAC, zBAC, HNFza, algzok, algzog, algzob 
    real, Dimension(50,1000)       :: akiz, agrz, ablz, algzkz, algzgz, algzbz
    real, Dimension(azSTrs,1000)   :: TGZoo          
    double precision               :: Qquell,QSenk 

    save hczoo1


                                                                       
    !!wy open(unit=79,file='konsum.tst')                                  
                                                                       
    iein = 1 
    
    iTGZoo = 0 
                                                                       
    Cagr = 0.48 
    Caki = 0.48 
    Cabl = 0.48

    CRot = 0.45
 
    dokrit = 2.5 

!    mormax = 0.32
    mormax = 0.15
    ASSmxR = 0.84            ! Verschoor et al. (2007)
    respaR = 0.203

    Emort = 2. 
    EASS = 0.705             ! Verschoor et al. (2007)

    thresR = 1.12 
    thmorR = 1.077
    thIng = 1.08            
                                                                       
    zqz10 = 2.23 
    ztmax = 26.1 
    ztopt = 22.2 

    zagr = zagre 
    zaki = zakie 
    zabl = zable 

    ZellVGr = 320.
    ZellVKi = 645.
    ZellVBl = 1000.

    if(iwied==0)then
      do j = 1, azStrs
        do jj = 1,anze+1
          TGZoo(j,jj) = GRote
        enddo
      enddo
    Endif
                                                                      
    FOPTR = FopIRe 
    GROT = GRote 

    irmax = IRmaxe ! in [1/d]
    RotC = GROT*CRot

  if(iTGZoo==0)then  
    if(IRmaxe<0.0)then
      up_CROT = -0.8377*log10(RotC)+0.3131   ! up_CROT: Gewichtszpezifische max. Ingestionsrate �C^(-2/3)*d-1
      up_CROT = 10**up_CROT
        else      
          up_CROT = IRmaxe 
    endif                                                                    
                                                                    
    IRmax = up_CROT*RotC**(2./3.)

    if(FopIRe<0.0)then
      ClearRlog = -0.9987*log10(RotC)-0.706 
      ClearR = (10**(ClearRlog)*RotC**(2./3.))*1.e5   ! Filtrierrate in 1/h
      
      VolRot = RotC*1.e6/0.12

      ClearR_Ind = ClearR*VolRot/1.e9
   
      IRmax_Ind = IRmax*VolRot*0.00012/24.

      FKs = IRmax_Ind/ClearR_Ind
        else
          FKs = FopIRe
    endif  
 Endif

    do j = 1,anze+1  !Beginn Knotenschleife

     ior = j

  if(iTGZoo==1)then 

    if(TGZoo(mstr,ior)>0.0)then
       RotC = TGZoo(mstr,ior) * CRot
       zagr = min(1.,max(0.0,-0.656*log10(ZellVGr/RotC)+3.27)) 
       zaki = min(1.,max(0.0,-0.656*log10(ZellVKi/RotC)+3.27)) 
       zabl = min(1.,max(0.0,-0.656*log10(ZellVBl/RotC)+3.27)) 
    endif
 
    if(IRmaxe<0.0)then
      up_CROT = -0.8377*log10(RotC)+0.3131   ! up_CROT: Gewichtszpezifische max. Ingestionsrate �C^(-2/3)*d-1
      up_CROT = 10**up_CROT
        else      
          up_CROT = IRmaxe 
    endif                                                                    
                                                                    
    IRmax = up_CROT*RotC**(2./3.)

    if(FopIRe<0.0)then
      ClearRlog = -0.9987*log10(RotC)-0.706 
      ClearR = (10**(ClearRlog)*RotC**(2./3.))*1.e5   ! Filtrierrate in 1/h
      
      VolRot = RotC*1.e6/0.12

      ClearR_Ind = ClearR*VolRot/1.e9
   
      IRmax_Ind = IRmax*VolRot*0.00012/24.

      FKs = IRmax_Ind/ClearR_Ind
        else
          FKs = FopIRe 
    endif  
 Endif
     if(zooind(ior)<0.0)zooind(ior) = 0.0

      if(vabfl(ior)>=0.0.and.vabfl(ior+1)<0.0)then
        hczoo1 = zooind(ior)
        hcTGZoo1 = TGZoo(mstr,ior)
      endif

      ior_flag = 0
      if(flag(ior)==6.and.vabfl(ior)<0.0.and.vabfl(ior+1)>0.0)then
        ior = ior+1
        ior_flag = 1
      endif

      if(ilbuhn==1)then
        nkzs(ior) = 1
          else if(flag(ior)/=4)then
            else                        ! Ber�cksichtigung der Einleitungen
              m = 1
              ihcQ = 0
              if(vabfl(ior-1)<0.0.and.vabfl(ior)<0.0)m = -1
              if(vabfl(ior-1)<0.0.and.vabfl(ior)>0.0)ihcQ = 1 ! Konzentration an der Einleitstelle 
                                                              ! ist gleich der Konzentration der Einleitung 

              
              hczoo = zooind(ior-m)     ! Umbenennen der ben�tigten Variablen; 1D
              hcQ = vabfl(ior-m)
              hcTGZoo = TGZoo(mstr,ior-m)
              if(hcQ<0.0)hcQ = abs(hcQ)
              if(hcQ==0.0.or.ihcQ==1)hcQ = 1.e-10

              if(ihcQ==1)then
                hczoo = hczoo1
                hcTGZoo = hcTGZoo1
              endif
              
              do ji=1,jiein(ior)   ! Beginn Einleitungsschleife  
              hcQE = max(0.0,qeinl(iein))

              hczooE = ezind(iein)
              if(hczooE<0.0)hczooE = hczoo
              hcTGZooE = hcTGZoo

             zooind(ior) = (hcQ*hczoo+hcQE*hczooE)/(hcQ+hcQE) 
             if(ezind(iein)>0.0.and.qeinl(iein)==0.0)then
               zooind(ior) = ezind(iein)
             endif

             TGZoo(mstr,ior) = (hcQ*hcTGZoo+hcQE*hcTGZooE)/(hcQ+hcQE) 

             hcQ = hcQ+qeinl(iein) 
             iein = iein+1 

             hczoo = zooind(ior) 
             hcTGZoo = TGZoo(mstr,ior)
           enddo                        ! Ende Einleitungsschleife
           if(ior_flag==1)then
             iein = iein - jiein(ior)  
             ior = ior-1
             zooind(ior) = zooind(ior+1)
             TGZoo(mstr,ior) = TGZoo(mstr,ior+1)
           endif
    endif                               ! Ende Einleitungs-flag                                                                  
                                                                  
    if(ior>1)then
      zooind(ior-1) = zooint
      TGZoo(mstr,ior-1) = TGZoot    
    endif                                                                       
                                                                       
    abszo(ior) = 0.0 
    mueRot = 0.0 
    ir(ior) = 0.0 
    iras(ior) = 0.0 
    zHNF(ior) = 0.0 
    HNFza(ior) = 0.0 
    dzres2(ior) = 0.0 
    zexki(ior) = 0.0 
    zexgr(ior) = 0.0 
    zexbl(ior) = 0.0 
                                                                       
!   Temperaturabhaengigkeit der Ingestionsrate             

      fTing = thIng**(Tempw(ior)-20.)
!      fTing = exp(-0.0085*(Tempw(ior)-21.)**2)                         

        if(tempw(ior)>=tmax)then 
!          fTing = 0.01 
            else 
              LNQ = 0.61519 
              W = LNQ*(TMAX-TOPT) 
              X = (W**2*(1+SQRT(1+40/W))**2)/400. 
              FTA = ((TMAX-TEMPW(ior))/(TMAX-TOPT))**X 
!              fTing = FTA*EXP(X*(1-((TMAX-TEMPW(ior))/(TMAX-TOPT)))) 
        endif
                                                                       
!   Umrechnung der Individienzahl in Biomasse (g*m-3)                   
    ROT = zooind(ior)*GROT/1000. 
                                                                       
                                                                       
!   filtrierbare Algenbiomasse                                         
    filabio = aki(ior)*Caki+agr(ior)*Cagr+abl(ior)*Cabl 
                                                                       
!   Grundrespiration                                                  
!   Temperaturabh�ngigkeit                                            
    fTresR = thresR**(tempw(ior)-20.) 
    respRg = zresge*fTresR 
                                                                        
!   Mortalitaetsrate                                                  
!   Berechnung unter Beruecksichtigung der Futterkonz.                  
!   des Sauerstoffgehalts und der Temperatur                            
                                                                       
!   O2-Einfluss                                                          
    filo2 = (dokrit-vo2(ior))/dokrit

    filo2 = 1.-filo2 
                                                                       
!   Nahrungseinfluss                                                     
    hconF = filabio/(filabio+FKs) !Filabio in mgC/l 
    if(hconF>1.)hconF = 1. 
    if(hconF<0.0)hconF = 0.0 

    hcaki = aki(ior)
    if((aki(ior)+agr(ior)+abl(ior))==0.0)hcaki = 0.000001
    hconki = hcaki/(hcaki+agr(ior)+abl(ior)) 
    hconGr = agr(ior)/(hcaki+agr(ior)+abl(ior)) 
    hconBl = abl(ior)/(hcaki+agr(ior)+abl(ior))

    hconF = hconF*(zaki*hconki + zagr*hcongr +zabl*hconbl)
                                                                       
!   Temperatureinfluss                                                 
    fTmorR = thmorR**(tempw(ior)-20.) 
                                                                       
    hconM = min(hconF,filO2)
!    morRot = mormax*exp(-hconM*Emort) 
    morRot = -0.14*hconM**2-0.0093*hconM+mormax

    morRot = morRot*fTmorR
                                                                         
!   Zooplanktonwachstum                                               
                                                                       
!   Assimilationsrate                                                 
                                                                 
    if(hconF==0.0)then 
      zass = 0.0 
        else      
         zass = ASSmxR*exp(-EASS*hconF) 
         if(zass>1.)zass = 1. 
    endif 

    ir_F = irmax*hconF

    ProdRot = (zass-respaR)*ir_F*fTing-respRg

    ir(ior) = ir_F*fTing*tflie*ROT 
    iras(ior) = ir_F*fTing 
                                                                      
!   ir - Ingestionsrate in mg/(l*h)                                  
!   zHNF - Aufnahmerate der HNF 
!   zBAC - Aufnahmerate der Bakterien                                     
!   ir/A - Filtriertes Wasservolumen l/h                     
                                                                       
    zHNF(ior) = 0.0 
    if(ir(ior)==0.0)then
      else 
        zHNF(ior) = ir(ior)*CHNF(ior)/(CHNF(ior)+agr(ior)+aki(ior)+abl(ior))
        zBAC(ior) = ir(ior)*BAC(ior)/(BAC(ior)+agr(ior)+aki(ior)+abl(ior)) 
        zBAC(ior) = 0.0
    endif

!   Ausgabe                                                             
    if(CHNF(ior)==0.0)then
       else 
         HNFza(ior) = (zHNF(ior)/CHNF(ior))*24. 
    endif
                                                                       
    ROTt = ROT * exp((ProdRot-morRot)*tflie) ! Rotatorienzunahme

    !!wy if(mstr==1)write(79,*)ior,ProdRot,zass,respaR,ir_F,fTing,respRg

    if(iTGZoo==1)then
      TGZoot = TGZoo(mstr,ior)*exp(ProdRot*0.20*tflie)
    endif

    dzres1(ior) = ROT*(1.-(exp(-respRg*tflie))) 
    ABSZO(ior) = ROTt*(1.-(EXP(-morRot*TFLIE))) 

    dzres2(ior) = respaR*ir(ior)
                                                                       
    zexki(ior) = ir(ior)*(1.-zass)*hconki 
    zexgr(ior) = ir(ior)*(1.-zass)*hconGr 
    zexbl(ior) = ir(ior)*(1.-zass)*hconBl 
                                                                       
    algzok(ior) = min((aki(ior)*zaki),ir(ior)*hconki) 
    algzog(ior) = min((agr(ior)*zagr),ir(ior)*hconGr) 
    algzob(ior) = min((abl(ior)*zabl),ir(ior)*hconBl) 
                                                                       
        if(nkzs(ior)==1)then 
         else
!          2D-Modellierung                                                   
 
           do nkz = 1,nkzs(ior) 
           hcaki = akiz(nkz,ior)
           if((akiz(nkz,ior)+agrz(nkz,ior)+ablz(nkz,ior))==0.0)hcaki = 0.000001                                         
             hconki = hcaki/(hcaki+agrz(nkz,ior)+ablz(nkz,ior)) 
             hcongr = agrz(nkz,ior)/(hcaki+agrz(nkz,ior)+ablz(nkz,ior)) 
             hconbl = ablz(nkz,ior)/(hcaki+agrz(nkz,ior)+ablz(nkz,ior)) 
                                                                       
             algzkz(nkz,ior) = ir(ior)*hconki 
             algzgz(nkz,ior) = ir(ior)*hcongr 
             algzbz(nkz,ior) = ir(ior)*hconBl 
           enddo 
     endif
                                                                  
    zooint = (ROTt*1000./GROT)

    delzoo = zooint-zooind(ior) 
    if(zooind(ior)<0.0)zooint = (zooind(ior)/(zooind(ior)+abs(delzoo)))*zooind(ior)      
                                                                      
                                                                       
!   Ausgabeparameter                                                   
    rmuas(ior) = ProdRot - morRot 
    rakr(ior) = morRot   ! ras(ior)*respaR 
    rbar(ior) = respRg 
                                                                      
    enddo ! Ende Knotenschleife 
                                                                       
    zooind(anze+1) = zooint
    TGZoo(mstr,anze+1) = TGZoot 
                                                                       
    RETURN 
  END subroutine konsum                                           
