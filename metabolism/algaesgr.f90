      SUBROUTINE algaesgr(SCHWI,TFLIE,TEMPW,RAU,TIEFE,VMITT,VNO3,VNH4,GELP,svhemg,CHLA,SSALG,dalggr,dalgag              &
                ,flag,elen,ior,anze,sedalg,algzog,dgrmor,fkm,vkigr,chlaki,chlagr,vabfl,qeinl,jiein,evkigr,eantbl        &
                ,agchl,aggmax,agksn,agksp,agremi,vco2,algdrg,pbiogr,Q_PK,Q_NK,iph,akbcm,agbcm,aki,agr,cmatgr            &
                ,cmatki,abbcm,antbl,abl,pbiobl,chlabl,extk,extk_lamda		       	                             	    &
                ,ilamda,eta,aw,ack,acg,acb,ah,as,al       					       	                             	    & !!wy, Einlesen von e_extnct.dat nicht hier
                ,tpgr,uhrz,iwied,algcog                                                                                 &
                ,figaus,agmuea,fhegas,agreau,tauscs,ischif,ilbuhn,ieros,asgre,echla,ess,ss,zooind,GRote,Q_PG,Q_NG       &
                ,vNH4z,vNO3z,gelPz,dalggz,nkzs,dH2D,tempwz,cpfad,itags,monats,mstr,up_PGz,up_NGz,Qmx_PG                 &
                ,Qmn_PG,upmxPG,Qmx_NG,Qmn_NG,upmxNG,IKge,frmuge,alamda,agrtbr,agrbrz,akiz,agrz,ablz                     &
                ,chlaz,hchlkz,hchlgz,hchlbz,hCChlgz,algagz,algzgz,Dz2D,ToptG,kTemp_Gr,ifix,sedAlg_MQ,sedAlg0, hQ_NGz    &
                ,a1Gr,a2Gr,a3Gr,ifehl,ifhstr,isim_end,agmor_1,azStrs                                                    &
                ,kontroll ,jjj )              !!wy                                                                           

    
!###### UNTERPROGRAMM ZUR BERECHNUNG DES Grünalgenwachstums ######                   
                                                                       
!     AUTOR :VOLKER KIRCHESCH                                           

                                                                       
!     STAND :08.09.2015                                                 
                                                                       
                                                                       
!     UNTERPROGRAMME :TAGE,ALBEDO                                       
                                                                       
      logical kontroll !wy
      integer jjj !wy
      character (len=255) cpfad
      character (len=275)                      :: pfadstring
      character (len=2) ckenn_Vers1

 
      integer                                  :: anze, azStrs
      integer, Dimension(1000)                 :: flag, jiein, ischif, nkzs

      real                                     :: LNQ, Ihemm, Iprod, Icz, Ic, Ic0, lamda0, IKg, IKge, kTemp_Gr, N_Cmax
      real                                     :: Icmit, kTresp
      real, Dimension(40)                      :: eta, aw, ack, acg, acb, ah, as, al, I0, Iz 
      real, Dimension(50)                      :: agrtz, Pz, F5z, aggrwz, CChlaz, CChlazt, Chlagrzt, xroh_Chlz, roh_Chlz 
      real, Dimension(50)                      :: Y, YY, hc_temp, Q_PGz, dmorChlgz, agresz, dzMasse, Masse_neu, dzMasse0
      real, Dimension(50)                      :: xroh_Chl 
      real, Dimension(100)                     :: qeinl, evkigr, eantbl, echla, ess, hemm 
      real, Dimension(1000)                    :: tempw, chla, ssalg, vno3, vnh4, gelp, vco2, chlaki, svhemg, dalggr
      real, Dimension(1000)                    :: dalgag, chlagr, vmitt, rau, tiefe, elen, vabfl, vkigr, antbl, Q_PK   
      real, Dimension(1000)                    :: Q_NK, sedalg, algzog, dgrmor, agrtbr, fkm, algdrg, algcog, cmatgr 
      real, Dimension(1000)                    :: cmatki, tpgr, extk, akbcm, agbcm, aki, agr, zooind, abl, chlabl 
      real, Dimension(1000)                    :: abbcm, ss, figaus, agmuea, fhegas, agreau, up_PG, up_NG, Q_PG, Q_NG 
      real, Dimension(1000)                    :: schwi, Dz2D, sedAlg0

      real, Dimension(40,1000)                 :: extk_lamda
      real, Dimension(50,1000)                 :: agrbrz, up_PGz, up_NGz, akiz, agrz, ablz, algagz, dgmorz, algzgz
      real, Dimension(50,1000)                 :: vNH4z, vNO3z, gelPz, dalggz, tempwz, chlaz
      real, Dimension(azStrs,1000)             :: sedAlg_MQ, agmor_1 

      real, Dimension(azStrs,50,1000)          :: hchlkz, hchlgz, hchlbz, hCChlgz, hQ_NGz


      save Cchlaz, agrzt
                                                                       
      ispek = 0 
      iein = 1
      itemp = 1
 
      ifoto = 0     !Lichtabhängigkeit der Fotosyntheserate nach Ross (2009) 
      ifoto = 1     !Lichtabhängigkeit der Fotosyntheserate nach Geider (1998)

      isyn = 0      ! Chlorophylla-Synthese nach Geider (1998)
      isyn = 1      ! Chlorophylla-Synthese nach Geider (1997) 


      if(chla(1)<0.0)then  ! falls kein Chla-Randbedingung
        do ior = 1,anze+1
          sedalg(ior) = 0.0
          Sedalg0(ior) = 0.0
          sedAlg_MQ(mstr,ior) = 0.0 
        enddo
          else ! ansonsten
                                                                       
!      open(unit=98,file='gr.tst') 
!                                                                       
!.....Eingaben                                                          
!                                                                       
!     tauad = Relaxationszeit der Algen in min                          
!                                                                       
      TOPT = ToptG

      kTresp = 0.058

      tauad = 100.             !Relaxationszeit der Algen in sec 
      Iref = 140.

      Cagr = 0.48 
      Caki = 0.48 
      Cabl = 0.48 
!                                                                       
      IKg = IKge 
      frespg = frmuge 

      Te0 = 20.

      if(agChl>=32.2)then
       agchl_max = 32.2
         else
           agChl_max = 12.4
           if(agChl==20.6)agChl_max = 20.6
           if(agChl==15.6)agChl_max = 15.6
       endif
      
!     C:Chla = abchl * exp(-a1*T) * a2 * I * exp(-a3*T)

      CChl0 = agchl * exp(-a1Gr * Te0)               ! C:Chla bei 0°C mgC/mgChla

      if(agChl>agChl_max)then
        CChl0 = agChl_max * exp(-a1gr * Te0)
      endif

!Umrechnung der maximalen Wachstumsrate bei 20°C auf Wachstumsrate unter optimalen Temperatur-Bedingungen

       aggmaxTopt = aggmax
       if(iTemp==1)then
         aggmaxTopt = aggmax/(exp(-kTemp_Gr*(Te0-Topt)**2))
         upmxPG = aggmaxTopt * Qmx_PG               ! s. Geider (1998), Angabe pro mg Biomasse
         upmxNG = aggmaxTopt * Qmx_NG               ! Geider (1998)
        endif    
                                                                       
!.....Einlesen der                                                      
!wird wieder aktiviert wenn Datei in Gerris erzeugt wird!!!             
!!wy      write(pfadstring,'(2A)')trim(adjustl(cpfad)),'e_extnct.dat'
!!wy      open(unit=101, file=pfadstring)
!!wy      rewind(101) 
!!wy      read(101,'(A2)')ckenn_vers1
!!wy      if(ckenn_vers1/='*V')then 
!!wy        else
!!wy          read(101,'(2x)') 
!!wy      endif
!!wy      read(101,'(i2)')ilamda 
!                                                                       
      sumaw = 0.0 
      sumas = 0.0 
      sumacg = 0.0 
!                                                                       
    do i=1,ilamda 
!!wy      read(101,1000)eta(i),aw(i),ack(i),acg(i),acb(i),ah(i)             &
!!wy     &,as(i),al(i)                                                      
      sumaw = sumaw+aw(i) 
      sumas = sumas+as(i) 
      sumacg = sumacg+acg(i) 
    enddo 

      awmit = sumaw/ilamda 
      asmit = sumas/ilamda 
      acmitg = sumacg/ilamda 
!                                                                       
 1000 format(f5.1,7(2x,f8.6)) 
!                                                                       
!....Berechnung des Absorptionsspektrums für Gelbstoffe                 
!                                                                       
      lamda0 = 440. 
      Slamda = 0.016 
      sumah = 0.0 
      do i = 1,ilamda 
        ah(i) = alamda*exp(-Slamda*(eta(i)-lamda0)) 
        sumah = sumah+ah(i) 
      enddo
 
      ahmit = sumah/ilamda 
                                                                       
                                                                       
!############################                                           
!...Beginn der Segmentschleife                                          
      do 111 ior = 1,anze+1

    sumac = 0.0 
    do i=1,ilamda 
      sumac = sumac+(ack(i)*vkigr(ior)+acg(i)*(1.-vkigr(ior)-antbl(ior))+acb(i)*antbl(ior))                                               
    enddo
 
      acmit = sumac/ilamda 
!                                                                       
!......schwi*4.2 - Umrechnung von cal/(cm2*h) in J/(cm2*h)              
      OBFLI = 5.846*(schwi(ior)*4.2) ! Lichtgeschwindigkeit fuer Luft, da µE bei Wachstumsversuchen in Luft gemessen 
                                                                        
      IF(OBFLI<0.0)obfli = 0.0 
!                                                                       
!.....Spektrale Auflösung der photosynth. aktiven Strahlung             

      if(ispek==0)then
        I0(1) = obfli
          else
           do i=1,ilamda  
             I0(i) = obfli*al(i) 
          enddo
      endif                                                              
                                                                       
!....Nullsetzen                                                         
    do nkz = 1,nkzs(ior) 
      Pz(nkz) = 0.0 
      F5z(nkz) = 0.0 
    enddo 
                                                                       
      if(ilbuhn.eq.1)nkzs(ior) = 1 
                                                                       
      if(ior.gt.1)then 
      agr(ior-1) = agrt
      chlagr(ior-1) = Chlagrt
      agbcm(ior-1) = agbcmt
 
      do nkz = 1,nkzs(ior-1) 
        agrz(nkz,ior-1) = agrtz(nkz) 
        hchlgz(mstr,nkz,ior-1) = chlagrzt(nkz) 
        hCChlgz(mstr,nkz,ior-1) = CChlazt(nkz)
      enddo
      endif 
                                                                       
      if(ilbuhn.eq.1.and.tiefe(ior).lt.0.05)goto 111 
                                                                       
                                                                       
!##### Temperaturabhaengigkeit der Respirationsrate  ######                     
                                                                       
      FTEMP = exp(kTresp*(TEMPW(ior)-20.))          ! hier

!                                                                       
!....Temperaturabhaengigkeit der Wachstumsrate                          

      if(itemp==0)then
        if(tempw(ior)>=tmax)then 
          fta = 0.01 
        else 
              LNQ = log(1.73) 
              W = LNQ*(TMAX-TOPT) 
              X = (W**2*(1+SQRT(1+40/W))**2)/400. 
              FTA = ((TMAX-TEMPW(ior))/(TMAX-TOPT))**X 
              FTA = FTA*EXP(X*(1-((TMAX-TEMPW(ior))/(TMAX-TOPT)))) 
        endif                                                                
      else
        FTA = exp(-kTemp_Gr*(Tempw(ior)-Topt)**2) 
      endif                                                                       
      FTA = max(0.01,FTA) !!wy
      if(kontroll) print*,'algaesgr:Temperaturabhaengigkeit jjj FTA,iTemp,tmax=',jjj,FTA,iTemp,tmax !!wy


!     Berechnung der Schubspannungsgeschwindigkeit                      
!                                                                       
      FN = 1./RAU(ior) 
      G = 9.81 
      UST = ((FN*G**0.5)/TIEFE(ior)**0.166667)*abs(VMITT(ior)) 
                                                                       
!     Berechnung des mittleren vertikalen Dispersionskoeffizient        
!     nach Fischer im ein-dimensionalen Fall (gute Näherung)            
                                                                       
      a = 0.4*ust 
      dztot = a*tiefe(ior)/6. 
!                                                                       
      if(nkzs(ior)==1.or.Dz2D(ior)==0.0)then
        else 
          dztot = Dz2D(ior) 
       endif 

      dz = sqrt(tauad*2.*Dztot)               ! Dicke der Schicht im vertikalen Profil 

      PCmax = (aggmaxTopt+agremi * exp(kTresp*(Topt-20.)))/(1.-frespg)
                                                                             
!.... Berechnung der Lichtabsorption im gesamten Wasserk”rper           
!                                                                       
      js = 0   					 !!wy
      if(dz.gt. 0.0) js = int(tiefe(ior)/dz)  	 !!wy
      !      js = int(tiefe(ior)/dz) 

      if(js>30)then 
        js = 30 
        dz = tiefe(ior)/js
      endif

      if(js<1)then
        js = 1 
        dz = tiefe(ior)
      endif
 
      deltaz = tiefe(ior)-js*dz
      if(deltaz>0.0001)then
        js = js + 1
          else
           deltaz = 0.0
      endif

      dz_spline = dz 
                                                                       
      sumPc = 0.0
      sumRoh_Chl = 0.0
      sumH = 0.0

      if(nkzs(ior)>1)then
        n_neu_s = js
        n_alt_s = nkzs(ior)
        do nkz = 1,nkzs(ior)
          Y(nkz) = tempwz(nkz,ior)
        enddo 
        iaus = 0
        i_zeiger = 0
        call lin_spline (dH2D, dz_spline, deltaz, n_alt_s, n_neu_s, Y, YY,i_zeiger,iaus,ior)

        do j = 1,n_neu_s
          hc_temp(j) = YY(j)
        enddo

        dz_spline = dz 

        do nkz = 1,nkzs(ior)
          Y(nkz) = hCChlgz(mstr,nkz,ior)
        enddo 

        i_zeiger = 0
        call lin_spline (dH2D, dz_spline, deltaz, n_alt_s, n_neu_s, Y, YY,i_zeiger,iaus,ior)
        iaus = 0
        do j = 1,n_neu_s
          CChlaz(j) = YY(j)
         enddo
      endif   

! #### Nährstoffabhängigkeit des Wachstums (1D-Fall) ######

!...Temperaturabhängigkeit von KP, KN,KSi                               
                                                                       
      fT_Ks = 1.15**(20.-tempw(ior))
      fT_Ks = 1. 
                                                                       
      if((Qmx_NG/Qmn_NG)<1.25)then 
        F51 = (VNO3(ior)+VNH4(ior))/(agksN*ft_ks+VNO3(ior)+VNH4(ior))                                             
          else 
            F51 = (Q_NG(ior)-Qmn_NG)/(Qmx_NG-Qmn_NG)
      endif
                                         
      if((Qmx_PG/Qmn_PG)<1.25)then 
        F52 = gelP(ior)/(agksp*ft_ks+gelP(ior)) 
          else 
            F52 = (Q_PG(ior)-Qmn_PG)/(Qmx_PG-Qmn_PG)
      endif
                                         
      F5 = F51 
      F5 = min(F51,F52)
      if(F5<0.0)F5 = 0.0

    do j = 1,js                        ! Schleife über die Schichten 
    
      dz1 = dz
                                                                      
      if(j==js.and.deltaz>0.0)dz = deltaz 

      if(ispek==1)then
        Ic = 0.0
        do i = 1,ilamda 
          Iz(i) = I0(i)*exp(-extk_lamda(i,ior)*dz)
          Ic = Ic + max(0.0,(I0(i)/(extk_lamda(i,ior)*dz))*(1.-exp(-extk_lamda(i,ior)*dz)))
        enddo
          else
            Icz = I0(1)*exp(-extk(ior)*dz)
            Ic = max(0.00,(I0(1)/(extk(ior)*dz))*(1.-exp(-extk(ior)*dz)))
      endif
                  
      if(nkzs(ior)==1)then
        hc_temp(j) = tempw(ior)
        CChlaz(j) = agbcm(ior)
          else
      endif

      yK = 1.-svhemg(ior) 

      CChl_Stern = CChl0 *exp(a1Gr * hc_temp(j))        ! dunkeladaptierte Algen

      call LichtHemmung(tflie,Ic,yK,CChl_Stern,CChlaz,j)                                             
      hemm(j) = yK 

       Saettg = IKg * 0.183*exp(0.0848*hc_temp(j))
       if(IKg==103.9)Saettg = IKg * 0.234*exp(0.0726*hc_temp(j))

       alpha_chl = PCmax*FTA*CChl_Stern/(Saettg*86400.) 

      if(ifoto==0)then
        Pc = Pcmax*FTA*(1.-exp(-Ic/Saettg))*hemm(j)
          else if(ifoto==1)then
            Pc = Pcmax*FTA*(1.-exp(-Ic*alpha_Chl/(max(CChl_Stern,CChlaz(j))*Pcmax*FTA/86400.)))*hemm(j)
      endif 

! Berechnung roh_Chl (wird für die Neuberechnung der Chlorophyll-a-Konzentration)

      N_Cmax = Qmx_NG/Cagr

      xroh_Chlz(j) = CChlaz(j)/(alpha_Chl*(max(0.1,Ic))*N_Cmax) ! 2D-Fall

      if(isyn==1)then
        xroh_Chlz(j) = CChlaz(j)/(alpha_Chl*(max(0.1,Ic)))
        xroh_Chl = xroh_Chlz(j) * PC*F5/86400. ! 1D-Fall
        xroh_Chlz(j) = xroh_Chlz(j) * PC/86400.
          else
            xroh_Chl = xroh_Chlz(j) * PC * F5/86400. ! 1D-Fall
            xroh_Chlz(j) = xroh_Chlz(j) * PC/86400.
        endif

      Pz(j) = PC

        sumPc = sumPc+Pz(j)*dz
        sumRoh_Chl = sumRoh_Chl + xroh_Chl(j)*dz
        sumH = sumH + dz
                                                                       
      if(ispek==1)then
        do i = 1,ilamda
          I0(i) = Iz(i)
        enddo
          else
            I0(1) = Icz
      endif
                                                                       
   enddo                                    ! Ende Schichtenschleife
                                                                       
      Pcmit = sumPc/sumH
      roh_Chlzmit = sumRoh_Chl/sumH
                                                                       
!....2D-Modellierung                                                    
!                                                                       
      if(nkzs(ior)>1)then               !    2D-Modellierung Anfang
 
       n_neu_s = nkzs(ior)
           
        iaus = 0
        i_zeiger = 1
        dz_spline = dz1
        call lin_spline (dz_spline, dH2D, deltaz, js, n_neu_s, CChlaz, YY,i_zeiger,iaus,ior)
        do nkz = 1,nkzs(ior)
          CChlaz(nkz) = YY(nkz)
        enddo
        iaus = 0

        i_zeiger = 1
        dz_spline = dz1 
        call lin_spline (dz_spline, dH2D, deltaz, js, n_neu_s, Pz, YY,i_zeiger,iaus,ior)
        do nkz = 1,nkzs(ior)
          Pz(nkz) = YY(nkz)
        enddo

        i_zeiger = 1
        dz_spline = dz1
        call lin_spline (dz_spline, dH2D, deltaz, js, n_neu_s, xroh_Chlz, YY,i_zeiger,iaus,ior)
        do nkz = 1,nkzs(ior)
          roh_Chlz(nkz) = YY(nkz)
        enddo

  endif                       ! 2D-Modellierung Ende
!                                                                       
!....Mittelwertbildung der Hemmung                                      
      sumyK = 0.0 
      sumH = 0.0

      if(js==1)then
        svhemg(ior) = 1.-hemm(1)
          else
            do j = 1,js 
              sumyK = sumyK+hemm(j)*dz 
              sumH = sumH+dz 
            enddo
            svhemg(ior) = 1.-(sumyK/sumH) 
      endif
                                                                       
      if(nkzs(ior)==1)then
        else 
!....2D-Modellierung                                                    

! ##### Nährstoffabhängigkeit des Wachstums (2D-Fall) ######    
    do nkz = 1,nkzs(ior) 
      if((Qmx_NG/Qmn_NG)<1.25)then 
        F51 = (VNO3z(nkz,ior)+VNH4z(nkz,ior))/(agksN*ft_ks+VNO3z(nkz,ior)+VNH4z(nkz,ior)) 
          else                      
            F51 = (hQ_NGz(mstr,nkz,ior)-Qmn_NG)/(Qmx_NG-Qmn_NG)
      endif

     if((Qmx_PG/Qmn_PG)<1.25)then
       F52 = gelPz(nkz,ior)/(agksp*ft_ks+gelPz(nkz,ior))
     endif
 
      F5z(nkz) = min(F51,F52) 
      if(F5z(nkz)<0.0)F5z(nkz) = 0.0 
    enddo 
 endif 

! #### BERECHNUNG DER RESPIRATIONSRATE                                   

      aggrow = Pcmit*F5 
      
      agres = aggrow * frespg + agremi*ftemp

      agrt = agr(ior)*exp(aggrow*tflie)

      dalggr(ior) = agrt-agr(ior)
      dalgag(ior) = agrt*(1.-(exp(-agres*tflie)))
      agrt = agrt - dalgag(ior) 

      if(nkzs(ior)>1)then 
!....2D-Modellierung

      sumQN = 0.0
      sumQP = 0.0
      sumH = 0.0
      do nkz = 1,nkzs(ior)            ! Schleifenbeginn 2D 
                                                                       
!     BERECHNUNG DER RESPIRATIONSRATE                                   

      aggrwz(nkz) = Pz(nkz)*F5z(nkz)

      frespgx = frespg
      if((aggrwz(nkz)-akremi*ftemp)<0.0)frespx = 0.0                                  
      
      agresz(nkz) = aggrwz(nkz) * frespg + agremi*ftemp

      agrtz(nkz) = agrz(nkz,ior)*exp(aggrwz(nkz)*tflie) 
      dalggz(nkz,ior) = agrtz(nkz)-agrz(nkz,ior) 

      algagz(nkz,ior) = agrtz(nkz)*(1.-(exp(-agresz(nkz)*tflie))) 
      agrtz(nkz) = agrtz(nkz) - algagz(nkz,ior)
                                                                 
      agrbrz(nkz,ior) = max(0.0001,agrtz(nkz)) 
                                                                       
! ##### neue Aufnahmeraten ########

      if((Qmx_PG/Qmn_PG)<=1.25)then 
        up_PGz(nkz,ior) = Qmx_PG*(max(0.0,(dalggz(nkz,ior)-algagz(nkz,ior))))/(agrbrz(nkz,ior)-algagz(nkz,ior)) 
        Q_PGz(nkz) = Qmx_PG 
          else 
                                                                       
!....Phosphor                                                           
      yk = Q_PG(ior) 
      xk = aggrwz(nkz) - agresz(nkz)
      Qmxi = Qmx_PG 
      Qmni = Qmn_PG 
      CNaehr = gelPz(nkz,ior) 
      Halbi = agksP*ft_ks 
      upmxi = upmxPG * FTA 
      jcyano = 0 
      j_aus = 0 

      call uptake(yk,xk,Qmxi,Qmni,CNaehr,Halbi,upmxi,tflie,up_Ci,up_N2i,abr,jcyano,ifix,j_aus)                                   

      up_PGz(nkz,ior) = up_Ci
      Q_PGz(nkz) = yk 
         endif 
                                                                       
!....Stickstoff                                                         
      if((Qmx_NG/Qmn_NG)<=1.25)then 
        up_NGz(nkz,ior) = Qmx_NG*(max(0.0,(dalggz(nkz,ior)-algagz(nkz,ior))))/(agrbrz(nkz,ior)-algagz(nkz,ior)) 
        hQ_NGz(mstr,nkz,ior) = Qmx_NG 
          else 

      sumN = vNH4z(nkz,ior)+vNO3z(nkz,ior) 
      yk = hQ_NGz(mstr,nkz,ior) 
      xk = aggrwz(nkz) - agresz(nkz) 
      Qmxi = Qmx_NG 
      Qmni = Qmn_NG 
      CNaehr = sumN 
      Halbi = agksN*ft_ks 
      upmxi = upmxNG * FTA

      j_aus = 0
      call uptake(yk,xk,Qmxi,Qmni,CNaehr,Halbi,upmxi,tflie,up_Ci,up_N2i,abr,jcyano,ifix,j_aus)                                   
      j_aus = 0
      up_NGz(nkz,ior) = up_Ci 
      hQ_NGz(mstr,nkz,ior) = yk 
        endif 
                                                                       
! #### Neuberechnung der Chlorophyll-a-Konzentration #####

   roh_Chlz(nkz) = roh_Chlz(nkz) * F5z(nkz)
   xagres = aggrwz(nkz) - agresz(nkz) 
   xup_N = up_NGz(nkz,ior)

   if(isyn==1)then
     roh_Chlz(nkz) = roh_Chlz(nkz)*F51
      xaC = agrz(nkz,ior)*Cagr
      xagrow = aggrwz(nkz)
      xchla = hChlgz(mstr,nkz,ior) 
    endif
 
   call C_Chla(roh_Chlz, xup_N, xagres, CChlaz, nkz, tflie, Cagr, CChl_Stern, xChla, xaC, xagrow, isyn, iaus)

   CChlazt(nkz) = CChlaz(nkz)

   if(nkz>1)then
     sumQN = sumQN + ((hQ_NGz(mstr,nkz-1,ior)+hQ_NGz(mstr,nkz,ior))/2.)*dH2D
     sumQP = sumQP + ((Q_PGz(nkz-1)+Q_PGz(nkz))/2.)*dH2D
     sumH = sumH + dH2D
   endif
    enddo                         ! Schleifenende 2D 
                                                                       
    Q_NG(ior) = min(Qmx_NG,sumQN/sumH)
    Q_PG(ior) = min(Qmx_PG,sumQP/sumH)
                                                                       
  Endif    ! Ende 2D 
                                                                     
!...neue zellulären Nährstoffgehalte                                    
!                                                                       
      agrtbr(ior) = agrt 


      if((Qmx_PG/Qmn_PG)<=1.25)then 
        up_PG(ior) = Qmx_PG*(max(0.0,(dalggr(ior)-dalgag(ior))))/(agrtbr(ior)-dalgag(ior)) 
        Q_PG(ior) = Qmx_PG 
          else 

!....Phosphor                                                           
      yk = Q_PG(ior) 
      xk = aggrow - agres 
      Qmxi = Qmx_PG 
      Qmni = Qmn_PG 
      CNaehr = gelP(ior)
      abr = agrtbr(ior) 
      Halbi = agksP*ft_ks 
      upmxi = upmxPG * FTA 
      jcyano = 0 

      call uptake(yk,xk,Qmxi,Qmni,CNaehr,Halbi,upmxi,tflie,up_Ci,up_N2i,abr,jcyano,ifix,j_aus)                                   

      up_PG(ior) = up_Ci
      Q_PG(ior) = yk 
         endif 
                                                                       
!....Stickstoff                                                         
      if((Qmx_NG/Qmn_NG)<=1.25)then 
        up_NG(ior) = Qmx_NG*(max(0.0,(dalggr(ior)-dalgag(ior))))/(agrtbr(ior)-dalgag(ior)) 
        Q_NG(ior) = Qmx_NG 
          else 
      sumN = vNH4(ior)+vNO3(ior) 
      yk = Q_NG(ior) 
      xk = aggrow - agres 
      Qmxi = Qmx_NG 
      Qmni = Qmn_NG 
      CNaehr = sumN 
      Halbi = agksN*ft_ks 
      upmxi = upmxNG * FTA

      call uptake(yk,xk,Qmxi,Qmni,CNaehr,Halbi,upmxi,tflie,up_Ci,up_N2i,abr,jcyano,ifix,j_aus)                                   

      up_NG(ior) = up_Ci
      Q_NG(ior) = yk 
         endif 

! #### Neuberechnung der Chlorophyll-a-Konzentration ####

   nkz = 1
   roh_Chlz(1) = roh_Chlzmit
   xagres = aggrow - agres
   xup_N = up_NG(ior)

   if(isyn==1)then
      xaC = agr(ior)*Cagr
      xagrow = aggrow
      xchla = chlagr(ior) 
    endif

   CChlaz(nkz) = agbcm(ior)

   iaus = 0
   call C_Chla(roh_Chlz, xup_N, xagres, CChlaz, nkz, tflie, Cagr, CChl_Stern, xChla, xaC, xagrow, isyn, iaus)

   iaus = 0 
                                                                     
      if(nkzs(ior)==1)then 
      dalggz(1,ior) = dalggr(ior) 
      agrbrz(1,ior) = agrtbr(ior) 
      up_PGz(1,ior) = up_PG(ior) 
      up_NGz(1,ior) = up_NG(ior) 
      endif 
                                                                       
                                                                       
! ##### SEDIMENTATION DER ALGEN #####                                          
!                                                                       
!....Schiffseinfluss                                                    
      ustkri = sqrt(tauscs/1000.) 
      vkrit = (ustkri*tiefe(ior)**0.166667)/((1./rau(ior))*g) 
!                                                                       
      tiefe1 = tiefe(ior) 
      if(ischif(ior)==0)then 
        v6 = 0.0 
          else 
            nschif = ischif(ior) 
            vmitt1 = vmitt(ior) 
            call schiff(vmitt1,tiefe1,v6,nschif) 
      endif
                                                                       
      vges = vmitt(ior)+v6 
                                                                       

      agrs = asgre*agr(ior) 

      ised = 1      ! Schalter zur Kennzeichnung der hier berücksichtigten partik. Wasserinhaltsstoffe
      jsed = 1
      ZellV = 300.
      call Sedimentation(ior,tiefe,ised,ust,qsgr,oc,Oc0,tflie,wst,jsed,ZellV,kontroll,jjj)

      ceq = agrs*qsgr 
                                                                       
      SEDALg(ior) = (agrs - Ceq) * oc

      sedalg0(ior) = wst*asgre*qsgr

      sedAlg_MQ(mstr,ior) = sedAlg_MQ(mstr,ior) + SEDALg(ior)
 
!      if(ieros.eq.1.and.vges.gt.vkrit)sedalg(ior) = 0.0                
                                                                      
                                                                      
!****** algenmortalitaet, abhängig vom Nährstoffgehalt der Zelle, bzw. Nährstoffangebot ******                                            
!                                                                       
      agmomi = 0.02 
      agmoma = 0.8 
      agmor = agmomi 
 
      fmor0 = 0.05

      fmor1 = f51 
      fmor2 = f52 

      if((Qmx_NG/Qmn_NG)>=1.25)then
        fmor1 = (Q_NG(ior)-Qmn_NG)/(Qmx_NG-Qmn_NG)
      endif 

      if((Qmx_PG/Qmn_PG)>=1.25)then
        fmor2 = (Q_PG(ior)-Qmn_PG)/(Qmx_PG-Qmn_PG)
      endif
   
      fmor = min(fmor1,fmor2)

      agmor = agmomi+agmoma*(1.-((min(fmor0,fmor))/fmor0)**8.)
      agmor = min(max(agmor,agmomi),agmoma) !!wy stay within limits

      if(agmor<agmor_1(mstr,ior))then
         agmor = agmor_1(mstr,ior)
      else
         agmor_1(mstr,ior) = agmor
      endif 

      dgrmor(ior) = agrt*(1.-(exp(-agmor*tflie))) 
                                                                       
!....2D-Modellierung                                                    
                                                                       
      do nkz = 1,nkzs(ior) 
        dgmorz(nkz,ior) = agrtz(nkz)*(1.-(exp(-agmor*tflie))) 
      enddo

!     Quellen/Senken-Term                                               
!     +++Gruenalgen+++                                                  
                                                                       
      hconql = dalggr(ior)+cmatgr(ior) 
      hconsk = dgrmor(ior)+dalgag(ior)+sedalg(ior)+algzog(ior)+algdrg(ior)+algcog(ior)                                          
  
      agrt = agr(ior)+hconql-hconsk
      dagr = abs(hconql-hconsk) 

      if(agrt<0.0)then
        agrt = (agr(ior)/(agr(ior)+dagr))*agr(ior)
      endif             

     Chlagrt = agrt*Cagr*1000./CChlaz(1)

     if(agrt<1.e-5)then
       agrt = 1.e-5
       Chlagrt = agrt*Cagr*1000./CChlaz(1)
     endif

    if(nkzs(ior)==1)then
      dgmorz(1,ior) = dgrmor(ior) 
      algagz(1,ior) = dalgag(ior) 
      algzgz(1,ior) = algzog(ior) 
      agrtz(1) = agrt
      chlagrzt(1) = Chlagrt 
    endif
      agbcmt = CChlaz(1) 
     
    if(nkzs(ior)>1)then !  2D-Modellierung  

    do nkz = 1,nkzs(ior) 
      hconql = dalggz(nkz,ior)+cmatgr(ior) 
      hconsk = dgmorz(nkz,ior)+algagz(nkz,ior)+sedalg(ior)+algzgz(nkz,ior)+algdrg(ior)+algcog(ior)                          
      
      CChlaz(nkz) = CChlazt(nkz)

        agrtz(nkz) = agrz(nkz,ior)+hconql-hconsk
        dagr = abs(hconql-hconsk) 

        if(agrtz(nkz)<0.0)then
          agrtz(nkz) = (agrz(nkz,ior)/(agrz(nkz,ior)+dagr))*agrz(nkz,ior)                                                    
        endif
 
        Chlagrzt(nkz) =  agrtz(nkz)*Cagr*1000./CChlaz(nkz)

        if(agrtz(nkz)<1.e-5)then
          agrtz(nkz) = 1.e-5
          Chlagrzt(nkz) =  agrtz(nkz)*Cagr*1000./CChlaz(nkz)
        endif
      
      Chlaz(nkz,ior) = chlagrzt(nkz) + hchlkz(mstr,nkz,ior) + hchlbz(mstr,nkz,ior) 
   enddo

  endif    ! Ende 2D

      chla(ior) = chlaki(ior) + chlabl(ior) +chlagrt 

      hctest = chlaki(ior) + chlabl(ior) +chlagrt
      if( hctest>1.e-10)vkigr(ior) = chlaki(ior)/(chlagrt+chlaki(ior)+chlabl(ior)) 
      if( hctest>1.e-10)antbl(ior) = chlabl(ior)/(chlagrt+chlaki(ior)+chlabl(ior))

      if(nkzs(ior)==1)Chlaz(1,ior) = chla(ior)

!...Fehlermeldung                                                       
      ifehl = 0
      if(ISNAN(chla(ior)))then
        print*,'algaesgr ISNAN(chla)',mstr,ior,chlaki(ior),chlabl(ior),chlagrt
        ifehl = 25 
        ifhStr = mstr 
        exit 
      endif 

!......Ausgaben                                                         

      agmuea(ior) =  aggrow 
      fhegas(ior) = svhemg(ior) 
      agreau(ior) = agres
!      agreau(ior) = Q_PG(ior)/Qmx_PG                                   

      if(schwi(ior)<=0.001.and.isim_end==0)then
        tpgr(ior) = 0.0
        figaus = 0.0
          else if(schwi(ior)>0.001)then
            tpgr(ior) = F52
            figaus(ior) = Pcmit/Pcmax*FTA 
              else if(isim_end==1)then
                 tpgr(ior) = F52
                 figaus(ior) = 0.0 
       endif  
                                                                       
                                                                       
  111 continue 

      agr(anze+1) = agrt 
      agbcm(anze+1) = agbcmt
      chlagr(anze+1) = chlagrt 
                                                                      
      do nkz = 1,nkzs(anze+1) 
        agrz(nkz,anze+1) = agrtz(nkz)
        hchlgz(mstr,nkz,anze+1) = chlagrzt(nkz)
        hCChlgz(mstr,nkz,anze+1) = CChlazt(nkz)
      enddo 
     endif        ! else kein Chla-Randbedingung                                                                 
                                                                       
      End subroutine algaesgr 
