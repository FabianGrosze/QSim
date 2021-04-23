      SUBROUTINE algaesbl(SCHWI,TFLIE,TEMPW,flag,elen,RAU,TIEFE,VMITT,VNO3,VNH4,GELP,svhemb,CHLA,ir                        &
                         ,dalgbl,dalgab,ior,anze,sedalb,algzob,dblmor,fkm,vabfl,abchl,abgmax,abksn,abksp,saettb,abremi     &
                         ,vco2,iph,vkigr,abbcm,abl,tpbl,uhrz,iwied,fibaus,abmuea,fhebas,abreau,tauscs,ischif,ilbuhn,ieros  &
                         ,zakie,zagre,zable,asble,qeinl,jiein,echla,ess,algdrb,algcob,antbl,zooind,GRote,SS,extk           &
                         ,extk_lamda                                                                                       &
                         ,ilamda,eta,aw,ack,acg,acb,ah,as,al       						                             	   & !!wy, Einlesen von e_extnct.dat nicht hier
                         ,vNH4z,vNO3z,gelPz,dalgbz,nkzs,dH2D,tempwz,cpfad,up_PBz,up_NBz,Qmx_PB,Qmn_PB                      &
                         ,upmxPB,Qmx_NB,Qmn_NB,upmxNB,Q_NB,Q_PB,IKbe,frmube,alamda,abltbr,ablbrz,up_N2z,ablz               &
                         ,chlabl,a1Bl,a2Bl,a3Bl,hchlbz,hCChlbz,algabz,algzbz,Dz2D,ToptB,kTemp_Bl,ifix,sedAlg_MQ            &                               
                         ,sedAlb0,hQ_NBz, mstr,itags,monats,isim_end,abmor_1,azStrs                                        &
                         ,kontroll ,jjj )              !!wy                                                                           
!                                                                       
!                                                                       
!     UNTERPROGRAMM ZUR BERECHNUNG DES BLAUALGENWACHSTUMS                  
                                                                       
                                                                       
                                                                       
!     AUTOR :VOLKER KIRCHESCH                                           
                                                                       
!     STAND :01.08.2012                                                 
                                                                       
                                                                       
!     UNTERPROGRAMME :TAGE,ALBEDO                                       
!
      logical kontroll !wy
      integer jjj !wy

      character (len=255) cpfad
      character (len=275)                       :: pfadstring 
      character (len=2) ckenn_Vers1
 
      integer                                   :: anze, azStrs
      integer, Dimension(1000)                  :: ischif, jiein, flag, nkzs

      real                                      :: LNQ, Ihemm, IKb, IKbe, kTemp_Bl, Icz, Ic0, Ic, N_Cmax, Icmit, kTresp 
      
      real, Dimension(1000)                     :: tempw, chla, vno3, elen, vnh4, gelp, ir, vco2, svhemb, dalgbl 
      real, Dimension(1000)                     :: dalgab, vabfl, vmitt, rau, tiefe, sedalb, algzob, dblmor, fkm
      real, Dimension(1000)                     :: tpbl, zooind, abbcm, abl, chlabl, fibaus, abmuea, fhebas, abreau
      real, Dimension(1000)                     :: ss, antbl, vkigr, Q_NB, Q_PB, up_PB, up_NB, up_N2
      real, Dimension(1000)                     :: algdrb, algcob, abltbr, extk, Dz2D, sedAlb0, schwi 

      real, Dimension(40)                       :: eta, aw, ack, acg, acb, ah,as, al, I0, Iz
      real, Dimension(50)                       :: chlablzt, hc_temp, Pz, F5z, abgrwz, CChlaz, CChlazt, xroh_Chlz, roh_Chlz
      real, Dimension(50)                       :: Y, YY, abltz, Q_PBz, dmorChlbz, abresz, dzMasse, Masse_neu, dzMasse0
      real, Dimension(50)                       :: xroh_Chl
      real, Dimension(100)                      :: echla, ess, qeinl, hemm

      real, Dimension(40,1000)                  :: extk_lamda 

      real, Dimension(50,1000)                  :: up_NBz, up_PBz, ablz, algzbz, ablbrz, algabz, dbmorz, up_N2z, vNH4z
      real, Dimension(50,1000)                  :: vNO3z, tempwz, gelPz, dalgbz  
      real, Dimension(azStrs,1000)              :: sedAlg_MQ, abmor_1  
      real, Dimension(azStrs,50,1000)           :: hchlbz, hQ_Nbz, hCChlbz

      save Cchlaz, ablzt 
!                                                                       
      iein = 1 
      ispek = 0
      iTemp = 1

      ifoto = 0     !Lichtabhängigkeit der Fotosyntheserate nach Ross (2009)
      ifoto = 1     !Lichtabhängigkeit der Fotosyntheserate nach Geider (1998)

      isyn = 0      ! Chlorophylla-Synthese nach Geider (1998)
      isyn = 1      ! Chlorophylla-Synthese nach Geider (1997) 
 
      if(chla(1)<0.0)then  ! falls kein Chla-Randbedingung
        do ior = 1,anze+1
          sedalb(ior) = 0.0
          Sedalb0(ior) = 0.0
          sedAlg_MQ(mstr,ior) = 0.0 
        enddo
          else ! ansonsten

!                                                                       
!      open(unit=91,file='bl.tst')                                      
!                                                                       
!                                                                       
!.....Eingaben                                                          
                                                                       
      TOPT = ToptB

      kTresp = 0.09 

      Ihemm = 600. 
      tauad = 100.  ! Relaxationszeit der Algen in sec                                                    

      Cabl = 0.48 

      IKb = IKbe 
      frespg = frmube 
      Iref = 140. 
                                                                       
      Te0 = 20.

      if(abChl>=86.)then
       abchl_max = 86.
         else
           abChl_max = 68.
           if(abChl==20.6)abChl_max = 20.6 
           if(abChl==35.)abChl_max = 35. 
       endif

!     C:Chla = abchl * exp(-a1*T) * a2 * I * exp(-a3*T)

      CChl0 = abchl * exp(-a1Bl * Te0)               ! C:Chla bei 0°C  mgC/mgChla

      if(abChl>abChl_max)then
        CChl0 = abChl_max * exp(-a1bl * Te0)
      endif


!Umrechnung der maximalen Wachstumsrate bei 20°C auf Wachstumsrate unter optimalen Temperatur-Bedingungen

       abgmaxTopt = abgmax
       if(iTemp==1)then
         abgmaxTopt = abgmax/(exp(-kTemp_Bl*(Te0-Topt)**2))
         upmxPB = abgmaxTopt * Qmx_PB               ! s. Geider (1998), Angabe pro mg Biomasse
         upmxNB = abgmaxTopt * Qmx_NB               ! Geider (1998)
        endif    

!.....Einlesen
!!wy      write(pfadstring,'(2A)')trim(adjustl(cpfad)),'e_extnct.dat'
!!wy      open(unit=101, file=pfadstring)
!!wy      rewind(101) 
!!wy      read(101,'(A2)')ckenn_vers1
!!wy      if(ckenn_vers1/='*V')then 
!!wy        else
!!wy          read(101,'(2x)') 
!!wy      endif
!!wy      read(101,'(i2)')ilamda 
!!wy    do i=1,ilamda 
!!wy      read(101,1000)eta(i),aw(i),ack(i),acg(i),acb(i),ah(i),as(i),al(i)                                                      
!!wy    enddo 
                                                                       
 1000 format(f5.1,7(2x,f8.6)) 
                                                                       
!##################################                                     
!....Beginn der Segmentschleife                                         
      do ior=1,anze+1
                                                                       
      sumac = 0.0 
      do i=1,ilamda 
        sumac = sumac+(ack(i)*vkigr(ior)+acg(i)*(1.-vkigr(ior)-antbl(ior))+acb(i)*antbl(ior))                                               
      enddo 
      acmit = sumac/ilamda 
!                                                                       
!......schwi*4.2 - Umrechnung von cal/(cm2*h) in J/(cm2*h)              
      OBFLI = 5.846*(schwi(ior)*4.2)   ! Lichtgeschwindigkeit in Luft, da µE bei Wachstumsversuchen in Luft gemessen.
      IF(OBFLI<0.001)obfli = 0.0 
                                                                       
!.....Spektrale Auflösung der photosynth. aktiven Strahlung             
                                                                       
      if(ispek==0)then
        I0(1) = obfli
          else 
           do i = 1,ilamda 
             I0(i) = obfli*al(i) 
           enddo
      endif
                                                                       
!....Nullsetzen                                                         
      do nkz = 1,nkzs(ior) 
        Pz(nkz) = 0.0 
        F5z(nkz) = 0.0 
      enddo 
                                                                       
      if(ilbuhn==1)nkzs(ior) = 1 
                                                                       
      if(ior>1)then 
      abl(ior-1) = ablt
      chlabl(ior-1) = chlablt 
      abbcm(ior-1) = abbcmt 
      do nkz = 1,nkzs(ior-1) 
        ablz(nkz,ior-1) = abltz(nkz)
        hchlbz(mstr,nkz,ior-1) = chlablzt(nkz) 
        hCChlbz(mstr,nkz,ior-1) = CChlazt(nkz)
      enddo 
      endif 
                                                                       
      if(ilbuhn.eq.1.and.tiefe(ior).lt.0.05)cycle
                                                                       
! ##### Temperaturabhaengigkeit der Respirationsrate ######                     
                                                                        
         FTEMP = exp(kTresp*(TEMPW(ior)-20.))          
                                                                       
                                                                       
! ##### Temperaturabhaengigkeit der Wachstumsrate ######                         
                                                                       
     if(iTemp==0)then 
       if(tempw(ior)>=Tmax)then 
         FTA = 0.01 
       else 
         LNQ = 0.61519 
         W = LNQ*(TMAX-TOPT) 
         X = (W**2*(1+SQRT(1+40/W))**2)/400. 
         FTA = ((TMAX-TEMPW(ior))/(TMAX-TOPT))**X 
         FTA = FTA*EXP(X*(1-((TMAX-TEMPW(ior))/(TMAX-TOPT)))) 
        endif                                                                       
      else 
        FTA = exp(-kTemp_Bl*(Tempw(ior)-Topt)**2) 
      endif 
      FTA = max(0.01,FTA) !!wy
      if(kontroll) print*,'algaesbl:Temperaturabhaengigkeit jjj, FTA,iTemp,tmax=',jjj,FTA,iTemp,tmax !!wy
                                                                       
!     Berechnung der Schubspannungsgeschwindigkeit                      
                                                                       
      FN = 1./RAU(ior) 
      G = 9.81 
      UST = ((FN*G**0.5)/TIEFE(ior)**0.166667)*abs(VMITT(ior)) 
                                                                       
!     Berechnung des mittleren vertikalen Dispersionskoeffizient        
!     nach Fischer im ein-dimensionalen Fall (gute Näherung)            
                                                                       
      a = 0.4*ust 
      dztot = a*tiefe(ior)/6. 
                                                                       
      if(nkzs(ior)==1.or.Dz2D(ior)==0.0)then
        else 
          dztot = Dz2D(ior)
      endif 

      dz = sqrt(tauad*2.*dztot) 
                                                                       
      PCmax = (abgmaxTopt+abremi * exp(kTresp*(Topt-20.)))/(1.-frespg)   ! max C-spezifische Photosyntheserate bei optimal Temperatur

!.... Berechnung der Lichtabsorption im gesamten Wasserkörper           
!                                                                       
      js = 0   !!wy
      if(dz.gt. 0.0) js = int(tiefe(ior)/dz)   !!wy Division durch Null vermeiden
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
                                                                      
      sumPc = 0.0
      sumRoh_Chl = 0.0
      sumH = 0.0

      if(nkzs(ior)>1)then
        n_neu_s = js
        n_alt_s = nkzs(ior)
        dz_spline = dz 

        do nkz = 1,nkzs(ior)
          Y(nkz) = tempwz(nkz,ior)
        enddo
        iaus  = 0  
        i_zeiger = 0
        call lin_spline (dH2D, dz_spline, deltaz, n_alt_s, n_neu_s, Y, YY,i_zeiger,iaus,ior)

        do j = 1,n_neu_s
          hc_temp(j) = YY(j)
        enddo

        iaus = 0

        dz_spline = dz 

        do nkz = 1,nkzs(ior)
            Y(nkz) = hCChlbz(mstr,nkz,ior)
        enddo 

        i_zeiger = 0
        call lin_spline (dH2D, dz_spline, deltaz, n_alt_s, n_neu_s, Y, YY,i_zeiger,iaus,ior)
        iaus = 0
        do j = 1,n_neu_s
          CChlaz(j) = YY(j)
         enddo
      endif                             

!###### Nährstoffabhängigkeit des Wachstums #######

!...Temperaturabhängigkeit von KP, KN,KSi                               
                                                                       
      fT_Ks = 1.15**(20.-tempw(ior)) 
      fT_Ks = 1.
                                                                       
      F51_N2 = 0.0 

      if((Qmx_NB/Qmn_NB)<1.25)then
        F51 = (VNO3(ior)+VNH4(ior))/(abksN*ft_ks+VNO3(ior)+VNH4(ior))                                             
        if(ifix==1)then
          F51_N2_1D = 1. - F51
          F51_1D = F51                                            
          F51 = 1.
        endif 
          else 
            F51 = (Q_NB(ior)-Qmn_NB)/(Qmx_NB-Qmn_NB)
            if(ifix==1)then
               F51_N2_1D = 1. - F51
               F51_1D = F51
               F51 = 1.
            endif
         endif
                                         
      if((Qmx_PB/Qmn_PB)<1.25)then 
        F52 = gelP(ior)/(abksp*ft_ks+gelP(ior)) 
          else 
            F52 = (Q_PB(ior)-Qmn_PB)/(Qmx_PB-Qmn_PB)
      endif
                                         
      F5 = min(F51,F52) 

      F51_1D = F51

      do j = 1,js                                  ! Schleife über die Schichten Anfang 

      dz1 = dz
      if(j==js.and.deltaz>0.0)dz = deltaz 
      Ic = 0.0 
      if(ispek==1)then
        do i = 1,ilamda 
          Iz(i) = I0(i)*exp(-extk_lamda(i,ior)*dz)
          Ic = Ic + max(0.0,(I0(i)/(extk(ior)*dz))*(1.-exp(-extk(ior)*dz)))
        enddo
          else
            Icz = I0(1)*exp(-extk(ior)*dz)
            Ic = max(0.0,(I0(1)/(extk(ior)*dz))*(1.-exp(-extk(ior)*dz)))
      endif

      if(nkzs(ior)==1)then
        hc_temp(j) = tempw(ior)
        CChlaz(j) = abbcm(ior)
          else
      endif
                 
      yK = 1.-svhemb(ior) 

      CChl_Stern = CChl0 * exp(a1Bl * hc_temp(j))    ! dunkeladaptierte Algen

      call LichtHemmung(tflie,Ic,yK,CChl_Stern,CChlaz,j)                                             

      hemm(j) = yK

      Saettb = Ikb * 0.525*exp(0.0322*hc_temp(j))
      if(IKb==191.)Saettb = Ikb * 3.9*exp(-0.068*hc_temp(j))

      alpha_chl = PCmax*FTA*CChl_Stern/(Saettb*86400.) 

      if(ifoto==0)then
        Pc = Pcmax*FTA*(1.-exp(-Ic/Saettb))*hemm(j)
          else if(ifoto==1)then
            Pc = Pcmax*FTA*(1.-exp(-Ic*alpha_Chl/(max(CChl_Stern,CChlaz(j))*Pcmax*FTA/86400.)))*hemm(j)
      endif 

! #### Berechnung roh_Chl (wird für die Neuberechnung der Chlorophyll-a-Konzentration benötigt) #####

      N_Cmax = Qmx_NB/Cabl

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
                                                                       
    enddo                                   ! Ende Schleife                                                                     
                                                                       
      Pcmit = sumPc/sumH 
      roh_Chlzmit = sumRoh_Chl/sumH
      
!....2D-Modellierung
                                                                       
      if(nkzs(ior)>1)then 

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
        sumH = 0.0
        sumRoh_Chl = 0.0 
        call lin_spline (dz_spline, dH2D, deltaz, js, n_neu_s, xroh_Chlz, YY,i_zeiger,iaus,ior)
        do nkz = 1,nkzs(ior)
          roh_Chlz(nkz) = YY(nkz)
        enddo
 endif 
!....Mittelwertbildung der Hemmung                                      
      sumyK = 0.0 
      sumH = 0.0 
      
      if(js==1)then
        svhemb(ior) = 1.-hemm(1)
          else
            do j = 1,js 
              sumyK = sumyK+hemm(j)*dz 
              sumH = sumH+dz 
          enddo
          svhemb(ior) = 1.-(sumyK/sumH) 
      endif
                                                                       
      if(nkzs(ior)==1)then
        else  
!....2D-Modellierung                                                    
    do nkz = 1,nkzs(ior)
! #### Nährstoffabhängigkeit (2D-Fall) #######
 
      F51_N2 = 0.0 

      if((Qmx_NB/Qmn_NB)<1.25)then
        F51 = (VNO3z(nkz,ior)+VNH4z(nkz,ior))/(abksN*ft_ks+VNO3z(nkz,ior)+VNH4z(nkz,ior))                      
        if(ifix==1)then
          F51_N2 = 1. - F51                                  
          F51 = 1.
        endif
          else
            F51 = (hQ_NBz(mstr,nkz,ior)-Qmn_NB)/(Qmx_NB-Qmn_NB)
              if(ifix==1)then
                F51_N2 = 1. - F51
                F51 = 1.
              endif 
      endif
 
      if((Qmx_PB/Qmn_PB)<1.25)then 
        F52 = gelPz(nkz,ior)/(abksp*ft_ks+gelPz(nkz,ior)) 
      endif

      f5z(nkz) = min(F51,F52)
      if(f5z(nkz)<0.0)f5z(nkz) = 0.0  
    enddo 
      endif 
                                                                      
      abgrow = Pcmit*F5 

! #### BERECHNUNG DER RESPIRATIONSRATE ####                                  
                                                                       
      abgrow = Pcmit*F5 
                                                                       
      abres = abgrow * frespg + abremi*ftemp 
                                                                       
      ablt = abl(ior)*exp(abgrow*tflie) 
      dalgbl(ior) = ablt-abl(ior)

      dalgab(ior) = ablt*(1.-(exp(-abres*tflie))) 
      ablt = ablt - dalgab(ior) 
 

      if(nkzs(ior)>1)then    ! goto 205 
!....2D-Modellierung                                                    
                                                                       
      sumQN = 0.0
      sumQP = 0.0
      sumH = 0.0

      do nkz = 1,nkzs(ior)            ! Schleifenbeginn 2D
 
! #### BERECHNUNG DER RESPIRATIONSRATE (2D-Fall) #####                                 

       abgrwz(nkz) = Pz(nkz)*F5z(nkz)
                                                                       
       abresz(nkz) = abgrwz(nkz) * frespg + abremi*ftemp
                                                                       
      abltz(nkz) = ablz(nkz,ior)*exp(abgrwz(nkz)*tflie) 

      dalgbz(nkz,ior) = abltz(nkz)-ablz(nkz,ior)
      algabz(nkz,ior) = abltz(nkz)*(1.-(exp(-abresz(nkz)*tflie))) 

      abltz(nkz) = abltz(nkz) - algabz(nkz,ior)
                                                                      
! ##### neue zellulären Nährstoffgehalte #####                                    
      ablbrz(nkz,ior) = abltz(nkz) 
                                                                       
      if((Qmx_PB/Qmn_PB).le.1.25)then 
        up_PBz(nkz,ior) = Qmx_PB*(max(0.0,(dalgbz(nkz,ior)-algabz(nkz,ior))))/(ablbrz(nkz,ior)-algabz(nkz,ior)) 
        Q_PBz(nkz) = Qmx_PB 
          else 

!....Phosphor                                                           
      yk = Q_PB(ior) 
      xk = abgrwz( nkz) - abresz(nkz) 
      Qmxi = Qmx_PB 
      Qmni = Qmn_PB 
      CNaehr = gelPz(nkz,ior) 
      Halbi = abksP*ft_ks 
      upmxi = upmxPB * FTA
      jcyano = 0 

      j_aus = 0
      call uptake(yk,xk,Qmxi,Qmni,CNaehr,Halbi,upmxi,tflie,up_Ci,up_N2i,abr,jcyano,ifix,j_aus)                                   

      up_PBz(nkz,ior) = up_Ci
      Q_PBz(nkz) = yk 
         endif 
                                                                       
!....Stickstoff                                                         
      jcyano = 1 
      sumN = vNH4z(nkz,ior)+vNO3z(nkz,ior) 
      if((Qmx_NB/Qmn_NB).le.1.25)then 
        up_NBz(nkz,ior) = Qmx_PB*(max(0.0,(dalgbz(nkz,ior)-algabz(nkz,ior))*(F51-F51_N2)))/(ablbrz(nkz,ior)-algabz(nkz,ior)) 
        up_N2z(nkz,ior) = Qmx_PB*(max(0.0,(dalgbz(nkz,ior)-algabz(nkz,ior))*F51_N2))/(ablbrz(nkz,ior)-algabz(nkz,ior)) 
        if(up_N2z(nkz,ior).lt.0.0)up_N2z(nkz,ior) = 0.0 
        hQ_NBz(mstr,nkz,ior) = Qmx_NB 
          else 
      yk = hQ_NBz(mstr,nkz,ior) 
      xk = abgrwz(nkz) - abresz(nkz) 
      Qmxi = Qmx_NB 
      Qmni = Qmn_NB 
      CNaehr = sumN 
      Halbi = abksN*ft_ks 
      upmxi = upmxNB * FTA

      call uptake(yk,xk,Qmxi,Qmni,CNaehr,Halbi,upmxi,tflie,up_Ci,up_N2i,abr,jcyano,ifix,j_aus)
                                   
      up_NBz(nkz,ior) = up_Ci
      up_N2z(nkz,ior) = up_N2i 
      hQ_NBz(mstr,nkz,ior) = yk 
         endif 

      xup_N = up_NBz(nkz,ior) + up_N2z(nkz,ior)

! #### Neuberechnung der Chlorophyll-a-Konzentration #####

   roh_Chlz(nkz) = roh_Chlz(nkz) * F5z(nkz)
   xabres = abgrwz(nkz) - abresz(nkz)
   xup_N = up_NBz(nkz,ior) + up_N2z(nkz,ior)

   if(isyn==1)then
      roh_Chlz(nkz) = roh_Chlz(nkz) * F51
      xaC = ablz(nkz,ior)*Cabl
      xagrow = abgrwz(nkz)
      xchla = hChlbz(mstr,nkz,ior)
    endif

   iaus=0

   call C_Chla(roh_Chlz, xup_N, xabres, CChlaz, nkz, tflie, Cabl, CChl_Stern, xChla, xaC, xagrow, isyn, iaus)

   CChlazt(nkz) = CChlaz(nkz)

   if(nkz>1)then
     sumQN = sumQN + ((hQ_NBz(mstr,nkz-1,ior)+hQ_NBz(mstr,nkz,ior))/2.)*dH2D
     sumQP = sumQP + ((Q_PBz(nkz-1)+Q_PBz(nkz))/2.)*dH2D
     sumH = sumH + dH2D
   endif
                                                                       
   enddo                   ! Schleifenende 

    
    Q_NB(ior) = min(Qmx_NB,sumQN/sumH)
    Q_PB(ior) = min(Qmx_PB,sumQP/sumH)
                                                                       
  Endif  ! Ende 2D 
                                                                       
                                                                       
      abltbr(ior) = ablt 

      sumN = vNH4(ior)+vNO3(ior) 

      if((Qmx_PB/Qmn_PB).le.1.25)then 
        up_PB(ior) = Qmx_PB*(max(0.0,(dalgbl(ior)-dalgab(ior))))/(abltbr(ior)-dalgab(ior))
        Q_PB(ior) = Qmx_PB 
        else 
!....Phosphor                                                           
      yk = Q_PB(ior) 
      xk = abgrow - abres 
      Qmxi = Qmx_PB 
      Qmni = Qmn_PB 
      CNaehr = gelP(ior) 
      abr = abltbr(ior) 
      Halbi = abksP*ft_ks 
      upmxi = upmxPB * FTA
      jcyano = 0 
      j_aus = 0
      call uptake(yk,xk,Qmxi,Qmni,CNaehr,Halbi,upmxi,tflie,up_Ci,up_N2i,abr,jcyano,ifix,j_aus)                                   

      up_PB(ior) = up_Ci
      Q_PB(ior) = yk 
         endif 
!                                                                       
!....Stickstoff                                                         
      jcyano = 1 

      if((Qmx_NB/Qmn_NB).le.1.25)then 
        up_NB(ior) = Qmx_NB*(max(0.0,(dalgbl(ior)-dalgab(ior))*(F51_1D-F51_N2_1D)))/(abltbr(ior)-dalgab(ior))
        up_N2(ior) = Qmx_NB*(max(0.0,(dalgbl(ior)-dalgab(ior))*F51_N2_1D))/(abltbr(ior)-dalgab(ior))
        if(up_N2(ior).lt.0.0)up_N2(ior) = 0.0 
        Q_NB(ior) = Qmx_NB 
          else 
      yk = Q_NB(ior) 
      xk = abgrow - abres
      Qmxi = Qmx_NB 
      Qmni = Qmn_NB 
      CNaehr = sumN 
      Halbi = abksN*ft_ks 
      upmxi = upmxNB * FTA

      call uptake(yk,xk,Qmxi,Qmni,CNaehr,Halbi,upmxi,tflie,up_Ci,up_N2i,abr,jcyano,ifix,j_aus)                                   

   
      up_NB(ior) = up_Ci
      up_N2(ior) = up_N2i 
      Q_NB(ior) = yk 
         endif

! ##### Neuberechnung der Chlorophyll-a-Konzentration (1D-Fall) #####

   roh_Chlz(1) = roh_Chlzmit

   nkz = 1
   xabres = abgrow - abres
   xup_N = up_NB(ior) + up_N2(ior)

   if(isyn==1)then
      xaC = abl(ior)*Cabl
      xagrow = abgrow
      xchla = chlabl(ior) 
    endif

   CChlaz(nkz) = abbcm(ior)

   iaus = 0
   call C_Chla(roh_Chlz, xup_N, xabres, CChlaz, nkz, tflie, Cabl, CChl_Stern, xChla, xaC, xagrow, isyn, iaus)
   iaus = 0
                                                                     
      if(nkzs(ior)==1)then 
      dalgbz(1,ior) = dalgbl(ior) 
      ablbrz(1,ior) = abltbr(ior) 
      up_PBz(1,ior) = up_PB(ior) 
      up_NBz(1,ior) = up_NB(ior) 
      up_N2z(1,ior) = up_N2(ior) 
      endif 
                                                                       
                                                                       
! #### SEDIMENTATION DER ALGEN #####                                          
                                                                       
!....Schiffseinfluss                                                    
      ustkri = sqrt(tauscs/1000.) 
      vkrit = (ustkri*tiefe(ior)**0.166667)/((1./rau(ior))*g) 
!                                                                       
      tiefe1 = tiefe(ior) 
      if(ischif(ior).eq.0)then 
      v6 = 0.0 
      goto 33 
      endif 
      nschif = ischif(ior) 
      vmitt1 = vmitt(ior) 
      call schiff(vmitt1,tiefe1,v6,nschif) 
!                                                                       
   33 vges = vmitt(ior)+v6 
                                                                       
                                                                       
      abls = asble*abl(ior) 

      ised = 1      ! Schalter zur Kennzeichnung der hier berücksichtigten partik. Wasserinhaltsstoffe
      jsed = 1
      ZellV = 1000.
     call Sedimentation(ior,tiefe,ised,ust,qsgr,oc,Oc0,tflie,wst,jsed,ZellV,kontroll,jjj)

      ceq = abls*qsgr 
                                                                       
      SEDALb(ior) = (abls - Ceq) * oc

      sedalb0(ior) = wst*asble*qsgr
      sedAlg_MQ(mstr,ior) = sedAlg_MQ(mstr,ior) + SEDALb(ior)
 
      if(ieros.eq.1.and.vges.gt.vkrit)sedalb(ior) = 0.0 
                                                                      
!     neu!! algenmortalitaet                                            
                                                                       
      abmomi = 0.02 
      abmoma = 0.8
      abmor = abmomi
      
      fmor0 = 0.05
                                                                       
      fmor1 = f51 
      fmor2 = f52 

      if((Qmx_NB/Qmn_NB)>=1.25)then
        fmor1 = (Q_NB(ior)-Qmn_NB)/(Qmx_NB-Qmn_NB)
        if(ifix==1)fmor1 = 1.
      endif
                                         
      if((Qmx_PB/Qmn_PB)>=1.25)then 
        fmor2 = (Q_PB(ior)-Qmn_PB)/(Qmx_PB-Qmn_PB)
      endif
                                         
      fmor = min(fmor1,fmor2) 

      abmor = abmomi+abmoma*(1.-((min(fmor0,fmor))/fmor0)**8.)
      abmor = min(max(abmor,abmomi),abmoma) !!wy stay within limits

      if(abmor<abmor_1(mstr,ior))then
         abmor = abmor_1(mstr,ior)
      else
         abmor_1(mstr,ior) = abmor
      endif 

      dblmor(ior) = ablt*(1.-(exp(-abmor*tflie))) 
                                                                       
!.....2D-Modellierung                                                   
      do nkz = 1,nkzs(ior) 
        dbmorz(nkz,ior) = abltz(nkz)*(1.-(exp(-abmor*tflie))) 
      enddo

!     Quellen/Senken-Term                                               
!     +++Kieselalgen+++                                                 
                                                                       
      hconql = dalgbl(ior) 
      hconsk = dblmor(ior)+dalgab(ior)+sedalb(ior)+algzob(ior)+algdrb(ior)+algcob(ior) 

      ablt = abl(ior)+hconql-hconsk
      dabl = abs(hconql-hconsk)

      if(ablt<0.0)then
        ablt = (abl(ior)/(abl(ior)+dabl))*abl(ior)
      endif             

     Chlablt = ablt*Cabl*1000./CChlaz(1)

     if(ablt<1.e-5)then
       ablt = 1.e-5
       Chlablt = ablt*Cabl*1000./CChlaz(1)
     endif  

    if(nkzs(ior)==1)then
      dbmorz(1,ior) = dblmor(ior) 
      algabz(1,ior) = dalgab(ior) 
      algzbz(1,ior) = algzob(ior) 
      abltz(1) = ablt
      chlablzt(1) = Chlablt 
    endif
     abbcmt = CChlaz(1)


    if(nkzs(ior)>1)then 
 
      do nkz = 1,nkzs(ior) 
        hconql = dalgbz(nkz,ior) 
        hconsk = dbmorz(nkz,ior)+algabz(nkz,ior)+sedalb(ior)+algzbz(nkz,ior)+algdrb(ior)+algcob(ior)                          

          CChlaz(nkz) = CChlazt(nkz)

        abltz(nkz) = ablz(nkz,ior)+hconql-hconsk 
        dabl = abs(hconql-hconsk)

        if(abltz(nkz)<0.0)then
          abltz(nkz) = (ablz(nkz,ior)/(ablz(nkz,ior)+dabl))*ablz(nkz,ior)
        endif                                                    

        Chlablzt(nkz) =  abltz(nkz)*Cabl*1000./CChlaz(nkz)

        if(abltz(nkz)<1.e-5)then
          abltz(nkz) = 1.e-5
          Chlablzt(nkz) =  abltz(nkz)*Cabl*1000./CChlaz(nkz)
        endif

     enddo 
    endif      ! Ende 2D
                                                                       
!Ausgaben                                                               
      abmuea(ior) =  abgrow 
      fhebas(ior) = svhemb(ior) 
      abreau(ior) = abres                                              
!      abreau(ior) = abbcm(ior) 

      if(schwi(ior)<=0.001.and.isim_end==0)then
        tpbl(ior) = 0.0
        fibaus = 0.0
          else if(schwi(ior)>0.001)then
            tpbl(ior) = F52
            fibaus(ior) = Pcmit/Pcmax*FTA 
              else if(isim_end==1)then
                tpbl(ior) = F52
                fibaus(ior) = 0.0 
       endif  

                                                                       
    Enddo     ! Ende Segmentschleife 

      abl(anze+1) = ablt
      chlabl(anze+1) = chlablt 
      abbcm(anze+1) = abbcmt 
                                                                       
      do nkz = 1,nkzs(anze+1) 
        ablz(nkz,anze+1) = abltz(nkz)
        hchlbz(mstr,nkz,anze+1) = chlablzt(nkz) 
        hCChlbz(mstr,nkz,anze+1) = CChlazt(nkz) 
      enddo
   endif

   END Subroutine algaesbl                                           
