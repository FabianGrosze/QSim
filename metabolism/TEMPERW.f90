      SUBROUTINE temperw(RO,TEMPL,TEMPW,SCHWI,WGE,TIEFE,TFLIE,flag,elen,ior,anze,etemp,ewaerm,typ,qeinl,vabfl    &
                         ,jiein,cloud,typw,iwied,uhrz,ilbuhn,nwaerm,fkm,nkzs,tempwz,dH2D,iorLa,iorLe,ieinLs,flae &
                         ,qeinlL,etempL,mstr,IDWe,ilang,dtemp,FluxT1,extk,itags,monats,Tsed,Wlage,hWS,iRHKW      &
                         ,htempw,htempz,WUEBKS,SPEWKSS,PSREFSS,extkS,ifehl,ifhStr,azStrs,iwsim                   &                                                   
                         ,kontroll ,jjj ) !!wy  
!
!
!
!     EIN PROGRAMM ZUR BERECHNUNG DER WASSERTEMPERATUR
!
!
!     AUTOR : VOLKER KIRCHESCH
!
!
!
!     STAND : 19.11.1987
!
!
!     Parameterliste
!     --------------
!
!     VABFL  : Abfluss im Vorfluter m3/s
!     EL     : Dampfdruck der Luft in mm Hg od. *1.3333 in mbar
!     EW     : Dampfdruck des Wassers in mm Hg od. *1.3333 in mbar
!     stbk   : Stefan-Boltzmann-Konstante in KJ/(m2*h*k**4)
!              (2.0411e-7)
!     SCHWI  : Globalstrahlung in cal/(cm2*h) 
!     A      : Ausstrahlung
!     G      : Gegenstrahlung
!     HR     : Verdunstungshoehe in mm/d
!     VDW    : Verdunstungswaerme in Kcal/Kg
!     WV     : Waermestromdichte durch Verdunstung in cal/cm2/h
!     ROH2O  : Dichte des Wassers (1000.[Kg/m3])
!     SDDW   : Saettigungsdampfdruck bei Wassertemperatur an der
!              Wasseroberflaeche [hPa]
!     SDTT   : Saettigungsdampfdruck bei Lufttemperatur [hPa]
!     PDLTT  : Partialdampfdruck der Luft bei der Temperatur TT [hPa]
!     speWKW : spezifische Wärmekapazität des Wassers in KJ/(Kg*K)
!     speWKS : spezifische Wärmekapazität des Sediments in KJ/(Kg*K)
!     rohS   : Dichte des Sediments Kg/m3
!     WUEBK  : Wärmeübergangskoeffizient in KJ/(K*m2*h)
!     APARS  : Anteil PARS an der Globalstrahlung
!     EWAERM : Waermeeinleitung MJ/s (wird hier in Mcal/s umgerechnet "/4.2")
!     ETEMP  : Temperatur des Einleiters/Nebengewässers iein
!
!     ieinLs : Anzahl der Linienquellen im Strang (mstr)
!     ieinL  : Laufvariable der Linienquellen
!     iorLa	 : AnfangsKnoten der Linienquelle ieinL des Strangs mstr
!     iorLe  : EndKnoten der Linienquelle ieinL des Strangs mstr  
!
!
!....Bei der Simulation eines Tracer-Durchgangs wird automatisch die Einleiterkonz.
!....auf 0 gesetzt.
!


      logical kontroll !!wy
      integer jjj !!wy
integer                         :: anze, azStrs

integer, Dimension(100)         :: iorLa, iorLe, typ
integer, Dimension(azStrs)      :: ieinls
integer, Dimension(1000)        :: flag, jiein, nkzs
integer, Dimension(azStrs,1000) :: IDWe

real                            :: LageM, hctemp1
Real, Dimension(20)             :: RO, WGE, typw, cloud
real, Dimension(50)             :: D, Cpart, hctemp_2d, hctemp1z
real, Dimension(1000)           :: tempw, vabfl, fkm, flae, tiefe, elen, schwi, Templ, fluxT1, extk, Tsed 
real, Dimension(100)            :: qeinlL, etempL, etemp, qeinl, ewaerm
real, Dimension(50,1000)        :: tempwz, hctemz, dtemp 
real, Dimension(azStrs,1000)    :: Wlage, hWS, htempw, WUEBKS, SPEWKSS, PSREFSS, extkS  
real, Dimension(azStrs,50,1000) :: htempz

save hctemp1,hctemp1z

!      open(unit=23,file='temp.tst')
!
!     Konstanten
!    
!      
!....Schalter: mSed = 1 > Sedimenteinfluss auf die Temperatur
      mSed = 1

      jiter = 0
      stbk = 2.0411e-7
      roh2o = 1000.

      PSREFS0 = 0.8
      SedPV = 0.5
      speWKW = 4.187
      speWKS0 = 0.8
      rohS = 1825.
      WUEBK0 = 350.
      APARS = 0.45

      iein = 1

!....Berücksichtigung der Linienquelle
    
      do ieinL = 1, ieinLs(mstr)
        if(qeinlL(ieinL)>=0.0.and.etempL(ieinL)==-9.99)cycle
        do ior = 1,anze+1
          if(iorLe(ieinL)<ior)cycle
          if(iorLa(ieinL)<=ior.and.iorLe(ieinL)>=ior)then
            if(qeinlL(ieinL)<=0.0)qeinlL(ieinL) = 0.0
               do nkz = 1,nkzs(ior)  ! 2D
                 tempwz(nkz,ior) = tempwz(nkz,ior)+((etempL(ieinL)-tempwz(nkz,ior))*qeinlL(ieinL)/flae(ior))*tflie*86400.
               enddo
               tempw(ior) = tempw(ior)+((etempL(ieinL)-tempw(ior))*qeinlL(ieinL)/flae(ior))*tflie*86400.  ! 1D
                 else
          endif
       enddo ! Ende Knotenschleife 
     enddo   ! Ende Schleife Linienquellen


   do j = 1,anze+1 ! Beginn Knotenschleife
   ilauf = 0

   ior_flag = 0

   ior = j

   itimes = 1
   Tsed_vor = Tsed(ior)

   WUEBK = WUEBK0
   speWKS = speWKS0
   PSREFS = PSREFS0  

   if(WUEBKS(mstr,ior)>0.0)WUEBK =  WUEBKS(mstr,ior)  
   if(SPEWKSS(mstr,ior)>0.0)SPEWKS = SPEWKSS(mstr,ior)  
   if(PSREFSS(mstr,ior)>0.0)PSREFS =  PSREFSS(mstr,ior)  

!...Windkorrektur

!.... zWmess Höhe der Windmessung, Lage der Wetterstation
!.... zW2 Wind in 2 m Höhe über der Wasseroberfläche

      zW2 = 2.
      if((WLage(mstr,ior)-hWS(mstr,ior))<=0.0)then
      fkWind = 1.
        else
      zWmess = WLage(mstr,ior)-hWS(mstr,ior)
      fkWind = (zW2/zWmess)**0.11
        endif

      if(extk(ior)<=0.0.and.extkS(mstr,ior)>0.0)extk(ior) = extkS(mstr,ior)
      if(extk(ior)<=0.0.and.extkS(mstr,ior)<=0.0)extk(ior) = 1.5  ! 0.17 reines Wasser; 0.13 Schwebstoffe; 0.094 Ki; 0.0145 Gr

      btiefe = tiefe(ior)
      if(Btiefe<0.01)Btiefe = 0.01

!### hctemp1 wird bei der Berechnung der Temperatur nach Wärmeeinleitung benutzt, falls sich
!    die Temperatur an der Einleitstelle auch nach Oberstrom ausdehnt.

      if(vabfl(ior)>=0.0.and.vabfl(ior+1)<0.0)then
        hctemp1 = tempw(ior)
         do nkz = 1,nkzs(ior)
           hctemp1z(nkz) = tempwz(nkz,ior)
         enddo

      endif                                                 
                                                                    
                                                                        
      if(flag(ior)==6.and.vabfl(ior)<0.0.and.vabfl(ior+1)>0.0)then  ! An der Einleitstelle verteilt sich die eingeleitete
                                                                    ! Temperatur ober- und unterstromig  
        ior = ior+1
        ior_flag = 1
      endif


      if(ilbuhn==1)then    ! Buhnen sind vorhanden, keine Einleitung in ein Buhnenfeld
        nkzs(ior) = 1
          else if(flag(ior)==4.and.ilbuhn==0)then  ! Berücksichtigung der Einleitungen
            m = 1
            ihcQ = 0
            if(vabfl(ior-1)<0.0.and.vabfl(ior)<0.0)m = -1
            if(vabfl(ior-1)<0.0.and.vabfl(ior)>0.0)ihcQ = 1

            do nkz=1,nkzs(ior)
              hctemz(nkz,ior) = tempwz(nkz,ior-m) ! hc.. Hilfsgrößen, Umbenennung der Variablen 
            enddo

            hctemp = tempw(ior-m)
             

            hcQ = vabfl(ior-m)
            if(hcQ.lt.0.0)hcQ = abs(hcQ)
            if(hcQ==0.0.or.ihcQ==1)then
               Q_ein = hcQ
               hcQ = 1.e-10
            endif 

              if(ihcQ==1)then
                hctemp = hctemp1  
                do nkz = 1, nkzs(ior)
                  hctemz(nkz,ior) = hctemp1z(nkz)
                enddo
              endif

            do ji=1,jiein(ior) ! Beginn Einleiterschleife
              hcWE = 0.0
              hcQE = max(0.0,qeinl(iein))

              if(iwsim==5)ewaerm(iein)=-9999.9

              if(ewaerm(iein)>-9999.9)then  ! Waermeeinleitung 
               
               hcWE = ewaerm(iein)/4.2 
                 if(hcQE==0.0)then
                 tempw(ior) = hctemp+hcWE/(hcQ+Q_ein)   ! 1D

                 Do nkz = 1,nkzs(ior)                   ! 2D 
                   tempwz(nkz,ior) = hctemz(nkz,ior)+hcWE/(hcQ+Q_ein)
                 enddo
                    else
                      deltTW = hcWE/(hcQ+hcQE)

                      tempw(ior) = hctemp+deltTW  ! 1D

                      hcTE = (tempw(ior)*(hcQ+hcQE)-hcQ*hctemp)/hcQE

                      rohE = Dichte_1D(hcTE) ! Dichte im Wärmeeinleiter

                      call Dichte(hctemz,nkzs,D,ior,itags,uhrz,fkm) ! Dichte im Vorfluter

 
                      call Einleiter_Misch(nkzs,ior,hctemz,Cpart,hcQ,hcQE,hcTE,rohE,D,dH2D)  ! Berechnung der vertikalen Einmischung
                      tempwz(1:nkzs(ior),ior) = Cpart(1:nkzs(ior))
                 endif
               else    ! Temperatureinleitung         
                 hcTE = etemp(iein)
                 if(iwsim==5.and.etemp(iein)<0.0)hcTE = hctemp

              if(iwsim/=5)then
                 if(hcTE<-9.8)then
                   hcTE = hctemp
                   if(nkzs(ior)>1)then
                     do nkz = 1,nkzs(ior)
                       tempwz(nkz,ior) = hctemz(nkz,ior)
                     enddo
                   endif
                     else

                 rohE = Dichte_1D(hcTE) ! Dichte im Einleiter

                 call Dichte(hctemz,nkzs,D,ior,itags,uhrz,fkm) ! Dichte im Vorfluter

                 call Einleiter_Misch(nkzs,ior,hctemz,Cpart,hcQ,hcQE,hcTE,rohE,D,dH2D)  ! Berechnung der vertikalen Einmischung

                 tempwz(1:nkzs(ior),ior) = Cpart(1:nkzs(ior))
               endif
              endif                 
               tempw(ior) = (hcQ*hctemp+hcTE*hcQE)/(hcQ+hcQE)   ! 1D
            endif

              hcQ = hcQ+hcQE
              iein = iein+1

              do nkz = 1,nkzs(ior)
                hctemz(nkz,ior) = tempwz(nkz,ior)
              enddo

              hctemp = tempw(ior)
  
           enddo  ! Ende Einleiterschleife
           if(ior_Flag==1)then
             iein = iein - jiein(ior) 
             ior = ior-1
             tempw(ior) = tempw(ior+1)
             do nkz = 1,nkzs(ior)
               tempwz(nkz,ior) = tempwz(nkz,ior+1)
             enddo
           endif 

          endif   ! Ende Einleiter-flag 

      if(ior>1)then
        tempw(ior-1) = tempmt
        tempwz(1,ior-1) = tempwt
      endif

      if(iwsim==5)then
        tempmt = tempw(ior)
        cycle
      endif       

      TEMPW1 = TEMPWz(1,ior) 
      TEMPW2 = TEMPWz(1,ior)
      tempmv = tempw(ior)

      DTEMP1 = 0.0

!       1.STRAHLUNGSBILANZ

!     Beruecksichtigung der Wolkenart

      if(typw(IDWe(mstr,ior))==0)wtyp = 0.0
      if(typw(IDWe(mstr,ior))==1)wtyp = 0.04
      if(typw(IDWe(mstr,ior))==2)wtyp = 0.08
      if(typw(IDWe(mstr,ior))==3)wtyp = 0.17
      if(typw(IDWe(mstr,ior))==4.or.typw(IDWe(mstr,ior))==5)wtyp = 0.2
      if(typw(IDWe(mstr,ior))>5)wtyp = 0.25

      if(typw(IDWe(mstr,ior))<0)wtyp = 0.17 ! keien Eingabe des Wolkentyps

      cls = 1.+wtyp*(cloud(IDWe(mstr,ior))/8.)**2.6
!

  999 do itime = 1,itimes
      
      do jiter = 1,50 ! Beginn Iterationsschleife

      G = 9.37e-6*stbk*(templ(ior)+273.16)**6   
      G = G*cls*0.97
      G = G/42.     ! Umrechnung KJ/m2/h in cal/cm2/h
      A = 0.97*stbk*((TEMPW1+273.16)**4)                       
      A = A/42.

      PARSab = schwi(ior)*APARS*(1.-exp(-extk(ior)*Btiefe))
      WB = schwi(ior)*(1.-APARS)+PARSab+G-A

!      2. WARMEVERLUST DURCH VERDUNSTUNG

      ATkor = exp(-9.81*hWS(mstr,ior)/(287.*(templ(ior)+273.16)))
      sddw = 6.1078*exp(17.08085*tempw1/(234.175+tempw1))
      sdtt = 6.1078*exp(17.08085*templ(ior)/(234.175+templ(ior)))
      pdltt = ro(IDWe(mstr,ior))*sdtt/100.

!...Formel nach RIMSHAW & Donschenko (FGSM-Handbuch)
!      fwind = 0.211+0.1030*wge(IDWe(mstr,ior))*fkWind
!...Formel nach WMO (FGSM-Handbuch)
!      fwind = 0.13+0.0936*wge(IDWe(mstr,ior))*fkWind
      fwind = 0.153+0.063*wge(IDWe(mstr,ior))*fkWind  ! Sweers (1976) over Land
      HR = fwind*(sddw-pdltt)*ATkor

!....Umrechnung der Verdunstungshoehe von mm/d in m/h 

      HR = HR/24000.
      VDW = 595.24-0.569*TEMPW1                                          
      WV = roh2o*VDW*HR
!...  Umrechnung der Verdunstungswaerme von Kcal/(m2*h) in cal/(cm2*h)

      WV = WV*0.1

!       3.KONVEKTION

      if((sddw-pdltt)==0.0)sddw = sddw+0.001
      WL = ((TEMPW1-TEMPL(ior))/(1.53*(sddw-pdltt)))*WV

      DT1D =(1./(BTIEFE*100.))*(WB-WV-WL)*TFLIE*24./itimes
      DT2D =(1./(dH2D*100.))*(WB-WV-WL)*TFLIE*24./itimes
            
      dTW = 0.0

    if(mSed==0)then ! 0 ohne Sedimenteinfluss
      else

!.....Einfluss des Sediments
!.... Änderung der Sedimenttemperatur
      hconS = sedPV*roh2o*speWKW+(1-sedPV)*rohS*speWKS
      sedHW = Btiefe*(hconS/(roh2o*speWKW))
      if(sedHW.lt.0.6)sedHW = 0.6
!      if(sedHW.gt.Btiefe)sedHW = Btiefe      
!...  PARS an der Sohle
      SchwiS = (Schwi(ior)*APARS)*exp(-extk(ior)*Btiefe)
!...  Umrechnung von SchwiS in KJ/(m2*h)
      SchwiS = SchwiS*42.
!...  Wärmestrom aus Temperaturdifferenz zwischen Wasser und Sediment
      
      TH2O = tempw1
      if(nkzs(ior).gt.1)TH2O = tempw(ior)
      WSTRWS = WUEBK*(TH2O-Tsed(ior))
      dTsed = (SchwiS*(1.-PSREFS)+WSTRWS)/(hconS*SedHW)      
      dTsed = dTsed*tflie*24./itimes

      SchwiSr = PSREFS*SCHwiS
      SCHwia = SchwiSr*(1.-exp(-extk(ior)*Btiefe))

      dTW = (Schwia-WSTRWS)/(roh2o*speWKW*Btiefe)
      dTW = dTW*tflie*24./itimes
     
      dTW2D = Schwia/(roh2o*speWKW*Btiefe)*tflie*24./itimes
      dTW2Du = -WSTRWS/(roh2o*speWKW*Btiefe)*tflie*24./itimes
    endif
      
      DT1D_it = max(abs(DT1D),abs(dTW),abs(DT1D+dTW))

      DT1D = DT1D+dTW

      TEMPW1 = TEMPW2+DT1D
      if(nkzs(ior)>1)TEMPW1 = TEMPW2+DT2D
      DT1 = ABS(DT1D-DTEMP1)

      if(abs(DT1D_it)>1.5)exit
 
      IF(DT1>0.001)THEN                                               
        DTEMP1 = DT1D
        TempW1 = (TEMPW2+TEMPW1)/2  
          else
            exit                                        
      endif

   enddo ! Ende Iterationsschleife

      if(abs(DT1D_it)>1.5)exit
      tempmt = tempmv+dT1D
      tempwt = tempw2+dT1D
      Tsed(ior) = Tsed(ior)+dTsed

     if(TEMPmt<0.0)tempmt = (abs(tempmv)/(abs(tempmv)+abs(DT1D)))*tempmv
     if(TEMPwt<0.0)tempwt = (abs(tempw2)/(abs(tempw2)+abs(DT1D)))*tempw2

     if(tempwt<=0.001)tempwt = 0.001
     if(tempmt<=0.001)tempmt = 0.001

     tempw2 = tempwt
     tempw1 = tempw2
     tempmv = tempmt

   enddo ! Zeitschleife

  if(abs(DT1D_it)>1.5)then
    if(ilauf==0)then
      itimes = int(abs(DT1D_it)/0.5)+1
      ilauf = 1
      goto 999
        else
          itimes = itimes * 1.25
          goto 999 
    endif 
  endif
  
!      if(nkzs(ior)>1)tempwt = tempw2
      if(nkzs(ior)>1)tempwt = tempwz(1,ior)     

      if(nkzs(ior)==1)Cycle ! Überspringen der vertikalen Temperaturberechnung

      WBn = Schwi(ior)*APARS
!      WLn = Schwi(ior)*(1.-APARS)+G-(WV+WL+A)
      WLn = Schwi(ior)*(1.-0.032)*(1.-APARS)+G  ! 0.032: Anteil UVA/UVB


    do nkz = 1,nkzs(ior)  ! Beginn Schleife über die vertikalen Schichten
      hconP = exp(-extk(ior)*dH2D)
      hconL = 0.0137*dH2D**(-1.06)
      hconL = exp(-4.*dH2D)  ! 4.

      WBn_1 = WBn*hconP
      WLn_1 = WLn*hconL
      if(nkz==1)then
        dtemp(nkz,ior) = ((WBn-WBn_1)+(WLn-WLn_1)-WV-WL-A)*(tflie*24./(dH2D*100.))
        dtemp(nkz,ior) = dtemp(nkz,ior) + dTW2D
        FluxT1(ior) = (WBn+WLn)*10000.
          else   
           dtemp(nkz,ior) = ((WBn-WBn_1)+(WLn-WLn_1))*(tflie*24./(dH2D*100.))
           if(nkz==nkzs(ior))dtemp(nkz,ior) = dtemp(nkz,ior) + dTW2Du
      endif

      WBn = WBn_1
      WLn = WLn_1

      dtemp(nkz,ior) = dtemp(nkz,ior)/(tflie*24.)

    enddo ! Ende Schleife über die vertikalen Schichten 

!...Fehlermeldung                                                       
      ifehl = 0
      if(ISNAN(tempmt))then 
        ifehl = 24 
        ifhStr = mstr 
        exit 
      endif 

      extk(ior) = -1.

  enddo ! Ende Knotenschleife

      tempwz(1,anze+1) = tempwt
      tempw(anze+1) = tempmt

      extk(anze+1) = -1.

!      tempwz(1,anze+1) = tempwt
!      tempw(anze+1) = tempmt

      RETURN
      END subroutine temperw
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!     Unterprogramm zur Berechnung der Dichte im 1-dimensionalen Fall
!
      real function Dichte_1D(hcTE)
!      
!
!
      a0 = 999.842594
      a1 = 6.793952e-2
      a2 = -9.095290e-3
      a3 = 1.001685e-4
      a4 = -1.120083e-6
      a5 = 6.536332e-9
!
      Dichte_1D = a0+a1*hcTE+a2*hcTE**2+a3*hcTE**3+a4*hcTE**4+a5*hcTE**5
! 
      return
      end
