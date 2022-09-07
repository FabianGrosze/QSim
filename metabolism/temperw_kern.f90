! --------------------------------------------------------------------------- !
!  QSim - Programm zur Simulation der Wasserqualität                          !
!                                                                             !
!  Copyright (C) 2022                                                         !
!  Bundesanstalt für Gewässerkunde                                            !
!  Koblenz (Deutschland)                                                      !
!  http://www.bafg.de                                                         !
!                                                                             !
!  Dieses Programm ist freie Software. Sie können es unter den Bedingungen    !
!  der GNU General Public License, Version 3, wie von der Free Software       !
!  Foundation veröffentlicht, weitergeben und/oder modifizieren.              !
!                                                                             !
!  Die Veröffentlichung dieses Programms erfolgt in der Hoffnung, dass es     !
!  Ihnen von Nutzen sein wird, aber ohne irgendeine Garantie, sogar ohne die  !
!  implizite Garantie der Makrtreife oder der Verwendbarkeit für einen        !
!  bestimmten Zweck.                                                          !
!                                                                             !
!  Details finden Sie in der GNU General Public License.                      !
!  Sie sollten ein Exemplar der GNU General Public License zusammen mit       !
!  diesem Programm erhalten haben.                                            !
!  Falls nicht, siehe http://www.gnu.org/licenses/.                           !
!                                                                             !
!  Programmiert von                                                           !
!  1979 bis 2018   Volker Kirchesch                                           !
!  seit 2011       Jens Wyrwa, Wyrwa@bafg.de                                  !
! --------------------------------------------------------------------------- !

! das Abarbeiten der einzelnen Schichten erfolgt von
! der Oberfläche zur Gewässersohle.(nkz=1: Oberflächenschicht; nkz=xnkzs: Sohlschicht)
! übergeben wird die Temperaturänderung dtemp in den einzelnen Schichten: xdtemp_nkz , xdtemp_mit
subroutine temperw_kern(nkz,xnkzs,xtypw,xschwi,xextk,xhWS,xtempl,xro,xwge,xcloud,xWlage,dH2D, xdtemp_mit                  &
                        ,tflie,WUEBK,SPEWKS,PSREFS,xtempwz1,tempmt,xtempw,btiefe,xTsed,xdtemp_nkz,dtempS_mit,iform_VerdR  &
                        ,kontroll ,jjj )
   implicit none
   integer                         :: nkz, itime, itimes, jiter, mSed, ilauf, iouter, iform_VerdR, xnkzs
   real                            :: wtyp, G, A, cls, stbk, tempw1, tempw2, PARSab, APARS, AntUV, expWUEB, dTWUEB
   real                            :: ATkor, sddw, sdtt, pdltt, fwind, fkWind, zW2, zWmess, WB, HR
   real                            :: VDW, WV, roh2o, WL, DT1D, DT2D, DT1, btiefe, tflie, hconS, sedPV
   real                            :: speWKW, sedHW, rohS, WUEBK, SPEWKS, PSREFS, dTW, TH2O
   real                            :: WSTRWS, dTsed, SchwiSr, SCHwia, dTW2D, dTW2Du, DT1D_it, dtemp1
   real                            :: tempmt, tempmv, tempwt, WBn, WLn1, WLn2, hconP, hconL1, hconL2, WBn_1, WLn1_1
   real                            :: WLn2_1, dH2D, xDH2D, cv_t, P, gamma_t, cp_air, v, xdtemp_mit
   real                            :: AntL1, AntL2, extkL1, extkL2, slope_t, Qn, b1, a3, b3, dtempS_mit, WSTRWSmax
   real                            :: xtypw, xschwi, xextk, xhWS, xtempl, xro, xwge, xcloud, xWLage, xtempw, xTsed
   double precision                :: hconSR2,  SchwiSr_PARS,  SchwiSr_L1, SchwiSr_L2, SchwiS_PARS, SchwiS_L1, SchwiS_L2
   double precision                :: SchwiS, Schwia_PARS, Schwia_L1, Schwia_L2, WRSn, WRSn1, WRSn2, WRSn_1, WRSn1_1, WRSn2_1
   double precision                :: hconSR, hconSR1, WRS, G1, G2, WBL1, WBL2
   !real, Dimension(50)          :: xtempwz, xdtemp ,
   real                            :: xtempwz1,xdtemp_nkz
   logical, intent(in)             :: kontroll  !< debugging
   integer, intent(in)             :: jjj       !< debugging
   save  WBn, WLn1, WLn2, WRSn, WRSn1, WRSn2
   
   ! Liste der neuen Übergabeparameter
   !------------------------------------
   !    nkz        :   Zähler Tiefenschichten (nkz=1: Oberflächenschicht; nkz=xnkzs: Sohlschicht)
   !    xnkzs      :   Anzahl der Tiefenschichten am Querprofil
   ! ### Die Tiefenschichten müssen immer von 1 bis xnkzs nacheinander aufgerufen werden ####
   !    xtypw      :   Wolkentyp (0-6)
   !    xschwi     :   Globalstrahlung am Querprofil [cal/(cm2*h)]
   !    xextk      :   Lichtextinktion [1/m]
   !    xhWS       :   Wasserspiegellage am Querprofil, Höhe ü. NN [m]
   !    xtempl     :   Lufttemperatur im Zeitschritt [°C]
   !    xro        :   relative Luftfeuchte im Zeitschritt [%]
   !    xwge       :   die in der Höhe zWmess gemessene Windgeschwindigkeit [m/s]
   !    xcloud     :   Bedeckungsgrad in achtel
   !    xWlage     :   Lage der Wetterstation, Höhe ü. NN [m]
   !    xdtemp_mit :   mittlere Temperaturänderung in der Wassersäule [°C]
   !    xtempwz    :   Temperatur in der Tiefenschicht nkz am Querprofil [°C]
   !    xtempw     :   Mittelwert der Wassertemperatur im Querprofil [°C]
   !    tempmt     :   Mittelwert der Wassertemperatur im Querprofil nach dem Zeitschritt tflie [°C]
   !    xTsed      :   Sedimenttemperatur [°C]
   !    xdtemp     :   Temperaturänderung in den einzelnen Tiefenschichten [°C/h]
   !    dtempS_mit :   Temperaturänderung durch Sedimenteinfluss (bezogen auf die gesamte Wassersäule) [°C]
   !    IFORM_VERDR:   Schalter für die Auswahl der Verdunstungsformeln
   !    iform_VerdR==1 ! WMO (FGSM-Handbuch)
   !    iform_VerdR==2 ! Sweers (1976) over Land
   !    iform_VerdR==3 ! Rimsha & Donschenko
   !    iform_VerdR==4 ! Priestley-Taylor (1972)
   !    iform_VerdR==5 ! Delclaux et al. (2007)
   if (kontroll)print*,"Beginn temperw_kern jjj = ",jjj," xtempw,xTsed,xtempl = ",xtempw,xTsed,xtempl
   if (nkz == 1) then
      G = 0.0
      G1 = 0.0
      G2 = 0.0
      A = 0.0
      WB = 0.0
      WBL1 = 0.0
      WBL2 = 0.0
      PARSab = 0.0
      VDW = 0.0
      WV = 0.0
      WL = 0.0
      ATkor = 0.0
      sddw = 0.0
      sdtt = 0.0
      pdltt = 0.0
      HR = 0.0
      cv_t = 0.0
      P = 0.0
      gamma_t = 0.0
      slope_t = 0.0
      Qn = 0.0
      WSTRWS = 0.0
      fwind = 0.0
      fkWind = 0.0
      hconS = 0.0
      sedHW = 0.0
      SchwiS_PARS = 0.0
      SchwiSr_PARS = 0.0
      SchwiSr_L1 = 0.0
      SchwiSr_L2 = 0.0
      SchwiS_L1 = 0.0
      SchwiS_L2 = 0.0
      Schwia = 0.0
      Schwia_PARS = 0.0
      Schwia_L1 = 0.0
      Schwia_L2 = 0.0
      TH2O = 0.0
      SchwiS = 0.0
      dTsed = 0.0
      dTW = 0.0
      DT1D_it = 0.0
      DT1D = 0.0
      TEMPW1 = 0.0
      TEMPW2 = 0.0
      DTEMP1 = 0.0
      tempmt = 0.0
      tempmv = 0.0
      Tempwt = 0.0
      xdH2D = 0.0
      WBn = 0.0
      WLn1 = 0.0
      WLn2 = 0.0
      WRSn = 0.0
      WRSn1 = 0.0
      WRSn2 = 0.0
      hconP = 0.0
      hconL1 = 0.0
      hconL2 = 0.0
      hconSR = 0.0
      hconSR1 = 0.0
      hconSR2 = 0.0
      WBn_1 = 0.0
      WLn1_1 = 0.0
      WLn2_1 = 0.0
      WRSn_1 = 0.0
      WRSn1_1 = 0.0
      WRSn2_1 = 0.0
      WRS = 0.0
   endif
   !      mSed = 1: Sedimenteinfluss auf die Temperatur
   !      mSed = 0: ohne Berücksichtigung des Sediments
   mSed = 1
   APARS = 0.45
   AntUV = 0.032  !0.046  Ki
   AntL1 = 0.21
   AntL2 = 0.294
   extkL1 = 4. ! 3.   Ki
   extkL2 = 4. ! 93.  Ki
   roh2o = 1000.
   SedPV = 0.5
   speWKW = 4.187
   rohS = 1825.
   stbk = 2.0411e-7
   cp_air = 1.005
   v = 0.622
   DTEMP1 = 0.0
   itimes = 1
   ilauf = 0
   
   if (nkz == 1) then
      TEMPW1 = xtempwz1
      TEMPW2 = xtempwz1
      tempmv = xtempw

      ! ##### Windkorrektur ######
      !.... zWmess Höhe der Windmessung, wLage = Lage der Wetterstation
      !.... zW2 Wind in 2 m Höhe über der Wasseroberfläche
      
      zW2 = 2.
      if ((xWLage-xhWS) <= 0.0) then
         fkWind = 1.
      else
         zWmess = xWLage-xhWS
         fkWind = (zW2/zWmess)**0.11
      endif
      
      ! #######################################
      !       STRAHLUNGSBILANZ
      ! #######################################
      !     Beruecksichtigung der Wolkenart
      if (xtypw == 0)wtyp = 0.0
      if (xtypw == 1)wtyp = 0.04
      if (xtypw == 2)wtyp = 0.08
      if (xtypw == 3)wtyp = 0.17
      if (xtypw == 4 .or. xtypw == 5)wtyp = 0.2
      if (xtypw > 5)wtyp = 0.25
      if (xtypw < 0)wtyp = 0.17 ! keine Eingabe des Wolkentyps
      cls = 1.+wtyp*(xcloud/8.)**2.6
      outerloop: do iouter = 1,100
         do itime = 1,itimes
            Schwia = 0.0
            WSTRWS = 0.0
            
            do jiter = 1,50 ! Beginn Iterationsschleife
               G = 9.37e-6*stbk*(xtempl+273.16)**6
               G = G*cls*0.97
               G = G/42.     ! Umrechnung KJ/m2/h in cal/cm2/h
               A = 0.97*stbk*((TEMPW1+273.16)**4)
               A = A/42.
               PARSab = xschwi*(APARS)*(1.-exp(max(-25.,-xextk*Btiefe)))  ! +AntUV
               WBL1 = xschwi*AntL1*(1.-exp(max(-25.,-extkL1*Btiefe)))
               WBL2 = xschwi*AntL2*(1.-exp(max(-25.,-extkL2*Btiefe)))
               G1 = G*(AntL1/(AntL1+AntL2))*(1.-exp(max(-25.,-extkL1*Btiefe)))
               G2 = G*(AntL2/(AntL1+AntL2))*(1.-exp(max(-25.,-extkL2*Btiefe)))
               !      WB = PARSab+WBL1+WBL2+G1+G2-A     ! Ki neu
               WB = xschwi*(1-APARS)+PARSab+G-A  ! alt Ki
               ! #########################################
               !      2. WARMEVERLUST DURCH VERDUNSTUNG
               ! #########################################
               VDW = max(2400.,2501.-2.361*TEMPW1)
               ATkor = exp(-9.81*xhWS/(287.*(xtempl+273.16)))
               sddw = 6.1078*exp(17.08085*tempw1/(234.175+tempw1))
               sdtt = 6.1078*exp(17.08085*xtempl/(234.175+xtempl))
               pdltt = xro*sdtt/100.
               select case (iform_VerdR)
                     !....Verdunstungshoehe in m/h
                  case(1)                                    ! WMO (FGSM-Handbuch)
                     fwind = 0.13+0.0936*xwge*fkWind
                     HR = (fwind*(sddw-pdltt)*ATkor)/24000.
                  case(2)                                    ! Sweers (1976) over Land
                     fwind = 0.153+0.063*xwge*fkWind
                     HR = (fwind*(sddw-pdltt)*ATkor)/24000.
                  case(3)                                    ! Rimsha & Donschenko
                     fwind = 0.211+0.103*xwge*fkWind
                     HR = (fwind*(sddw-pdltt)*ATkor)/24000.
                  case(4)                                    ! nach Priestley-Taylor (1972)
                     cv_t = VDW
                     P = 101.3*((293.-0.0065*xhWS)/293.)**5.26
                     P = P * 10.
                     gamma_t = cp_air*P/(cv_t*v)
                     slope_t = (0.04145*exp(0.06088*xtempl))*10.
                     Qn = WB*42.+Schwia-WSTRWS
                     HR = slope_t/(slope_t+gamma_t)*(abs(Qn)/(cv_t*roh2O))
                     b1 = 2.805
                     HR = HR*((sddw-pdltt)/sddw) * b1
                  case(5)                                    ! Delclaux et al. (2007)
                     a3 = 0.04
                     b3 = 27.375
                     cv_t = VDW
                     Qn = WB*42.+Schwia-WSTRWS
                     HR = (xtempl+b3)*abs(Qn)/(cv_t*roh2O)
                     HR = HR * ((sddw-pdltt)/sddw)*a3
                     case default
                     print*,'temperw_kern: Verdunstungsoption iform_VerdR = ',iform_VerdR,' nicht zulässig.'
                     stop 123
               end select
               WV = roh2o*VDW*HR
               !...  Umrechnung der Verdunstungswaerme von KJ/(m2*h) in cal/(cm2*h)
               WV = WV/42.
               ! ###############################
               !       3.KONVEKTION
               !################################
               if ((sddw-pdltt) == 0.0)sddw = sddw+0.001
               WL = ((TEMPW1-xTEMPL)/(1.53*(sddw-pdltt)))*WV
               DT1D = (1./(BTIEFE*100.))*(WB-WV-WL)*TFLIE*24./itimes
               DT2D = (1./(dH2D*100.))*(WB-WV-WL)*TFLIE*24./itimes
               
               dTW = 0.0
               if (mSed == 0) then
               else
                  !.....Einfluss des Sediments
                  !.... Änderung der Sedimenttemperatur
                  hconS = sedPV*roh2o*speWKW+(1-sedPV)*rohS*speWKS
                  sedHW = Btiefe*(hconS/(roh2o*speWKW))
                  if (sedHW < 0.6)sedHW = 0.6
                  !      if(sedHW.gt.Btiefe)sedHW = Btiefe
                  !...  PARS an der Sohle
                  !      SchwiS_PARS = xSchwi*(APARS+AntUV)*exp(max(-25.,-xextk*Btiefe)) ! neu Ki
                  SchwiS_PARS = xSchwi*(APARS)*exp(max(-25.,-xextk*Btiefe))       ! alt Ki
                  SchwiS_L1 = (xSchwi*AntL1+G*AntL1/(AntL1+AntL2))*exp(max(-25.,-extkL1*Btiefe))
                  SchwiS_L2 = (xSchwi*AntL2+G*AntL2/(AntL1+AntL2))*exp(max(-25.,-extkL2*Btiefe))
                  !...  Umrechnung von SchwiS_x in KJ/(m2*h)
                  SchwiS_PARS = SchwiS_PARS*42.
                  SchwiS_L1 = SchwiS_L1*42.
                  SchwiS_L2 = SchwiS_L2*42.
                  !...  Wärmestrom aus Temperaturdifferenz zwischen Wasser und Sediment
                  
                  TH2O = tempw1
                  if (xnkzs > 1)TH2O = xtempw    !    xtempwz(xnkzs)  neu Ki   ! xtempw alt Ki
                  SchwiS_L1 = 0.0 ! alt Ki
                  SchwiS_L2 = 0.0 ! alt Ki
                  SchwiS = SchwiS_PARS + SchwiS_L1 + SchwiS_L2
                  WSTRWS = WUEBK*(TH2O-xTsed)
                  dTsed = (SchwiS*(1.-PSREFS)+WSTRWS)/(hconS*SedHW)
                  dTsed = dTsed*tflie*24./itimes
                  SchwiSr_PARS = PSREFS*SCHwiS_PARS
                  SchwiSr_L1 = PSREFS*SCHwiS_L1
                  SchwiSr_L2 = PSREFS*SCHwiS_L2
                  Schwia_PARS = SchwiSr_PARS*(1.-exp(max(-25.,-xextk*Btiefe)))
                  Schwia_L1 = SchwiSr_L1*(1.-exp(max(-25.,-extkL1*Btiefe)))
                  Schwia_L2 = SchwiSr_L2*(1.-exp(max(-25.,-extkL2*Btiefe)))
                  Schwia_L1 = 0.0 ! alt Ki
                  Schwia_L2 = 0.0 ! alt Ki
                  Schwia = Schwia_PARS + Schwia_L1 + Schwia_L2
                  
                  dTW2D = Schwia/(roh2o*speWKW*Btiefe)*tflie*24./itimes   ! alt Ki
                  !####################################################################
                  ! neu eingeführt damit die Änderung dTW durch Wärmeübergang "WSTRWS"
                  ! nicht grösser ist als TH2O-xTsed. Kann bei sehr kleinen Tiefen
                  ! zum Programmabsturz führen
                  !####################################################################
                  WSTRWSmax = abs((TH2O - xTsed)*WUEBK*roh2o*speWKW*Btiefe)
                  WSTRWS = min(WSTRWSmax,abs(WSTRWS))
                  if (TH2O < xTsed)WSTRWS = -1.*abs(WSTRWS)
                  
                  dTW = (Schwia-WSTRWS)/(roh2o*speWKW*Btiefe)
                  dTW = dTW*tflie*24./itimes
                  
               endif
               DT1D_it = max(abs(DT1D),abs(dTW),abs(DT1D+dTW))
               DT1D = DT1D+dTW
               TEMPW1 = TEMPW2+DT1D
               if (xnkzs > 1)TEMPW1 = TEMPW2+DT2D
               DT1 = ABS(DT1D-DTEMP1)
               if (abs(DT1D_it) > 1.5)exit
               
               if (DT1 > 0.001) then
                  DTEMP1 = DT1D
                  TempW1 = (TEMPW2+TEMPW1)/2
               else
                  exit
               endif
            enddo ! Ende Iterationsschleife
            if (abs(DT1D_it) > 1.5)exit
            if (jiter > 50 .and. DT1 > 0.001)exit   ! Ki
            tempmt = tempmv+dT1D
            tempwt = tempw2+dT2D
            xTsed = xTsed+dTsed
            if (tempwt <= 0.001)tempwt = 0.001
            if (tempmt <= 0.001)tempmt = 0.001
            tempw2 = tempwt
            tempw1 = tempw2
            tempmv = tempmt
         enddo ! Zeitschleife
         if (abs(DT1D_it) > 1.5) then
            if (ilauf == 0) then
               itimes = int(abs(DT1D_it)/0.5)+1
               ilauf = 1
               TEMPW1 = xtempwz1
               TEMPW2 = xtempwz1
               tempmv = xtempw
               cycle outerloop
            else
               itimes = itimes * 1.25
               TEMPW1 = xtempwz1
               TEMPW2 = xtempwz1
               tempmv = xtempw
               cycle outerloop
            endif
         endif
         exit outerloop
      enddo outerloop
      xdtemp_mit = DT1D
      !#########################################################################
      ! Berechnung der Temperaturänderung in den einzelnen Schichten
      !#########################################################################
      xdH2D = dH2D
      if (xnkzs == 1)xdH2D = btiefe
      
      WBn = xSchwi*(APARS + AntUV) ! neu Ki
      WBn = xSchwi*APARS          ! alt Ki
      WLn1 = xSchwi*(1.-0.032)*(1.-APARS)+G ! alt Ki
      WLn2 = 0.0                            ! alt KI
      !      WLn1 = xSchwi*AntL1+G*AntL1/(AntL1+AntL2)  ! neu Ki
      !      WLn2 = xSchwi*AntL2+G*AntL2/(AntL1+AntL2)  ! neu KI
      WRSn = (SchwiSr_PARS/42.)*exp(max(-25.,-xextk*btiefe))
      WRSn1 = (SchwiSr_L1/42.)*exp(max(-25.,-extkL1*btiefe))
      WRSn2 = (SchwiSr_L2/42.)*exp(max(-25.,-extkL2*btiefe))
      hconP = exp(max(-25.,-xextk*xdH2D))
      hconL1 = exp(max(-25.,-extkL1*xdH2D))
      hconL2 = exp(max(-25.,-extkL2*xdH2D))
      hconSR = exp(min(25.,xextk*xdH2D))
      hconSR1 = exp(min(25.,extkL1*xdH2D))
      hconSR2 = exp(min(25.,extkL2*xdH2D))
      WBn_1 = WBn*hconP
      WLn1_1 = WLn1*hconL1
      WLn2_1 = WLn2*hconL2
      WRSn_1 = min((SchwiSr_PARS/42.),WRSn*hconSR)
      WRSn1_1 = min((SchwiSr_L1/42.),WRSn1*hconSR1)
      WRSn2_1 = min((SchwiSr_L2/42.),WRSn2*hconSR2)
      WRS = (WRSn_1-WRSn) + (WRSn1_1-WRSn1) + (WRSn2_1-WRSn2)
      
      WRS = 0.0   ! alt
      xdtemp_nkz = ((WBn-WBn_1)+(WLn1-WLn1_1)+(WLn2-WLn2_1)+WRS-WV-WL-A)*(tflie*24./(max(0.01,xdH2D)*100.))
      xdtemp_nkz = xdtemp_nkz + dTW2D
      if (xnkzs == 1)xdtemp_nkz = xdtemp_nkz - WSTRWS/(roh2o*speWKW*max(0.01,xdH2D))*tflie*24.
      if (tflie > 0.0)xdtemp_nkz = xdtemp_nkz/(tflie*24.)
   else
      !        if(nkz==xnkzs-1)xdH2D = btiefe - (xnkzs-2)*dH2D  ! ki
      hconP = exp(max(-25.,-xextk*xdH2D))
      hconL1 = exp(max(-25.,-extkL1*xdH2D))
      hconL2 = exp(max(-25.,-extkL2*xdH2D))
      hconSR = exp(min(25.,xextk*xdH2D))
      hconSR1 = exp(min(25.,extkL1*xdH2D))
      hconSR2 = exp(min(25.,extkL2*xdH2D))
      WBn_1 = WBn*hconP
      WLn1_1 = WLn1*hconL1
      WLn2_1 = WLn2*hconL2
      WRSn_1 = min((SchwiSr_PARS/42.),WRSn*hconSR)
      WRSn1_1 = min((SchwiSr_L1/42.),WRSn1*hconSR1)
      WRSn2_1 = min((SchwiSr_L2/42.),WRSn2*hconSR2)
      WRS = (WRSn_1-WRSn) + (WRSn1_1-WRSn1) + (WRSn2_1-WRSn2)
      WRS = 0.0
      xdtemp_nkz = ((WBn-WBn_1)+(WLn1-WLn1_1)+(WLn2-WLn2_1)+WRS)*(tflie*24./(max(0.01,xdH2D)*100.))
      if (nkz == xnkzs)xdtemp_nkz = xdtemp_nkz - WSTRWS/(roh2o*speWKW*btiefe)*tflie*24.
      !          if(nkz==xnkzs)then
      !           xdtemp_nkz = - WSTRWS/(roh2o*speWKW*btiefe)*tflie*24.
      !           dtempS_mit = - WSTRWS/(roh2o*speWKW*btiefe)*tflie*24
      !         endif
      if (tflie > 0.0)xdtemp_nkz = xdtemp_nkz/(tflie*24.)
   endif
   WBn = WBn_1
   WLn1 = WLn1_1
   WLn2 = WLn2_1
   WRSn = WRSn_1
   WRSn1 = WRSn1_1
   WRSn2 = WRSn2_1
   
   if (kontroll) then
      print*,"Ende temperw_kern jjj = " , jjj,                 &
             " xtempw,xTsed,xtempl = "  , xtempw,xTsed,xtempl
   endif
end subroutine temperw_kern
