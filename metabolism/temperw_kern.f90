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
subroutine temperw_kern(nkz,xnkzs,xwtyp,xschwi,xextk,xhWS,xtempl,xro,xwge,xcloud,xWlage,dH2D, xdtemp_mit                  &
                        ,tflie,WUEBK,SPEWKS,PSREFS,xtempwz1,tempmt,xtempw,btiefe,xTsed,xdtemp_nkz,dtempS_mit,iform_VerdR  &
                        ,kontroll ,jjj )
   implicit none
   integer                         :: nkz, itime, itimes, jiter, mSed, ilauf, iouter, iform_VerdR, xnkzs
   real                            :: G, A, cls, stbk, tempw1, tempw2, PARSab, APARS, AntUV, expWUEB, dTWUEB
   real                            :: ATkor, sddw, sdtt, pdltt, fwind, fkWind, zW2, zWmess, WB, HR
   real                            :: VDW, WV, roh2o, WL, DT1D, DT2D, DT1, btiefe, tflie, hconS, sedPV
   real                            :: speWKW, sedHW, rohS, WUEBK, SPEWKS, PSREFS, dTW, TH2O
   real                            :: WSTRWS, dTsed, SchwiSr, SCHwia, dTW2D, dTW2Du, DT1D_it, dtemp1
   real                            :: tempmt, tempmv, tempwt, WBn, WLn1, WLn2, hconP, hconL1, hconL2, WBn_1, WLn1_1
   real                            :: WLn2_1, dH2D, xDH2D, cv_t, P, gamma_t, cp_air, v, xdtemp_mit
   real                            :: AntL1, AntL2, extkL1, extkL2, slope_t, Qn, b1, a3, b3, dtempS_mit, WSTRWSmax
   real                            :: xwtyp, xschwi, xextk, xhWS, xtempl, xro, xwge, xcloud, xWLage, xtempw, xTsed
   double precision                :: hconSR2,  SchwiSr_PARS,  SchwiSr_L1, SchwiSr_L2, SchwiS_PARS, SchwiS_L1, SchwiS_L2
   double precision                :: SchwiS, Schwia_PARS, Schwia_L1, Schwia_L2, WRSn, WRSn1, WRSn2, WRSn_1, WRSn1_1, WRSn2_1
   double precision                :: hconSR, hconSR1, WRS, G1, G2, WBL1, WBL2
   !real, Dimension(50)          :: xtempwz, xdtemp ,
   real                            :: xtempwz1,xdtemp_nkz
   logical, intent(in)             :: kontroll  !< debugging
   integer, intent(in)             :: jjj       !< debugging
   character(1000)                 :: message
   
   external                        :: qerror
   
   save  WBn, WLn1, WLn2, WRSn, WRSn1, WRSn2
   
   ! Liste der neuen Übergabeparameter
   !------------------------------------
   !    nkz        :   Zähler Tiefenschichten (nkz=1: Oberflächenschicht; nkz=xnkzs: Sohlschicht)
   !    xnkzs      :   Anzahl der Tiefenschichten am Querprofil
   ! ### Die Tiefenschichten müssen immer von 1 bis xnkzs nacheinander aufgerufen werden ####
   !    xwtyp      :   Cloud reflectance(?) derived from cloud type (see set_cloud_reflectance.f90) [-]
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
   
   if (kontroll) print*, "Beginn temperw_kern jjj = ",jjj," xtempw,xTsed,xtempl = ",xtempw,xTsed,xtempl
   if (nkz == 1) then
      g = 0.0
      g1 = 0.0
      g2 = 0.0
      a = 0.0
      wb = 0.0
      wbl1 = 0.0
      wbl2 = 0.0
      parsab = 0.0
      vdw = 0.0
      wv = 0.0
      wl = 0.0
      atkor = 0.0
      sddw = 0.0
      sdtt = 0.0
      pdltt = 0.0
      hr = 0.0
      cv_t = 0.0
      p = 0.0
      gamma_t = 0.0
      slope_t = 0.0
      qn = 0.0
      wstrws = 0.0
      fwind = 0.0
      fkwind = 0.0
      hcons = 0.0
      sedhw = 0.0
      schwis_pars = 0.0
      schwisr_pars = 0.0
      schwisr_l1 = 0.0
      schwisr_l2 = 0.0
      schwis_l1 = 0.0
      schwis_l2 = 0.0
      schwia = 0.0
      schwia_pars = 0.0
      schwia_l1 = 0.0
      schwia_l2 = 0.0
      th2o = 0.0
      schwis = 0.0
      dtsed = 0.0
      dtw = 0.0
      dt1d_it = 0.0
      dt1d = 0.0
      tempw1 = 0.0
      tempw2 = 0.0
      dtemp1 = 0.0
      tempmt = 0.0
      tempmv = 0.0
      tempwt = 0.0
      xdh2d = 0.0
      wbn = 0.0
      wln1 = 0.0
      wln2 = 0.0
      wrsn = 0.0
      wrsn1 = 0.0
      wrsn2 = 0.0
      hconp = 0.0
      hconl1 = 0.0
      hconl2 = 0.0
      hconsr = 0.0
      hconsr1 = 0.0
      hconsr2 = 0.0
      wbn_1 = 0.0
      wln1_1 = 0.0
      wln2_1 = 0.0
      wrsn_1 = 0.0
      wrsn1_1 = 0.0
      wrsn2_1 = 0.0
      wrs = 0.0
   endif
   
   ! mSed = 1: Sedimenteinfluss auf die Temperatur
   ! mSed = 0: ohne Berücksichtigung des Sediments
   msed = 1
   apars = 0.45
   antuv = 0.032  !0.046  Ki
   antl1 = 0.21
   antl2 = 0.294
   extkl1 = 4. ! 3.   Ki
   extkl2 = 4. ! 93.  Ki
   roh2o = 1000.
   sedpv = 0.5
   spewkw = 4.187
   rohs = 1825.
   stbk = 2.0411e-7
   cp_air = 1.005
   v = 0.622
   dtemp1 = 0.0
   itimes = 1
   ilauf = 0
   
   if (nkz == 1) then
      tempw1 = xtempwz1
      tempw2 = xtempwz1
      tempmv = xtempw

      ! Windkorrektur
      ! zWmess:  Höhe der Windmessung
      ! wLage:   Lage der Wetterstation
      ! zW2:     Wind in 2 m Höhe über der Wasseroberfläche
      
      zW2 = 2.
      if ((xWLage-xhWS) <= 0.0) then
         fkWind = 1.
      else
         zWmess = xWLage-xhWS
         fkWind = (zW2/zWmess)**0.11
      endif
      
      ! -----------------------------------------------------------------------
      !  balance of radiation
      ! -----------------------------------------------------------------------
      ! cloud type
      cls = 1. + xwtyp * (xcloud / 8.)**2.6
      outerloop: do iouter = 1,100
         do itime = 1,itimes
            Schwia = 0.0
            WSTRWS = 0.0
            
            ! start iteration loop
            do jiter = 1,50 
               g = 9.37e-6 * stbk * (xtempl+273.15)**6
               g = g * cls * 0.97
               
               ! convert kj/m2/h in cal/cm2/h
               g = g/42.     
               a = 0.97 * stbk * ((tempw1+273.15)**4)
               a = a/42.
               parsab = xschwi * (apars) * (1. - exp(max(-25., -xextk * btiefe)))  ! +antuv
               wbl1 = xschwi * antl1 * (1. - exp(max(-25., -extkl1 * btiefe)))
               wbl2 = xschwi * antl2 * (1. - exp(max(-25., -extkl2 * btiefe)))
               g1 = g*(antl1/(antl1+antl2)) * (1. - exp(max(-25., -extkl1 * btiefe)))
               g2 = g*(antl2/(antl1+antl2)) * (1. - exp(max(-25., -extkl2 * btiefe)))
               ! WB = PARSab+WBL1+WBL2+G1+G2-A     ! Ki neu
               wb = xschwi * (1 - apars) + parsab + g - a  ! alt Ki
               
               ! --------------------------------------------------------------
               ! heat loss through evaporation
               ! --------------------------------------------------------------
               VDW = max(2400.,2501.-2.361*TEMPW1)
               ATkor = exp(-9.81*xhWS/(287.*(xtempl + 273.15)))
               sddw = 6.1078*exp(17.08085*tempw1/(234.175+tempw1))
               sdtt = 6.1078*exp(17.08085*xtempl/(234.175+xtempl))
               pdltt = xro*sdtt/100.
               
               select case (iform_VerdR)
                  ! evaporation height [m/h]
                  
                  case(1) ! WMO (FGSM-Handbuch)
                     fwind = 0.13+0.0936*xwge*fkwind
                     hr = (fwind*(sddw-pdltt)*atkor)/24000.
                  
                  case(2) ! Sweers (1976) over Land
                     fwind = 0.153+0.063*xwge*fkwind
                     hr = (fwind*(sddw-pdltt)*atkor)/24000.
                  
                  case(3) ! Rimsha & Donschenko
                     fwind = 0.211+0.103*xwge*fkwind
                     hr = (fwind*(sddw-pdltt)*atkor)/24000.
                  
                  case(4) ! nach Priestley-Taylor (1972)
                     cv_t = vdw
                     p = 101.3*((293.-0.0065*xhws)/293.)**5.26
                     p = p * 10.
                     gamma_t = cp_air*p/(cv_t*v)
                     slope_t = (0.04145*exp(0.06088*xtempl))*10.
                     qn = wb*42.+schwia-wstrws
                     hr = slope_t/(slope_t+gamma_t)*(abs(qn)/(cv_t*roh2o))
                     b1 = 2.805
                     hr = hr*((sddw-pdltt)/sddw) * b1
                  
                  case(5) ! Delclaux et al. (2007)
                     a3 = 0.04
                     b3 = 27.375
                     cv_t = vdw
                     qn = wb*42.+schwia-wstrws
                     hr = (xtempl+b3)*abs(qn)/(cv_t*roh2o)
                     hr = hr * ((sddw-pdltt)/sddw)*a3
                  
                  case default
                     write(message, "(a,i0)") "subroutine temperw_kern: The &
                        &given value for 'iform_verdr' is invalid: ", iform_verdr
                     call qerror(message)
               end select
               
               WV = roh2o * VDW * HR
               ! convert heat of evaporation [KJ/(m2*h)] to [cal/(cm2*h)]
               WV = WV / 42.
               
               
               ! -------------------------------------------------------------
               !  convection
               ! -------------------------------------------------------------
               if (sddw - pdltt == 0.0) sddw = sddw + 0.001
               wl = ((tempw1-xtempl) / (1.53 * (sddw-pdltt))) * wv
               dt1d = (1./(btiefe * 100.)) * (wb-wv-wl) * tflie * 24./itimes
               dt2d = (1./(dh2d   * 100.)) * (wb-wv-wl) * tflie * 24./itimes
               
               dTW = 0.0
               if (mSed /= 0) then
                  ! influence of sediment
                  ! change in sediment temperature
                  hcons = sedpv * roh2o * spewkw+(1-sedpv) * rohs * spewks
                  sedhw = btiefe * (hcons/(roh2o * spewkw))
                  if (sedhw < 0.6) sedhw = 0.6
                  ! if(sedHW > Btiefe) sedHW = Btiefe
                  
                  
                  ! PARS at riverbed
                  !SchwiS_PARS = xSchwi*(APARS+AntUV)*exp(max(-25.,-xextk*Btiefe)) ! neu Ki
                  schwis_pars = xschwi*(apars)*exp(max(-25.,-xextk*btiefe))       ! alt ki
                  schwis_l1 = (xschwi*antl1+g*antl1/(antl1+antl2))*exp(max(-25.,-extkl1*btiefe))
                  schwis_l2 = (xschwi*antl2+g*antl2/(antl1+antl2))*exp(max(-25.,-extkl2*btiefe))
                  !convert  umrechnung schwis_x to  kj/(m2*h)
                  schwis_pars = schwis_pars*42.
                  schwis_l1 = schwis_l1*42.
                  schwis_l2 = schwis_l2*42.
                  
                  ! calculation of heatflux from difference in temperature
                  ! between water and sediment
                  th2o = tempw1
                  if (xnkzs > 1) th2o = xtempw    ! xtempwz(xnkzs)  neu Ki   ! xtempw alt Ki
                  schwis_l1 = 0.0 ! alt Ki
                  schwis_l2 = 0.0 ! alt Ki
                  schwis = schwis_pars + schwis_l1 + schwis_l2
                  wstrws = wuebk*(th2o-xtsed)
                  dtsed = (schwis*(1.-psrefs)+wstrws)/(hcons*sedhw)
                  dtsed = dtsed*tflie*24./itimes
                  schwisr_pars = psrefs*schwis_pars
                  schwisr_l1 = psrefs*schwis_l1
                  schwisr_l2 = psrefs*schwis_l2
                  schwia_pars = schwisr_pars*(1.-exp(max(-25.,-xextk*btiefe)))
                  schwia_l1 = schwisr_l1*(1.-exp(max(-25.,-extkl1*btiefe)))
                  schwia_l2 = schwisr_l2*(1.-exp(max(-25.,-extkl2*btiefe)))
                  schwia_l1 = 0.0 ! alt Ki
                  schwia_l2 = 0.0 ! alt Ki
                  schwia = schwia_pars + schwia_l1 + schwia_l2
                  
                  dtw2d = schwia/(roh2o*spewkw*btiefe)*tflie*24./itimes   ! alt Ki
                  
                  ! neu eingeführt damit die Änderung dTW durch Wärmeübergang "WSTRWS"
                  ! nicht grösser ist als TH2O-xTsed. Kann bei sehr kleinen Tiefen
                  ! zum Programmabsturz führen
                  wstrwsmax = abs((th2o - xtsed)*wuebk*roh2o*spewkw*btiefe)
                  wstrws = min(wstrwsmax,abs(wstrws))
                  if (th2o < xtsed)wstrws = -1.*abs(wstrws)
                  
                  dtw = (schwia-wstrws)/(roh2o*spewkw*btiefe)
                  dtw = dtw*tflie*24./itimes
               endif
               
               dt1d_it = max(abs(dt1d),abs(dtw),abs(dt1d+dtw))
               dt1d = dt1d+dtw
               tempw1 = tempw2+dt1d
               if (xnkzs > 1)tempw1 = tempw2+dt2d
               dt1 = abs(dt1d-dtemp1)
               if (abs(dt1d_it) > 1.5)exit
               
               if (dt1 > 0.001) then
                  dtemp1 = dt1d
                  tempw1 = (tempw2+tempw1)/2
               else
                  exit
               endif
            enddo ! end of iteration loop
            
            if (abs(DT1D_it) > 1.5)exit
            if (jiter > 50 .and. DT1 > 0.001)exit   ! Ki
            tempmt = tempmv+dT1D
            tempwt = tempw2+dT2D
            xTsed = xTsed+dTsed
            if (tempwt <= 0.001) tempwt = 0.001
            if (tempmt <= 0.001) tempmt = 0.001
            tempw2 = tempwt
            tempw1 = tempw2
            tempmv = tempmt
         enddo ! end of timeloop
         
         
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
      
      ! -----------------------------------------------------------------------
      ! Calculation of temperature change in every layer
      ! -----------------------------------------------------------------------
      xdh2d = dh2d
      if (xnkzs == 1)xdh2d = btiefe
      
      wbn = xschwi*(apars + antuv) ! neu Ki
      wbn = xschwi*apars          ! alt Ki
      wln1 = xschwi*(1.-0.032)*(1.-apars)+g ! alt Ki
      wln2 = 0.0                            ! alt Ki
      ! wln1 = xschwi*antl1+g*antl1/(antl1+antl2)  ! neu Ki
      ! wln2 = xschwi*antl2+g*antl2/(antl1+antl2)  ! neu Ki
      wrsn = (schwisr_pars/42.)*exp(max(-25.,-xextk*btiefe))
      wrsn1 = (schwisr_l1/42.)*exp(max(-25.,-extkl1*btiefe))
      wrsn2 = (schwisr_l2/42.)*exp(max(-25.,-extkl2*btiefe))
      hconp = exp(max(-25.,-xextk*xdh2d))
      hconl1 = exp(max(-25.,-extkl1*xdh2d))
      hconl2 = exp(max(-25.,-extkl2*xdh2d))
      hconsr = exp(min(25.,xextk*xdh2d))
      hconsr1 = exp(min(25.,extkl1*xdh2d))
      hconsr2 = exp(min(25.,extkl2*xdh2d))
      wbn_1 = wbn*hconp
      wln1_1 = wln1*hconl1
      wln2_1 = wln2*hconl2
      wrsn_1 = min((schwisr_pars/42.),wrsn*hconsr)
      wrsn1_1 = min((schwisr_l1/42.),wrsn1*hconsr1)
      wrsn2_1 = min((schwisr_l2/42.),wrsn2*hconsr2)
      wrs = (wrsn_1-wrsn) + (wrsn1_1-wrsn1) + (wrsn2_1-wrsn2)
      
      wrs = 0.0   ! alt
      xdtemp_nkz = ((wbn-wbn_1)+(wln1-wln1_1)+(wln2-wln2_1)+wrs-wv-wl-a)*(tflie*24./(max(0.01,xdh2d)*100.))
      xdtemp_nkz = xdtemp_nkz + dtw2d
      if (xnkzs == 1)xdtemp_nkz = xdtemp_nkz - wstrws/(roh2o*spewkw*max(0.01,xdh2d))*tflie*24.
      if (tflie > 0.0)xdtemp_nkz = xdtemp_nkz/(tflie*24.)
   else
      ! if(nkz==xnkzs-1)xdh2d = btiefe - (xnkzs-2)*dh2d  ! Ki
      hconp = exp(max(-25.,-xextk*xdh2d))
      hconl1 = exp(max(-25.,-extkl1*xdh2d))
      hconl2 = exp(max(-25.,-extkl2*xdh2d))
      hconsr = exp(min(25.,xextk*xdh2d))
      hconsr1 = exp(min(25.,extkl1*xdh2d))
      hconsr2 = exp(min(25.,extkl2*xdh2d))
      wbn_1 = wbn*hconp
      wln1_1 = wln1*hconl1
      wln2_1 = wln2*hconl2
      wrsn_1 = min((schwisr_pars/42.),wrsn*hconsr)
      wrsn1_1 = min((schwisr_l1/42.),wrsn1*hconsr1)
      wrsn2_1 = min((schwisr_l2/42.),wrsn2*hconsr2)
      wrs = (wrsn_1-wrsn) + (wrsn1_1-wrsn1) + (wrsn2_1-wrsn2)
      wrs = 0.0
      xdtemp_nkz = ((wbn-wbn_1)+(wln1-wln1_1)+(wln2-wln2_1)+wrs)*(tflie*24./(max(0.01,xdh2d)*100.))
      if (nkz == xnkzs) then
         xdtemp_nkz = xdtemp_nkz - wstrws/(roh2o*spewkw*btiefe)*tflie*24.
      endif
      ! if(nkz==xnkzs)then
      !    xdtemp_nkz = - wstrws/(roh2o*spewkw*btiefe)*tflie*24.
      !    dtemps_mit = - wstrws/(roh2o*spewkw*btiefe)*tflie*24
      ! endif
      if (tflie > 0.0) xdtemp_nkz = xdtemp_nkz/(tflie*24.)
   endif
   
   wbn = wbn_1
   wln1 = wln1_1
   wln2 = wln2_1
   wrsn = wrsn_1
   wrsn1 = wrsn1_1
   wrsn2 = wrsn2_1
   
   if (kontroll) then
      print*,"Ende temperw_kern jjj = " , jjj,                 &
             " xtempw,xTsed,xtempl = "  , xtempw,xTsed,xtempl
   endif
end subroutine temperw_kern
