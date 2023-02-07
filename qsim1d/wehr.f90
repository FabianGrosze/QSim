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

!> Ermittlung des Einflusses von Wehren auf den Stoffhaushalt
!!
!! @author Volker Kirchesch
!! @date 17.08.2005   

subroutine wehr(wehrh, wehrb, ho2, hQaus, O2zt, htempw, ho2_z, ho2z_z, hlf,   &
                hpw, hmw, hph, hph_z, iph, tzt, hte_z, htez_z, chlazt,        &
                hchlaz_z, akizt, hakiz_z, agrzt, hagrz_z, ablzt, hablz_z,     &
                NH4zt, hNH4z_z, NO2zt, hNO2z_z, NO3zt, hNO3z_z, Pzt, hPz_z,   &
                gSizt, hsiz_z, chlkzt, hchlkz_z, chlgzt, hchlgz_z, chlbzt,    &
                hchlbz_z, gesPzt, hgesPz_z, gesNzt, hgesNz_z, Q_NKzt,         &
                hQ_NKz_z, Q_NBzt, hQ_NBz_z, Q_NGzt,  hQ_NGz_z, dH2D, ESTRNR,  &
                kanz, inkzmx, iSta, nstr, istr, jnkz, iflRi, jlWO2, CChlkzt,  &
                hCChlkz_z, CChlbzt, hCChlbz_z, CChlgzt, hCChlgz_z, janzWS,    &
                janzWt, hnkzs, mwehr, mstr, WSP_UW, WSP_OW, iB)
   
   ! TODO: CO2-Austrag muss noch geändert werden
   
   use allodim
   implicit none
   
   integer                             :: nstr, nkz, mstr, mstep0, kanz
   integer                             :: jnkz, istr, ista, iph, inkzmx
   integer                             :: ilwo2, iiter, ib
   real                                :: hcono2, y, y2, y1, vphw, abst
   real                                :: tempww, sumt, sumsi, sumq_nk, sumq_ng
   real                                :: sumq_nb, sump, sumo2, sumn4, sumn3
   real                                :: sumn2, sumh, sumgesp, sumgesn, sumchl
   real                                :: sumchlk, sumchlg, sumchlb, sumcchlk, sumcchlg
   real                                :: sumcchlb, sumaki, sumagr, sumabl, str
   real                                :: step0, so2, saettw, saetco2, reyn
   real                                :: realy2, realy1, pww, poh, pkw
   real                                :: pk2, pk1, ph2, ph1, ph0
   real                                :: oh, oein, o2neu, h, hk
   real                                :: hcon, hconwb, hcont, hconte, hconr
   real                                :: hcon2, hcon1, froude, fhco3, fco3
   real                                :: fco2, fallhoehe, eta, dh2d, delph
   real                                :: defco2, dco2w, dco2o, c, beta
   real                                :: mue, mwW, lfW, lgk1, lgk2, k1, k2, lgh, lgoh, moco2, mohco3
   real                                :: moco3, mgco2, molgco
   integer, dimension(azStrs)          :: iflRi, janzWS, janzWt, mWehr, jlwo2
   integer, dimension(2*azStrs,azStrs) :: ESTRNR
   integer, dimension(azStrs,1000)     :: hnkzs
   real, dimension(azStrs)             :: wehrh, wehrb, ho2_z, hte_z, hph_z, wsp_UW, WSP_OW
   real, dimension(azStrs,50)          :: ho2z_z, hchlaz_z, hakiz_z, hagrz_z, hablz_z, hNh4z_z, hNO3z_z,hPz_z
   real, dimension(azStrs,50)          :: hchlkz_z, hchlgz_z, hchlbz_z, hNO2z_z, hSiz_z, htez_z, hgesPz_z, hgesNz_z
   real, dimension(azStrs,50)          :: hQ_NKz_z, hQ_NBz_z, hQ_NGz_z, hCChlkz_z, hCChlbz_z, hCChlgz_z
   real, dimension(azStrs,50,2)        :: o2zt, tzt, chlazt, chlkzt, chlgzt, chlbzt, akizt, agrzt, ablzt, NH4zt, NO2zt
   real, dimension(azStrs,50,2)        :: NO3zt, Pzt, gSizt, gesPzt, gesNzt, Q_NKzt, Q_NBzt, Q_NGzt, CChlkzt
   real, dimension(azStrs,50,2)        :: CChlbzt, CChlgzt
   real, dimension(azStrs,1000)        :: hQaus, ho2, htempw, hlf, hpw, hmw, hph
   
   ilwo2 = 0
   oein = 0.0
   mstep0 = 0
   ho2_z(ESTRNR(istr,nstr)) = ho2(ESTRNR(istr,nstr),kanz)
   hte_z(ESTRNR(istr,nstr)) = htempw(ESTRNR(istr,nstr),kanz)
   hph_z(ESTRNR(istr,nstr)) = hph(ESTRNR(istr,nstr),kanz)
   
   do nkz = 1,hnkzs(ESTRNR(istr,nstr),kanz)
      ho2z_z(ESTRNR(istr,nstr),nkz)    = o2zt(ESTRNR(istr,nstr),nkz,jnkz)
      htez_z(ESTRNR(istr,nstr),nkz)    = tzt(ESTRNR(istr,nstr),nkz,jnkz)
      hchlaz_z(ESTRNR(istr,nstr),nkz)  = chlazt(ESTRNR(istr,nstr),nkz,jnkz)
      hakiz_z(ESTRNR(istr,nstr),nkz)   = akizt(ESTRNR(istr,nstr),nkz,jnkz)
      hagrz_z(ESTRNR(istr,nstr),nkz)   = agrzt(ESTRNR(istr,nstr),nkz,jnkz)
      hablz_z(ESTRNR(istr,nstr),nkz)   = ablzt(ESTRNR(istr,nstr),nkz,jnkz)
      hNH4z_z(ESTRNR(istr,nstr),nkz)   = NH4zt(ESTRNR(istr,nstr),nkz,jnkz)
      hNO2z_z(ESTRNR(istr,nstr),nkz)   = NO2zt(ESTRNR(istr,nstr),nkz,jnkz)
      hNO3z_z(ESTRNR(istr,nstr),nkz)   = NO3zt(ESTRNR(istr,nstr),nkz,jnkz)
      hPz_z(ESTRNR(istr,nstr),nkz)     = Pzt(ESTRNR(istr,nstr),nkz,jnkz)
      hSiz_z(ESTRNR(istr,nstr),nkz)    = gSizt(ESTRNR(istr,nstr),nkz,jnkz)
      hchlkz_z(ESTRNR(istr,nstr),nkz)  = chlkzt(ESTRNR(istr,nstr),nkz,jnkz)
      hchlgz_z(ESTRNR(istr,nstr),nkz)  = chlgzt(ESTRNR(istr,nstr),nkz,jnkz)
      hchlbz_z(ESTRNR(istr,nstr),nkz)  = chlbzt(ESTRNR(istr,nstr),nkz,jnkz)
      hgesPz_z(ESTRNR(istr,nstr),nkz)  = gesPzt(ESTRNR(istr,nstr),nkz,jnkz)
      hgesNz_z(ESTRNR(istr,nstr),nkz)  = gesNzt(ESTRNR(istr,nstr),nkz,jnkz)
      hQ_NKz_z(ESTRNR(istr,nstr),nkz)  = Q_NKzt(ESTRNR(istr,nstr),nkz,jnkz)
      hQ_NBz_z(ESTRNR(istr,nstr),nkz)  = Q_NBzt(ESTRNR(istr,nstr),nkz,jnkz)
      hQ_NGz_z(ESTRNR(istr,nstr),nkz)  = Q_NGzt(ESTRNR(istr,nstr),nkz,jnkz)
      hCChlkz_z(ESTRNR(istr,nstr),nkz) = CChlkzt(ESTRNR(istr,nstr),nkz,jnkz)
      hCChlbz_z(ESTRNR(istr,nstr),nkz) = CChlbzt(ESTRNR(istr,nstr),nkz,jnkz)
      hCChlgz_z(ESTRNR(istr,nstr),nkz) = CChlgzt(ESTRNR(istr,nstr),nkz,jnkz)
   enddo
   
   if (iflRi(ESTRNR(istr,nstr)) == -1) goto 999  ! keine Wehrbelüftung bei Fliessumkehr
   if (wehrb(ESTRNR(istr,nstr)) <= 0.0) goto 999 ! keine Wehrbelüftung bei Wehrbreite <0
   
   if (wehrh(ESTRNR(istr,nstr)) <= 0.0) then 
      ! falls keine Wehrhoehe eingegeben wird sie aus der Wasserspiegeldiff. ermittelt
      Fallhoehe = wsp_OW(ESTRNR(istr,nstr)) - WSP_UW(mstr)
   else
      ! eingegebene Wehrhöhe oder berechnete (in subroutine wehrles) fiktive Wehrhöhe
      Fallhoehe = wehrh(ESTRNR(istr,nstr))              
   endif
   if (jlwo2(ESTRNR(istr,nstr)) == 0) then            
      ! keine Angaben ueber Anzahl der Wehrfelder und gezogene Wehrklappen;
      ! keine wehro2.txt. Wehrüberfall soll berücksichtigt werden
      janzWt(ESTRNR(istr,nstr)) = 1
      janzWS(ESTRNR(istr,nstr)) = 1
      jlwo2(ESTRNR(istr,nstr)) = 2
      ilwo2 = 1
   endif
   if (jlWO2(ESTRNR(istr,nstr)) == 2 .and. janzWt(ESTRNR(istr,nstr)) == 0)goto 999  ! kein Wehrüberfall, da kein Wehrfeld gezogen
   if (ESTRNR(istr,nstr) == 0)goto 888
   
   if (jlWO2(ESTRNR(istr,nstr)) < 2)hconWB = 1.
   if (jlWO2(ESTRNR(istr,nstr)) == 2) then
      hcon1 = janzWt(ESTRNR(istr,nstr))
      hcon2 = janzWS(ESTRNR(istr,nstr))
      ! Verhältnis von gezogenen Wehrklappen zur Gesamtzahl an Wehrklappen
      hconWB = hcon1/hcon2
   endif
   
   str = hQaus(ESTRNR(istr,nstr),iSta)/(wehrb(ESTRNR(istr,nstr))*hconWB)
   
   SAETTw = 14.603-htempw(ESTRNR(istr,nstr),kanz)*0.40215+(htempw(ESTRNR(istr,nstr),kanz)**2) &
            *0.007687-(htempw(ESTRNR(istr,nstr),kanz)**3)*0.0000693
   
   ! Froude-Zahl des Wehrueberfalls
   froude = 1.488*((Fallhoehe**3/str**2))**0.25
   
   ! Reynolds-Zahl des Wehrueberfalles
   reyn = str/1.143e-6
   hconr = 1.+0.627e-4*froude**1.78*reyn**0.53
   
   ! Temperaturkorrektur
   hcont = (1.+0.046*htempw(ESTRNR(istr,nstr),kanz))/1.69
   
   hconr = hconr*hcont
   if (hconr < 1.)hconr = 1.
   
   realY1 = janzWt(ESTRNR(istr,nstr))
   realY2 = janzWS(ESTRNR(istr,nstr))
   if (hnkzs(ESTRNR(istr,nstr),kanz) > 1) then
      if (jlWO2(ESTRNR(istr,nstr)) == 2)step0 = realY1/realY2
      if (jlWO2(ESTRNR(istr,nstr)) < 2)step0 = 1.
      mstep0 = int(hnkzs(ESTRNR(istr,nstr),kanz)*step0)
      sumT = 0.0
      sumO2 = 0.0
      sumH = 0.0
      do nkz = 1,mstep0-1
         sumO2 = sumO2+((o2zt(ESTRNR(istr,nstr),nkz,jnkz)+O2zt(ESTRNR(istr,nstr),nkz+1,jnkz))/2.)*dH2D
         sumT = sumT+((tzt(ESTRNR(istr,nstr),nkz,jnkz)+tzt(ESTRNR(istr,nstr),nkz+1,jnkz))/2.)*dH2D
         sumH = sumH+dH2D
      enddo
      hconO2 = sumO2/sumH
      hconTe = sumT/sumH
   else
      hconO2 = o2zt(ESTRNR(istr,nstr),1,jnkz)
      hconTe = tzt(ESTRNR(istr,nstr),1,jnkz)
   endif
   
   if (jlWO2(ESTRNR(istr,nstr)) == 2 .or. jlWO2(ESTRNR(istr,nstr)) < 1)oein = (saettw-hcono2)*(1.-(1./hconr))
   if (jlWO2(ESTRNR(istr,nstr)) == 1)oein = (saettw-ho2(ESTRNR(istr,nstr),kanz))*(1.-(1./hconr))
   O2neu = hconO2+oein
   hcon1 = janzWt(ESTRNR(istr,nstr))
   hcon2 = janzWS(ESTRNR(istr,nstr))-janzWt(ESTRNR(istr,nstr))
   if (jlWO2(ESTRNR(istr,nstr)) == 2) then
      do nkz = 1,mstep0
         o2zt(ESTRNR(istr,nstr),nkz,jnkz) = o2neu
         tzt(ESTRNR(istr,nstr),nkz,jnkz) = hconTe
      enddo
   endif
   
   if (jlWO2(ESTRNR(istr,nstr)) <= 1) then
      do nkz = 1,inkzmx
         o2zt(ESTRNR(istr,nstr),nkz,jnkz) = o2zt(ESTRNR(istr,nstr),nkz,jnkz)+oein
         tzt(ESTRNR(istr,nstr),nkz,jnkz) = hconTe
      enddo
   endif
   
   if (jlWO2(ESTRNR(istr,nstr)) == 2) then
      ho2(ESTRNR(istr,nstr),kanz) = (ho2(ESTRNR(istr,nstr),kanz)*hcon2+O2neu*hcon1)/janzWS(ESTRNR(istr,nstr))
      htempw(ESTRNR(istr,nstr),kanz) = (htempw(ESTRNR(istr,nstr),kanz)*hcon2+hconTe*hcon1)/janzWS(ESTRNR(istr,nstr))
   endif
   
   if (jlWO2(ESTRNR(istr,nstr)) <= 1) then
      ho2(ESTRNR(istr,nstr),kanz) = ho2(ESTRNR(istr,nstr),kanz)+oein
   endif
   
   ! CO2-Austrag an Wehren, pH-Aenderung
   if (iph == 0)goto 999
   
   ! Berechnung der Kohlensaeuresumme in mol/l
   mwW = hmw(ESTRNR(istr,nstr),kanz)*1.e-3
   pwW = hpw(ESTRNR(istr,nstr),kanz)*1.e-3
   c = mwW-pwW
   vphW = hph(ESTRNR(istr,nstr),kanz)
   
   ! Berechnung der absoluten Temperatur
   abst = htempw(ESTRNR(istr,nstr),kanz) + 273.15
   tempwW = htempw(ESTRNR(istr,nstr),kanz)
   
   ! Berechnung der negativen Logarithmen der Dissoziationskonstantenl
   ! bei einer Ionenstaerke von 0 mol/l
   ! in Abhaengigkeit von der abs. Temperatur
   
   pk1 = (17052./abst)+215.21*log10(abst)-0.12675*abst-545.56
   pk2 = (2902.39/abst)+0.02379*abst-6.498
   pkw = (4471.33/abst)+0.017053*abst-6.085
   
   ! Einfluss der Ionenstaerke
   lfW = hlf(ESTRNR(istr,nstr),kanz)
   if (lfW < 0.0)lfW = 0.0
   mue = 1.7e-5*lfW
   lgk1 = sqrt(mue)/(1.+1.4*sqrt(mue))
   lgk2 = (2.*sqrt(mue))/(1.+1.4*sqrt(mue))
   hk = (0.5*sqrt(mue))/(1.+1.4*sqrt(mue))
   
   ! Berechnung der von Temperatur und Ionenstaerke abhaengigen Konstanten
   k1 = pk1-lgk1
   k2 = pk2-lgk2
   k1 = 10**(-k1)
   k2 = 10**(-k2)
   
   ! Berechnung der Konzentrationen an H+ und OH-
   lgh = vphW-hk
   h = 10**(-lgh)
   poh = pkw-vphW
   lgoh = poh-hk
   oh = 10**(-lgoh)
   
   ! Berechnung der Kohlenssaeureformen(nur einmal)
   beta = 1+(k1/h)+(k1*k2/h**2)
   fco2 = 1./beta
   fhco3 = k1/(beta*h)
   fco3 = (k1*k2)/(beta*h**2)
   
   moco2 = c*fco2
   mohco3 = c*fhco3
   moco3 = c*fco3
   
   mgco2 = (moco2*44.)*1000.
   
   saetco2 = 1.-0.0375*tempwW+0.0008*tempwW**2-0.00000793*tempwW**3
   DefCO2 = saetco2-MGCO2
   molgco = 44.
   hcon = sqrt(32./molgco)
   dco2w = 0.0
   dco2o = 0.0
   
   
   DCO2w = defco2*(1.-(1./hconr))
   if (jlWO2(ESTRNR(istr,nstr)) == 2)DCO2w = DCO2W*hcon1/janzWS(ESTRNR(istr,nstr))
   mgco2 = mgco2+dco2w
   moco2 = mgco2/44.0/1000.
   
   mwW = mohco3+2*moco3+oh-h
   pwW = moco2*(-1.)+moco3+oh-h
   
   c = moco2+mohco3+moco3
   
   ! Schaetzer
   ph1 = 0.
   ph2 = 14.
   
   ! Berechnung der Konzentrationen an H+ und OH-
   iiter = 0
   do iiter = 1,50
      ph0 = (ph1+ph2)/2.  !1
      poh = pkw-ph0
      lgh = ph0 -hk
      lgoh = poh-hk
      h = 10**(-lgh)
      oh = 10**(-lgoh)
      y1 = oh-h
      
      ! Berechnung des Aequivalenzfaktors eta
      eta = (k1*h+2*k1*k2)/((h**2)+k1*h+k1*k2)
      y2 = mwW-c*eta
      y = y2-y1
      
      delph = ph2-ph1
      if (delph < 0.001)exit
      if (y < 0.0) then
         ph2 = ph0
         cycle
      endif
      ph1 = ph0
      cycle
   enddo
   
   hph(ESTRNR(istr,nstr),kanz) = ph1
   
999 continue
   if (hnkzs(ESTRNR(istr,nstr),kanz) > 1) then
      sO2 = 0.0
      sumT = 0.0
      sumchl = 0.0
      sumaki = 0.0
      sumagr = 0.0
      sumabl = 0.0
      sumN4 = 0.0
      sumN2 = 0.0
      sumN3 = 0.0
      sumP = 0.0
      sumSi = 0.0
      sumchlk = 0.0
      sumchlg = 0.0
      sumchlb = 0.0
      sumgesP = 0.0
      sumgesN = 0.0
      sumQ_NK = 0.0
      sumQ_NB = 0.0
      sumQ_NG = 0.0
      sumCChlk = 0.0
      sumCChlb = 0.0
      sumCChlg = 0.0
      sumH = 0.0
      do nkz = 1,hnkzs(ESTRNR(istr,nstr),kanz)-1
         sO2 = sO2+((o2zt(ESTRNR(istr,nstr),nkz,jnkz)+O2zt(ESTRNR(istr,nstr),nkz+1,jnkz))/2.)*dH2D
         sumT = sumT+((tzt(ESTRNR(istr,nstr),nkz,jnkz)+tzt(ESTRNR(istr,nstr),nkz+1,jnkz))/2.)*dH2D
         sumchl = sumchl+((chlazt(ESTRNR(istr,nstr),nkz,jnkz)+chlazt(ESTRNR(istr,nstr),nkz+1,jnkz))/2.)*dH2D
         sumaki = sumaki+((akizt(ESTRNR(istr,nstr),nkz,jnkz)+akizt(ESTRNR(istr,nstr),nkz+1,jnkz))/2.)*dH2D
         sumagr = sumagr+((agrzt(ESTRNR(istr,nstr),nkz,jnkz)+agrzt(ESTRNR(istr,nstr),nkz+1,jnkz))/2.)*dH2D
         sumabl = sumabl+((ablzt(ESTRNR(istr,nstr),nkz,jnkz)+ablzt(ESTRNR(istr,nstr),nkz+1,jnkz))/2.)*dH2D
         sumN4 = sumN4+((NH4zt(ESTRNR(istr,nstr),nkz,jnkz)+NH4zt(ESTRNR(istr,nstr),nkz+1,jnkz))/2.)*dH2D
         sumN2 = sumN2+((NO2zt(ESTRNR(istr,nstr),nkz,jnkz)+NO2zt(ESTRNR(istr,nstr),nkz+1,jnkz))/2.)*dH2D
         sumN3 = sumN3+((NO3zt(ESTRNR(istr,nstr),nkz,jnkz)+NO3zt(ESTRNR(istr,nstr),nkz+1,jnkz))/2.)*dH2D
         sumP = sumP+((Pzt(ESTRNR(istr,nstr),nkz,jnkz)+Pzt(ESTRNR(istr,nstr),nkz+1,jnkz))/2.)*dH2D
         sumSi = sumSi+((gSizt(ESTRNR(istr,nstr),nkz,jnkz)+gSizt(ESTRNR(istr,nstr),nkz+1,jnkz))/2.)*dH2D
         sumchlk = sumchlk+((chlkzt(ESTRNR(istr,nstr),nkz,jnkz)+chlkzt(ESTRNR(istr,nstr),nkz+1,jnkz))/2.)*dH2D
         sumchlg = sumchlg+((chlgzt(ESTRNR(istr,nstr),nkz,jnkz)+chlgzt(ESTRNR(istr,nstr),nkz+1,jnkz))/2.)*dH2D
         sumchlb = sumchlb+((chlbzt(ESTRNR(istr,nstr),nkz,jnkz)+chlbzt(ESTRNR(istr,nstr),nkz+1,jnkz))/2.)*dH2D
         sumgesP = sumgesP+((gesPzt(ESTRNR(istr,nstr),nkz,jnkz)+gesPzt(ESTRNR(istr,nstr),nkz+1,jnkz))/2.)*dH2D
         sumgesN = sumgesN+((gesNzt(ESTRNR(istr,nstr),nkz,jnkz)+gesNzt(ESTRNR(istr,nstr),nkz+1,jnkz))/2.)*dH2D
         sumQ_NK = sumQ_NK+((Q_NKzt(ESTRNR(istr,nstr),nkz,jnkz)+Q_NKzt(ESTRNR(istr,nstr),nkz+1,jnkz))/2.)*dH2D
         sumQ_NB = sumQ_NB+((Q_NBzt(ESTRNR(istr,nstr),nkz,jnkz)+Q_NBzt(ESTRNR(istr,nstr),nkz+1,jnkz))/2.)*dH2D
         sumQ_NG = sumQ_NG+((Q_NGzt(ESTRNR(istr,nstr),nkz,jnkz)+Q_NGzt(ESTRNR(istr,nstr),nkz+1,jnkz))/2.)*dH2D
         sumCChlk = sumCChlk+((CChlkzt(ESTRNR(istr,nstr),nkz,jnkz)+CChlkzt(ESTRNR(istr,nstr),nkz+1,jnkz))/2.)*dH2D
         sumCChlb = sumCChlb+((CChlbzt(ESTRNR(istr,nstr),nkz,jnkz)+CChlbzt(ESTRNR(istr,nstr),nkz+1,jnkz))/2.)*dH2D
         sumCChlg = sumCChlg+((CChlgzt(ESTRNR(istr,nstr),nkz,jnkz)+CChlgzt(ESTRNR(istr,nstr),nkz+1,jnkz))/2.)*dH2D
         sumH = sumH+dH2D
      enddo
      if (mwehr(ESTRNR(istr,nstr)) > 0) then
         do nkz = 1,hnkzs(ESTRNR(istr,nstr),kanz)
            tzt(ESTRNR(istr,nstr),nkz,jnkz) = sumT/SumH
            o2zt(ESTRNR(istr,nstr),nkz,jnkz) = sO2/SumH
            chlazt(ESTRNR(istr,nstr),nkz,jnkz) = sumchl/SumH
            akizt(ESTRNR(istr,nstr),nkz,jnkz) = sumaki/SumH
            agrzt(ESTRNR(istr,nstr),nkz,jnkz) = sumagr/SumH
            ablzt(ESTRNR(istr,nstr),nkz,jnkz) = sumabl/SumH
            NH4zt(ESTRNR(istr,nstr),nkz,jnkz) = sumN4/SumH
            NO2zt(ESTRNR(istr,nstr),nkz,jnkz) = sumN2/SumH
            NO3zt(ESTRNR(istr,nstr),nkz,jnkz) = sumN3/SumH
            Pzt(ESTRNR(istr,nstr),nkz,jnkz) = sumP/SumH
            gSizt(ESTRNR(istr,nstr),nkz,jnkz) = sumSi/SumH
            chlkzt(ESTRNR(istr,nstr),nkz,jnkz) = sumchlk/SumH
            chlgzt(ESTRNR(istr,nstr),nkz,jnkz) = sumchlg/SumH
            chlbzt(ESTRNR(istr,nstr),nkz,jnkz) = sumchlb/SumH
            gesPzt(ESTRNR(istr,nstr),nkz,jnkz) = sumgesP/SumH
            gesNzt(ESTRNR(istr,nstr),nkz,jnkz) = sumgesN/SumH
            Q_NKzt(ESTRNR(istr,nstr),nkz,jnkz) = sumQ_NK/SumH
            Q_NBzt(ESTRNR(istr,nstr),nkz,jnkz) = sumQ_NB/SumH
            Q_NGzt(ESTRNR(istr,nstr),nkz,jnkz) = sumQ_NG/SumH
            CChlkzt(ESTRNR(istr,nstr),nkz,jnkz) = sumCChlk/SumH
            CChlbzt(ESTRNR(istr,nstr),nkz,jnkz) = sumCChlb/SumH
            CChlgzt(ESTRNR(istr,nstr),nkz,jnkz) = sumCChlg/SumH
         enddo
      endif
   endif
   if (hnkzs(mstr,iB) == 1 .and. hnkzs(ESTRNR(istr,nstr),kanz) > 1) then
      tzt(ESTRNR(istr,nstr),1,jnkz) = sumT/SumH
      o2zt(ESTRNR(istr,nstr),1,jnkz) = sO2/SumH
      chlazt(ESTRNR(istr,nstr),1,jnkz) = sumchl/SumH
      akizt(ESTRNR(istr,nstr),1,jnkz) = sumaki/SumH
      agrzt(ESTRNR(istr,nstr),1,jnkz) = sumagr/SumH
      ablzt(ESTRNR(istr,nstr),1,jnkz) = sumabl/SumH
      NH4zt(ESTRNR(istr,nstr),1,jnkz) = sumN4/SumH
      NO2zt(ESTRNR(istr,nstr),1,jnkz) = sumN2/SumH
      NO3zt(ESTRNR(istr,nstr),1,jnkz) = sumN3/SumH
      Pzt(ESTRNR(istr,nstr),1,jnkz) = sumP/SumH
      gSizt(ESTRNR(istr,nstr),1,jnkz) = sumSi/SumH
      chlkzt(ESTRNR(istr,nstr),1,jnkz) = sumchlk/SumH
      chlgzt(ESTRNR(istr,nstr),1,jnkz) = sumchlg/SumH
      chlbzt(ESTRNR(istr,nstr),1,jnkz) = sumchlb/SumH
      gesPzt(ESTRNR(istr,nstr),1,jnkz) = sumgesP/SumH
      gesNzt(ESTRNR(istr,nstr),1,jnkz) = sumgesN/SumH
      Q_NKzt(ESTRNR(istr,nstr),1,jnkz) = sumQ_NK/SumH
      Q_NBzt(ESTRNR(istr,nstr),1,jnkz) = sumQ_NB/SumH
      Q_NGzt(ESTRNR(istr,nstr),1,jnkz) = sumQ_NG/SumH
      CChlkzt(ESTRNR(istr,nstr),1,jnkz) = sumCChlk/SumH
      CChlbzt(ESTRNR(istr,nstr),1,jnkz) = sumCChlb/SumH
      CChlgzt(ESTRNR(istr,nstr),1,jnkz) = sumCChlg/SumH
   endif
   if (hnkzs(mstr,iB) > 1 .and. hnkzs(ESTRNR(istr,nstr),kanz) == 1) then
      do nkz = 1,hnkzs(mstr,iB)
         tzt(ESTRNR(istr,nstr),nkz,jnkz) = tzt(ESTRNR(istr,nstr),1,jnkz)
         o2zt(ESTRNR(istr,nstr),nkz,jnkz) = o2zt(ESTRNR(istr,nstr),1,jnkz)
         chlazt(ESTRNR(istr,nstr),nkz,jnkz) = chlazt(ESTRNR(istr,nstr),1,jnkz)
         akizt(ESTRNR(istr,nstr),nkz,jnkz) = akizt(ESTRNR(istr,nstr),1,jnkz)
         agrzt(ESTRNR(istr,nstr),nkz,jnkz) = agrzt(ESTRNR(istr,nstr),1,jnkz)
         ablzt(ESTRNR(istr,nstr),nkz,jnkz) = ablzt(ESTRNR(istr,nstr),1,jnkz)
         NH4zt(ESTRNR(istr,nstr),nkz,jnkz) = NH4zt(ESTRNR(istr,nstr),1,jnkz)
         NO2zt(ESTRNR(istr,nstr),nkz,jnkz) = NO2zt(ESTRNR(istr,nstr),1,jnkz)
         NO3zt(ESTRNR(istr,nstr),nkz,jnkz) = NO3zt(ESTRNR(istr,nstr),1,jnkz)
         Pzt(ESTRNR(istr,nstr),nkz,jnkz) = Pzt(ESTRNR(istr,nstr),1,jnkz)
         gSizt(ESTRNR(istr,nstr),nkz,jnkz) = gSizt(ESTRNR(istr,nstr),1,jnkz)
         chlkzt(ESTRNR(istr,nstr),nkz,jnkz) = chlkzt(ESTRNR(istr,nstr),1,jnkz)
         chlgzt(ESTRNR(istr,nstr),nkz,jnkz) = chlgzt(ESTRNR(istr,nstr),1,jnkz)
         chlbzt(ESTRNR(istr,nstr),nkz,jnkz) = chlbzt(ESTRNR(istr,nstr),1,jnkz)
         gesPzt(ESTRNR(istr,nstr),nkz,jnkz) = gesPzt(ESTRNR(istr,nstr),1,jnkz)
         gesNzt(ESTRNR(istr,nstr),nkz,jnkz) = gesNzt(ESTRNR(istr,nstr),1,jnkz)
         Q_NKzt(ESTRNR(istr,nstr),nkz,jnkz) = Q_NKzt(ESTRNR(istr,nstr),1,jnkz)
         Q_NBzt(ESTRNR(istr,nstr),nkz,jnkz) = Q_NBzt(ESTRNR(istr,nstr),1,jnkz)
         Q_NGzt(ESTRNR(istr,nstr),nkz,jnkz) = Q_NGzt(ESTRNR(istr,nstr),1,jnkz)
         CChlkzt(ESTRNR(istr,nstr),nkz,jnkz) = CChlkzt(ESTRNR(istr,nstr),1,jnkz)
         CChlbzt(ESTRNR(istr,nstr),nkz,jnkz) = CChlbzt(ESTRNR(istr,nstr),1,jnkz)
         CChlgzt(ESTRNR(istr,nstr),nkz,jnkz) = CChlgzt(ESTRNR(istr,nstr),1,jnkz)
      enddo
   endif
   
   888 if (ilwo2 == 1)jlwo2(ESTRNR(istr,nstr)) = 0
end subroutine WEHR
