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

!> Unterprogramm zur Berechnung des Grünalgenwachstums
!! @author: Autor: Volker Kirchesch
!! @date: 08.09.2015

subroutine algaesgr(schwi, tflie, tempw, rau, tiefe, vmitt, vno3, vnh4, gelp, &
                    svhemg, chla, dalggr, dalgag, anze, sedalg, algzog,       &
                    dgrmor, vkigr, chlaki, chlagr, algdrg, agbcm, agr, cmatgr,&
                    antbl, chlabl, extk, extk_lamda, ilamda, eta, aw, ack,    &
                    acg, acb, ah, as, al, tpgr, algcog, figaus, agmuea,       &
                    fhegas, agreau, tausc, ischif, ilbuhn, q_pg, q_ng, vnh4z, &
                    vno3z, gelpz, dalggz, nkzs, dh2d, tempwz, mstr, up_pgz,   &
                    up_ngz, agrtbr, agrbrz, agrz, chlaz, hchlkz, hchlgz,      &
                    hchlbz, hcchlgz, algagz, algzgz, dz2d, sedalg_mq, sedalg0,&
                    hq_ngz, a1gr, isim_end, agmor_1,                          &
                    control, jjj)
   
  
   
   use module_alloc_dimensions
   use module_aparam
   implicit none
   
   integer                          :: n_neu_s, n_alt_s, nkz, mstr
   integer                          :: j_aus, j, js, jsed, jjj
   integer                          :: jcyano, i_zeiger, i
   integer                          :: isyn, ispek, isim_end, ised
   integer                          :: ior, ilbuhn, ilamda
   integer                          :: ifoto, iaus, anze
   real                             :: zellv, yk,xup_n
   real                             :: xk, xchla, xagrow, xagres, xac
   real                             :: wst, vkrit, vges
   real                             :: v6, ust, ustkri, up_n2i, up_ci
   real                             :: tiefe1, tflie, upmxi
   real                             :: sumyk, sumroh_chl, sumqp, sumqn
   real                             :: sumpc, sumn, sumh, sumaw, sumas
   real                             :: sumah, sumac, sumacg, saettg
   real                             :: roh_chlzmit, qsgr,  qmxi
   real                             :: qmni, pc, pcmit, pcmax
   real                             :: oc, oc0, obfli
   real                             :: hctest, hconsk, hconql, halbi, g
   real                             :: ft_ks, ftemp, fta
   real                             :: frespx, frespgx, fn, fmor
   real                             :: fmor2, fmor1, fmor0, f5, f52, a1Gr
   real                             :: f51, dz_spline, dz, dztot, dz1
   real                             :: dh2d, deltaz, dagr, cnaehr, chlagrt
   real                             :: ceq, cchl_stern, cchl0
   real                             :: awmit, asmit
   real                             :: alpha_chl, ahmit
   real                             :: agrt, agrs, agres, agmor, aggrow
   real                             :: aggmaxtopt, agchl_max, agbcmt
   real                             :: acmit, acmitg, abr
   real                             :: Icz, Ic, N_Cmax
   real                             :: agrt_old
   logical                          :: control
   integer, dimension(1000)         :: ischif, nkzs
   real, dimension(40)              :: eta, aw, ack, acg, acb, ah, as, al, I0, Iz
   real, dimension(50)              :: agrtz, Pz, F5z, aggrwz, CChlaz, CChlazt, Chlagrzt, xroh_Chlz, roh_Chlz
   real, dimension(50)              :: Y, YY, hc_temp, Q_PGz, agresz
   real, dimension(50)              :: xroh_Chl
   real, dimension(100)             :: hemm
   real, dimension(1000)            :: tempw, chla, vno3, vnh4, gelp, chlaki, svhemg, dalggr
   real, dimension(1000)            :: dalgag, chlagr, vmitt, rau, tiefe,  vkigr, antbl
   real, dimension(1000)            :: sedalg, algzog, dgrmor, agrtbr, algdrg, algcog, cmatgr
   real, dimension(1000)            :: tpgr, extk, agbcm, agr, chlabl
   real, dimension(1000)            :: figaus, agmuea, fhegas, agreau, up_PG, up_NG, Q_PG, Q_NG
   real, dimension(1000)            :: schwi, Dz2D, sedAlg0
   real, dimension(40,1000)         :: extk_lamda
   real, dimension(50,1000)         :: agrbrz, up_PGz, up_NGz,  agrz, algagz, dgmorz, algzgz
   real, dimension(50,1000)         :: vNH4z, vNO3z, gelPz, dalggz, tempwz, chlaz
   real, dimension(azStrs,1000)     :: sedAlg_MQ, agmor_1
   real, dimension(azStrs,50,1000)  :: hchlkz, hchlgz, hchlbz, hCChlgz, hQ_NGz
   real, dimension(azStrs,1000)     :: tausc
   
   
   real, parameter :: kTresp = 0.058
   real, parameter :: tauad = 100.    ! Relaxationszeit der Algen in sec
   real, parameter :: te0 = 20.
   real, parameter :: lamda0 = 440.
   real, parameter :: slamda = 0.016
   real, parameter :: agmomi = 0.02
   real, parameter :: agmoma = 0.8
   
   external :: lin_spline, lichthemmung, uptake, c_chla, schiff, sedimentation
   external :: print_clipping, qerror
   
   save Cchlaz
   
   ispek = 0
   
   ifoto = 0     !Lichtabhängigkeit der Fotosyntheserate nach Ross (2009)
   ifoto = 1     !Lichtabhängigkeit der Fotosyntheserate nach Geider (1998)
   isyn = 0      ! Chlorophylla-Synthese nach Geider (1998)
   isyn = 1      ! Chlorophylla-Synthese nach Geider (1997)
   if (chla(1) < 0.0) then  ! falls kein Chla-Randbedingung
      do ior = 1,anze+1
         sedalg(ior) = 0.0
         Sedalg0(ior) = 0.0
         sedAlg_MQ(mstr,ior) = 0.0
      enddo
      return
   endif
   

   if (agChl >= 32.2) then
      agchl_max = 32.2
   else
      agChl_max = 12.4
      if (agChl == 20.6) agChl_max = 20.6
      if (agChl == 15.6) agChl_max = 15.6
   endif
   
   ! C:Chla = abchl * exp(-a1*T) * a2 * I * exp(-a3*T)
   if (agChl > agChl_max) then
      CChl0 = agChl_max * exp(-a1gr * Te0)
   else
      CChl0 = agchl * exp(-a1Gr * Te0)               ! C:Chla bei 0°C mgC/mgChla
   endif
   
   ! Umrechnung der maximalen Wachstumsrate bei 20°C auf Wachstumsrate unter 
   ! optimalen Temperaturbedingungen
   aggmaxTopt = aggmax/(exp(-kTemp_Gr*(Te0-toptg)**2))
   upmxPG = aggmaxTopt * Qmx_PG               ! s. Geider (1998), Angabe pro mg Biomasse
   upmxNG = aggmaxTopt * Qmx_NG               ! Geider (1998)


   sumaw = 0.0
   sumas = 0.0
   sumacg = 0.0
   do i = 1,ilamda
      sumaw = sumaw + aw(i)
      sumas = sumas + as(i)
      sumacg = sumacg + acg(i)
   enddo
   awmit = sumaw / ilamda
   asmit = sumas / ilamda
   acmitg = sumacg / ilamda
   
   ! Berechnung des Absorptionsspektrums für Gelbstoffe
   sumah = 0.0
   do i = 1,ilamda
      ah(i) = alamda * exp(-Slamda*(eta(i)-lamda0))
      sumah = sumah + ah(i)
   enddo
   ahmit = sumah / ilamda
   
   ! Beginn der Segmentschleife
   do ior = 1,anze+1
      sumac = 0.0
      do i = 1,ilamda
         sumac = sumac                                      &
               + ack(i) * vkigr(ior)                        &
               + acg(i) * (1. - vkigr(ior) - antbl(ior))    &
               + acb(i) * antbl(ior)
      enddo
      
      acmit = sumac / ilamda
       
      ! schwi*4.2 - Umrechnung von cal/(cm2*h) in J/(cm2*h)
      obfli = 5.846 * (schwi(ior)*4.2) ! Lichtgeschwindigkeit fuer Luft, da µE bei Wachstumsversuchen in Luft gemessen
      if (obfli < 0.0) obfli = 0.0
      
      ! Spektrale Auflösung der photosynth. aktiven Strahlung
      if (ispek == 0) then
         i0(1) = obfli
      else
         do i = 1,ilamda
            i0(i) = obfli * al(i)
         enddo
      endif
      
      ! Nullsetzen
      do nkz = 1,nkzs(ior)
         Pz(nkz) = 0.0
         F5z(nkz) = 0.0
      enddo
      
      if (ilbuhn == 1) nkzs(ior) = 1
      
      if (ior > 1) then
         agr(ior-1) = agrt
         chlagr(ior-1) = Chlagrt
         agbcm(ior-1) = agbcmt
         
         do nkz = 1,nkzs(ior-1)
            agrz(nkz,ior-1) = agrtz(nkz)
            hchlgz(mstr,nkz,ior-1) = chlagrzt(nkz)
            hCChlgz(mstr,nkz,ior-1) = CChlazt(nkz)
         enddo
      endif
      
      if (ilbuhn == 1 .and. tiefe(ior) < 0.05) cycle
      
      ! Temperaturabhaengigkeit der Respirationsrate
      ftemp = exp(ktresp*(tempw(ior)-20.))
      
      ! Temperaturabhaengigkeit der Wachstumsrate
      fta = exp(-ktemp_gr*(tempw(ior)-toptg)**2)
      fta = max(0.01,fta)
       
      ! Berechnung der Schubspannungsgeschwindigkeit
      fn = 1./rau(ior)
      g = 9.81
      ust = ((fn*g**0.5)/tiefe(ior)**0.166667)*abs(vmitt(ior))
      
      ! Berechnung des mittleren vertikalen Dispersionskoeffizient
      ! nach Fischer im ein-dimensionalen Fall (gute Näherung)
      if (nkzs(ior) == 1 .or. Dz2D(ior) == 0.0) then
         dztot = 0.4 * ust * tiefe(ior) / 6.
      else
         dztot = Dz2D(ior)
      endif
      dz = sqrt(tauad*2.*Dztot)               ! Dicke der Schicht im vertikalen Profil
      PCmax = (aggmaxTopt+agremi * exp(kTresp*(toptg-20.)))/(1.-frmuge)
      
      ! Berechnung der Lichtabsorption im gesamten Wasserkörper
      if (dz > 0.0) then
         js = int(tiefe(ior)/dz)
      else
         js = 0
      endif
      
      if (js > 30) then
         js = 30
         dz = tiefe(ior)/js
      
      else if (js < 1) then
         js = 1
         dz = tiefe(ior)
      endif
      
      deltaz = tiefe(ior) - js * dz
      if (deltaz > 0.0001) then
         js = js + 1
      else
         deltaz = 0.0
      endif
      dz_spline = dz
      
      sumPc = 0.0
      sumRoh_Chl = 0.0
      sumH = 0.0
      if (nkzs(ior) > 1) then
         n_neu_s = js
         n_alt_s = nkzs(ior)
         do nkz = 1,nkzs(ior)
            Y(nkz) = tempwz(nkz,ior)
         enddo
         
         iaus = 0
         i_zeiger = 0
         call lin_spline(dH2D, dz_spline, deltaz, n_alt_s, n_neu_s, Y, YY,i_zeiger)
         do j = 1,n_neu_s
            hc_temp(j) = YY(j)
         enddo
         dz_spline = dz
         do nkz = 1,nkzs(ior)
            Y(nkz) = hCChlgz(mstr,nkz,ior)
         enddo
         
         call lin_spline(dH2D, dz_spline, deltaz, n_alt_s, n_neu_s, Y, YY,i_zeiger)
         do j = 1,n_neu_s
            CChlaz(j) = YY(j)
         enddo
      endif
      
      ! Nährstoffabhängigkeit des Wachstums (1D-Fall)
      ! Temperaturabhängigkeit von KP, KN,KSi
      ! fT_Ks = 1.15**(20.-tempw(ior))
      fT_Ks = 1.
      
      if (qmx_ng/qmn_ng < 1.25) then
         f51 = (vno3(ior) + vnh4(ior)) / (agksn * ft_ks + vno3(ior) + vnh4(ior))
      else
         f51 = (q_ng(ior) - qmn_ng) / (qmx_ng - qmn_ng)
      endif
      
      if (qmx_pg/qmn_pg < 1.25) then
         f52 = gelp(ior) / (agksp * ft_ks + gelp(ior))
      else
         f52 = (q_pg(ior) - qmn_pg) / (qmx_pg - qmn_pg)
      endif
      
 
      f5 = min(f51,f52)
      if (f5 < 0.0)f5 = 0.0
      
      do j = 1,js ! Schleife über die Schichten
         
         dz1 = dz
         
         if (j == js .and. deltaz > 0.0) dz = deltaz
         if (ispek == 1) then
            Ic = 0.0
            do i = 1,ilamda
               Iz(i) = I0(i)*exp(-extk_lamda(i,ior)*dz)
               Ic = Ic + max(0.0,(I0(i)/(extk_lamda(i,ior)*dz))*(1.-exp(-extk_lamda(i,ior)*dz)))
            enddo
         else
            Icz = I0(1)*exp(-extk(ior)*dz)
            Ic = max(0.00,(I0(1)/(extk(ior)*dz))*(1.-exp(-extk(ior)*dz)))
         endif
         
         if (nkzs(ior) == 1) then
            hc_temp(j) = tempw(ior)
            CChlaz(j) = agbcm(ior)
         endif
         
         yK = 1.-svhemg(ior)
         CChl_Stern = CChl0 *exp(a1Gr * hc_temp(j))        ! dunkeladaptierte Algen
         call lichthemmung(tflie, ic, yk, cchl_stern, cchlaz(j))
         
         hemm(j) = yk
         saettg = ikge * 0.183*exp(0.0848*hc_temp(j))
         if (ikge == 103.9) saettg = ikge * 0.234*exp(0.0726*hc_temp(j))
         alpha_chl = pcmax*fta*cchl_stern/(saettg*86400.)
         
         if (ifoto == 0) then
            pc = pcmax * fta * (1.-exp(-ic/saettg)) * hemm(j)
         else if (ifoto == 1) then
            pc = pcmax * fta * (1.-exp(-ic * alpha_chl/(max(cchl_stern,cchlaz(j))*pcmax * fta/86400.))) * hemm(j)
         endif
         
         
         ! Berechnung roh_Chl (wird für die Neuberechnung der Chlorophyll-a-Konzentration)
         N_Cmax = Qmx_NG/Cagr
         xroh_Chlz(j) = CChlaz(j)/(alpha_Chl*(max(0.1,Ic))*N_Cmax) ! 2D-Fall
         if (isyn == 1) then
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
         
         if (ispek == 1) then
            do i = 1,ilamda
               I0(i) = Iz(i)
            enddo
         else
            I0(1) = Icz
         endif
         
      enddo ! Ende Schichtenschleife
      
      Pcmit = sumPc/sumH
      roh_Chlzmit = sumRoh_Chl/sumH
      
      ! 2D-Modellierung
      if (nkzs(ior) > 1) then
         n_neu_s = nkzs(ior)
         
         iaus = 0
         i_zeiger = 1
         dz_spline = dz1
         call lin_spline(dz_spline, dH2D, deltaz, js, n_neu_s, CChlaz, YY,i_zeiger)
         do nkz = 1,nkzs(ior)
            CChlaz(nkz) = YY(nkz)
         enddo
         
         dz_spline = dz1
         call lin_spline(dz_spline, dH2D, deltaz, js, n_neu_s, Pz, YY,i_zeiger)
         do nkz = 1,nkzs(ior)
            Pz(nkz) = YY(nkz)
         enddo
         
         dz_spline = dz1
         call lin_spline(dz_spline, dH2D, deltaz, js, n_neu_s, xroh_Chlz, YY,i_zeiger)
         do nkz = 1,nkzs(ior)
            roh_Chlz(nkz) = YY(nkz)
         enddo
      endif
      
      ! Mittelwertbildung der Hemmung
      sumyK = 0.0
      sumH = 0.0
      if (js == 1) then
         svhemg(ior) = 1.-hemm(1)
      else
         do j = 1,js
            sumyK = sumyK+hemm(j)*dz
            sumH = sumH+dz
         enddo
         svhemg(ior) = 1.-(sumyK/sumH)
      endif
      
      if (nkzs(ior) /= 1) then
         ! 2D-Modellierung
         ! Nährstoffabhängigkeit des Wachstums (2D-Fall)
         do nkz = 1,nkzs(ior)
            if ((Qmx_NG/Qmn_NG) < 1.25) then
               F51 = (VNO3z(nkz,ior)+VNH4z(nkz,ior))/(agksN*ft_ks+VNO3z(nkz,ior)+VNH4z(nkz,ior))
            else
               F51 = (hQ_NGz(mstr,nkz,ior)-Qmn_NG)/(Qmx_NG-Qmn_NG)
            endif
            if ((Qmx_PG/Qmn_PG) < 1.25) then
               F52 = gelPz(nkz,ior)/(agksp*ft_ks+gelPz(nkz,ior))
            endif
            
            F5z(nkz) = min(F51,F52)
            if (F5z(nkz) < 0.0)F5z(nkz) = 0.0
         enddo
      endif
      
      ! Berechnung der Respirationsrate
      aggrow = Pcmit*F5
      
      agres = aggrow * frmuge + agremi*ftemp
      agrt = agr(ior) * exp(aggrow * tflie)
      dalggr(ior) = agrt - agr(ior)
      dalgag(ior) = agrt * (1.-(exp(-agres*tflie)))
      agrt = agrt - dalgag(ior)
      
      if (nkzs(ior) > 1) then
         ! 2D-Modellierung
         sumQN = 0.0
         sumQP = 0.0
         sumH = 0.0
         do nkz = 1,nkzs(ior)            ! Schleifenbeginn 2D
            
            ! Berechnung der Respirationsrate
            aggrwz(nkz) = Pz(nkz)*F5z(nkz)
            frespgx = frmuge
            if ((aggrwz(nkz)-akremi*ftemp) < 0.0)frespx = 0.0
            
            agresz(nkz) = aggrwz(nkz) * frmuge + agremi*ftemp
            agrtz(nkz) = agrz(nkz,ior)*exp(aggrwz(nkz)*tflie)
            dalggz(nkz,ior) = agrtz(nkz)-agrz(nkz,ior)
            algagz(nkz,ior) = agrtz(nkz)*(1.-(exp(-agresz(nkz)*tflie)))
            agrtz(nkz) = agrtz(nkz) - algagz(nkz,ior)
            
            agrbrz(nkz,ior) = max(0.0001,agrtz(nkz))
            
            ! neue Aufnahmeraten
            if ((Qmx_PG/Qmn_PG) <= 1.25) then
               up_PGz(nkz,ior) = Qmx_PG*(max(0.0,(dalggz(nkz,ior)-algagz(nkz,ior))))/(agrbrz(nkz,ior)-algagz(nkz,ior))
               Q_PGz(nkz) = Qmx_PG
            else
               
               ! Phosphor
               yk = Q_PG(ior)
               xk = aggrwz(nkz) - agresz(nkz)
               Qmxi = Qmx_PG
               Qmni = Qmn_PG
               CNaehr = gelPz(nkz,ior)
               Halbi = agksP*ft_ks
               upmxi = upmxPG * FTA
               jcyano = 0
               j_aus = 0
               call uptake(yk, xk, Qmxi, Qmni, CNaehr, Halbi, upmxi, tflie, up_Ci, up_N2i, jcyano, ifix)
               up_PGz(nkz,ior) = up_Ci
               Q_PGz(nkz) = yk
            endif
            
            ! Stickstoff
            if ((Qmx_NG/Qmn_NG) <= 1.25) then
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
               call uptake(yk, xk, Qmxi, Qmni, CNaehr, Halbi, upmxi, tflie, up_Ci, up_N2i, jcyano, ifix)
               j_aus = 0
               up_NGz(nkz,ior) = up_Ci
               hQ_NGz(mstr,nkz,ior) = yk
            endif
            
            ! Neuberechnung der Chlorophyll-a-Konzentration
            roh_Chlz(nkz) = roh_Chlz(nkz) * F5z(nkz)
            xagres = aggrwz(nkz) - agresz(nkz)
            xup_N = up_NGz(nkz,ior)
            if (isyn == 1) then
               roh_Chlz(nkz) = roh_Chlz(nkz)*F51
               xaC = agrz(nkz,ior)*Cagr
               xagrow = aggrwz(nkz)
               xchla = hChlgz(mstr,nkz,ior)
            endif
            
            call c_chla(roh_chlz(nkz), xup_n, xagres, cchlaz(nkz), tflie, cagr, cchl_stern, xchla, xac, xagrow, isyn)
            CChlazt(nkz) = CChlaz(nkz)
            if (nkz > 1) then
               sumQN = sumQN + ((hQ_NGz(mstr,nkz-1,ior)+hQ_NGz(mstr,nkz,ior))/2.)*dH2D
               sumQP = sumQP + ((Q_PGz(nkz-1)+Q_PGz(nkz))/2.)*dH2D
               sumH = sumH + dH2D
            endif
         enddo  ! Schleifenende 2D
         
         Q_NG(ior) = min(Qmx_NG,sumQN/sumH)
         Q_PG(ior) = min(Qmx_PG,sumQP/sumH)
         
      endif    ! Ende 2D
      
      ! neue zellulären Nährstoffgehalte
      agrtbr(ior) = agrt
      if ((Qmx_PG/Qmn_PG) <= 1.25) then
         up_PG(ior) = Qmx_PG*(max(0.0,(dalggr(ior)-dalgag(ior))))/(agrtbr(ior)-dalgag(ior))
         Q_PG(ior) = Qmx_PG
      else
         ! Phosphor
         yk = Q_PG(ior)
         xk = aggrow - agres
         Qmxi = Qmx_PG
         Qmni = Qmn_PG
         CNaehr = gelP(ior)
         abr = agrtbr(ior)
         Halbi = agksP*ft_ks
         upmxi = upmxPG * FTA
         jcyano = 0
         call uptake(yk, xk, Qmxi, Qmni, CNaehr, Halbi, upmxi, tflie, up_Ci, up_N2i, jcyano, ifix)
         up_PG(ior) = up_Ci
         Q_PG(ior) = yk
      endif
      
      ! Stickstoff
      if ((Qmx_NG/Qmn_NG) <= 1.25) then
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
         call uptake(yk, xk, Qmxi, Qmni, CNaehr, Halbi, upmxi, tflie, up_Ci, up_N2i, jcyano, ifix)
         up_NG(ior) = up_Ci
         Q_NG(ior) = yk
      endif
      
      !Neuberechnung der Chlorophyll-a-Konzentration
      nkz = 1
      roh_Chlz(1) = roh_Chlzmit
      xagres = aggrow - agres
      xup_N = up_NG(ior)
      if (isyn == 1) then
         xaC = agr(ior)*Cagr
         xagrow = aggrow
         xchla = chlagr(ior)
      endif
      CChlaz(nkz) = agbcm(ior)
      iaus = 0
      call c_chla(roh_chlz(nkz), xup_n, xagres, cchlaz(nkz), tflie, cagr, cchl_stern, xchla, xac, xagrow, isyn)
      iaus = 0
      
      if (nkzs(ior) == 1) then
         dalggz(1,ior) = dalggr(ior)
         agrbrz(1,ior) = agrtbr(ior)
         up_PGz(1,ior) = up_PG(ior)
         up_NGz(1,ior) = up_NG(ior)
      endif
      
      
      ! Sedimentation
      ! Schiffseinfluss
      ustkri = sqrt(tausc(mstr,ior)/1000.)
      vkrit = (ustkri*tiefe(ior)**0.166667)/((1./rau(ior))*g)
      
      tiefe1 = tiefe(ior)
      if (ischif(ior) == 0) then
         v6 = 0.0
      else
         call schiff(vmitt(ior), tiefe1, ischif(ior), v6)
      endif
      
      vges = vmitt(ior) + v6
      
      agrs = asgre*agr(ior)
      ised = 1      ! Schalter zur Kennzeichnung der hier berücksichtigten partik. Wasserinhaltsstoffe
      jsed = 1
      ZellV = 300.
      call Sedimentation(tiefe(ior),ised,ust,qsgr,oc,Oc0,tflie,wst,jsed,ZellV,control,jjj)
      ceq = agrs*qsgr
      
      sedalg(ior) = (agrs - ceq) * oc
      sedalg0(ior) = wst*asgre*qsgr
      sedalg_mq(mstr,ior) = sedalg_mq(mstr,ior) + sedalg(ior)
      
      ! if (ieros == 1 .and. vges > vkrit) sedalg(ior) = 0.0
      
      
      ! Algenmortalität, abhängig vom Nährstoffgehalt der Zelle, bzw. Nährstoffangebot
      agmor = agmomi
      
      fmor0 = 0.05
      fmor1 = f51
      fmor2 = f52
      if ((Qmx_NG/Qmn_NG)>=1.25) then
         fmor1 = (Q_NG(ior)-Qmn_NG)/(Qmx_NG-Qmn_NG)
      endif
      if ((Qmx_PG/Qmn_PG)>=1.25) then
         fmor2 = (Q_PG(ior)-Qmn_PG)/(Qmx_PG-Qmn_PG)
      endif
      
      fmor = min(fmor1,fmor2)
      agmor = agmomi+agmoma*(1.-((min(fmor0,fmor))/fmor0)**8.)
      agmor = min(max(agmor,agmomi),agmoma) !!wy stay within limits
      if (agmor < agmor_1(mstr,ior)) then
         agmor = agmor_1(mstr,ior)
      else
         agmor_1(mstr,ior) = agmor
      endif
      dgrmor(ior) = agrt*(1.-(exp(-agmor*tflie)))
      
      ! 2D-Modellierung
      do nkz = 1,nkzs(ior)
         dgmorz(nkz,ior) = agrtz(nkz)*(1.-(exp(-agmor*tflie)))
      enddo
      
      ! Quellen/Senken-Term
      ! Grünalgen
      hconql = dalggr(ior)+cmatgr(ior)
      hconsk = dgrmor(ior)+dalgag(ior)+sedalg(ior)+algzog(ior)+algdrg(ior)+algcog(ior)
      
      agrt = agr(ior)+hconql-hconsk
      dagr = abs(hconql-hconsk)
      if (agrt < 0.0) then
         agrt_old = agrt
         agrt = (agr(ior)/(agr(ior)+dagr))*agr(ior)
         call print_clipping("algaesgr", "agrt", agrt_old, agrt, "mg/l")
      endif
      if (agrt < 1.e-5) agrt = 1.e-5
      Chlagrt = 1.e-5  !!wy prevent isnan(Chlagr)
      if (CChlaz(1) > 0.0) Chlagrt = agrt*Cagr*1000./CChlaz(1)
      if (nkzs(ior) == 1) then
         dgmorz(1,ior) = dgrmor(ior)
         algagz(1,ior) = dalgag(ior)
         algzgz(1,ior) = algzog(ior)
         agrtz(1) = agrt
         chlagrzt(1) = Chlagrt
      endif
      agbcmt = CChlaz(1)
      
      if (nkzs(ior) > 1) then !  2D-Modellierung
         do nkz = 1,nkzs(ior)
            hconql = dalggz(nkz,ior)+cmatgr(ior)
            hconsk = dgmorz(nkz,ior)+algagz(nkz,ior)+sedalg(ior)+algzgz(nkz,ior)+algdrg(ior)+algcog(ior)
            
            CChlaz(nkz) = CChlazt(nkz)
            agrtz(nkz) = agrz(nkz,ior)+hconql-hconsk
            dagr = abs(hconql-hconsk)
            if (agrtz(nkz) < 0.0) then
               agrtz(nkz) = (agrz(nkz,ior)/(agrz(nkz,ior)+dagr))*agrz(nkz,ior)
            endif
            
            Chlagrzt(nkz) = agrtz(nkz)*Cagr*1000./CChlaz(nkz)
            if (agrtz(nkz) < 1.e-5) then
               agrtz(nkz) = 1.e-5
               Chlagrzt(nkz) = agrtz(nkz)*Cagr*1000./CChlaz(nkz)
            endif
            
            Chlaz(nkz,ior) = chlagrzt(nkz) + hchlkz(mstr,nkz,ior) + hchlbz(mstr,nkz,ior)
         enddo
      endif    ! Ende 2D
      chla(ior) = chlaki(ior) + chlabl(ior) +chlagrt
      hctest = chlaki(ior) + chlabl(ior) +chlagrt
      if ( hctest > 1.e-10)vkigr(ior) = chlaki(ior)/(chlagrt+chlaki(ior)+chlabl(ior))
      if ( hctest > 1.e-10)antbl(ior) = chlabl(ior)/(chlagrt+chlaki(ior)+chlabl(ior))
      if (nkzs(ior) == 1)Chlaz(1,ior) = chla(ior)
      
      ! Fehlermeldung
      if (isnan(chla(ior))) then
         print*, "subroutine algesgr: isnan(chla)"
         print*, "   mstr    = ", mstr
         print*, "   ior     = ", ior
         print*, "   chlaki  = ", chlaki(ior)
         print*, "   chlabl  = ", chlabl(ior)
         print*, "   chlagrt = ", chlagrt
         print*,'algaesgr ISNAN(chla)',mstr,ior,chlaki(ior),chlabl(ior),chlagrt
         
         call qerror("Division by zero in subroutine algaesgr")
      endif
      
      ! Ausgaben
      agmuea(ior) = aggrow
      fhegas(ior) = svhemg(ior)
      agreau(ior) = agres
      ! agreau(ior) = Q_PG(ior)/Qmx_PG
      
      if (schwi(ior) <= 0.001 .and. isim_end == 0) then
         tpgr(ior) = 0.0
         figaus = 0.0
      else if (schwi(ior) > 0.001) then
         tpgr(ior) = F52
         figaus(ior) = Pcmit/Pcmax*FTA
      else if (isim_end == 1) then
         tpgr(ior) = F52
         figaus(ior) = 0.0
      endif
   enddo
   
   agr(anze+1) = agrt
   agbcm(anze+1) = agbcmt
   chlagr(anze+1) = chlagrt
   
   do nkz = 1,nkzs(anze+1)
      agrz(nkz,anze+1) = agrtz(nkz)
      hchlgz(mstr,nkz,anze+1) = chlagrzt(nkz)
      hCChlgz(mstr,nkz,anze+1) = CChlazt(nkz)
   enddo

end subroutine algaesgr
