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

!> UNTERPROGRAMM ZUR BERECHNUNG DES KIESELALGENWACHSTUMS
!! AUTOR: VOLKER KIRCHESCH
!! STAND: 08.09.2015

subroutine algaeski(schwi, tflie, tempw, tempwz, rau, tiefe, vmitt, vno3, vnh4, &
                    gelp, svhemk, svhemb, svhemg, chla, si, dalgki, dalgak,     &
                    flag, anze, sedalk, algzok, echla, qeinl, vabfl,            &
                    dkimor, jiein, evkigr, vkigr, antbl, eantbl, saettk, akbcm, &
                    abbcm, agbcm, aki, abl, agr, extk, extk_lamda, ilamda, eta, &
                    aw, ack, acg, acb, ah, as, al, sised, tpki, iwied, akmuea,  &
                    ftaaus, fiaus, fheaus, akraus, tausc, ischif, ilbuhn,       &
                    cmatki, algdrk, algcok, zooind, grote, ss, q_pk, q_nk, q_sk,&
                    vnh4z, vno3z, gelpz, siz, dalgkz, nkzs, dh2d, up_pkz,       &
                    up_nkz, up_siz, skmor, akitbr, chlaz, akibrz, akiz, algakz, &
                    algzkz, ablz, agrz, chlaki, hchlkz, hchlgz, hchlbz, hcchlkz,&
                    hcchlbz, hcchlgz, dz2d, chlabl, chlagr, a1ki, sedalg_mq,    &
                    sedalk0, hq_nkz, hq_ngz, hq_nbz, q_pg, q_ng, q_pb, q_nb,    &
                    mstr, it_h, isim_end, extks, akmor_1, agmor_1, abmor_1,     &
                    control , jjj)
   
   use module_alloc_dimensions
   use module_aparam
   implicit none
   
   real, intent(in), dimension(1000)    :: schwi
   real, intent(in)                     :: tflie
   real, intent(in), dimension(1000)    :: tempw
   real, intent(in), dimension(50,1000) :: tempwz
   real, intent(in), dimension(1000)    :: rau
   real, intent(in), dimension(1000)    :: tiefe
   real, intent(in), dimension(1000)    :: vmitt
   real, intent(in), dimension(1000)    :: vno3
   real, intent(in), dimension(1000)    :: vnh4
   real, intent(in), dimension(1000)    :: gelp
   real, intent(inout), dimension(1000) :: svhemk
   real, intent(inout), dimension(1000) :: svhemb
   real, intent(inout), dimension(1000) :: svhemg
   real, intent(inout), dimension(1000) :: chla 
   real, intent(inout), dimension(1000) :: si 
   real, intent(out), dimension(1000)   :: dalgki
   real, intent(out), dimension(1000)   :: dalgak
   
   logical                         :: control
   integer                         :: jjj
   integer                         :: n_neu_s, n_alt_s, nkz, m
   integer                         :: mstr, j_aus, j, js
   integer                         :: jsed, jj, ji, jcyano, i_zeiger
   integer                         :: i, iwied, it_h_aus
   integer                         :: isyn, ispek, isim_end, ised
   integer                         :: ior_flag, ior, ilbuhn, ilamda, ihcq
   integer                         :: ifoto, iein, iaus
   real                            :: hcchlae, zellv, yk, xup_n
   real                            :: xmuet, xk, xchla, xakres, xagrow
   real                            :: xac, wst, vkrit
   real                            :: vges, v6, ust, ustkri, up_n2i
   real                            :: up_ci,  upmxi
   real                            :: tiefe1
   real                            :: svhemkt, sumyk
   real                            :: sumroh_chl, sumqs, sumqp, sumqn, sumpz
   real                            :: sumpc, sumn, sumh, sumaw, sumas
   real                            :: sumah, sumac, sumack, slamda
   real                            :: saettk, roh_chlzmit, q_skt, q_pkt, q_nkt
   real                            :: qsgr,  qmxi
   real                            :: qmni, pc
   real                            :: pcmit, pcmax, oc, oc0, obfli
   real                            :: hczooind, hcvkigr, hcvkigre, hcvkigr1, hcsvhemk
   real                            :: hcsvhemg, hcsvhemb, hcq_sk, hcq_pk, hcq_pg
   real                            :: hcq_pb, hcq_nk, hcq_ng, hcq_nb, hcq
   real                            :: hcqe, hcon, hconsk, hconql, hconoc
   real                            :: hcchlki, hcchlkiz, hcchlgr, hcchlgrz, hcchlbl
   real                            :: hcchlblz, hcchla, hcchla1, hcantbl, hcantble
   real                            :: hcantbl1, hcakmor_1, hcakmor_11, hcakbcm, hcagmor_1
   real                            :: hcagmor_11, hcagbcm, hcabmor_1, hcabmor_11, hcabbcm
   real                            :: halbi, g, grote, ft_ks, ftemp
   real                            :: fta, fn, fmor
   real                            :: fmor3, fmor2, fmor1, f5
   real                            :: f53, f52, f51, dz_spline, dz
   real                            :: dz1, dh2d, deltaz, daki, cnaehr
   real                            :: chlakit, ceq, cchl_stern, cchlazvor, cchl0
   real                            :: awmit
   real                            :: asmit, alpha_chl,  akres
   real                            :: akmor_1v, akmor_1t, akmor
   real                            :: akit, akis, akgrow
   real                            :: akgmaxtopt, akchl_max, akbcmt, ahmit
   real                            :: acmit, acmitk, abr
   real                            :: a1ki, akit_old
   integer                         :: anze
   integer, dimension(1000)        :: flag, jiein, ischif, nkzs
   integer, dimension(azStrs,1000) :: it_h
   real                            :: Icz, ic, lamda0, N_Cmax
   real, dimension(40)             :: eta, aw, ack, acg, acb, ah, as, al, I0, Iz
   real, dimension(50)             :: Pz, F5z, akgrwz, CChlaz, CChlazt,hcchla1z, akitz, chlakizt
   real, dimension(50)             :: hcchlaz, Y, YY, hc_temp, xroh_Chlz, roh_Chlz, akresz
   real, dimension(50)             :: hcCChlkz, hcCChlbz, hcCChlgz,  xroh_Chl
   real, dimension(50)             :: hcQ_NKz, hcQ_NGz, hcQ_NBz, Q_PKz, Q_NKz, Q_SKz
   real, dimension(100)            :: qeinl, echla, eantbl, evkigr, hemm

   real, dimension(1000)           :: sedalk, algzok, dkimor
   real, dimension(1000)           :: abbcm, agbcm, abl, agr, chlaki, sised, Q_PK, Q_NK, Q_SK
   real, dimension(1000)           :: Q_NG, Q_PG, Q_NB, Q_PB, akbcm, aki, vkigr, antbl, tpki, zooind, up_NK, up_PK
   real, dimension(1000)           :: ss, up_Si, SKmor, akmuea, ftaaus, fiaus, fheaus, akraus, cmatki, algdrk
   real, dimension(1000)           :: algcok, extk, Dz2D, sedAlk0, chlabl, chlagr, akitbr, vabfl
   real, dimension(40,1000)        :: extk_lamda
   real, dimension(50,1000)        :: vNH4z, vNO3z, gelPz, Siz, dalgkz, akibrz, up_PKz, up_NKz
   real, dimension(50,1000)        :: up_Siz, algakz, algzkz, dkmorz, chlaz, akiz, ablz, agrz
   real, dimension(azStrs,1000)    :: sedAlg_MQ, extkS, akmor_1, agmor_1, abmor_1
   real, dimension(azStrs,50,1000) :: hchlkz, hchlgz, hchlbz, hQ_NKz, hQ_NGz, hQ_NBz, hCChlkz, hCChlbz, hCChlgz
   real, dimension(azstrs,1000)    :: tausc
   
   real, parameter :: kTresp = 0.07 
   real, parameter :: tauad = 100. !100. Relaxationszeit der Algen in sec
   real, parameter :: Te0 = 20.
   real, parameter :: akmomi = 0.02
   real, parameter :: agmomi = 0.02
   real, parameter :: abmomi = 0.02
   real, parameter :: akmoma = 0.8
   real, parameter :: fmor0 = 0.05
   
   
   external :: lin_spline, lichthemmung, uptake, c_chla, schiff, sedimentation
   external :: print_clipping
   
   save Cchlaz, hcakbcm, hcabbcm, hcagbcm, hcsvhemk, hcsvhemg, hcsvhemb, hcChla1, hcvkigr1, hcantbl1, hcchla1z, xmuet
   ! FG: Added the following variables here; otherwise debug run for Mittelelbe fails
   save hcakmor_11, hcagmor_11, hcabmor_11, hcQ_PK, hcQ_NK, hcQ_SK, hcQ_PG, hcQ_NG, hcQ_PB, hcQ_NB
   save hcCChlkz, hcCChlbz, hcCChlgz, hcQ_NKz, hcQ_NGz, hcQ_NBz
   
   iein = 1
   ispek = 0
   
   ! Lichtabhängigkeit der Fotosyntheserate
   ! 0: nach Ross (2009)
   ! 1: nach Geider (1998)
   ifoto = 1
   
   ! Chlorophyll-A-Synthese
   ! 0: nach Geider (1998)
   ! 1: nach Geider (1997)
   isyn = 1
   
   if (iwied == 0) then
      do ior = 1,anze+1
         Dz2D(ior) = 0.0
      enddo
   endif
   
   if (chla(1) < 0.0) then  ! falls kein Chla-Randbedingung
      do ior = 1,anze+1
         sedalk(ior) = 0.0
         Sedalk0(ior) = 0.0
         sedAlg_MQ(mstr,ior) = 0.0
      enddo
      return
   endif
      
  
   if (akChl >= 65.7) then
      akchl_max = 65.7
   else
      akChl_max = 13.3
      if (akChl == 20.6) akChl_max = 20.6
      if (akChl == 21.9) akChl_max = 21.9
      if (akChl == 20.4) akChl_max = 20.4
   endif
   
   ! C:Chla = akchl * exp(-a1*T) * a2 * I * exp(-a3*T)
   CChl0 = akchl * exp(-a1ki * Te0)               ! C:Chla bei 0°C mgC/mgChl
   if (akChl > akChl_max) then
      CChl0 = akChl_max * exp(-a1ki * Te0)
   endif
   
   ! Umrechnung der maximalen Wachstumsrate bei 20°C auf Wachstumsrate unter 
   ! optimalen Temperatur-Bedingungen
   akgmaxTopt = akgmax/(exp(-kTemp_Ki*(Te0-toptk)**2))
   upmxPK = akgmaxTopt * Qmx_PK   ! s. Geider (1998)
   upmxNK = akgmaxTopt * Qmx_NK   ! Geider (1998)
         
   sumaw = 0.0
   sumas = 0.0
   sumack = 0.0
   
   do i = 1,ilamda
      sumaw = sumaw+aw(i)
      sumas = sumas+as(i)
      sumack = sumack+ack(i)
   enddo
   
   ! Mittelwertbildung 
   awmit = sumaw/ilamda
   asmit = sumas/ilamda
   acmitk = sumack/ilamda
   
   ! Berechnung des Absorptionsspektrums für Gelbstoffe
   lamda0 = 440.
   Slamda = 0.016
   sumah = 0.0
   do i = 1,ilamda
      ah(i) = alamda*exp(-Slamda*(eta(i)-lamda0))
      sumah = sumah+ah(i)
   enddo
   
   ahmit = sumah/ilamda
   
   ! Beginn Segmentschleife
   do jj = 1,anze+1
      ior = jj
      if (akmor_1(mstr,ior) == 0.0) akmor_1(mstr,ior) = akmomi
      if (agmor_1(mstr,ior) == 0.0) agmor_1(mstr,ior) = agmomi
      if (abmor_1(mstr,ior) == 0.0) abmor_1(mstr,ior) = abmomi
      sedAlg_MQ(mstr,ior) = 0.0
      
      ! schwi*4.2 - Umrechnung von cal/(cm2*h) in J/(cm2*h)
      ! Lichtgeschwindigkeit Luft, da Messungen der µE bei Wachstumsversuchen in Luft
      obfli = 5.846*(schwi(ior)*4.2) 
      ! if(obfli<0.001)obfli = 0.001
      
      ! Spektrale Auflösung der photosynth. aktiven Strahlung
      sumas = 0.0
      do i = 1,ilamda
         I0(i) = obfli*al(i)
      enddo
      
      ! Nullsetzen
      do nkz = 1,nkzs(ior)
         Pz(nkz) = 0.0
         F5z(nkz) = 0.0
      enddo

      if (vabfl(ior)>=0.0 .and. vabfl(ior+1) < 0.0) then
         hcakbcm = akbcm(ior)
         hcabbcm = abbcm(ior)
         hcagbcm = agbcm(ior)
         hcsvhemk = svhemk(ior)
         hcsvhemg = svhemg(ior)
         hcsvhemb = svhemb(ior)
         hcChla1 = chla(ior)
         hcvkigr1 = vkigr(ior)
         hcakmor_11 = akmor_1(mstr,ior)
         hcagmor_11 = agmor_1(mstr,ior)
         hcabmor_11 = abmor_1(mstr,ior)
         hcantbl1 = antbl(ior)
         hcQ_PK = Q_PK(ior)
         hcQ_NK = Q_NK(ior)
         hcQ_SK = Q_SK(ior)
         hcQ_PG = Q_PG(ior)
         hcQ_NG = Q_NG(ior)
         hcQ_PB = Q_PB(ior)
         hcQ_NB = Q_NB(ior)
         
         do nkz = 1,nkzs(ior)
            hcchla1z(nkz) = chlaz(nkz,ior)
            hcCChlkz(nkz) = hCChlkz(mstr,nkz,ior)
            hcCChlbz(nkz) = hCChlbz(mstr,nkz,ior)
            hcCChlgz(nkz) = hCChlgz(mstr,nkz,ior)
            hcQ_NKz(nkz) = hQ_NKz(mstr,nkz,ior)
            hcQ_NGz(nkz) = hQ_NGz(mstr,nkz,ior)
            hcQ_NBz(nkz) = hQ_NBz(mstr,nkz,ior)
         enddo
      endif
      
      ior_flag = 0
      if (flag(ior) == 6 .and. vabfl(ior) < 0.0.and.vabfl(ior+1) > 0.0) then
         ior = ior+1
         ior_flag = 1
      endif
      
      if (ilbuhn == 1) then
         nkzs(ior) = 1
      else if (flag(ior) == 4) then
         ! Berücksichtigung der Einleitungen
         m = 1
         ihcQ = 0
         if (vabfl(ior-1) < 0.0 .and. vabfl(ior) < 0.0)m = -1
         if (vabfl(ior-1) < 0.0 .and. vabfl(ior) > 0.0)ihcQ = 1 ! Konzentration an der Einleitstelle
         ! ist gleich der Konzentration der Einleitung
         
         hcChla = Chla(ior-m)     ! Umbenennen der benötigten Variablen; 1D
         hcQ = vabfl(ior-m)
         hcvkigr = vkigr(ior-m)
         hcantbl = antbl(ior-m)
         hcakmor_1 = akmor_1(mstr,ior-m)
         hcagmor_1 = agmor_1(mstr,ior-m)
         hcabmor_1 = abmor_1(mstr,ior-m)
         do nkz = 1,nkzs(ior)
            hcchlaz(nkz) = chlaz(nkz,ior-m)
         enddo
         
         if (hcQ < 0.0)hcQ = abs(hcQ)
         if (hcQ == 0.0 .or. ihcQ == 1)hcQ = 1.e-10
         if (vabfl(ior-1) < 0.0 .and. vabfl(ior) > 0.0) then
            hcChla = hcChla1
            hcvkigr = hcvkigr1
            hcantbl = hcantbl1
            hcakmor_1 = hcakmor_11
            hcagmor_1 = hcagmor_11
            hcabmor_1 = hcabmor_11
            do nkz = 1,nkzs(ior)
               hcchlaz(nkz) = hcchla1z(nkz)
            enddo
            hcQ = 1.e-10
         endif
         do ji = 1,jiein(ior)   ! Beginn Einleitungsschleife
            hcQE = max(0.0,qeinl(iein))
            hcchlaE = eChla(iein)
            if (hcChlaE < 0.0)hcChlaE = hcChla
            hcvkigrE = evkigr(iein)
            if (hcvkigrE < 0.0)hcvkigrE = hcvkigr
            
            hcantblE = eantbl(iein)
            if (hcantblE < 0.0)hcantblE = hcantbl
            hcchlki = (hcQ*hcchla*hcvkigr+hcQE*hcchlaE*hcvkigrE)/(hcQ+hcQE)
            hcchlbl = (hcQ*hcchla*hcantbl+hcQE*hcchlaE*hcantblE)/(hcQ+hcQE)
            hcchlgr = (hcQ*hcchla*(1.-hcvkigr-hcantbl)+hcQE*hcchlaE*(1.-hcvkigrE-hcantblE))/(hcQ+hcQE)
            
            chla(ior) = (hcQ*hcchla+hcQE*hcchlaE)/(hcQ+hcQE)
            akmor_1(mstr,ior) = (hcQ*hcakmor_1+hcQE*akmomi)/(hcQ+hcQE)
            agmor_1(mstr,ior) = (hcQ*hcakmor_1+hcQE*agmomi)/(hcQ+hcQE)
            abmor_1(mstr,ior) = (hcQ*hcakmor_1+hcQE*abmomi)/(hcQ+hcQE)
            akbcm(ior) = akbcm(ior-m)
            abbcm(ior) = abbcm(ior-m)
            agbcm(ior) = agbcm(ior-m)
            svhemk(ior) = svhemk(ior-m)
            svhemg(ior) = svhemg(ior-m)
            svhemb(ior) = svhemb(ior-m)
            Q_PK(ior) = Q_PK(ior-m)
            Q_NK(ior) = Q_NK(ior-m)
            Q_SK(ior) = Q_SK(ior-m)
            Q_PG(ior) = Q_PG(ior-m)
            Q_NG(ior) = Q_NG(ior-m)
            Q_PB(ior) = Q_PB(ior-m)
            Q_NB(ior) = Q_NB(ior-m)
            do nkz = 1,nkzs(ior)
               hCChlkz(mstr,nkz,ior) = hCChlkz(mstr,nkz,ior-m)
               hCChlbz(mstr,nkz,ior) = hCChlbz(mstr,nkz,ior-m)
               hCChlgz(mstr,nkz,ior) = hCChlgz(mstr,nkz,ior-m)
               hQ_NKz(mstr,nkz,ior) = hQ_NKz(mstr,nkz,ior-m)
               hQ_NGz(mstr,nkz,ior) = hQ_NGz(mstr,nkz,ior-m)
               hQ_NBz(mstr,nkz,ior) = hQ_NBz(mstr,nkz,ior-m)
            enddo
            if (vabfl(ior-1) < 0.0 .and. vabfl(ior) > 0.0) then
               akbcm(ior) = hcakbcm
               abbcm(ior) = hcabbcm
               agbcm(ior) = hcagbcm
               svhemk(ior) = hcsvhemk
               svhemg(ior) = hcsvhemg
               svhemb(ior) = hcsvhemb
               Q_PK(ior) = hcQ_PK
               Q_NK(ior) = hcQ_NK
               Q_SK(ior) = hcQ_SK
               Q_PG(ior) = hcQ_PG
               Q_NG(ior) = hcQ_NG
               Q_PB(ior) = hcQ_PB
               Q_NB(ior) = hcQ_NB
               do nkz = 1,nkzs(ior)
                  hCChlkz(mstr,nkz,ior) = hcCChlkz(nkz)
                  hCChlbz(mstr,nkz,ior) = hcCChlbz(nkz)
                  hCChlgz(mstr,nkz,ior) = hcCChlgz(nkz)
                  hQ_NKz(mstr,nkz,ior) = hcQ_NKz(nkz)
                  hQ_NGz(mstr,nkz,ior) = hcQ_NGz(nkz)
                  hQ_NBz(mstr,nkz,ior) = hcQ_NBz(nkz)
               enddo
            endif
            aki(ior) = (hcchlki/1000.)*(akbcm(ior)/Caki)
            abl(ior) = (hcchlbl/1000.)*(abbcm(ior)/Cabl)
            agr(ior) = (hcchlgr/1000.)*(agbcm(ior)/Cagr)
            Chlaki(ior) = hcchlki
            Chlabl(ior) = hcchlbl
            Chlagr(ior) = hcchlgr
            
            do nkz = 1,nkzs(ior)                        !2D
               hcchlkiz = (hcQ*hcchlaz(nkz)*hcvkigr+hcQE*hcchlaE*hcvkigrE)/(hcQ+hcQE)
               hchlkz(mstr,nkz,ior) = hcchlkiz
               akiz(nkz,ior) = (hcchlkiz/1000.)*(akbcm(ior)/Caki)
               hcchlblz = (hcQ*hcchlaz(nkz)*hcantbl+hcQE*hcchlaE*hcantblE)/(hcQ+hcQE)
               hchlbz(mstr,nkz,ior) = hcchlblz
               ablz(nkz,ior) = (hcchlblz/1000.)*(abbcm(ior)/Cabl)
               hcchlgrz = (hcQ*hcchlaz(nkz)*(1.-hcvkigr-hcantbl)+hcQE*hcchlaE*(1.-hcvkigrE-hcantblE))/(hcQ+hcQE)
               hchlgz(mstr,nkz,ior) = hcchlgrz
               agrz(nkz,ior) = (hcchlgrz/1000.)*(agbcm(ior)/Cagr)
               chlaz(nkz,ior) = (hcQ*hcchlaz(nkz)+hcQE*hcchlaE)/(hcQ+hcQE)
            enddo
            
            vkigr(ior) = hcchlki/(max(0.00001,(hcchlki+hcchlgr+hcchlbl)))
            antbl(ior) = hcchlbl/(max(0.00001,(hcchlki+hcchlgr+hcchlbl)))
            hcQ = hcQ+qeinl(iein)
            iein = iein+1
            hcchla = chla(ior)
            hcvkigr = vkigr(ior)
            hcantbl = antbl(ior)
            hcakmor_1 = akmor_1(mstr,ior)
            hcagmor_1 = agmor_1(mstr,ior)
            hcabmor_1 = abmor_1(mstr,ior)
            do nkz = 1,nkzs(ior)
               hcchlaz(nkz) = chlaz(nkz,ior)
            enddo
         enddo                        ! Ende Einleitungsschleife
         
         if (ior_flag == 1) then
            iein = iein - jiein(ior)
            ior = ior-1
            aki(ior) = aki(ior+1)
            abl(ior) = abl(ior+1)
            agr(ior) = agr(ior+1)
            chla(ior) = chla(ior+1)
            akbcm(ior) = akbcm(ior+1)
            abbcm(ior) = abbcm(ior+1)
            agbcm(ior) = agbcm(ior+1)
            svhemk(ior) = svhemk(ior+1)
            svhemg(ior) = svhemg(ior+1)
            svhemb(ior) = svhemb(ior+1)
            vkigr(ior) = vkigr(ior+1)
            antbl(ior) = antbl(ior+1)
            Chlaki(ior) = Chlaki(ior+1)
            Chlabl(ior) = Chlabl(ior+1)
            Chlagr(ior) = Chlagr(ior+1)
            Q_PK(ior) = Q_PK(ior+1)
            Q_NK(ior) = Q_NK(ior+1)
            Q_SK(ior) = Q_SK(ior+1)
            Q_PG(ior) = Q_PG(ior+1)
            Q_NG(ior) = Q_NG(ior+1)
            Q_PB(ior) = Q_PB(ior+1)
            Q_NB(ior) = Q_NB(ior+1)
            akmor_1(mstr,ior) = akmor_1(mstr,ior+1)
            agmor_1(mstr,ior) = agmor_1(mstr,ior+1)
            abmor_1(mstr,ior) = abmor_1(mstr,ior+1)
            do nkz = 1,nkzs(ior)
               akiz(nkz,ior) = akiz(nkz,ior+1)
               ablz(nkz,ior) = ablz(nkz,ior+1)
               agrz(nkz,ior) = agrz(nkz,ior+1)
               chlaz(nkz,ior) = chlaz(nkz,ior+1)
               hchlkz(mstr,nkz,ior) = hchlkz(mstr,nkz,ior+1)
               hchlgz(mstr,nkz,ior) = hchlgz(mstr,nkz,ior+1)
               hchlbz(mstr,nkz,ior) = hchlbz(mstr,nkz,ior+1)
               hQ_NKz(mstr,nkz,ior) = hQ_NKz(mstr,nkz,ior+1)
            enddo
         endif
      endif  ! Ende Einleitungs-flag
      
      if (ior > 1) then
         aki(ior-1) = akit
         Q_PK(ior-1) = Q_PKt
         Q_NK(ior-1) = Q_NKt
         Q_SK(ior-1) = Q_SKt
         akmor_1(mstr,ior-1) = akmor_1t
         do nkz = 1,nkzs(ior-1)
            akiz(nkz,ior-1) = akitz(nkz)
            hchlkz(mstr,nkz,ior-1) = chlakizt(nkz)
            hCChlkz(mstr,nkz,ior-1) = CChlazt(nkz)
            hQ_NKz(mstr,nkz,ior-1) = Q_NKz(nkz)
         enddo
         akbcm(ior-1) = akbcmt
         Chlaki(ior-1) = Chlakit
         svhemk(ior-1) = svhemkt
      endif
      
      if (ilbuhn == 1 .and. tiefe(ior) < 0.05) cycle
      
      ! Temperaturabhaengigkeit der Respirationsrate
      ftemp = exp(ktresp*(tempw(ior)-20.))
      
      ! Temperaturabhaengigkeit der Wachstumsrate
      fta = exp(-ktemp_ki*(tempw(ior)-toptk)**2)
      fta = max(0.01, fta)
     
      sumac = 0.0
      hczooind = max(0.0, zooind(ior))
      do i = 1,ilamda
         sumac = sumac                                 &          
               + ack(i) * vkigr(ior)                   &
               + acg(i) * (1.-vkigr(ior)-antbl(ior))   &
               + acb(i) * antbl(ior)
         extk_lamda(i,ior) = (ack(i)*vkigr(ior)+acg(i)*(1.-vkigr(ior)-antbl(ior))+acb(i)*antbl(ior))*Chla(ior)  &
                             + as(i)*(SS(ior)+hcZooind*GRote/1000.)+aw(i)+ah(i)
      enddo
      
      acmit = sumac/ilamda                ! mittlere Absorption durch Chlorophyll
      EXTK(ior) = asmit*(SS(ior)+(hczooind*GRote/1000.))+acmit*CHLA(ior)+awmit+ahmit  ! mittlerer Extinktionskoeff.
      if (EXTKS(mstr,ior) > 0.0)EXTK(ior) = EXTKS(mstr,ior)
      
      
      ! Berechnung der Schubspannungsgeschwindigkeit
      fn = 1./rau(ior)
      g = 9.81
      ust = ((fn*g**0.5)/tiefe(ior)**0.166667)*abs(vmitt(ior))
      
      ! Berechnung des mittleren vertikalen Dispersionskoeffizient
      ! nach Fischer im ein-dimensionalen Fall (gute Näherung)
      if (nkzs(ior) == 1 .or. Dz2D(ior) == 0.0) then
         xmuet = 0.4 * ust * tiefe(ior) / 6.
      else
         xmuet = Dz2D(ior)
      endif
      
      ! Dicke der Schicht im vertikalen Profil
      dz = sqrt(tauad*2.*xmuet)
      if (ispek == 0) I0(1) = obfli
      
      ! max C-spezifische Photosyntheserate bei optimal Temperatur
      PCmax = (akgmaxTopt + akremi * exp(kTresp*(toptk-20.))) / (1.-frmuke)  
      
      ! Berechnung der Lichtabsorption im gesamten Wasserkörper
      if (dz > 0.0) then
         js = int(tiefe(ior)/dz)
      else
         js = 0
      endif
      
      if (js > 30) then
         js = 30
         dz = tiefe(ior)/js
      endif
      
      if (js < 1) then
         js = 1
         dz = tiefe(ior)
      endif
      
      deltaz = tiefe(ior) - js * dz
      if (deltaz > 0.0001) then
         js = js + 1
      else
         deltaz = 0.0
      endif
      
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
         dz_spline = dz
         call lin_spline(dH2D, dz_spline, deltaz, n_alt_s, n_neu_s, Y, YY,i_zeiger)
         do j = 1,n_neu_s
            hc_temp(j) = YY(j)
         enddo
         
         dz_spline = dz
         do nkz = 1,nkzs(ior)
            Y(nkz) = hCChlkz(mstr,nkz,ior)
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
      
      if ((Qmx_NK/Qmn_NK) < 1.25) then
         F51 = (VNO3(ior)+VNH4(ior))/(akksN*fT_ks+VNO3(ior)+VNH4(ior))
      else
         F51 = (Q_NK(ior)-Qmn_NK)/(Qmx_NK-Qmn_NK)
      endif
      
      if ((Qmx_PK/Qmn_PK) < 1.25) then
         F52 = gelP(ior)/(akksp*ft_ks+gelP(ior))
      else
         F52 = (Q_PK(ior)-Qmn_PK)/(Qmx_PK-Qmn_PK)
      endif
      
      if ((Qmx_SK/Qmn_SK) < 1.25) then
         F53 = Si(ior)/(akkssi*ft_ks+Si(ior))
      else
         F53 = (Q_SK(ior)-Qmn_SK)/(Qmx_SK-Qmn_SK)
      endif
      
      F5 = min(F51,F52,F53)
      if (F5 < 0.0) F5 = 0.0
      do j = 1,js                         ! Schleife über die Schichten
         
         dz1 = dz
         
         if (j == js .and. deltaz > 0.0) dz = deltaz
         Ic = 0.0
         if (ispek == 1) then
            do i = 1,ilamda
               Iz(i) = I0(i)*exp(-extk_lamda(i,ior)*dz)
               Ic = Ic + max(0.00,(I0(1)/(extk(ior)*dz))*(1.-exp(-extk(ior)*dz)))
            enddo
         else
            Icz = I0(1)*exp(-extk(ior)*dz)
            Ic = max(0.00,(I0(1)/(extk(ior)*dz))*(1.-exp(-extk(ior)*dz)))
         endif
         
         yK = 1.-svhemk(ior)
         if (nkzs(ior) == 1) then
            hc_temp(j) = tempw(ior)
            CChlaz(j) = akbcm(ior)
         endif
         
         CChl_Stern = CChl0 * exp(a1Ki * hc_temp(j))    ! Dunkeladaptierte Algen
         call lichthemmung(tflie, ic, yk, cchl_stern, cchlaz(j))
         hemm(j) = yK
         if (ikke == 95.5) then
            Saettk = ikke
         else if (ikke == 297.) then
            SaettK = ikke * 0.5*exp(0.0347*hc_temp(j))
         else
            SaettK = ikke * 0.837*exp(0.0089*hc_temp(j))
         endif
         
         alpha_chl = PCmax*FTA*CChl_Stern/(Saettk*86400.)
         if (ifoto == 0) then
            Pc = Pcmax*FTA*(1.-exp(-Ic/Saettk))*hemm(j)
         else 
            Pc = Pcmax*FTA*(1.-exp(-Ic*alpha_Chl/(max(CChl_Stern,CChlaz(j))*Pcmax*FTA/86400.)))*hemm(j)
         endif
         
         ! Berechnung roh_Chl (wird für die Neuberechnung des C:Chl-Verhältnisses benötigt)
         ! roh_Chlz hat die Dimension mgChl*sec/mgN, PC in sec
         N_Cmax = Qmx_NK/Caki
         xroh_Chlz(j) = CChlaz(j)/(alpha_Chl*(max(0.1,Ic))*N_Cmax) ! 2D-Fall
         if (isyn == 1) then
            xroh_Chlz(j) = CChlaz(j)/(alpha_Chl*(max(0.00001,Ic)))
            xroh_Chl(j) = xroh_Chlz(j) * PC*F5*1./86400. ! 1D-Fall
            xroh_Chlz(j) = xroh_Chlz(j) * PC/86400.      ! 2D-Fall
         else
            xroh_Chl(j) = xroh_Chlz(j) * PC * F5/86400. ! 1D-Fall
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
      enddo               ! Ende Schichtenschleife
      
      Pcmit = sumPc/sumH
      roh_Chlzmit = sumRoh_Chl/sumH     ! Mittelwert
      Ic = (obfli/(extk(ior)*tiefe(ior))) * (1.-exp(-extk(ior)*tiefe(ior)))
      ic = max(0.0, ic)
      
      ! roh_Chlzmit = (akbcm(ior)/(alpha_Chl*(max(0.001,Ic))))*PCmit * F5/86400.
      ! 2D-Modellierung
      if (nkzs(ior) > 1) then
         n_neu_s = nkzs(ior)
         
         i_zeiger = 1
         dz_spline = dz1
         call lin_spline (dz_spline, dH2D, deltaz, js, n_neu_s, Pz, YY,i_zeiger)
         do nkz = 1,nkzs(ior)
            Pz(nkz) = YY(nkz)
            sumPz = sumPz + Pz(nkz)
         enddo
         
         dz_spline = dz1
         call lin_spline (dz_spline, dH2D, deltaz, js, n_neu_s, CChlaz, YY,i_zeiger)
         do nkz = 1,nkzs(ior)
            CChlaz(nkz) = YY(nkz)
         enddo
        
         dz_spline = dz1
         call lin_spline (dz_spline, dH2D, deltaz, js, n_neu_s, xroh_Chlz, YY,i_zeiger)
         do nkz = 1,nkzs(ior)
            roh_Chlz(nkz) = YY(nkz)
         enddo
      endif
      
      ! Mittelwertbildung der Hemmung
      sumyK = 0.0
      sumH = 0.0
      if (js == 1) then
         svhemkt = 1.-hemm(1)
      else
         do j = 1,js
            sumyK = sumyK+hemm(j)*dz
            sumH = sumH+dz
         enddo
         svhemkt = 1.-(sumyK/sumH)
      endif
      
      if (nkzs(ior) /= 1) then
         ! 2D-Modellierung
         do nkz = 1,nkzs(ior)
            ! Nährstoffabhängigkeit des Wachstums (2D-Fall) 
            if ((Qmx_NK/Qmn_NK) <= 1.25) then
               F51 = (VNO3z(nkz,ior)+VNH4z(nkz,ior))/(akksN*fT_Ks+VNO3z(nkz,ior)+VNH4z(nkz,ior))
            else
               F51 = (hQ_NKz(mstr,nkz,ior)-Qmn_NK)/(Qmx_NK-Qmn_NK)
            endif
            
            if ((Qmx_PK/Qmn_PK) <= 1.25) then
               F52 = gelPz(nkz,ior)/(akksp*ft_ks+gelPz(nkz,ior))
            endif
            
            if ((Qmx_SK/Qmn_SK) < 1.25) then
               F53 = Siz(nkz,ior)/(akkssi*ft_ks+Siz(nkz,ior))
            endif
            
            F5z(nkz) = min(F51,F52,F53)
            if (F5z(nkz) < 0.0)F5z(nkz) = 0.0
         enddo
      endif
      
      ! Berechnung der Respirationsrate
      akgrow = Pcmit*F5
      akres = akgrow * frmuke +akremi*ftemp
      
      ! Berechnung des Biomassezuwachses
      akit = aki(ior)*exp(akgrow*tflie)
      dalgki(ior) = akit-aki(ior)
      dalgak(ior) = akit*(1.-(exp(-akres*tflie)))
      akit = akit - dalgak(ior)
      
      if (akit > huge(akit)) then
         print*,'algaeski:Biomassezuwachses akit,dalgak,akgrow,akres = ',akit,dalgak,akgrow,akres
      endif
      
      
      if (nkzs(ior) > 1) then   ! 2D-Fall
         
         sumQN = 0.0
         sumQP = 0.0
         sumQS = 0.0
         sumH = 0.0
         do nkz = 1,nkzs(ior)     ! Schleifenbeginn 2D-Modellierung
            
            ! Berechnung der Respirationsrate
            akgrwz(nkz) = Pz(nkz)*F5z(nkz)
            
            akresz(nkz) = akgrwz(nkz) * frmuke + akremi*ftemp
            akitz(nkz) = akiz(nkz,ior)*exp(akgrwz(nkz)*tflie)
            dalgkz(nkz,ior) = akitz(nkz)-akiz(nkz,ior)
            
            algakz(nkz,ior) = akitz(nkz)*(1.-(exp(-akresz(nkz)*tflie)))
            akitz(nkz) = akitz(nkz) - algakz(nkz,ior)
            
            ! neue Aufnahmeraten
            akibrz(nkz,ior) = akitz(nkz)
            if (akibrz(nkz,ior) == 0.0)akibrz(nkz,ior) = 0.0001
            
            if ((Qmx_PK/Qmn_PK) <= 1.25) then
               up_PKz(nkz,ior) = Qmx_PK*(max(0.0,(dalgkz(nkz,ior)-algakz(nkz,ior))))/(akibrz(nkz,ior)-algakz(nkz,ior))
               Q_PKz(nkz) = Qmx_PK
            
            else
               ! Phosphor
               yk = Q_PK(ior)
               xk = akgrwz(nkz) - akresz(nkz)
               Qmxi = Qmx_PK
               Qmni = Qmn_PK
               CNaehr = gelPz(nkz,ior)
               Halbi = akksP*ft_ks
               upmxi = upmxPK * FTA
               jcyano = 0
               j_aus = 0
               call uptake(yk, xk, Qmxi, Qmni, CNaehr, Halbi, upmxi, tflie, up_Ci, up_N2i, jcyano, ifix)
               up_PKz(nkz,ior) = up_Ci
               Q_PKz(nkz) = yk
            endif
            
            ! Stickstoff
            if ((Qmx_NK/Qmn_NK) <= 1.25) then
               up_NKz(nkz,ior) = Qmx_NK*(max(0.0,(dalgkz(nkz,ior)-algakz(nkz,ior))))/(akibrz(nkz,ior)-algakz(nkz,ior))
               Q_NKz(nkz) = Qmx_NK
            
            else
               sumN = vNH4z(nkz,ior)+vNO3z(nkz,ior)
               yk = hQ_NKz(mstr,nkz,ior)
               xk = akgrwz(nkz) - akresz(nkz)
               Qmxi = Qmx_NK
               Qmni = Qmn_NK
               CNaehr = sumN
               Halbi = akksN*ft_ks
               upmxi = upmxNK * FTA
               j_aus = 0
               call uptake(yk, xk, Qmxi, Qmni, CNaehr, Halbi, upmxi, tflie, up_Ci, up_N2i, jcyano, ifix)
               up_NKz(nkz,ior) = up_Ci
               Q_NKz(nkz) = yk
            endif
            
            j_aus = 0
            ! Neuberechnung der Chla-Konzentration
            if (isyn == 1) then
               roh_Chlz(nkz) = roh_Chlz(nkz)*F5z(nkz)
            else
               roh_Chlz(nkz) = roh_Chlz(nkz)*F5z(nkz)
            endif
            
            xakres = akgrwz(nkz) - akresz(nkz)
            xup_N = up_NKz(nkz,ior)
            
            if (isyn == 1) then
               xaC = akiz(nkz,ior)*Caki
               xagrow = akgrwz(nkz)
               xchla = hChlkz(mstr,nkz,ior)
            endif
            
            call c_chla(roh_chlz(nkz), xup_n, xakres, cchlaz(nkz), tflie, caki, cchl_stern, xchla, xac, xagrow, isyn)
            CChlazt(nkz) = CChlaz(nkz) !!wy sonst werden 2D Felder im 1D-Fall nicht bedient und haben beliebige Werte
            iaus = 0
            
            ! Silikat (kein zellinterner Speicher)
            hcon = (Siz(nkz,ior)+akkssi)
            if (hcon /= 0.0) hcon = Siz(nkz,ior)/hcon ! Silikatgehlat der Zelle ist abhängig vom Silikatgehalt im Außenmedium
            Q_SKz(nkz) = Qmx_SK*hcon
            up_Siz(nkz,ior) = akibrz(nkz,ior)-algakz(nkz,ior)
            if (up_Siz(nkz,ior) /= 0.0)up_Siz(nkz,ior) = Qmx_SK*hcon*(max(0.0,(dalgkz(nkz,ior)-algakz(nkz,ior))))/up_Siz(nkz,ior)
            
            if (nkz > 1) then
               sumQN = sumQN + ((Q_NKz(nkz-1)+Q_NKz(nkz))/2.)*dH2D
               sumQP = sumQP + ((Q_PKz(nkz-1)+Q_PKz(nkz))/2.)*dH2D
               sumQS = sumQS + ((Q_SKz(nkz-1)+Q_SKz(nkz))/2.)*dH2D
               sumH = sumH + dH2D
            endif
         enddo          ! Schleifenende 2D
         Q_NKt = min(Qmx_NK,sumQN/sumH)
         Q_PKt = min(Qmx_PK,sumQP/sumH)
         Q_SKt = min(Qmx_SK,sumQS/sumH)
         
      endif   ! Ende 2D
      
      akitbr(ior) = akit
      
      if ((Qmx_PK/Qmn_PK) <= 1.25) then
         up_PK(ior) = Qmx_PK*(max(0.0,(dalgki(ior)-dalgak(ior))))/(akitbr(ior)-dalgak(ior))
         if (nkzs(ior) == 1)Q_PKt = Qmx_PK
      else
         ! Phosphor
         yk = Q_PK(ior)
         xk = akgrow - akres
         Qmxi = Qmx_PK
         Qmni = Qmn_PK
         CNaehr = gelP(ior)
         abr = akitbr(ior)
         Halbi = akksP*ft_ks
         upmxi = upmxPK * FTA
         jcyano = 0
         call uptake(yk, xk, Qmxi, Qmni, CNaehr, Halbi, upmxi, tflie, up_Ci, up_N2i, jcyano, ifix)
         up_PK(ior) = up_Ci
         if (nkzs(ior) == 1)Q_PKt = yk
      endif
      
      ! Stickstoff
      if ((Qmx_NK/Qmn_NK) <= 1.25) then
         up_NK(ior) = Qmx_NK*(max(0.0,(dalgki(ior)-dalgak(ior))))/(akitbr(ior)-dalgak(ior))
         if (nkzs(ior) == 1)Q_NKt = Qmx_NK
      else
         sumN = vNH4(ior)+vNO3(ior)
         yk = Q_NK(ior)
         xk = akgrow - akres
         Qmxi = Qmx_NK
         Qmni = Qmn_NK
         CNaehr = sumN
         Halbi = akksN*ft_ks
         upmxi = upmxNK * FTA
         j_aus = 0
         call uptake(yk, xk, Qmxi, Qmni, CNaehr, Halbi, upmxi, tflie, up_Ci, up_N2i, jcyano, ifix)
         j_aus = 0
         up_NK(ior) = up_Ci
         if (nkzs(ior) == 1)Q_NKt = yk
      endif
      
      ! Neuberechnung der Chlorophyll-a-Konzentration (1D-Fall)
      nkz = 1
      roh_Chlz(1) = roh_Chlzmit
      xakres = akgrow - akres
      xup_N = up_NK(ior)
      if (isyn == 1) then
         xaC = aki(ior)*Caki
         xagrow = akgrow
         xchla = chlaki(ior)
      endif
      CChlaz(nkz) = akbcm(ior)
      if (mstr == 2)iaus = 0
      call c_chla(roh_chlz(nkz), xup_n, xakres, cchlaz(nkz), tflie, caki, cchl_stern, xchla, xac, xagrow, isyn)
      iaus = 0
      
      ! Silikat (kein zellinterner Speicher)
      hcon = Si(ior)+akkssi
      if (hcon /= 0.0) hcon = Si(ior)/hcon ! Silikatgehlat der Zelle ist abhängig vom Silikatgehalt im Außenmedium
      if (nkzs(ior) == 1)Q_SKt = Qmx_SK*hcon
      up_Si(ior) = akitbr(ior)-dalgak(ior)
      if (up_Si(ior) /= 0.0)up_Si(ior) = Qmx_SK*hcon*(max(0.0,(dalgki(ior)-dalgak(ior))))/up_Si(ior)
      
      if (nkzs(ior) == 1) then
         dalgkz(1,ior) = dalgki(ior)
         akibrz(1,ior) = akitbr(ior)
         up_NKz(1,ior) = up_NK(ior)
         up_PKz(1,ior) = up_PK(ior)
         up_Siz(1,ior) = up_Si(ior)
      endif
      
      ! ##### SEDIMENTATION DER ALGEN ######
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
      
      akis = askie * aki(ior)
      ised = 1      ! Schalter zur Kennzeichnung der hier berücksichtigten partik. Wasserinhaltsstoffe
      jsed = 1
      ZellV = 1400.
      call sedimentation(tiefe(ior),ised,ust,qsgr,oc,Oc0,tflie,wst,jsed,ZellV,control,jjj)
      if (control) then
         print*,'Sedimentation algaeski: tiefe,ised,ust,qsgr,oc,Oc0,tflie,wst,jsed,ZellV,control,jjj'   &
                  ,tiefe(ior),ised,ust,qsgr,oc,Oc0,tflie,wst,jsed,ZellV,control,jjj
      endif
      ceq = akis * qsgr
      
      SEDALk(ior) = (akis - Ceq) * oc
      Sedalk0(ior) = wst * askie * qsgr
      sedAlg_MQ(mstr,ior) = SEDALk(ior)
      ! if(ieros.eq.1.and.vges.gt.vkrit)sedalk(ior) = 0.0
      
      ! sised-Menge an Silikat an der Gewässersohle infolge sedimentierter K
      sised(ior) = sised(ior)+sedalk(ior)*Q_SK(Ior)
      
      ! neu: Algenmortalität
      akmor = akmomi
      fmor1 = f51
      fmor2 = f52
      fmor3 = f53
      if ((Qmx_NK/Qmn_NK)>=1.25) then
         fmor1 = (Q_NKt-Qmn_NK)/(Qmx_NK-Qmn_NK)
      endif
      
      if ((Qmx_PK/Qmn_PK)>=1.25) then
         fmor2 = (Q_PKt-Qmn_PK)/(Qmx_PK-Qmn_PK)
      endif
      
      fmor = min(fmor1,fmor2)
      akmor_1v = akmor_1(mstr,ior)
      akmor = akmomi+akmoma*(1.-((min(fmor0,fmor))/fmor0)**8.)
      akmor = min(max(akmor,akmomi),akmoma) !!wy stay within limits
      
      if (akmor < akmor_1(mstr,ior)) then
         akmor = akmor_1(mstr,ior)
         akmor_1t = akmor_1(mstr,ior)
      else
         akmor_1t = akmor
      endif
      
      dkimor(ior) = akit*(1.-(exp(-akmor*tflie)))
      if ((dkimor(ior) > huge(dkimor(ior))) .or. (dkimor(ior) < -1*huge(dkimor(ior)))) then
         print*,'akit*(1.-(exp(-akmor*tflie)))',akit,akmor,tflie
         print*,'fmor = min(fmor1,fmor2) ; akmor = akmomi+akmoma*(1.-((min(fmor0,fmor))/fmor0)**8.)'
         print*,'akmomi,akmoma,fmor,fmor0,fmor2 = ',akmomi,akmoma,fmor,fmor0,fmor1,fmor2,fmor3
         print*,'akmor_1(mstr,ior),akmor_1v,akmor_1t = ',akmor_1(mstr,ior),akmor_1v,akmor_1t
      endif 
      
      ! SKmor - aufsummierte abgestorbene Kieselalgen im Wasserkörper
      ! Verringerung der abgestorbenen Kieselalgenbiomasse durch Sedimentation
      ! Annahme: sedimentiert wie lebende Biomasse
      hconoc = 0.0
      if (aki(ior) > 0.0) hconoc = sedalk(ior)/aki(ior)
      SKmor(ior) = (SKmor(ior)+dkimor(ior)*Q_SK(ior))*(1.-hconoc)
      
      ! sised-Menge an Silikat an der Gewässersohle infolge sedimentierter Algen (aufsummiert für den Simulationszeitraum)
      sised(ior) = sised(ior) + SKmor(ior) * hconoc
      
      
      ! 2D-Modellierung
      do nkz = 1,nkzs(ior)
         dkmorz(nkz,ior) = akitz(nkz)*(1.-(exp(-akmor*tflie)))
      enddo
      
      ! Quellen/Senken-Term
      ! Kieselalgen
      hconql = dalgki(ior)+cmatki(ior)
      hconsk = dkimor(ior)+dalgak(ior)+sedalk(ior)+algzok(ior)+algdrk(ior)+algcok(ior)
      akit = aki(ior)+hconql-hconsk
      daki = abs(hconql-hconsk)
      
      if (akit < 0.0) then
         akit_old = akit
         akit = (aki(ior)/(aki(ior)+daki))*aki(ior)
         call print_clipping("algaeski", "akit", akit_old, akit, "mg/l")
      endif
      
      if (akit > huge(akit)) then
         print*,'akit INF',mstr,ior,aki(ior),daki,akit
         print*,'hconql = dalgki(ior)+cmatki(ior)'
         print*,hconql,dalgki(ior),cmatki(ior)
         print*,'hconsk = dkimor(ior)+dalgak(ior)+sedalk(ior)+algzok(ior)+algdrk(ior)+algcok(ior)'
         print*,hconsk,dkimor(ior),dalgak(ior),sedalk(ior),algzok(ior),algdrk(ior),algcok(ior)
      endif 
      if (akit < 1.e-5) akit = 1.e-5
      chlakit = 1.e-5 
      
      if (CChlaz(1) > 0.0) Chlakit = akit*Caki*1000./CChlaz(1)
      if (nkzs(ior) == 1) then
         dkmorz(1,ior) = dkimor(ior)
         algakz(1,ior) = dalgak(ior)
         algzkz(1,ior) = algzok(ior)
         akitz(1) = akit
         chlakizt(1) = Chlakit
      endif
      
      akbcmt = CChlaz(1)
      if (nkzs(ior) > 1) then    ! 2D-Modellierung
         do nkz = 1,nkzs(ior)
            hconql = dalgkz(nkz,ior)+cmatki(ior)
            hconsk = dkmorz(nkz,ior)+algakz(nkz,ior)+sedalk(ior)+algzkz(nkz,ior)+algdrk(ior)+algcok(ior)  !hier
            CChlazvor = CChlaz(1)
            CChlaz(nkz) = CChlazt(nkz)
            
            akitz(nkz) = akiz(nkz,ior)+hconql-hconsk
            daki = abs(hconql-hconsk)
            
            if (akitz(nkz) < 0.0) then
               akitz(nkz) = (akiz(nkz,ior)/(akiz(nkz,ior)+daki))*akiz(nkz,ior)
            endif
            
            Chlakizt(nkz) = akitz(nkz)*Caki*1000./CChlaz(nkz)
            
            if (akitz(nkz) < 1.e-5) then
               akitz(nkz) = 1.e-5
               Chlakizt(nkz) = akitz(nkz)*Caki*1000./CChlaz(nkz)
            endif
         enddo
      endif       ! Ende 2D
      
      
      ! Ausgabeparameter
      akmuea(ior) = akgrow
      ftaaus(ior) = fta
      fheaus(ior) = svhemkt
      akraus(ior) = akres
      ! akraus(ior) = Q_PK(ior)
      it_h_aus = it_h(mstr,ior)
      if (schwi(ior) <= 0.001 .and. isim_end == 0) then
         tpki(ior) = 0.0
         fiaus = 0.0
      else if (schwi(ior) > 0.001) then
         tpki(ior) = F52
         fiaus(ior) = Pcmit/Pcmax*FTA
      else if (isim_end == 1) then
         tpki(ior) = f53  ! F52
         fiaus(ior) = 0.0
      endif
      
   enddo     ! Ende Segmentschleife
   
   aki(anze+1) = akit
   akbcm(anze+1) = akbcmt
   chlaki(anze+1) = chlakit
   Q_PK(anze+1) = Q_PKt
   Q_NK(anze+1) = Q_NKt
   Q_SK(anze+1) = Q_SKt
   svhemk(anze+1) = svhemkt
   
   do nkz = 1,nkzs(anze)
      akiz(nkz,anze+1) = akitz(nkz)
      hchlkz(mstr,nkz,anze+1) = chlakizt(nkz)
      hCChlkz(mstr,nkz,anze+1) = CChlazt(nkz)
      hQ_NKz(mstr,nkz,anze+1) = Q_NKz(nkz)
   enddo
   
end subroutine algaeski
