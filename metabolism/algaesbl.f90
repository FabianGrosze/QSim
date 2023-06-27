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

!> Berechnung des Blaualgenwachstums
!! @author Volker Kirchesch
!! @date 01.08.2012
subroutine algaesbl(schwi, tflie, tempw, rau, tiefe, vmitt, vno3, vnh4, gelp,  &
                    svhemb, chla, dalgbl, dalgab, anze, sedalb, algzob, dblmor,&
                    saettb, vkigr, abbcm, abl, tpbl, fibaus, abmuea, fhebas,   & 
                    abreau, tausc, ischif, ilbuhn, ieros, algdrb, algcob,      &
                    antbl, extk, extk_lamda, ilamda, ack, acg, acb, al,        &
                    vnh4z, vno3z, gelpz, dalgbz, nkzs, dh2d, tempwz, up_pbz,   &
                    up_nbz, q_nb, q_pb,  abltbr, ablbrz, up_n2z, ablz, chlabl, &
                    a1bl, hchlbz, hcchlbz, algabz, algzbz, dz2d, sedalg_mq,    &
                    sedalb0, hq_nbz, mstr, isim_end, abmor_1,                  &
                    control, jjj)
                    
   use module_alloc_dimensions
   use module_aparam
   implicit none
   
   real,    intent(in),    dimension(1000)           :: schwi
   real,    intent(in)                               :: tflie
   real,    intent(in),    dimension(1000)           :: tempw
   real,    intent(in),    dimension(1000)           :: rau
   real,    intent(in),    dimension(1000)           :: tiefe
   real,    intent(in),    dimension(1000)           :: vmitt
   real,    intent(in),    dimension(1000)           :: vno3
   real,    intent(in),    dimension(1000)           :: vnh4
   real,    intent(in),    dimension(1000)           :: gelp
   real,    intent(inout), dimension(1000)           :: svhemb
   real,    intent(in),    dimension(1000)           :: chla
   real,    intent(out),   dimension(1000)           :: dalgbl
   real,    intent(out),   dimension(1000)           :: dalgab
   integer, intent(in)                               :: anze
   real,    intent(out),   dimension(1000)           :: sedalb   
   real,    intent(in),    dimension(1000)           :: algzob   
   real,    intent(out),   dimension(1000)           :: dblmor   
   real,    intent(out)                              :: saettb
   real,    intent(in),    dimension(1000)           :: vkigr
   real,    intent(inout), dimension(1000)           :: abbcm
   real,    intent(inout), dimension(1000)           :: abl
   real,    intent(out),   dimension(1000)           :: tpbl
   real,    intent(out),   dimension(1000)           :: fibaus
   real,    intent(out),   dimension(1000)           :: abmuea
   real,    intent(out),   dimension(1000)           :: fhebas
   real,    intent(out),   dimension(1000)           :: abreau
   real,    intent(in),    dimension(azstrs,1000)    :: tausc
   integer, intent(in),    dimension(1000)           :: ischif
   integer, intent(in)                               :: ilbuhn
   integer, intent(in)                               :: ieros
   real,    intent(in),    dimension(1000)           :: algdrb
   real,    intent(in),    dimension(1000)           :: algcob
   real,    intent(in),    dimension(1000)           :: antbl
   real,    intent(in),    dimension(1000)           :: extk
   real,    intent(in),    dimension(40,1000)        :: extk_lamda
   integer, intent(in)                               :: ilamda
   real,    intent(in),    dimension(40)             :: ack, acg, acb, al
   real,    intent(in),    dimension(50,1000)        :: vNH4z
   real,    intent(in),    dimension(50,1000)        :: vNO3z
   real,    intent(in),    dimension(50,1000)        :: gelPz
   real,    intent(out),   dimension(50,1000)        :: dalgbz
   integer, intent(inout), dimension(1000)           :: nkzs
   real,    intent(inout)                            :: dh2d
   real,    intent(in),    dimension(50,1000)        :: tempwz
   real,    intent(out),   dimension(50,1000)        :: up_PBz
   real,    intent(out),   dimension(50,1000)        :: up_NBz
   real,    intent(inout), dimension(1000)           :: Q_NB
   real,    intent(inout), dimension(1000)           :: Q_PB
   real,    intent(out),   dimension(1000)           :: abltbr
   real,    intent(out),   dimension(50,1000)        :: ablbrz
   real,    intent(out),   dimension(50,1000)        :: up_N2z
   real,    intent(inout), dimension(50,1000)        :: ablz
   real,    intent(inout), dimension(1000)           :: chlabl
   real,    intent(in)                               :: a1Bl
   real,    intent(inout), dimension(azStrs,50,1000) :: hchlbz
   real,    intent(inout), dimension(azStrs,50,1000) :: hCChlbz
   real,    intent(out),   dimension(50,1000)        :: algabz
   real,    intent(out),   dimension(50,1000)        :: algzbz
   real,    intent(in),    dimension(1000)           :: Dz2D
   real,    intent(inout), dimension(azStrs,1000)    :: sedAlg_MQ
   real,    intent(out),   dimension(1000)           :: sedAlb0
   real,    intent(inout), dimension(azStrs,50,1000) :: hQ_Nbz
   integer, intent(in)                               :: mstr
   integer, intent(in)                               :: isim_end
   real,    intent(inout), dimension(azStrs,1000)    :: abmor_1
   logical, intent(in)                               :: control
   integer, intent(in)                               :: jjj

   integer                  :: n_neu_s, n_alt_s, nkz
   integer                  :: j_aus, j, js, jsed
   integer                  :: jcyano, i_zeiger, i
   integer                  :: isyn, ispek, ised
   integer                  :: iref, ior, ifoto, iaus
   real                     :: vkrit, zellv, yk, xup_n, xk, xchla
   real                     :: xagrow, xac, xabres, wst
   real                     :: vges, v6, ust, ustkri
   real                     :: up_n2i, up_ci, upmxi, tiefe1
   real                     :: te0, tauad, sumyk, sumroh_chl
   real                     :: sumqp, sumqn, sumpc, sumn, sumh
   real                     :: sumac, roh_chlzmit, qsgr, qmxi, qmni
   real                     :: pc, pcmit, pcmax, oc, oc0
   real                     :: obfli, hconsk, hconql, halbi
   real                     :: ft_ks, ftemp, fta
   real                     :: fn, fmor, fmor2, fmor1
   real                     :: f5, f52, f51_n2_1d, f51_n2
   real                     :: f51_1d, f51, dz_spline, dz, dztot
   real                     :: dz1, deltaz, dabl, cnaehr
   real                     :: chlablt, ceq, cchl_stern, cchl0, alpha_chl
   real                     :: acmit, abr, abres, abmor, ablt, abls
   real                     :: abgrow, abgmaxtopt
   real                     :: abchl_max, abbcmt, ablt_old
   real                     :: Ihemm, Icz, Ic, N_Cmax, kTresp
   real, dimension(1000)    :: up_PB, up_NB, up_N2
   real, dimension(40)      :: I0, Iz
   real, dimension(50)      :: chlablzt, hc_temp, Pz, F5z, abgrwz, CChlaz
   real, dimension(50)      :: CChlazt, xroh_Chlz, roh_Chlz
   real, dimension(50)      :: Y, YY, abltz, Q_PBz, abresz, xroh_Chl
   real, dimension(100)     :: hemm
   real, dimension(50,1000) :: dbmorz

   real, parameter :: g = 9.81
   real, parameter :: abmomi = 0.02
   real, parameter :: abmoma = 0.8
   real, parameter :: fmor0 = 0.05
   
   external :: lin_spline, lichthemmung, uptake, c_chla, schiff, sedimentation
   external :: print_clipping
   
   save Cchlaz
   
   ispek = 0
   
   ! Lichtabhängigkeit der Fotosyntheserate
   ! 0: nach Ross (2009)
   ! 1: nach Geider (1998)
   ifoto = 1
   
   ! Chlorophyll A Synthese
   ! 0: nach Geider (1998)
   ! 1: nach Geider (1997)
   isyn = 1
   
   if (chla(1) < 0.0) then  ! falls kein Chla-Randbedingung
      do ior = 1,anze+1
         sedalb(ior) = 0.0
         Sedalb0(ior) = 0.0
         sedAlg_MQ(mstr,ior) = 0.0
      enddo
      return
   endif
   
   ! Eingaben
   kTresp = 0.09
   Ihemm = 600.
   tauad = 100.  ! Relaxationszeit der Algen [sec]
   Iref = 140.
   
   Te0 = 20.
   if (abChl>=86.) then
      abchl_max = 86.
   else
      abChl_max = 68.
      if (abChl == 20.6)abChl_max = 20.6
      if (abChl == 35.)abChl_max = 35.
   endif
   ! C:Chla = abchl * exp(-a1*T) * a2 * I * exp(-a3*T)
   
   ! C:Chla bei 0°C [mgC/mgChla]
   CChl0 = abchl * exp(-a1Bl * Te0)
   if (abChl > abChl_max) then
      CChl0 = abChl_max * exp(-a1bl * Te0)
   endif
   
   ! Umrechnung der maximalen Wachstumsrate bei 20°C auf Wachstumsrate unter
   ! optimalen Temperatur-Bedingungen
   abgmaxTopt = abgmax
   ! nach Geider (1998)
   abgmaxTopt = abgmax / (exp(-kTemp_Bl*(Te0-ToptB)**2))
   upmxPB = abgmaxTopt * Qmx_PB
   upmxNB = abgmaxTopt * Qmx_NB
   
   
   ! Beginn der Segmentschleife
   do ior = 1,anze+1
      sumac = 0.0
      do i = 1,ilamda
         sumac = sumac                                     &
               + ack(i) * vkigr(ior)                       &
               + acg(i) * (1. - vkigr(ior) - antbl(ior))   &
               + acb(i) * antbl(ior)
      enddo
      acmit = sumac / ilamda
      
      ! schwi*4.2 - Umrechnung von cal/(cm2*h) in J/(cm2*h)
      ! Lichtgeschwindigkeit in Luft, da µE bei Wachstumsversuchen in Luft gemessen.
      obfli = 5.846 * (schwi(ior)*4.2)   
      if (obfli < 0.001) obfli = 0.0
      
      ! Spektrale Auflösung der photosynthetisch aktiven Strahlung
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
         abl(ior-1) = ablt
         chlabl(ior-1) = chlablt
         abbcm(ior-1) = abbcmt
         do nkz = 1,nkzs(ior-1)
            ablz(nkz,ior-1) = abltz(nkz)
            hchlbz(mstr,nkz,ior-1) = chlablzt(nkz)
            hCChlbz(mstr,nkz,ior-1) = CChlazt(nkz)
         enddo
      endif
      
      if (ilbuhn == 1 .and. tiefe(ior) < 0.05) cycle
      
      ! Temperaturabhaengigkeit der Respirationsrate
      ftemp = exp(ktresp * (tempw(ior) - 20.))
      
      
      ! Temperaturabhaengigkeit der Wachstumsrate
      fta = exp(-ktemp_bl * (tempw(ior) - toptb)**2)
      fta = max(0.01, fta) 
     
      ! Berechnung der Schubspannungsgeschwindigkeit
      fn = 1./rau(ior)
      ust = ((fn*g**0.5)/tiefe(ior)**0.166667)*abs(vmitt(ior))
      
      ! Berechnung des mittleren vertikalen Dispersionskoeffizient
      ! nach Fischer im ein-dimensionalen Fall (gute Näherung)
      dztot = 0.4 * ust * tiefe(ior) / 6.
      
      if (nkzs(ior) /= 1 .and. Dz2D(ior) /= 0.0) then
         dztot = Dz2D(ior)
      endif
      dz = sqrt(tauad * 2. * dztot)
      
      ! max C-spezifische Photosyntheserate bei optimal Temperatur
      PCmax = (abgmaxTopt + abremi * exp(kTresp*(ToptB-20.))) / (1.-frmube)
      
      ! Berechnung der Lichtabsorption im gesamten Wasserkörper
      if (dz > 0.0) then
         js = int(tiefe(ior)/dz)   ! Division durch Null vermeiden
      else
         js = 0
      endif
      
      if (js > 30) then
         js = 30
         dz = tiefe(ior) / js
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
         dz_spline = dz
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
            Y(nkz) = hCChlbz(mstr,nkz,ior)
         enddo
         
         call lin_spline(dH2D, dz_spline, deltaz, n_alt_s, n_neu_s, Y, YY,i_zeiger)
         do j = 1,n_neu_s
            CChlaz(j) = YY(j)
         enddo
      endif
      
      ! Nährstoffabhängigkeit des Wachstums
      ! Temperaturabhängigkeit von KP, KN,KSi
      ! fT_Ks = 1.15**(20.-tempw(ior))
      fT_Ks = 1.
      
      f51_n2 = 0.0
      if (qmx_nb / qmn_nb < 1.25) then
         f51 = (vno3(ior) + vnh4(ior)) / (abksn * ft_ks + vno3(ior) + vnh4(ior))
         if (ifix == 1) then
            f51_n2_1d = 1. - f51
            f51_1d = f51
            f51 = 1.
         endif
      else
         f51 = (q_nb(ior)-qmn_nb)/(qmx_nb-qmn_nb)
         if (ifix == 1) then
            f51_n2_1d = 1. - f51
            f51_1d = f51
            f51 = 1.
         endif
      endif
      
      if (Qmx_PB/Qmn_PB < 1.25) then
         F52 = gelP(ior) / (abksp * ft_ks + gelP(ior))
      else
         F52 = (Q_PB(ior) - Qmn_PB) / (Qmx_PB - Qmn_PB)
      endif
      
      F5 = min(F51,F52)
      F51_1D = F51
      
      do j = 1, js ! Schleife über die Schichten Anfang
         dz1 = dz
         if (j == js .and. deltaz > 0.0) dz = deltaz
         
         ic = 0.0
         if (ispek == 1) then
            do i = 1,ilamda
               iz(i) = i0(i)*exp(-extk_lamda(i,ior)*dz)
               ic = ic + max(0.0,(i0(i)/(extk(ior)*dz))*(1.-exp(-extk(ior)*dz)))
            enddo
         else
            icz = i0(1)*exp(-extk(ior)*dz)
            ic = max(0.0,(i0(1)/(extk(ior)*dz))*(1.-exp(-extk(ior)*dz)))
         endif
         
         if (nkzs(ior) == 1) then
            hc_temp(j) = tempw(ior)
            cchlaz(j) = abbcm(ior)
         endif
         
         yK = 1. - svhemb(ior)
         CChl_Stern = CChl0 * exp(a1Bl * hc_temp(j))    ! dunkeladaptierte Algen
         call lichthemmung(tflie, ic, yk, cchl_stern, cchlaz(j))
         hemm(j) = yK
         
         if (ikbe == 191.) then
            Saettb = ikbe * 3.9   * exp(-0.068 * hc_temp(j))
         else
            Saettb = ikbe * 0.525 * exp(0.0322 * hc_temp(j))
         endif
         
         alpha_chl = PCmax * FTA * CChl_Stern / (Saettb * 86400.)
         
         select case(ifoto)
            case(0)
               Pc = Pcmax * FTA * (1.-exp(-Ic/Saettb)) * hemm(j)
               
            case(1)
               Pc = Pcmax * FTA * (1.-exp(-Ic*alpha_Chl/(max(CChl_Stern,CChlaz(j))*Pcmax*FTA/86400.)))*hemm(j)
         end select
         
         ! Berechnung roh_Chl (wird für die Neuberechnung der Chlorophyll-a-Konzentration benötigt)
         N_Cmax = Qmx_NB/Cabl
         xroh_Chlz(j) = CChlaz(j) / (alpha_Chl * (max(0.1,Ic))  *N_Cmax) ! 2D-Fall
         
         select case(isyn)
            case(0)
               xroh_Chl = xroh_Chlz(j) * PC * F5 / 86400. ! 1D-Fall
               xroh_Chlz(j) = xroh_Chlz(j) * PC / 86400.
               
            case(1)
               xroh_Chlz(j) = CChlaz(j) / (alpha_Chl*(max(0.1,Ic)))
               xroh_Chl = xroh_Chlz(j) * PC * F5 / 86400. ! 1D-Fall
               xroh_Chlz(j) = xroh_Chlz(j) * PC / 86400.
         end select
         
         Pz(j) = PC
         sumPc = sumPc + Pz(j) * dz
         sumRoh_Chl = sumRoh_Chl + xroh_Chl(j) * dz
         sumH = sumH + dz
         
         if (ispek == 1) then
            I0(:) = Iz(i)
         else
            I0(1) = Icz
         endif
         
      enddo ! Ende Schleife
      
      Pcmit = sumPc / sumH
      roh_Chlzmit = sumRoh_Chl / sumH
      
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
         sumH = 0.0
         sumRoh_Chl = 0.0
         call lin_spline(dz_spline, dH2D, deltaz, js, n_neu_s, xroh_Chlz, YY,i_zeiger)
         do nkz = 1,nkzs(ior)
            roh_Chlz(nkz) = YY(nkz)
         enddo
      endif
      
      ! Mittelwertbildung der Hemmung
      sumyK = 0.0
      sumH = 0.0
      
      if (js == 1) then
         svhemb(ior) = 1. - hemm(1)
      else
         do j = 1,js
            sumyK = sumyK + hemm(j) * dz
            sumH = sumH + dz
         enddo
         svhemb(ior) = 1.-(sumyK/sumH)
      endif
      
      if (nkzs(ior) > 1) then
      
         ! 2D-Modellierung
         do nkz = 1,nkzs(ior)
            ! Nährstoffabhängigkeit (2D-Fall)
            f51_n2 = 0.0
            if (qmx_nb/qmn_nb < 1.25) then
               f51 = (vno3z(nkz,ior) + vnh4z(nkz,ior)) / (abksn * ft_ks + vno3z(nkz,ior) + vnh4z(nkz,ior))
            
            else
               f51 = (hq_nbz(mstr,nkz,ior)-qmn_nb) / (qmx_nb - qmn_nb)
            endif
            
            if (ifix == 1) then
               f51_n2 = 1. - f51
               f51 = 1.
            endif
            
            if (qmx_pb/qmn_pb < 1.25) then
               f52 = gelpz(nkz,ior)/(abksp*ft_ks+gelpz(nkz,ior))
            endif
            f5z(nkz) = min(f51,f52)
            if (f5z(nkz) < 0.0) f5z(nkz) = 0.0
         enddo
      endif
      
     
      ! Berechnung der Respirationsrate
      abgrow = Pcmit * f5
      abres = abgrow * frmube + abremi * ftemp
      
      ablt = abl(ior) * exp(abgrow * tflie)
      dalgbl(ior) = ablt-abl(ior)
      dalgab(ior) = ablt*(1.-(exp(-abres*tflie)))
      ablt = ablt - dalgab(ior)
      
      if (nkzs(ior) > 1) then
         ! 2D-Modellierung
         sumQN = 0.0
         sumQP = 0.0
         sumH = 0.0
         
         do nkz = 1,nkzs(ior)
            
            ! Berechnung der Respirationsrate
            abgrwz(nkz) = Pz(nkz)*F5z(nkz)
            abresz(nkz) = abgrwz(nkz) * frmube + abremi*ftemp
            abltz(nkz) = ablz(nkz,ior)*exp(abgrwz(nkz)*tflie)
            dalgbz(nkz,ior) = abltz(nkz)-ablz(nkz,ior)
            algabz(nkz,ior) = abltz(nkz)*(1.-(exp(-abresz(nkz)*tflie)))
            abltz(nkz) = abltz(nkz) - algabz(nkz,ior)
            
            ! neue zellulären Nährstoffgehalte
            ablbrz(nkz,ior) = abltz(nkz)
            
            if ((Qmx_PB/Qmn_PB) <= 1.25) then
               up_PBz(nkz,ior) = Qmx_PB*(max(0.0,(dalgbz(nkz,ior)-algabz(nkz,ior))))/(ablbrz(nkz,ior)-algabz(nkz,ior))
               Q_PBz(nkz) = Qmx_PB
            else
               ! Phosphor
               yk = Q_PB(ior)
               xk = abgrwz( nkz) - abresz(nkz)
               Qmxi = Qmx_PB
               Qmni = Qmn_PB
               CNaehr = gelPz(nkz,ior)
               Halbi = abksP*ft_ks
               upmxi = upmxPB * FTA
               jcyano = 0
               j_aus = 0
               call uptake(yk, xk, Qmxi, Qmni, CNaehr, Halbi, upmxi, tflie, up_Ci, up_N2i, jcyano, ifix)
               up_PBz(nkz,ior) = up_Ci
               Q_PBz(nkz) = yk
            endif
            
            ! Stickstoff
            jcyano = 1
            sumN = vNH4z(nkz,ior)+vNO3z(nkz,ior)
            if ((Qmx_NB/Qmn_NB) <= 1.25) then
               up_NBz(nkz,ior) = Qmx_PB*(max(0.0,(dalgbz(nkz,ior)-algabz(nkz,ior))*(F51-F51_N2)))/(ablbrz(nkz,ior)-algabz(nkz,ior))
               up_N2z(nkz,ior) = Qmx_PB*(max(0.0,(dalgbz(nkz,ior)-algabz(nkz,ior))*F51_N2))/(ablbrz(nkz,ior)-algabz(nkz,ior))
               if (up_N2z(nkz,ior) < 0.0)up_N2z(nkz,ior) = 0.0
               hQ_NBz(mstr,nkz,ior) = Qmx_NB
            else
               yk = hQ_NBz(mstr,nkz,ior)
               xk = abgrwz(nkz) - abresz(nkz)
               Qmxi = Qmx_NB
               Qmni = Qmn_NB
               CNaehr = sumN
               Halbi = abksN*ft_ks
               upmxi = upmxNB * FTA
               call uptake(yk, xk, Qmxi, Qmni, CNaehr, Halbi, upmxi, tflie, up_Ci, up_N2i, jcyano, ifix)
               
               up_NBz(nkz,ior) = up_Ci
               up_N2z(nkz,ior) = up_N2i
               hQ_NBz(mstr,nkz,ior) = yk
            endif
            xup_N = up_NBz(nkz,ior) + up_N2z(nkz,ior)
            
            ! Neuberechnung der Chlorophyll-a-Konzentration
            roh_Chlz(nkz) = roh_Chlz(nkz) * F5z(nkz)
            xabres = abgrwz(nkz) - abresz(nkz)
            xup_N = up_NBz(nkz,ior) + up_N2z(nkz,ior)
            if (isyn == 1) then
               roh_Chlz(nkz) = roh_Chlz(nkz) * F51
               xaC = ablz(nkz,ior)*Cabl
               xagrow = abgrwz(nkz)
               xchla = hChlbz(mstr,nkz,ior)
            endif
            iaus = 0
            call c_chla(roh_chlz(nkz), xup_n, xabres, cchlaz(nkz), tflie, cabl, cchl_stern, xchla, xac, xagrow, isyn)
            CChlazt(nkz) = CChlaz(nkz)
            if (nkz > 1) then
               sumQN = sumQN + ((hQ_NBz(mstr,nkz-1,ior)+hQ_NBz(mstr,nkz,ior))/2.)*dH2D
               sumQP = sumQP + ((Q_PBz(nkz-1)+Q_PBz(nkz))/2.)*dH2D
               sumH = sumH + dH2D
            endif
            
         enddo ! Schleifenende
         
         Q_NB(ior) = min(Qmx_NB,sumQN/sumH)
         Q_PB(ior) = min(Qmx_PB,sumQP/sumH)
         
      endif  ! Ende 2D
      
      
      abltbr(ior) = ablt
      sumn = vnh4(ior) + vno3(ior)
      if (qmx_pb/qmn_pb <= 1.25) then
         up_pb(ior) = qmx_pb*(max(0.0,(dalgbl(ior)-dalgab(ior))))/(abltbr(ior)-dalgab(ior))
         q_pb(ior) = qmx_pb
      else
         ! Phosphor
         yk = q_pb(ior)
         xk = abgrow - abres
         qmxi = qmx_pb
         qmni = qmn_pb
         cnaehr = gelp(ior)
         abr = abltbr(ior)
         halbi = abksp * ft_ks
         upmxi = upmxpb * fta
         jcyano = 0
         j_aus = 0
         call uptake(yk, xk, Qmxi, Qmni, CNaehr, Halbi, upmxi, tflie, up_Ci, up_N2i, jcyano, ifix)
         up_pb(ior) = up_ci
         q_pb(ior) = yk
      endif
      
      ! Stickstoff
      jcyano = 1
      if ((Qmx_NB/Qmn_NB) <= 1.25) then
         up_NB(ior) = Qmx_NB*(max(0.0,(dalgbl(ior)-dalgab(ior))*(F51_1D-F51_N2_1D)))/(abltbr(ior)-dalgab(ior))
         up_N2(ior) = Qmx_NB*(max(0.0,(dalgbl(ior)-dalgab(ior))*F51_N2_1D))/(abltbr(ior)-dalgab(ior))
         if (up_N2(ior) < 0.0)up_N2(ior) = 0.0
         Q_NB(ior) = Qmx_NB
      else
         yk = Q_NB(ior)
         xk = abgrow - abres
         Qmxi = Qmx_NB
         Qmni = Qmn_NB
         CNaehr = sumN
         Halbi = abksN*ft_ks
         upmxi = upmxNB * FTA
         call uptake(yk, xk, Qmxi, Qmni, CNaehr, Halbi, upmxi, tflie, up_Ci, up_N2i, jcyano, ifix)
         
         up_NB(ior) = up_Ci
         up_N2(ior) = up_N2i
         Q_NB(ior) = yk
      endif
      
      ! Neuberechnung der Chlorophyll-a-Konzentration (1D-Fall)
      roh_Chlz(1) = roh_Chlzmit
      nkz = 1
      xabres = abgrow - abres
      xup_N = up_NB(ior) + up_N2(ior)
      if (isyn == 1) then
         xaC = abl(ior)*Cabl
         xagrow = abgrow
         xchla = chlabl(ior)
      endif
      CChlaz(nkz) = abbcm(ior)
      iaus = 0
      call c_chla(roh_chlz(nkz), xup_n, xabres, cchlaz(nkz), tflie, cabl, cchl_stern, xchla, xac, xagrow, isyn)
      iaus = 0
      
      if (nkzs(ior) == 1) then
         dalgbz(1,ior) = dalgbl(ior)
         ablbrz(1,ior) = abltbr(ior)
         up_PBz(1,ior) = up_PB(ior)
         up_NBz(1,ior) = up_NB(ior)
         up_N2z(1,ior) = up_N2(ior)
      endif
      
      
      ! Sedimentation der Algen
      ! Schiffseinfluss
      ustkri = sqrt(tausc(mstr,ior) / 1000.)
      vkrit = (ustkri*tiefe(ior)**0.166667)/((1./rau(ior))*g)
      
      tiefe1 = tiefe(ior)
      if (ischif(ior) == 0) then
         v6 = 0.
      else
         call schiff(vmitt(ior), tiefe1, ischif(ior), v6)
      endif
      vges = vmitt(ior) + v6
      
      abls = asble * abl(ior)
      ised = 1      ! Schalter zur Kennzeichnung der hier berücksichtigten partik. Wasserinhaltsstoffe
      jsed = 1
      ZellV = 1000.
      call sedimentation(tiefe(ior),ised,ust,qsgr,oc,Oc0,tflie,wst,jsed,ZellV,control,jjj)
      ceq = abls*qsgr
      
      sedalb(ior) = (abls - ceq) * oc
      sedalb0(ior) = wst * asble * qsgr
      sedalg_mq(mstr,ior) = sedalg_mq(mstr,ior) + sedalb(ior)
      
      if (ieros == 1 .and. vges > vkrit) sedalb(ior) = 0.0
      
      ! Algenmortalität
      if (Qmx_NB/Qmn_NB >= 1.25) then
         if (ifix == 1) then
            fmor1 = 1.
         else
            fmor1 = (Q_NB(ior)-Qmn_NB)/(Qmx_NB-Qmn_NB)
         endif
      else
         fmor1 = f51
      endif
      
      if (Qmx_PB/Qmn_PB >= 1.25) then
         fmor2 = (Q_PB(ior)-Qmn_PB)/(Qmx_PB-Qmn_PB)
      else
         fmor2 = f52   
      endif
      
      fmor = min(fmor1,fmor2)
      abmor = abmomi + abmoma*(1.-((min(fmor0,fmor))/fmor0)**8.)
      abmor = min(max(abmor,abmomi),abmoma) !!wy stay within limits
      if (abmor < abmor_1(mstr,ior)) then
         abmor = abmor_1(mstr,ior)
      else
         abmor_1(mstr,ior) = abmor
      endif
      dblmor(ior) = ablt * (1. - (exp(-abmor * tflie)))
      
      ! 2D-Modellierung
      do nkz = 1,nkzs(ior)
         dbmorz(nkz,ior) = abltz(nkz)*(1.-(exp(-abmor*tflie)))
      enddo
      
      ! Quellen/Senken-Term
      ! Kieselalgen
      
      hconql = dalgbl(ior)
      hconsk = dblmor(ior)    &
             + dalgab(ior)    &
             + sedalb(ior)    &
             + algzob(ior)    &
             + algdrb(ior)    &
             + algcob(ior)
             
      ablt = abl(ior) + hconql - hconsk
      dabl = abs(hconql-hconsk)
      if (ablt < 0.0) then
         ablt_old = ablt
         ablt = (abl(ior)/(abl(ior)+dabl))*abl(ior)
         if (ablt < 1.e-5) ablt = 1.e-5
         call print_clipping("algaesbl", "ablt", ablt_old, ablt, "mg/l")
      endif
      
      
      if (CChlaz(1) > 0.0) then
         Chlablt = ablt * Cabl * 1000. / CChlaz(1)
      else
         Chlablt = 1.e-5 
      endif
      
      
      if (nkzs(ior) == 1) then
         dbmorz(1,ior) = dblmor(ior)
         algabz(1,ior) = dalgab(ior)
         algzbz(1,ior) = algzob(ior)
         abltz(1) = ablt
         chlablzt(1) = Chlablt
      
      else
         do nkz = 1,nkzs(ior)
            hconql = dalgbz(nkz,ior)
            hconsk = dbmorz(nkz,ior)+algabz(nkz,ior)+sedalb(ior)+algzbz(nkz,ior)+algdrb(ior)+algcob(ior)
            CChlaz(nkz) = CChlazt(nkz)
            abltz(nkz) = ablz(nkz,ior)+hconql-hconsk
            dabl = abs(hconql-hconsk)
            if (abltz(nkz) < 0.0) then
               abltz(nkz) = (ablz(nkz,ior)/(ablz(nkz,ior)+dabl))*ablz(nkz,ior)
            endif
            Chlablzt(nkz) = abltz(nkz)*Cabl*1000./CChlaz(nkz)
            if (abltz(nkz) < 1.e-5) then
               abltz(nkz) = 1.e-5
               Chlablzt(nkz) = abltz(nkz)*Cabl*1000./CChlaz(nkz)
            endif
         enddo
      endif      ! Ende 2D
      abbcmt = CChlaz(1)
      
      ! Ausgaben
      abmuea(ior) = abgrow
      fhebas(ior) = svhemb(ior)
      abreau(ior) = abres
      ! abreau(ior) = abbcm(ior)
      
      if (schwi(ior) <= 0.001 .and. isim_end == 0) then
         tpbl(ior) = 0.0
         fibaus = 0.0
      else if (schwi(ior) > 0.001) then
         tpbl(ior) = F52
         fibaus(ior) = Pcmit/Pcmax*FTA
      else if (isim_end == 1) then
         tpbl(ior) = F52
         fibaus(ior) = 0.0
      endif
      
   enddo     ! Ende Segmentschleife
   abl(anze+1) = ablt
   chlabl(anze+1) = chlablt
   abbcm(anze+1) = abbcmt
   
   do nkz = 1,nkzs(anze+1)
      ablz(nkz,anze+1) = abltz(nkz)
      hchlbz(mstr,nkz,anze+1) = chlablzt(nkz)
      hCChlbz(mstr,nkz,anze+1) = CChlazt(nkz)
   enddo

end subroutine algaesbl
