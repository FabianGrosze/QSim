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

!> Ermittlung der SoffFluxe aus dem Sediment
!!
!! DiToro 2001; QUAL2K
!!
!! @author: Volker Kirchesch
!! @date 12.11.2008
subroutine sedflux(tiefe,vmitt,rau,sedAlg_MQ,hSedOM,hw2,hBedGS,hsedvvert,hdKorn,vO2,vNO3,vNH4,gelP,Tempw,anze,mstr       &
                   ,hJNO3,hJNH4,hJPO4,hJO2,hJN2,sedalk,sedalg,sedalb,sedSS_MQ,KNH4e,kapN3e                               &
                   ,tflie,ilbuhn,itags,monats,uhrz,vo2z,vnh4z,vno3z,gelPz,nkzs,SorpCape,Klange                           &
                   ,KdNh3e,fPOC1e,fPOC2e,orgCsd_abb,hCD,JDOC1,JDOC2,Q_NK,Q_PK,Q_NG,Q_PG,Q_NB                             &
                   ,Q_PB,pl0,nl0,Si,hSised,hJSi,aki,agr,abl,Chlaki,Chlagr,Chlabl,hFluN3,ilang,iwied,yNmx1e,Stks1e &
                   ,obsb,ocsb,kontroll,jjj)
   
   use allodim
   implicit none
   
   integer                        :: mstr, monats, kdsi2, kdsi1, i
   integer                        :: izaehl_str, iwied, it, itags, ior
   integer                        :: ilbuhn, ilang, ibedg, ibedgs
   real                           :: temps, zwxalphals, zwsumsdss_mq, zwsumsdalg_mq, zwsumpopsed
   real                           :: zwsumponsed, zwsumpocsed, zwsumdw2, zwjpop_neu, zwjpon_neu
   real                           :: zwjpoc_neu, ynmx1e, w20, w12, vvert
   real                           :: vvert2, vvert1, vcb, vbc, uhrz
   real                           :: thtasi, thtaom, thtan4, thtan3, thtad
   real                           :: thtadp, thtac4, tflie, sumw21, sumpopsedz
   real                           :: sumponsedz, sumpocsedz, sumdw22, sumdw21, stks1e
   real                           :: sorpcape, sorpcap1, sod, sodinit, sisaett
   real                           :: si2, si1, si0, sech, psi2
   real                           :: psi2init, psi1, poro2, poro1, popsed
   real                           :: popsed2, popsed1, ponsed, ponsed2, ponsed1
   real                           :: poc_1, pocsed, pocsed2, pocsed1, poc1_r
   real                           :: po42tinit, po42por, po41por, po40, po40z
   real                           :: o2cpo4, o20, hjsiz, hjpo4z, hjo2z
   real                           :: hjno3z, hjnh4z, hjch4aqz, hcono, hconn
   real                           :: hcond2, hcond1, hcon2, hcon1, hcalphals
   real                           :: h2, h1, h11, g, ftpom, adsorbP
   real                           :: fsi_c, fpsi2, fpsi1, fpom, fpoc2e
   real                           :: fpoc1e, fpoc0, fp2, fp1, fo_nit
   real                           :: fdsi2, fdsi1, fdnc_n, fd2, fd1_aus
   real                           :: fd1, fc_p, fc_o2, fc_n, fakh
   real                           :: dsumdw22, dsumdw21, dp, dpocvert, dkdp41
   real                           :: dim_kn, difkp2, difkp1, diffk2, diffk1
   real                           :: dichto, dichte, dichta, delta_adp, dalgvert
   real                           :: csod, csodmx, csod1, ch4sat, ch41
   real                           :: ch40, caki, cagr, cabl, c4toc2
   real                           :: bettf, benmx1, benks1, aus_kd, alphals
   integer                        :: anze, anzZschritt
   integer, dimension(1000)       :: nkzs
   real, dimension(1000)          :: Tiefe,vmitt,rau,Tempw,vo2,vNH4,vNO3,gelP,Si,JCH4aq
   real, dimension(1000)          :: sedalk,sedalg,sedalb,Q_NK,Q_PK,Q_NG,Q_PG, aki, agr ,abl, Chlaki, Chlagr, Chlabl
   real, dimension(1000)          :: Q_NB,Q_PB,pl0,nl0, JDOC1,JDOC2, obsb, ocsb
   real, dimension(azStrs,1000)   :: hSedOM,hBedGS,hsedvvert,hw2,hJNO3,hJNH4,hJPO4,hJO2,hdKorn, hSised , hJN2
   real, dimension(50,1000)       :: vnh4z, vno3z, gelPz, vo2z
   real, dimension(azStrs,1000)   :: orgCsd_abb, sedAlg_MQ
   real, dimension(azStrs,1000)   :: hJSi, hFluN3
   real, dimension(azStrs,1000)   :: sedSS_MQ
   real, dimension(azStrs,2,1000) :: hCD
   real                           :: KNH4,KNH4e,KL12,N4toN3,KM_NH4,KMO_N4,KMO_NO3,KM_NO3,kSi, JPSi
   real                           :: kappC, KdNh31,KdNh32,kappN3,kapN3e,km_PSi
   real                           :: KdPO42,KdPO41,JDeniG,NO30,NH40,NSOD,langCon1,Klange,KdNh3e,kappaSi1,kappaSi2
   real                           :: Jc,JN,Jc1,JN1,Jcneu,JPON,JPOC,N20
   real                           :: JPOP,JC4N31,JC4N32,JP1,JP
   real, dimension(2)             :: Denit ,JDenit , NH4, NH4T, NO3, Sipor, PSi, SSi, SiT, N2
   real, dimension(3)             :: KdiaPC,POC2,PON1,PON2,POC1,POP2,POP1,PO4z1,PO4z2,fPOC
   real, dimension(2)             :: PO4T,PO4, DOC0, DOC1, DOC2, xk1DOC
   real                           :: m1,m2,KL01P,KL12P
   double precision               :: s,a11,a12,a21,a22,b1,b2
   logical, intent(in)            :: kontroll  !< debugging
   integer, intent(in)            :: jjj       !< debugging
   
   real, dimension(:,:), allocatable, save :: sumPOCsed, sumPONsed, sumPOPsed, sumsdSS_MQ, sumsdAlg_MQ, sumdw2, xalphals, JPOC_neu
   real, dimension(:,:), allocatable, save :: JPON_neu, JPOP_neu, bsumPOCsed, bsumPONsed, bsumPOPsed, bsumsdSS_MQ
   real, dimension(:,:), allocatable, save :: bsumsdAlg_MQ, bsumdw2, bxalphals, bJPOC_neu, bJPON_neu, bJPOP_neu,POCvert1,POCvert2
   
   external :: sed_diffk, lin_sys
   
   save anzZschritt, izaehl_Str
   
   if (.not.allocated(sumPOCsed))   allocate(sumPOCsed(azStrs,1000))
   if (.not.allocated(sumPONsed))   allocate(sumPONsed(azStrs,1000))
   if (.not.allocated(sumPOPsed))   allocate(sumPOPsed(azStrs,1000))
   if (.not.allocated(sumsdSS_MQ))  allocate(sumsdSS_MQ(azStrs,1000))
   if (.not.allocated(sumsdAlg_MQ)) allocate(sumsdAlg_MQ(azStrs,1000))
   if (.not.allocated(sumdw2))      allocate(sumdw2(azStrs,1000))
   if (.not.allocated(xalphals))    allocate(xalphals(azStrs,1000))
   if (.not.allocated(JPOC_neu))    allocate(JPOC_neu(azStrs,1000))
   if (.not.allocated(JPON_neu))    allocate(JPON_neu(azStrs,1000))
   if (.not.allocated(JPOP_neu))    allocate(JPOP_neu(azStrs,1000))
   if (.not.allocated(bsumPOCsed))  allocate(bsumPOCsed(azStrs,1000))
   if (.not.allocated(bsumPONsed))  allocate(bsumPONsed(azStrs,1000))
   if (.not.allocated(bsumPOPsed))  allocate(bsumPOPsed(azStrs,1000))
   if (.not.allocated(bsumsdSS_MQ)) allocate(bsumsdSS_MQ(azStrs,1000))
   if (.not.allocated(bsumsdAlg_MQ))allocate(bsumsdAlg_MQ(azStrs,1000))
   if (.not.allocated(bsumdw2))     allocate(bsumdw2(azStrs,1000))
   if (.not.allocated(bxalphals))   allocate(bxalphals(azStrs,1000))
   if (.not.allocated(bJPOC_neu))   allocate(bJPOC_neu(azStrs,1000))
   if (.not.allocated(bJPON_neu))   allocate(bJPON_neu(azStrs,1000))
   if (.not.allocated(bJPOP_neu))   allocate(bJPOP_neu(azStrs,1000))
   if (.not.allocated(POCvert1))    allocate(POCvert1(azStrs,1000))
   if (.not.allocated(POCvert2))    allocate(POCvert2(azStrs,1000))
   diffK1 = 0.0
   diffk2 = 0.0
   Caki = 0.48
   Cagr = 0.48
   Cabl = 0.48
   iBedGs = 1
   if (ilbuhn == 0)izaehl_Str = izaehl_Str + 1
   if (ilang == 0) then
      do ior = 1, anze + 1
         sumsdSS_MQ(mstr,ior) = 0.0
         sumsdAlg_MQ(mstr,ior) = 0.0
      enddo
      if (izaehl_Str == 1 .and. ilbuhn == 0)anzZschritt = 0
   endif
   if (izaehl_Str == 1 .and. ilbuhn == 0)anzZschritt = anzZschritt + 1
   do ior = 1, anze + 1  ! Knotenschleife
      if (kontroll) then
         print*,'sedflux anfang Knotenschleife: JPOC_neu,sumPOCsed,mstr,ior = '  &
               ,JPOC_neu(mstr,ior),sumPOCsed(mstr,ior),mstr,ior
         print*,'DOC1(),DOC2()',DOC1(:),DOC2(:)
         print*,'POC1(),POC2()',POC1(:),POC2(:)
      endif
      !print*,"sedflux mstr,ior,vo2(ior)",mstr,ior,vo2(ior)
      if (iwied == 0) then
         sumPOCsed(mstr,ior) = 0.0
         sumPONsed(mstr,ior) = 0.0
         sumPOPsed(mstr,ior) = 0.0
         sumsdSS_MQ(mstr,ior) = 0.0
         sumsdAlg_MQ(mstr,ior) = 0.0
         sumdw2(mstr,ior) = 0.0
         xalphals(mstr,ior) = 0.0
         JPOC_neu(mstr,ior) = 0.0
         JPON_neu(mstr,ior) = 0.0
         JPOP_neu(mstr,ior) = 0.0
         bsumPOCsed(mstr,ior) = 0.0
         bsumPONsed(mstr,ior) = 0.0
         bsumPOPsed(mstr,ior) = 0.0
         bsumsdSS_MQ(mstr,ior) = 0.0
         bsumsdAlg_MQ(mstr,ior) = 0.0
         bsumdw2(mstr,ior) = 0.0
         bxalphals(mstr,ior) = 0.0
         bJPOC_neu(mstr,ior) = 0.0
         bJPON_neu(mstr,ior) = 0.0
         bJPOP_neu(mstr,ior) = 0.0
         POCvert1(mstr,ior) = 0.0
         POCvert2(mstr,ior) = 0.0
      endif
      
      if (ilbuhn == 1) then
         zwsumPOCsed = sumPOCsed(mstr,ior)
         zwsumPONsed = sumPONsed(mstr,ior)
         zwsumPOPsed = sumPOPsed(mstr,ior)
         zwsumsdSS_MQ = sumsdSS_MQ(mstr,ior)
         zwsumsdAlg_MQ = sumsdAlg_MQ(mstr,ior)
         zwsumdw2 = sumdw2(mstr,ior)
         zwxalphals = xalphals(mstr,ior)
         zwJPOC_neu = JPOC_neu(mstr,ior)
         zwJPON_neu = JPON_neu(mstr,ior)
         zwJPOP_neu = JPOP_neu(mstr,ior)
         
         sumPOCsed(mstr,ior) = bsumPOCsed(mstr,ior)
         sumPONsed(mstr,ior) = bsumPONsed(mstr,ior)
         sumPOPsed(mstr,ior) = bsumPOPsed(mstr,ior)
         sumsdSS_MQ(mstr,ior) = bsumsdSS_MQ(mstr,ior)
         sumsdAlg_MQ(mstr,ior) = bsumsdAlg_MQ(mstr,ior)
         sumdw2(mstr,ior) = bsumdw2(mstr,ior)
         xalphals(mstr,ior) = bxalphals(mstr,ior)
         JPOC_neu(mstr,ior) = bJPOC_neu(mstr,ior)
         JPON_neu(mstr,ior) = bJPON_neu(mstr,ior)
         JPOP_neu(mstr,ior) = bJPOP_neu(mstr,ior)
      endif
      !      POCvert1 = 0.0
      !      POCvert2 = 0.0
      vvert = max(0.0,hsedvvert(mstr,ior))*24./1000.       ! hsedvvert in mm/h; vvert in m/d
      sumsdSS_MQ(mstr,ior) = sumsdSS_MQ(mstr,ior) + sedSS_MQ(mstr,ior)
      sumsdAlg_MQ(mstr,ior) = sumsdAlg_MQ(mstr,ior) + sedAlg_MQ(mstr,ior)
      if (hBedGS(mstr,ior)>=0.0)iBedGs = 2
      if (ilbuhn == 1)iBedGs = 1
      do iBedG = 1,iBedGs
         if (iBedG == 2) then
            sumPOCsedz = sumPOCsed(mstr,ior)
            sumPONsedz = sumPONsed(mstr,ior)
            sumPOPsedz = sumPOPsed(mstr,ior)
            sumPOCsed(mstr,ior) = 0.0
            sumPONsed(mstr,ior) = 0.0
            sumPOPsed(mstr,ior) = 0.0
         endif
         O20 = vo2(ior)
         if (nkzs(ior) > 1)O20 = vo2z(nkzs(ior),ior)
         if (O20 < 0.1)O20 = 0.1
         NO30 = vNO3(ior)
         ! if(nkzs(ior)>1)NO30 = vNO3z(nkzs(ior),ior)
         if (NO30 < 0.01)NO30 = 0.01
         NH40 = vNH4(ior)
         ! if(nkzs(ior)>1)NH40 = vNH4z(nkzs(ior),ior)
         if (NH40 < 0.01)NH40 = 0.01
         PO40 = gelP(ior)
         ! if(nkzs(ior)>1)PO40 = gelPz(nkzs(ior),ior)
         if (PO40 < 0.001)PO40 = 0.001
         Si0 = si(ior)
         if (Si0 < 0.01)Si0 = 0.01
         DOC0(1) = hCD(mstr,1,ior)
         DOC0(2) = hCD(mstr,2,ior)
         N20 = hFluN3(mstr,ior)
         
         fPOM = hSedOM(mstr,ior) ! Anteil organisches Material im Sediment
         if (iBedG == 2)fPOM = 0.001
         w20 = hw2(mstr,ior) ! Burial-Geschwindigkeit
         Temps = Tempw(ior)
         CH40 = 0.2
         g = 9.81 ! in Modul??
         !
         ! Porosität (s. DiTorro (2001) Seite 4)
         Poro1 = 0.98*fPOM/(fPOM+0.011)
         if (Poro1 < 0.29)Poro1 = 0.29
         Poro2 = Poro1 - 0.05
         !
         Dp = 0.00012
         Dichta = 2.6
         Dichto = 1.2
         fPOC0 = 0.378 ! Kohlenstoffanteil im organischen Material
         fC_O2 = 2.76
         fC_N = 10.1
         fC_P = 117.
         fdnC_N = 1.31
         fO_Nit = 4.57
         fSi_C = 0.05            ! 0.12 Chase
         POC1_R = 0.1 !POC1_R in mg/g
         !
         Dichte = dichta*(1.-fPOM)+fPOM*Dichto
         !
         !.Feststoffkonzentration
         ! m = Ro*(1-Poro)  mg*cm-3
         m1 = Dichte*(1.-Poro1)
         m2 = Dichte*(1.-Poro2)
         JPOC = dichte*1000.*1000.*fPOM*fPOC0*w20
         if (kontroll)print*,'sedflux: JPOC,dichte,fPOM,fPOC0,w20,ior = ',JPOC,dichte,fPOM,fPOC0,w20,ior
         ! w20 = (((sumsdSS_MQ(mstr,ior)+sumsdAlg_MQ(mstr,ior))/anzZschritt)*tiefe(ior)*(1./tflie))/(dichte*1000.*1000.)
         ! if(w20<5.e-6)w20 = 5.e-6
         JPSi = JPOC * fSi_C
         JPON = JPOC*(1./fC_N)
         JPOP = JPOC*(1./fC_P)
         ThtaD = 1.08
         ThtaN3 = 1.08
         ThtaN4 = 1.123
         ThtaC4 = 1.08
         ThtaOM = 1.14
         ThtaDp = 1.117
         ThtaSi = 1.1
         !
         if (fPOC1e < 0.0 .and. fPOC2e < 0.0) then
            fPOC(1) = 0.65
            fPOC(2) = 0.15
            fPOC(3) = 0.2
         else if (fPOC1e > 0.0 .and. fPOC2e > 0.0) then
            fPOC(1) = fPOC1e
            fPOC(2) = fPOC2e
            fPOC(3) = 1. - fPOC1e - fPOC2e
         endif
         KappN3 = kapN3e ! Denitrifikationsgeschwindigkeit m/d
         KNH4 = KNH4e    ! Nitrifikationsgeschwindigkeit m/d
         kappC = 0.57 !2.8    ! Methanoxidationsgeschwindigkeit 0.57 m/d
         !Kohlenstoffmineralisationsgeschwindigkeit
         KdiaPC(1) = 0.035
         KdiaPC(2) = 0.0018
         KdiaPC(3) = 0.0
         KM_NH4 = 0.728 ! Halbsättigungskonstante für N bei Nitrifikation
         KMO_N4 = 0.37  ! Halbsättigungskonstante für O2 bei Nitrifikation  !0.37
         KM_NO3 = 0.4   ! Halbsättigungskonstante für N bei Denitrifikation und P_Freisetzung (Boudreau)
         KMO_NO3 = 0.26 ! Halbsättigungskonstante für O2 bei Denitrifikation und P_Freisetzung (Boudreau)
         kSi = 0.03    ! Lösungsrate in 1/d für partikuläres Silikat, optimiert aus Daten der Saar
         km_PSi = 50.  ! Halbsättigungskonstante in gSi/m3 für Mineralisation von partik. Silikat, DiToro
         vbc = obsb(ior)/ocsb(ior)
         xK1DOC(1) = 0.18
         xK1DOC(2) = 0.3
         alphals = 0.848*vcb**0.347
         Dim_KN = 1.3
         if (KdNh3e < -1.)KdNh3e = 0.0
         if (KdNh3e < 0.0) then
            KdNH31 = Dim_KN*Poro1/((1.-Poro1)*Dichte)
            KdNH32 = Dim_KN*Poro2/((1.-Poro2)*Dichte)
         else
            KdNH31 = KdNh3e !Partitionskoeffizient für Ammonium
            KdNH32 = KdNh31
         endif
         KdPO42 = 350 !Partitionskoeffizient für P in der anaeroben Schicht
         dKdP41 = 330.
         O2cPO4 = 2.
         KdSi1 = 14.! Partitionskoeffizient für Silikat
         
         H2 = 0.1    ! Dicke der anaeroben Schicht
         H1 = 0.01  ! Anfangsdicke der aeroben Schicht
         !
         !  Berechnung der partikulären und gelösten Fraktion für Ammonium in Schicht1 und 2
         !
         fd1 = 1./(1.+m1*KdNH31)
         !if(ISNAN(fd1))print*,"sedflux m1*KdNH31",m1,KdNH31
         fp1 = 1.-fd1
         fd2 = 1./(1.+m2*KdNH32)
         fp2 = 1.-fd2
         !
         NO3(1) = NO30
         NO3(2) = NO30
         NH4(1) = NH40
         NH4(2) = NH40
         
         ! Diagenese
         fTPOM = ThtaOM**(Temps-20.)
         w12 = Dp
         do it = 1,10 ! Iterationsschleife Anfang
            !do it = 1,100 ! Iterationsschleife Anfang Test feinere Iteration !!wy
            if (kontroll) then
               print*,'sedflux (Iterationsschleife Anfang): JPOC_neu,sumPOCsed,mstr,ior = '  &
                     ,JPOC_neu(mstr,ior),sumPOCsed(mstr,ior),mstr,ior
               print*,'DOC1(),DOC2()',DOC1(:),DOC2(:)
               print*,'POC1(),POC2()',POC1(:),POC2(:)
            endif
            
            ! Berechnung des Diffusionskoeffizienten in der aeroben und anaeroben Schicht, DiffK1, DIFFK2
            call Sed_DiffK(tiefe,vmitt,rau,H1,H2,hdKorn,DiffK1,DiffK2,DifKP1,DifKP2,poro1,poro2,vvert,vvert1,vvert2      &
                           ,mstr,ior,itags,monats,uhrz, kontroll ,jjj )
            if (kontroll)print*,'sedflux: it,ior,iBedG = ',it,ior,iBedG
            if ((kontroll) .and. (it == 1).and.(ior == 1)) then
               print*,'sedflux: nach Sed_DiffK '
               print*,'tiefe,vmitt,rau,H1,H2,hdKorn = ',tiefe(ior),vmitt(ior),rau(ior),H1,H2,hdKorn(mstr,ior)
               print*,'DiffK1,DiffK2,DifKP1,DifKP2 = ',DiffK1,DiffK2,DifKP1,DifKP2
               print*,'poro1,poro2,vvert,vvert1,vvert2 = ',poro1,poro2,vvert,vvert1,vvert2
            end if
            if (it == 1)s = Dp/H1
            KL12 = DiffK2*ThtaD**(Temps-20.)/(H2/2.) !! nur diese diffusivität geht ein.
            do i = 1,3 ! Fraktionsschleife Anfang
               
               ! -- POC ---
               ! Schicht 1
               a11 = -KdiaPC(i)*fTPOM*H1-w20-w12
               a12 = w12
               b1 = -fPOC(i)*JPOC
               ! Schicht 2
               a21 = w20+w12
               a22 = -KdiaPC(i)*fTPOM*H2-w20-w12
               b2 = -fPOC(i)*JPOC
               
               call lin_sys(a11,a12,a21,a22,b1,b2,POC1(i),POC2(i))
               ! --- DOC ---
               if (i < 3) then
                  !  Schicht 1
                  a11 = -xK1DOC(i)*fTPOM*H1-KL12-s
                  a12 = KL12
                  b1 = -s*DOC0(i)
                  !  Schicht 2
                  a22 = -KdiaPC(i)*fTPOM*H2-KL12
                  a22 = -KL12
                  b2 = 0.0
                  call lin_sys(a11,a12,a21,a22,b1,b2,DOC1(i),DOC2(i))
               endif
               
               ! --- PON ---
               ! Schicht 1
               a11 = -KdiaPC(i)*fTPOM*H1-w20-w12
               a12 = w12
               b1 = -fPOC(i)*JPON
               !  Schicht 2
               a21 = w20+w12
               a22 = -KdiaPC(i)*fTPOM*H2-w20-w12
               b2 = -fPOC(i)*JPON
               call lin_sys(a11,a12,a21,a22,b1,b2,PON1(i),PON2(i))
               
               ! --- POP ---
               ! Schicht 1
               a11 = -KdiaPC(i)*fTPOM*H1-w20-w12
               a12 = w12
               b1 = -fPOC(i)*JPOP
               ! Schicht 2
               a21 = w20+w12
               a22 = -KdiaPC(i)*fTPOM*H2-w20-w12
               b2 = -fPOC(i)*JPOP
               
               call lin_sys(a11,a12,a21,a22,b1,b2,POP1(i),POP2(i))
               
            enddo ! Fraktionsschleife Ende
            
            Jc1 = 0.0
            JN1 = 0.0
            JP1 = 0.0
            do i = 1,2 ! Summenbildung in der aeroben Schicht
               if (i == 1) then
                  H11 = H1 - sumdw2(mstr,ior) * xalphals(mstr,ior)
                  hcalphals = xalphals(mstr,ior)
               else
                  H11 = H1 - sumdw2(mstr,ior) * (1.-xalphals(mstr,ior))
                  hcalphals = (1.-xalphals(mstr,ior))
               endif
               if (H11 < 0.0) then
                  H11 = 0.0
                  fakH = 1.
               else
                  fakH = sumdw2(mstr,ior)/H1
               endif
               !! TODO Summation falsch??? der JPOC_neu und der  POCvert1 zweimal hinzuaddiert???
               Jc1 = Jc1                                                       &
                   + KdiaPC(i) * fTPOM * POC1(i) * H11                         &
                   + xK1DOC(i) * fTPOM * JPOC_neu(mstr,ior) * hcalphals * fakH & 
                   + xK1DOC(i) * fTPOM * DOC1(i)*H1                            &
                   + xK1DOC(i) * fTPOM * POCvert1(mstr,ior) * hcalphals
                   
               JN1 = JN1                                                       &
                   + KdiaPC(i) * fTPOM * PON1(i) * H11                         &
                   + xK1DOC(i) * fTPOM * JPON_neu(mstr,ior) * hcalphals * fakH &
                   + xK1DOC(i) * fTPOM * DOC1(i) * nl0(ior) * H1               &
                   + xK1DOC(i) * fTPOM * POCvert1(mstr,ior) * nl0(ior) * hcalphals
               
               JP1 = JP1                                                       &
                   + KdiaPC(i) * fTPOM * POP1(i) * H11                         &
                   + xK1DOC(i) * fTPOM * JPOP_neu(mstr,ior) * hcalphals * fakH &
                   + xK1DOC(i) * fTPOM * DOC1(i) * pl0(ior) * H1               &
                   + xK1DOC(i) * fTPOM * POCvert1(mstr,ior) * pl0(ior) * hcalphals
            enddo
            Jc = 0.0
            JN = 0.0
            JP = 0.0
            
            do i = 1,2 !Summenbildung in der anaeroben Schicht
               !! Summation falsch??? POCvert2 zweimal hinzuaddiert???
               Jc = Jc                                      &
                  + KdiaPC(i) * fTPOM * POC2(i) * H2        &
                  + xK1DOC(i) * fTPOM * DOC2(i) * H2        &
                  + xK1DOC(2) * fTPOM * POCvert2(mstr,ior)
                  
               JN = JN                                                  &
                  + KdiaPC(i) * fTPOM * PON2(i) * H2                    &
                  + xK1DOC(i) * fTPOM * DOC2(i) * nl0(ior) * H2         &
                  + xK1DOC(2) * fTPOM * POCvert2(mstr,ior) * nl0(ior)   
               
               JP = JP                                                  &
                  + KdiaPC(i) * fTPOM * POP2(i) * H2                    &
                  + xK1DOC(i) * fTPOM * DOC2(i) * pl0(ior) * H2         &
                  + xK1DOC(2) * fTPOM * POCvert2(mstr,ior) * pl0(ior)
            enddo
            Jc  = Jc  * fC_O2
            Jc1 = Jc1 * fC_O2
            if (it == 1) then
               SOD = Jc1+Jc+4.57*JN1
            else
            endif
            CH4sat = 100.*(1.+(Tiefe(ior)/10.))*1.024**(20.-Temps)
            POC_1 = POC2(1)/(1000.*m2)
            w12 = (Dp/H2)*ThtaDp**(Temps-20.)*(POC_1/POC1_R) !*(O20/(O20+0.5))
            
            s = SOD/O20
            H1 = 0.5
            if (s /= 0.0)H1 = DiffK1*ThtaD**(Temps-20.)/s ! Neuberechnung der Dicke der aeroben Schicht
            if (H1 <= 0.0) H1 = 0.01
            if (H1 > 0.5)     H1 = 0.5
            ! H1 = KL12*H2/s
            
            ! --- Ammonium ---
            N4toN3 = 0.0
            if ((s /= 0.0) .and. (O20 /= 0.0)) N4toN3 = KNH4**2*ThtaN4**(Temps - 20)/s*(O20/2.)/(KMO_N4 + (O20/2.))
            !*KM_NH4/(KM_NH4 + NH4(1))
            ! N4toN3 = KNH4*ThtaN4**(Temps - 20)*(O20/2.)/(KMO_N4 + (O20/2.))
            
            ! Schicht 1
            a11 = -KL12*fd1-w12*fp1-N4toN3*fd1-s*fd1-w20
            a12 = KL12*fd2+w12*fp2
            b1 = -JN1-s*NH40
            
            ! Schicht 2
            a21 = KL12*fd1+w12*fp1+w20
            a22 = -KL12*fd2-w12*fp2-w20
            b2 = -JN
            call lin_sys(a11,a12,a21,a22,b1,b2,NH4T(1),NH4T(2))
            
            NH4(1) = NH4T(1)*fd1
            NH4(2) = NH4T(2)*fd2
            fd1_aus = fd1
            NSOD = fO_Nit*N4toN3*NH4(1)
            BENMX1 = yNmx1e
            BENKS1 = Stks1e
            BETTF = BENMX1*(nh4(1)/(VNH4(1)+BENKS1))*(O20/2.)/(KMO_N4 + (O20/2.))*ThtaN4**(Temps - 20)
            
            ! --- Denitrifikation/Nitrat ---
            hconD1 = (KMO_NO3/(KMO_NO3+(O20/2.)))
            !*NO3(1)/(NO3(1)+KM_NO3)
            hconD2 = NO3(2)/(NO3(2)+KM_NO3)
            ! Denit(1) = sqrt(KappN3*ThtaN3**(Temps-20.)*s)
            Denit(1) = 0.0
            if ((s*hconD1) /= 0.0)Denit(1) = KappN3**2*ThtaN3**(Temps-20.)/s*hconD1 ! nach DiToro
            Denit(2) = kappN3*ThtaN3**(Temps-20.)
            
            ! Schicht 1
            a11 = -KL12-Denit(1)-s
            a12 = KL12
            b1 = -s*NO30-N4toN3*NH4(1)
            
            ! Schicht 2
            a21 = KL12
            a22 = -KL12-Denit(2)
            b2 = 0.0
            
            call lin_sys(a11,a12,a21,a22,b1,b2,NO3(1),NO3(2))
            JDenit(1) = Denit(1)*NO3(1)
            JDenit(2) = Denit(2)*NO3(2)
            JDeniG = JDenit(1)+JDenit(2)
            
            JC4N31 = JC1*hconD1*NO3(1)/(NO3(1)+KM_NO3)
            JC4N32 = JDenit(2)*fdnC_N*fC_O2
            
            ! --- N2 Bildung ---
            ! Schicht 1
            a11 = -KL12-s
            a12 = KL12
            b1 = -s*N20-Denit(1)*1.1*NO3(1)
            
            ! Schicht 2
            a21 = KL12
            a22 = -KL12
            b2 = -Denit(2)*1.1*NO3(2)
            
            call lin_sys(a11,a12,a21,a22,b1,b2,N2(1),N2(2))
            
            ! --- SOD ---
            ! Methanconsumption durch Denitrifikation in Layer 1 und Layer 2
            Jcneu = Jc-JC4N32
            if (Jcneu < 0.0)Jcneu = 0.0
            CSODmx = sqrt(2.*KL12*CH4sat*Jcneu)
            if (Jcneu < CSODmx)CSODmx = Jcneu
            hcon1 = 0.0
            hcon2 = 0.0
            if (SOD /= 0.0) then
               hcon1 = kappC*ThtaC4**(Temps-20.)*O20/SOD
               hcon2 = O20/SOD
            end if
            
            if (hcon1 > 400.) then
               CSOD = CSODmx
            else
               sech = 2./(exp(hcon1)+exp(-hcon1))
               CSOD = CSODmx*(1.-sech)
            endif
            CSOD1 = Jc1 - JC4N31
            SODinit = SOD
            SOD = (SOD+CSOD+CSOD1+NSOD)/2.
            if (kontroll) then
               print*,'sedflux (Iterationsschleife Ende): it,ior,iBedG = ',it,ior,iBedG
               print*,'SOD,SODinit,CSOD,CSOD1,NSOD',SOD,SODinit,CSOD,CSOD1,NSOD
               print*,'H1,H2,hdKorn,tiefe,vmitt,rau = ',H1,H2,hdKorn(mstr,ior),tiefe(ior),vmitt(ior),rau(ior)
               print*,'DiffK1,DiffK2,DifKP1,DifKP2 = ',DiffK1,DiffK2,DifKP1,DifKP2
               print*,'poro1,poro2,vvert,vvert1,vvert2 = ',poro1,poro2,vvert,vvert1,vvert2
               print*,'Jc1,Jcneu,Jc = ',Jc1,Jcneu,Jc
               print*,'DOC1(),DOC2()',DOC1(:),DOC2(:)
               print*,'POC1(),POC2()',POC1(:),POC2(:)
            end if
            if (abs((SOD-SODinit)/SOD)*100. < 1.)exit
            !if(abs((SOD-SODinit)/SOD)*100.< 0.1)exit
            
            ! --- Methankonzentration in der aeroben Schicht ---
            C4toC2 = (kappC**2*ThtaC4**((Temps-20.)/2.))/s
            CH41 = (CSODmx+s*CH40)/(C4toC2+s)
            
         enddo ! Iterationsschleife Ende
         
         ! --- orthoPhosphat und Silikat ---
         ! PHOSPHOR
         KL01P = (DifKP1/H1)*ThtaD**(Temps-20.)
         KL12P = DifKP2*ThtaD**(Temps-20.)/(H2/2.)
         if (SorpCape < 0.0) then
            SorpCap1 = 4.34*fPOM**1.05
         else
            SorpCap1 = SorpCape
         endif
         LangCon1 = Klange
         PO41Por = 0.01
         PO42Por = 0.01
         
         ! SILIKAT
         Sipor(1) = 0.001
         Sipor(2) = 0.001
         Sisaett = 0.4667*67.8+0.4667*1.48*Temps
         
         do it = 1,30
            !  PHOSPHOR
            adsorbP = SorpCap1*LangCon1*PO41Por/(1.+LangCon1*PO41Por)
            adsorbP = adsorbP/1000.
            PO41Por = PO41Por/1000./1000.
            KdPO41 = adsorbP/PO41Por
            aus_kd = KdPO41
            hconO = 0.0
            hconN = 0.0
            hconO = 1.             ! da in Schicht 2 kein Sauerstoff
            hconN = KM_NO3/(KM_NO3+NO3(2))
            adsorbP = SorpCap1*LangCon1*PO42Por/(1.+LangCon1*PO42Por)
            adsorbP = adsorbP*(1.-(hconO*hconN))
            adsorbP = adsorbP/1000.
            PO42Por = PO42Por/1000./1000.
            KdPO42 = adsorbP/PO42Por
            fd1 = 1./(1.+m1*KdPO41)
            fp1 = 1.-fd1
            fd2 = 1./(1.+m2*KdPO42)
            fp2 = 1.-fd2
            !
            !      Schicht 1
            a11 = -KL12*fd1-w12*fp1-s*fd1-w20
            a12 = KL12*fd2+w12*fp2
            b1 = -s*PO40-JP1
            !
            !      Schicht 2
            a21 = KL12*fd1+w12*fp1+w20
            a22 = -KL12*fd2-w12*fp2-w20
            b2 = -Jp
            !
            PO42Tinit = PO4T(2)
            call lin_sys(a11,a12,a21,a22,b1,b2,PO4T(1),PO4T(2))
            !
            PO4(1) = PO4T(1)*fd1
            PO4(2) = PO4T(2)*fd2
            if (abs(PO4T(2)-PO42Tinit) < 0.05)exit
            PO42Por = (PO42Por*1000. + PO4(2))/2.
            PO41Por = (PO41Por*1000. + PO4(1))/2.
         enddo  ! Ende Phosphor
         
         ! --- Phosphor-Desorption ---
         PO40z = PO40
         PO40 = 0.000
         ! Schicht 1
         a11 = -KL12*fd1-w12*fp1-s*fd1-w20
         a12 = KL12*fd2+w12*fp2
         b1 = -s*PO40-JP1
         
         ! Schicht 2
         a21 = KL12*fd1+w12*fp1+w20
         a22 = -KL12*fd2-w12*fp2-w20
         b2 = -Jp
         
         call lin_sys(a11,a12,a21,a22,b1,b2,PO4T(1),PO4T(2))
         
         PO4z1(1) = PO4T(1)*fd1
         PO4z1(2) = PO4T(2)*fd2
         hconO = 0.0
         hconN = 0.0
         hconO = KMO_NO3/(KMO_NO3+(O20/2.))
         hconN = KM_NO3/(KM_NO3+NO3(1))
         delta_adP = hconO*hconN
         adsorbP = SorpCap1*LangCon1*PO41Por/(1.+LangCon1*PO41Por)
         adsorbP = adsorbP*(1.-delta_adP)
         adsorbP = adsorbP
         PO41Por = PO41Por/1000
         KdPO41 = adsorbP/PO41Por
         fd1 = 1./(1.+m1*KdPO41)
         fp1 = 1.-fd1
         fd2 = 1./(1.+m2*KdPO42)
         fp2 = 1.-fd2
         PO4z2(1) = PO4T(1)*fd1
         PO4z2(2) = PO4T(2)*fd2
         
         PO4(1) = PO4(1)+(PO4z2(1)-PO4z1(1))
         PO40 = PO40z
         !Ende Phosphor
         
         
         
         do it = 1, 30
            ! Silikat
            if (it == 1) then
               Si1 = 0.0
               Si2 = 0.0
               PSi1 = 1000.
               PSi2 = 1000.
            else
               Si1 = (Si1 + Sipor(1))/2.
               Si2 = (Si2 + Sipor(2))/2.
               PSi1 = (PSi1 + PSi(1))/2.
               PSi2 = (PSi2 + PSi(2))/2.
            endif
            kappaSi1 = kSi*ThtaSi**(Temps-20.)*(1.-(min(Sisaett,Si1))/Sisaett)*(PSi1/(PSi1+km_PSi))
            kappaSi2 = kSi*ThtaSi**(Temps-20.)*(1.-(min(Sisaett,Si2))/Sisaett)*(PSi2/(PSi2+km_PSi))
            SSi(1) = kSi*H1*ThtaSi**(Temps-20.)*(PSi1/(PSi1+km_PSi))*(SiSaett-min(Sisaett,Si1))
            SSi(2) = kSi*H2*ThtaSi**(Temps-20.)*(PSi2/(PSi2+km_PSi))*(SiSaett-min(Sisaett,Si2))
            
            kdSi2 = kdSi1 ! Partitionskoeffizient in der anaeroben Schicht
            hconO = 1.
            hconN = KM_NO3/(KM_NO3+NO3(2))
            fdSi1 = 1./(1.+m1*KdSi1)
            fpSi1 = 1.-fdSi1
            fdSi2 = 1./(1.+m2*KdSi2)
            fpSi2 = 1.-fdSi2
            a11 = -kappaSi1*H1-w12-w20
            a12 = w12
            b1 = -JPSi
            a21 = w20+w12
            a22 = -kappaSi2*H2-w20-w12
            b2 = -JPSi
            PSi2init = PSi(2)
            call lin_sys(a11,a12,a21,a22,b1,b2,PSi(1),PSi(2))
            PSi(1) = PSi(1) + hSised(mstr,ior)
            a11 = -KL12*fdSi1-w12*fpSi1-s*fdSi1-w20
            a12 = KL12*fdSi2+w12*fpSi2
            b1 = -SSi(1)-s*Si0
            a21 = KL12*fdSi1+w12*fpSi1+w20
            a22 = -KL12*fdSi2-w12*fpSi2-w20
            b2 = -SSi(2)
            call lin_sys(a11,a12,a21,a22,b1,b2,SiT(1),SiT(2))
            Sipor(1) = min(Sisaett,SiT(1)*fdSi1)
            Sipor(2) = min(Sisaett,SiT(2)*fdSi2)
            if (abs(PSi(2)-PSi2init) < 0.01)exit
         enddo  ! Silikat
         
         ! --------------------------------------------------------------------
         ! Stoffflüsse in den Wasserkörper
         ! --------------------------------------------------------------------
         hJNO3(mstr,ior) = s * (NO3(1) - NO30) ! + Flux vom Sediment in den Wasserkörper
         hJNH4(mstr,ior) = s * (NH4(1) - NH40)
         hJPO4(mstr,ior) = s * (PO4(1) - PO40)
         JDOC1(ior) = s * (DOC1(1) - DOC0(1))
         JDOC2(ior) = s * (DOC1(2) - DOC0(2))
         if (kontroll)print*,'sedfluc: ,DOC1(),DOC2(),POC1(),POC2(),ior = ',DOC1(:),DOC2(:),POC1(:),POC2(:),ior
         hJSi(mstr,ior) = s *(Sipor(1) - Si0)
         hJO2(mstr,ior) = SOD
         JCH4aq(ior) = s * (CH41 - CH40)
         hJN2(mstr,ior) = s * (N2(1) - N20)
         
         ! --------------------------------------------------------------------
         ! Berechnung der Menge an Algen die aktiv ins Sediment transportiert werden
         ! --------------------------------------------------------------------
         if (vvert > 0.0 .and. ilbuhn == 0) then
            dalgvert = (aki(ior)*Caki + agr(ior)*Cagr + abl(ior)*Cabl)*vvert1
            dPOCvert = POCvert1(mstr,ior)*KdiaPC(1)*fTPOM*tflie
            POCvert1(mstr,ior) = max(0.0,POCvert1(mstr,ior) + dalgvert - dPOCvert)
            POCvert1(mstr,ior) = 0.0
            dPOCvert = POCvert2(mstr,ior)*KdiaPC(1)*fTPOM*tflie
            POCvert2(mstr,ior) = max(0.0,POCvert2(mstr,ior) - dPOCvert + dalgvert * vvert2/vvert1)
            POCvert2(mstr,ior) = 0.0 * vvert2/vvert1
            aki(ior) = aki(ior) - aki(ior)*vvert1*tflie/Tiefe(ior)
            agr(ior) = agr(ior) - agr(ior)*vvert1*tflie/Tiefe(ior)
            abl(ior) = abl(ior) - abl(ior)*vvert1*tflie/Tiefe(ior)
            if (kontroll)print*,'sedflux: Chlaki(ior)_vorher,ior = ',Chlaki(ior),ior
            Chlaki(ior) = Chlaki(ior) - Chlaki(ior)*vvert1*tflie/Tiefe(ior)
            if (kontroll)print*,'sedflux: Chlaki(ior)_nachher,ior,vvert1,tflie,Tiefe(ior) = ',Chlaki(ior),ior,vvert1,tflie,Tiefe(ior)
            Chlagr(ior) = Chlagr(ior) - Chlagr(ior)*vvert1*tflie/Tiefe(ior)
            Chlabl(ior) = Chlabl(ior) - Chlabl(ior)*vvert1*tflie/Tiefe(ior)
         endif
        
         if (iBedG == 1) then
            hJNO3z = hJNO3(mstr,ior)
            hJNH4z = hJNH4(mstr,ior)
            hJPO4z = hJPO4(mstr,ior)
            hJO2z = hJO2(mstr,ior)
            hJCH4aqz = JCH4aq(ior)
            hJSiz = hJSi(mstr,ior)
         else
            hJNO3(mstr,ior) = hJNO3(mstr,ior)*(1.-hBedGS(mstr,ior)) + hJNO3z*hBedGS(mstr,ior)
            hJNH4(mstr,ior) = hJNH4(mstr,ior)*(1.-hBedGS(mstr,ior)) + hJNH4z*hBedGS(mstr,ior)
            hJPO4(mstr,ior) = hJPO4(mstr,ior)*(1.-hBedGS(mstr,ior)) + hJPO4z*hBedGS(mstr,ior)
            hJO2(mstr,ior) = hJO2(mstr,ior)*(1.-hBedGS(mstr,ior)) + hJO2z*hBedGS(mstr,ior)
            JCH4aq(ior) = JCH4aq(ior)*(1.-hBedGS(mstr,ior)) + hJCH4aqz*hBedGS(mstr,ior)
            hJSi(mstr,ior) = hJSi(mstr,ior)*(1.-hBedGS(mstr,ior)) + hJSiz*hBedGS(mstr,ior)
            exit
         endif
         if (ilang == 0) then
            sumPOCsed(mstr,ior) = 0.0
            sumPONsed(mstr,ior) = 0.0
            sumPOPsed(mstr,ior) = 0.0
            sumdw2(mstr,ior) = 0.0
            xalphals(mstr,ior) = 0.0
         else
            
            ! Veränderung des leichtabbaubaren Kohlenstoffsgehalts in der
            ! aeroben Schicht durch Sedimentation im Zeitschritt<tflie>
            POCsed = orgCsd_abb(mstr,ior)+Sedalk(ior)*Caki+sedalg(ior)*Cagr+sedalb(ior)*Cabl
            PONsed = orgCsd_abb(mstr,ior)*nl0(ior)+Sedalk(ior)*Q_NK(ior)+sedalg(ior)*Q_NG(ior)+sedalb(ior)*Q_NB(ior)
            POPsed = orgCsd_abb(mstr,ior)*pl0(ior)+Sedalk(ior)*Q_PK(ior)+sedalg(ior)*Q_PG(ior)+sedalb(ior)*Q_PB(ior)
            POCsed1 = POCsed*tiefe(ior) * alphals                   ! PODsed1: leichtabbaubarer Anteil
            POCsed2 = POCsed*tiefe(ior) * (1.-alphals)              ! PODsed2: schwerabbarer Anteil
            PONsed1 = PONsed*tiefe(ior) * alphals
            PONsed2 = PONsed*tiefe(ior) * (1.-alphals)
            POPsed1 = POPsed*tiefe(ior) * alphals
            POPsed2 = POPsed*tiefe(ior) * (1.-alphals)
            dsumdw21 = sumdw2(mstr,ior)*xalphals(mstr,ior)*xK1DOC(1)*tflie
            dsumdw22 = sumdw2(mstr,ior)*(1.-xalphals(mstr,ior))*xK1DOC(2)*tflie
            sumdw21 = sumdw2(mstr,ior)*xalphals(mstr,ior) - dsumdw21 + POCsed1/(dichto*1000.*1000.)
            sumdw22 = sumdw2(mstr,ior)*(1.-xalphals(mstr,ior)) - dsumdw22 + POCsed2/(dichto*1000.*1000.)
            if (sumw21 > 0.0) then
               xalphals(mstr,ior) = sumdw21/(sumdw21+sumdw22)
            endif
            sumdw2(mstr,ior) = sumdw21+sumdw22
            ! fehler?? POCsed = (POCsed1+POCsed1)*(1./tflie)
            POCsed = (POCsed1+POCsed2)*(1./tflie)
            PONsed = (PONsed1+PONsed2)*(1./tflie)
            POPsed = (POPsed1+POPsed2)*(1./tflie)
            
            sumPOCsed(mstr,ior) = sumPOCsed(mstr,ior) + POCsed
            JPOC_neu(mstr,ior) = sumPOCsed(mstr,ior)/(anzZschritt-1)
            sumPONsed(mstr,ior) = sumPONsed(mstr,ior) + PONsed
            JPON_neu(mstr,ior) = sumPONsed(mstr,ior)/(anzZschritt-1)
            
            sumPOPsed(mstr,ior) = sumPOPsed(mstr,ior) + POPsed
            JPOP_neu(mstr,ior) = sumPOPsed(mstr,ior)/(anzZschritt-1)
         endif
         if (kontroll)print*,'sedflux: ,JPOC_neu,POCsed,sumPOCsed,iBedG,mstr,ior = ' &
             ,JPOC_neu(mstr,ior),POCsed,sumPOCsed(mstr,ior),iBedG,mstr,ior
         if (kontroll)print*,'sedflux: orgCsd_abb,Sedalk,alphals = '   &
             ,orgCsd_abb(mstr,ior),Sedalk(ior),alphals
      enddo !Ende Schleife für Berücksichtigung des Bedeckungsgrads #do iBedG = 1,iBedGs
      
      if (iBedGs == 2) then
         sumPOCsed(mstr,ior) = sumPOCsedz
         sumPONsed(mstr,ior) = sumPONsedz
         sumPOPsed(mstr,ior) = sumPOPsedz
      endif
      if (ilbuhn == 1) then
         bsumPOCsed(mstr,ior) = sumPOCsed(mstr,ior)
         bsumPONsed(mstr,ior) = sumPONsed(mstr,ior)
         bsumPOPsed(mstr,ior) = sumPOPsed(mstr,ior)
         bsumsdSS_MQ(mstr,ior) = sumsdSS_MQ(mstr,ior)
         bsumsdAlg_MQ(mstr,ior) = sumsdAlg_MQ(mstr,ior)
         bsumdw2(mstr,ior) = sumdw2(mstr,ior)
         bxalphals(mstr,ior) = xalphals(mstr,ior)
         bJPOC_neu(mstr,ior) = JPOC_neu(mstr,ior)
         bJPON_neu(mstr,ior) = JPON_neu(mstr,ior)
         bJPOP_neu(mstr,ior) = JPOP_neu(mstr,ior)
         sumPOCsed(mstr,ior) = zwsumPOCsed
         sumPONsed(mstr,ior) = zwsumPONsed
         sumPOPsed(mstr,ior) = zwsumPOPsed
         sumsdSS_MQ(mstr,ior) = zwsumsdSS_MQ
         sumsdAlg_MQ(mstr,ior) = zwsumsdAlg_MQ
         sumdw2(mstr,ior) = zwsumdw2
         xalphals(mstr,ior) = zwxalphals
         JPOC_neu(mstr,ior) = zwJPOC_neu
         JPON_neu(mstr,ior) = zwJPON_neu
         JPOP_neu(mstr,ior) = zwJPOP_neu
      endif
      
      ! Reduktion des neu sedimentierten partikulären Silikats
      hSised(mstr,ior) = hSised(mstr,ior) - ((SSi(1)*hSised(mstr,ior))/(H1*PSi(1)))*tflie
      if (kontroll)print*,'sedflux: JPOC_neu,sumPOCsed,mstr,ior = '  &
          ,JPOC_neu(mstr,ior),sumPOCsed(mstr,ior),mstr,ior
   enddo !Ende Knotenschleife
   
   if (izaehl_Str == azStrs)izaehl_Str = 0
   
   return
end subroutine sedflux

