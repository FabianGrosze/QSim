! --------------------------------------------------------------------------- !
!  qsim - programm zur simulation der wasserqualität                          !
!                                                                             !
!  copyright (c) 2022                                                         !
!  bundesanstalt für gewässerkunde                                            !
!  koblenz (deutschland)                                                      !
!  http://www.bafg.de                                                         !
!                                                                             !
!  dieses programm ist freie software. sie können es unter den bedingungen    !
!  der gnu general public license, version 3, wie von der free software       !
!  foundation veröffentlicht, weitergeben und/oder modifizieren.              !
!                                                                             !
!  die veröffentlichung dieses programms erfolgt in der hoffnung, dass es     !
!  ihnen von nutzen sein wird, aber ohne irgendeine garantie, sogar ohne die  !
!  implizite garantie der makrtreife oder der verwendbarkeit für einen        !
!  bestimmten zweck.                                                          !
!                                                                             !
!  details finden sie in der gnu general public license.                      !
!  sie sollten ein exemplar der gnu general public license zusammen mit       !
!  diesem programm erhalten haben.                                            !
!  falls nicht, siehe http://www.gnu.org/licenses/.                           !
!                                                                             !
!  programmiert von                                                           !
!  1979 bis 2018   volker kirchesch                                           !
!  seit 2011       jens wyrwa, wyrwa@bafg.de                                  !
! --------------------------------------------------------------------------- !

!> ermittlung der sofffluxe aus dem sediment
!!
!! ditoro 2001; qual2k
!!
!! @author: volker kirchesch
!! @date 12.11.2008
subroutine sedflux(tiefe, vmitt, rau, sedalg_mq, hsedom, hw2, hbedgs, hsedvvert,    &
                   hdkorn, vo2, vno3, vnh4, gelp, tempw, anze, mstr, hjno3,         &
                   hjnh4, hjpo4, hjo2, hjn2, sedalk, sedalg, sedalb, sedss_mq,      &
                   tflie, ilbuhn, kdnh3e, fpoc1e, fpoc2e, orgcsd_abb, hcd, jdoc1,   &
                   jdoc2, q_nk, q_pk, q_ng, q_pg, q_nb, q_pb, pl0, nl0, si, hsised, &
                   hjsi, aki, agr, abl, chlaki, chlagr, chlabl, hflun3, ilang,      &
                   iwied, ynmx1e, stks1e, obsb, ocsb,                               &
                   control, jjj)
   
   use module_alloc_dimensions
   use module_aparam
   implicit none
   
   integer                        :: mstr, kdsi2, kdsi1, i
   integer                        :: izaehl_str, iwied, it, ior
   integer                        :: ilbuhn, ilang, ibedg, ibedgs
   real                           :: temps, zwxalphals, zwsumsdss_mq, zwsumsdalg_mq, zwsumpopsed
   real                           :: zwsumponsed, zwsumpocsed, zwsumdw2, zwjpop_neu, zwjpon_neu
   real                           :: zwjpoc_neu, ynmx1e, w20, w12, vvert
   real                           :: vvert2, vvert1, vcb, vbc
   real                           :: thtasi, thtaom, thtan4, thtan3, thtad
   real                           :: thtadp, thtac4, tflie, sumw21, sumpopsedz
   real                           :: sumponsedz, sumpocsedz, sumdw22, sumdw21, stks1e
   real                           :: sorpcap1, sod, sodinit, sisaett
   real                           :: si2, si1, si0, sech, psi2
   real                           :: psi2init, psi1, poro2, poro1, popsed
   real                           :: popsed2, popsed1, ponsed, ponsed2, ponsed1
   real                           :: poc_1, pocsed, pocsed2, pocsed1, poc1_r
   real                           :: po42tinit, po42por, po41por, po40, po40z
   real                           :: o2cpo4, o20, hjsiz, hjpo4z, hjo2z
   real                           :: hjno3z, hjnh4z, hjch4aqz, hcono, hconn
   real                           :: hcond2, hcond1, hcon2, hcon1, hcalphals
   real                           :: h2, h1, h11, ftpom, adsorbp
   real                           :: fsi_c, fpsi2, fpsi1, fpom, fpoc2e
   real                           :: fpoc1e, fpoc0, fp2, fp1, fo_nit
   real                           :: fdsi2, fdsi1, fdnc_n, fd2, fd1_aus
   real                           :: fd1, fc_p, fc_o2, fc_n, fakh
   real                           :: dsumdw22, dsumdw21, dp, dpocvert, dkdp41
   real                           :: dim_kn, difkp2, difkp1, diffk2, diffk1
   real                           :: dichto, dichte, dichta, delta_adp, dalgvert
   real                           :: csod, csodmx, csod1, ch4sat, ch41
   real                           :: ch40, c4toc2
   real                           :: bettf, benmx1, benks1, aus_kd, alphals
   integer                        :: anze, anzzschritt
   real, dimension(1000)          :: tiefe,vmitt,rau,tempw,vo2,vnh4,vno3,gelp,si,jch4aq
   real, dimension(1000)          :: sedalk,sedalg,sedalb,q_nk,q_pk,q_ng,q_pg, aki, agr ,abl, chlaki, chlagr, chlabl
   real, dimension(1000)          :: q_nb,q_pb,pl0,nl0, jdoc1,jdoc2, obsb, ocsb
   real, dimension(azstrs,1000)   :: hsedom,hbedgs,hsedvvert,hw2,hjno3,hjnh4,hjpo4,hjo2,hdkorn, hsised , hjn2
   real, dimension(azstrs,1000)   :: orgcsd_abb, sedalg_mq
   real, dimension(azstrs,1000)   :: hjsi, hflun3
   real, dimension(azstrs,1000)   :: sedss_mq
   real, dimension(azstrs,2,1000) :: hcd
   real                           :: kl12,n4ton3,km_nh4,kmo_n4,kmo_no3,km_no3,ksi, jpsi
   real                           :: kappc, kdnh31,kdnh32,km_psi
   real                           :: kdpo42,kdpo41,jdenig,no30,nh40,nsod,langcon1,kdnh3e,kappasi1,kappasi2
   real                           :: jc,jn,jc1,jn1,jcneu,jpon,jpoc,n20
   real                           :: jpop,jc4n31,jc4n32,jp1,jp
   real, dimension(2)             :: denit ,jdenit , nh4, nh4t, no3, sipor, psi, ssi, sit, n2
   real, dimension(3)             :: kdiapc,poc2,pon1,pon2,poc1,pop2,pop1,po4z1,po4z2,fpoc
   real, dimension(2)             :: po4t,po4, doc0, doc1, doc2, xk1doc
   real                           :: m1,m2,kl01p,kl12p
   double precision               :: s,a11,a12,a21,a22,b1,b2
   logical, intent(in)            :: control  !< debugging
   integer, intent(in)            :: jjj       !< debugging
   
   real, dimension(:,:), allocatable, save :: sumpocsed, sumponsed, sumpopsed, sumsdss_mq, sumsdalg_mq, sumdw2, xalphals, jpoc_neu
   real, dimension(:,:), allocatable, save :: jpon_neu, jpop_neu, bsumpocsed, bsumponsed, bsumpopsed, bsumsdss_mq
   real, dimension(:,:), allocatable, save :: bsumsdalg_mq, bsumdw2, bxalphals, bjpoc_neu, bjpon_neu, bjpop_neu,pocvert1,pocvert2
   
   external :: sed_diffk, lin_sys
   
   save anzzschritt, izaehl_str
   
   if (.not.allocated(sumpocsed))   allocate(sumpocsed(azstrs,1000))
   if (.not.allocated(sumponsed))   allocate(sumponsed(azstrs,1000))
   if (.not.allocated(sumpopsed))   allocate(sumpopsed(azstrs,1000))
   if (.not.allocated(sumsdss_mq))  allocate(sumsdss_mq(azstrs,1000))
   if (.not.allocated(sumsdalg_mq)) allocate(sumsdalg_mq(azstrs,1000))
   if (.not.allocated(sumdw2))      allocate(sumdw2(azstrs,1000))
   if (.not.allocated(xalphals))    allocate(xalphals(azstrs,1000))
   if (.not.allocated(jpoc_neu))    allocate(jpoc_neu(azstrs,1000))
   if (.not.allocated(jpon_neu))    allocate(jpon_neu(azstrs,1000))
   if (.not.allocated(jpop_neu))    allocate(jpop_neu(azstrs,1000))
   if (.not.allocated(bsumpocsed))  allocate(bsumpocsed(azstrs,1000))
   if (.not.allocated(bsumponsed))  allocate(bsumponsed(azstrs,1000))
   if (.not.allocated(bsumpopsed))  allocate(bsumpopsed(azstrs,1000))
   if (.not.allocated(bsumsdss_mq)) allocate(bsumsdss_mq(azstrs,1000))
   if (.not.allocated(bsumsdalg_mq))allocate(bsumsdalg_mq(azstrs,1000))
   if (.not.allocated(bsumdw2))     allocate(bsumdw2(azstrs,1000))
   if (.not.allocated(bxalphals))   allocate(bxalphals(azstrs,1000))
   if (.not.allocated(bjpoc_neu))   allocate(bjpoc_neu(azstrs,1000))
   if (.not.allocated(bjpon_neu))   allocate(bjpon_neu(azstrs,1000))
   if (.not.allocated(bjpop_neu))   allocate(bjpop_neu(azstrs,1000))
   if (.not.allocated(pocvert1))    allocate(pocvert1(azstrs,1000))
   if (.not.allocated(pocvert2))    allocate(pocvert2(azstrs,1000))
   diffk1 = 0.0
   diffk2 = 0.0
 
 
   ibedgs = 1
   if (ilbuhn == 0)izaehl_str = izaehl_str + 1
   if (ilang == 0) then
      do ior = 1, anze + 1
         sumsdss_mq(mstr,ior) = 0.0
         sumsdalg_mq(mstr,ior) = 0.0
      enddo
      if (izaehl_str == 1 .and. ilbuhn == 0)anzzschritt = 0
   endif
   if (izaehl_str == 1 .and. ilbuhn == 0)anzzschritt = anzzschritt + 1
   
   do ior = 1, anze + 1
      if (iwied == 0) then
         sumpocsed(mstr,ior) = 0.0
         sumponsed(mstr,ior) = 0.0
         sumpopsed(mstr,ior) = 0.0
         sumsdss_mq(mstr,ior) = 0.0
         sumsdalg_mq(mstr,ior) = 0.0
         sumdw2(mstr,ior) = 0.0
         xalphals(mstr,ior) = 0.0
         jpoc_neu(mstr,ior) = 0.0
         jpon_neu(mstr,ior) = 0.0
         jpop_neu(mstr,ior) = 0.0
         bsumpocsed(mstr,ior) = 0.0
         bsumponsed(mstr,ior) = 0.0
         bsumpopsed(mstr,ior) = 0.0
         bsumsdss_mq(mstr,ior) = 0.0
         bsumsdalg_mq(mstr,ior) = 0.0
         bsumdw2(mstr,ior) = 0.0
         bxalphals(mstr,ior) = 0.0
         bjpoc_neu(mstr,ior) = 0.0
         bjpon_neu(mstr,ior) = 0.0
         bjpop_neu(mstr,ior) = 0.0
         pocvert1(mstr,ior) = 0.0
         pocvert2(mstr,ior) = 0.0
      endif
      
      if (ilbuhn == 1) then
         zwsumpocsed = sumpocsed(mstr,ior)
         zwsumponsed = sumponsed(mstr,ior)
         zwsumpopsed = sumpopsed(mstr,ior)
         zwsumsdss_mq = sumsdss_mq(mstr,ior)
         zwsumsdalg_mq = sumsdalg_mq(mstr,ior)
         zwsumdw2 = sumdw2(mstr,ior)
         zwxalphals = xalphals(mstr,ior)
         zwjpoc_neu = jpoc_neu(mstr,ior)
         zwjpon_neu = jpon_neu(mstr,ior)
         zwjpop_neu = jpop_neu(mstr,ior)
         
         sumpocsed(mstr,ior) = bsumpocsed(mstr,ior)
         sumponsed(mstr,ior) = bsumponsed(mstr,ior)
         sumpopsed(mstr,ior) = bsumpopsed(mstr,ior)
         sumsdss_mq(mstr,ior) = bsumsdss_mq(mstr,ior)
         sumsdalg_mq(mstr,ior) = bsumsdalg_mq(mstr,ior)
         sumdw2(mstr,ior) = bsumdw2(mstr,ior)
         xalphals(mstr,ior) = bxalphals(mstr,ior)
         jpoc_neu(mstr,ior) = bjpoc_neu(mstr,ior)
         jpon_neu(mstr,ior) = bjpon_neu(mstr,ior)
         jpop_neu(mstr,ior) = bjpop_neu(mstr,ior)
      endif
      
      vvert = max(0.0,hsedvvert(mstr,ior))*24./1000.       ! hsedvvert in mm/h; vvert in m/d
      sumsdss_mq(mstr,ior) = sumsdss_mq(mstr,ior) + sedss_mq(mstr,ior)
      sumsdalg_mq(mstr,ior) = sumsdalg_mq(mstr,ior) + sedalg_mq(mstr,ior)
      if (hbedgs(mstr,ior)>=0.0)ibedgs = 2
      if (ilbuhn == 1)ibedgs = 1
      do ibedg = 1,ibedgs
         if (ibedg == 2) then
            sumpocsedz = sumpocsed(mstr,ior)
            sumponsedz = sumponsed(mstr,ior)
            sumpopsedz = sumpopsed(mstr,ior)
            sumpocsed(mstr,ior) = 0.0
            sumponsed(mstr,ior) = 0.0
            sumpopsed(mstr,ior) = 0.0
         endif
         o20 = vo2(ior)
         if (o20 < 0.1)o20 = 0.1
         no30 = vno3(ior)
         if (no30 < 0.01)no30 = 0.01
         nh40 = vnh4(ior)
         if (nh40 < 0.01)nh40 = 0.01
         po40 = gelp(ior)
  
         if (po40 < 0.001)po40 = 0.001
         si0 = si(ior)
         if (si0 < 0.01)si0 = 0.01
         doc0(1) = hcd(mstr,1,ior)
         doc0(2) = hcd(mstr,2,ior)
         n20 = hflun3(mstr,ior)
         
         fpom = hsedom(mstr,ior) ! anteil organisches material im sediment
         if (ibedg == 2)fpom = 0.001
         w20 = hw2(mstr,ior) ! burial-geschwindigkeit
         temps = tempw(ior)
         ch40 = 0.2
         
         ! porosität (s. ditorro (2001) seite 4)
         poro1 = 0.98*fpom/(fpom+0.011)
         if (poro1 < 0.29)poro1 = 0.29
         poro2 = poro1 - 0.05
         
         dp = 0.00012
         dichta = 2.6
         dichto = 1.2
         fpoc0 = 0.378 ! kohlenstoffanteil im organischen material
         fc_o2 = 2.76
         fc_n = 10.1
         fc_p = 117.
         fdnc_n = 1.31
         fo_nit = 4.57
         fsi_c = 0.05            ! 0.12 chase
         poc1_r = 0.1 !poc1_r in mg/g
         
         dichte = dichta*(1.-fpom)+fpom*dichto
         
         ! feststoffkonzentration
         ! m = ro*(1-poro)  mg*cm-3
         m1 = dichte*(1.-poro1)
         m2 = dichte*(1.-poro2)
         jpoc = dichte*1000.*1000.*fpom*fpoc0*w20
         
         jpsi = jpoc * fsi_c
         jpon = jpoc*(1./fc_n)
         jpop = jpoc*(1./fc_p)
         thtad = 1.08
         thtan3 = 1.08
         thtan4 = 1.123
         thtac4 = 1.08
         thtaom = 1.14
         thtadp = 1.117
         thtasi = 1.1
         
         if (fpoc1e < 0.0 .and. fpoc2e < 0.0) then
            fpoc(1) = 0.65
            fpoc(2) = 0.15
            fpoc(3) = 0.2
         else if (fpoc1e > 0.0 .and. fpoc2e > 0.0) then
            fpoc(1) = fpoc1e
            fpoc(2) = fpoc2e
            fpoc(3) = 1. - fpoc1e - fpoc2e
         endif
         kappc = 0.57 !2.8    ! methanoxidationsgeschwindigkeit 0.57 m/d
         
         ! kohlenstoffmineralisationsgeschwindigkeit
         kdiapc(1) = 0.035
         kdiapc(2) = 0.0018
         kdiapc(3) = 0.0
         km_nh4 = 0.728 ! halbsättigungskonstante für n bei nitrifikation
         kmo_n4 = 0.37  ! halbsättigungskonstante für o2 bei nitrifikation  !0.37
         km_no3 = 0.4   ! halbsättigungskonstante für n bei denitrifikation und p_freisetzung (boudreau)
         kmo_no3 = 0.26 ! halbsättigungskonstante für o2 bei denitrifikation und p_freisetzung (boudreau)
         ksi = 0.03     ! lösungsrate in 1/d für partikuläres silikat, optimiert aus daten der saar
         km_psi = 50.   ! halbsättigungskonstante in gsi/m3 für mineralisation von partik. silikat, ditoro
         vbc = obsb(ior)/ocsb(ior)
         xk1doc(1) = 0.18
         xk1doc(2) = 0.3
         alphals = 0.848*vcb**0.347
         dim_kn = 1.3
         if (kdnh3e < -1.)kdnh3e = 0.0
         if (kdnh3e < 0.0) then
            kdnh31 = dim_kn*poro1/((1.-poro1)*dichte)
            kdnh32 = dim_kn*poro2/((1.-poro2)*dichte)
         else
            kdnh31 = kdnh3e !partitionskoeffizient für ammonium
            kdnh32 = kdnh31
         endif
         
         kdpo42 = 350 ! partitionskoeffizient für p in der anaeroben schicht
         dkdp41 = 330.
         o2cpo4 = 2.
         kdsi1 = 14.! partitionskoeffizient für silikat
         
         h2 = 0.1   ! dicke der anaeroben schicht
         h1 = 0.01  ! anfangsdicke der aeroben schicht
         
         !  berechnung der partikulären und gelösten fraktion für ammonium in schicht 1 und 2
         fd1 = 1. - (1./(1.+m1 * kdnh31))
         fd2 = 1. - (1./(1.+m2 * kdnh32))
         
         no3(1) = no30
         no3(2) = no30
         nh4(1) = nh40
         nh4(2) = nh40
         
         ! diagenese
         ftpom = thtaom**(temps-20.)
         w12 = dp
         do it = 1,10 ! iterationsschleife anfang
            
            ! berechnung des diffusionskoeffizienten in der aeroben und anaeroben schicht, diffk1, diffk2
            call sed_diffk(tiefe(ior), vmitt(ior), rau(ior), h1, h2, hdkorn(mstr,ior), vvert, &
                           poro1, poro2, diffk1, diffk2, difkp1, difkp2, vvert1, vvert2)
                           
            if (it == 1) s = dp/h1
            kl12 = diffk2*thtad**(temps-20.)/(h2/2.) ! nur diese diffusivität geht ein
            
            do i = 1,3 ! fraktionsschleife anfang
               
               ! -- poc ---
               ! schicht 1
               a11 = -kdiapc(i)*ftpom*h1-w20-w12
               a12 = w12
               b1 = -fpoc(i)*jpoc
               ! schicht 2
               a21 = w20+w12
               a22 = -kdiapc(i)*ftpom*h2-w20-w12
               b2 = -fpoc(i)*jpoc
               
               call lin_sys(a11,a12,a21,a22,b1,b2,poc1(i),poc2(i))
               ! --- doc ---
               if (i < 3) then
                  !  schicht 1
                  a11 = -xk1doc(i)*ftpom*h1-kl12-s
                  a12 = kl12
                  b1 = -s*doc0(i)
                  !  schicht 2
                  a22 = -kdiapc(i)*ftpom*h2-kl12
                  a22 = -kl12
                  b2 = 0.0
                  call lin_sys(a11,a12,a21,a22,b1,b2,doc1(i),doc2(i))
               endif
               
               ! --- pon ---
               ! schicht 1
               a11 = -kdiapc(i)*ftpom*h1-w20-w12
               a12 = w12
               b1 = -fpoc(i)*jpon
               !  schicht 2
               a21 = w20+w12
               a22 = -kdiapc(i)*ftpom*h2-w20-w12
               b2 = -fpoc(i)*jpon
               call lin_sys(a11,a12,a21,a22,b1,b2,pon1(i),pon2(i))
               
               ! --- pop ---
               ! schicht 1
               a11 = -kdiapc(i)*ftpom*h1-w20-w12
               a12 = w12
               b1 = -fpoc(i)*jpop
               ! schicht 2
               a21 = w20+w12
               a22 = -kdiapc(i)*ftpom*h2-w20-w12
               b2 = -fpoc(i)*jpop
               
               call lin_sys(a11,a12,a21,a22,b1,b2,pop1(i),pop2(i))
               
            enddo ! fraktionsschleife ende
            
            jc1 = 0.0
            jn1 = 0.0
            jp1 = 0.0
            do i = 1,2 ! summenbildung in der aeroben schicht
               if (i == 1) then
                  h11 = h1 - sumdw2(mstr,ior) * xalphals(mstr,ior)
                  hcalphals = xalphals(mstr,ior)
               else
                  h11 = h1 - sumdw2(mstr,ior) * (1.-xalphals(mstr,ior))
                  hcalphals = (1.-xalphals(mstr,ior))
               endif
               if (h11 < 0.0) then
                  h11 = 0.0
                  fakh = 1.
               else
                  fakh = sumdw2(mstr,ior)/h1
               endif
               
               ! todo summation falsch? der jpoc_neu und der  pocvert1 zweimal hinzuaddiert?
               jc1 = jc1                                                       &
                   + kdiapc(i) * ftpom * poc1(i) * h11                         &
                   + xk1doc(i) * ftpom * jpoc_neu(mstr,ior) * hcalphals * fakh & 
                   + xk1doc(i) * ftpom * doc1(i)*h1                            &
                   + xk1doc(i) * ftpom * pocvert1(mstr,ior) * hcalphals
                   
               jn1 = jn1                                                       &
                   + kdiapc(i) * ftpom * pon1(i) * h11                         &
                   + xk1doc(i) * ftpom * jpon_neu(mstr,ior) * hcalphals * fakh &
                   + xk1doc(i) * ftpom * doc1(i) * nl0(ior) * h1               &
                   + xk1doc(i) * ftpom * pocvert1(mstr,ior) * nl0(ior) * hcalphals
               
               jp1 = jp1                                                       &
                   + kdiapc(i) * ftpom * pop1(i) * h11                         &
                   + xk1doc(i) * ftpom * jpop_neu(mstr,ior) * hcalphals * fakh &
                   + xk1doc(i) * ftpom * doc1(i) * pl0(ior) * h1               &
                   + xk1doc(i) * ftpom * pocvert1(mstr,ior) * pl0(ior) * hcalphals
            enddo
            jc = 0.0
            jn = 0.0
            jp = 0.0
            
            do i = 1,2 !summenbildung in der anaeroben schicht
               ! summation falsch? pocvert2 zweimal hinzuaddiert?
               jc = jc                                      &
                  + kdiapc(i) * ftpom * poc2(i) * h2        &
                  + xk1doc(i) * ftpom * doc2(i) * h2        &
                  + xk1doc(2) * ftpom * pocvert2(mstr,ior)
                  
               jn = jn                                                  &
                  + kdiapc(i) * ftpom * pon2(i) * h2                    &
                  + xk1doc(i) * ftpom * doc2(i) * nl0(ior) * h2         &
                  + xk1doc(2) * ftpom * pocvert2(mstr,ior) * nl0(ior)   
               
               jp = jp                                                  &
                  + kdiapc(i) * ftpom * pop2(i) * h2                    &
                  + xk1doc(i) * ftpom * doc2(i) * pl0(ior) * h2         &
                  + xk1doc(2) * ftpom * pocvert2(mstr,ior) * pl0(ior)
            enddo
            jc  = jc  * fc_o2
            jc1 = jc1 * fc_o2
            if (it == 1) then
               sod = jc1+jc+4.57*jn1
            else
            endif
            ch4sat = 100.*(1.+(tiefe(ior)/10.))*1.024**(20.-temps)
            poc_1 = poc2(1)/(1000.*m2)
            w12 = (dp/h2)*thtadp**(temps-20.)*(poc_1/poc1_r)
            
            s = sod/o20
            h1 = 0.5
            if (s /= 0.0) h1 = diffk1*thtad**(temps-20.)/s ! neuberechnung der dicke der aeroben schicht
            if (h1 <= 0.0) h1 = 0.01
            if (h1 > 0.5)  h1 = 0.5
            
            ! --- ammonium ---
            if (s == 0.0 .or. o20 == 0.0) then
               n4ton3 = 0.0
            else
               n4ton3 = knh4**2*thtan4**(temps - 20)/s*(o20/2.)/(kmo_n4 + (o20/2.))
            endif
            
            ! schicht 1
            a11 = -kl12*fd1-w12*fp1-n4ton3*fd1-s*fd1-w20
            a12 = kl12*fd2+w12*fp2
            b1 = -jn1-s*nh40
            
            ! schicht 2
            a21 = kl12*fd1+w12*fp1+w20
            a22 = -kl12*fd2-w12*fp2-w20
            b2 = -jn
            call lin_sys(a11,a12,a21,a22,b1,b2,nh4t(1),nh4t(2))
            
            nh4(1) = nh4t(1)*fd1
            nh4(2) = nh4t(2)*fd2
            fd1_aus = fd1
            nsod = fo_nit*n4ton3*nh4(1)
            benmx1 = ynmx1e
            benks1 = stks1e
            bettf = benmx1*(nh4(1)/(vnh4(1)+benks1))*(o20/2.)/(kmo_n4 + (o20/2.))*thtan4**(temps - 20)
            
            ! --- denitrifikation/nitrat ---
            hcond1 = (kmo_no3/(kmo_no3+(o20/2.)))
            !*no3(1)/(no3(1)+km_no3)
            hcond2 = no3(2)/(no3(2)+km_no3)
            ! denit(1) = sqrt(kapn3*thtan3**(temps-20.)*s)
            denit(1) = 0.0
            if ((s*hcond1) /= 0.0)denit(1) = kapn3**2*thtan3**(temps-20.)/s*hcond1 ! nach ditoro
            denit(2) = kapn3*thtan3**(temps-20.)
            
            ! schicht 1
            a11 = -kl12-denit(1)-s
            a12 = kl12
            b1 = -s*no30-n4ton3*nh4(1)
            
            ! schicht 2
            a21 = kl12
            a22 = -kl12-denit(2)
            b2 = 0.0
            
            call lin_sys(a11,a12,a21,a22,b1,b2,no3(1),no3(2))
            jdenit(1) = denit(1)*no3(1)
            jdenit(2) = denit(2)*no3(2)
            jdenig = jdenit(1)+jdenit(2)
            
            jc4n31 = jc1*hcond1*no3(1)/(no3(1)+km_no3)
            jc4n32 = jdenit(2)*fdnc_n*fc_o2
            
            ! --- n2 bildung ---
            ! schicht 1
            a11 = -kl12-s
            a12 = kl12
            b1 = -s*n20-denit(1)*1.1*no3(1)
            
            ! schicht 2
            a21 = kl12
            a22 = -kl12
            b2 = -denit(2)*1.1*no3(2)
            
            call lin_sys(a11,a12,a21,a22,b1,b2,n2(1),n2(2))
            
            ! --- sod ---
            ! methanconsumption durch denitrifikation in layer 1 und layer 2
            jcneu = jc-jc4n32
            if (jcneu < 0.0)jcneu = 0.0
            csodmx = sqrt(2.*kl12*ch4sat*jcneu)
            if (jcneu < csodmx)csodmx = jcneu
            hcon1 = 0.0
            hcon2 = 0.0
            if (sod /= 0.0) then
               hcon1 = kappc*thtac4**(temps-20.)*o20/sod
               hcon2 = o20/sod
            endif
            
            if (hcon1 > 400.) then
               csod = csodmx
            else
               sech = 2./(exp(hcon1)+exp(-hcon1))
               csod = csodmx*(1.-sech)
            endif
            csod1 = jc1 - jc4n31
            sodinit = sod
            sod = (sod+csod+csod1+nsod)/2.
            if (abs((sod-sodinit)/sod)*100. < 1.)exit
            
            ! --- methankonzentration in der aeroben schicht ---
            c4toc2 = (kappc**2*thtac4**((temps-20.)/2.))/s
            ch41 = (csodmx+s*ch40)/(c4toc2+s)
            
         enddo ! iterationsschleife ende
         
         ! --- orthophosphat und silikat ---
         ! phosphor
         kl01p = (difkp1/h1)*thtad**(temps-20.)
         kl12p = difkp2*thtad**(temps-20.)/(h2/2.)
         if (sorpcap < 0.0) then
            sorpcap1 = 4.34*fpom**1.05
         else
            sorpcap1 = sorpcap
         endif
         langcon1 = klang
         po41por = 0.01
         po42por = 0.01
         
         ! silikat
         sipor(1) = 0.001
         sipor(2) = 0.001
         sisaett = 0.4667*67.8+0.4667*1.48*temps
         
         do it = 1,30
            ! phosphor
            adsorbp = sorpcap1*langcon1*po41por/(1.+langcon1*po41por)
            adsorbp = adsorbp/1000.
            po41por = po41por/1000./1000.
            kdpo41 = adsorbp/po41por
            aus_kd = kdpo41
            hcono = 0.0
            hconn = 0.0
            hcono = 1. ! da in schicht 2 kein sauerstoff
            hconn = km_no3/(km_no3+no3(2))
            adsorbp = sorpcap1*langcon1*po42por/(1.+langcon1*po42por)
            adsorbp = adsorbp*(1.-(hcono*hconn))
            adsorbp = adsorbp/1000.
            po42por = po42por/1000./1000.
            kdpo42 = adsorbp/po42por
            fd1 = 1./(1.+m1*kdpo41)
            fp1 = 1.-fd1
            fd2 = 1./(1.+m2*kdpo42)
            fp2 = 1.-fd2
            !
            !      schicht 1
            a11 = -kl12*fd1-w12*fp1-s*fd1-w20
            a12 = kl12*fd2+w12*fp2
            b1 = -s*po40-jp1
            !
            !      schicht 2
            a21 = kl12*fd1+w12*fp1+w20
            a22 = -kl12*fd2-w12*fp2-w20
            b2 = -jp
            !
            po42tinit = po4t(2)
            call lin_sys(a11,a12,a21,a22,b1,b2,po4t(1),po4t(2))
            !
            po4(1) = po4t(1)*fd1
            po4(2) = po4t(2)*fd2
            if (abs(po4t(2)-po42tinit) < 0.05)exit
            po42por = (po42por*1000. + po4(2))/2.
            po41por = (po41por*1000. + po4(1))/2.
         enddo  ! ende phosphor
         
         ! --- phosphor-desorption ---
         po40z = po40
         po40 = 0.000
         ! schicht 1
         a11 = -kl12*fd1-w12*fp1-s*fd1-w20
         a12 = kl12*fd2+w12*fp2
         b1 = -s*po40-jp1
         
         ! schicht 2
         a21 = kl12*fd1+w12*fp1+w20
         a22 = -kl12*fd2-w12*fp2-w20
         b2 = -jp
         
         call lin_sys(a11,a12,a21,a22,b1,b2,po4t(1),po4t(2))
         
         po4z1(1) = po4t(1)*fd1
         po4z1(2) = po4t(2)*fd2
         hcono = 0.0
         hconn = 0.0
         hcono = kmo_no3/(kmo_no3+(o20/2.))
         hconn = km_no3/(km_no3+no3(1))
         delta_adp = hcono*hconn
         adsorbp = sorpcap1*langcon1*po41por/(1.+langcon1*po41por)
         adsorbp = adsorbp*(1.-delta_adp)
         adsorbp = adsorbp
         po41por = po41por/1000
         kdpo41 = adsorbp/po41por
         fd1 = 1./(1.+m1*kdpo41)
         fp1 = 1.-fd1
         fd2 = 1./(1.+m2*kdpo42)
         fp2 = 1.-fd2
         po4z2(1) = po4t(1)*fd1
         po4z2(2) = po4t(2)*fd2
         
         po4(1) = po4(1)+(po4z2(1)-po4z1(1))
         po40 = po40z
         !ende phosphor
         
         do it = 1, 30
            ! silikat
            if (it == 1) then
               si1 = 0.0
               si2 = 0.0
               psi1 = 1000.
               psi2 = 1000.
            else
               si1 = (si1 + sipor(1))/2.
               si2 = (si2 + sipor(2))/2.
               psi1 = (psi1 + psi(1))/2.
               psi2 = (psi2 + psi(2))/2.
            endif
            kappasi1 = ksi*thtasi**(temps-20.)*(1.-(min(sisaett,si1))/sisaett)*(psi1/(psi1+km_psi))
            kappasi2 = ksi*thtasi**(temps-20.)*(1.-(min(sisaett,si2))/sisaett)*(psi2/(psi2+km_psi))
            ssi(1) = ksi*h1*thtasi**(temps-20.)*(psi1/(psi1+km_psi))*(sisaett-min(sisaett,si1))
            ssi(2) = ksi*h2*thtasi**(temps-20.)*(psi2/(psi2+km_psi))*(sisaett-min(sisaett,si2))
            
            kdsi2 = kdsi1 ! partitionskoeffizient in der anaeroben schicht
            hcono = 1.
            hconn = km_no3/(km_no3+no3(2))
            fdsi1 = 1./(1.+m1*kdsi1)
            fpsi1 = 1.-fdsi1
            fdsi2 = 1./(1.+m2*kdsi2)
            fpsi2 = 1.-fdsi2
            a11 = -kappasi1*h1-w12-w20
            a12 = w12
            b1 = -jpsi
            a21 = w20+w12
            a22 = -kappasi2*h2-w20-w12
            b2 = -jpsi
            psi2init = psi(2)
            call lin_sys(a11,a12,a21,a22,b1,b2,psi(1),psi(2))
            psi(1) = psi(1) + hsised(mstr,ior)
            a11 = -kl12*fdsi1-w12*fpsi1-s*fdsi1-w20
            a12 = kl12*fdsi2+w12*fpsi2
            b1 = -ssi(1)-s*si0
            a21 = kl12*fdsi1+w12*fpsi1+w20
            a22 = -kl12*fdsi2-w12*fpsi2-w20
            b2 = -ssi(2)
            call lin_sys(a11,a12,a21,a22,b1,b2,sit(1),sit(2))
            sipor(1) = min(sisaett,sit(1)*fdsi1)
            sipor(2) = min(sisaett,sit(2)*fdsi2)
            if (abs(psi(2)-psi2init) < 0.01)exit
         enddo  ! silikat
         
         ! --------------------------------------------------------------------
         ! stoffflüsse in den wasserkörper
         ! --------------------------------------------------------------------
         hjno3(mstr,ior) = s * (no3(1) - no30) ! + flux vom sediment in den wasserkörper
         hjnh4(mstr,ior) = s * (nh4(1) - nh40)
         hjpo4(mstr,ior) = s * (po4(1) - po40)
         jdoc1(ior) = s * (doc1(1) - doc0(1))
         jdoc2(ior) = s * (doc1(2) - doc0(2))
         hjsi(mstr,ior) = s *(sipor(1) - si0)
         hjo2(mstr,ior) = sod
         jch4aq(ior) = s * (ch41 - ch40)
         hjn2(mstr,ior) = s * (n2(1) - n20)
         
         ! --------------------------------------------------------------------
         ! berechnung der menge an algen die aktiv ins sediment transportiert werden
         ! --------------------------------------------------------------------
         if (vvert > 0.0 .and. ilbuhn == 0) then
            dalgvert = (aki(ior)*caki + agr(ior)*cagr + abl(ior)*cabl)*vvert1
            dpocvert = pocvert1(mstr,ior)*kdiapc(1)*ftpom*tflie
            pocvert1(mstr,ior) = max(0.0,pocvert1(mstr,ior) + dalgvert - dpocvert)
            pocvert1(mstr,ior) = 0.0
            dpocvert = pocvert2(mstr,ior)*kdiapc(1)*ftpom*tflie
            pocvert2(mstr,ior) = max(0.0,pocvert2(mstr,ior) - dpocvert + dalgvert * vvert2/vvert1)
            pocvert2(mstr,ior) = 0.0 * vvert2/vvert1
            aki(ior) = aki(ior) - aki(ior)*vvert1*tflie/tiefe(ior)
            agr(ior) = agr(ior) - agr(ior)*vvert1*tflie/tiefe(ior)
            abl(ior) = abl(ior) - abl(ior)*vvert1*tflie/tiefe(ior)
            chlaki(ior) = chlaki(ior) - chlaki(ior)*vvert1*tflie/tiefe(ior)
            chlagr(ior) = chlagr(ior) - chlagr(ior)*vvert1*tflie/tiefe(ior)
            chlabl(ior) = chlabl(ior) - chlabl(ior)*vvert1*tflie/tiefe(ior)
         endif
        
         if (ibedg == 1) then
            hjno3z = hjno3(mstr,ior)
            hjnh4z = hjnh4(mstr,ior)
            hjpo4z = hjpo4(mstr,ior)
            hjo2z = hjo2(mstr,ior)
            hjch4aqz = jch4aq(ior)
            hjsiz = hjsi(mstr,ior)
         else
            hjno3(mstr,ior) = hjno3(mstr,ior)*(1.-hbedgs(mstr,ior)) + hjno3z*hbedgs(mstr,ior)
            hjnh4(mstr,ior) = hjnh4(mstr,ior)*(1.-hbedgs(mstr,ior)) + hjnh4z*hbedgs(mstr,ior)
            hjpo4(mstr,ior) = hjpo4(mstr,ior)*(1.-hbedgs(mstr,ior)) + hjpo4z*hbedgs(mstr,ior)
            hjo2(mstr,ior) = hjo2(mstr,ior)*(1.-hbedgs(mstr,ior)) + hjo2z*hbedgs(mstr,ior)
            jch4aq(ior) = jch4aq(ior)*(1.-hbedgs(mstr,ior)) + hjch4aqz*hbedgs(mstr,ior)
            hjsi(mstr,ior) = hjsi(mstr,ior)*(1.-hbedgs(mstr,ior)) + hjsiz*hbedgs(mstr,ior)
            exit
         endif
         
         if (ilang == 0) then
            sumpocsed(mstr,ior) = 0.0
            sumponsed(mstr,ior) = 0.0
            sumpopsed(mstr,ior) = 0.0
            sumdw2(mstr,ior) = 0.0
            xalphals(mstr,ior) = 0.0
         else
            
            ! veränderung des leichtabbaubaren kohlenstoffsgehalts in der
            ! aeroben schicht durch sedimentation im zeitschritt<tflie>
            pocsed = orgcsd_abb(mstr,ior)+sedalk(ior)*caki+sedalg(ior)*cagr+sedalb(ior)*cabl
            ponsed = orgcsd_abb(mstr,ior)*nl0(ior)+sedalk(ior)*q_nk(ior)+sedalg(ior)*q_ng(ior)+sedalb(ior)*q_nb(ior)
            popsed = orgcsd_abb(mstr,ior)*pl0(ior)+sedalk(ior)*q_pk(ior)+sedalg(ior)*q_pg(ior)+sedalb(ior)*q_pb(ior)
            pocsed1 = pocsed*tiefe(ior) * alphals                   ! podsed1: leichtabbaubarer anteil
            pocsed2 = pocsed*tiefe(ior) * (1.-alphals)              ! podsed2: schwerabbarer anteil
            ponsed1 = ponsed*tiefe(ior) * alphals
            ponsed2 = ponsed*tiefe(ior) * (1.-alphals)
            popsed1 = popsed*tiefe(ior) * alphals
            popsed2 = popsed*tiefe(ior) * (1.-alphals)
            dsumdw21 = sumdw2(mstr,ior)*xalphals(mstr,ior)*xk1doc(1)*tflie
            dsumdw22 = sumdw2(mstr,ior)*(1.-xalphals(mstr,ior))*xk1doc(2)*tflie
            sumdw21 = sumdw2(mstr,ior)*xalphals(mstr,ior) - dsumdw21 + pocsed1/(dichto*1000.*1000.)
            sumdw22 = sumdw2(mstr,ior)*(1.-xalphals(mstr,ior)) - dsumdw22 + pocsed2/(dichto*1000.*1000.)
            
            if (sumw21 > 0.0) then
               xalphals(mstr,ior) = sumdw21/(sumdw21+sumdw22)
            endif
            sumdw2(mstr,ior) = sumdw21+sumdw22
            
            ! fehler? pocsed = (pocsed1+pocsed1)*(1./tflie)
            pocsed = (pocsed1+pocsed2)*(1./tflie)
            ponsed = (ponsed1+ponsed2)*(1./tflie)
            popsed = (popsed1+popsed2)*(1./tflie)
            
            sumpocsed(mstr,ior) = sumpocsed(mstr,ior) + pocsed
            jpoc_neu(mstr,ior) = sumpocsed(mstr,ior)/(anzzschritt-1)
            sumponsed(mstr,ior) = sumponsed(mstr,ior) + ponsed
            jpon_neu(mstr,ior) = sumponsed(mstr,ior)/(anzzschritt-1)
            
            sumpopsed(mstr,ior) = sumpopsed(mstr,ior) + popsed
            jpop_neu(mstr,ior) = sumpopsed(mstr,ior)/(anzzschritt-1)
         endif
      
      enddo
      
      if (ibedgs == 2) then
         sumpocsed(mstr,ior) = sumpocsedz
         sumponsed(mstr,ior) = sumponsedz
         sumpopsed(mstr,ior) = sumpopsedz
      endif
      
      if (ilbuhn == 1) then
         bsumpocsed(mstr,ior)  = sumpocsed(mstr,ior)
         bsumponsed(mstr,ior)  = sumponsed(mstr,ior)
         bsumpopsed(mstr,ior)  = sumpopsed(mstr,ior)
         bsumsdss_mq(mstr,ior) = sumsdss_mq(mstr,ior)
         bsumsdalg_mq(mstr,ior)= sumsdalg_mq(mstr,ior)
         bsumdw2(mstr,ior)     = sumdw2(mstr,ior)
         bxalphals(mstr,ior)   = xalphals(mstr,ior)
         bjpoc_neu(mstr,ior)   = jpoc_neu(mstr,ior)
         bjpon_neu(mstr,ior)   = jpon_neu(mstr,ior)
         bjpop_neu(mstr,ior)   = jpop_neu(mstr,ior)
         sumpocsed(mstr,ior)   = zwsumpocsed
         sumponsed(mstr,ior)   = zwsumponsed
         sumpopsed(mstr,ior)   = zwsumpopsed
         sumsdss_mq(mstr,ior)  = zwsumsdss_mq
         sumsdalg_mq(mstr,ior) = zwsumsdalg_mq
         sumdw2(mstr,ior)      = zwsumdw2
         xalphals(mstr,ior)    = zwxalphals
         jpoc_neu(mstr,ior)    = zwjpoc_neu
         jpon_neu(mstr,ior)    = zwjpon_neu
         jpop_neu(mstr,ior)    = zwjpop_neu
      endif
      
      ! reduktion des neu sedimentierten partikulären silikats
      hsised(mstr,ior) = hsised(mstr,ior) - ((ssi(1)*hsised(mstr,ior))/(h1*psi(1)))*tflie
      
   enddo
   
   if (izaehl_str == azstrs)izaehl_str = 0
   
   return
end subroutine sedflux

