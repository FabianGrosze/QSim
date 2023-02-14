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

!> Transport
!! @author Volker Kirchesch
!! @date 25.07.2019
subroutine Transport(anze,deltat,izeits,isub_dt,isub_dt_Mac,hvmitt,elen,flag,tempw,vo2,vnh4,vno3,vno2,vx0                  &
                     ,vx02,Si,mstr,gelP,obsb,ocsb,vbsb,vcsb,CHNF,BVHNF,CD,CP,CM,BAC,zooind,chla,aki,agr,abl,chlaki,chlagr  &
                     ,chlabl,vkigr,antbl,abrzo1,ssalg,ss,svhemk,svhemg,svhemb,akbcm,agbcm,abbcm,fssgr,fbsgr,frfgr,gesN     &
                     ,gesP,nl0,pl0,Q_NK,Q_PK,Q_SK,Q_NG,Q_PG,Q_NB,Q_PB,stind,mw,pw,ca,lf,coli,DOSCF                         &
                     ,dlarvn,vph,iph,iwsim,htempw,hgesN,hgesP,hbsb,hcsb,hCHNF,hBVHNF,hCD,hCP,hCM,hBAC,hnh4,ho2             &
                     ,hno3,hno2,hx0,hx02,hsi,hchla,haki,hagr,habl,hchlak,hchlag,hchlab,hvkigr,hantbl,hssalg,hss,hzooi      &
                     ,hgelp,hmw,hpw,hca,hlf,hph,hdlarn,hcoli,hDOSCF,hvbsb,hvcsb,SKmor,hSKmor,iflRi,dl,Uvert,iMAC           &
                     ,iwied,nkzs,tflie,jpoin1,itags,monats,Uhrz,iverfahren,ianze_max,Qmx_NK,Qmx_NB,Qmx_NG,Qmx_PK           &
                     ,Qmx_PB,Qmx_PG,hFluN3,TGZoo,akmor_1,agmor_1,abmor_1                                                   &
                     ,hgsZn,hglZn,hgsCad,hglCad,hgsCu,hglCu,hgsNi,hglNi,hgsAs,hglAs,hgsPb,hglPb,hgsCr,hglCr,hgsFe,hglFe    &
                     ,hgsHg,hglHg,hgsMn,hglMn,hgsU,hglU,mtracer,nkztot_max,ischwer)
   
   use allodim
   
   implicit none
   
   integer                             :: nkz, nkztot_max, mtracer, mstr, monats
   integer                             :: ktrans, kktrans, j, jpoin1, izeits
   integer                             :: iwsim, iwied, iwahld, iverfahren, itime
   integer                             :: itags, isub_dtx, ischwer, iph, ior
   integer                             :: iork, iorks, ianze_max, anze
   real                                :: vhrann, vhrand, uhrz, tflie, tempn
   real                                :: temp0, sumdet, qmx_pk, qmx_pg, qmx_pb
   real                                :: qmx_nk, qmx_ng, qmx_nb, hk, deltat
   integer, dimension(azStrs)          :: iflRi, imac, isub_dt, isub_dt_Mac
   integer, dimension(ialloc2)         :: iore, flag, nkzs
   real                                :: mue, lgh
   real, dimension(ialloc2)            :: elen, vmitt, tempw, vo2, Ue, vnh4, vno2, vno3, vx0, U, vx02, Si
   real, dimension(ialloc2)            :: gelP, dl, obsb, ocsb, vbsb, vcsb, CHNF, BVHNF, CM, BAC, zooind, chla
   real, dimension(ialloc2)            :: aki, agr, abl, SKmor, chlaki, chlagr, chlabl, vkigr, antbl, abrzo1
   real, dimension(ialloc2)            :: ssalg, ss, svhemk, svhemg, svhemb, akbcm, agbcm, stind, abbcm, fbsgr
   real, dimension(ialloc2)            :: fssgr, gesN, gesP, frfgr, nl0, pl0, Q_NK, Q_PK, Q_SK, Q_NG, Q_PG
   real, dimension(ialloc2)            :: Q_NB, Q_PB, mw, pw, ca,lf, coli, DOSCF, dlarvn, vph, vh
   real, dimension(2,ialloc2)          :: CD, CP
   real, dimension(50,ialloc2)         :: Uvert
   real, dimension(azStrs,ialloc2)     :: hvmitt, hDOSCF, htempw, hgesN, hgesP, hbsb, hcsb, hCHNF, hBVHNF, hBAC
   real, dimension(azStrs,ialloc2)     :: hCM, hnh4, ho2, hno3, hno2, hx0, hx02, hsi, hchla, haki, hagr, habl
   real, dimension(azStrs,ialloc2)     :: hchlak, hchlag, hchlab, hvkigr, hantbl, hssalg, hss, hzooi, hgelp
   real, dimension(azStrs,ialloc2)     :: hmw, hpw, hca, hlf, hph, hdlarn, hcoli, hvbsb, hvcsb, hSKmor,hFluN3
   real, dimension(azStrs,ialloc2)     :: TGZoo, akmor_1, agmor_1, abmor_1
   real, dimension(azStrs,ialloc2)     :: hgsZn, hglZn, hgsCad, hglCad, hgsCu, hglCu, hgsNi, hglNi
   real, dimension(azStrs,ialloc2)     :: hgsAs, hglAs, hgsPb, hglPb, hgsCr, hglCr, hgsFe, hglFe
   real, dimension(azStrs,ialloc2)     :: hgsHg, hglHg, hgsMn, hglMn, hgsU, hglU
   real, dimension(azStrs,2,ialloc2)   :: hCD, hCP
   
   external :: advdiff
   
   iwahlD = 1
   nkz = 1
   j = 1
   iork = 0
   sumdet = 0.0
   isub_dtx = isub_dt(mstr)
   if (imac(mstr) == 1)isub_dtx = isub_dt_Mac(mstr)
   
   ! Total number of transport variables
   kktrans = 69
   if (ischwer == 1) kktrans = kktrans + 22
   
   do 1811 itime = 1,izeits
      ktrans = 1
      jpoin1 = 0
      
      do ior = 1,anze+1
         U(ior) = tempw(ior)
         vmitt(ior) = hvmitt(mstr,ior)
         if (nkzs(ior) == 1)Uvert(1,ior) = vmitt(ior)
      enddo
      
      temp0 = htempw(mstr,1)
      tempn = htempw(mstr,anze+1)
      
      888 continue
      !      if(iflRi(mstr).eq.0)goto 911
      if (U(1) < 0.0 .and. ktrans /= 1 .and. ktrans /= 57) goto 911
      
      call advdiff(anze,elen,vmitt,Uvert,dl,flag,ktrans,U,temp0,tempn                                                  &
                   ,deltat,sumdet,itime,izeits,mstr,iwied,iwahlD,nkz,nkzs,tflie,iFlRi                                  &
                   ,jpoin1,itags,monats,isub_dtx,imac,iverfahren,kktrans,nkztot_max,ianze_max,mtracer,iwsim,uhrz)
      
      911 goto (600,602,604,606,608,610,612,614,616,618,620             &
      ,622,624,626,628,630,632,634,636,638,640,642,644                  &
      ,646,648,650,658,660,662,664,666                                  &
      ,668,670,672,674,676,678,680,682                                  &
      ,690,692,694,696,698,500,502,504,506,508                          &
      ,510,512,514,516,518,520,522,524,526,528                          &
      ,530,532,534,536,537,539,540,560,565,570                          &
      ,542,544,546,548,550,552,554,556,558,580,582,584                  &
      ,586,588,590,592,594,596,598,400,402,404)ktrans
      
      600 continue
      do ior = 1,anze+1
         tempw(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      jpoin1 = 1
      if (iwsim == 4 .or. iwsim == 5 .or. (iwsim == 2 .and. coli(1) < 0.0)) then
         goto 1811
      elseif (iwsim == 2 .and. coli(1)>=0.0) then
         ktrans = 55
         goto 818
      endif
      goto 700
      
      602 continue
      do ior = 1,anze+1
         vo2(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 702
      
      604 continue
      do ior = 1,anze+1
         vNH4(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 704
      
      606 continue
      do ior = 1,anze+1
         vNO2(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 706
      
      608 continue
      do ior = 1,anze+1
         vNO3(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 708
      
      610 continue
      do ior = 1,anze+1
         vx0(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 710
      
      612 continue
      do ior = 1,anze+1
         vx02(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 712
      
      614 continue
      do ior = 1,anze+1
         Si(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 714
      
      616 continue
      do ior = 1,anze+1
         gelP(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 716
      
      618 continue
      do ior = 1,anze+1
         obsb(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 718
      
      620 continue
      do  ior = 1,anze+1
         ocsb(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 720
      
      622 continue
      if (iflri(mstr) /= 0) then
         do ior = 1,anze+1
            chla(ior) = U(ior)
         enddo
      endif
      ktrans = ktrans+1
      goto 722
      
      624 continue
      if (iflri(mstr) /= 0) then
         do ior = 1,anze+1
            chlaki(ior) = U(ior)
         enddo
      endif
      ktrans = ktrans+1
      goto 724
      
      626 continue
      do ior = 1,anze+1
         chlagr(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 726
      
      628 continue
      do ior = 1,anze+1
         chlabl(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 728
      
      630 continue
      do ior = 1,anze+1
         aki(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 730
      
      632 continue
      do ior = 1,anze+1
         agr(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 732
      
      634 continue
      do ior = 1,anze+1
         abl(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 734
      
      636 continue
      if (iflri(mstr) /= 0) then
         do ior = 1,anze+1
            vkigr(ior) = U(ior)
         enddo
      endif
      ktrans = ktrans+1
      goto 736
      
      638 continue
      if (iflri(mstr) /= 0) then
         do ior = 1,anze+1
            antbl(ior) = U(ior)
         enddo
      endif
      ktrans = ktrans+1
      goto 738
      
      640 continue
      do ior = 1,anze+1
         svhemk(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 740
      
      642 continue
      do ior = 1,anze+1
         svhemg(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 742
      
      644 continue 
      do ior = 1,anze+1
         svhemb(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 744
      
      646 continue
      do ior = 1,anze+1
         akbcm(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 746
      
      648 continue
      do ior = 1,anze+1
         agbcm(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 748
      
      650 continue
      do ior = 1,anze+1
         abbcm(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 756
      
      658 continue
      do ior = 1,anze+1
         Q_NK(ior) = min(U(ior),Qmx_NK)
      enddo
      ktrans = ktrans+1
      goto 758
      
      660 continue
      do ior = 1,anze+1
         Q_PK(ior) = min(U(ior),Qmx_PK)
      enddo
      ktrans = ktrans+1
      goto 760
      
      662 continue
      do ior = 1,anze+1
         Q_SK(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 762
      
      664 continue
      do ior = 1,anze+1
         Q_NG(ior) = min(U(ior),Qmx_NG)
      enddo
      ktrans = ktrans+1
      goto 764
      
      666 continue
      do ior = 1,anze+1
         Q_PG(ior) = min(U(ior),Qmx_PG)
      enddo
      ktrans = ktrans+1
      goto 766
      
      668 continue
      do ior = 1,anze+1
         Q_NB(ior) = min(U(ior),Qmx_NB)
      enddo
      ktrans = ktrans+1
      goto 768
      
      670 continue
      do ior = 1,anze+1
         Q_PB(ior) = min(U(ior),Qmx_PB)
      enddo
      ktrans = ktrans+1
      goto 770
      
      672 continue
      do ior = 1,anze+1
         CD(1,ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 772
      
      674 continue
      do ior = 1,anze+1
         CD(2,ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 774
      
      676 continue
      do ior = 1,anze+1
         CP(1,ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 776
      
      678 continue
      do ior = 1,anze+1
         CP(2,ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 778
      
      680 continue
      do ior = 1,anze+1
         CM(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 780
      
      682 continue
      do ior = 1,anze+1
         BAC(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 788
      
      690 continue
      do ior = 1,anze+1
         vbsb(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 790
      
      692 continue
      do ior = 1,anze+1
         vcsb(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 792
      
      694 continue
      do ior = 1,anze+1
         CHNF(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 794
      
      696 continue
      do ior = 1,anze+1
         BVHNF(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 796
      
      698 continue
      do ior = 1,anze+1
         zooind(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 798
      
      500 continue
      do ior = 1,anze+1
         abrzo1(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 800
      
      502 continue
      do ior = 1,anze+1
         ssalg(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 802
      
      504 continue
      do ior = 1,anze+1
         ss(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 804
      
      506 continue
      do ior = 1,anze+1
         fssgr(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 806
      
      508 continue
      do ior = 1,anze+1
         fbsgr(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 808
      
      510 continue
      do ior = 1,anze+1
         frfgr(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 810
      
      512 continue
      do ior = 1,anze+1
         nl0(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 812
      
      514 continue
      do ior = 1,anze+1
         pl0(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 814
      
      516 continue
      do ior = 1,anze+1
         stind(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 816
      
      518 continue
      do ior = 1,anze+1
         dlarvn(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 818
      
      520 continue
      do ior = 1,anze+1
         coli(ior) = U(ior)
      enddo
      if (iwsim == 2 .or. iwsim == 5) then
         ktrans = 64
         goto 835
      endif
      
      ktrans = ktrans+1
      goto 820
      
      522 continue
      do ior = 1,anze+1
         mw(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 822
      
      524 continue 
      do ior = 1,anze+1
         pw(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 824
      
      526 continue
      do ior = 1,anze+1
         ca(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 826
      
      528 continue
      do ior = 1,anze+1
         lf(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 828
      
      530 continue
      do ior = 1,anze+1
         vh(ior) = U(ior)
         mue = 1.7e-5*lf(ior)
         if (mue < 0.0)mue = 0.0
         hk = (0.5*sqrt(mue))/(1.+1.4*sqrt(mue))
         vph(ior) = log10(vh(ior))
         vph(ior) = (-1.*vph(ior))+hk
      enddo
      ktrans = ktrans+1
      goto 830
      
      532 continue
      do ior = 1,anze+1
         gesN(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 832
      
      534 continue
      do ior = 1,anze+1
         gesP(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 834
      
      536 continue
      do ior = 1,anze+1
         SKmor(ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 835
      
      537 continue
      do ior = 1,anze+1
         DOSCF(ior) = U(ior)
      enddo
      if (iwsim == 2 .or. iwsim == 5)goto 1811
      ktrans = ktrans+1
      goto 837
      
      539 continue
      do ior = 1, anze+1
         hFluN3(mstr,ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 840
      
      540 continue
      do ior = 1, anze+1
         TGZoo(mstr,ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 860
      
      560 continue
      do ior = 1, anze+1
         akmor_1(mstr,ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 865

      565 continue
      do ior = 1, anze+1
         agmor_1(mstr,ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 870

      570 continue
      do ior = 1, anze+1
         abmor_1(mstr,ior) = U(ior)
      enddo
      if (ischwer == 0) goto 1811
      ktrans = ktrans+1
      goto 842
      
      ! --- Schwermetalle ---
      542 continue
      do ior = 1, anze+1
         hgsZn(mstr,ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 844
      
      544 continue
      do ior = 1, anze+1
         hglZn(mstr,ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 846
      
      546 continue
      do ior = 1, anze+1
         hgsCad(mstr,ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 848

      548 continue
      do ior = 1, anze+1
         hglCad(mstr,ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 850
      
      550 continue
      do ior = 1, anze+1
         hgsCu(mstr,ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 852
      
      552 continue
      do ior = 1, anze+1
         hglCu(mstr,ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 854
      
      554 continue
      do ior = 1, anze+1
         hgsNi(mstr,ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 856
      
      556 continue
      do ior = 1, anze+1
         hglNi(mstr,ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 858
      
      558 continue
      do ior = 1, anze+1
         hgsAs(mstr,ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 880
      580 continue
      do ior = 1, anze+1
         hglAs(mstr,ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 882
      
      582 continue
      do ior = 1, anze+1
         hgsPb(mstr,ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 884
      
      584 continue
      do ior = 1, anze+1
         hglPb(mstr,ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 886
      
      586 continue
      do ior = 1, anze+1
         hgsCr(mstr,ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 890
      
      588 continue
      do ior = 1, anze+1
         hglCr(mstr,ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 892
      
      590 continue
      do ior = 1, anze+1
         hgsFe(mstr,ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 894
      
      592 continue
      do ior = 1, anze+1
         hglFe(mstr,ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 896
      594 continue
      do ior = 1, anze+1
         hgsHg(mstr,ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 898
      
      596 continue
      do ior = 1, anze+1
         hglHg(mstr,ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 900
      
      598 continue
      do ior = 1, anze+1
         hgsMn(mstr,ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 902
      
      400 continue
      do ior = 1, anze+1
         hglMn(mstr,ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 904

      402 continue
      do ior = 1, anze+1
         hgsU(mstr,ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 906

      404 continue
      do ior = 1, anze+1
         hglU(mstr,ior) = U(ior)
      enddo
      goto 1811
      
      ! O2-Transport
      700 continue
      j = 1
      iork = 0
      do ior = 1,anze+1
         U(ior) = vo2(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = vo2(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! NH4-Transport
      702 continue
      j = 1
      iork = 0
      temp0 = hnh4(mstr,1)
      tempn = hnh4(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = vnh4(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = vnh4(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! NO2-Transport
      704  continue
      j = 1
      iork = 0
      temp0 = hno2(mstr,1)
      tempn = hno2(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = vNO2(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = vNO2(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! NO3-Transport
      706 continue
      j = 1
      iork = 0
      temp0 = hno3(mstr,1)
      tempn = hno3(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = vNO3(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = vNO3(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! vx0-Transport
      708 continue
      j = 1
      iork = 0
      temp0 = hx0(mstr,1)
      tempn = hx0(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = vx0(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = vx0(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! vx0-Transport
      710 continue
      j = 1
      iork = 0
      temp0 = hx02(mstr,1)
      tempn = hx02(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = vx02(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = vx02(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! Si-Transport
      712 continue
      j = 1
      iork = 0
      temp0 = hSi(mstr,1)
      tempn = hsi(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = Si(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = Si(ior)
         endif
      enddo
      iorks = iork
      goto 888
      !
      ! gelP-Transport
      714 continue
      j = 1
      iork = 0
      temp0 = hgelP(mstr,1)
      tempn = hgelP(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = gelP(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = gelP(ior)
         endif
      enddo 
      iorks = iork
      goto 888
      
      ! obsb-Transport
      716 continue
      j = 1
      iork = 0
      temp0 = hbsb(mstr,1)
      tempn = hbsb(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = obsb(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = obsb(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! ocsb-Transport
      718 continue
      j = 1
      iork = 0
      temp0 = hcsb(mstr,1)
      tempn = hcsb(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = ocsb(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = ocsb(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! Chla-Transport
      720 continue
      j = 1
      iork = 0
      temp0 = hChla(mstr,1)
      tempn = hChla(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = chla(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = chla(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! Chlaki-Transport
      722 continue
      j = 1
      iork = 0
      temp0 = hChlak(mstr,1)
      tempn = hChlak(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = chlaki(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = chlaki(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! Chlagr-Transport
      724 continue
      j = 1
      iork = 0
      temp0 = hChlag(mstr,1)
      tempn = hChlag(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = chlagr(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = chlagr(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! Chlabl-Transport
      726 continue
      j = 1
      iork = 0
      temp0 = hChlab(mstr,1)
      tempn = hChlab(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = chlabl(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = chlabl(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! aki-Transport
      728 continue
      j = 1
      iork = 0
      temp0 = haki(mstr,1)
      tempn = haki(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = aki(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = aki(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! agr-Transport
      730 continue
      j = 1
      iork = 0
      temp0 = hagr(mstr,1)
      tempn = hagr(mstr,anze+1)
      
      do  ior = 1,anze+1
         U(ior) = agr(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = agr(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! abl-Transport
      732 continue
      j = 1
      iork = 0
      temp0 = habl(mstr,1)
      tempn = habl(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = abl(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = abl(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! vkigr-Transport
      734 continue
      j = 1
      iork = 0
      temp0 = hvkigr(mstr,1)
      tempn = hvkigr(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = vkigr(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = vkigr(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! antbl-Transport
      736 continue
      j = 1
      iork = 0
      temp0 = hantbl(mstr,1)
      tempn = hantbl(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = antbl(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = antbl(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! svhemk-Transport
      738 continue
      j = 1
      iork = 0
      temp0 = svhemk(1)
      tempn = svhemk(anze+1)
      
      do ior = 1,anze+1
         U(ior) = svhemk(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = svhemk(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! svhemg-Transport
      740 continue
      j = 1
      iork = 0
      temp0 = svhemg(1)
      tempn = svhemg(anze+1)
      
      do ior = 1,anze+1
         U(ior) = svhemg(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = svhemg(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! svhemb-Transport
      742 continue
      j = 1
      iork = 0
      temp0 = svhemb(1)
      tempn = svhemb(anze+1)
      
      do ior = 1,anze+1
         U(ior) = svhemb(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = svhemb(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! akbcm-Transport
      744 continue
      j = 1
      iork = 0
      temp0 = akbcm(1)
      tempn = akbcm(anze+1)
      
      do ior = 1,anze+1
         U(ior) = akbcm(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = akbcm(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! agbcm-Transport
      746 continue
      j = 1
      iork = 0
      temp0 = agbcm(1)
      tempn = agbcm(anze+1)
      
      do ior = 1,anze+1
         U(ior) = agbcm(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = agbcm(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! abbcm-Transport
      748 continue
      j = 1
      iork = 0
      temp0 = abbcm(1)
      tempn = abbcm(anze+1)
      
      do ior = 1,anze+1
         U(ior) = abbcm(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = abbcm(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! Q_NK-Transport
      756 continue
      j = 1
      iork = 0
      temp0 = Q_NK(1)
      tempn = Q_NK(anze+1)
      
      do ior = 1,anze+1
         U(ior) = Q_NK(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = Q_NK(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! Q_PK-Transport
      758 continue
      j = 1
      iork = 0
      temp0 = Q_PK(1)
      tempn = Q_PK(anze+1)
      
      do ior = 1,anze+1
         U(ior) = Q_PK(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = Q_PK(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! Q_SK-Transport
      760 continue
      j = 1
      iork = 0
      temp0 = Q_SK(1)
      tempn = Q_SK(anze+1)
      
      do ior = 1,anze+1
         U(ior) = Q_SK(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = Q_SK(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! Q_NG-Transport
      762 continue
      j = 1
      iork = 0
      temp0 = Q_NG(1)
      tempn = Q_NG(anze+1)
      
      do ior = 1,anze+1
         U(ior) = Q_NG(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = Q_NG(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! Q_PG-Transport
      764 continue
      j = 1
      iork = 0
      temp0 = Q_PG(1)
      tempn = Q_PG(anze+1)
      
      do ior = 1,anze+1
         U(ior) = Q_PG(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = Q_PG(ior)
         endif
      enddo
      iorks = iork
      goto 888

      ! Q_NB-Transport
      766 continue
      j = 1
      iork = 0
      temp0 = Q_NB(1)
      tempn = Q_NB(anze+1)
      
      do ior = 1,anze+1
         U(ior) = Q_NB(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = Q_NB(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! Q_PB-Transport
      768 continue
      j = 1
      iork = 0
      temp0 = Q_PB(1)
      tempn = Q_PB(anze+1)
      
      do ior = 1,anze+1
         U(ior) = Q_PB(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = Q_PB(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! CD1-Transport
      770 continue
      j = 1
      iork = 0
      temp0 = hCD(mstr,1,1)
      tempn = hCD(mstr,1,anze+1)
      
      do ior = 1,anze+1
         U(ior) = CD(1,ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = CD(1,ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! CD2-Transport
      772 continue
      j = 1
      iork = 0
      temp0 = hCD(mstr,2,1)
      tempn = hCD(mstr,2,anze+1)
      
      do ior = 1,anze+1
         U(ior) = CD(2,ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = CD(2,ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! CP1-Transport
      774 continue
      j = 1
      iork = 0
      temp0 = hCP(mstr,1,1)
      tempn = hCP(mstr,1,anze+1)
      
      do ior = 1,anze+1
         U(ior) = CP(1,ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = CP(1,ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! CP2-Transport
      776 continue
      j = 1
      iork = 0
      temp0 = hCP(mstr,2,1)
      tempn = hCP(mstr,2,anze+1)
      
      do ior = 1,anze+1
         U(ior) = CP(2,ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = CP(2,ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! CM-Transport
      778 continue
      j = 1
      iork = 0
      temp0 = hCM(mstr,1)
      tempn = hCM(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = CM(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = CM(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! BAC-Transport
      780 continue
      j = 1
      iork = 0
      temp0 = hBAC(mstr,1)
      tempn = hBAC(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = BAC(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = BAC(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      
      ! vbsb-Transport
      788 continue
      j = 1
      iork = 0
      temp0 = hvbsb(mstr,1)
      tempn = hvbsb(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = vbsb(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = vbsb(ior)
         endif
      enddo
      iorks = iork
      goto 888
      !
      ! vcsb-Transport
      790 continue
      j = 1
      iork = 0
      temp0 = hvcsb(mstr,1)
      tempn = hvcsb(mstr,anze+1)
      
      do  ior = 1,anze+1
         U(ior) = vcsb(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = vcsb(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! CHNF-Transport
      792 continue
      j = 1
      iork = 0
      temp0 = hCHNF(mstr,1)
      tempn = hCHNF(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = CHNF(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = CHNF(ior)
         endif
      enddo
      iorks = iork
      goto 888
      !
      ! BVHNF-Transport
      794 continue
      j = 1
      iork = 0
      temp0 = hBVHNF(mstr,1)
      tempn = hBVHNF(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = BVHNF(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = BVHNF(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! Zooind-Transport
      796 continue
      j = 1
      iork = 0
      temp0 = hZooi(mstr,1)
      tempn = hZooi(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = Zooind(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = Zooind(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! abrzo1-Transport
      798 continue
      j = 1
      iork = 0
      temp0 = abrzo1(1)
      tempn = abrzo1(anze+1)
      
      do ior = 1,anze+1
         U(ior) = abrzo1(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = abrzo1(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! ssalg-Transport
      800 continue
      j = 1
      iork = 0
      temp0 = hssalg(mstr,1)
      tempn = hssalg(mstr,anze+1)
      !
      do ior = 1,anze+1
         U(ior) = ssalg(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = ssalg(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! ss-Transport
      802 continue
      j = 1
      iork = 0
      temp0 = hss(mstr,1)
      tempn = hss(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = ss(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = ss(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! fssgr-Transport
      804 continue
      j = 1
      iork = 0
      temp0 = fssgr(1)
      tempn = fssgr(anze+1)
      
      do ior = 1,anze+1
         U(ior) = fssgr(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = fssgr(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! fbsgr-Transport
      806 continue
      j = 1
      iork = 0
      temp0 = fbsgr(1)
      tempn = fbsgr(anze+1)
      
      do ior = 1,anze+1
         U(ior) = fbsgr(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = fbsgr(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! frfgr-Transport
      808 continue
      j = 1
      iork = 0
      temp0 = frfgr(1)
      tempn = frfgr(anze+1)
      
      do ior = 1,anze+1
         U(ior) = frfgr(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = frfgr(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! nl0-Transport
      810 continue
      j = 1
      iork = 0
      temp0 = nl0(1)
      tempn = nl0(anze+1)
      
      do ior = 1,anze+1
         U(ior) = nl0(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = nl0(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! pl0-Transport
      812 continue
      j = 1
      iork = 0
      temp0 = pl0(1)
      tempn = pl0(anze+1)
      
      do ior = 1,anze+1
         U(ior) = pl0(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = pl0(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! stind-Transport
      814 continue
      j = 1
      iork = 0
      temp0 = stind(1)
      tempn = stind(anze+1)
      
      do ior = 1,anze+1
         U(ior) = stind(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = stind(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! dlarvn-Transport
      816 continue
      j = 1
      iork = 0
      temp0 = hdlarn(mstr,1)
      tempn = hdlarn(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = dlarvn(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = dlarvn(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! coli-Transport
      818 continue
      j = 1
      iork = 0
      temp0 = hcoli(mstr,1)
      tempn = hcoli(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = coli(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = coli(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! mw-Transport
      820 continue
      if (iph == 0) then
         ktrans = 61
         goto 830
      endif
      j = 1
      iork = 0
      temp0 = hmw(mstr,1)
      tempn = hmw(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = mw(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = mw(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! pw-Transport
      822 continue
      j = 1
      iork = 0
      temp0 = hpw(mstr,1)
      tempn = hpw(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = pw(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = pw(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! ca-Transport
      824 continue
      j = 1
      iork = 0
      temp0 = hca(mstr,1)
      tempn = hca(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = ca(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = ca(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! lf-Transport
      826 continue
      j = 1
      iork = 0
      temp0 = hlf(mstr,1)
      tempn = hlf(mstr,anze+1)
      
      do ior = 1,anze+1
         ! Berechnung der H+-Konzentration
         mue = 1.7e-5*lf(ior)
         if (mue < 0.0)mue = 0.0
         hk = (0.5*sqrt(mue))/(1.+1.4*sqrt(mue))
         lgh = vph(ior)-hk
         vh(ior) = 10**(-lgh)
         
         U(ior) = lf(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = lf(ior)
         endif
      enddo
      
      mue = 1.7e-5*hlf(mstr,1)
      if (mue < 0.0)mue = 0.0
      hk = (0.5*sqrt(mue))/(1.+1.4*sqrt(mue))
      lgh = hph(mstr,1)-hk
      vhrand = 10**(-lgh)
      
      mue = 1.7e-5*hlf(mstr,anze+1)
      if (mue < 0.0)mue = 0.0
      hk = (0.5*sqrt(mue))/(1.+1.4*sqrt(mue))
      lgh = hph(mstr,anze+1)-hk
      vhrann = 10**(-lgh)
      
      iorks = iork
      goto 888
      
      ! H+-Transport
      828 continue
      j = 1
      iork = 0
      temp0 = vhrand
      tempn = vhrann
      
      do ior = 1,anze+1
         U(ior) = vh(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = vh(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! gesN-Transport
      830 continue
      j = 1
      iork = 0
      temp0 = hgesN(mstr,1)
      tempn = hgesN(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = gesN(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = gesN(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! gesP-Transport
      832 continue
      j = 1
      iork = 0
      temp0 = hgesP(mstr,1)
      tempn = hgesP(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = gesP(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = gesP(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! SKmor-Transport
      834 continue 
      j = 1
      iork = 0
      temp0 = hSKmor(mstr,1)
      tempn = hSKmor(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = SKmor(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = SKmor(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! DOSCF-Transport
      835 continue
      j = 1
      iork = 0
      temp0 = hDOSCF(mstr,1)
      tempn = hDOSCF(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = DOSCF(ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = DOSCF(ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! FluN3-Transport
      837 continue
      j = 1
      iork = 0
      temp0 = hFluN3(mstr,1)
      tempn = hFluN3(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = hFluN3(mstr,ior)
         if (flag(ior) == 4) then
            iork = iork+1
            iore(iork) = ior
            Ue(iore(iork)) = hFluN3(mstr,ior)
         endif
      enddo
      iorks = iork
      goto 888
      
      ! Trockengewicht Zooplankter
      840 continue 
      j = 1
      temp0 = TGZoo(mstr,1)
      tempn = TGZoo(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = TGZoo(mstr,ior)
      enddo
      goto 888
      
      860 continue
      j = 1
      temp0 = akmor_1(mstr,1)
      tempn = akmor_1(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = akmor_1(mstr,ior)
      enddo
      goto 888
      
      865 continue
      j = 1
      temp0 = agmor_1(mstr,1)
      tempn = agmor_1(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = agmor_1(mstr,ior)
      enddo
      goto 888
 
      870 continue 
      j = 1
      temp0 = abmor_1(mstr,1)
      tempn = abmor_1(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = abmor_1(mstr,ior)
      enddo
      goto 888
      
      
      !  --- Schwermetalle ---
      842 continue
      j = 1
      temp0 = hgsZn(mstr,1)
      tempn = hgsZn(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = hgsZn(mstr,ior)
         if (U(1) < 0.0) then
            ktrans = ktrans+1
            goto 844
         endif
      enddo
      goto 888
      
      844 continue
      j = 1
      temp0 = hglZn(mstr,1)
      tempn = hglZn(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = hglZn(mstr,ior)
         if (U(1) < 0.0) then
            ktrans = ktrans+1
            goto 846
         endif
      enddo
      goto 888
      
      846 continue
      j = 1
      temp0 = hgsCad(mstr,1)
      tempn = hgsCad(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = hgsCad(mstr,ior)
         if (U(1) < 0.0) then
            ktrans = ktrans+1
            goto 848
         endif
      enddo
      goto 888
      
      848 continue
      j = 1
      temp0 = hglCad(mstr,1)
      tempn = hglCad(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = hglCad(mstr,ior)
         if (U(1) < 0.0) then
            ktrans = ktrans+1
            goto 850
         endif
      enddo
      goto 888

      850 continue
      j = 1
      temp0 = hgsCu(mstr,1)
      tempn = hgsCu(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = hgsCu(mstr,ior)
         if (U(1) < 0.0) then
            ktrans = ktrans+1
            goto 852
         endif
      enddo
      goto 888
      
      852 continue
      j = 1
      temp0 = hglCu(mstr,1)
      tempn = hglCu(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = hglCu(mstr,ior)
         if (U(1) < 0.0) then
            ktrans = ktrans+1
            goto 854
         endif
      enddo
      goto 888
      854 continue
      j = 1
      temp0 = hgsNi(mstr,1)
      tempn = hgsNi(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = hgsNi(mstr,ior)
         if (U(1) < 0.0) then
            ktrans = ktrans+1
            goto 856
         endif
      enddo
      goto 888
      856 continue
      j = 1
      temp0 = hglNi(mstr,1)
      tempn = hglNi(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = hglNi(mstr,ior)
         if (U(1) < 0.0) then
            ktrans = ktrans+1
            goto 858
         endif
      enddo
      goto 888
      
      858 continue
      j = 1
      temp0 = hgsAs(mstr,1)
      tempn = hgsAs(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = hgsAs(mstr,ior)
         if (U(1) < 0.0) then
            ktrans = ktrans+1
            goto 880
         endif
      enddo
      goto 888
      
      880 continue
      j = 1
      temp0 = hglAs(mstr,1)
      tempn = hglAs(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = hglAs(mstr,ior)
         if (U(1) < 0.0) then
            ktrans = ktrans+1
            goto 882
         endif
      enddo
      goto 888
      
      882 continue
      j = 1
      temp0 = hgsPb(mstr,1)
      tempn = hgsPb(mstr,anze+1)
      do ior = 1,anze+1
         U(ior) = hgsPb(mstr,ior)
         if (U(1) < 0.0) then
            ktrans = ktrans+1
            goto 884
         endif
      enddo
      goto 888
      
      884 continue
      j = 1
      temp0 = hglPb(mstr,1)
      tempn = hglPb(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = hglPb(mstr,ior)
         if (U(1) < 0.0) then
            ktrans = ktrans+1
            goto 886
         endif
      enddo
      goto 888
      
      886 continue
      j = 1
      temp0 = hgsCr(mstr,1)
      tempn = hgsCr(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = hgsCr(mstr,ior)
         if (U(1) < 0.0) then
            ktrans = ktrans+1
            goto 890
         endif
      enddo
      goto 888
      
      890 continue
      j = 1
      temp0 = hglCr(mstr,1)
      tempn = hglCr(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = hglCr(mstr,ior)
         if (U(1) < 0.0) then
            ktrans = ktrans+1
            goto 892
         endif
      enddo
      goto 888
      
      892 continue
      j = 1
      temp0 = hgsFe(mstr,1)
      tempn = hgsFe(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = hgsFe(mstr,ior)
         if (U(1) < 0.0) then
            ktrans = ktrans+1
            goto 894
         endif
      enddo
      goto 888
      
      894 continue
      j = 1
      temp0 = hglFe(mstr,1)
      tempn = hglFe(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = hglFe(mstr,ior)
         if (U(1) < 0.0) then
            ktrans = ktrans+1
            goto 896
         endif
      enddo
      goto 888
      
      896 continue
      j = 1
      temp0 = hgsHg(mstr,1)
      tempn = hgsHg(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = hgsHg(mstr,ior)
         if (U(1) < 0.0) then
            ktrans = ktrans+1
            goto 898
         endif
      enddo
      goto 888
      
      898 continue
      j = 1
      temp0 = hglHg(mstr,1)
      tempn = hglHg(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = hglHg(mstr,ior)
         if (U(1) < 0.0) then
            ktrans = ktrans+1
            goto 900
         endif
      enddo
      goto 888
      
      900 continue
      j = 1
      temp0 = hgsMn(mstr,1)
      tempn = hgsMn(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = hgsMn(mstr,ior)
         if (U(1) < 0.0) then
            ktrans = ktrans+1
            goto 902
         endif
      enddo
      goto 888
      
      902 continue
      j = 1
      temp0 = hglMn(mstr,1)
      tempn = hglMn(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = hglMn(mstr,ior)
         if (U(1) < 0.0) then
            ktrans = ktrans+1
            goto 904
         endif
      enddo
      goto 888

      904 continue
      j = 1
      temp0 = hgsU(mstr,1)
      tempn = hgsU(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = hgsU(mstr,ior)
         if (U(1) < 0.0) then
            ktrans = ktrans+1
            goto 906
         endif
      enddo
      goto 888
      
      906 continue
      j = 1
      temp0 = hglU(mstr,1)
      tempn = hglU(mstr,anze+1)
      
      do ior = 1,anze+1
         U(ior) = hglU(mstr,ior)
         if (U(1) < 0.0) then
            ktrans = ktrans+1
            goto 1811
         endif
      enddo
      goto 888
      
   1811 continue
   
   return
end
