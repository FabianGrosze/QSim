!---------------------------------------------------------------------------------------
!
!   QSim - Programm zur Simulation der Wasserqualität
!
!   Copyright (C) 2020 Bundesanstalt für Gewässerkunde, Koblenz, Deutschland, http://www.bafg.de
!
!   Dieses Programm ist freie Software. Sie können es unter den Bedingungen der 
!   GNU General Public License, Version 3,
!   wie von der Free Software Foundation veröffentlicht, weitergeben und/oder modifizieren. 
!   Die Veröffentlichung dieses Programms erfolgt in der Hoffnung, daß es Ihnen von Nutzen sein wird, 
!   aber OHNE IRGENDEINE GARANTIE, sogar ohne die implizite Garantie der MARKTREIFE oder der VERWENDBARKEIT FÜR EINEN BESTIMMTEN ZWECK. 
!   Details finden Sie in der GNU General Public License.
!   Sie sollten ein Exemplar der GNU General Public License zusammen mit diesem Programm erhalten haben. 
!   Falls nicht, siehe http://www.gnu.org/licenses/.  
!   
!	Programmiert von:
!	1979 bis 2018 Volker Kirchesch
!	seit 2011 Jens Wyrwa, Wyrwa@bafg.de
!
!---------------------------------------------------------------------------------------

      subroutine Transport(anze,deltat,izeits,isub_dt,isub_dt_Mac,hvmitt,elen,flag,tempw,vo2,vnh4,vno3,vno2,vx0       &
                ,vx02,Si,mstr,gelP,obsb,ocsb,vbsb,vcsb,CHNF,BVHNF,CD,CP,CM,BAC,zooind,chla,aki,agr,abl,chlaki,chlagr  &      
                ,chlabl,vkigr,antbl,abrzo1,ssalg,ss,svhemk,svhemg,svhemb,akbcm,agbcm,abbcm,fssgr,fbsgr,frfgr,gesN     &
                ,gesP,nl0,pl0,Q_NK,Q_PK,Q_SK,Q_NG,Q_PG,Q_NB,Q_PB,stind,mw,pw,ca,lf,coli,DOSCF                         &
                ,dlarvn,vph,iph,iwsim,htempw,hgesN,hgesP,hbsb,hcsb,hCHNF,hBVHNF,hCD,hCP,hCM,hBAC,hnh4,ho2             &                                          
                ,hno3,hno2,hx0,hx02,hsi,hchla,haki,hagr,habl,hchlak,hchlag,hchlab,hvkigr,hantbl,hssalg,hss,hzooi      &
                ,hgelp,hmw,hpw,hca,hlf,hph,hdlarn,hcoli,hDOSCF,hvbsb,hvcsb,SKmor,hSKmor,iflRi,dl,Uvert,iMAC           &
                ,iwied,nkzs,tflie,jpoin1,itags,monats,Uhrz,iverfahren,azStrs,ianze_max,Qmx_NK,Qmx_NB,Qmx_NG,Qmx_PK    &
                ,Qmx_PB,Qmx_PG,hFluN3,TGZoo,akmor_1,agmor_1,abmor_1,GRote                                             &
                ,hgsZn,hglZn,hgsCad,hglCad,hgsCu,hglCu,hgsNi,hglNi,mtracer,nkztot_max,ischwer)                                                                                                      
                                                                       
                                                                         
                                                                       
                                                                       
!     AUTOR:VOLKER KIRCHESCH                                            
                                                                       
!     STAND: 08.01.2014                                                  
                                                                       
                                                                       
      integer                                 :: anze, azStrs
      integer, Dimension(azStrs)              :: iflRi, imac, isub_dt, isub_dt_Mac 
      integer, Dimension(1000)                :: iore, flag, nkzs

      real                                    :: mue, lgh
      real, Dimension(1000)                   :: x, elen, vmitt, tempw, vo2, Ue, vnh4, vno2, vno3, vx0, U, vx02, Si
      real, Dimension(1000)                   :: gelP, dl, obsb, ocsb, vbsb, vcsb, CHNF, BVHNF, CM, BAC, zooind, chla
      real, Dimension(1000)                   :: aki, agr, abl, SKmor, chlaki, chlagr, chlabl, vkigr, antbl, abrzo1 
      real, Dimension(1000)                   :: ssalg, ss, svhemk, svhemg, svhemb, akbcm, agbcm, stind, abbcm, fbsgr
      real, Dimension(1000)                   :: fssgr, gesN, gesP, frfgr, nl0, pl0, Q_NK, Q_PK, Q_SK, Q_NG, Q_PG
      real, Dimension(1000)                   :: Q_NB, Q_PB, mw, pw, ca,lf, coli, DOSCF, dlarvn, vph, vh   
      real, Dimension(2,1000)                 :: CD, CP

      real, Dimension(50,1000)                :: Uvert
      real, Dimension(azStrs,1000)            :: hvmitt, hDOSCF, htempw, hgesN, hgesP, hbsb, hcsb, hCHNF, hBVHNF, hBAC
      real, Dimension(azStrs,1000)            :: hCM, hnh4, ho2, hno3, hno2, hx0, hx02, hsi, hchla, haki, hagr, habl
      real, Dimension(azStrs,1000)            :: hchlak, hchlag, hchlab, hvkigr, hantbl, hssalg, hss, hzooi, hgelp 
      real, Dimension(azStrs,1000)            :: hmw, hpw, hca, hlf, hph, hdlarn, hcoli, hvbsb, hvcsb, hSKmor,hFluN3
      real, Dimension(azStrs,1000)            :: TGZoo, akmor_1, agmor_1, abmor_1 
      real, Dimension(azStrs,1000)            :: hgsZn, hglZn, hgsCad, hglCad, hgsCu, hglCu, hgsNi, hglNi
      real, Dimension(azStrs,2,1000)          :: hCD, hCP   
 
                                                                       
!                                                                       
      iwahlD = 1 
      nkz = 1 
      j = 1 
      iork = 0 
      sumdet = 0.0
      isub_dtx = isub_dt(mstr)
      if(imac(mstr)==1)isub_dtx = isub_dt_Mac(mstr)  

      kktrans = 77 !! nötig??? kktrans = 69
      if(ischwer==1)kktrans = 77

      do 1811 itime = 1,izeits 
      ktrans = 1 
      jpoin1 = 0 
                                                                       
      do ior = 1,anze+1 
        U(ior) = tempw(ior)
        vmitt(ior) = hvmitt(mstr,ior)
        if(nkzs(ior)==1)Uvert(1,ior) = vmitt(ior)
      enddo 
                                                                       
      temp0 = htempw(mstr,1) 
      tempn = htempw(mstr,anze+1) 

                                                                       
  888 continue 
!      if(iflRi(mstr).eq.0)goto 911

     if(U(1)<0.0.and.ktrans/=1.and.ktrans/=57)goto 911

       !print*,"Transport: ktrans,anze,ianze_max,azStrs,kktrans,iverfahren=",ktrans,anze,ianze_max,azStrs,kktrans,iverfahren

      call AdvDiff(anze,elen,vmitt,Uvert,dl,flag,ktrans,U,temp0,tempn                                                           &
                  ,deltat,sumdet,itime,izeits,mstr,iwied,iwahlD,nkz,nkzs,tflie,iFlRi                                            &
                  ,jpoin1,itags,monats,isub_dtx,imac,iverfahren,azStrs,kktrans,nkztot_max,ianze_max,mtracer,iwsim,uhrz)                               
                                                                       
  911 goto (600,602,604,606,608,610,612,614,616,618,620                      &
           ,622,624,626,628,630,632,634,636,638,640,642,644                  &
           ,646,648,650,658,660,662,664,666                                  &
           ,668,670,672,674,676,678,680,682                                  &
           ,690,692,694,696,698,500,502,504,506,508                          &
           ,510,512,514,516,518,520,522,524,526,528                          &
           ,530,532,534,536,537,539,540,560,565,570                          &
           ,542,544,546,548,550,552,554,556)ktrans                                       
                                                                       
  600 do ior = 1,anze+1 
        tempw(ior) = U(ior) 
      enddo 
      ktrans = ktrans+1
      jpoin1 = 1
      if(iwsim==5)goto 1811 
      if(iwsim==4)goto 1811 
      if(iwsim==2.and.coli(1)>=0.0)then
        ktrans=55
        goto 818
          else if(iwsim==2.and.coli(1)<0.0)then
            goto 1811
      endif

      goto 700 
                                                                       
  602 do ior = 1,anze+1 
        vo2(ior) = U(ior) 
      enddo 
      ktrans = ktrans+1 
      goto 702 
                                                                       
  604 do ior = 1,anze+1 
        vNH4(ior) = U(ior) 
      enddo 
      ktrans = ktrans+1 
      goto 704 
                                                                       
  606 do ior = 1,anze+1 
        vNO2(ior) = U(ior) 
      enddo 
      ktrans = ktrans+1 
      goto 706 
                                                                       
  608 do ior = 1,anze+1 
        vNO3(ior) = U(ior) 
      enddo 
      ktrans = ktrans+1 
      goto 708 
                                                                       
  610 do ior = 1,anze+1 
        vx0(ior) = U(ior) 
      enddo 
      ktrans = ktrans+1 
      goto 710 
                                                                       
  612 do ior = 1,anze+1 
        vx02(ior) = U(ior) 
      enddo 
      ktrans = ktrans+1 
      goto 712 
                                                                       
  614 do ior = 1,anze+1 
        Si(ior) = U(ior) 
      enddo 
      ktrans = ktrans+1 
      goto 714 
                                                                       
  616 do ior = 1,anze+1 
        gelP(ior) = U(ior) 
      enddo 
      ktrans = ktrans+1 
      goto 716 
!                                                                       
  618 do 128 ior = 1,anze+1 
      obsb(ior) = U(ior) 
  128 continue 
      ktrans = ktrans+1 
      goto 718 
!                                                                       
  620 do 130 ior = 1,anze+1 
      ocsb(ior) = U(ior) 
  130 continue 
      ktrans = ktrans+1 
      goto 720 
!                                                                       
  622 if(iflri(mstr)/=0)then 
        do ior = 1,anze+1 
          chla(ior) = U(ior) 
        enddo
      endif  
      ktrans = ktrans+1 
      goto 722 
                                                                       
  624 if(iflri(mstr)/=0)then 
        do ior = 1,anze+1 
          chlaki(ior) = U(ior) 
        enddo
      endif
      ktrans = ktrans+1 
      goto 724 
                                                                       
  626 do 136 ior = 1,anze+1 
      chlagr(ior) = U(ior) 
  136 continue 
      ktrans = ktrans+1 
      goto 726 
!                                                                       
  628 do 138 ior = 1,anze+1 
      chlabl(ior) = U(ior) 
  138 continue 
      ktrans = ktrans+1 
      goto 728 
!                                                                       
  630 do 140 ior = 1,anze+1 
      aki(ior) = U(ior) 
  140 continue 
      ktrans = ktrans+1 
      goto 730 
!                                                                       
  632 do 142 ior = 1,anze+1 
      agr(ior) = U(ior) 
  142 continue 
      ktrans = ktrans+1 
      goto 732 
!                                                                       
  634 do 144 ior = 1,anze+1 
      abl(ior) = U(ior) 
  144 continue 
      ktrans = ktrans+1 
      goto 734 
                                                                       
  636 if(iflri(mstr)/=0)then
        do ior = 1,anze+1 
          vkigr(ior) = U(ior) 
        enddo
      endif
      ktrans = ktrans+1 
      goto 736
                                                                       
  638 if(iflri(mstr)/=0)then 
        do ior = 1,anze+1 
          antbl(ior) = U(ior)
        enddo
      endif 
      ktrans = ktrans+1 
      goto 738 
                                                                       
  640 do 150 ior = 1,anze+1 
      svhemk(ior) = U(ior) 
  150 continue 
      ktrans = ktrans+1 
      goto 740 
!                                                                       
  642 do 152 ior = 1,anze+1 
      svhemg(ior) = U(ior) 
  152 continue 
      ktrans = ktrans+1 
      goto 742 
!                                                                       
  644 do 154 ior = 1,anze+1 
      svhemb(ior) = U(ior) 
  154 continue 
      ktrans = ktrans+1 
      goto 744 
!                                                                       
  646 do 156 ior = 1,anze+1 
      akbcm(ior) = U(ior) 
  156 continue 
      ktrans = ktrans+1 
      goto 746 
!                                                                       
  648 do 158 ior = 1,anze+1 
      agbcm(ior) = U(ior) 
  158 continue 
      ktrans = ktrans+1 
      goto 748 
!                                                                       
  650 do 160 ior = 1,anze+1 
      abbcm(ior) = U(ior) 
  160 continue 
      ktrans = ktrans+1 
      goto 756
                                                                       
  658 do 168 ior = 1,anze+1 
      Q_NK(ior) = min(U(ior),Qmx_NK) 
  168 continue 
      ktrans = ktrans+1 
      goto 758 
!                                                                       
  660 do 170 ior = 1,anze+1 
      Q_PK(ior) = min(U(ior),Qmx_PK)
       
  170 continue 
      ktrans = ktrans+1 
      goto 760 
!                                                                       
  662 do 172 ior = 1,anze+1 
      Q_SK(ior) = U(ior) 
  172 continue 
      ktrans = ktrans+1 
      goto 762 
!                                                                       
  664 do 174 ior = 1,anze+1 
      Q_NG(ior) = min(U(ior),Qmx_NG) 
  174 continue 
      ktrans = ktrans+1 
      goto 764 
!                                                                       
  666 do 176 ior = 1,anze+1 
      Q_PG(ior) = min(U(ior),Qmx_PG) 
  176 continue 
      ktrans = ktrans+1 
      goto 766 
!                                                                       
  668 do 178 ior = 1,anze+1 
      Q_NB(ior) = min(U(ior),Qmx_NB) 
  178 continue 
      ktrans = ktrans+1 
      goto 768 
!                                                                       
  670 do 180 ior = 1,anze+1 
      Q_PB(ior) = min(U(ior),Qmx_PB) 
  180 continue 
      ktrans = ktrans+1 
      goto 770 
!                                                                       
  672 do 182 ior = 1,anze+1 
      CD(1,ior) = U(ior) 
  182 continue 
      ktrans = ktrans+1 
      goto 772 
!                                                                       
  674 do 184 ior = 1,anze+1 
      CD(2,ior) = U(ior) 
  184 continue 
      ktrans = ktrans+1 
      goto 774 
!                                                                       
  676 do 186 ior = 1,anze+1 
      CP(1,ior) = U(ior) 
  186 continue 
      ktrans = ktrans+1 
      goto 776 
!                                                                       
  678 do 188 ior = 1,anze+1 
      CP(2,ior) = U(ior) 
  188 continue 
      ktrans = ktrans+1 
      goto 778 
!                                                                       
  680 do 190 ior = 1,anze+1 
      CM(ior) = U(ior) 
  190 continue 
      ktrans = ktrans+1 
      goto 780 
!                                                                       
  682 do 192 ior = 1,anze+1 
      BAC(ior) = U(ior) 
  192 continue 
      ktrans = ktrans+1 
      goto 788 
!                                                                       
  690 do 200 ior = 1,anze+1 
      vbsb(ior) = U(ior) 
  200 continue 
      ktrans = ktrans+1 
      goto 790 
!                                                                       
  692 do 202 ior = 1,anze+1 
      vcsb(ior) = U(ior) 
  202 continue 
      ktrans = ktrans+1 
      goto 792 
!                                                                       
  694 do 204 ior = 1,anze+1 
      CHNF(ior) = U(ior) 
  204 continue 
      ktrans = ktrans+1 
      goto 794 
!                                                                       
  696 do 206 ior = 1,anze+1 
      BVHNF(ior) = U(ior) 
  206 continue 
      ktrans = ktrans+1 
      goto 796 
!                                                                       
  698 do 208 ior = 1,anze+1 
      zooind(ior) = U(ior) 
  208 continue 
      ktrans = ktrans+1 
      goto 798 
!                                                                       
  500 do 210 ior = 1,anze+1 
      abrzo1(ior) = U(ior) 
  210 continue 
      ktrans = ktrans+1 
      goto 800 
!                                                                       
  502 do 212 ior = 1,anze+1 
      ssalg(ior) = U(ior) 
  212 continue 
      ktrans = ktrans+1 
      goto 802 
!                                                                       
  504 do 214 ior = 1,anze+1 
      ss(ior) = U(ior) 
  214 continue 
      ktrans = ktrans+1 
      goto 804 
!                                                                       
  506 do 216 ior = 1,anze+1 
      fssgr(ior) = U(ior) 
  216 continue 
      ktrans = ktrans+1 
      goto 806 
!                                                                       
  508 do 218 ior = 1,anze+1 
      fbsgr(ior) = U(ior) 
  218 continue 
      ktrans = ktrans+1 
      goto 808 
!                                                                       
  510 do 220 ior = 1,anze+1 
      frfgr(ior) = U(ior) 
  220 continue 
      ktrans = ktrans+1 
      goto 810 
!                                                                       
  512 do 222 ior = 1,anze+1 
      nl0(ior) = U(ior) 
  222 continue 
      ktrans = ktrans+1 
      goto 812 
!                                                                       
  514 do 224 ior = 1,anze+1 
      pl0(ior) = U(ior) 
  224 continue 
      ktrans = ktrans+1 
      goto 814 
!                                                                       
  516 do 226 ior = 1,anze+1 
      stind(ior) = U(ior) 
  226 continue 
      ktrans = ktrans+1 
      goto 816 
!                                                                       
  518 do 228 ior = 1,anze+1 
      dlarvn(ior) = U(ior) 
  228 continue 
      ktrans = ktrans+1 
      goto 818 
!                                                                       
  520 do 230 ior = 1,anze+1 
      coli(ior) = U(ior) 
  230 continue
      if(iwsim==2.or.iwsim==5)then
        ktrans=64
        goto 835
      endif
 
      ktrans = ktrans+1 
      goto 820 
!                                                                       
  522 do 232 ior = 1,anze+1 
      mw(ior) = U(ior) 
  232 continue 
      ktrans = ktrans+1 
      goto 822 
!                                                                       
  524 do 234 ior = 1,anze+1 
      pw(ior) = U(ior) 
  234 continue 
      ktrans = ktrans+1 
      goto 824 
!                                                                       
  526 do 236 ior = 1,anze+1 
      ca(ior) = U(ior) 
  236 continue 
      ktrans = ktrans+1 
      goto 826 
!                                                                       
  528 do 238 ior = 1,anze+1 
      lf(ior) = U(ior) 
  238 continue 
      ktrans = ktrans+1 
      goto 828 
!                                                                       
  530 do 240 ior = 1,anze+1 
      vh(ior) = U(ior) 
      mue = 1.7e-5*lf(ior) 
      if(mue.lt.0.0)mue = 0.0 
      hk = (0.5*sqrt(mue))/(1.+1.4*sqrt(mue)) 
      vph(ior) = log10(vh(ior))
      vph(ior) = (-1.*vph(ior))+hk 
  240 continue 
      ktrans = ktrans+1 
      goto 830 
!                                                                       
  532 do 242 ior = 1,anze+1 
      gesN(ior) = U(ior) 
  242 continue 
      ktrans = ktrans+1 
      goto 832 
!                                                                       
  534 do 244 ior = 1,anze+1 
      gesP(ior) = U(ior) 
  244 continue 
      ktrans = ktrans+1 
      goto 834 
!                                                                       
  536 do 246 ior = 1,anze+1 
      SKmor(ior) = U(ior) 
  246 continue 
      ktrans = ktrans+1 
      goto 835 
!                                                                       
  537 do 247 ior = 1,anze+1 
      DOSCF(ior) = U(ior) 
  247 continue 
      if(iwsim==2.or.iwsi==5)goto 1811
      ktrans = ktrans+1
      goto 837

  539 do ior = 1, anze+1
        hFluN3(mstr,ior) = U(ior)
      enddo
       ktrans = ktrans+1
       goto 840

  540 do ior = 1, anze+1
        TGZoo(mstr,ior) = U(ior)
      enddo
       ktrans = ktrans+1
       goto 860

  560 do ior = 1, anze+1
        akmor_1(mstr,ior) = U(ior)
      enddo
       ktrans = ktrans+1
       goto 865

  565 do ior = 1, anze+1
        agmor_1(mstr,ior) = U(ior)
      enddo
       ktrans = ktrans+1
       goto 870

  570 do ior = 1, anze+1
        abmor_1(mstr,ior) = U(ior)
      enddo
       ktrans = ktrans+1
       goto 842
                                                                        
! ##### Schwermetalle #####

  542 do ior = 1, anze+1
        hgsZn(mstr,ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 844  

  544 do ior = 1, anze+1
        hglZn(mstr,ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 846  

  546 do ior = 1, anze+1
        hgsCad(mstr,ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 848  

  548 do ior = 1, anze+1
        hglCad(mstr,ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 850  

  550 do ior = 1, anze+1
        hgsCu(mstr,ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 852  

  552 do ior = 1, anze+1
        hglCu(mstr,ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 854  

  554 do ior = 1, anze+1
        hgsNi(mstr,ior) = U(ior)
      enddo
      ktrans = ktrans+1
      goto 856 

  556 do ior = 1, anze+1
        hglNi(mstr,ior) = U(ior)
      enddo

      goto 1811 
                                                                       
                                                                       
!.....O2-Transport                                                      
  700 j = 1 
      iork = 0 
                                                                       
      do 301 ior = 1,anze+1 
      U(ior) = vo2(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = vo2(ior) 
      endif 
  301 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....NH4-Transport                                                     
  702 j = 1 
      iork = 0 
      temp0 = hnh4(mstr,1) 
      tempn = hnh4(mstr,anze+1) 
!                                                                       
      do 303 ior = 1,anze+1 
      U(ior) = vnh4(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = vnh4(ior) 
      endif 
  303 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....NO2-Transport                                                     
  704 j = 1 
      iork = 0 
      temp0 = hno2(mstr,1) 
      tempn = hno2(mstr,anze+1) 
!                                                                       
      do 305 ior = 1,anze+1 
      U(ior) = vNO2(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = vNO2(ior) 
      endif 
  305 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....NO3-Transport                                                     
  706 j = 1 
      iork = 0 
      temp0 = hno3(mstr,1) 
      tempn = hno3(mstr,anze+1) 
!                                                                       
      do 307 ior = 1,anze+1 
      U(ior) = vNO3(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = vNO3(ior) 
      endif 
  307 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....vx0-Transport                                                     
  708 j = 1 
      iork = 0 
      temp0 = hx0(mstr,1) 
      tempn = hx0(mstr,anze+1) 
!                                                                       
      do 309 ior = 1,anze+1 
      U(ior) = vx0(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = vx0(ior) 
      endif 
  309 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....vx0-Transport                                                     
  710 j = 1 
      iork = 0 
      temp0 = hx02(mstr,1) 
      tempn = hx02(mstr,anze+1) 
!                                                                       
      do 311 ior = 1,anze+1 
      U(ior) = vx02(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = vx02(ior) 
      endif 
  311 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....Si-Transport                                                      
  712 j = 1 
      iork = 0 
      temp0 = hSi(mstr,1) 
      tempn = hsi(mstr,anze+1) 
!                                                                       
      do 313 ior = 1,anze+1 
      U(ior) = Si(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = Si(ior) 
      endif 
  313 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....gelP-Transport                                                    
  714 j = 1 
      iork = 0 
      temp0 = hgelP(mstr,1) 
      tempn = hgelP(mstr,anze+1) 
!                                                                       
      do 315 ior = 1,anze+1 
      U(ior) = gelP(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = gelP(ior) 
      endif 
  315 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....obsb-Transport                                                    
  716 j = 1 
      iork = 0 
      temp0 = hbsb(mstr,1) 
      tempn = hbsb(mstr,anze+1) 
!                                                                       
      do 317 ior = 1,anze+1 
      U(ior) = obsb(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = obsb(ior) 
      endif 
  317 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....ocsb-Transport                                                    
  718 j = 1 
      iork = 0 
      temp0 = hcsb(mstr,1) 
      tempn = hcsb(mstr,anze+1) 
!                                                                       
      do 319 ior = 1,anze+1 
      U(ior) = ocsb(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = ocsb(ior) 
      endif 
  319 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....Chla-Transport                                                    
  720 j = 1 
      iork = 0 
      temp0 = hChla(mstr,1) 
      tempn = hChla(mstr,anze+1) 
!                                                                       
      do 321 ior = 1,anze+1 
      U(ior) = chla(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = chla(ior) 
      endif 
  321 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....Chlaki-Transport                                                  
  722 j = 1 
      iork = 0 
      temp0 = hChlak(mstr,1) 
      tempn = hChlak(mstr,anze+1) 
!                                                                       
      do 323 ior = 1,anze+1 
      U(ior) = chlaki(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = chlaki(ior) 
      endif 
  323 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....Chlagr-Transport                                                  
  724 j = 1 
      iork = 0 
      temp0 = hChlag(mstr,1) 
      tempn = hChlag(mstr,anze+1) 
!                                                                       
      do 325 ior = 1,anze+1 
      U(ior) = chlagr(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = chlagr(ior) 
      endif 
  325 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....Chlabl-Transport                                                  
  726 j = 1 
      iork = 0 
      temp0 = hChlab(mstr,1) 
      tempn = hChlab(mstr,anze+1) 
!                                                                       
      do 327 ior = 1,anze+1 
      U(ior) = chlabl(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = chlabl(ior) 
      endif 
  327 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....aki-Transport                                                     
  728 j = 1 
      iork = 0 
      temp0 = haki(mstr,1) 
      tempn = haki(mstr,anze+1) 
!                                                                       
      do 329 ior = 1,anze+1 
      U(ior) = aki(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = aki(ior) 
      endif 
  329 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....agr-Transport                                                     
  730 j = 1 
      iork = 0 
      temp0 = hagr(mstr,1) 
      tempn = hagr(mstr,anze+1) 
!                                                                       
      do 331 ior = 1,anze+1 
      U(ior) = agr(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = agr(ior) 
      endif 
  331 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....abl-Transport                                                     
  732 j = 1 
      iork = 0 
      temp0 = habl(mstr,1) 
      tempn = habl(mstr,anze+1) 
!                                                                       
      do 333 ior = 1,anze+1 
      U(ior) = abl(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = abl(ior) 
      endif 
  333 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....vkigr-Transport                                                   
  734 j = 1 
      iork = 0 
      temp0 = hvkigr(mstr,1) 
      tempn = hvkigr(mstr,anze+1) 
!                                                                       
      do 335 ior = 1,anze+1 
      U(ior) = vkigr(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = vkigr(ior) 
      endif 
  335 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....antbl-Transport                                                   
  736 j = 1 
      iork = 0 
      temp0 = hantbl(mstr,1) 
      tempn = hantbl(mstr,anze+1) 
!                                                                       
      do 337 ior = 1,anze+1 
      U(ior) = antbl(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = antbl(ior) 
      endif 
  337 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....svhemk-Transport                                                  
  738 j = 1 
      iork = 0 
      temp0 = svhemk(1) 
      tempn = svhemk(anze+1) 
!                                                                       
      do 339 ior = 1,anze+1 
      U(ior) = svhemk(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = svhemk(ior) 
      endif 
  339 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....svhemg-Transport                                                  
  740 j = 1 
      iork = 0 
      temp0 = svhemg(1) 
      tempn = svhemg(anze+1) 
!                                                                       
      do 341 ior = 1,anze+1 
      U(ior) = svhemg(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = svhemg(ior) 
      endif 
  341 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....svhemb-Transport                                                  
  742 j = 1 
      iork = 0 
      temp0 = svhemb(1) 
      tempn = svhemb(anze+1) 
!                                                                       
      do 343 ior = 1,anze+1 
      U(ior) = svhemb(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = svhemb(ior) 
      endif 
  343 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....akbcm-Transport                                                   
  744 j = 1 
      iork = 0 
      temp0 = akbcm(1) 
      tempn = akbcm(anze+1) 
!                                                                       
      do 345 ior = 1,anze+1 
      U(ior) = akbcm(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = akbcm(ior) 
      endif 
  345 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....agbcm-Transport                                                   
  746 j = 1 
      iork = 0 
      temp0 = agbcm(1) 
      tempn = agbcm(anze+1) 
!                                                                       
      do 347 ior = 1,anze+1 
      U(ior) = agbcm(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = agbcm(ior) 
      endif 
  347 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....abbcm-Transport                                                   
  748 j = 1 
      iork = 0 
      temp0 = abbcm(1) 
      tempn = abbcm(anze+1) 
!                                                                       
      do 349 ior = 1,anze+1 
      U(ior) = abbcm(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = abbcm(ior) 
      endif 
  349 continue 
      iorks = iork 
      goto 888 
                                                                       
!.....Q_NK-Transport                                                    
  756 j = 1 
      iork = 0 
      temp0 = Q_NK(1) 
      tempn = Q_NK(anze+1) 
!                                                                       
      do 357 ior = 1,anze+1 
      U(ior) = Q_NK(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = Q_NK(ior) 
      endif 
  357 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....Q_PK-Transport                                                    
  758 j = 1 
      iork = 0 
      temp0 = Q_PK(1)
      tempn = Q_PK(anze+1) 
!                                                                       
      do 359 ior = 1,anze+1 
      U(ior) = Q_PK(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = Q_PK(ior) 
      endif 
  359 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....Q_SK-Transport                                                    
  760 j = 1 
      iork = 0 
      temp0 = Q_SK(1) 
      tempn = Q_SK(anze+1) 
!                                                                       
      do 361 ior = 1,anze+1 
      U(ior) = Q_SK(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = Q_SK(ior) 
      endif 
  361 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....Q_NG-Transport                                                    
  762 j = 1 
      iork = 0 
      temp0 = Q_NG(1) 
      tempn = Q_NG(anze+1) 
!                                                                       
      do 363 ior = 1,anze+1 
      U(ior) = Q_NG(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = Q_NG(ior) 
      endif 
  363 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....Q_PG-Transport                                                    
  764 j = 1 
      iork = 0 
      temp0 = Q_PG(1) 
      tempn = Q_PG(anze+1) 
!                                                                       
      do 365 ior = 1,anze+1 
      U(ior) = Q_PG(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = Q_PG(ior) 
      endif 
  365 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....Q_NB-Transport                                                    
  766 j = 1 
      iork = 0 
      temp0 = Q_NB(1) 
      tempn = Q_NB(anze+1) 
!                                                                       
      do 367 ior = 1,anze+1 
      U(ior) = Q_NB(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = Q_NB(ior) 
      endif 
  367 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....Q_PB-Transport                                                    
  768 j = 1 
      iork = 0 
      temp0 = Q_PB(1) 
      tempn = Q_PB(anze+1) 
!                                                                       
      do 369 ior = 1,anze+1 
      U(ior) = Q_PB(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = Q_PB(ior) 
      endif 
  369 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....CD1-Transport                                                     
  770 j = 1 
      iork = 0 
      temp0 = hCD(mstr,1,1) 
      tempn = hCD(mstr,1,anze+1) 
!                                                                       
      do 371 ior = 1,anze+1 
      U(ior) = CD(1,ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = CD(1,ior) 
      endif 
  371 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....CD2-Transport                                                     
  772 j = 1 
      iork = 0 
      temp0 = hCD(mstr,2,1) 
      tempn = hCD(mstr,2,anze+1) 
!                                                                       
      do 373 ior = 1,anze+1 
      U(ior) = CD(2,ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = CD(2,ior) 
      endif 
  373 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....CP1-Transport                                                     
  774 j = 1 
      iork = 0 
      temp0 = hCP(mstr,1,1) 
      tempn = hCP(mstr,1,anze+1) 
!                                                                       
      do 375 ior = 1,anze+1 
      U(ior) = CP(1,ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = CP(1,ior) 
      endif 
  375 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....CP2-Transport                                                     
  776 j = 1 
      iork = 0 
      temp0 = hCP(mstr,2,1) 
      tempn = hCP(mstr,2,anze+1) 
!                                                                       
      do 377 ior = 1,anze+1 
      U(ior) = CP(2,ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = CP(2,ior) 
      endif 
  377 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....CM-Transport                                                      
  778 j = 1 
      iork = 0 
      temp0 = hCM(mstr,1) 
      tempn = hCM(mstr,anze+1) 
!                                                                       
      do 379 ior = 1,anze+1 
      U(ior) = CM(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = CM(ior) 
      endif 
  379 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....BAC-Transport                                                     
  780 j = 1 
      iork = 0 
      temp0 = hBAC(mstr,1) 
      tempn = hBAC(mstr,anze+1) 
!                                                                       
      do 381 ior = 1,anze+1 
      U(ior) = BAC(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = BAC(ior) 
      endif 
  381 continue 
      iorks = iork 
      goto 888 
                                                                       
                                                                     
!.....vbsb-Transport                                                    
  788 j = 1 
      iork = 0 
      temp0 = hvbsb(mstr,1) 
      tempn = hvbsb(mstr,anze+1) 
!                                                                       
      do 389 ior = 1,anze+1 
      U(ior) = vbsb(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = vbsb(ior) 
      endif 
  389 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....vcsb-Transport                                                    
  790 j = 1 
      iork = 0 
      temp0 = hvcsb(mstr,1) 
      tempn = hvcsb(mstr,anze+1) 
!                                                                       
      do 391 ior = 1,anze+1 
      U(ior) = vcsb(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = vcsb(ior) 
      endif 
  391 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....CHNF-Transport                                                    
  792 j = 1 
      iork = 0 
      temp0 = hCHNF(mstr,1) 
      tempn = hCHNF(mstr,anze+1) 
!                                                                       
      do 393 ior = 1,anze+1 
      U(ior) = CHNF(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = CHNF(ior) 
      endif 
  393 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....BVHNF-Transport                                                   
  794 j = 1 
      iork = 0 
      temp0 = hBVHNF(mstr,1) 
      tempn = hBVHNF(mstr,anze+1) 
!                                                                       
      do 395 ior = 1,anze+1 
      U(ior) = BVHNF(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = BVHNF(ior) 
      endif 
  395 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....Zooind-Transport                                                  
  796 j = 1 
      iork = 0 
      temp0 = hZooi(mstr,1) 
      tempn = hZooi(mstr,anze+1) 
!                                                                       
      do 397 ior = 1,anze+1 
      U(ior) = Zooind(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = Zooind(ior) 
      endif 
  397 continue 

      iorks = iork 
      goto 888 
                                                                       
!.....abrzo1-Transport                                                  
  798 j = 1 
      iork = 0 
      temp0 = abrzo1(1) 
      tempn = abrzo1(anze+1) 
!                                                                       
      do 399 ior = 1,anze+1 
      U(ior) = abrzo1(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = abrzo1(ior) 
      endif 
  399 continue 

      iorks = iork 
      goto 888 
!                                                                       
!.....ssalg-Transport                                                   
  800 j = 1 
      iork = 0 
      temp0 = hssalg(mstr,1) 
      tempn = hssalg(mstr,anze+1) 
!                                                                       
      do 401 ior = 1,anze+1 
      U(ior) = ssalg(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = ssalg(ior) 
      endif 
  401 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....ss-Transport                                                      
  802 j = 1 
      iork = 0 
      temp0 = hss(mstr,1) 
      tempn = hss(mstr,anze+1) 
!                                                                       
      do 403 ior = 1,anze+1 
      U(ior) = ss(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = ss(ior) 
      endif 
  403 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....fssgr-Transport                                                   
  804 j = 1 
      iork = 0 
      temp0 = fssgr(1) 
      tempn = fssgr(anze+1) 
!                                                                       
      do 405 ior = 1,anze+1 
      U(ior) = fssgr(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = fssgr(ior) 
      endif 
  405 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....fbsgr-Transport                                                   
  806 j = 1 
      iork = 0 
      temp0 = fbsgr(1) 
      tempn = fbsgr(anze+1) 
!                                                                       
      do 407 ior = 1,anze+1 
      U(ior) = fbsgr(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = fbsgr(ior) 
      endif 
  407 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....frfgr-Transport                                                   
  808 j = 1 
      iork = 0 
      temp0 = frfgr(1) 
      tempn = frfgr(anze+1) 
!                                                                       
      do 409 ior = 1,anze+1 
      U(ior) = frfgr(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = frfgr(ior) 
      endif 
  409 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....nl0-Transport                                                     
  810 j = 1 
      iork = 0 
      temp0 = nl0(1) 
      tempn = nl0(anze+1) 
!                                                                       
      do 411 ior = 1,anze+1 
      U(ior) = nl0(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = nl0(ior) 
      endif 
  411 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....pl0-Transport                                                     
  812 j = 1 
      iork = 0 
      temp0 = pl0(1) 
      tempn = pl0(anze+1) 
!                                                                       
      do 413 ior = 1,anze+1 
      U(ior) = pl0(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = pl0(ior) 
      endif 
  413 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....stind-Transport                                                   
  814 j = 1 
      iork = 0 
      temp0 = stind(1) 
      tempn = stind(anze+1) 
!                                                                       
      do 415 ior = 1,anze+1 
      U(ior) = stind(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = stind(ior) 
      endif 
  415 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....dlarvn-Transport                                                  
  816 j = 1 
      iork = 0 
      temp0 = hdlarn(mstr,1) 
      tempn = hdlarn(mstr,anze+1) 
!                                                                       
      do 417 ior = 1,anze+1 
      U(ior) = dlarvn(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = dlarvn(ior) 
      endif 
  417 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....coli-Transport                                                    
  818 j = 1 
      iork = 0 
      temp0 = hcoli(mstr,1) 
      tempn = hcoli(mstr,anze+1) 
!                                                                       
      do 419 ior = 1,anze+1 
      U(ior) = coli(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = coli(ior) 
      endif 
  419 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....mw-Transport                                                      
  820 if(iph.eq.0)then 
      ktrans = 61 
      goto 830 
      endif 
      j = 1 
      iork = 0 
      temp0 = hmw(mstr,1) 
      tempn = hmw(mstr,anze+1) 
!                                                                       
      do 421 ior = 1,anze+1 
      U(ior) = mw(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = mw(ior) 
      endif 
  421 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....pw-Transport                                                      
  822 j = 1 
      iork = 0 
      temp0 = hpw(mstr,1) 
      tempn = hpw(mstr,anze+1) 
!                                                                       
      do 423 ior = 1,anze+1 
      U(ior) = pw(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = pw(ior) 
      endif 
  423 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....ca-Transport                                                      
  824 j = 1 
      iork = 0 
      temp0 = hca(mstr,1) 
      tempn = hca(mstr,anze+1) 
!                                                                       
      do 425 ior = 1,anze+1 
      U(ior) = ca(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = ca(ior) 
      endif 
  425 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....lf-Transport                                                      
  826 j = 1 
      iork = 0 
      temp0 = hlf(mstr,1) 
      tempn = hlf(mstr,anze+1) 
!                                                                       
      do 427 ior = 1,anze+1 
!...Berechnung der H+-Konzentration                                     
!                                                                       
      mue = 1.7e-5*lf(ior) 
      if(mue.lt.0.0)mue = 0.0 
      hk = (0.5*sqrt(mue))/(1.+1.4*sqrt(mue)) 
      lgh = vph(ior)-hk 
      vh(ior) = 10**(-lgh) 
                                                                       
      U(ior) = lf(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = lf(ior) 
      endif 
  427 continue 
!                                                                       
      mue = 1.7e-5*hlf(mstr,1) 
      if(mue.lt.0.0)mue = 0.0 
      hk = (0.5*sqrt(mue))/(1.+1.4*sqrt(mue)) 
      lgh = hph(mstr,1)-hk 
      vhrand = 10**(-lgh) 
!                                                                       
      mue = 1.7e-5*hlf(mstr,anze+1) 
      if(mue.lt.0.0)mue = 0.0 
      hk = (0.5*sqrt(mue))/(1.+1.4*sqrt(mue)) 
      lgh = hph(mstr,anze+1)-hk 
      vhrann = 10**(-lgh) 
                                                                       
      iorks = iork 
      goto 888 
!                                                                       
!.....H+-Transport                                                      
  828 j = 1 
      iork = 0 
      temp0 = vhrand 
      tempn = vhrann 
!                                                                       
      do 429 ior = 1,anze+1 
      U(ior) = vh(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = vh(ior) 
      endif 
  429 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....gesN-Transport                                                    
  830 j = 1 
      iork = 0 
      temp0 = hgesN(mstr,1) 
      tempn = hgesN(mstr,anze+1) 
!                                                                       
      do 431 ior = 1,anze+1 
      U(ior) = gesN(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = gesN(ior) 
      endif 
  431 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....gesP-Transport                                                    
  832 j = 1 
      iork = 0 
      temp0 = hgesP(mstr,1) 
      tempn = hgesP(mstr,anze+1) 
!                                                                       
      do 433 ior = 1,anze+1 
      U(ior) = gesP(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = gesP(ior) 
      endif 
  433 continue 
      iorks = iork 
      goto 888 
                                                                        
!.....SKmor-Transport                                                   
  834 j = 1 
      iork = 0 
      temp0 = hSKmor(mstr,1) 
      tempn = hSKmor(mstr,anze+1) 
!                                                                       
      do 435 ior = 1,anze+1 
      U(ior) = SKmor(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = SKmor(ior) 
      endif 
  435 continue 
      iorks = iork 
      goto 888 
!                                                                       
!.....DOSCF-Transport                                                   
  835 j = 1 
      iork = 0 
      temp0 = hDOSCF(mstr,1) 
      tempn = hDOSCF(mstr,anze+1) 
                                                                       
      do 436 ior = 1,anze+1 
      U(ior) = DOSCF(ior) 
      if(flag(ior).eq.4)then 
      iork = iork+1 
      iore(iork) = ior 
      Ue(iore(iork)) = DOSCF(ior) 
      endif 
  436 continue 
      iorks = iork 
      goto 888 


!.....FluN3-Transport                                                   
  837 j = 1 
      iork = 0 
      temp0 = hFluN3(mstr,1) 
      tempn = hFluN3(mstr,anze+1) 
!                                                                       
      do ior = 1,anze+1 
        U(ior) = hFluN3(mstr,ior) 
        if(flag(ior).eq.4)then 
          iork = iork+1 
          iore(iork) = ior 
           Ue(iore(iork)) = hFluN3(mstr,ior) 
        endif 
      enddo 
      iorks = iork 
      goto 888 

!.....Trockengewicht Zooplankter                                                   
  840 j = 1 
      temp0 = GRote 
      tempn = TGZoo(mstr,anze+1) 
                                                                       
      do ior = 1,anze+1 
        U(ior) = TGZoo(mstr,ior)
      enddo 
      goto 888 

  860 j = 1 
      temp0 = akmor_1(mstr,1)
      tempn = akmor_1(mstr,anze+1) 
                                                                       
      do ior = 1,anze+1 
        U(ior) = akmor_1(mstr,ior)
      enddo 
      goto 888 

  865 j = 1 
      temp0 = agmor_1(mstr,1)
      tempn = agmor_1(mstr,anze+1) 
                                                                       
      do ior = 1,anze+1 
        U(ior) = agmor_1(mstr,ior)
      enddo 
      goto 888 

  870 j = 1 
      temp0 = abmor_1(mstr,1)
      tempn = abmor_1(mstr,anze+1) 
                                                                       
      do ior = 1,anze+1 
        U(ior) = abmor_1(mstr,ior)
      enddo 
      goto 888 

! ..... Schwermetalle ......
      
   842 j = 1
       temp0 = hgsZn(mstr,1) 
       tempn = hgsZn(mstr,anze+1)
 
      do ior = 1,anze+1 
        U(ior) = hgsZn(mstr,ior)
        if(U(1)<0.0)then
          ktrans = ktrans+1
          goto 844
        endif 
      enddo 
      goto 888 

   844 j = 1
       temp0 = hglZn(mstr,1) 
       tempn = hglZn(mstr,anze+1)
 
      do ior = 1,anze+1 
        U(ior) = hglZn(mstr,ior)
        if(U(1)<0.0)then
          ktrans = ktrans+1
          goto 1811
        endif 
      enddo 
      goto 888 

   846 j = 1
       temp0 = hgsCad(mstr,1) 
       tempn = hgsCad(mstr,anze+1)
 
      do ior = 1,anze+1 
        U(ior) = hgsCad(mstr,ior)
        if(U(1)<0.0)then
          ktrans = ktrans+1
          goto 848
        endif 
      enddo 
      goto 888 

   848 j = 1
       temp0 = hglCad(mstr,1) 
       tempn = hglCad(mstr,anze+1)
 
      do ior = 1,anze+1 
        U(ior) = hglCad(mstr,ior)
        if(U(1)<0.0)then
          ktrans = ktrans+1
          goto 850
        endif 
      enddo 
      goto 888 

   850 j = 1
       temp0 = hgsCu(mstr,1) 
       tempn = hgsCu(mstr,anze+1)
 
      do ior = 1,anze+1 
        U(ior) = hgsCu(mstr,ior)
        if(U(1)<0.0)then
          ktrans = ktrans+1
          goto 852
        endif 
      enddo 
      goto 888 

   852 j = 1
       temp0 = hglCu(mstr,1) 
       tempn = hglCu(mstr,anze+1)
 
      do ior = 1,anze+1 
        U(ior) = hglCu(mstr,ior)
        if(U(1)<0.0)then
          ktrans = ktrans+1
          goto 854
        endif 
      enddo 
      goto 888 

   854 j = 1
       temp0 = hgsNi(mstr,1) 
       tempn = hgsNi(mstr,anze+1)
 
      do ior = 1,anze+1 
        U(ior) = hgsNi(mstr,ior)
        if(U(1)<0.0)then
          ktrans = ktrans+1
          goto 856
        endif 
      enddo 
      goto 888 

   856 j = 1
       temp0 = hglNi(mstr,1) 
       tempn = hglNi(mstr,anze+1)
 
      do ior = 1,anze+1 
        U(ior) = hglNi(mstr,ior)
        if(U(1)<0.0)then
          ktrans = ktrans+1
          goto 1811
        endif 
      enddo 
      goto 888 
                                                                      
 1811 continue 
                                                                       
      return 
      END                                           
