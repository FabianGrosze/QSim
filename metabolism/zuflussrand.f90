!!wy ...

!> SUBROUTINE ini_algae() wird beschrieben in: \ref lnk_algendoc \n
!! Quelle zuflussrand.f90
      SUBROUTINE ini_algae(akchl,abchl,agchl,Cagr,Caki,Cabl,CZoo,a1Ki,a2Ki,a3Ki,a1Bl,a2Bl,a3Bl,a1Gr,a2Gr,a3Gr)
      implicit none
      real :: akchl,abchl,agchl,Cagr,Caki,Cabl,CZoo,a1Ki,a2Ki,a3Ki,a1Bl,a2Bl,a3Bl,a1Gr,a2Gr,a3Gr
      !! Setzung von Parametern, die alle in QSimDatenfelder vereinbart wurden. z.T in Abhängigkeit von APARM.txt Parametern
!     aus qsim.f90:
!     Berechnung des Chlorophyll-a/Kohlenstoff-Verhaeltnisses           
!     Angabe in mgChla/mgC                                              
!                                                                       
      Cagr = 0.48 ! mgC/mgbio Kohlenstoffanteil der Biomasse, hier Grünalgen
      Caki = 0.48 
      Cabl = 0.48
      CZoo = 0.45 

      a1Ki = -1.
      a1Bl = -1.
      a1Gr = -1.
 
      if(akchl==13.3)a1Ki = -0.059
      if(akchl==20.4)a1Ki = -0.064
      if(akchl==65.7)a1Ki = -0.039
      if(akchl==20.6)a1Ki = -0.055
      if(akchl==21.9)a1Ki = -0.039
      if(a1Ki==-1.)a1Ki = -0.059

      a2Ki = 0.13
      a3Ki = 0.05

      if(abchl==68.)a1Bl = -0.062
      if(abchl==86.)a1Bl = -0.055
      if(abchl==20.6)a1Bl = -0.055
      if(abchl==35)a1Bl = -0.039
      if(a1Bl==-1.)a1Bl = -0.062
 
      a2Bl = 0.66
      a3Bl = 0.063

      if(agchl==12.4)a1Gr = -0.032
      if(agchl==32.2)a1Gr = 0.0144
      if(agchl==20.6)a1Gr = -0.055
      if(agchl==15.6)a1Gr = -0.039
      if(a1Gr==-1.)a1Gr = -0.032

      a2Gr =  0.58
      a3Gr =  0.112

      !print*,'ini_algae done'
      RETURN 
      END subroutine ini_algae
!----+-----+----

!> SUBROUTINE algae_start() wird beschrieben in: \ref lnk_algendoc \n
!! Quelle zuflussrand.f90\n
!!     Berechnung des Chlorophyll-a/Kohlenstoff-Verhaeltnisses \n          
!!     Angabe in mgChla/mgC                                  \n            
!!.....Temperaturabhängigkeit des C:Chla-Verhältnisses   !....ag(k,b)chl gilt für 20°C  in mgC/mgChla       ! mg Algenbiomasse, Chla in µg/l   \n           
      subroutine algae_start(                               &
     &     chlas ,vkigrs ,antbls, tempws,                   &
     &     akbcms ,abbcms ,agbcms,                          &
     &     akis ,abls ,agrs,                                &
     &     a1Ki,a1Bl,a1Gr,Caki,Cabl,Cagr,akchl,abchl,agchl, &
     &     chlaks ,chlabs ,chlags                           &
     &     )
      implicit none
      real :: chlas ,vkigrs ,antbls, tempws
      real :: akbcms ,abbcms ,agbcms  
      real :: akis ,abls ,agrs 
      real :: a1Ki,a1Bl,a1Gr,Caki,Cabl,Cagr,akchl,abchl,agchl 
      real :: chlaks ,chlabs ,chlags

      real :: Te0,CChl0k,CChl0b,CChl0g


!....ag(k,b)chl gilt für 20°C  in mgC/mgChla                                         
      Te0 = 20.

      CChl0k = akchl * exp(-a1Ki * Te0)              ! C:Chla bei 0°C für Kieselalgen mgC/mgChla 

      CChl0b = abchl * exp(-a1Bl * Te0)              ! Blaualgen              

      CChl0g = agchl * exp(-a1Gr * Te0)              ! Grünalgen            

      akbcms  = CChl0k * exp(a1Ki * tempws )

      abbcms  = CChl0b * exp(a1Bl * tempws )

      agbcms  = CChl0g * exp(a1Gr * tempws )

! mg Algenbiomasse, Chla in µg/l 

      akis  = (chlas *vkigrs /1000.)*(akbcms /Caki)                                   
                                    
      abls  = (chlas *antbls /1000.)*(abbcms /Cabl)                                    

      agrs  = (chlas *(1.-vkigrs -antbls )/1000.)*(agbcms /Cagr)
                  
      chlaks  = chlas *vkigrs  
      chlabs  = chlas *antbls  
      chlags  = chlas *(1.-vkigrs -antbls )                                                

        RETURN 
  END subroutine algae_start
!----+-----+---- 

!> SUBROUTINE orgc_start() wird beschrieben in: \ref BSB \n
!! Quelle zuflussrand.f90\n
!!....Berechnung der "BSB-Komponenten" am oberen Rand  \n                  
!!     (auch bei Stundenwert-Generierung)!!!!!    \n                       
subroutine orgc_start(                                    &
     &     TOC_CSB,bsbZoo,GRote,              &
     &     akis ,abls ,agrs ,                      &
     &     Caki,Cabl,Cagr,CZoo ,bsbki,bsbbl,bsbgr,  csbki,csbbl,csbgr,              &
     &     zooins ,vbsbs ,vcsbs ,              &
     &     obsbs ,ocsbs ,              &
     &     CMs ,  CDs1,CDs2, CPs1,CPs2,         &
     &     ssalgs ,frfgrs ,BACs ,CHNFs,        &
     &     CPges,CDges,Cref,TOC )

      implicit none
      real :: TOC_CSB,bsbZoo,GRote
      real :: akis ,abls ,agrs
      real :: Caki,Cabl,Cagr,CZoo, bsbki,bsbbl,bsbgr, csbki,csbbl,csbgr
      real :: zooins ,vbsbs ,vcsbs
      real :: obsbs ,ocsbs
      real :: CMs ,  CDs1,CDs2, CPs1,CPs2
      real :: ssalgs ,frfgrs ,BACs,CHNFs
      real :: CPges,CDges,Cref,TOC

      real :: algb5,algcs,alphlP,ant_CrefP
      real :: zoobsb,zoocsb
      real :: vcb,antBAC,BTOC5s,BTOCs,alphaD,alphlD !,alphaD1
      real :: hc_CPg,fak_aCref,hc_CP

      !....Berechnung der "BSB-Komponenten" am oberen Rand                    
!     (auch bei Stundenwert-Generierung)!!!!!                           

      TOC_CSB = 3.1
      bsbZoo = 1.6 

      algb5 = akis*Caki*bsbki+abls*Cabl*bsbbl+agrs*Cagr*bsbgr
      algcs = akis*Caki*csbki+abls*Cabl*csbbl+agrs*Cagr*csbgr
                                                                       
      if(zooins<0.0)zooins = 0.0 
      zoobsb = (zooins*GRote/1000.)*bsbZoo
      zoocsb = zooins*(GRote*CZoo/1000.)*TOC_CSB
!                                                                       
      if(vbsbs==0.0.and.vcsbs>0.0)vbsbs = vcsbs*0.1
      obsbs = vbsbs-algb5-zoobsb
      if(obsbs.lt.0.25)obsbs = 0.25

      if(vbsbs>0.0.and.vcsbs==0.0)vcsbs = vbsbs/0.1
                                                                       
      ocsbs = vcsbs-algcs-zoocsb 
      if(ocsbs<2.5)ocsbs = 2.5
                                                                       
      vbsbs = obsbs+algb5+zoobsb 
      vcsbs = ocsbs+algcs+zoocsb 

      vcb = obsbs*1.5/ocsbs

      antBAC = 0.0462*vcb 
      CMs = 0.03 
!...Berechnung des BTOC                                                 
      BTOC5s = obsbs*1.5/TOC_CSB
      BTOCs = BTOC5s*0.782*vcb**(-0.921) !  löschen
      BTOCs = BTOC5s*0.8822*vcb**(-0.988)
!....Berechnung des Anteils an gel”sten org. C-Verbindungen             
      alphaD = 0.802*vcb+0.1
      if(alphaD.gt.1.)alphaD = 0.95 
      if(alphaD.lt.0.0)alphaD = 0.0

      CDges = BTOCs*alphaD-CMs 
      if(CDges.lt.0.00001)CDges = 0.00001 
!.......Aufteilung der gel”sten Fraktion in leicht und schwer abbaubar  
      alphlD = 0.864*vcb+0.161 
      if(alphlD.lt.0.0)alphlD = 0.0 
      CDs1 = CDges*alphlD 
      CDs2 = CDges*(1.-alphlD) 
!.......Aufteilung der part. Fraktion in leicht und schwer abbaubar     
      CPges = BTOCs*(1.-alphaD) 
 
!  hc_CPg: org C-Gehalt des Sestons
!  hc_CP : abbaubarer C-Gehalt des Sestons, soll nicht kleiner sein als der abbaubare Anteil des part. Kohlensoffs 
!  0.45 :
!  0.4  :
! refr. partik. Anteil (ant_CrefP) ergibt sich aus dem Verhältnis von CPges und CDges
      
      if(ssalgs>0.0)then
        hc_CPg = ssalgs-akis-agrs-abls-(zooins*GRote/1000.)                   
        hc_CPg = hc_CPg * 0.45 * 0.4
        TOC = max((BTOCs+0.01),ocsbs/TOC_CSB)

        Cref = TOC - BTOCs
 
        fak_aCref = CPges/(CDges+CPges)  ! Anteil ref. am hc_CPg ?????
 
!##################################################
! Test ob errechneter CPges größer als hc_CP ist   ! ! löschen??
!##################################################      

!        hc_CP = hc_CPg - hc_CPg * fak_aCref
!        if(CPges>hc_CP)then
!          CPges = hc_CP
!          CDges = BTOCs - CPges
!          CDs(mstr,1,mRB) = CDges*alphlD 
!          CDs(mstr,2,mRB) = CDges*(1.-alphlD) 
!        endif
      endif

      BACs = CPges*antBAC 

      CPges = CPges-BACs 
      alphlP = 1.807*vcb+0.006 
      if(alphlP.lt.0.0)alphlP = 0.0
      CPs1 = CPges*alphlP 
      CPs2 = CPges*(1.-alphlP)

!....Verringerung der einzelnen Fraktionen aufrund von HNF              
      CDs1 = CDs1-0.1*CHNFs 
      CDs2 = CDs2-0.1*CHNFs 
      CPs1 = CPs1-0.35*CHNFs 
      CPs2 = CPs2-0.35*CHNFs 
      if(CDs1.le.0.0)CDs1 = 0.000001 
      if(CDs2.le.0.0)CDs2 = 0.000001 
      if(CPs1.le.0.0)CPs1 = 0.000001 
      if(CPs2.le.0.0)CPs2 = 0.000001 

      frfgrs = min(1.,(hc_CPg * ant_CrefP/Cref))

      RETURN 
END subroutine orgc_start
!----+-----+---- 

!> SUBROUTINE naehr_start() wird beschrieben in: \ref Stickstoff und \ref Phosphor  \n
!! Quelle zuflussrand.f90\n
subroutine naehr_start(                                                             &
     &     akis ,abls ,agrs ,                                   &
     &     vnh4s ,vNO3s ,vno2s ,gesNs ,                &
     &     zooins ,nZoo, pZoo, GRote,                                             &
     &     gelPs ,gesPs ,                                                &
     &     Q_NKs ,Q_PKs ,Q_SKs ,Q_NGs ,Q_PGs ,Q_NBs ,Q_PBs ,   &
     &     Qmx_NK,Qmn_NK,Qmx_PK,Qmn_PK,Qmx_SK,Qmn_SK, Qmx_NG,Qmn_NG,Qmx_PG,Qmn_PG, Qmx_NB,Qmn_NB,Qmx_PB,Qmn_PB,               &
     &     CPges,CDges,Cref,BACs ,CMs ,                                  &
     &     nl0s , pl0s ,                                                 &
     &     sss , ssalgs ,                                                &
     &     itags,monats,mstr,mRB,  einmalig, kontroll, jjj )      

      implicit none
      logical kontroll,  einmalig
      integer jjj
      real :: akis ,abls ,agrs
      real :: vnh4s ,vNO3s ,vno2s ,gesNs
      real :: zooins ,nZoo, pZoo, GRote
      real :: gelPs ,gesPs
      real :: Q_NKs ,Q_PKs ,Q_SKs ,Q_NGs ,Q_PGs ,Q_NBs ,Q_PBs
      real :: Qmx_NK,Qmn_NK,Qmx_PK,Qmn_PK,Qmx_SK,Qmn_SK, Qmx_NG,Qmn_NG,Qmx_PG,Qmn_PG, Qmx_NB,Qmn_NB,Qmx_PB,Qmn_PB
      real :: CPges,CDges,Cref,BACs ,CMs
      real :: nl0s , pl0s
      real :: sss , ssalgs
      integer :: itags,monats,mstr,mRB

      real :: hcsum, hcsumP
      real :: orgN, akiNmx,agrNmx, ablNmx, algNmx, g7sN, orgN1, orgN2, gsN
      real :: hcon1, hcon2, dgsN, dalgN, faNneu
      real :: orgP, akiPmx,agrPmx, ablPmx, algPmx, orgP1, orgP2, GsP 
      real :: dgsP, g7sP, faPneu, dalgP
      integer :: m
      real :: Sum_N, f_NK
      real :: hcQ_NK, hcQ_NG, hcQ_NB
      real :: hcQ_PK, hcQ_PG, hcQ_PB
      
!....zelluläre Nährstoffgehalte 
      Sum_N = vnh4s+vNO3s 
      f_NK = 0.582*(Sum_N/(Sum_N+0.011))
      f_NK = 1.                                       
      Q_NKs = Qmx_NK-(1.-f_NK)*(Qmx_NK-Qmn_NK) 
      Q_PKs = Qmn_PK+(Qmx_PK-Qmn_PK)/1.   
      Q_SKs = Qmx_SK 
      Q_NGs = Qmn_NG+(Qmx_NG-Qmn_NG)/1. 
      Q_PGs = Qmn_PG+(Qmx_PG-Qmn_PG)/1. 
      Q_NBs = Qmn_NB+(Qmx_NB-Qmn_NB)/1. 
      Q_PBs = Qmn_PB+(Qmx_PB-Qmn_PB)/1. 
       
!....Berechnung von nL0 und pL0 (N und P Gehalt der Abwasserbuertigen   
!    org Substanz)                                                      
!                                                                       
      hcsum = agrs+akis+abls              &
     &+zooins+vnh4s+vno2s                 &
     &+vno3s                                                  
      hcsumP = agrs+akis+abls             &
     &+zooins+gelPs                                 
!                                                                       
      if(gesNs.lt.0.0)goto 831 
      orgN = gesNs-(agrs*Q_NGs            &
     &+abls*Q_NBs+akis*Q_NKs    &
     &+(zooins*GRote/1000.)*nZoo                              &
     &+vnh4s+vno2s+vno3s)                 
      nl0s = orgN/(Cref+CDges+CPges+BACs+CMs)
 
      if(nl0s.gt.0.04)goto 833 
                                                                       
      nl0s = 0.04 

      do m = 1,2
        orgN = nl0s*(Cref+CDges+CPges+BACs+CMs)
        akiNmx = akis*Q_NKs 
        agrNmx = agrs*Q_NGs 
        ablNmx = abls*Q_NBs 
        algNmx = akiNmx+agrNmx+ablNmx 
        gsN = algNmx+orgN+(zooins*GRote/1000.)*nZoo+vnh4s+vno2s+vno3s                  

        if(gsN<=gesNs)exit
        hcon1 = gsN-gesNs
        hcon2 = orgN-hcon1
        nl0s = nl0s*hcon2/orgN
        if((nl0s<0.01).and.(einmalig))then 
! ......Fehlermeldung .........     
           write(*,1405)mRB,mstr,jjj   ! jjj-3D knotennummer
           1405 format('  Zufluss:',i3,'  Strang-Nr.:',i3,'   jjj=',I8)
           write(*,*) "  Summe aus orgN und NH4, NO2 und NO3 ist groesser als der Wert fuer gesN"                 
           einmalig=.false.
           nl0s = 0.01
           exit
        endif
      enddo  

      dgsN = gsN-gesNs 
      dalgN = algNmx-dgsN 
      if(algNmx.gt.0.0)faNneu = dalgN/algNmx 
      if(algNmx.eq.0.0)faNneu = 0.0 
!                                                                       
      Q_NKs = Q_NKs*faNneu 
      Q_NGs = Q_NGs*faNneu 
      Q_NBs = Q_NBs*faNneu

      hcQ_NK = 0.7*(Qmx_NK - Qmn_NK)+Qmn_NK
      hcQ_NG = 0.7*(Qmx_NG - Qmn_NG)+Qmn_NG
      hcQ_NB = 0.7*(Qmx_NB - Qmn_NB)+Qmn_NB
                                                                       
      if(Q_NKs<hcQ_NK)Q_NKs = hcQ_NK 
      if(Q_NGs<hcQ_NG)Q_NGs = hcQ_NG 
      if(Q_NBs<hcQ_NB)Q_NBs = hcQ_NB
      goto 833 
!                                                                       
  831 continue 
!....kein gesN am oberen Profil vorhanden wird errechnet!!              
!                                                                       
      nl0s = 0.04 
      orgN1 = (Cref+CDges+CPges+BACs+CMs)*nl0s 
      orgN2 = agrs*Q_NGs                            &
     &+abls*Q_NBs+akis*Q_NKs    &
     &+(zooins*GRote/1000.)*nZoo                              
      gesNs = orgN1+orgN2+vnh4s+vno2s     &
     &+vno3s                                                  
!                                                                       
      if(hcsum.eq.0.0)then 
      nl0s = -1. 
      gesNs = -1. 
      endif 
                                                                       
  833 if(gesPs.lt.0.0)goto 832 
      orgP = gesPs-(agrs*Q_PGs            &
     &+abls*Q_PBs+akis*Q_PKs    &
     &+(zooins*GRote/1000.)*pZoo+gelps)             
      pl0s = orgP/(Cref+CDges+CPges+BACs+CMs)
      if(pl0s.gt.0.005)goto 834 
                                                                       
      pl0s = 0.005 
      do m = 1, 2
        orgP = pl0s*(Cref+CPges+CDges+BACs+CMs) 
        akiPmx = akis*Q_PKs 
        agrPmx = agrs*Q_PGs 
        ablPmx = abls*Q_PBs 
        algPmx = akiPmx+agrPmx+ablPmx 
        gsP = algPmx+orgP+(zooins*GRote/1000.)*pZoo+gelps                                                  
        
        if(gsP<=gesPs)exit        

        hcon1 = gsP-gesPs
        hcon2 = orgP-hcon1
        pl0s = pl0s*hcon2/orgP

        if(pl0s<0.0001)then
! ......Fehlermeldung .........
            write(*,1405)mRB,mstr,itags,monats,jjj   ! jjj-3D knotennummer
            write(*,*) "  Summe aus orgP und gelP ist groesser als der Wert fuer gesP"                  
            pl0s = 0.001
          exit
        endif
      enddo  

      dgsP = gsP-gesPs 
      dalgP = algPmx-dgsP
      if(algPmx.gt.0.0)faPneu = dalgP/algPmx 
      if(algPmx.eq.0.0)faPneu = 0.0 
      Q_PKs = Q_PKs*faPneu 
      Q_PGs = Q_PGs*faPneu 
      Q_PBs = Q_PBs*faPneu 
       
      hcQ_PK = 0.7*(Qmx_PK - Qmn_PK)+Qmn_PK
      hcQ_PG = 0.7*(Qmx_PG - Qmn_PG)+Qmn_PG
      hcQ_PB = 0.7*(Qmx_PB - Qmn_PB)+Qmn_PB
                                                                       
      if(Q_PKs<hcQ_PK)Q_PKs = hcQ_PK 
      if(Q_PGs<hcQ_PG)Q_PGs = hcQ_PG 
      if(Q_PBs<hcQ_PB)Q_PBs = hcQ_PB 
                                                                       
      goto 834 
                                                                       
  832 continue 
!....kein gesP am oberen Profil vorhanden wird errechnet!!              
                                                                       
      pl0s = 0.01 
      orgP1 = (Cref+CPges+CDges+BACs+CMs)*pl0s 
      orgP2 = agrs*Q_PGs                            &
     &+abls*Q_PBs+akis*Q_PKs    &
     &+(zooins*GRote/1000.)*pZoo                              
      gesPs = orgP1+orgP2+gelps 
!                                                                       
      if(hcsumP.eq.0.0)then 
      pl0s = -1. 
      gesPs = -1. 
      endif 
!                                                                       
  834 sss = ssalgs-akis-agrs    &
     &-abls-(zooins*GRote/1000.)                    
      if(sss.lt.1.0)sss = 1. 
      
      RETURN 
END subroutine naehr_start
!----+-----+---- 
