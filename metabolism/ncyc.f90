      SUBROUTINE ncyc(tempw,vx0,vnh4,tflie,rau,tiefe,vmitt,rhyd,vo2,go2n,vno3,dC_DenW,flag,elen,ior,anze                 &                 
                     ,enh4,eno3,ex0,qeinl,vabfl,pfl,sgo2n,sedx0,don,susn,bettn,susno,agrnh4,akinh4,dzres1,dzres2         &                 
                     ,agrno3,akino3,jiein,ischif,ynmx1e,stks1e,anitrie,bnmx1e,bnks1e,vph,vno2,ij                         &
                     ,albewg,alberg,albewk,alberk,resdr,aki,agr,exdrvk,exdrvg,vx02,ex02,eno2,ynmx2e,stks2e,anitri2e      &                 
                     ,abl,ablnh4,ablno3,exdrvb,bnmx2e,bnks2e,nl0,zooind,GRote,nzoo,gesN,orgCsd                           &
                     ,egesN,sedalk,sedalb,sedalg,ilbuhn,iwied,fkm,CD,CP,CM,BAC,bsbct,nkzs,vnh4z,vno2z,vno3z,dH2D         &                 
                     ,hJNO3,hJNH4,hJN2,susO2N,hFluN3,akksN,agksN,abksN,Qmx_NK,Q_NK,up_NKz,Qmx_NG,Q_NG,up_NGz,Qmx_NB      &
                     ,Q_NB,up_NBz,dalgkz,dalgbz,dalggz,agnh4z,aknh4z,abnh4z,agno3z,akno3z,abno3z,vo2z,abltbr             &
                     ,akitbr,agrtbr,agrbrz,akibrz,ablbrz,mstr,uhrz,itags,monats,enl0,algakz,algagz,algabz                &
                     ,up_N2z,iorLa,iorLe,ieinLs,flae,qeinlL,eNH4L,eNO2L,eNO3L,gesNL,hgesNz,algdrk,algdrg,algdrb          &
                     ,ifehl,ifhstr, azStrs                                                                               &                                                   
                     ,kontroll ,jjj ) !!wy  
                                                                       
                                                                       
!     UNTERPROGRAMM ZUR BERECHNUNG DEs Ammoniums und des Nitrits und Nitrats                                                  
                                                                       
                                                                       
!     AUTOR:VOLKER KIRCHESCH                                            
                                                                       
!     STAND: 27.10.2011                                                     
                                                                       
                                                                       
                                                                       
      logical kontroll !!wy
      integer jjj !!wy
    integer                             :: anze, azStrs
    integer, Dimension(azStrs)          :: ieinLs
    integer, Dimension(100)             :: iorLa, iorLe
    integer, Dimension(1000)            :: flag, jiein, ischif, nkzs

    real                                :: nzoo, NDR, KD_N2, KNH3_X1, KHNO2_X1, KNH3_X2, KHNO2_X2, KMO_NO3, KM_NO3
    real                                :: nwgr, nwki, nhno, nreski, nresgr, nbiogr, nresbl, nl0t  

    real, Dimension(50)                 :: vnh4zt, vno2zt, vno3zt, nwgrz, nwkiz, nwblz, vxzt, susNz
    real, Dimension(50)                 :: nresgz, nreskz, nresbz, hcvNH4z, hcvNO2z, hcvNO3z, hcgesNz, dNO3Denz, susN2z, vx2zt    
    real, Dimension(50)                 :: hcvnh41z, hcvno21z, hcvno31z, alpha_upN4k, alpha_upN4g, alpha_upN4b, hgesNzt
    real, Dimension(50)                 :: vnh4z_neu, vno2z_neu, vno3z_neu, gesNz_neu, hcgesN1z
    real, Dimension(50)                 :: hcnh4Ez, hcno2Ez, hcno3Ez, hcgesNEz 
    real, Dimension(100)                :: enh4, qeinl, ex0, eno3, ex02, eno2, egesN, enl0, qeinlL, eNH4L, eNO2L, eNO3L, gesNL  
    real, Dimension(1000)               :: vx0, vnh4, tempw, vo2, go2n, vno3, dC_DenW, agrnh4, akinh4, flae
    real, Dimension(1000)               :: rau, tiefe, vmitt, rhyd,sgo2n, elen, nl0, abltbr, akitbr, agrtbr 
    real, Dimension(1000)               :: sedx0, don, susn, bettn, susno, pfl, agrno3, akino3, vph, vno2, dzres1  
    real, Dimension(1000)               :: resdr, aki, agr, dzres2, vabfl, ablno3, albewg, alberg, albewk, alberk
    real, Dimension(1000)               :: exdrvk, exdrvg, gesN, abl, ablnh4, exdrvb, vx02, fkm,susO2N,algdrk, algdrg, algdrb    
    real, Dimension(1000)               :: zooind, sedalk, sedalb, sedalg, CM, BAC, bsbct, Q_NK, Q_NG, Q_NB, betO2N      
    real, Dimension(2,1000)             :: CD, CP
    real, Dimension(50,1000)            :: up_NKz, up_NGz, up_NBz, up_N2z, vnh4z, vno2z, vno3z
    real, Dimension(azStrs,1000)        :: hJNH4, hJNO3, hJN2, hFluN3, orgCsd
    real, Dimension(50,1000)            :: dalggz, dalgkz, dalgbz, algagz, algakz, algabz, agnh4z, aknh4z, abnh4z 
    real, Dimension(50,1000)            :: agno3z, akno3z, abno3z, vo2z, agrbrz, akibrz, ablbrz 
    real, Dimension(azStrs,50,1000)     :: hgesNz

    save hcvnh41z, hcvno21z, hcvno31z, hcvnh41, hcgesN1z, hcvno21, hcvno31, hcvx01, hcvx021, hcgesN1 

                                                                       
!      open(543,file='ncyc.tst')                                        
                                                                       
      UST   =0.0 
      G     =0.0 
      ASEDN =0.0 
      BSEDN =0.0 
      WSGR  =0.0 
      QSGR  =0.0 
      QSSED =0.0 
      CSEDN =0.0 
      RHYMO =0.0 

      vnh4t = 0.0 
      vxt = 0.0
      vno3t = 0.0
      vno2t = 0.0
      vxt2 = 0.0
      gesNt = 0.0
      PFLN2 = 0.0                                                                       
      vhno2 = 0.0

      hcvnh41 = 0.0
      hcvno21 = 0.0 
      hcvno31 = 0.0 
      hcvx01 = 0.0 
      hcvx021 = 0.0 
      hcnl01 = 0.0 


      RcDenWat = 0.5
      KMO_NO3 = 0.26
      KM_NO3 = 0.4
      TcDenWat = 1.07

!      j_up: Wahl der Formel zur Beschreibung der Interaktion zwischen NH4-N und NO3-N-Aufnahme durch Algen
!      j_up = 0
!      j_up = 1: nach Yajnik & Sharada (2003)             

       j_up = 0

!        ******************************                                 
!        *    Ammonium-Stickstoff     *                                 
!        ******************************                                 
                                                                       
      iein = 1 
      v0 = 0.0 
                                                                       
!....Berücksichtigung der Linienquelle                                  
    
      do ieinL = 1, ieinLs(mstr)
        do ior = 1,anze+1
          if(iorLe(ieinL)<ior)cycle
          if(iorLa(ieinL)<=ior.and.iorLe(ieinL)>=ior)then
            if(qeinlL(ieinL)<=0.0)then
              eNH4L(ieinL) = 0.0
              eNO2L(ieinL) = 0.0
              eNO3L(ieinL) = 0.0
              gesNL(ieinL) = 0.0
            endif
            do nkz = 1,nkzs(ior)  ! 2D
              if(qeinlL(ieinL)>=0.0.and.eNH4L(ieinL)==-1)then
                else
                  vNH4z(nkz,ior) = vNH4z(nkz,ior)+((eNH4L(ieinL)-vNH4z(nkz,ior))*qeinlL(ieinL)/flae(ior))*tflie*86400.
              endif
              if(qeinlL(ieinL)>=0.0.and.eNO2L(ieinL)==-1)then
                else
                  if(vNO2z(nkz,ior)>=0.0)               &
                    vNO2z(nkz,ior) = vNO2z(nkz,ior)+((eNO2L(ieinL)-vNO2z(nkz,ior))*qeinlL(ieinL)/flae(ior))*tflie*86400.
              endif
              if(qeinlL(ieinL)>=0.0.and.eNO3L(ieinL)==-1)then
                else
                  vNO3z(nkz,ior) = vNO3z(nkz,ior)+((eNO3L(ieinL)-vNO3z(nkz,ior))*qeinlL(ieinL)/flae(ior))*tflie*86400.
              endif
            enddo

              if(qeinlL(ieinL)>=0.0.and.eNH4L(ieinL)==-1)then
                else  
                 vnh4aus = vNH4(ior)
                 vNH4(ior) = vNH4(ior)+((eNH4L(ieinL)-vNH4(ior))*qeinlL(ieinL)/flae(ior))*tflie*86400.  ! 1D
              endif 
              if(qeinlL(ieinL)>=0.0.and.eNO2L(ieinL)==-1)then
                else  
                 if(vNO2(ior)>=0.0)                     &
                   vNO2(ior) = vNO2(ior)+((eNO2L(ieinL)-vNO2(ior))*qeinlL(ieinL)/flae(ior))*tflie*86400.  ! 1D
              endif 
              if(qeinlL(ieinL)>=0.0.and.eNO3L(ieinL)==-1)then
                else  
                 vNO3(ior) = vNO3(ior)+((eNO3L(ieinL)-vNO3(ior))*qeinlL(ieinL)/flae(ior))*tflie*86400.  ! 1D
              endif 
              if(qeinlL(ieinL)>=0.0.and.gesNL(ieinL)==-1)then
                else  
                 if(gesN(ior)>=0.0)                    &
                   gesN(ior) = gesN(ior)+((gesNL(ieinL)-gesN(ior))*qeinlL(ieinL)/flae(ior))*tflie*86400.  ! 1D
              endif 
            else
         endif
       enddo ! Ende Knotenschleife 
     enddo   ! Ende Schleife Linienquellen


      do j=1,anze+1                   ! Beginn Knotenschleife 
  

      ior = j
!
       if(vabfl(ior)>=0.0.and.vabfl(ior+1)<0.0)then
         hcvnh41 = vnh4(ior)
         hcvno21 = vno2(ior) 
         hcvno31 = vno3(ior) 
         hcvx01 = vx0(ior) 
         hcvx021 = vx02(ior) 
         hcnl01 = nl0(ior) 
         if(gesN(ior)>0.0)hcgesN1 = gesN(ior) 
           do nkz = 1,nkzs(ior)
             hcvnh41z(nkz) = vnh4z(nkz,ior)
             hcvno21z(nkz) = vno2z(nkz,ior)
             hcvno31z(nkz) = vno3z(nkz,ior)
             if(gesN(ior)>0.0)hcgesN1z(nkz) = hgesNz(mstr,nkz,ior)  
           enddo
        endif

       ior_flag = 0
       if(flag(ior)==6.and.vabfl(ior)<0.0.and.vabfl(ior+1)>0.0)then
         ior = ior+1
         ior_flag = 1
       endif

      if(ilbuhn==1)then
        nkzs(ior) = 1
          else if(flag(ior)/=4)then
            else                        ! Berücksichtigung der Einleitungen
              m = 1
              ihcQ = 0
              if(vabfl(ior-1)<0.0.and.vabfl(ior)<0.0)m = -1
              if(vabfl(ior-1)<0.0.and.vabfl(ior)>0.0)ihcQ = 1 ! Konzentration an der Einleitstelle 
                                                              ! ist gleich der Konzentration der Einleitung 

              
              hcvnh4 = vnh4(ior-m)        ! Umbenennen der benötigten Variablen; 1D
              hcvno2 = vno2(ior-m) 
              hcvno3 = vno3(ior-m) 
              hcvx0 = vx0(ior-m) 
              hcvx02 = vx02(ior-m) 
              hcnl0 = nl0(ior-m) 
              hcgesN = gesN(ior-m)
              hcFluN3 = hFluN3(mstr,ior-m) 

              hcQ = vabfl(ior-m)
              if(hcQ<0.0)hcQ = abs(hcQ)
              if(hcQ==0.0.or.ihcQ==1)hcQ = 1.e-10
              
              if(ihcQ==1)then

              hcvnh4 = hcvnh41       
              hcvno2 = hcvno21 
              hcvno3 = hcvno31
              hcvx0 = hcvx01
              hcvx02 = hcvx021 
              hcnl0 = hcnl01 
              hcgesN = hcgesN1  
            endif

               nkzs_alt = nkzs(ior-m)
               nkzs_neu = nkzs(ior)

               do nkz = 1,nkzs_alt
                 vnh4z_neu(nkz) = vnh4z(nkz,ior-m)
                 if(ihcQ==1)vnh4z_neu(nkz) = hcvnh41z(nkz)
                 vno2z_neu(nkz) = vno2z(nkz,ior-m)
                 if(ihcQ==1)vno2z_neu(nkz) = hcvno21z(nkz)
                 vno3z_neu(nkz) = vno3z(nkz,ior-m)
                 if(ihcQ==1)vno3z_neu(nkz) = hcvno31z(nkz)
                 if(gesN(ior)>0.0)then
                   gesNz_neu(nkz) = hgesNz(mstr,nkz,ior-m)
                   if(ihcQ==1)gesNz_neu(nkz) = hcgesN1z(nkz)
                 endif 
               enddo

!              call z_gitter_einl(dh2D,ior,nkzs_alt,nkzs_neu,vnh4z_neu,hcvnh4z)
!              call z_gitter_einl(dh2D,ior,nkzs_alt,nkzs_neu,vno2z_neu,hcvno2z)
!              call z_gitter_einl(dh2D,ior,nkzs_alt,nkzs_neu,vno3z_neu,hcvno3z)
!              if(gesN(ior)>0.0)call z_gitter_einl(dh2D,ior,nkzs_alt,nkzs_neu,gesNz_neu,hcgesNz)


               do nkz = 1, nkzs_neu
                 hcvnh4z(nkz) = vnh4z_neu(nkz)
                 hcvnO2z(nkz) = vnO2z_neu(nkz)
                 hcvnO3z(nkz) = vnO3z_neu(nkz)
                 if(gesN(ior)>0.0)hcgesNz(nkz) = gesNz_neu(nkz)
               enddo 

              do ji=1,jiein(ior)   ! Beginn Einleitungsschleife  
              hcQE = max(0.0,qeinl(iein))

              hcNH4E = eNH4(iein)
              hcNO2E = eNO2(iein)
              hcNO3E = eNO3(iein)
              hcx0E = ex0(iein)
              hcx02E = ex02(iein)
              hcgesNE = egesN(iein)
              hcnl0E = enl0(iein)

              if(hcNH4E<0.0)hcNH4E = hcvNH4
              if(hcNO2E<0.0)hcNO2E = hcvNO2
              if(hcNO3E<0.0)hcNO3E = hcvNO3
              if(hcx0E<0.0)hcx0E = hcvx0
              if(hcx02E<0.0)hcx02E = hcvx02
              if(hcgesNE<0.0)hcgesNE = hcgesN
              if(hcnl0E<0.0)hcnl0E = hcnl0

              do nkz = 1, nkzs(ior)
                hcNH4Ez(nkz) = eNH4(iein)
                if(hcNH4Ez(nkz)<0.0)hcNH4Ez(nkz) = hcvnh4z(nkz)
                hcno2Ez(nkz) = eNO2(iein)
                if(hcno2Ez(nkz)<0.0)hcno2Ez(nkz) = hcvno2z(nkz)
                hcno3Ez(nkz) = eNO3(iein)
                if(hcno3Ez(nkz)<0.0)hcno3Ez(nkz) = hcvno3z(nkz)
                hcgesNEz(nkz) = egesN(iein)
                if(hcgesNEz(nkz)<0.0)hcgesNEz(nkz) = hcgesNz(nkz)
              enddo

 
              do nkz = 1,nkzs(ior)      ! 2D 
                vNH4z(nkz,ior) = (hcQ*hcvNH4z(nkz)+hcQE*hcNH4Ez(nkz))/(hcQ+hcQE)                                                     
                vNO2z(nkz,ior) = (hcQ*hcvNO2z(nkz)+hcQE*hcNO2Ez(nkz))/(hcQ+hcQE)                                                     
                vNO3z(nkz,ior) = (hcQ*hcvNO3z(nkz)+hcQE*hcNO3Ez(nkz))/(hcQ+hcQE)                                                     
                if(gesN(ior)>0.0)hgesNz(mstr,nkz,ior) = (hcQ*hcgesNz(nkz)+hcQE*hcgesNEz(nkz))/(hcQ+hcQE)                                                     
              enddo 
             
              vNH4(ior) = (hcQ*hcvNH4+hcQE*hcNH4E)/(hcQ+hcQE) 
              vNO2(ior) = (hcQ*hcvNO2+hcQE*hcNO2E)/(hcQ+hcQE) 
              vNO3(ior) = (hcQ*hcvNO3+hcQE*hcNO3E)/(hcQ+hcQE) 
              vx0(ior) = (hcQ*hcvx0+hcQE*hcx0E)/(hcQ+hcQE) 
              vx02(ior) = (hcQ*hcvx02+hcQE*hcx02E)/(hcQ+hcQE) 
              if(gesN(ior)>0.0)then
                gesN(ior) = (hcQ*hcgesN+hcQE*hcgesNE)/(hcQ+hcQE) 
              endif
              nL0(ior) = (hcQ*hcnl0+hcQE*hcnl0E)/(hcQ+hcQE) 
              Q_NK(ior) = (hcQ*Q_NK(ior-m)+hcQE*Qmx_NK)/(hcQ+hcQE) 
              Q_NG(ior) = (hcQ*Q_NG(ior-m)+hcQE*Qmx_NG)/(hcQ+hcQE) 
              Q_NB(ior) = (hcQ*Q_NB(ior-m)+hcQE*Qmx_NB)/(hcQ+hcQE) 

              hFluN3(mstr,ior) = hcFluN3

              
              hcQ = hcQ+qeinl(iein) 
              iein = iein+1 

              hcvnh4 = vnh4(ior)
              hcvno2 = vno2(ior) 
              hcvno3 = vno3(ior) 
              hcvx0 = vx0(ior) 
              hcvx02 = vx02(ior) 
              hcnl0 = nl0(ior) 
              hcgesN = gesN(ior) 


             do nkz = 1,nkzs(ior) 
               hcvNH4z(nkz) = vNH4z(nkz,ior)
               hcvNO2z(nkz) = vNO2z(nkz,ior)
               hcvNO3z(nkz) = vNO3z(nkz,ior)
               hcgesNz(nkz) = hgesNz(mstr,nkz,ior)  
             enddo 
           enddo                        ! Ende Einleitungsschleife

           if(ior_flag==1)then
             iein = iein - jiein(ior)
             ior = ior-1
             vNH4(ior) = vNH4(ior+1)
             vNO2(ior) = vNO2(ior+1)
             vNO3(ior) = vNO3(ior+1)
             vx0(ior) = vx0(ior+1)
             vx02(ior) = vx02(ior+1)
             if(gesN(ior+1)>0.0)gesN(ior) = gesN(ior+1)
             nL0(ior) = nL0(ior+1)
             Q_NK(ior) = Q_NK(ior+1) 
             Q_NG(ior) = Q_NG(ior+1) 
             Q_NB(ior) = Q_NB(ior+1)
             do nkz = 1,nkzs(ior)
               vNH4z(nkz,ior) = vNH4z(nkz,ior+1)                                                     
               vNO2z(nkz,ior) = vNO2z(nkz,ior+1)                                                     
               vNO3z(nkz,ior) = vNO3z(nkz,ior+1)
               if(gesN(ior+1)>0.0)hgesNz(mstr,nkz,ior) = hgesNz(mstr,nkz,ior+1)
             enddo                   
          endif
    endif                               ! Ende Einleitungs-flag                                                                  

      if(ior>1)then 
        vnh4(ior-1) = vnh4t 
        vx0(ior-1) = vxt 
        vno3(ior-1) = vno3t 
        vno2(ior-1) = vno2t 
        vx02(ior-1) = vxt2 
        gesN(ior-1) = gesNt 
                                                                       
    do nkz = 1,nkzs(ior-1) 
      vnh4z(nkz,ior-1) = vnh4zt(nkz) 
      vno2z(nkz,ior-1) = vno2zt(nkz) 
      vno3z(nkz,ior-1) = vno3zt(nkz)
      hgesNz(mstr,nkz,ior-1) = hgesNzt(nkz)
    enddo  
      endif 
                                                                        
!****** Umspeichern der KE-Werte*******                                                                       
      ba = 0.1 
      YNMAX1 = YNMX1e 
      ynmax2 = ynmx2e 
      stks2 = stks2e 
      STKS1 = STKS1e                                                                       

      NDR = 0.1             ! Stickstoffanteil der Muschelbiomasse      

!     TEMPERATURABHAENGIGKEIT DER WACHSTUMSRATE,DER HALBSAETTIGUNGSKON- 
!     STANTEN(STICKSTOFF UND SAUERSTOFF) UND DER ABSTERBERATE FUER      
!     SUSP. NITRIFIKANTEN ,SOWIE UMSATZRATE UND HALBSAETIGUNGSKONST.    
!     FUER SESSILE NITRIF. WIRD NACH W O L F BERUECKSICHTIGT            
                                                                       
      IF(TEMPW(ior)<15.)then 
        ALPHAT = 0.75*1.108**(TEMPW(ior)-15.) 
          else
           ALPHAT = 1.5/(((TEMPW(ior)-32.)/17.)**2+1) 
      endif 
                                                                    
!     Einfluss des pH auf die Nitrifikation                             
                                                                       
      fph1n2 = 1. 
      fph2n2 = 1. 
      fph1n3 = 1. 
      fph2n3 = 1. 
                                                                       
!***Berechnung der Ammoniak-Konzentration****                         
!                                                                       
      if(vph(ior)<0.0)then 
        else                                                                
          pka = 0.09018+(2729.92/(273.16 + tempw(ior)))
          vnh3 = vNH4(ior)/(1.+10**(pka-vph(ior))) 
                                                                       
            if(vx02(ior)>0.0)then 
            KD_N2 = exp(-2300./(273.16+tempw(ior)))    
            vhno2 = vno2(ior)/(1.+KD_N2*10**vph(ior))
            endif 
                                                                   
            KNH3_X1 = 35.
            KHNO2_X1 = 5.e-5
            KNH3_X2 = 0.75
            KHNO2_X2 = 0.18

          fph1n3 = 1./(1.+vnh3/KNH3_X1) 
          fph2n3 = 1.+vnh3/KNH3_X2 
          if(vx02(ior)>0.0)then 
            fph1n2 = 1.+vhno2/KHNO2_X1
            fph2n2 = 1./(1.+vhno2/KHNO2_X2) 
          endif
      endif 
                                                                       
                                                                       
!     ABHAENGIGKEIT DER NITRIFIKATIONSLEISTUNG SESSILER NITRIFIKANTEN   
!     VON DER FLIESSGESCHWINDIGKEIT                                     
!                                                                       
      RHYMO = 0.00875

      VMOD  = abs(VMITT(ior))*(RHYMO/RHYD(ior))**0.5 
      FV = 1.+(9.5*(vmod-0.045)) 
                                                                       
      BENMX1 = bnmx1e 
      BENKS1 = bnks1e
                                                                       
      BENMX2 = bnmx2e 
      BENKS2 = bnks2e 
                                                                       
!**** 1.Stufe: NH4N -> NO2N ***********

      EKX0 = 0.06                                                           
                                                                       
       do nkz = 1, nkzs(ior)
        ALPHAO = 1-EXP(-1.39*(VO2z(nkz,ior)-0.5)) ! NACH HAJEK,NEUMANN,BISCHOFSBERGER 
        IF(ALPHAO.LT.0.0)ALPHAO = 0.0 
        YN = YNMAX1*fph1n3*(VNH4(ior)/(STKS1*fph1n2+VNH4z(nkz,ior)))  ! Wachstumsrate 
        YN = YN*ALPHAO*alphat 
        VXzT(nkz) = VX0(ior)*EXP(YN*TFLIE) 

        SUSNz(nkz) = (VXzt(nkz)-VX0(ior))/EKX0
        if(SUSNz(nkz)>VNH4z(nkz,ior))then
           susNz(nkz) = vNh4z(nkz,ior)
           Vxzt(nkz) = Vx0(ior) + susNz(nkz)*EKX0
           if(vxzt(nkz)<=0.0.or.vx0(ior)<=0.0)then
             YN = 0.0
               else 
                 YN = (log(Vxzt(nkz)) - log(Vx0(ior)))/tflie
            endif
        endif 

      ANITRI = ANITRIe*(STKS1/(STKS1+VNH4z(nkz,ior)))*ALPHAT 
      if(kontroll) print*,"VX0(ior)*EXP((YN-ANITRI)*TFLIE)",VX0(ior),YN,ANITRI,TFLIE
      VXzT(nkz) = VX0(ior)*EXP((YN-ANITRI)*TFLIE)
      if(kontroll) print*,"VXzT(nkz),jjj,nkz,ior",VXzT(nkz),jjj,nkz,ior
    enddo
                                                                       
!*****Ammoniumoxidation auf Makrophyten*****
      
      U3 = 300.                                     
                                                                      
      BETTF = BENMX1*fph1n3*(vnh4(ior)/(VNH4(ior)+BENKS1*fph1n2))*FV*alphao*alphat   ! flächenbezogener Umsatz auf Makrophyten                                                       
                                                                       
      PflN1 = ((BETTF*pfl(ior))/(U3*TIEFE(ior)))*TFLIE 
                                                                       
    if(vx02(ior)<=0.0)then 
      susn2 = 0.0 
      vx02(ior) = 0.0 
        else   !********2. Stufe NO2N -> NO3N**********   
              
          EKX02 = 0.02

          ANITRI = ANITRI2e*ALPHAT 

          do nkz = 1,nkzs(ior)
            ALPHAO = 1-EXP(-1.39*(VO2z(nkz,ior)-0.5)) ! NACH HAJEK,NEUMANN,BISCHOFSBERGER 
            IF(ALPHAO.LT.0.0)ALPHAO = 0.0 
            YN = YNMAX2*fph2n2*(vno2(ior)/(STKS2*fph2n3+VNO2z(nkz,ior))) 
            YN = YN*ALPHAO*alphat 
            VX2zt(nkz) = VX02(ior)*EXP(YN*TFLIE) 
            SUSN2z(nkz) = (VX2zt(nkz)-VX02(ior))/EKX02
            if(SUSN2z(nkz)>VNO2z(nkz,ior))then
              susN2z(nkz) = vNO2z(nkz,ior)
              Vx2zt(nkz) = Vx02(ior) + susN2z(nkz)*EKX0
              YN = (log(Vx2zt(nkz)) - log(Vx02(ior)))/tflie
           endif 
 
            ANITRI = ANITRI2e*(STKS2/(STKS2+VNO2z(nkz,ior)))*ALPHAT 
            VX2zt(nkz) = VX02(ior)*EXP((YN-ANITRI)*TFLIE)
          enddo
                                                                       
          BETN2F = BENMX2*fph2n2*(vno2t/(vno2t+BENKS2*fph2n3))*FV*alphao*alphat   ! Makrophythen                              
          PflN2 = ((BETN2F*pfl(ior))/(U3*TIEFE(ior)))*TFLIE 
    endif

          sum_susN = 0.0
          sum_susN2 = 0.0
          sum_vx0 = 0.0
          sum_vx02 = 0.0
          sumH = 0.0

          do nkz=1,nkzs(ior)-1
            sum_susN = sum_susN + ((susNz(nkz)+susNz(nkz+1))/2.)*dH2D
            sum_susN2 = sum_susN2 + ((susN2z(nkz)+susN2z(nkz+1))/2.)*dH2D
            sum_vx0 = sum_vx0 + ((vxzt(nkz)+vxzt(nkz+1))/2.)*dH2D
            sum_vx02 = sum_vx02 + ((vx2zt(nkz)+vx2zt(nkz+1))/2.)*dH2D
            sumH = sumH + dH2D
          enddo
          
          if(nkzs(ior)==1)then
            susN(ior) = susNz(1)
            susN2 = susN2z(1)
            vxT = vxzt(1)
            vxt2 = vx2zt(1)
              else
                susN(ior) = sum_susN/sumH
                susN2 = sum_susN2/sumH
                vxT = sum_vx0/sumH
                vxT2 = sum_vx02/sumH
          endif  
          if(kontroll) print*,"2. Stufe NO2N -> NO3N: vxT = vxzt(1)",vxT,vxzt(1)
!**** SEDIMENTATION DER NITRIFIKANTEN***************                                   

      g = sqrt(9.81) 
      ust = (((1./rau(ior))*g)/(tiefe(ior)**0.16667))*abs(vmitt(ior))                                                  

      CSEDN = VX0(ior)*0.69 
      CSEDN2 = vx02(ior)*0.69 

      ised = 4
      jsed = 1
      ZellV = 0.0

      call Sedimentation(ior,tiefe,ised,ust,qsgr,oc,Oc0,tflie,wst,jsed,ZellV,kontroll,jjj)

      if(kontroll) print*,"nach call Sedimentation: csedn*qsgr,oc,ust",csedn,qsgr,oc,ust

      ceq = csedn*qsgr 
      ceq2 = csedn2*qsgr 

      sednit = max(0.0,(CSEDN-ceq)) * oc
      sednt2 = max(0.0,(CSEDN2-ceq2)) * oc
                                                                       
      sedx0(ior) = sednit*1000.                          !  sedimentierte Nitrosomonasbiomasse nur Ausgabewert in µg/l 
                                                                       
      vxt = vxt-sednit                                   ! Nitrosomonasbiomasse nach Sedimentationsverlusten 
      vxt2 = vxt2-sednt2                                 ! Nitrobacterbiomasse nach Sedimentationsverlusten 
                                                                       
      bettn(ior) = hJNH4(mstr,ior)*tflie*(1./(24.*tflie))/Tiefe(ior)    ! Ausgabewert 
                                                                       
      if(vx02(ior)<=0.0)then
        go2n(ior) = (SUSN(ior)+PFLN1)*4.33                              ! O2-Verbrauch durch Nitrifikation (NH4N -> NO3N) 
        susno(ior) = go2n(ior)                                          ! Ausgabewert 
          else
            GO2N(ior) = (SUSN(ior)+PFLN1)*3.22+(SUSN2+PFLN2)*1.11       ! O2-Verbrauch durch Nitrifikation (NH4N -> NO2N -> NO3N 
            susno(ior) = susn(ior)*3.22+susn2*1.11                      ! Ausgabewert
      endif

      if(ior==1)then 
        SGO2N(ior) = GO2N(ior) 
         else 
           sgo2n(ior) = sgo2n(ior-1)+go2n(ior)                           ! Aufsummierter Nitrifikationssauerstoffverbrauch, nur Ausgabe 
      endif
      if(kontroll) print*,"SEDIMENTATION DER NITRIFIKANTEN: vxt-sednit",vxt,sednit
!++++++++++++++++++++++++++++++++++++++++                               
!   doN		Ammoniumfreisetzung beim C-Abbau (wird im Baustein <orgC> berechnet      
!   dzN		Ammoniumfreisetzung durch Zooplankton (Rotatorienrespiration)
!   ddrN		Ammoniumfreisetzung durch Grund- und aktive Respiration der benthischen Filtrierer (Dreissena)
                                                                    
!******* Zooplankton **************
    if(aki(ior)==0.0.and.agr(ior)==0.0.and.abl(ior)==0.0)then                                              
      hconki = 0.0 
      hcongr = 0.0 
      hconbl = 0.0 
        else 
          hconKi = aki(ior)/(aki(ior)+agr(ior)+abl(ior)) 
          hcongr = agr(ior)/(aki(ior)+agr(ior)+abl(ior)) 
          hconbl = abl(ior)/(aki(ior)+agr(ior)+abl(ior)) 
    endif
                                                                       
      dzn = dzres1(ior)*Nzoo+(dzres2(ior)*hconKi*Q_NK(ior))+(dzres2(ior)*hcongr*Q_NG(ior))    &
           +(dzres2(ior)*hconbl*Q_NB(ior))                                    
                                                                       
!******* benthische Filtrierer*************                                    
      ddrn = resdr(ior)*NDR+exdrvk(ior)*Q_NK(ior)+exdrvg(ior)*Q_NG(ior)+exdrvb(ior)*Q_NB(ior)                                  
!                                                                       
!******* Algen **********                                                
!
!                                                                          
      do nkz = 1,nkzs(ior)  ! Beginn 2D-Schleife 

! ##### Ammoniumverbrauch durch Aufnahme (Gruen-,Kiesel- und Blaualgen) ######
      nwgrz(nkz) = up_NGz(nkz,ior)*(agrbrz(nkz,ior)-algagz(nkz,ior))+(albewg(ior)-alberg(ior))*Qmx_NG 
      nwkiz(nkz) = up_NKz(nkz,ior)*(akibrz(nkz,ior)-algakz(nkz,ior))+(albewk(ior)-alberk(ior))*Qmx_NK            !                                  (Kiesel) 
      nwblz(nkz) = up_NBz(nkz,ior)*(ablbrz(nkz,ior)- algabz(nkz,ior))                          !                                  (Blau) 

      if(nwgrz(nkz)==0.0.and.nwkiz(nkz)==0.0.and.nwblz(nkz)==0.0)then                                           
         alpha_upN4 = 0.0 
           else
             if(j_up==0)then    
               alpha_upN4 = vNH4z(nkz,ior)*vNO3z(nkz,ior)/((akksN+vNH4z(nkz,ior))*(akksN+vNO3z(nkz,ior)))  &
                           +vNH4z(nkz,ior)*akksN/((vNH4z(nkz,ior)+vNO3z(nkz,ior))*(akksN+vNO3z(nkz,ior)))
                 else if(j_up==1)then
                   a_up = 1.
                   b_up = 3.     
                   hc_upN3 = vNO3z(nkz,ior)*(1.+a_up*vNH4z(nkz,ior))/((akksN+vNO3z(nkz,ior))*(1.+b_up*vNH4z(nkz,ior)))
                   hc_upN4 = vNH4z(nkz,ior)/(akksN+vNH4z(nkz,ior))
                   alpha_upN4 = hc_upN4/(hc_upN3+hc_upN4)
               endif
             alpha_upN4k(nkz) = min(1.,alpha_upN4)

             aknh4z(nkz,ior) = nwkiz(nkz)*alpha_upN4k(nkz) 

             if(j_up==0)then
               alpha_upN4 = vNH4z(nkz,ior)*vNO3z(nkz,ior)/((agksN+vNH4z(nkz,ior))*(agksN+vNO3z(nkz,ior)))  &
                           +vNH4z(nkz,ior)*agksN/((vNH4z(nkz,ior)+vNO3z(nkz,ior))*(agksN+vNO3z(nkz,ior)))
                 else if(j_up==1)then 
                   hc_upN3 = vNO3z(nkz,ior)*(1.+a_up*vNH4z(nkz,ior))/((agksN+vNO3z(nkz,ior))*(1.+b_up*vNH4z(nkz,ior)))
                   hc_upN4 = vNH4z(nkz,ior)/(agksN+vNH4z(nkz,ior))
                   alpha_upN4 = hc_upN4/(hc_upN3+hc_upN4)
             endif
           alpha_upN4g(nkz) = min(1.,alpha_upN4)

           agnh4z(nkz,ior) = nwgrz(nkz)*alpha_upN4g(nkz) 

             if(j_up==0)then
               alpha_upN4 = vNH4z(nkz,ior)*vNO3z(nkz,ior)/((abksN+vNH4z(nkz,ior))*(abksN+vNO3z(nkz,ior)))  &
                           +vNH4z(nkz,ior)*abksN/((vNH4z(nkz,ior)+vNO3z(nkz,ior))*(abksN+vNO3z(nkz,ior)))
                 else if(j_up==1)then
                   hc_upN3 = vNO3z(nkz,ior)*(1.+a_up*vNH4z(nkz,ior))/((abksN+vNO3z(nkz,ior))*(1.+b_up*vNH4z(nkz,ior)))
                   hc_upN4 = vNH4z(nkz,ior)/(abksN+vNH4z(nkz,ior))
                   alpha_upN4 = hc_upN4/(hc_upN3+hc_upN4)
             endif

             alpha_upN4b(nkz) = min(1.,alpha_upN4)

             abnh4z(nkz,ior) = nwblz(nkz)*alpha_upN4b(nkz)
      endif                                        
                                                                     
      vnh4zt(nkz) = vnh4z(nkz,ior)-susNz(nkz)-PflN1+doN(ior)+dzN+ddrN+nreskz(nkz)+nresgz(nkz)+nresbz(nkz)  &
                   -agnh4z(nkz,ior)-aknh4z(nkz,ior)-abnh4z(nkz,ior) 

      if(nkzs(ior)==1)then
         vnh4zt(nkz) = vNH4zt(nkz)+hJNH4(mstr,ior)*tFlie/Tiefe(ior)                                                                   
           else
      endif
                                                                      
      delN4 = vnh4zt(nkz)-vnh4z(nkz,ior) 
      if(vnh4zt(nkz)<0.0)vnh4zt(nkz) = (vnh4z(nkz,ior)/(vnh4z(nkz,ior)+abs(delN4)))*vnh4z(nkz,ior) 
      if(vnh4zt(nkz)<0.0001)vnh4zt(nkz) = 0.0001                                                  
                                                                       
!....NO2                                                                
      if(vx02(ior)<=0.0)then
        else 
          vno2zt(nkz) = vno2z(nkz,ior)+susNz(nkz)-susN2z(nkz)-PflN2+PflN1                                                      
      endif
                                                                       
      delN2 = vno2zt(nkz)-vno2z(nkz,ior) 
      if(vno2zt(nkz)<0.0)vno2zt(nkz) = (vno2z(nkz,ior)/(vno2z(nkz,ior)+abs(delN2)))*vno2z(nkz,ior) 
      if(vno2zt(nkz)<0.0001)vno2zt(nkz) = 0.0001                                                  
                                                                       
     enddo      ! Ende 2D-Schleife 
!                                                                       
!**** Neue Ammonium und Nitritkonzentration fuer den 1D_Fall   
    sagn4 = 0.0 
    sakn4 = 0.0 
    sabn4 = 0.0 
    sargn4 = 0.0 
    sarkn4 = 0.0 
    sarbn4 = 0.0 

    do nkz = 1,nkzs(ior)-1 ! Mittelwertbildung der vertikalen Ammoniumaenderungen durch Aufnahme und Respiration der Algen 

      sagn4 = sagn4+((agnh4z(nkz,ior)+agnh4z(nkz+1,ior))/2.)*dH2D 
      sakn4 = sakn4+((aknh4z(nkz,ior)+aknh4z(nkz+1,ior))/2.)*dH2D 
      sabn4 = sabn4+((abnh4z(nkz,ior)+abnh4z(nkz+1,ior))/2.)*dH2D 
      sargn4 = sargn4+((nresgz(nkz)+nresgz(nkz+1))/2.)*dH2D 
      sarkn4 = sarkn4+((nreskz(nkz)+nreskz(nkz+1))/2.)*dH2D 
      sarbn4 = sarbn4+((nresbz(nkz)+nresbz(nkz+1))/2.)*dH2D 
    enddo
                                                                       
      if(nkzs(ior)==1)then 
        agrnh4(ior) = agnh4z(1,ior) 
        akinh4(ior) = aknh4z(1,ior) 
        ablnh4(ior) = abnh4z(1,ior) 

        arN4m = nresgz(1)+nreskz(1)+nresbz(1) 
          else
           agrnh4(ior) = sagn4/tiefe(ior) 
           akinh4(ior) = sakn4/tiefe(ior) 
           ablnh4(ior) = sabn4/tiefe(ior) 

           arN4m = (sargn4+sarkn4+sarbn4)/tiefe(ior) 
      endif 

      vnh4t = vnh4(ior)-susN(ior)+hJNH4(mstr,ior)*tflie/Tiefe(ior)-PFLN1+doN(ior)+dzN+ddrN       &
             -agrnh4(ior)-akinh4(ior)-ablnh4(ior)+arN4m 
      !if(ISNAN(vnh4t))then
      !   print*,"ISNAN(vnh4t) ... mstr,ior=",mstr,ior
      !   print*,"vnh4(ior)-susN(ior)+hJNH4(mstr,ior)*tflie/Tiefe(ior)"
      !   print*,"-PFLN1+doN(ior)+dzN+ddrN-agrnh4(ior)-akinh4(ior)-ablnh4(ior)+arN4m" 
      !   print*,vnh4(ior),susN(ior),hJNH4(mstr,ior),tflie,Tiefe(ior)
      !   print*,PFLN1,doN(ior),dzN,ddrN,agrnh4(ior),akinh4(ior),ablnh4(ior),arN4m
      !end if ! ISNAN(vnh4t)
      delnh4 = vnh4t-vnh4(ior) 
      if(vnh4t<0.0)vnh4t = (vnh4(ior)/(vnh4(ior)+abs(delnh4)))*vnh4(ior)                         
      if(vnh4t<0.0001)vnh4t = 0.0001



      vno2t = vno2(ior)+susn(ior)+PFLN1-susN2-PfLN2 

      delno2 = vno2t-vno2(ior) 
      if(vno2t<0.0)vno2t = (vno2(ior)/(vno2(ior)+abs(delno2)))*vno2(ior)                         
      if(vno2t<0.0001)vno2t = 0.0001 
                                                                       
                                                                       
      delx0 = vxt-vx0(ior) 
      delx2 = vxt2-vx02(ior) 
                                                                       
      if(vxt<0.0)vxt = (vx0(ior)/(vx0(ior)+abs(delx0)))*vx0(ior)                               
      if(vxt2<0.0)vxt2 = (vx02(ior)/(vx02(ior)+abs(delx2)))*vx02(ior)                           
                                                                       
      if(kontroll) print*,"if(vxt<0.0): /(vx0(ior)+abs(delx0)",vx0(ior),delx0

!       *********************************                               
!       *   Nitrat-Stickstoff           *                               
!       *********************************                               
                                                                       
      if(vx02(ior)>0.0)then 
        vno3t = vno3(ior)+SUSN2+PflN2+hJNO3(mstr,ior)*tflie/Tiefe(ior) 
         else 
           vno3t = vno3(ior)+SUSN(ior)+PFLN1+hJNO3(mstr,ior)*tflie/Tiefe(ior) 
         endif 

!...hFluN3     
!      hFluN3(mstr,ior) = hJNO3(mstr,ior)*tflie/((tflie*24.)*tiefe(ior))  ! Ausgabewert des NitratFluxes Wasser/Sediment in mgN/(l*h)

       hFluN3(mstr,ior) = hFluN3(mstr,ior) + hJN2(mstr,ior)*tflie/tiefe(ior)                 ! Ausgabewert des NitratFluxes Wasser/Sediment in mgN/(l*h)

!*****Einfluss der Algen (2D)**********                                                

      do nkz = 1,nkzs(ior)
        
        agno3z(nkz,ior) = (1.-alpha_upN4g(nkz))*nwgrz(nkz) 

        akno3z(nkz,ior) = (1.-alpha_upN4k(nkz))*nwkiz(nkz) 

        abno3z(nkz,ior) = (1.-alpha_upN4b(nkz))*nwblz(nkz) 

                                                                       
        if(vx02(ior)>0.0)then 
          vno3zt(nkz) = vno3z(nkz,ior)+susn2z(nkz)+PFLN2
            else 
              vno3zt(nkz) = vno3z(nkz,ior)+susnz(nkz)+PFLN1 
        endif 


        DenWatz = bsbCt(ior)*(KMO_NO3/(KMO_NO3 + vO2z(nkz,ior)))*(vNO3z(nkz,ior)/(vNO3z(nkz,ior) + KM_NO3))

        dNO3Denz(nkz) = 0.93*DenWatz      ! 0.93 s. Dokumentation

        vno3zt(nkz) = vno3zt(nkz)-agno3z(nkz,ior)-akno3z(nkz,ior)-abno3z(nkz,ior) - dNO3Denz(nkz)

        if(nkzs(ior)==1)vno3zt(nkz) = vno3zt(nkz)+hJNO3(mstr,ior)*tflie/Tiefe(ior)
        if(vno3zt(nkz)<0.0001)vno3zt(nkz) = 0.0001  

       enddo 
                                                                      
!**** Neue Nitratkonzentration fuer den 1D_Fall unter Berücksichtigung der Algen  

      sagn4 = 0.0 
      sakn4 = 0.0 
      sabn4 = 0.0 
      saN2 = 0.0
      sNO3Den = 0.0 
      do nkz = 1,nkzs(ior)-1 
        sagn4 = sagn4+((agno3z(nkz,ior)+agno3z(nkz+1,ior))/2.)*dH2D 
        sakn4 = sakn4+((akno3z(nkz,ior)+akno3z(nkz+1,ior))/2.)*dH2D 
        sabn4 = sabn4+((abno3z(nkz,ior)+abno3z(nkz+1,ior))/2.)*dH2D 
        saN2 = saN2+((up_N2z(nkz,ior)+up_N2z(nkz+1,ior))/2.)*dH2D     ! up_N2 Stickstoffaufnahme aus der Luft durch Blaualgen
        sNO3Den = sNO3Den+((dNO3Denz(nkz)+dNO3Denz(nkz+1))/2.)*dH2D 

      enddo 
!                                                                       
      if(nkzs(ior)>1)then 
      agrno3(ior) = sagn4/tiefe(ior) 
      akino3(ior) = sakn4/tiefe(ior) 
      ablno3(ior) = sabn4/tiefe(ior)
      up_N2m = saN2/tiefe(ior) 
      algN3m = agrno3(ior)+akino3(ior)+ablno3(ior) 
      dNO3Den = sNO3Den/Tiefe(ior)      
        else                                                                      
          agrno3(ior) = agno3z(1,ior) 
          akino3(ior) = akno3z(1,ior) 
          ablno3(ior) = abno3z(1,ior) 
          up_N2m = up_N2z(1,ior) 
          algN3m = agno3z(1,ior)+akno3z(1,ior)+abno3z(1,ior)                                                    
          dNO3Den = dNO3Denz(1)
      endif 
                                                                   
      vno3t = vno3t-algN3m - dNO3Den
      dC_DenW(ior) = dNO3Den/0.93               ! C-Abbau durch Denitrifikation in der Wassersäule  
                                                                       
      delno3 = vno3t-vno3(ior) 
      if(vno3t<0.0)vno3t = (vno3(ior)/(vno3(ior)+abs(delno3)))*vno3(ior)             
      if(vno3t<0.000002)vno3t = 0.000002 
                                                                       
!****Veraenderung des gesN durch Sedimentation, Denitrifikation und Fluxe in und aus dem Sediment****

      if(gesN(ior)>0.0)then 
        dNsed = hJNH4(mstr,ior)*tflie/Tiefe(ior)+hJNO3(mstr,ior)*tflie/Tiefe(ior)                                 
        gesNt = gesN(ior)-orgCsd(mstr,ior)*nl0(ior)-sedalk(ior)*Q_NK(ior)-sedalb(ior)*Q_NB(ior)                            &
               -sedalg(ior)*Q_NG(ior)+dNsed+up_N2m*(abltbr(ior)-algabz(1,ior)) - dNO3Den-algdrk(ior)*Q_NK(ior)-algdrg(ior)*Q_NG(ior)       &
               -algdrb(ior)*Q_NB(ior) - (albewg(ior)-alberg(ior))*Qmx_NG - (albewk(ior)-alberk(ior))*Qmx_NK     
         if(gesNt<0.001)gesNt = 0.001                                               

         if(nkzs(ior)>1)then
           do nkz = 1,nkzs(ior)
             hgesNzt(nkz) = hgesNz(mstr,nkz,ior)-orgCsd(mstr,ior)*nl0(ior)-sedalk(ior)*Q_NK(ior)-sedalb(ior)*Q_NB(ior)      &
                    -sedalg(ior)*Q_NG(ior)+up_N2z(nkz,ior)*(ablbrz(nkz,ior)-algabz(nkz,ior)) - dNO3Den - (albewg(ior)-alberg(ior))*Qmx_NG     &
                    -(albewk(ior)-alberk(ior))*Qmx_NK                                              
             if(hgesNzt(nkz)<0.001)hgesNzt(nkz) = 0.001
           enddo
             else
               nkz = 1
               hgesNzt(nkz) = hgesNz(mstr,nkz,ior)-orgCsd(mstr,ior)*nl0(ior)-sedalk(ior)*Q_NK(ior)-sedalb(ior)*Q_NB(ior)    &
                           -sedalg(ior)*Q_NG(ior)+up_N2z(nkz,ior)*(ablbrz(nkz,ior)-algabz(nkz,ior)) + dNsed - dNO3Den-algdrk(ior)*Q_NK(ior)   &
                           -algdrg(ior)*Q_NG(ior)-algdrb(ior)*Q_NB(ior) - (albewg(ior)-alberg(ior))*Qmx_NG                  & 
                           -(albewk(ior)-alberk(ior))*Qmx_NK                                              
             if(hgesNzt(nkz)<0.001)hgesNzt(nkz) = 0.001
         endif
          else 
            gesNt = gesN(ior)
 
            do nkz = 1,nkzs(ior) 
              hgesNzt(nkz) = gesNt
            enddo
      endif 

!...Fehlermeldung                                                       
      ifehl = 0
      if(ISNAN(vnh4t))then 
        ifehl = 22 
        ifhStr = mstr 
        exit 
      endif 
      if(ISNAN(vno3t))then 
        ifehl = 22 
        ifhStr = mstr 
        exit 
      endif 

  enddo                              ! Ende Knotenschleife 
                                                                       
      vnh4(anze+1) = vnh4t 
      vx0(anze+1) = vxt 
      vno3(anze+1) = vno3t 
      vno2(anze+1) = vno2t 
      vx02(anze+1) = vxt2 
      gesN(anze+1) = gesNt 
                                                                       
      do nkz = 1,nkzs(anze) 
        vnh4z(nkz,anze+1) = vnh4zt(nkz) 
        vno2z(nkz,anze+1) = vno2zt(nkz) 
        vno3z(nkz,anze+1) = vno3zt(nkz) 
      enddo
      close (58) 
                                                                       
      RETURN 
  END subroutine NCYC                                           
