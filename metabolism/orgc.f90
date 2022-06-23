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
!> Berechnung des biochemischen Sauerstoffbedarfs (BSB)
!! @author Volker Kirchesch
!! @date 22.05.2015
subroutine orgC(obsb,ocsb,TIEFE,RAU,TFLIE,VMITT,flae,zooind,abszo,tempw,vbsb,bsbt,flag,elen,ior,anze              &
                ,ecsb,ebsb,qeinl,vabfl,sdbsb,zexki,zexgr,bsbbet,dkimor,dgrmor,jiein,bsbgr,bsbki,akbcm             &
                ,agbcm,pfl,ezind,abl,abbcm,bsbbl,csbbl,dblmor,zexbl,drfaeb,csbki,csbgr,ischif,echla               &
                ,evkigr,eantbl,aki,agr,drfaek,drfaeg,drfaes,ssdr,orgCsd,orgCsd0,orgCsd_abb,CD,CP,CM,BAC,eCD       &
                ,eCP,eCM,eBAC,TOC_CSB,GRote,vcsb,vkigr,antbl,HNFBAC,BSBHNF,CHNF,zBAC                              &
                ,BVHNF,eCHNF,fbsgr,frfgr,fbsgrs,frfgrs,BACmua,dorgSS,ilbuhn,iwied,fkm,bsbct,qeinlL                &
                ,iorLa,iorLe,ieinLs,pl0,Q_PK,Q_PB,Q_PG,pZoo,nl0,Q_NK,Q_NB,Q_NG,nzoo,etemp,bsbctP                  &
                ,doN,hsdFluB,hyPe,hymxDe,KsD1e,KsD2e,KsMe,upBACe,JDOC1,JDOC2,YBACe,rsGBACe                        &
                ,nkzs,mstr,itags,monats,uhrz,azStrs,bsbZoo                                                        &
                ,kontroll,jjj)
   
   logical, intent(in)                  :: kontroll !< debugging
   integer, intent(in)                  :: jjj      !< debugging
   real                                 :: sumC
   integer                              :: anze, azStrs
   integer, dimension(azStrs)           :: ieinLs
   integer, dimension(100)              :: iorLa, iorLe
   integer, dimension(1000)             :: flag, jiein, ischif, nkzs
   real                                 :: KsMe, KsD1e, KsD2e, nzoo, ksM, k_BSB
   real, dimension(2)                   :: hymxD, ksD, hyD, hyP, CDt, CPt, dCD, ddCM, xCP, xCD
   real, dimension(100)                 :: echla, evkigr, eantbl, qeinl, ecsb, ebsb, ezind, eCHNF, qeinlL, etemp
   real, dimension(azStrs,100)          :: eCM, eBAC, frfgrs
   real, dimension(1000)                :: obsb, ocsb, zooind, abszo, tempw, pfl, CM, BAC, vbsb, bsbt, sdbsb
   real, dimension(1000)                :: tiefe, rau, vmitt, elen, bsbbet, abl, abbcm, dblmor, zexbl, drfaeb
   real, dimension(1000)                :: bsbctP, Q_PK, Q_PG, Q_PB, doN, antbl, JDOC2, orgCsd0
   real, dimension(1000)                :: flae,vcsb, BACmua, dkimor, dgrmor, zexki, zexgr, drfaek, drfaeg, drfaes
   real, dimension(1000)                :: akbcm, agbcm, aki, agr, fbsgr, frfgr, pl0, vkigr, nl0, Q_NK, Q_NB, Q_NG
   real, dimension(1000)                :: ssdr, vabfl, bsbct, dorgSS, HNFBAC, BSBHNF, CHNF, BVHNF, fkm, bsbCNB,JDOC1, zBAC
   real, dimension(2,1000)              :: CD, CP
   real, dimension(azStrs,1000)         :: orgCsd, orgCsd_abb
   real, dimension(azStrs,2,100)        :: eCD, eCP
   
   ! die Schwebstoffaufnahme der Dreissena <ssdr> wird vorlaeufig auf
   ! Null gesetzt (Baustein Dreissen)
   v0 = 0.0
   iein = 1
   
   bk1 = 0.51
   bk2 = 0.02
   Cagr = 0.48
   Caki = 0.48
   Cabl = 0.48
   CZoo = 0.45
   famD1 = 0.3
   famD2 = 0.2
   famP1 = 0.1
   famP2 = 0.3
   famR = 0.1
   !....Berücksichtigung der Linienquelle hier nur Verdunstung
   
   do ieinL = 1, ieinLs(mstr)
      do ior = 1,anze+1
         if (iorLe(ieinL) < ior)cycle
         if (iorLa(ieinL) <= ior .and. iorLe(ieinL)>=ior.and.qeinlL(ieinL) <= 0.0) then
            ocsb(ior) = ocsb(ior)+(-ocsb(ior)*qeinlL(ieinL)/flae(ior))*tflie*86400.
            obsb(ior) = obsb(ior)+(-obsb(ior)*qeinlL(ieinL)/flae(ior))*tflie*86400.
            CD(1,ior) = CD(1,ior)+(-CD(1,ior)*qeinlL(ieinL)/flae(ior))*tflie*86400.
            CD(2,ior) = CD(2,ior)+(-CD(2,ior)*qeinlL(ieinL)/flae(ior))*tflie*86400.
            CP(1,ior) = CP(1,ior)+(-CP(1,ior)*qeinlL(ieinL)/flae(ior))*tflie*86400.
            CP(2,ior) = CP(2,ior)+(-CP(2,ior)*qeinlL(ieinL)/flae(ior))*tflie*86400.
            CM(ior) = CM(ior)+(-CM(ior)*qeinlL(ieinL)/flae(ior))*tflie*86400.
            BAC(ior) = BAC(ior)+(-BAC(ior)*qeinlL(ieinL)/flae(ior))*tflie*86400.
         else
         endif
      enddo ! Ende Knotenschleife
   enddo   ! Ende Schleife Linienquellen
   do j = 1,anze+1                   ! Beginn Knotenschleife
      ior = j
      !....Flagellatenbiomasse: P:C und N:C-Verhältnis
      HNF_P_C = 0.0
      HNF_N_C = 0.0
      if (BVHNF(ior) > 0.0) then
         HNF_P_C = 0.004*BVHNF(ior)**0.367
         HNF_N_C = 0.183*BVHNF(ior)**0.0361
      endif
      !........
      ior_flag = 0
      if (flag(ior) == 6 .and. vabfl(ior) < 0.0.and.vabfl(ior+1) > 0.0) then
         ior = ior+1
         ior_flag = 1
      endif
      if (ilbuhn == 1) then
         nkzs(ior) = 1
      else if (flag(ior) /= 4) then
      else                        ! Berücksichtigung der Einleitungen
         m = 1
         ihcQ = 0
         if (vabfl(ior-1) < 0.0 .and. vabfl(ior) < 0.0)m = -1
         if (vabfl(ior-1) < 0.0 .and. vabfl(ior) > 0.0)ihcQ = 1 ! Konzentration an der Einleitstelle
         ! ist gleich der Konzentration der Einleitung
         
         ! Umbenennen der benötigten Variablen; 1D
         hcocsb = ocsb(ior-m)
         hcobsb = obsb(ior-m)
         hcCD1 = CD(1,ior-m)
         hcCD2 = CD(2,ior-m)
         hcCP1 = CP(1,ior-m)
         hcCP2 = CP(2,ior-m)
         hcCM = CM(ior-m)
         hcBAC = BAC(ior-m)
         hcfbsg = fbsgr(ior-m)
         hcfrfg = frfgr(ior-m)
         hcQ = vabfl(ior-m)
         if (hcQ < 0.0)hcQ = abs(hcQ)
         if (hcQ == 0.0 .or. ihcQ == 1)hcQ = 1.e-10
         
         do ji = 1,jiein(ior)
            if (ecsb(iein) <= 0.0 .and. ebsb(iein) <= 0.0) then
               secsb = hcocsb
               sebsb = hcobsb
               seCD1 = hcCD1
               seCD2 = hcCD2
               seCP1 = hcCP1
               seCP2 = hcCP2
               seCM = hcCM
               seBAC = hcBAC
               seOBSB = hcOBSB
               sefbsg = hcfrfg
               sefrfg = hcfrfg
            else
               
               algb5 = 0.0
               zoobsb = 0.0
               algcs = 0.0
               zoocsb = 0.0
               if (echla(iein)>=0.0) then !goto 318
                  if (evkigr(iein) < 0.0)evkigr(iein) = vkigr(ior-m)
                  if (eantbl(iein) < 0.0)eantbl(iein) = antbl(ior-m)
                  if (eantbl(iein) < 0.0)eantbl(iein) = 0.0
                  algb5 = echla(iein)*evkigr(iein)*akbcm(ior-m)*bsbki*0.001                        &
                          +(1.-evkigr(iein)-eantbl(iein))*echla(iein)*agbcm(ior-m)*bsbgr*0.001      &
                          +eantbl(iein)*echla(iein)*abbcm(ior-m)*bsbbl*0.001
                  
                  algcs = echla(iein)*evkigr(iein)*akbcm(ior-m)*csbki*0.001                        &
                          +(1.-evkigr(iein)-eantbl(iein))*echla(iein)*agbcm(ior-m)*csbgr*0.001      &
                          +eantbl(iein)*echla(iein)*abbcm(ior-m)*csbbl*0.001
               endif
               
               if (ezind(iein)>=0.0) then
                  zoobsb = (ezind(iein)*GRote/1000.)*bsbzoo
                  zoocsb = ezind(iein)*(GRote*CZoo/1000.)*TOC_CSB
               endif
               
               sebsb = ebsb(iein)-algb5-zoobsb
               secsb = ecsb(iein)-algcs-zoocsb
               
               if (sebsb < 0.25 .and. ebsb(iein) > 0.0)sebsb = 0.25
               if (ebsb(iein) < 0.0)sebsb = hcobsb
               if (secsb < 2.5 .and. ecsb(iein) > 0.0)secsb = 2.5
               if (ecsb(iein) < 0.0)secsb = hcocsb
            endif
            if (ebsb(iein) > 0.0 .or. ecsb(iein) > 0.0)seCD1 = eCD(mstr,1,iein)
            if (ebsb(iein) > 0.0 .or. ecsb(iein) > 0.0)seCD2 = eCD(mstr,2,iein)
            if (ebsb(iein) > 0.0 .or. ecsb(iein) > 0.0)seCP1 = eCP(mstr,1,iein)
            if (ebsb(iein) > 0.0 .or. ecsb(iein) > 0.0)seCP2 = eCP(mstr,2,iein)
            if (ebsb(iein) > 0.0 .or. ecsb(iein) > 0.0)seCM = eCM(mstr,iein)
            if (ebsb(iein) > 0.0 .or. ecsb(iein) > 0.0)seBAC = eBAC(mstr,iein)
            sefbsg = fbsgrs
            sefrfg = frfgrs(mstr,iein)
            if (seCD1 < 0.000002)seCD1 = hcCD1
            if (seCD2 < 0.000002)seCD2 = hcCD2
            if (seCP1 < 0.000002)seCP1 = hcCP1
            if (seCP2 < 0.000002)seCP2 = hcCP2
            hcQE = max(0.0,qeinl(iein))
            
            obsb(ior) = (hcQ*hcobsb+sebsb*hcQE)/(hcQ+hcQE)
            ocsb(ior) = (hcQ*hcocsb+secsb*hcQE)/(hcQ+hcQE)
            CD(1,ior) = (hcQ*hcCD1+seCD1*hcQE)/(hcQ+hcQE)
            CD(2,ior) = (hcQ*hcCD2+seCD2*hcQE)/(hcQ+hcQE)
            CP(1,ior) = (hcQ*hcCP1+seCP1*hcQE)/(hcQ+hcQE)
            CP(2,ior) = (hcQ*hcCP2+seCP2*hcQE)/(hcQ+hcQE)
            CM(ior) = (hcQ*hcCM+seCM*hcQE)/(hcQ+hcQE)
            BAC(ior) = (hcQ*hcBAC+seBAC*hcQE)/(hcQ+hcQE)
            fbsgr(ior) = (hcQ*hcfbsg+sefbsg*hcQE)/(hcQ+hcQE)
            frfgr(ior) = (hcQ*hcfrfg+sefrfg*hcQE)/(hcQ+hcQE)
            
            hcQ = hcQ+qeinl(iein)
            iein = iein+1
            hcocsb = ocsb(ior)
            hcobsb = obsb(ior)
            hcCD1 = CD(1,ior)
            hcCD2 = CD(2,ior)
            hcCP1 = CP(1,ior)
            hcCP2 = CP(2,ior)
            hcCM = CM(ior)
            hcBAC = BAC(ior)
            hcfbsg = fbsgr(ior)
            hcfrfg = frfgr(ior)
         enddo                                                  ! Ende Einleiterschleife
         
         if (ior_flag == 1) then
            iein = iein - jiein(ior)
            ior = ior-1
            obsb(ior) = obsb(ior+1)
            ocsb(ior) = ocsb(ior+1)
            CD(1,ior) = CD(1,ior+1)
            CD(2,ior) = CD(2,ior+1)
            CP(1,ior) = CP(1,ior+1)
            CP(2,ior) = CP(2,ior+1)
            CM(ior) = CM(ior+1)
            BAC(ior) = BAC(ior+1)
            fbsgr(ior) = fbsgr(ior+1)
            frfgr(ior) = frfgr(ior+1)
         endif
      endif                                                         ! Ende Einleiter-flag
      
      if (ior > 1) then
         ocsb(ior-1) = ocsbt
         obsb(ior-1) = obsbt
         CD(1,ior-1) = CDt(1)
         CD(2,ior-1) = CDt(2)
         CP(1,ior-1) = CPt(1)
         CP(2,ior-1) = CPt(2)
         CM(ior-1) = CMt
         BAC(ior-1) = BACt
         fbsgr(ior-1) = fbsgrt
         frfgr(ior-1) = frfgrt
      endif
      vcb = 0.0
      if (ocsb(ior) > 0.0) vcb = obsb(ior)/ocsb(ior)
      Cref = 0.0
      if (TOC_CSB > 0.0) &
          Cref = (ocsb(ior)/TOC_CSB)-CD(1,ior)-CD(2,ior)-CP(1,ior)-CP(2,ior)-CM(ior)-(1.-famR)*BAC(ior)-(1.-famR)*CHNF(ior)
      orgN = (Cref+CD(1,ior)+CD(2,ior)+CP(1,ior)+CP(2,ior)+CM(ior)+(1.-famR)*BAC(ior)+(1.-famR)*CHNF(ior)) * nl0(ior)
      orgP = (Cref+CD(1,ior)+CD(2,ior)+CP(1,ior)+CP(2,ior)+CM(ior)+(1.-famR)*BAC(ior)+(1.-famR)*CHNF(ior)) * pl0(ior)
      !     Temperaturabhaengigkeit
      
      Topt = 25.
      dti = 15.
      ftemp = exp(-((Tempw(ior)-Topt)**2)/(dti**2))
      
      !     ksM       -       Halbsaettigungskons. fuer die Aufnahme mono-
      !                       molekularer C-Verbindungen mgC/l
      !     upBACm    -       maximale Aufnahmerate 1/d
      !     YBAC      -       Ertragskoeffizient
      !     morBAC    -       Bakterienabsterberate 1/d
      
      ksM = KsMe
      upBACm = upBACe
      YBAC = YBACe
      resGBAC = rsGBACe
      
      hymxD(1) = hymxDe
      hymxD(2) = 0.474*vcb**(-1.346)
      if (hymxD(2) > 6.)hymxD(2) = 6.
      hyP(1) = hyPe
      hyP(2) = 1.51*vcb**2.31
      ksD(1) = KsD1e
      ksD(2) = KsD2e
      
      !     Aenderung der geloesten organischen C-Verbindungen
      !     aus partikulaeren C-Verbindungen
      
      dCD(1) = 0.0
      dCD(2) = 0.0
      do i = 1,2
         dCD(i) = hyP(i)*ftemp*CP(i,ior)*tflie
      enddo
      !..Aenderung der partik. und geloesten org. C-Verbindungen
      CDt(1) = CD(1,ior)+dCD(1)
      CDt(2) = CD(2,ior)+dCD(2)
      CPt(1) = CP(1,ior)-dCD(1)
      CPt(2) = CP(2,ior)-dCD(2)
      
      !.. Aenderung der Konz. an monomolekularen C-Verbindungen
      
      ddCM(1) = 0.0
      ddCM(2) = 0.0
      do i = 1,2
         hyD(i) = 0.0
         if ((CDt(i)+ksD(i)) > 0.0) hyD(i) = hymxD(i)*(CDt(i)/(CDt(i)+ksD(i)))
         ddCM(i) = hyD(i)*ftemp*BAC(ior)*tflie
         if (ddCM(i) > CDt(i))ddCM(i) = CDt(i) - 0.00001
      enddo
      
      CDt(1) = CDt(1)-ddCM(1)
      CDt(2) = CDt(2)-ddCM(2)
      if (CDt(1) < 0.00001)CDt(1) = 0.00001
      if (CDt(2) < 0.00001)CDt(2) = 0.00001
      
      CMt = CM(ior)+ddCM(1)+ddCM(2)
      CMtaus = CMt  !loeschen
      
      !     Bakterienwachstum
      
      upBAC = 0.0
      if ((CMt+ksM) > 0.0) upBAC = upBACm*ftemp*(CMt/(CMt+ksM))
      dCM = BAC(ior)*(exp(upBAC*tflie)-1.)
      if (dCM > CMt) then
         upBAC = 0.0
         if (tflie > 0.0) upBAC = (log(BAC(ior)+CMt-0.00001)-log(BAC(ior)))/tflie
         dCM = BAC(ior)*(exp(upBAC*tflie)-1.)
      endif
      
      resBAC = resGBAC*ftemp+upBAC*(1.-YBAC)
      if (resBAC < 0.0)resBAC = 0.0
      dBAC = BAC(ior)*(exp((upBAC-resBAC)*tflie)-1.)
      BACt = BAC(ior)+dBAC
      BACmua(ior) = upBAC-resBAC     ! Ausgabewert
      
      CMt = CMt-dCM
      if (CMt < 0.00001)CMt = 0.00001
      bsbct(ior) = BAC(ior)*(1.-exp(-resBAC*tflie))
      bsbts = bsbct(ior)*TOC_CSB
      
      !..Einfluss der sessilen Organismen
      !....FLUX in g/(m2*d)
      
      !....Aenderung der gel. C-Verbindungen durch Organismen auf Makrophyten
      !.... wird überarbeitet!!!!!!
      
      FluxD1 = 0.62*(CD(1,ior)+CD(2,ior))**0.817
      fvcb = 0.62*log(vcb)+2.2
      if (vcb < 0.0)fvcb = 0.0
      FluxD1 = FluxD1*fvcb
      
      FluxD2 = 0.56*(CD(1,ior)+CD(2,ior))**0.916
      fvcb = -3.11*vcb+1.407
      FluxD2 = FluxD2*fvcb
      
      !.......Umrechnung auf g/(m3*d)
      hconPf = 0.00
      if (tiefe(ior) > 0.0) hconPf = pfl(ior)/(300.*tiefe(ior))
      
      CDt(1) = CDt(1)-FluxD1*hconPf*ftemp*tflie
      if (CDt(1) < 0.00001)CDt(1) = 0.00001
      CDt(2) = CDt(2)-FluxD2*hconPf*ftemp*tflie
      if (CDt(2) < 0.00001)CDt(2) = 0.00001
      bsbct(ior) = bsbct(ior)+(FLuxD1+FluxD2)*hconPf*ftemp*tflie
      !....... Einfluss des Sediments
      if (tiefe(ior) > 0.0) then
         CDt(1) = CDt(1) + JDOC1(ior)*tflie/Tiefe(ior)
         CDt(2) = CDt(2) + JDOC2(ior)*tflie/Tiefe(ior)
      endif
      if (CDt(1) < 0.00001)CDt(1) = 0.00001
      if (CDt(2) < 0.00001)CDt(2) = 0.00001
      
      !....bsbct mineralisierter Kohlenstoffgehalt in der Wassersäule(einschli
      !.... doN mineralisierter N-Gehalt in der Wassersäule (einschließlich
      
      bsbctP(ior) = bsbct(ior)*pl0(ior)
      doN(ior) = bsbct(ior)*nl0(ior)
      !......Sauerstoffverbrauch durch Organismen auf Makrophyten
      
      FluxO2 = 0.758*(CD(1,ior)+CD(2,ior))+0.21
      fvcb = -5.476*(vcb**2)+2.256*vcb+0.789
      FluxO2 = FluxO2*fvcb
      
      bsbtb = FluxO2*hconPf*ftemp*tflie
      bsbt(ior) = bsbts+bsbtb
      BSBbet(ior) = bsbtb        !  Ausgabewert
      
      !     einfluss der Sedimentation
      
      g = sqrt(9.81)
      ust = 0.0
      if ( (rau(ior) > 0.0) .and. (tiefe(ior) > 0.0) )   &
          ust = (((1/rau(ior))*g)/(tiefe(ior)**0.16667))*abs(vmitt(ior))
      
      ASEDC = 1.44E-6
      BSEDC = 3.13
      CP1sd = fbsgr(ior)*CP(1,ior)
      CP2sd = fbsgr(ior)*CP(2,ior)
      BACsd = fbsgr(ior)*BAC(ior)
      Crfsd = frfgr(ior)*Cref
      
      ised = 2
      jsed = 1
      ZellV = 0.0
      call Sedimentation(tiefe(ior),ised,ust,qsgr,oc,Oc0,tflie,wst,jsed,ZellV,kontroll,jjj)
      Ceq1 = CP1sd*qsgr
      Ceq2 = CP2sd*qsgr
      Ceq3 = BACsd*qsgr
      Ceq4 = Crfsd*qsgr
      
      sedCP1 = (CP1sd-Ceq1) * oc
      sedCP2 = (CP2sd-Ceq2) * oc
      sedBAC = (BACsd-Ceq3) * oc
      sedCrf = (Crfsd-Ceq4) * oc
      orgCsd0(ior) = CP1sd * oc0 + CP2sd * oc0 + BaCsd *oc0 + Crfsd * oc0
      Creft = Cref
      
      orgCsd(mstr,ior) = sedCP1+sedCP2+sedBAC+sedCrf
      orgCsd_abb(mstr,ior) = sedCP1+sedCP2+sedBAC           ! sedimentiertes biologisch abbaubares organ.Material
      !.....Aenderung der Fraktionen durch abgstorbene HNF,
      !     Zooplankter und Algen
      
      
      !...Versuchsweise Aufteilung der abgestorbenen Algenbiomasse nachModelli
      !      famD1 = 0.35
      !      famD2 = 0.35
      !      famP1 = 0.1
      !      famP2 = 0.1
      !      famR =  0.1
      !
      
      dCD1t = famD1*BSBHNF(ior)+famD1*dkimor(ior)*Caki+famD1*dgrmor(ior)*Cagr+famD1*dblmor(ior)*Cabl    &
              +famD1*abszo(ior)*Czoo
      CDt(1) = CDt(1)+dCD1t
      
      dCD2t = famD2*BSBHNF(ior)+famD2*dkimor(ior)*Caki+famD2*dgrmor(ior)*Cagr+famD2*dblmor(ior)*Cabl    &
              +famD2*abszo(ior)*Czoo
      CDt(2) = CDt(2)+dCD2t
      
      dCP1t = famP1*BSBHNF(ior)+famP1*dkimor(ior)*Caki+famP1*dgrmor(ior)*Cagr+famP1*dblmor(ior)*Cabl    &
              +famP1*abszo(ior)*Czoo
      CPt(1) = CPt(1)+dCP1t
      
      dCP2t = famP2*BSBHNF(ior)+famP2*dkimor(ior)*Caki+famP2*dgrmor(ior)*Cagr+famP2*dblmor(ior)*Cabl    &
              +famP2*abszo(ior)*Czoo
      CPt(2) = CPt(2)+dCP2t
      
      Creft = Creft+famR*BSBHNF(ior)+famR*dkimor(ior)*Caki+famR*dgrmor(ior)*Cagr+famR*dblmor(ior)*Cabl &
              +famR*abszo(ior)*Czoo
      
      !..Aenderung des orgN und orgP-Gehalt
      
      dorgP = BSBHNF(ior)*HNF_P_C+dkimor(ior)*Q_PK(ior)+dgrmor(ior)*Q_PG(ior)+dblmor(ior)*Q_PB(ior)           &
              +abszo(ior)*pzoo
      
      dorgN = BSBHNF(ior)*HNF_N_C+dkimor(ior)*Q_NK(ior)+dgrmor(ior)*Q_NG(ior)+dblmor(ior)*Q_NB(ior)           &
              +abszo(ior)*nzoo
      
      orgPn = orgP + dorgP - bsbctP(ior)
      orgNn = orgN +dorgN - doN(ior)
      
      !....Erhoehung durch Faces-Bildung von Zooplankter und Dreissena
      
      CDt(1) = CDt(1)+famD1*zexki(ior)*Caki+famD1*zexgr(ior)*Cagr+famD1*drfaek(ior)*Caki              &
               +famD1*drfaeg(ior)*Cagr+famD1*zexbl(ior)*Cabl+famD1*drfaeb(ior)*Cabl
      CDt(2) = CDt(2)+famD2*zexki(ior)*Caki+famD2*zexgr(ior)*Cagr+famD2*drfaek(ior)*Caki              &
               +famD2*drfaeg(ior)*Cagr+famD2*zexbl(ior)*Cabl+famD2*drfaeb(ior)*Cabl
      
      CPt(1) = CPt(1)+famP1*zexki(ior)*Caki+famP1*zexgr(ior)*Cagr+famP1*drfaek(ior)*Caki              &
               +famP1*drfaeg(ior)*Cagr+famP1*zexbl(ior)*Cabl+famP1*drfaeb(ior)*Cabl
      
      CPt(2) = CPt(2)+famP2*zexki(ior)*Caki+famP2*zexgr(ior)*Cagr+famP2*drfaek(ior)*Caki              &
               +famP2*drfaeg(ior)*Cagr+famP2*zexbl(ior)*Cabl+famP2*drfaeb(ior)*Cabl
      
      Creft = Creft+famR*zexki(ior)*Caki+famR*zexgr(ior)*Cagr+famR*drfaek(ior)*Caki                   &
              +famR*drfaeg(ior)*Cagr+famR*zexbl(ior)*Cabl+famR*drfaeb(ior)*Cabl
      
      !..Aenderung ses orgP und orgN-Gehaltes
      
      dorgP = zexki(ior)*Q_PK(ior)+zexgr(ior)*Q_PG(ior)+drfaek(ior)*Q_PK(ior)+drfaeg(ior)*Q_PG(ior)   &
              +zexbl(ior)*Q_PB(ior)+drfaeb(ior)*Q_PB(ior)
      
      dorgN = zexki(ior)*Q_NK(ior)+zexgr(ior)*Q_NG(ior)+drfaek(ior)*Q_NK(ior)+drfaeg(ior)*Q_NG(ior)   &
              +zexbl(ior)*Q_NB(ior)+drfaeb(ior)*Q_NB(ior)
      
      orgPn = orgPn + dorgP
      orgNn = orgNn + dorgN
      
      !....Aenderung der partik. C-Verbindungen durch Schwebstoffaufnahme
      !... der Dreissena
      !####################################################################
      !      Annahmen:
      !       Anteil organisches Material 30%
      !       Anteil orgC 40%
      !       Anteil partikuläres refakteres C ergibt sich aus Verhältnis CPges/CDges+CPges)
      !####################################################################
      !
      sumc = CP(1,ior)+CP(2,ior)+CD(1,ior)+CD(2,ior)
      fakCref = 0.0
      if (sumc > 0.0) fakCref = (CP(1,ior)+CP(2,ior))/sumc
      sumc = (CP(1,ior)+CP(2,ior)+fakCref*Cref)
      if (sumc > 0.0) CPt(1) = CPt(1)-ssdr(ior)*0.3*0.4*(CP(1,ior)/ sumc )
      if (sumc > 0.0) CPt(2) = CPt(2)-ssdr(ior)*0.3*0.4*(CP(2,ior)/ sumc )
      if (sumc > 0.0) Creft = Creft-ssdr(ior)*0.3*0.4*(cref/ sumc )
      
      !.....Beruecksichtigung der Sedimentation
      
      CPt(1) = CPt(1)-sedCP1
      CPt(2) = CPt(2)-sedCP2
      BACt = BACt-sedBAC
      Creft = Cref-sedCrf
      orgNn = orgNn - sedCP1*nl0(ior) - sedCP2*nl0(ior) - sedCrf*nl0(ior)
      orgPn = orgPn - sedCP1*pl0(ior) - sedCP2*pl0(ior) - sedCrf*pl0(ior)
      !......Verlust der Bakterien durch HNF-Grazing
      BACt = BACt-HNFBAC(ior)-zBAC(ior)
      if (BACt < 0.00001)BACt = 0.00001
      
      !......Neuberechnung des BSB5
      BL01t = (CDt(1)+CPt(1)+(famD1+famP1)*BACt+CMt+(famD1+famP1)*CHNF(ior)) * TOC_CSB
      BL02t = (CDt(2)+CPt(2)+(famD2+famP2)*BACt+(famD2+famP2)*CHNF(ior)) * TOC_CSB
      BL01 = (CD(1,ior)+CP(1,ior)+(famD1+famP1)*BAC(ior)+CM(ior)+(famD1+famP1)*CHNF(ior)) * TOC_CSB
      BL02 = (CD(2,ior)+CP(2,ior)+(famD2+famP2)*BAC(ior)+(famD2+famP2)*CHNF(ior)) * TOC_CSB
      BL0t = BL01t + BL02t
      BL0 = BL01 + Bl02
      
      call CBSB5(BL0, BL0t, obsb, obsbt, ior, mstr)
      
      !..Neuberechnung des Faktors zur Berechnung der ablagerungsfreien
      !  Grenzkonzentration
      sumc = CP(1,ior)+CP(2,ior)+fakCref*Cref
      
      hc1 = CP(2,ior)-sedCP2+famP2*BSBHNF(ior)+famP2*dkimor(ior)*Caki+famP2*dgrmor(ior)*Cagr+famP2*dblmor(ior)*Cabl
      hc1 = hc1+famP2*abszo(ior)*Czoo
      hc1 = hc1+famP2*zexki(ior)*Caki+famP2*zexgr(ior)*Cagr+famP2*drfaek(ior)*Caki+famP2*drfaeg(ior)*Cagr
      hc1 = hc1+famP2*zexbl(ior)*Cabl+famP2*drfaeb(ior)*Cabl
      if (sumc > 0.0) hc1 = hc1-(ssdr(ior)*0.3*0.4*(CP(2,ior)/ sumc ))
      
      hc2 = CP2sd-sedCP2+famP2*BSBHNF(ior)+famP2*dkimor(ior)*Caki+famP2*dgrmor(ior)*Cagr+famP2*dblmor(ior)*Cabl
      hc2 = hc2+famP2*abszo(ior)*Czoo
      hc2 = hc2+famP2*zexki(ior)*Caki+famP2*zexgr(ior)*Cagr+famP2*drfaek(ior)*Caki+famP2*drfaeg(ior)*Cagr
      hc2 = hc2+famP2*zexbl(ior)*Cabl+famP2*drfaeb(ior)*Cabl
      if (sumc > 0.0) hc2 = hc2-(ssdr(ior)*0.3*0.4*(CP(2,ior)/ sumc ))
      
      fbsgrt = 0.0
      if (hc1 > 0.0) fbsgrt = max(0.0,min(0.9,(hc2/hc1)))
      
      
      hc1 = Cref-sedCrf+famR*BSBHNF(ior)+famR*dkimor(ior)*Caki+famR*dgrmor(ior)*Cagr+famR*dblmor(ior)*Cabl
      hc1 = hc1+famR*abszo(ior)*Czoo
      hc1 = hc1+famR*zexki(ior)*Caki+famR*zexgr(ior)*Cagr+famR*drfaek(ior)*Caki+famR*drfaeg(ior)*Cagr
      hc1 = hc1+famR*zexbl(ior)*Cabl+famR*drfaeb(ior)*Cabl
      if (sumc > 0.0) hc1 = hc1-(ssdr(ior)*0.3*0.4*(cref/ sumc ))
      
      hc2 = Crfsd-sedCrf+famR*BSBHNF(ior)+famR*dkimor(ior)*Caki+famR*dgrmor(ior)*Cagr+famR*dblmor(ior)*Cabl
      hc2 = hc2+famR*abszo(ior)*Czoo
      hc2 = hc2+famR*zexki(ior)*Caki+famR*zexgr(ior)*Cagr+famR*drfaek(ior)*Caki+famR*drfaeg(ior)*Cagr
      hc2 = hc2+famR*zexbl(ior)*Cabl+famR*drfaeb(ior)*Cabl
      if (sumc > 0.0) hc2 = hc2-(ssdr(ior)*0.3*0.4*(cref/ sumc ))
      
      frfgrt = 0.0
      if (hc1 > 0.0) frfgrt = max(0.0,min(0.9,(hc2/hc1)))
      
      delbsg = fbsgr(ior)-fbsgrt
      delfrg = frfgr(ior)-frfgrt
      
      delbs = obsbt-obsb(ior)
      delcs = ocsbt-ocsb(ior)
      delCD1 = CDt(1)-CD(1,ior)
      delCD2 = CDt(2)-CD(2,ior)
      delCP1 = CPt(1)-CP(1,ior)
      delCP2 = CPt(2)-CP(2,ior)
      delCM = CMt-CM(ior)
      delBAC = BACt-BAC(ior)
      if (obsbt < 0.0)obsbt = max(0.0, (obsb(ior)/(obsb(ior)+abs(delbs)))*obsb(ior)   )
      if (CDt(1) < 0.0)CDt(1) = max(0.0, (CD(1,ior)/(CD(1,ior)+abs(delCD1)))*CD(1,ior)  )
      if (CDt(2) < 0.0)CDt(2) = max(0.0, (CD(2,ior)/(CD(2,ior)+abs(delCD2)))*CD(2,ior)  )
      if (CPt(1) < 0.0)CPt(1) = max(0.0, (CP(1,ior)/(CP(1,ior)+abs(delCP1)))*CP(1,ior)  )
      if (CPt(2) < 0.0)CPt(2) = max(0.0, (CP(2,ior)/(CP(2,ior)+abs(delCP2)))*CP(2,ior)  )
      if (CMt < 0.0)CMt = max(0.0, (CM(ior)/(CM(ior)+abs(delCM)))*CM(ior)         )
      if (BACt < 0.0)BACt = max(0.0, (BAC(ior)/(BAC(ior)+abs(delBAC)))*BAC(ior)     )
      ocsbt = Creft+CDt(1)+CDt(2)+CPt(1)+CPt(2)+CMt+(1.-famR)*BACt + (1.-famR)*CHNF(ior)
      ocsbt = ocsbt*TOC_CSB
      if (ocsbt < 0.0)ocsbt = max(0.0, (ocsb(ior)/(ocsb(ior)+abs(delcs)))*ocsb(ior)   )
      !...Neuberechnung von pl0 und nl0
      !
      nl0(ior) = 0.0
      pl0(ior) = 0.0
      if ((ocsbt > 0.0) .and. (TOC_CSB > 0.0)) then
         pl0(ior) = orgPn/(ocsbt/TOC_CSB)
         nl0(ior) = orgNn/(ocsbt/TOC_CSB)
      endif
      nl0(ior) = max(0.0,min(0.2,nl0(ior)))
      pl0(ior) = max(0.0,min(0.02,pl0(ior)))
      !     einfluss der lebenden Organismus auf BSB5 und CSB
      
      !...Algen
      algb51 = aki(ior)*Caki*bsbki
      algb52 = agr(ior)*Cagr*bsbgr
      algb53 = abl(ior)*Cabl*bsbbl
      
      algb5 = algb51 + algb52 + algb53
      algcs1 = aki(ior)*Caki*csbKi
      algcs2 = agr(ior)*Cagr*csbgr
      algcs3 = abl(ior)*Cabl*csbbl
      algcs = algcs1 + algcs2 + algcs3
      
      !.... Rotatorien
      
      !     Annahme: nur die vorhandenen Zooplankter tragen zum BSB5 bei.
      !              moegliches Hinzuwachsen der Zooplankter in der BSB-Flasch
      !              wird dadurch beruecksichtigt, dass die gesamte Algenbioma
      !              zum BSB5 beitraegt.
      !              In 5d verbraucht 1 mg Zooplanktonbiomasse 1.6 mg O2
      
      
      zoobsb = (zooind(ior)*GRote/1000.)*bsbZoo
      zooCSB = (zooind(ior)*GRote*Czoo/1000.)*TOC_CSB
      
      vbsb(ior) = obsbt+algb5+zoobsb
      vCSB(ior) = ocsbt+algcs+zooCSB
      
      !.....Veraenderung der org SS beim C-Abbau (dorgSS)
      dorgSS(ior) = (CPt(1)+sedCP1-CP(1,ior))+(CPt(2)+sedCP2-CP(2,ior)) &
                    +(BACt+sedBAC-BAC(ior))+(Creft+sedCrf-Cref)*fakCref
      
      !*****Umrechnung in TG****
      dorgSS(ior) = dorgSS(ior)/0.45
   enddo                                                ! Ende Knotenschleife
   
   ocsb(anze+1) = ocsbt
   obsb(anze+1) = obsbt
   CD(1,anze+1) = CDt(1)
   CD(2,anze+1) = CDt(2)
   CP(1,anze+1) = CPt(1)
   CP(2,anze+1) = CPt(2)
   CM(anze+1) = CMt
   BAC(anze+1) = BACt
   fbsgr(anze+1) = fbsgrt
   frfgr(anze+1) = frfgrt
   
   return
end subroutine orgC
