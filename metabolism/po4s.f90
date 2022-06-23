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

!> Berechnung des o-PO4-Gehalts
!! @author Volker Kirchesch
!! @date 10.01.2014
subroutine po4s(gelp,flag,elen,ior,tiefe,dalggr,dalgki,dalgag,dalgak,ep,qeinl,vabfl,anze,tflie         &
                ,dzres1,dzres2,jiein,sedalk,sedalb,sedalg,albewg,alberg,albewk,alberk,resdr            &
                ,aki,agr,exdrvk,exdrvg,pl0,abl,dalgbl,dalgab,exdrvb,gesP,orgCsd,zooind,GRote           &
                ,pZoo,egesP,ilbuhn,iwied,CD,CP,CM,BAC,bsbctP,Qmx_PK,Q_PK,up_PKz,Qmx_PG,Q_PG            &
                ,up_PGz,Qmx_PB,Q_PB,up_PBz,epl0,gelpz,agrtbr,akitbr,abltbr                             &
                ,agrbrz,akibrz,ablbrz,algakz,algagz,algabz,hJPO4,nkzs,dH2D,dH2De,mstr,iorLa,iorLe      &
                ,ieinLs,flae,qeinlL,gPL,gesPL,hgesPz,algdrk,algdrg,algdrb,itags,monats,uhrz,azStrs     &
                ,kontroll,jjj)
   
   logical, intent(in)                  :: kontroll !< debugging
   integer, intent(in)                  :: jjj      !< debugging
   integer                              :: anze, azStrs
   integer, dimension(1000)             :: flag, jiein, nkzs
   integer, dimension(azStrs)           :: ieinLs
   integer, dimension(100)              :: iorLa, iorLe
   real, dimension(50)                  :: agrP, akiP, ablP, hcgelPz, hcgelpEz, gelpzt, segelP, hgesPzt
   real, dimension(50)                  :: hcgesPz,hcgespEz,gelPz_neu, gesPz_neu
   real, dimension(100)                 :: qeinlL, gPL, gesPL, epl0, egesP, ep, qeinl
   real, dimension(1000)                :: bsbctP, dH2De, agrtbr, akitbr, abltbr, abl, dalgbl, dalgab, exdrvb
   real, dimension(1000)                :: CM, BAC, gesP, zooind, gelp, dalggr, dalgki, dalgag, dalgak, vabfl
   real, dimension(1000)                :: elen, tiefe, dzres1, dzres2, flae, pl0, sedalg, Q_PK, Q_PG
   real, dimension(1000)                :: Q_PB, sedalk, sedalb, albewg, alberg, albewk, alberk
   real, dimension(1000)                :: resdr, aki, agr, exdrvk, exdrvg, algdrk, algdrg, algdrb
   real, dimension(2,1000)              :: CD, CP
   real, dimension(50,1000)             :: algakz, algagz, algabz
   real, dimension(azStrs,1000)         :: hJPO4, orgCsd
   real, dimension(50,1000)             :: gelpz, akibrz, ablbrz, agrbrz, up_PKz, up_PGz
   real, dimension(50,1000)             :: up_PBz
   real, dimension(azStrs,50,1000)      :: hgesPz
   
   iein = 1
   gelPt = 0.0
   gesPt = 0.0
   
   !....Berücksichtigung der Linienquelle
   
   do ieinL = 1, ieinLs(mstr)
      do ior = 1,anze+1
         if (iorLe(ieinL) < ior)cycle
         if (iorLa(ieinL) <= ior .and. iorLe(ieinL)>=ior) then
            if (qeinlL(ieinL) <= 0.0) then
               gPL(ieinL) = 0.0
               gesPL(ieinL) = 0.0
            endif
            do nkz = 1,nkzs(ior)  ! 2D
               if (qeinlL(ieinL)>=0.0 .and. gPL(ieinL) == -1) then
               else
                  gelPz(nkz,ior) = gelPz(nkz,ior)+((gPL(ieinL)-gelPz(nkz,ior))*qeinlL(ieinL)/flae(ior))*tflie*86400.
               endif
            enddo
            if (qeinlL(ieinL)>=0.0 .and. gPL(ieinL) == -1) then
            else
               gelP(ior) = gelP(ior)+((gPL(ieinL)-gelP(ior))*qeinlL(ieinL)/flae(ior))*tflie*86400.  ! 1D
            endif
            if (qeinlL(ieinL)>=0.0 .and. gesPL(ieinL) == -1) then
            else
               gesP(ior) = gesP(ior)+((gesPL(ieinL)-gesP(ior))*qeinlL(ieinL)/flae(ior))*tflie*86400.  ! 1D
            endif
         else
         endif
      enddo ! Ende Knotenschleife
   enddo   ! Ende Schleife Linienquellen
   
   do j = 1,anze+1                ! Schleife longitudinale Gitterpunkte
      ior = j
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
         
         hcgelP = gelP(ior-m)        ! Umbenennen der benötigten Variablen; 1D
         hcgesP = gesP(ior-m)
         hcpl0 = pl0(ior-m)
         hcQ = vabfl(ior-m)
         if (hcQ < 0.0)hcQ = abs(hcQ)
         if (hcQ == 0.0 .or. ihcQ == 1)hcQ = 1.e-10
         
         nkzs_alt = nkzs(ior-m)
         nkzs_neu = nkzs(ior)
         
         do nkz = 1, nkzs_alt
            gelPz_neu(nkz) = gelPz(nkz,ior-m)
            if (gesP(ior) > 0.0) gesPz_neu(nkz) = hgesPz(mstr,nkz,ior-m)
         enddo
         
         !              call z_gitter_einl(dh2D,ior,nkzs_alt,nkzs_neu,gelPz_neu,hcgelPz)
         !              if(gesP(ior)>0.0)call z_gitter_einl(dh2D,ior,nkzs_alt,nkzs_neu,gesPz_neu,hcgesPz)
         do nkz = 1,nkzs_neu
            hcgelPz(nkz) = gelPz_neu(nkz)
            if (gesP(ior) > 0.0)hcgesPz(nkz) = gesPz_neu(nkz)
         enddo
         do ji = 1,jiein(ior)   ! Beginn Einleitungsschleife
            hcQE = max(0.0,qeinl(iein))
            hcgelpE = eP(iein)
            hcgespE = egesP(iein)
            hcpl0E = epl0(iein)
            
            if (hcpl0E < 0.0)hcpl0E = hcpl0
            if (hcgelpE < 0.0)hcgelpE = hcgelp
            if (hcgespE < 0.0)hcgespE = hcgesp
            do nkz = 1, nkzs(ior)
               hcgelpEz(nkz) = eP(iein)
               if (hcgelpEz(nkz) < 0.0)hcgelpEz(nkz) = hcgelpz(nkz)
               hcgespEz(nkz) = egesP(iein)
               if (hcgespEz(nkz) < 0.0)hcgespEz(nkz) = hcgespz(nkz)
            enddo
            do nkz = 1,nkzs(ior)      ! 2D
               gelpz(nkz,ior) = (hcQ*hcgelpz(nkz)+hcQE*hcgelpEz(nkz))/(hcQ+hcQE)
               if (gesP(ior) > 0.0)hgesPz(mstr,nkz,ior) = (hcQ*hcgespz(nkz)+hcQE*hcgespEz(nkz))/(hcQ+hcQE)
            enddo
            
            gelp(ior) = (hcQ*hcgelp+hcQE*hcgelpE)/(hcQ+hcQE)
            pl0(ior) = (hcQ*hcpl0+hcQE*hcpl0E)/(hcQ+hcQE)
            Q_PK(ior) = (hcQ*Q_PK(ior-m)+hcQE*Qmx_PK)/(hcQ+hcQE)
            Q_PG(ior) = (hcQ*Q_PG(ior-m)+hcQE*Qmx_PG)/(hcQ+hcQE)
            Q_PB(ior) = (hcQ*Q_PB(ior-m)+hcQE*Qmx_PB)/(hcQ+hcQE)
            if (gesP(ior) > 0.0) then
               gesP(ior) = (hcQ*hcgesP+hcQE*hcgespE)/(hcQ+hcQE)
            endif
            hcQ = hcQ+qeinl(iein)
            iein = iein+1
            hcgelP = gelP(ior)
            hcgesP = gesP(ior)
            hcpl0 = pl0(ior)
            
            do nkz = 1,nkzs(ior)
               hcgelpz(nkz) = gelPz(nkz,ior)
               hcgesPz(nkz) = hgesPz(mstr,nkz,ior)
            enddo
         enddo                        ! Ende Einleitungsschleife
         if (ior_flag == 1) then
            iein = iein - jiein(ior)
            ior = ior-1
            gelp(ior) = gelp(ior+1)
            if (gesP(ior+1)>=0.0)gesP(ior) = gesP(ior+1)
            pl0(ior) = pl0(ior+1)
            Q_PK(ior) = Q_PK(ior+1)
            Q_PG(ior) = Q_PG(ior+1)
            Q_PB(ior) = Q_PB(ior+1)
            
            do nkz = 1,nkzs(ior)
               gelPz(nkz,ior) = gelPz(nkz,ior+1)
               if (gesP(ior) > 0.0)hgesPz(mstr,nkz,ior) = hgesPz(mstr,nkz,ior+1)
            enddo
         endif
      endif                               ! Ende Einleitungs-flag
      
      if (ior > 1) then
         do nkz = 1,nkzs(ior-1)
            gelPz(nkz,ior-1) = gelPzt(nkz)
            hgesPz(mstr,nkz,ior-1) = hgesPzt(nkz)
         enddo
         
         gelp(ior-1) = gelPt
         gesP(ior-1) = gesPt
      endif
      
      !     Einfluss durch C-Abbau; dop ist die P-Aenderung beim Abbau durch s
      !     und Organismen auf Makrophyten
      
      dop = bsbctP(ior)
      
      !     Einfluss des Zooplanktons
      
      hconKi = 0.0
      hconGr = 0.0
      hconBl = 0.0
      if ((aki(ior)+agr(ior)+abl(ior)) > 0.0) then
         hconKi = aki(ior)/(aki(ior)+agr(ior)+abl(ior))
         hcongr = agr(ior)/(aki(ior)+agr(ior)+abl(ior))
         hconbl = abl(ior)/(aki(ior)+agr(ior)+abl(ior))
      endif
      
      gelpzo = dzres1(ior)*0.01+(dzres2(ior)*hconKi*Q_PK(ior))+(dzres2(ior)*hcongr*Q_PG(ior))      &
               +(dzres2(ior)*hconbl*Q_PB(ior))
      !     Einfluss von Dreissena
      
      gelpdr = resdr(ior)*0.01+exdrvk(ior)*Q_PK(ior)+exdrvg(ior)*Q_PG(ior)+exdrvb(ior)*Q_PB(ior)
      
      sagP = 0.0
      sakP = 0.0
      sabP = 0.0
      sumH = 0.0
      !
      !...2D-Modellierung
      do nkz = 1,nkzs(ior)      ! 2D-Modellierung, Schleifenbeginn
         agrp(nkz) = -up_PGz(nkz,ior)*(agrbrz(nkz,ior)-algagz(nkz,ior))-(albewg(ior)-alberg(ior))*Qmx_PG
         akip(nkz) = -up_PKz(nkz,ior)*(akibrz(nkz,ior)-algakz(nkz,ior))-(albewk(ior)-alberk(ior))*Qmx_PK
         ablp(nkz) = -up_PBz(nkz,ior)*(ablbrz(nkz,ior)-algabz(nkz,ior))
         if (nkz > 1) then
            sagP = sagP + ((agrp(nkz)+agrp(nkz-1))/2.)*dH2D
            sakP = sakP + ((akip(nkz)+akip(nkz-1))/2.)*dH2D
            sabP = sabP + ((ablp(nkz)+ablp(nkz-1))/2.)*dH2D
            sumH = sumH + dH2D
         endif
         
         gelpzt(nkz) = gelpz(nkz,ior)+(agrp(nkz)+akip(nkz)+ablp(nkz))
         gelpzt(nkz) = gelpzt(nkz)+dop+gelPzo+gelpdr
         
         delp = gelpzt(nkz)-gelpz(nkz,ior)
         if (gelpzt(nkz) < 0.0)gelpzt(nkz) = (gelPz(nkz,ior)/(gelPz(nkz,ior)+abs(delp)))*gelPz(nkz,ior)
      enddo         ! 2D Schleifenende
      Psed = 0.0
      if (Tiefe(ior) > 0.0) Psed = hJPO4(mstr,ior)*tflie/Tiefe(ior)
      !....1D-PO4-Konzentration
      if (nkzs(ior) > 1) then
         !gelPt = gelP(ior)+(sagP/sumH+sakP/sumH+sabP/sumH)+dop+Psed+gelPzo+gelPdr
         gelPt = gelP(ior)+dop+Psed+gelPzo+gelPdr
         if (sumH > 0.0) gelPt = gelPt+sagP/sumH+sakP/sumH+sabP/sumH
      else
         gelPt = gelP(ior)+(agrP(1)+akiP(1)+ablP(1))+dop+Psed+gelPzo+gelPdr
      endif
      delp = gelPt-gelP(ior)
      if (gelPt < 0.0) then
         gelPt = 0.0
         if ( (gelP(ior)+abs(delp)) > 0.0) gelPt = ( gelP(ior)/ (gelP(ior)+abs(delp)) ) *gelP(ior)
      endif
      !....Veraenderung des gesP durch Sedimentation
      
      if (gesP(ior) <= 0.0) then
         gesPt = gesP(ior)
         do nkz = 1,nkzs(ior)
            hgesPzt(nkz) = gesPt
         enddo
      else
         if (nkzs(ior) > 1) then
            do nkz = 1,nkzs(ior)
               hgesPzt(nkz) = hgesPz(mstr,nkz,ior)-orgCsd(mstr,ior)*pl0(ior)-sedalk(ior)*Q_PK(ior)           &
                              -sedalb(ior)*Q_PB(ior)-sedalg(ior)*Q_PG(ior)-(albewg(ior)-alberg(ior))*Qmx_PG   &
                              -(albewk(ior)-alberk(ior))*Qmx_PK
               if (hgesPzt(nkz) < 0.0001)hgesPzt(nkz) = 0.0001
            enddo
         else
            nkz = 1
            hgesPzt(nkz) = hgesPz(mstr,nkz,ior)-orgCsd(mstr,ior)*pl0(ior)-sedalk(ior)*Q_PK(ior)           &
                           -sedalb(ior)*Q_PB(ior)-sedalg(ior)*Q_PG(ior) + Psed-algdrk(ior)*Q_PK(ior)       &
                           -algdrg(ior)*Q_PG(ior)-algdrb(ior)*Q_PB(ior)-(albewg(ior)-alberg(ior))*Qmx_PG   &
                           -(albewk(ior)-alberk(ior))*Qmx_PK
            if (hgesPzt(nkz) < 0.0001)hgesPzt(nkz) = 0.0001
         endif
         gesPt = gesP(ior)-orgCsd(mstr,ior)*pl0(ior)-sedalk(ior)*Q_PK(ior)-sedalb(ior)*Q_PB(ior)           &
                 -sedalg(ior)*Q_PG(ior)+Psed-algdrk(ior)*Q_PK(ior)-algdrg(ior)*Q_PG(ior)                   &
                 -algdrb(ior)*Q_PB(ior)-(albewg(ior)-alberg(ior))*Qmx_PG-(albewk(ior)-alberk(ior))*Qmx_PK
      endif
      if (gesPt < 0.001)gesPt = 0.001
      
   enddo        ! Ende Schleife longitudinale Gitterpunkte
   
   do nkz = 1,nkzs(anze)
      gelPz(nkz,anze+1) = gelPzt(nkz)
   enddo
   
   gelp(anze+1) = gelPt
   gesP(anze+1) = gesPt
   
end subroutine po4s
