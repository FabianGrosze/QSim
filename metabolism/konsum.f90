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

!> Calculation of zooplankton dynamics
!!
!! @author: Volker Kirchesch
!! @date: 21.5.2015
subroutine konsum(vkigr,TEMPW,VO2,TFLIE                                             &
                  ,ezind,ZOOIND,abszo,ir,flag,elen,ior,anze,qeinl,vabfl             &
                  ,jiein,FopIRe,GRote,dzres1,dzres2,zresge                          &
                  ,irmaxe,zexki,zexgr,zexbl                                         &
                  ,aki,agr,abl,iwied,rmuas,iras,TGZoo,BAC,zBAC                      &
                  ,rakr,rbar,CHNF,zHNF,ilbuhn,zakie,zagre,zable,HNFza,algzok        &
                  ,algzog,algzob,akiz,agrz,ablz,algzkz,algzgz,algzbz,nkzs,monats    &
                  ,itags,uhrz,mstr                                                  &
                  ,kontroll ,jjj ) !!wy
   
   !   irmax  - max. Filtrierate in mueg Chl-a/(100 Ind*d)
   use allodim
   implicit none
   
   integer                      :: ji, nkz, m, mstr, monats, jjj
   integer                      :: lnq, j, jj, itgzoo, anze, iein, iwied
   integer                      :: itags, ior_flag, ior, ilbuhn, ihcq
   real                         :: zresge,  zooint, zass, zaki
   real                         :: zakie, zagr, zagre, zabl, zable
   real                         :: x, w, volrot, up_crot, uhrz
   real                         :: tgzoot, tflie, rot, rott, rotc, resprg
   real                         :: prodrot, hczoo, hczooe, hczoo1, hctgzoo
   real                         :: hctgzooe, hctgzoo1, hcq, hcqe, hconm
   real                         :: hconki, hcongr, hconf, hconbl, hcaki
   real                         :: grot, grote, ftresr, ftmorr
   real                         :: irmax, irmaxe, mueRot, morRot
   real                         :: irmax_Ind, ir_F, FTing,  delZoo
   real                         :: FOPTR, FopIre, ClearR, ClearRLog, ClearR_Ind
   real                         :: filO2, filAbio, fks
   real                         :: hcon, FTMoR, fta, zooint_old
   logical                      :: kontroll 
   integer, dimension(1000)     :: jiein, nkzs, flag
   real, dimension(100)         :: ezind, qeinl
   real, dimension(1000)        :: tempw, vo2, zooind, abszo, zexki, zexgr, zexbl, aki, agr, abl, vkigr
   real, dimension(1000)        :: elen, ir, dzres1, dzres2, vabfl, rmuas, iras, rakr, rbar
   real, dimension(1000)        :: CHNF, zHNF, BAC, zBAC, HNFza, algzok, algzog, algzob
   real, dimension(50,1000)     :: akiz, agrz, ablz, algzkz, algzgz, algzbz
   real, dimension(azSTrs,1000) :: TGZoo
   real, parameter              :: epsilon = 1.e-8
   real, parameter              :: Cagr    = 0.48
   real, parameter              :: Caki    = 0.48
   real, parameter              :: Cabl    = 0.48
   real, parameter              :: CRot    = 0.45
   real, parameter              :: dokrit  = 2.5
   real, parameter              :: mormax  = 0.15            !    mormax = 0.32
   real, parameter              :: ASSmxR  = 0.84            ! Verschoor et al. (2007)
   real, parameter              :: respaR  = 0.203
   real, parameter              :: Emort   = 2.
   real, parameter              :: EASS    = 0.705           ! Verschoor et al. (2007)
   real, parameter              :: thresR  = 1.12
   real, parameter              :: thmorR  = 1.077
   real, parameter              :: thIng   = 1.08
   real, parameter              :: zqz10   = 2.23
   real, parameter              :: ztmax   = 26.1
   real, parameter              :: ztopt   = 22.2
   real, parameter              :: ZellVGr = 320.
   real, parameter              :: ZellVKi = 645.
   real, parameter              :: ZellVBl = 1000.
   logical, parameter           :: isTGZoo = .false.
   
   save hczoo1, hcTGZoo1
   
   iein = 1
   
   zagr = zagre
   zaki = zakie
   zabl = zable
   
   if (iwied == 0) TGZoo(:,:) = GRote
   
   FOPTR = FopIRe
   GROT = GRote
   irmax = IRmaxe ! in [1/d]
   RotC = GROT * CRot
   if (.not. isTGZoo) then
      if (IRmaxe < 0. .and. RotC > 0.) then
         ! up_CROT: Gewichtszpezifische max. Ingestionsrate µC^(-2/3)*d-1
         up_CROT = -0.8377 * log10(RotC) + 0.3131
         up_CROT = 10**up_CROT
      else
         up_CROT = IRmaxe
      endif
      ! maximum ingestion rate
      IRmax = up_CROT * RotC**(2./3.)
      if (FopIRe < 0. .and. RotC > 0.) then
         ClearRlog = -0.9987 * log10(RotC) - 0.706
         ! Clearance rate (1/h)
         ClearR    = 1.e5 * (10**(ClearRlog)*RotC**(2./3.))
         VolRot = RotC * 1.e6 / 0.12
         ClearR_Ind = ClearR * VolRot / 1.e9
         IRmax_Ind = IRmax * VolRot * 0.00012/24.
         FKs = IRmax_Ind/ClearR_Ind
      else
         FKs = FopIRe
      endif
   endif
   
   do j = 1,anze+1  !Beginn Knotenschleife
      ior = j
      if (isTGZoo) then
         up_CROT = IRmaxe
         FKs     = 0.
         RotC    = TGZoo(mstr,ior) * CRot
         if (RotC > 0.0) then
            zagr = min(1.,max(0.0,-0.656*log10(ZellVGr/RotC)+3.27))
            zaki = min(1.,max(0.0,-0.656*log10(ZellVKi/RotC)+3.27))
            zabl = min(1.,max(0.0,-0.656*log10(ZellVBl/RotC)+3.27))
            if (IRmaxe < 0.0) then
               ! up_CROT: Gewichtszpezifische max. Ingestionsrate µC^(-2/3)*d-1
               up_CROT = -0.8377 * log10(RotC) + 0.3131
               up_CROT = 10**up_CROT
            endif
            ! maximum ingestion rate
            IRmax = up_CROT * RotC**(2./3.)
            if (FopIRe < 0.0) then
               ClearRlog = -0.9987*log10(RotC) - 0.706
               ClearR = (10**(ClearRlog) * RotC**(2./3.)) * 1.e5   ! Filtrierrate in 1/h
               
               VolRot = RotC * 1.e6 / 0.12
               ClearR_Ind = ClearR*VolRot/1.e9
            
               IRmax_Ind = IRmax*VolRot*0.00012/24.
               FKs = IRmax_Ind/ClearR_Ind
            else
               FKs = FopIRe
            endif
         endif
      endif
      zooind(ior) = max(0., zooind(ior))
      if (vabfl(ior) >= 0.0 .and. vabfl(ior+1) < 0.0) then
         hczoo1   = zooind(ior)
         hcTGZoo1 = TGZoo(mstr,ior)
      endif
      ior_flag = 0
      if (flag(ior) == 6 .and. vabfl(ior) < 0.0 .and. vabfl(ior+1) > 0.0) then
         ior = ior + 1
         ior_flag = 1
      endif
      if (ilbuhn == 1) then
         nkzs(ior) = 1
      elseif (flag(ior) == 4) then
         ! Berücksichtigung der Einleitungen
         m = 1
         ihcQ = 0
         if (vabfl(ior-1) < 0.0 .and. vabfl(ior) < 0.0) m = -1
         if (vabfl(ior-1) < 0.0 .and. vabfl(ior) > 0.0) ihcQ = 1 ! Konzentration an der Einleitstelle
         ! ist gleich der Konzentration der Einleitung
         
         hczoo = zooind(ior-m)     ! Umbenennen der benötigten Variablen; 1D
         hcQ = abs(vabfl(ior-m))
         hcTGZoo = TGZoo(mstr,ior-m)
         if (hcQ < epsilon .or. ihcQ == 1) hcQ = 1.e-10
         if (ihcQ == 1) then
            hczoo = hczoo1
            hcTGZoo = hcTGZoo1
         endif
         
         do ji = 1,jiein(ior)   ! Beginn Einleitungsschleife
            hcQE = max(0.0,qeinl(iein))
            hczooE = ezind(iein)
            if (hczooE < 0.0) hczooE = hczoo
            hcTGZooE = hcTGZoo
            zooind(ior) = (hcQ*hczoo+hcQE*hczooE)/(hcQ+hcQE)
            if (ezind(iein) > 0.0 .and. qeinl(iein) == 0.0) then
               zooind(ior) = ezind(iein)
            endif
            TGZoo(mstr,ior) = (hcQ*hcTGZoo+hcQE*hcTGZooE)/(hcQ+hcQE)
            hcQ = hcQ+qeinl(iein)
            iein = iein+1
            hczoo = zooind(ior)
            hcTGZoo = TGZoo(mstr,ior)
         enddo                        ! Ende Einleitungsschleife
         if (ior_flag == 1) then
            iein = iein - jiein(ior)
            ior = ior-1
            zooind(ior) = zooind(ior+1)
            TGZoo(mstr,ior) = TGZoo(mstr,ior+1)
         endif
      endif                               ! Ende Einleitungs-flag
      
      if (ior > 1) then
         zooind(ior-1) = zooint
         TGZoo(mstr,ior-1) = TGZoot
      endif
      
      abszo(ior) = 0.0
      mueRot = 0.0
      ir(ior) = 0.0
      iras(ior) = 0.0
      zHNF(ior) = 0.0
      HNFza(ior) = 0.0
      dzres2(ior) = 0.0
      zexki(ior) = 0.0
      zexgr(ior) = 0.0
      zexbl(ior) = 0.0
      
      !   Temperaturabhaengigkeit der Ingestionsrate
      fTing = thIng**(Tempw(ior)-20.)
      !TODO FG: Not used, hence commented
      !if (tempw(ior) < ztmax) then
      !   LNQ = 0.61519
      !   W = LNQ*(ztmax - ztopt)
      !   X = (W**2*(1. + SQRT(1. + 40/W))**2)/400.
      !   FTA = ((ztmax-TEMPW(ior))/(ztmax - ztopt))**X
      !endif
      
      !   Umrechnung der Individienzahl in Biomasse (g*m-3)
      ROT = zooind(ior) * GROT/1000.
      
      !   Grundrespiration
      !   Temperaturabhängigkeit
      fTresR = thresR**(tempw(ior)-20.)
      respRg = zresge * fTresR
      
      !   Mortalitaetsrate
      !   Berechnung unter Beruecksichtigung der Futterkonz.
      !   des Sauerstoffgehalts und der Temperatur
      
      !   O2-Einfluss
      filo2 = vo2(ior) / dokrit
      
      !   Nahrungseinfluss: filtrierbare Algenbiomasse (in mgC/l)
      filabio = aki(ior) * Caki + agr(ior) * Cagr + abl(ior) * Cabl
      hconF = max(0., min(1., filabio / (filabio + FKs)))
      
      hcaki = aki(ior)
      if ((aki(ior) + agr(ior) + abl(ior)) < epsilon) then
         hcaki = 1.e-6
      else
         hcaki = aki(ior)
      endif
      hconki = hcaki    / (hcaki + agr(ior) + abl(ior))
      hconGr = agr(ior) / (hcaki + agr(ior) + abl(ior))
      hconBl = abl(ior) / (hcaki + agr(ior) + abl(ior))
      hconF = hconF * (zaki * hconki + zagr * hcongr + zabl * hconbl)
      
      hconM = min(hconF, filO2)
      
      !   Temperatureinfluss
      fTmoR = thmorR**(tempw(ior) -20.)
      !    morRot = mormax*exp(-hconM*Emort)
      morRot = (-0.14 * hconM**2 - 0.0093 * hconM + mormax) * fTmoR
      
      !   Zooplanktonwachstum
      
      !   Assimilationsrate
      
      if (hconF == 0.0) then
         zass = 0.0
      else
         zass = min(1., ASSmxR * exp(-EASS * hconF))
      endif
      ir_F      = irmax * hconF
      ProdRot   = (zass - respaR) * ir_F * fTing - respRg
      ir(ior)   = ir_F * fTing * tflie*ROT
      iras(ior) = ir_F * fTing
      
      !   ir - Ingestionsrate in mg/(l*h)
      !   zHNF - Aufnahmerate der HNF
      !   zBAC - Aufnahmerate der Bakterien
      !   ir/A - Filtriertes Wasservolumen l/h
      
      zHNF(ior) = 0.0
      zBAC(ior) = 0.0
      if (ir(ior) /= 0.0) then
         zHNF(ior) = ir(ior) * CHNF(ior) / (CHNF(ior) + agr(ior) + aki(ior) + abl(ior))
         ! TODO FG: commented the two lines below
         !zBAC(ior) = ir(ior)*BAC(ior)/(BAC(ior)+agr(ior)+aki(ior)+abl(ior))
         !zBAC(ior) = 0.0
      endif
      !   Ausgabe
      if (CHNF(ior) /= 0.0) HNFza(ior) = (zHNF(ior)/CHNF(ior))*24.
      
      ROTt = ROT * exp((ProdRot-morRot)*tflie) ! Rotatorienzunahme
      !!wy if(mstr==1)write(79,*)ior,ProdRot,zass,respaR,ir_F,fTing,respRg
      
      TGZoot = TGZoo(mstr,ior)
      if (isTGZoo) TGZoot = TGZoot * exp(ProdRot*0.20*tflie)
      
      dzres1(ior) = ROT  * (1. - exp(-respRg * tflie))
      ABSZO(ior)  = ROTt * (1. - exp(-morRot * tflie))
      dzres2(ior) = respaR * ir(ior)
      
      zexki(ior) = ir(ior) * (1. - zass) * hconki
      zexgr(ior) = ir(ior) * (1. - zass) * hconGr
      zexbl(ior) = ir(ior) * (1. - zass) * hconBl
      
      algzok(ior) = min(aki(ior) * zaki, ir(ior) * hconki)
      algzog(ior) = min(agr(ior) * zagr, ir(ior) * hconGr)
      algzob(ior) = min(abl(ior) * zabl, ir(ior) * hconBl)
      
      ! 2D-Modellierung
      if (nkzs(ior) /= 1) then
         do nkz = 1,nkzs(ior)
            if ((akiz(nkz,ior) + agrz(nkz,ior) + ablz(nkz,ior)) < epsilon) then
               hcaki = 1.e-6
            else
               hcaki = akiz(nkz,ior)
            endif
            hconki = hcaki/(hcaki+agrz(nkz,ior)+ablz(nkz,ior))
            hcongr = agrz(nkz,ior)/(hcaki+agrz(nkz,ior)+ablz(nkz,ior))
            hconbl = ablz(nkz,ior)/(hcaki+agrz(nkz,ior)+ablz(nkz,ior))
            
            algzkz(nkz,ior) = ir(ior) * hconki
            algzgz(nkz,ior) = ir(ior) * hcongr
            algzbz(nkz,ior) = ir(ior) * hconBl
         enddo
      endif
      
      zooint = ROTt * 1000. / GROT
      if (zooint < 0.0) then
         zooint_old = zooint
         delzoo = zooint - zooind(ior)
         zooint = zooind(ior) / (zooind(ior) + abs(delzoo)) * zooind(ior)
         call print_clipping("konsum", "zooint", zooint_old, zooint, "Ind/l")
      endif
      
      ! Ausgabeparameter
      rmuas(ior) = ProdRot - morRot
      rakr(ior)  = morRot   ! ras(ior)*respaR
      rbar(ior)  = respRg
      
   enddo ! Ende Knotenschleife
   
   zooind(anze + 1)     = zooint
   TGZoo(mstr,anze + 1) = TGZoot
   
   return
end subroutine konsum
