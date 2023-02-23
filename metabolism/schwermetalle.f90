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
!> Berechnung der Schwermetallkonzentrationen
!!
!! Cd, Zn, Cu, Ni, As, Pb, Cr, Fe, Hg, Mn, U
!!
!! @author Volker Kirchesch
!! @date 22.07.2019
subroutine schwermetalle(vabfl,qeinl,mstr,flag,anze,anzZeit,jiein,azStr,ieros,iformVert,ianze_max      &
                         ,hglZn,hgsZn,egsZn,eglZn,ZnSed       &
                         ,hglCad,hgsCad,egsCad,eglCad,CadSed  &
                         ,hglCu,hgsCu,egsCu,eglCu,CuSed       &
                         ,hglNi,hgsNi,egsNi,eglNi,NiSed       &
                         ,hglAs,hgsAs,egsAs,eglAs,AsSed       &
                         ,hglPb,hgsPb,egsPb,eglPb,PbSed       &
                         ,hglCr,hgsCr,egsCr,eglCr,CrSed       &
                         ,hglFe,hgsFe,egsFe,eglFe,FeSed       &
                         ,hglHg,hgsHg,egsHg,eglHg,HgSed       &
                         ,hglMn,hgsMn,egsMn,eglMn,MnSed       &
                         ,hglU,hgsU,egsU,eglU,USed            &
                         ,sedss,sedalk,sedalb,sedalg,hssalg,SSalg,ess,hph,vph,eph,SSeros       &
                         ,ilang,iwied,kontroll,jjj)
   use aparam
   use allodim
   implicit none
   
   integer                             :: azStr, anze,ieros,ilang,iformVert,ianze_max,mstr
   integer                             :: iein,iwied,ior,m,ihcq,ior_flag,ji,j
   real                                :: hcq,hcQE,hcss,hcph
   integer, dimension(ialloc2)         :: flag, jiein
   real                                :: hcgsCad,hcglCad,hcgsZn,hcglZn,hcgsCu,hcglCu,hcgsNi,hcglNi,hcgsAs,hcglAs
   real                                :: hcgsPb,hcglPb,hcgsCr,hcglCr,hcgsFe,hcglFe,hcgsHg,hcglHg,hcgsMn,hcglMn,hcgsU,hcglU
   real                                :: hcgsCadE,hcglCadE,hcgsZnE,hcglZnE,hcgsCuE,hcglCuE,hcgsNiE,hcglNiE
   real                                :: hcgsAsE,hcglAsE,hcgsPbE,hcglPbE,hcgsCrE,hcglCrE,hcgsFeE,hcglFeE
   real                                :: hcgsHgE,hcglHgE,hcgsMnE,hcglMnE,hcgsUE,hcglUE
   real                                :: VTKoeff_Zn, VTKoeff_Cad, VTKoeff_Cu, VTKoeff_Ni
   real                                :: VTKoeff_As, VTKoeff_Pb, VTKoeff_Cr, VTKoeff_Fe
   real                                :: VTKoeff_Hg, VTKoeff_Mn, VTKoeff_U
   real, dimension(ialloc1)            :: qeinl, eph, ess
   real, dimension(ialloc2)            :: vabfl, sedss, SSalg, vph, sedalk, sedalb, sedalg
   real, dimension(azStrs,ialloc2)     :: hssalg, hph
   real, dimension(ialloc2)            :: SSeros
   real, dimension(azStrs,ialloc1)     :: egsZn, eglZn, egsCad, eglCad, egsCu, eglCu, egsNi, eglNi
   real, dimension(azStrs,ialloc1)     :: egsAs, eglAs, egsPb, eglPb, egsCr, eglCr, egsFe, eglFe
   real, dimension(azStrs,ialloc1)     :: egsHg, eglHg, egsMn, eglMn, egsU, eglU
   real, dimension(azStrs,ialloc2)     :: hglZn, hgsZn, hglCad, hgsCad, hglCu, hgsCu, hglNi, hgsNi
   real, dimension(azStrs,ialloc2)     :: hglAs, hgsAs, hglPb, hgsPb, hglCr, hgsCr, hglFe, hgsFe
   real, dimension(azStrs,ialloc2)     :: hglHg, hgsHg, hglMn, hgsMn, hglU, hgsU
   real                                :: hgsZnt,hgsCadt,hgsCut,hgsNit,hgsAst,hgsPbt,hgsCrt,hgsFet,hgsHgt,hgsMnt,hgsUt
   real                                :: hglZnt,hglCadt,hglCut,hglNit,hglAst,hglPbt,hglCrt,hglFet,hglHgt,hglMnt,hglUt
   real, dimension(azStrs,ialloc2)     :: ZnSed, CadSed, CuSed, NiSed
   real, dimension(azStrs,ialloc2)     :: AsSed, PbSed, CrSed, FeSed
   real, dimension(azStrs,ialloc2)     :: HgSed, MnSed, USed
   integer, dimension(azStrs,ialloc2)  :: anzZeit
   logical, intent(in)                 :: kontroll !< debugging
   integer, intent(in)                 :: jjj      !< debugging
   
   external :: sedimentbelastung, verteilungskoeff, schwermetalle_kern
   
   
   iein = 1
   if (iwied == 0) then
      ! initialize timecounter at simulation start
      if (azStr == 1) then
         anzZeit(:,:) = 0
      endif
      do ior = 1,anze+1
         ZnSed(mstr,ior) = 0.0
         CadSed(mstr,ior) = 0.0
         CuSed(mstr,ior) = 0.0
         NiSed(mstr,ior) = 0.0
         AsSed(mstr,ior) = 0.0
         PbSed(mstr,ior) = 0.0
         CrSed(mstr,ior) = 0.0
         FeSed(mstr,ior) = 0.0
         HgSed(mstr,ior) = 0.0
         MnSed(mstr,ior) = 0.0
         USed(mstr,ior) = 0.0
         call Sedimentbelastung(SSalg(ior)                                          &
                                ,hgsZn(mstr,ior),hglZn(mstr,ior),ZnSed(mstr,ior)    &
                                ,hgsCad(mstr,ior),hglCad(mstr,ior),CadSed(mstr,ior) &
                                ,hgsCu(mstr,ior),hglCu(mstr,ior),CuSed(mstr,ior)    &
                                ,hgsNi(mstr,ior),hglNi(mstr,ior),NiSed(mstr,ior)    &
                                ,hgsAs(mstr,ior),hglAs(mstr,ior),AsSed(mstr,ior)    &
                                ,hgsPb(mstr,ior),hglPb(mstr,ior),PbSed(mstr,ior)    &
                                ,hgsCr(mstr,ior),hglCr(mstr,ior),CrSed(mstr,ior)    &
                                ,hgsFe(mstr,ior),hglFe(mstr,ior),FeSed(mstr,ior)    &
                                ,hgsHg(mstr,ior),hglHg(mstr,ior),HgSed(mstr,ior)    &
                                ,hgsMn(mstr,ior),hglMn(mstr,ior),MnSed(mstr,ior)    &
                                ,hgsU(mstr,ior),hglU(mstr,ior),USed(mstr,ior)       &
                                ,anzZeit(mstr,ior),SSeros(ior)                      &
                                ,kontroll ,jjj)
      enddo ! ior = 1,anze+1
   endif ! iwied==0
   
   do j = 1,anze+1 ! Schleife longitudinale Gitterpunkte
      ior = j
      
      if (ieros == 0)SSeros(ior) = 0.0
      if (ilang == 1) then
         call Sedimentbelastung(SSalg(ior)                                                &
                                ,hgsZn(mstr,ior),hglZn(mstr,ior),ZnSed(mstr,ior)          &
                                ,hgsCad(mstr,ior),hglCad(mstr,ior),CadSed(mstr,ior)       &
                                ,hgsCu(mstr,ior),hglCu(mstr,ior),CuSed(mstr,ior)          &
                                ,hgsNi(mstr,ior),hglNi(mstr,ior),NiSed(mstr,ior)          &
                                ,hgsAs(mstr,ior),hglAs(mstr,ior),AsSed(mstr,ior)          &
                                ,hgsPb(mstr,ior),hglPb(mstr,ior),PbSed(mstr,ior)          &
                                ,hgsCr(mstr,ior),hglCr(mstr,ior),CrSed(mstr,ior)          &
                                ,hgsFe(mstr,ior),hglFe(mstr,ior),FeSed(mstr,ior)          &
                                ,hgsHg(mstr,ior),hglHg(mstr,ior),HgSed(mstr,ior)          &
                                ,hgsMn(mstr,ior),hglMn(mstr,ior),MnSed(mstr,ior)          &
                                ,hgsU(mstr,ior),hglU(mstr,ior),USed(mstr,ior)             &
                                ,anzZeit(mstr,ior),SSeros(ior)   &
                                ,kontroll ,jjj)
      endif
      ior_flag = 0
      if (flag(ior) == 6 .and. vabfl(ior) < 0.0 .and. vabfl(ior+1) > 0.0) then
         ior = ior+1
         ior_flag = 1
      endif
      
      if (flag(ior) == 4) then             ! Berücksichtigung der Einleitungen
         m = 1
         ihcQ = 0
         if (vabfl(ior-1) < 0.0 .and. vabfl(ior) < 0.0)m = -1
         if (vabfl(ior-1) < 0.0 .and. vabfl(ior) > 0.0)ihcQ = 1 ! Konzentration an der Einleitstelle
         ! ist gleich der Konzentration der Einleitung
         hcgsCad = hgsCad(mstr,ior-m)        ! Umbenennen der benötigten Variablen; 1D
         hcglCad = hglCad(mstr,ior-m)
         hcgsZn = hgsZn(mstr,ior-m)
         hcglZn = hglZn(mstr,ior-m)
         hcgsCu = hgsCu(mstr,ior-m)
         hcglCu = hglCu(mstr,ior-m)
         hcgsNi = hgsNi(mstr,ior-m)
         hcglNi = hglNi(mstr,ior-m)
         hcgsAs = hgsAs(mstr,ior-m)
         hcglAs = hglAs(mstr,ior-m)
         hcgsPb = hgsPb(mstr,ior-m)
         hcglPb = hglPb(mstr,ior-m)
         hcgsCr = hgsCr(mstr,ior-m)
         hcglCr = hglCr(mstr,ior-m)
         hcgsFe = hgsFe(mstr,ior-m)
         hcglFe = hglFe(mstr,ior-m)
         hcgsHg = hgsHg(mstr,ior-m)
         hcglHg = hglHg(mstr,ior-m)
         hcgsMn = hgsMn(mstr,ior-m)
         hcglMn = hglMn(mstr,ior-m)
         hcgsU = hgsU(mstr,ior-m)
         hcglU = hglU(mstr,ior-m)
         hcQ = vabfl(ior-m)
         if (hcQ < 0.0)hcQ = abs(hcQ)
         if (hcQ == 0.0 .or. ihcQ == 1)hcQ = 1.e-10
         
         do ji = 1,jiein(ior)   ! Beginn Einleitungsschleife
            hcQE = max(0.0,qeinl(iein))
            hcgsCadE = egsCad(mstr,iein)
            hcglCadE = eglCad(mstr,iein)
            hcgsZnE = egsZn(mstr,iein)
            hcglZnE = eglZn(mstr,iein)
            hcgsCuE = egsCu(mstr,iein)
            hcglCuE = eglCu(mstr,iein)
            hcgsNiE = egsNi(mstr,iein)
            hcglNiE = eglNi(mstr,iein)
            hcgsAsE = egsAs(mstr,iein)
            hcglAsE = eglAs(mstr,iein)
            hcgsPbE = egsPb(mstr,iein)
            hcglPbE = eglPb(mstr,iein)
            hcgsCrE = egsCr(mstr,iein)
            hcglCrE = eglCr(mstr,iein)
            hcgsFeE = egsFe(mstr,iein)
            hcglFeE = eglFe(mstr,iein)
            hcgsHgE = egsHg(mstr,iein)
            hcglHgE = eglHg(mstr,iein)
            hcgsMnE = egsMn(mstr,iein)
            hcglMnE = eglMn(mstr,iein)
            hcgsUE = egsU(mstr,iein)
            hcglUE = eglU(mstr,iein)
            
            hcSS = min(100.,ess(iein))
            hcph = eph(iein)
            if (hcSS < 0.0)hcss = min(100.,SSalg(ior-m))
            if (hcph < 0.0)hcph = vph(ior-m)
            call Verteilungskoeff( hcSS, hcph  &
                                  ,VTKoeff_Zn,VTKoeff_Cu,VTKoeff_Cad,VTKoeff_Ni,VTKoeff_As,VTKoeff_Pb   &
                                  ,VTKoeff_Cr,VTKoeff_Fe,VTKoeff_Hg ,VTKoeff_Mn,VTKoeff_U               &
                                  ,iformVert, kontroll,jjj,0)
            if (hcgsCadE < 0.0 .and. hcglCadE < 0.0) then
               hcgsCadE = hcgsCad
               hcglCadE = hcglCad
            endif
            if (hcgsCadE > 0.0 .and. hcglCadE <= 0.0) then
               hcglCadE = hcgsCadE/(1.+VTKoeff_Cad*hcSS/1000.)
            else if (hcgsCadE <= 0.0 .and. hcglCadE > 0.0) then
               hcgsCadE = hcglCadE*(1+VTKoeff_Cad*hcSS/1000.)
            else if (hcgsCadE > 0.0 .and. hcglCadE == hcgsCadE) then
               hcglCadE = hcgsCadE/(1.+VTKoeff_Cad*hcSS/1000.)
            else if (hcgsCadE < hcglCadE) then
               hcgsCadE = hcglCadE*(1+VTKoeff_Cad*hcSS/1000.)
            endif
            if (hcgsZnE < 0.0 .and. hcglZnE < 0.0) then
               hcgsZnE = hcgsZn
               hcglZnE = hcglZn
            endif
            if (hcgsZnE > 0.0 .and. hcglZnE <= 0.0) then
               hcglZnE = hcgsZnE/(1.+VTKoeff_Zn*hcSS/1000.)
            else if (hcgsZnE <= 0.0 .and. hcglZnE > 0.0) then
               hcgsZnE = hcglZnE*(1+VTKoeff_Zn*hcSS/1000.)
            else if (hcgsZnE > 0.0 .and. hcglZnE == hcgsZnE) then
               hcglZnE = hcgsZnE/(1.+VTKoeff_Zn*hcSS/1000.)
            else if (hcgsZnE < hcglZnE) then
               hcgsZnE = hcglZnE*(1+VTKoeff_Zn*hcSS/1000.)
            endif
            if (hcgsCuE < 0.0 .and. hcglCuE < 0.0) then
               hcgsCuE = hcgsCu
               hcglCuE = hcglCu
            endif
            if (hcgsCuE > 0.0 .and. hcglCuE <= 0.0) then
               hcglCuE = hcgsCuE/(1.+VTKoeff_Cu*hcSS/1000.)
            else if (hcgsCuE <= 0.0 .and. hcglCuE > 0.0) then
               hcgsCuE = hcglCuE*(1+VTKoeff_Cu*hcSS/1000.)
            else if (hcgsCuE > 0.0 .and. hcglCuE == hcgsCuE) then
               hcglCuE = hcgsCuE/(1.+VTKoeff_Cu*hcSS/1000.)
            else if (hcgsCuE < hcglCuE) then
               hcgsCuE = hcglCuE*(1+VTKoeff_Cu*hcSS/1000.)
            endif
            if (hcgsNiE < 0.0 .and. hcglNiE < 0.0) then
               hcgsNiE = hcgsNi
               hcglNiE = hcglNi
            endif
            if (hcgsNiE > 0.0 .and. hcglNiE <= 0.0) then
               hcglNiE = hcgsNiE/(1.+VTKoeff_Ni*hcSS/1000.)
            else if (hcgsNiE <= 0.0 .and. hcglNiE > 0.0) then
               hcgsNiE = hcglNiE*(1+VTKoeff_Ni*hcSS/1000.)
            else if (hcgsNiE > 0.0 .and. hcglNiE == hcgsNiE) then
               hcglNiE = hcgsNiE/(1.+VTKoeff_Ni*hcSS/1000.)
            else if (hcgsNiE < hcglNiE) then
               hcgsNiE = hcglNiE*(1+VTKoeff_Ni*hcSS/1000.)
            endif
            
            if (hcgsAsE < 0.0 .and. hcglAsE < 0.0) then
               hcgsAsE = hcgsAs
               hcglAsE = hcglAs
            endif
            if (hcgsAsE > 0.0 .and. hcglAsE <= 0.0) then
               hcglAsE = hcgsAsE/(1.+VTKoeff_As*hcSS/1000.)
            else if (hcgsAsE <= 0.0 .and. hcglAsE > 0.0) then
               hcgsAsE = hcglAsE*(1+VTKoeff_As*hcSS/1000.)
            else if (hcgsAsE > 0.0 .and. hcglAsE == hcgsAsE) then
               hcglAsE = hcgsAsE/(1.+VTKoeff_As*hcSS/1000.)
            else if (hcgsAsE < hcglAsE) then
               hcgsAsE = hcglAsE*(1+VTKoeff_As*hcSS/1000.)
            endif
            if (hcgsPbE < 0.0 .and. hcglPbE < 0.0) then
               hcgsPbE = hcgsPb
               hcglPbE = hcglPb
            endif
            if (hcgsPbE > 0.0 .and. hcglPbE <= 0.0) then
               hcglPbE = hcgsPbE/(1.+VTKoeff_Pb*hcSS/1000.)
            else if (hcgsPbE <= 0.0 .and. hcglPbE > 0.0) then
               hcgsPbE = hcglPbE*(1+VTKoeff_Pb*hcSS/1000.)
            else if (hcgsPbE > 0.0 .and. hcglPbE == hcgsPbE) then
               hcglPbE = hcgsPbE/(1.+VTKoeff_Pb*hcSS/1000.)
            else if (hcgsPbE < hcglPbE) then
               hcgsPbE = hcglPbE*(1+VTKoeff_Pb*hcSS/1000.)
            endif
            if (hcgsCrE < 0.0 .and. hcglCrE < 0.0) then
               hcgsCrE = hcgsCr
               hcglCrE = hcglCr
            endif
            if (hcgsCrE > 0.0 .and. hcglCrE <= 0.0) then
               hcglCrE = hcgsCrE/(1.+VTKoeff_Cr*hcSS/1000.)
            else if (hcgsCrE <= 0.0 .and. hcglCrE > 0.0) then
               hcgsCrE = hcglCrE*(1+VTKoeff_Cr*hcSS/1000.)
            else if (hcgsCrE > 0.0 .and. hcglCrE == hcgsCrE) then
               hcglCrE = hcgsCrE/(1.+VTKoeff_Cr*hcSS/1000.)
            else if (hcgsCrE < hcglCrE) then
               hcgsCrE = hcglCrE*(1+VTKoeff_Cr*hcSS/1000.)
            endif
            if (hcgsFeE < 0.0 .and. hcglFeE < 0.0) then
               hcgsFeE = hcgsFe
               hcglFeE = hcglFe
            endif
            if (hcgsFeE > 0.0 .and. hcglFeE <= 0.0) then
               hcglFeE = hcgsFeE/(1.+VTKoeff_Fe*hcSS/1000.)
            else if (hcgsFeE <= 0.0 .and. hcglFeE > 0.0) then
               hcgsFeE = hcglFeE*(1+VTKoeff_Fe*hcSS/1000.)
            else if (hcgsFeE > 0.0 .and. hcglFeE == hcgsFeE) then
               hcglFeE = hcgsFeE/(1.+VTKoeff_Fe*hcSS/1000.)
            else if (hcgsFeE < hcglFeE) then
               hcgsFeE = hcglFeE*(1+VTKoeff_Fe*hcSS/1000.)
            endif
            if (hcgsHgE < 0.0 .and. hcglHgE < 0.0) then
               hcgsHgE = hcgsHg
               hcglHgE = hcglHg
            endif
            if (hcgsHgE > 0.0 .and. hcglHgE <= 0.0) then
               hcglHgE = hcgsHgE/(1.+VTKoeff_Hg*hcSS/1000.)
            else if (hcgsHgE <= 0.0 .and. hcglHgE > 0.0) then
               hcgsHgE = hcglHgE*(1+VTKoeff_Hg*hcSS/1000.)
            else if (hcgsHgE > 0.0 .and. hcglHgE == hcgsHgE) then
               hcglHgE = hcgsHgE/(1.+VTKoeff_Hg*hcSS/1000.)
            else if (hcgsHgE < hcglHgE) then
               hcgsHgE = hcglHgE*(1+VTKoeff_Hg*hcSS/1000.)
            endif
            if (hcgsMnE < 0.0 .and. hcglMnE < 0.0) then
               hcgsMnE = hcgsMn
               hcglMnE = hcglMn
            endif
            if (hcgsMnE > 0.0 .and. hcglMnE <= 0.0) then
               hcglMnE = hcgsMnE/(1.+VTKoeff_Mn*hcSS/1000.)
            else if (hcgsMnE <= 0.0 .and. hcglMnE > 0.0) then
               hcgsMnE = hcglMnE*(1+VTKoeff_Mn*hcSS/1000.)
            else if (hcgsMnE > 0.0 .and. hcglMnE == hcgsMnE) then
               hcglMnE = hcgsMnE/(1.+VTKoeff_Mn*hcSS/1000.)
            else if (hcgsMnE < hcglMnE) then
               hcgsMnE = hcglMnE*(1+VTKoeff_Mn*hcSS/1000.)
            endif
            if (hcgsUE < 0.0 .and. hcglUE < 0.0) then
               hcgsUE = hcgsU
               hcglUE = hcglU
            endif
            if (hcgsUE > 0.0 .and. hcglUE <= 0.0) then
               hcglUE = hcgsUE/(1.+VTKoeff_U*hcSS/1000.)
            else if (hcgsUE <= 0.0 .and. hcglUE > 0.0) then
               hcgsUE = hcglUE*(1+VTKoeff_U*hcSS/1000.)
            else if (hcgsUE > 0.0 .and. hcglUE == hcgsUE) then
               hcglUE = hcgsUE/(1.+VTKoeff_U*hcSS/1000.)
            else if (hcgsUE < hcglUE) then
               hcgsUE = hcglUE*(1+VTKoeff_U*hcSS/1000.)
            endif
            hgsCad(mstr,ior) = (hcQ*hcgsCad+hcQE*hcgsCadE)/(hcQ+hcQE)
            hglCad(mstr,ior) = (hcQ*hcglCad+hcQE*hcglCadE)/(hcQ+hcQE)
            hgsZn(mstr,ior) = (hcQ*hcgsZn+hcQE*hcgsZnE)/(hcQ+hcQE)
            hglZn(mstr,ior) = (hcQ*hcglZn+hcQE*hcglZnE)/(hcQ+hcQE)
            hgsCu(mstr,ior) = (hcQ*hcgsCu+hcQE*hcgsCuE)/(hcQ+hcQE)
            hglCu(mstr,ior) = (hcQ*hcglCu+hcQE*hcglCuE)/(hcQ+hcQE)
            hgsNi(mstr,ior) = (hcQ*hcgsNi+hcQE*hcgsNiE)/(hcQ+hcQE)
            hglNi(mstr,ior) = (hcQ*hcglNi+hcQE*hcglNiE)/(hcQ+hcQE)
            hgsAs(mstr,ior) = (hcQ*hcgsAs+hcQE*hcgsAsE)/(hcQ+hcQE)
            hglAs(mstr,ior) = (hcQ*hcglAs+hcQE*hcglAsE)/(hcQ+hcQE)
            hgsPb(mstr,ior) = (hcQ*hcgsPb+hcQE*hcgsPbE)/(hcQ+hcQE)
            hglPb(mstr,ior) = (hcQ*hcglPb+hcQE*hcglPbE)/(hcQ+hcQE)
            hgsCr(mstr,ior) = (hcQ*hcgsCr+hcQE*hcgsCrE)/(hcQ+hcQE)
            hglCr(mstr,ior) = (hcQ*hcglCr+hcQE*hcglCrE)/(hcQ+hcQE)
            hgsFe(mstr,ior) = (hcQ*hcgsFe+hcQE*hcgsFeE)/(hcQ+hcQE)
            hglFe(mstr,ior) = (hcQ*hcglFe+hcQE*hcglFeE)/(hcQ+hcQE)
            hgsHg(mstr,ior) = (hcQ*hcgsHg+hcQE*hcgsHgE)/(hcQ+hcQE)
            hglHg(mstr,ior) = (hcQ*hcglHg+hcQE*hcglHgE)/(hcQ+hcQE)
            hgsMn(mstr,ior) = (hcQ*hcgsMn+hcQE*hcgsMnE)/(hcQ+hcQE)
            hglMn(mstr,ior) = (hcQ*hcglMn+hcQE*hcglMnE)/(hcQ+hcQE)
            hgsU(mstr,ior) = (hcQ*hcgsU+hcQE*hcgsUE)/(hcQ+hcQE)
            hglU(mstr,ior) = (hcQ*hcglU+hcQE*hcglUE)/(hcQ+hcQE)
            hcQ = hcQ+qeinl(iein)
            iein = iein+1
            hcgsCad = hgsCad(mstr,ior)
            hcglCad = hglCad(mstr,ior)
            hcgsZn = hgsZn(mstr,ior)
            hcglZn = hglZn(mstr,ior)
            hcgsCu = hgsCu(mstr,ior)
            hcglCu = hglCu(mstr,ior)
            hcgsNi = hgsNi(mstr,ior)
            hcglNi = hglNi(mstr,ior)
            hcgsAs = hgsAs(mstr,ior)
            hcglAs = hglAs(mstr,ior)
            hcgsPb = hgsPb(mstr,ior)
            hcglPb = hglPb(mstr,ior)
            hcgsCr = hgsCr(mstr,ior)
            hcglCr = hglCr(mstr,ior)
            hcgsFe = hgsFe(mstr,ior)
            hcglFe = hglFe(mstr,ior)
            hcgsHg = hgsHg(mstr,ior)
            hcglHg = hglHg(mstr,ior)
            hcgsMn = hgsMn(mstr,ior)
            hcglMn = hglMn(mstr,ior)
            hcgsU = hgsU(mstr,ior)
            hcglU = hglU(mstr,ior)
            
         enddo                        ! Ende Einleitungsschleife
         if (ior_flag == 1) then
            iein = iein - jiein(ior)
            ior = ior-1
            hgsCad(mstr,ior) = hgsCad(mstr,ior+1)
            hglCad(mstr,ior) = hglCad(mstr,ior+1)
            hgsZn(mstr,ior) = hgsZn(mstr,ior+1)
            hglZn(mstr,ior) = hglZn(mstr,ior+1)
            hgsCu(mstr,ior) = hgsCu(mstr,ior+1)
            hglCu(mstr,ior) = hglCu(mstr,ior+1)
            hgsNi(mstr,ior) = hgsNi(mstr,ior+1)
            hglNi(mstr,ior) = hglNi(mstr,ior+1)
            hgsAs(mstr,ior) = hgsAs(mstr,ior+1)
            hglAs(mstr,ior) = hglAs(mstr,ior+1)
            hgsPb(mstr,ior) = hgsPb(mstr,ior+1)
            hglPb(mstr,ior) = hglPb(mstr,ior+1)
            hgsCr(mstr,ior) = hgsCr(mstr,ior+1)
            hglCr(mstr,ior) = hglCr(mstr,ior+1)
            hgsFe(mstr,ior) = hgsFe(mstr,ior+1)
            hglFe(mstr,ior) = hglFe(mstr,ior+1)
            hgsHg(mstr,ior) = hgsHg(mstr,ior+1)
            hglHg(mstr,ior) = hglHg(mstr,ior+1)
            hgsMn(mstr,ior) = hgsMn(mstr,ior+1)
            hglMn(mstr,ior) = hglMn(mstr,ior+1)
            hgsU(mstr,ior) = hgsU(mstr,ior+1)
            hglU(mstr,ior) = hglU(mstr,ior+1)
         endif
      endif                               ! Ende Einleitungs-flag
      
      
      
      if (ior > 1) then
         hglZn(mstr,ior-1) = hglZnt
         hgsZn(mstr,ior-1) = hgsZnt
         hglCad(mstr,ior-1) = hglCadt
         hgsCad(mstr,ior-1) = hgsCadt
         hglCu(mstr,ior-1) = hglCut
         hgsCu(mstr,ior-1) = hgsCut
         hglNi(mstr,ior-1) = hglNit
         hgsNi(mstr,ior-1) = hgsNit
         hglAs(mstr,ior-1) = hglAst
         hgsAs(mstr,ior-1) = hgsAst
         hglPb(mstr,ior-1) = hglPbt
         hgsPb(mstr,ior-1) = hgsPbt
         hglCr(mstr,ior-1) = hglCrt
         hgsCr(mstr,ior-1) = hgsCrt
         hglFe(mstr,ior-1) = hglFet
         hgsFe(mstr,ior-1) = hgsFet
         hglHg(mstr,ior-1) = hglHgt
         hgsHg(mstr,ior-1) = hgsHgt
         hglMn(mstr,ior-1) = hglMnt
         hgsMn(mstr,ior-1) = hgsMnt
         hglU(mstr,ior-1) = hglUt
         hgsU(mstr,ior-1) = hgsUt
      endif
      hglZnt = hglZn(mstr,ior)
      hgsZnt = hgsZn(mstr,ior)
      hglCadt = hglCad(mstr,ior)
      hgsCadt = hgsCad(mstr,ior)
      hglCut = hglCu(mstr,ior)
      hgsCut = hgsCu(mstr,ior)
      hglNit = hglNi(mstr,ior)
      hgsNit = hgsNi(mstr,ior)
      hglAst = hglAs(mstr,ior)
      hgsAst = hgsAs(mstr,ior)
      hglPbt = hglPb(mstr,ior)
      hgsPbt = hgsPb(mstr,ior)
      hglCrt = hglCr(mstr,ior)
      hgsCrt = hgsCr(mstr,ior)
      hglFet = hglFe(mstr,ior)
      hgsFet = hgsFe(mstr,ior)
      hglHgt = hglHg(mstr,ior)
      hgsHgt = hgsHg(mstr,ior)
      hglMnt = hglMn(mstr,ior)
      hgsMnt = hgsMn(mstr,ior)
      hglUt = hglU(mstr,ior)
      hgsUt = hgsU(mstr,ior)
      
      !if(ior.eq.1)print*,'schwermetalle_kern mstr=',mstr
      call schwermetalle_kern(hssalg(mstr,ior),SSalg(ior),hph(mstr,ior),vph(ior),SSeros(ior),iformVert &
                              ,anzZeit(mstr,ior),sedss(ior),sedalk(ior),sedalg(ior),sedalb(ior)        &
                              ,hgsZnt,hglZnt,hgsCadt,hglCadt               &
                              ,hgsCut,hglCut,hgsNit,hglNit                 &
                              ,hgsAst,hglAst,hgsPbt,hglPbt                 &
                              ,hgsCrt,hglCrt,hgsFet,hglFet                 &
                              ,hgsHgt,hglHgt,hgsMnt,hglMnt                 &
                              ,hgsUt,hglUt                                 &
                              ,.false.,ior,0)
   enddo     ! Ende Schleife longitudinale Gitterpunkte
   
end subroutine schwermetalle
