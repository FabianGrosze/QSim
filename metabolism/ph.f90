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

!> Berechnung des pH-Wertes aus dem m-Wert und der Kohlensäuresumme
!! @author Volker Kirchesch
!! @date 11.06.1993
subroutine ph(mw,pw,ca,lf,tempw,tflie,susn,bsbct,dalgki,dalggr,dalgak,dalgag,po2p,po2r,rau,vmitt,tiefe            &
              ,flae,vabfl,flag,elen,ior,anze,vph,elfL,CaL,qeinlL,iorLa,iorLe,ieinLs,ssalg,stind,albewg            &
              ,alberg,albewk,alberk,wge,abl,dalgbl,dalgab,IDWe,iwied,fkm,ij,resdr,dzres1,dzres2,aki,agr           &
              ,ilbuhn,eph,emw,elf,eca,vco2,qeinl,jiein,mstr,cpfad,rhyd,WLage,hWS,itags,monats,uhrz,azStrs,iphy    &
              ,Caki, Cagr, Cabl                                                                                   &
              ,kontroll,jjj)
   
   logical, intent(in)                  :: kontroll !< debugging
   integer, intent(in)                  :: jjj      !< debugging
   character (len = 2)                  :: cwert
   character (len = 255)                :: cpfad
   character (len = 275)                :: pfadstring
   integer                              :: anze, azStrs
   integer, dimension(azStrs)           :: ieinLs
   integer, dimension(100)              :: iorLa, iorLe
   integer, dimension(1000)             :: flag, jiein
   integer, dimension(azStrs,1000)      :: IDWe
   real                                 :: MUE, lgk1, lgk2, lgkca, lgh, lgoh, moco2, mohco3, moco3, mwt,lft
   real                                 :: oh, h, k1, k2, mgco2, mghco3, mgco3, kca, moca
   real, dimension(20)                  :: wge
   real, dimension(100)                 :: eph, emw, elf, elfL, caL, eca, qeinl, qeinlL
   real, dimension(1000)                :: mw, pw, tempw, ca, lf, vph, flae, vmitt, tiefe, rau, bsbct, dalgki
   real, dimension(1000)                :: dalggr, dalgak, dalgag, susn, po2p, po2r, elen, ssalg, vabfl
   real, dimension(1000)                :: stind, albewg, alberg, albewk, alberk, fkm, resdr, dzres1, dzres2
   real, dimension(1000)                :: aki, agr, abl, dalgbl, dalgab, vco2, rhyd
   real, dimension(azStrs,1000)         :: WLage, hWS
   
   Crot = 0.45
   CDR = 0.38
   
   !      write(pfadstring,'(2A)')trim(adjustl(cpfad)),'funk.dat' !!wy
   !      open(unit=555, file=pfadstring)
   !      read(555,'(a2)',end=66)cwert
   !      read(555,5555,end=66)iphy,iphyw,zwgmes
   ! 5555 format(i1,2x,i1,2x,f5.1)
   !   66 close (555)
   
   iein = 1
   
   ! #### Berücksichtigung der Linienquelle ####
   
   do ieinL = 1, ieinLs(mstr)
      do ior = 1,anze+1
         if (iorLe(ieinL) < ior)cycle
         if (iorLa(ieinL) <= ior .and. iorLe(ieinL)>=ior) then
            if (qeinlL(ieinL) <= 0.0) then
               elfL(ieinL) = 0.0
               caL(ieinL) = 0.0
            endif
            if (elfL(ieinL)>=0.0)                        &
                Lf(ior) = Lf(ior)+((elfL(ieinL)-Lf(ior))*qeinlL(ieinL)/flae(ior))*tflie*86400.  ! 1D
            if (CaL(ieinL)>=0.0)                         &
                Ca(ior) = Ca(ior)+((CaL(ieinL)-Ca(ior))*qeinlL(ieinL)/flae(ior))*tflie*86400.   ! 1D
         else
         endif
      enddo ! Ende Knotenschleife
   enddo   ! Ende Schleife Linienquellen
   do j = 1,anze+1  ! Beginn Schleife Ortspunkte
      ior = j
      
      ior_flag = 0
      if (flag(ior) == 6 .and. vabfl(ior) < 0.0.and.vabfl(ior+1) > 0.0) then
         ior = ior+1
         ior_flag = 1
      endif
      if (ilbuhn == 1) then
      else if (flag(ior) /= 4) then
      else                        ! Berücksichtigung der Einleitungen
         m = 1
         ihcQ = 0
         if (vabfl(ior-1) < 0.0 .and. vabfl(ior) < 0.0)m = -1
         if (vabfl(ior-1) < 0.0 .and. vabfl(ior) > 0.0)ihcQ = 1 ! Konzentration an der Einleitstelle
         ! ist gleich der Konzentration der Einleitung
         
         hcmw = mw(ior-m)     ! Umbenennen der benötigten Variablen; 1D
         hcca = ca(ior-m)
         hcvph = vph(ior-m)
         hclf = lf(ior-m)
         mue = 1.7e-5*hclf
         hk = (0.5*sqrt(mue))/(1.+1.4*sqrt(mue))
         lgh = hcvph-hk
         hcvh = 10**(-lgh)
         hcQ = vabfl(ior-m)
         if (hcQ < 0.0)hcQ = abs(hcQ)
         if (hcQ == 0.0 .or. ihcQ == 1)hcQ = 1.e-10
         
         do ji = 1,jiein(ior)   ! Beginn Einleitungsschleife
            hcQE = max(0.0,qeinl(iein))
            hcphE = eph(iein)
            if (hcphE < 0.0)hcphE = hcvph
            hclfE = elf(iein)
            if (hclfE < 0.0)hclfE = hclf
            mue = 1.7e-5*hclfE
            hk = (0.5*sqrt(mue))/(1.+1.4*sqrt(mue))
            lgh = hcphE-hk
            hchE = 10**(-lgh)
            hcmwE = emw(iein)
            if (hcmwE < 0.0)hcmwE = hcmw
            hccaE = eca(iein)
            if (hccaE < 0.0)hccaE = hcca
            vhneu = (hcQ*hcvh+hcQE*hchE)/(hcQ+hcQE)
            mw(ior) = (hcQ*hcmw+hcQE*hcmwE)/(hcQ+hcQE)
            lf(ior) = (hcQ*hclf+hcQE*hclfE)/(hcQ+hcQE)
            ca(ior) = (hcQ*hcca+hcQE*hccaE)/(hcQ+hcQE)
            mue = 1.7e-5*lf(ior)
            hk = (0.5*sqrt(mue))/(1.+1.4*sqrt(mue))
            vph(ior) = log10(vhneu)
            vph(ior) = (-1.*vph(ior))+hk
            !
            hcQ = hcQ+qeinl(iein)
            iein = iein+1
            hcmw = mw(ior)
            hcca = ca(ior)
            hcvph = vph(ior)
            hclf = lf(ior)
            call pwert(mw(ior),vph(ior),lf(ior),tempw(ior),pw(ior))
         enddo                        ! Ende Einleitungsschleife
         
         if (ior_flag == 1) then
            iein = iein - jiein(ior)
            ior = ior-1
            vph(ior) = vph(ior+1)
            mw(ior) = mw(ior+1)
            lf(ior) = lf(ior+1)
            ca(ior) = ca(ior+1)
         endif
      endif                               ! Ende Einleitungs-flag
      if (ior > 1) then
         ca(ior-1) = cat
         mw(ior-1) = mwt
         pw(ior-1) = pwt
         lf(ior-1) = lft
         vph(ior-1) = vpht
      endif
      if ((iphy < 1) .or. (iphy > 4)) then
         write(*,*)'ph: aeration flag iphy',iphy,' out of bounds ior,mstr = ',ior,mstr
         stop 17
      endif
      cat = ca(ior)
      mwt = mw(ior)
      pwt = pw(ior)
      lft = lf(ior)
      vpht = vph(ior)
      call ph_kern( mwt,pwt,cat,lft,tempw(ior),vpht,vco2(ior)                                    &
                   ,tflie,rau(ior),vmitt(ior),tiefe(ior),rhyd(ior),flae(ior)                     &
                   ,wge(IDWe(mstr,ior)),WLage(mstr,ior),hWS(mstr,ior),iphy                       &
                   ,bsbct(ior),resdr(ior),dzres1(ior),dzres2(ior)                                &
                   ,dalgki(ior),dalggr(ior),dalgbl(ior),dalgak(ior),dalgag(ior),dalgab(ior)      &
                   ,Caki, Cagr, Cabl                                                             &
                   ,alberg(ior),alberk(ior),albewg(ior),albewk(ior)                              &
                   ,susn(ior),po2p(ior),po2r(ior),ssalg(ior),stind(ior)                          &
                   ,kontroll ,jjj )
   enddo   ! Ende Schleife Ortspunkte
   ca(anze+1) = cat
   mw(anze+1) = mwt
   pw(anze+1) = pwt
   lf(anze+1) = lft
   vph(anze+1) = vpht
   
end subroutine ph
