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

!> Berechnung der Schwebstoffkonzentration
!! @author Volker Kirchesch
!! @date 22.09.2011
subroutine SCHWEB(zooind,dorgSS,ss,ssalg,tiefe,rau,tflie,VMITT,flae,flag,elen,ior,anze,ess              &
                  ,ssL,qeinl,qeinlL,vabfl,dkimor,dgrmor,abszo,zexki,zexgr,iorLa,iorLe,ieinls            &
                  ,abl,zexbl,dblmor,drfaeb,jiein,aki,agr,ssdr,drfaek,drfaeg,drfaes,fssgr,sedss,sedSS_MQ &
                  ,fssgrs,tausc,ischif,ilbuhn,fkm,ieros,iwied,echla,vkigr,akbcm,agbcm,antbl,abbcm       &
                  ,ezind,mstr,itags,monats,uhrz                                                         &
                  ,kontroll,jjj)
   
   ! SS     ORG. UND ANORG. SCHWEBSTOFFE(OHNE ALGEN UND ZOOPLANKTER
   ! SSALG  GESAMTSCHWEBSTOFFE
   
   use allodim
   use aparam
   implicit none
   
   logical, intent(in)                  :: kontroll !< debugging
   integer, intent(in)                  :: jjj      !< debugging
   integer                              :: anze
   integer                              :: iein,mstr,itags,monats,ieinL,ior,j,ior_flag,ilbuhn,m,ihcq,iwied,ji,ieros
   real                                 :: tflie,uhrz
   real                                 :: hcss,hcsse,HCFSSG,HCQ,HCQE, akie, agre, able, hcfssgE, FSSGRS
   integer, dimension(ialloc1)          :: iorLa, iorLe
   integer, dimension(ialloc2)          :: flag, jiein, ischif
   real, dimension(ialloc1)             :: ess, echla, ezind, qeinl, ssL, qeinlL
   real, dimension(ialloc2)             :: zooind, ss, vabfl, ssalg, tiefe, rau, vmitt, flae, elen, zexki, zexgr, zexbl
   real, dimension(ialloc2)             :: fkm, dblmor ,drfaeb, abl, aki, agr, dkimor, dgrmor, abszo
   real, dimension(ialloc2), intent(in) :: dorgSS
   real, dimension(ialloc2)             :: ssdr, drfaek, drfaeg, drfaes, fssgr, sedss, vkigr, antbl, akbcm, agbcm, abbcm
   real, dimension(azStrs,ialloc2)      :: sedSS_MQ
   real, dimension(azStrs,ialloc2)      :: tausc
   integer, dimension(azStrs)           :: ieinLs
   
   external :: schweb_kern
   
   iein = 1
   
   ! is now defined in module aparam
   ! Cagr = 0.48
   ! Caki = 0.48
   ! Cabl = 0.48
   
   !....Berücksichtigung der Linienquelle
   
   do ieinL = 1, ieinLs(mstr)
      if (qeinlL(ieinL)>=0.0 .and. ssL(ieinL) == -1.)cycle
      do ior = 1,anze+1
         if (iorLe(ieinL) < ior)cycle
         if (iorLa(ieinL) <= ior .and. iorLe(ieinL)>=ior) then
            if (qeinlL(ieinL) <= 0.0)ssL(ieinL) = 0.0
            ss(ior) = ss(ior)+((ssL(ieinL)-ss(ior))*qeinlL(ieinL)/flae(ior))*tflie*86400.  ! 1D
         else
         endif
      enddo ! Ende Knotenschleife
   enddo   ! Ende Schleife Linienquellen
   do j = 1,anze+1                    ! Beginn Knotenschleife
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
         
         hcss = ss(ior-m)       ! Umbenennen der benötigten Variablen
         hcfssg = fssgr(ior-m)
         hcQ = vabfl(ior-m)
         if (hcQ < 0.0)hcQ = abs(hcQ)
         if (hcQ == 0.0 .or. ihcQ == 1)hcQ = 1.e-10
         
         do ji = 1,jiein(ior)   ! Beginn Einleitungsschleife
            hcQE = max(0.0,qeinl(iein))
            hcssE = ess(iein)
            if (hcssE < 0.0) then
               hcssE = hcss
            else
               if (echla(iein)>=0.0) then
                  akie = (echla(iein)*vkigr(ior-1)/1000.)*(akbcm(ior-1)/Caki)
                  agre = (echla(iein)*(1.-vkigr(ior-1)-antbl(ior-1))/1000.)*(agbcm(ior-1)/Cagr)
                  able = (echla(iein)*antbl(ior-1)/1000.)*(abbcm(ior-1)/Cabl)
                  hcssE = hcssE-akie-agre-able
               endif
               if (ezind(iein)>=0.0)hcssE = hcssE-(ezind(iein)*GROT/1000.)
            endif
            hcfssgE = fssgrs
            ss(ior) = (hcQ*hcss+hcQE*hcssE)/(hcQ+hcQE)
            fssgr(ior) = (hcQ*hcfssg+hcQE*hcfssgE)/(hcQ+hcQE)
            
            hcQ = hcQ+qeinl(iein)
            iein = iein+1
            hcss = ss(ior)
            hcfssg = fssgr(ior)
         enddo ! Ende Einleitungsschleife
         
         if (ior_flag == 1) then
            iein = iein - jiein(ior)
            ior = ior-1
            ss(ior) = ss(ior+1)
            fssgr(ior) = fssgr(ior+1)
         endif
      endif ! Ende Einleitungs-flag
      
      call schweb_kern(zooind(ior),dorgSS(ior),SS(ior),ssalg(ior),tiefe(ior)                               &
                      ,rau(ior),tflie,vmitt(ior)                                                           &
                      ,dkimor(ior),dgrmor(ior),abszo(ior),zexki(ior),zexgr(ior)                            &
                      ,abl(ior),zexbl(ior),dblmor(ior),drfaeb(ior),aki(ior),agr(ior),ssdr(ior),drfaek(ior) &
                      ,drfaeg(ior),drfaes(ior),fssgr(ior),sedss(ior),sedSS_MQ(mstr,ior)                    &
                      ,tausc(mstr,ior),ischif(ior),ieros                                                   &
                      ,kontroll,jjj)
      
   enddo ! Ende Knotenschleife
   
   if (kontroll)print*,mstr,jjj,' schweb computing SSALG,SS,fssgr = ',  &
       SSALG(jjj),SS(jjj),fssgr(jjj)
   
   return
end
