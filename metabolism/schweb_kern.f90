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
subroutine schweb_kern(zooinds,dorgSSs,sss,ssalgs,tiefes                        &
                       ,raus,tflie,VMITTs                                       &
                       ,dkimors,dgrmors,abszos,zexkis,zexgrs                    &
                       ,abls,zexbls,dblmor,drfaebs,akis,agrs,ssdrs,drfaeks      &
                       ,drfaegs,drfaess,fssgrs,sedsss,sedSS_MQs                 &
                       ,tauscs,ischifs,ieros                                    &
                       ,kontroll,jjj)
   
   use aparam
   implicit none
   real                 :: sss      !< organische und anorganischer Schwebstoffe (ohen Algen und Zooplankton)
   real                 :: ssalgs   !< Gesamtschwebstoffe
   integer              :: ischifs,ieros,ised,jsed
   real                 :: akis,agrs,abls
   real                 :: zooinds, dorgSSs, dkimors,dgrmors,dblmor, dblmors, drfaebs
   real                 :: ssdrs,drfaeks,drfaegs,drfaess,fssgrs,sedsss,sedSS_MQs
   real                 :: tflie,TIEFEs,RAUs,VMITTs,tauscs
   real                 :: UST, g, v6, vges,SSSED,ZellV,hc1,hc2,delfss,fssgrv
   real                 :: ustkri,vkrit,qsgr,oc,Oc0,wst,ceq,delss
   real                 :: exzo,zexkis,zexgrs,zexbls,sst,abszos
   logical, intent(in)  :: kontroll !< debugging
   integer, intent(in)  :: jjj      !< debugging
   
   
   if (kontroll) print*,'schweb_kern TIEFE,RAU,VMITT,tausc = ',TIEFEs,RAUs,VMITTs,tauscs
   fssgrv = fssgrs
   g = sqrt(9.81)
   ust = (((1./raus)*g)/(tiefes**0.16667))*abs(vmitts)
   ustkri = sqrt(tauscs/1000.)
   vkrit = (ustkri*tiefes**0.166667)/((1./raus)*g)
   
   ! Einfluss der Schifffahrt
   if (ischifs == 0) then
      v6 = 0.0
   else
      call schiff(VMITTs,tiefes,v6,ischifs)
   endif
   vges = VMITTs+v6
   
   SSSED = fssgrs*SSs
   ised = 3
   jsed = 1
   ZellV = 0.0
   call Sedimentation(tiefes,ised,ust,qsgr,oc,Oc0,tflie,wst,jsed,ZellV,kontroll,jjj)
   ceq = sssed*qsgr
   sedsss = max(0.0,(sssed-ceq)) * oc
   sedSS_MQs = sedsss
   if (ieros == 1 .and. vges > vkrit)sedsss = 0.0
   
   exzo = zexkis+zexgrs+zexbls
   
   !...Schwebstoffverluste durch Dreissena werden nicht berücksichtigt
   ssdrs = 0.0
   
   SSt = SSs                          &
       - sedsss                       &
       + exzo                         &
       + dkimors + dgrmors + dblmors  &
       + abszos                       &
       - ssdrs                        &
       + dorgSSs                      &
       + drfaeks + drfaegs + drfaebs  &
       + drfaess
   
   !     Neuberechnung des Faktors zur Berechnung der ablagerungsfreien
   !     Grenzkonzentration
   
   hc1 = SSs-sedsss+exzo+dkimors
   hc1 = hc1+dgrmors+dblmors+abszos-ssdrs
   hc1 = hc1+dorgSSs+drfaeks+drfaegs
   hc1 = hc1+drfaebs+drfaess
   
   hc2 = sssed-sedsss+exzo+dkimors
   hc2 = hc2+dgrmors+dblmors+abszos-ssdrs
   hc2 = hc2+dorgSSs+drfaeks+drfaegs
   hc2 = hc2+drfaebs+drfaess
   if (hc2 < 0.0)hc2 = 0.0
   if (hc1 < 0.0)hc1 = 0.0
   
   if (hc1 > 0.0) then
      fssgrs = hc2/hc1
   else
      fssgrs = 0.0
   endif
   delfss = fssgrv-fssgrs
   if (fssgrs < 0.0)fssgrs = (fssgrv/(fssgrv+abs(delfss)))*fssgrv
   !fssgrt = fssgrs
   !fssgrs = fssgrv
   
   delss = sst-sss
   if (sst < 0.0) then
      sss = (sss/(sss+abs(delss)))*sss
   else
      sss = sst
   endif
   SSALGs = SSs+agrs+akis+abls+(ZOOinds*GROT/1000.)
   return
end subroutine schweb_kern
