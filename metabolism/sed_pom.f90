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
subroutine sed_pom(tiefe1,ust,n,BSBC,PhytoC,GesSS,SedOM,dKorn,SedOMb,dKornb,  &
                   fsch,fOM_OC,mstr,mSta,jsed,w2,w2b,                         &
                   kontroll ,jjj)
   
   use allodim
   
   logical, intent(in)             :: kontroll  !< debugging
   integer, intent(in)             :: jjj       !< debugging
   real,dimension(azStrs,1000)     :: SedOM, dKorn, SedOMb, dKornb, w2, w2b
   double precision                :: sedoc,ws
   
   do ised = 1,3
      
      xtflie = 1.
      ior = 1
      ZellV = 500.
      call sedimentation(tiefe1,ised,ust,qsgr,oc,Oc0,xtflie,wst,jsed,ZellV,kontroll,jjj)
      ceq = qsgr
      
      if (ised == 1) then
         sdFluA = (1. - Ceq) * oc
      else if (ised == 2) then
         sdFluB = (1. - Ceq) * oc
      else
         sdFluS = (1. - Ceq) * oc
      endif
   enddo
   
   if (n == 2) then
      SedOMz = SedOM(mstr,mSta)
      dKornz = dKorn(mstr,mSta)
      w2z = w2(mstr,mSta)
   endif
   
   SedOC = (BSBC*sdFluB+PhytoC*sdFluA)/(GesSS*max(1.e-10,sdFluS))
   if (SedOM(mstr,mSta) > 0.0 .and. n == 1) then
      sedOM(mstr,mSta) = sedOM(mstr,mSta)/100.
   else if (SedOMb(mstr,mSta) > 0.0 .and. n == 2) then
      SedOM(mstr,mSta) = SedOMb(mstr,msta)/100.
   else
      SedOM(mstr,mSta) = SedOC !*fOM_OC
      xsedOM = SedOM(mstr,mSta)
      SedOM(mstr,mSta) = SedOM(mstr,mSta)*fsch
      if (SedOM(mstr,mSta) < 0.001)SedOM(mstr,mSta) = 0.005
   endif
   Dichta = 2.6
   Dichto = 1.2
   Dichte = dichta*(1.-SedOM(mstr,mSta))+SedOM(mstr,mSta)*Dichto
   ! w2(mstr,mSta) = 0.000109*exp(-579.2*ust)
   ! if(w2(mstr,mSta)>0.75e-5)w2(mstr,mSta)=0.75e-5
   ! w2(mstr,mSta) = ((BSBC*sdFluB+PhytoC*sdFluA)*3.1*tiefe1)/(SedOM(mstr,mSta)*1000.*1000.*Dichte)
   ! if(w2(mstr,mSta)>1.45e-5)w2(mstr,mSta)=0.74e-5
   w2(mstr,mSta) = 0.74e-5
   ! Berechnung des Mittleren Korndurchmessers in mm
   dKorn(mstr,mSta) = min(100.,0.0047*exp(64.89*ust))
   ! Umrechnung in m
   dKorn(mstr,mSta) = dKorn(mstr,mSta)/1000.
   
   if (n == 2) then
      SedOMb(mstr,mSta) = SedOM(mstr,mSta)
      dKornb(mstr,mSta) = dKorn(mstr,mSta)
      w2b(mstr,mSta) = w2(mstr,mSta)
      SedOM(mstr,mSta) = SedOMz
      dKorn(mstr,mSta) = dKornz
      w2(mstr,mSta) = w2z
   endif
   
   return
end subroutine sed_pom
