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

subroutine masse_neu_qsim(ior,nkzs,akiz,aki,ablz,abl,agrz,agr,vo2z,vo2,      &
                          vnh4z,vnh4,vno2z,vno2,vno3z,vno3,gelPz,gelP,Siz,   &
                          Si,chlaz,chla,hchlkz,chlaki,hchlbz,chlabl,hchlgz,  &
                          chlagr,hgesPz,gesP,hgesNz,gesN,dH2D,hCChlkz,akbcm, &
                          hCChlbz,abbcm,hCChlgz,agbcm,mstr,azStrs,Caki,Cabl, &
                          Cagr)
   integer                                   :: azStrs
   integer, dimension(1000)                  :: nkzs
   real, dimension(1000)                     :: aki, abl, agr, vo2, vnh4, vno2, vno3, gelP, Si, chla, chlaki
   real, dimension(1000)                     :: chlabl, chlagr, akbcm, abbcm, agbcm, gesP, gesN
   real, dimension(50)                       :: dzMassek,dzMasseb,dzMasseg, dzMasseO,Masse_neuk, Masse_neub, Masse_neug, Masse_neuO
   real, dimension(50)                       :: dzMasseN4,dzMasseN2,dzMasseN3,Masse_neuN4, Masse_neuN2, Masse_neuN3
   real, dimension(50)                       :: dzMassegP,dzMasseSi,Masse_neugP, Masse_neuSi, dzMasseChl
   real, dimension(50)                       :: dzMasseChlk, dzMasseChlb, dzMasseChlg, dzMasseCChlk
   real, dimension(50)                       :: dzMasseCChlb, dzMasseCChlg, dzMassegsP, dzMassegsN, Masse_neuChl
   real, dimension(50)                       :: Masse_neuChlk, Masse_neuChlb, Masse_neuChlg
   real, dimension(50)                       :: Masse_neuCChlk, Masse_neuCChlb, Masse_neuCChlg, Masse_neugsP
   real, dimension(50)                       :: Masse_neugsN
   real, dimension(50,1000)                  :: akiz, ablz, agrz, vo2z, vnh4z, vno2z, vno3z, gelPz, Siz, chlaz
   real, dimension(azStrs,50,1000)           :: hchlkz, hchlbz, hchlgz, hCChlkz, hCChlbz, hCChlgz, hgesPz, hgesNz

   ! Massenerhalt 
   sumakiz = 0.0
   sumablz = 0.0
   sumagrz = 0.0
   sumO2 = 0.0
   sumNH4 = 0.0
   sumNO2 = 0.0
   sumNo3 = 0.0
   sumgP = 0.0
   sumSi = 0.0
   sumChl = 0.0
   sumChlk = 0.0
   sumChlb = 0.0
   sumChlg = 0.0
   sumCChlk = 0.0
   sumCChlb = 0.0
   sumCChlg = 0.0
   sumgsP = 0.0
   sumgsN = 0.0
   do nkz = 1,nkzs(ior)
      if (nkz > 1) then
         sumakiz = sumakiz + ((akiz(nkz-1,ior)+akiz(nkz,ior))/2.)*dH2D
         dzMassek(nkz) = ((akiz(nkz-1,ior)+akiz(nkz,ior))/2.)*dH2D
         sumablz = sumablz + ((ablz(nkz-1,ior)+ablz(nkz,ior))/2.)*dH2D
         dzMasseb(nkz) = ((ablz(nkz-1,ior)+ablz(nkz,ior))/2.)*dH2D
         sumagrz = sumagrz + ((agrz(nkz-1,ior)+agrz(nkz,ior))/2.)*dH2D
         dzMasseg(nkz) = ((agrz(nkz-1,ior)+agrz(nkz,ior))/2.)*dH2D
         sumO2 = sumO2 + ((vo2z(nkz-1,ior)+vo2z(nkz,ior))/2.)*dH2D
         dzMasseO(nkz) = ((vo2z(nkz-1,ior)+vo2z(nkz,ior))/2.)*dH2D
         sumNH4 = sumNH4 + ((vnh4z(nkz-1,ior)+vnh4z(nkz,ior))/2.)*dH2D
         dzMasseN4(nkz) = ((vnh4z(nkz-1,ior)+vnh4z(nkz,ior))/2.)*dH2D
         sumNO2 = sumNO2 + ((vno2z(nkz-1,ior)+vno2z(nkz,ior))/2.)*dH2D
         dzMasseN2(nkz) = ((vno2z(nkz-1,ior)+vno2z(nkz,ior))/2.)*dH2D
         sumNO3 = sumNO3 + ((vno3z(nkz-1,ior)+vno3z(nkz,ior))/2.)*dH2D
         dzMasseN3(nkz) = ((vno3z(nkz-1,ior)+vno3z(nkz,ior))/2.)*dH2D
         sumgP = sumgP + ((gelPz(nkz-1,ior)+gelPz(nkz,ior))/2.)*dH2D
         dzMassegP(nkz) = ((gelPz(nkz-1,ior)+gelPz(nkz,ior))/2.)*dH2D
         sumSi = sumSi + ((Siz(nkz-1,ior)+Siz(nkz,ior))/2.)*dH2D
         dzMasseSi(nkz) = ((Siz(nkz-1,ior)+Siz(nkz,ior))/2.)*dH2D
         sumChl = sumChl + ((Chlaz(nkz-1,ior)+Chlaz(nkz,ior))/2.)*dH2D
         dzMasseChl(nkz) = ((Chlaz(nkz-1,ior)+Chlaz(nkz,ior))/2.)*dH2D
         sumChlk = sumChlk + ((hChlkz(mstr,nkz-1,ior)+hChlkz(mstr,nkz,ior))/2.)*dH2D
         dzMasseChlk(nkz) = ((hChlkz(mstr,nkz-1,ior)+hChlkz(mstr,nkz,ior))/2.)*dH2D
         sumChlb = sumChlb + ((hChlbz(mstr,nkz-1,ior)+hChlbz(mstr,nkz,ior))/2.)*dH2D
         dzMasseChlb(nkz) = ((hChlbz(mstr,nkz-1,ior)+hChlbz(mstr,nkz,ior))/2.)*dH2D
         sumChlg = sumChlg + ((hChlgz(mstr,nkz-1,ior)+hChlgz(mstr,nkz,ior))/2.)*dH2D
         dzMasseChlg(nkz) = ((hChlgz(mstr,nkz-1,ior)+hChlgz(mstr,nkz,ior))/2.)*dH2D
         sumCChlk = sumCChlk + ((hCChlkz(mstr,nkz-1,ior)+hCChlkz(mstr,nkz,ior))/2.)*dH2D
         dzMasseCChlk(nkz) = ((hCChlkz(mstr,nkz-1,ior)+hCChlkz(mstr,nkz,ior))/2.)*dH2D
         sumCChlb = sumCChlb + ((hCChlbz(mstr,nkz-1,ior)+hCChlbz(mstr,nkz,ior))/2.)*dH2D
         dzMasseCChlb(nkz) = ((hCChlbz(mstr,nkz-1,ior)+hCChlbz(mstr,nkz,ior))/2.)*dH2D
         sumCChlg = sumCChlg + ((hCChlgz(mstr,nkz-1,ior)+hCChlgz(mstr,nkz,ior))/2.)*dH2D
         dzMasseCChlg(nkz) = ((hCChlgz(mstr,nkz-1,ior)+hCChlgz(mstr,nkz,ior))/2.)*dH2D
         sumgsP = sumgsP + ((hgesPz(mstr,nkz-1,ior)+hgesPz(mstr,nkz,ior))/2.)*dH2D
         dzMassegsP(nkz) = ((hgesPz(mstr,nkz-1,ior)+hgesPz(mstr,nkz,ior))/2.)*dH2D
         sumgsN = sumgsN + ((hgesNz(mstr,nkz-1,ior)+hgesNz(mstr,nkz,ior))/2.)*dH2D
         dzMassegsN(nkz) = ((hgesNz(mstr,nkz-1,ior)+hgesNz(mstr,nkz,ior))/2.)*dH2D
      endif
   enddo
   
   D1Massek = aki(ior) * ((nkzs(ior)-1)*dH2D)
   D1Masseb = abl(ior) * ((nkzs(ior)-1)*dH2D)
   D1Masseg = agr(ior) * ((nkzs(ior)-1)*dH2D)
   D1MasseO = vo2(ior) * ((nkzs(ior)-1)*dH2D)
   D1MasseN4 = vnh4(ior) * ((nkzs(ior)-1)*dH2D)
   D1MasseN2 = vno2(ior) * ((nkzs(ior)-1)*dH2D)
   D1MasseN3 = vno3(ior) * ((nkzs(ior)-1)*dH2D)
   D1MassegP = gelP(ior) * ((nkzs(ior)-1)*dH2D)
   D1MasseSi = Si(ior) * ((nkzs(ior)-1)*dH2D)
   D1MasseChl = Chla(ior) * ((nkzs(ior)-1)*dH2D)
   D1MasseChlk = Chlaki(ior) * ((nkzs(ior)-1)*dH2D)
   D1MasseChlb = Chlabl(ior) * ((nkzs(ior)-1)*dH2D)
   D1MasseChlg = Chlagr(ior) * ((nkzs(ior)-1)*dH2D)
   D1MasseCChlk = akbcm(ior) * ((nkzs(ior)-1)*dH2D)
   D1MasseCChlb = abbcm(ior) * ((nkzs(ior)-1)*dH2D)
   D1MasseCChlg = agbcm(ior) * ((nkzs(ior)-1)*dH2D)
   D1MassegsP = gesP(ior) * ((nkzs(ior)-1)*dH2D)
   D1MassegsN = gesN(ior) * ((nkzs(ior)-1)*dH2D)
   dMassek = D1Massek - sumakiz
   dMasseb = D1Masseb - sumablz
   dMasseg = D1Masseg - sumagrz
   dMasseO = D1MasseO - sumO2
   dMasseN4 = D1MasseN4 - sumNH4
   dMasseN2 = D1MasseN2 - sumNO2
   dMasseN3 = D1MasseN3 - sumNO3
   dMassegP = D1MassegP - sumgP
   dMasseSi = D1MasseSi - sumSi
   dMasseChl = D1MasseChl - sumChl
   dMasseChlk = D1MasseChlk - sumChlk
   dMasseChlb = D1MasseChlb - sumChlb
   dMasseChlg = D1MasseChlg - sumChlg
   dMasseCChlk = D1MasseCChlk - sumCChlk
   dMasseCChlb = D1MasseCChlb - sumCChlb
   dMasseCChlg = D1MasseCChlg - sumCChlg
   dMassegsP = D1MassegsP - sumgsP
   dMassegsN = D1MassegsN - sumgsN
   do nkz = nkzs(ior),1,-1
      if (sumakiz > 0.0) then
         Masse_neuk(nkz) = dzMassek(nkz)+dMassek*dzMassek(nkz)/sumakiz
      else
         Masse_neuk(nkz) = dzMassek(nkz)
      endif
      
      if (sumablz > 0.0) then
         Masse_neub(nkz) = dzMasseb(nkz)+dMasseb*dzMasseb(nkz)/sumablz
      else
         Masse_neub(nkz) = dzMasseb(nkz)
      endif
      
      if (sumagrz > 0.0) then
         Masse_neug(nkz) = dzMasseg(nkz)+dMasseg*dzMasseg(nkz)/sumagrz
      else
         Masse_neug(nkz) = dzMasseg(nkz)
      endif
      
      if (sumO2 > 0.0) then
         Masse_neuO(nkz) = dzMasseO(nkz)+dMasseO*dzMasseO(nkz)/sumO2
      else
         Masse_neuO(nkz) = dzMasseO(nkz)
      endif
      
      if (sumNH4 > 0.0) then
         Masse_neuN4(nkz) = dzMasseN4(nkz)+dMasseN4*dzMasseN4(nkz)/sumNH4
      else
         Masse_neuN4(nkz) = dzMasseN4(nkz)
      endif
      if (sumNO2 > 0.0) then
         Masse_neuN2(nkz) = dzMasseN2(nkz)+dMasseN2*dzMasseN2(nkz)/sumNO2
      else
         Masse_neuN2(nkz) = dzMasseN2(nkz)
      endif
      if (sumNO3 > 0.0) then
         Masse_neuN3(nkz) = dzMasseN3(nkz)+dMasseN3*dzMasseN3(nkz)/sumNO3
      else
         Masse_neuN3(nkz) = dzMasseN3(nkz)
      endif
      if (sumgP > 0.0) then
         Masse_neugP(nkz) = dzMassegP(nkz)+dMassegP*dzMassegP(nkz)/sumgP
      else
         Masse_neugP(nkz) = dzMassegP(nkz)
      endif
      if (sumSi > 0.0) then
         Masse_neuSi(nkz) = dzMasseSi(nkz)+dMasseSi*dzMasseSi(nkz)/sumSi
      else
         Masse_neuSi(nkz) = dzMasseSi(nkz)
      endif
      if (sumChl > 0.0) then
         Masse_neuChl(nkz) = dzMasseChl(nkz)+dMasseChl*dzMasseChl(nkz)/sumChl
      else
         Masse_neuChl(nkz) = dzMasseChl(nkz)
      endif
      if (sumChlk > 0.0) then
         Masse_neuChlk(nkz) = dzMasseChlk(nkz)+dMasseChlk*dzMasseChlk(nkz)/sumChlk
      else
         Masse_neuChlk(nkz) = dzMasseChlk(nkz)
      endif
      if (sumChlb > 0.0) then
         Masse_neuChlb(nkz) = dzMasseChlb(nkz)+dMasseChlb*dzMasseChlb(nkz)/sumChlb
      else
         Masse_neuChlb(nkz) = dzMasseChlb(nkz)
      endif
      if (sumChlg > 0.0) then
         Masse_neuChlg(nkz) = dzMasseChlg(nkz)+dMasseChlg*dzMasseChlg(nkz)/sumChlg
      else
         Masse_neuChlg(nkz) = dzMasseChlg(nkz)
      endif
      if (sumCChlk > 0.0) then
         Masse_neuCChlk(nkz) = dzMasseCChlk(nkz)+dMasseCChlk*dzMasseCChlk(nkz)/sumCChlk
      else
         Masse_neuCChlk(nkz) = dzMasseCChlk(nkz)
      endif
      if (sumCChlb > 0.0) then
         Masse_neuCChlb(nkz) = dzMasseCChlb(nkz)+dMasseCChlb*dzMasseCChlb(nkz)/sumCChlb
      else
         Masse_neuCChlb(nkz) = dzMasseCChlb(nkz)
      endif
      if (sumCChlg > 0.0) then
         Masse_neuCChlg(nkz) = dzMasseCChlg(nkz)+dMasseCChlg*dzMasseCChlg(nkz)/sumCChlg
      else
         Masse_neuCChlg(nkz) = dzMasseCChlg(nkz)
      endif
      if (sumgsP > 0.0) then
         Masse_neugsP(nkz) = dzMassegsP(nkz)+dMassegsP*dzMassegsP(nkz)/sumgsP
      else
         Masse_neugsP(nkz) = dzMassegsP(nkz)
      endif
      if (sumgsN > 0.0) then
         Masse_neugsN(nkz) = dzMassegsN(nkz)+dMassegsN*dzMassegsN(nkz)/sumgsN
      else
         Masse_neugsN(nkz) = dzMassegsN(nkz)
      endif
      if (nkz == nkzs(ior)) then
         akiz(nkz,ior) = (Masse_neuk(nkz)/dH2D) - 0.5*(akiz(nkz-1,ior)-akiz(nkz,ior))
         ablz(nkz,ior) = (Masse_neub(nkz)/dH2D) - 0.5*(ablz(nkz-1,ior)-ablz(nkz,ior))
         agrz(nkz,ior) = (Masse_neug(nkz)/dH2D) - 0.5*(agrz(nkz-1,ior)-agrz(nkz,ior))
         vo2z(nkz,ior) = (Masse_neuO(nkz)/dH2D) - 0.5*(vo2z(nkz-1,ior)-vo2z(nkz,ior))
         vnh4z(nkz,ior) = (Masse_neuN4(nkz)/dH2D) - 0.5*(vnh4z(nkz-1,ior)-vnh4z(nkz,ior))
         vno2z(nkz,ior) = (Masse_neuN2(nkz)/dH2D) - 0.5*(vno2z(nkz-1,ior)-vno2z(nkz,ior))
         vno3z(nkz,ior) = (Masse_neuN3(nkz)/dH2D) - 0.5*(vno3z(nkz-1,ior)-vno3z(nkz,ior))
         gelPz(nkz,ior) = (Masse_neugP(nkz)/dH2D) - 0.5*(gelPz(nkz-1,ior)-gelPz(nkz,ior))
         Siz(nkz,ior) = (Masse_neuSi(nkz)/dH2D) - 0.5*(Siz(nkz-1,ior)-Siz(nkz,ior))
         Chlaz(nkz,ior) = (Masse_neuChl(nkz)/dH2D) - 0.5*(Chlaz(nkz-1,ior)-Chlaz(nkz,ior))
         hChlkz(mstr,nkz,ior) = (Masse_neuChlk(nkz)/dH2D) - 0.5*(hChlkz(mstr,nkz-1,ior)-hChlkz(mstr,nkz,ior))
         hChlbz(mstr,nkz,ior) = (Masse_neuChlb(nkz)/dH2D) - 0.5*(hChlbz(mstr,nkz-1,ior)-hChlbz(mstr,nkz,ior))
         hChlgz(mstr,nkz,ior) = (Masse_neuChlg(nkz)/dH2D) - 0.5*(hChlgz(mstr,nkz-1,ior)-hChlgz(mstr,nkz,ior))
         hCChlkz(mstr,nkz,ior) = (Masse_neuCChlk(nkz)/dH2D) - 0.5*(hCChlkz(mstr,nkz-1,ior)-hCChlkz(mstr,nkz,ior))
         hCChlbz(mstr,nkz,ior) = (Masse_neuCChlb(nkz)/dH2D) - 0.5*(hCChlbz(mstr,nkz-1,ior)-hCChlbz(mstr,nkz,ior))
         hCChlgz(mstr,nkz,ior) = (Masse_neuCChlg(nkz)/dH2D) - 0.5*(hCChlgz(mstr,nkz-1,ior)-hCChlgz(mstr,nkz,ior))
         hgesPz(mstr,nkz,ior) = (Masse_neugsP(nkz)/dH2D) - 0.5*(hgesPz(mstr,nkz-1,ior)-hgesPz(mstr,nkz,ior))
         hgesNz(mstr,nkz,ior) = (Masse_neugsN(nkz)/dH2D) - 0.5*(hgesNz(mstr,nkz-1,ior)-hgesNz(mstr,nkz,ior))
         cycle
      endif
      akiz(nkz,ior) = (Masse_neuk(nkz+1)*2./dH2D)-akiz(nkz+1,ior)
      ablz(nkz,ior) = (Masse_neub(nkz+1)*2./dH2D)-ablz(nkz+1,ior)
      agrz(nkz,ior) = (Masse_neug(nkz+1)*2./dH2D)-agrz(nkz+1,ior)
      vo2z(nkz,ior) = (Masse_neuO(nkz+1)*2./dH2D)-vo2z(nkz+1,ior)
      vnh4z(nkz,ior) = (Masse_neuN4(nkz+1)*2./dH2D)-vnh4z(nkz+1,ior)
      vno2z(nkz,ior) = (Masse_neuN2(nkz+1)*2./dH2D)-vno2z(nkz+1,ior)
      vno3z(nkz,ior) = (Masse_neuN3(nkz+1)*2./dH2D)-vno3z(nkz+1,ior)
      gelPz(nkz,ior) = (Masse_neugP(nkz+1)*2./dH2D)-gelPz(nkz+1,ior)
      Siz(nkz,ior) = (Masse_neuSi(nkz+1)*2./dH2D)-Siz(nkz+1,ior)
      Chlaz(nkz,ior) = (Masse_neuChl(nkz+1)*2./dH2D)-chlaz(nkz+1,ior)
      hChlkz(mstr,nkz,ior) = (Masse_neuChlk(nkz+1)*2./dH2D)-hchlkz(mstr,nkz+1,ior)
      hChlbz(mstr,nkz,ior) = (Masse_neuChlb(nkz+1)*2./dH2D)-hchlbz(mstr,nkz+1,ior)
      hChlgz(mstr,nkz,ior) = (Masse_neuChlg(nkz+1)*2./dH2D)-hchlgz(mstr,nkz+1,ior)
      hCChlkz(mstr,nkz,ior) = (Masse_neuCChlk(nkz+1)*2./dH2D)-hCChlkz(mstr,nkz+1,ior)
      hCChlbz(mstr,nkz,ior) = (Masse_neuCChlb(nkz+1)*2./dH2D)-hCChlbz(mstr,nkz+1,ior)
      hCChlgz(mstr,nkz,ior) = (Masse_neuCChlg(nkz+1)*2./dH2D)-hCChlgz(mstr,nkz+1,ior)
      hgesPz(mstr,nkz,ior) = (Masse_neugsP(nkz+1)*2./dH2D)-hgesPz(mstr,nkz+1,ior)
      hgesNz(mstr,nkz,ior) = (Masse_neugsN(nkz+1)*2./dH2D)-hgesNz(mstr,nkz+1,ior)
      ! hCChlkz(mstr,nkz,ior) =  akiz(nkz,ior)*Caki*1000./hChlkz(mstr,nkz,ior)
      ! hCChlbz(mstr,nkz,ior) =  ablz(nkz,ior)*Cabl*1000./hChlbz(mstr,nkz,ior)
      ! hCChlgz(mstr,nkz,ior) =  agrz(nkz,ior)*Cagr*1000./hChlgz(mstr,nkz,ior)
   enddo
end subroutine masse_neu_qsim
