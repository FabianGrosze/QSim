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

!> Ermittlung der vertikalen Konzentrationsverteilung mit Hilfe eines impliziten
!! Differenzenverfahren
!! @author Volker Kirchesch
!! @date 04.01.2005
subroutine v_konz(tempwz, hcdif1, hcdt, dtemp, nkzs, xD, ior, vo2z, hJO2,     &
                  dO2o2D, vz1, vNH4z, vNO2z, vNO3z, hJPO4, hJSi, hJNH4, hJNO3,&
                  vx02, itags, gelPz, Siz, akiz, agrz, ablz, chlaz, hchlkz,   &
                  hchlgz, hchlbz, hCChlkz, hCChlbz, hCChlgz, hgesPz, hgesNz,  &
                  orgCsd0, pl0, nl0, gesP, gesN, sedalk0, sedalb0, sedalg0,   &
                  tiefe, aki, abl, agr, Q_PK, Q_PB, Q_PG, hQ_NKz, hQ_NBz,     &
                  hQ_NGz, Q_NK, Q_NB, Q_NG, Qmx_NK, Qmx_NB, Qmx_NG, akbcm,    &
                  abbcm, agbcm, dH2D, tflie, mstr, uhrz, monats, azStrs)
   integer                           :: azStrs
   integer, dimension(1000)          :: nkzs
   real, dimension(1000)             :: dO2o2D, vx02, orgCsd0, pl0, gesP, sedalk0, sedalb0, sedalg0
   real, dimension(1000)             :: Q_PK, Q_PB, Q_PG, aki, abl, agr, nl0, gesN
   real, dimension(1000)             :: akbcm, abbcm, agbcm, Q_NK, Q_NB, Q_NG, tiefe
   real, dimension(50)               :: xD, hcdif1, a, b, c, dT, doo, dN4, dN2, dN3, dP, dSi, dKi, sedorgN, sedorgP, U
   real, dimension(50)               :: dGr, dBl, dChl, dChlk, dchlg, dchlb, dgesP, dgesN, dQ_NKz, dQ_NBz, dQ_NGz
   real, dimension(50)               :: dCChlk, dCChlb, dCChlg
   
   real, dimension(50,1000)          :: tempwz, dtemp, vo2z, vz1, vNH4z, vNO2z, vNO3z
   real, dimension(azStrs,1000)      :: hJNH4, hJPO4, hJNO3, hJSi, hJO2
   real, dimension(50,1000)          :: gelPz, Siz, akiz, agrz, ablz, chlaz
   real, dimension(azStrs,50,1000)   :: hchlkz, hchlgz, hchlbz, hgesPz, hgesNz, hQ_NKz, hQ_NBz, hQ_NGz
   real, dimension(azStrs,50,1000)   :: hCChlkz, hCChlbz, hCChlgz
   
   
   Caki = 0.48
   Cabl = 0.48
   Cagr = 0.48
   
   ! Ammonium
   dBetN4 = hJNH4(mstr,ior)/(0.5*dH2D*24.)   ! pro Stunde
   dBetN4 = dBetN4*hcdt
   
   dBetN2 = 0.0
   
   dBetN3 = hJNO3(mstr,ior)/(0.5*dH2D*24.)
   dBetN3 = dBetN3*hcdt
   
   ! Sauerstoff
   ! unterste Schicht
   dBetO2 = hJO2(mstr,ior)/(0.5*dH2D*24.)
   dBetO2 = dBetO2*hcdt
   
   ! oberste Schicht (physikalischer Sauerstoffeintrag)
   vz1t = vz1(1,ior)*hcdt
   dO2o2Dt = dO2o2D(ior)*hcdt
   DELO2 = Vz1t+dO2o2Dt
   
   dBetP = hJPO4(mstr,ior)/(0.5*dH2D*24.)
   dBetP = dBetP*hcdt
   
   dBetSi = hJSi(mstr,ior)/(0.5*dH2D*24.)
   dBetSi = dBetSi*hcdt
   
   ! Crank-Nicolson
   do nkz = 1,nkzs(ior)
      vz1t = vz1(nkz,ior)*hcdt
      if (nkz > 1) then
         vo2zt = vo2z(nkz,ior)+vz1t
         if (vo2zt < 0.0)vo2zt = (vo2z(nkz,ior)/(vo2z(nkz,ior)+abs(vz1t)))*vo2z(nkz,ior)
         vo2z(nkz,ior) = vo2zt
      endif
      
      tempzt = tempwz(nkz,ior)+dtemp(nkz,ior)*hcdt
      if (tempzt < 0.0)tempzt = (tempwz(nkz,ior)/(tempwz(nkz,ior)+abs(dtemp(nkz,ior)*hcdt)))*tempwz(nkz,ior)
      tempwz(nkz,ior) = tempzt
   enddo
   vo2zt = vo2z(1,ior)+delO2       ! oberflächenschicht, physikalischer Sauerstoffein/austrag
   if (vo2zt < 0.0)vo2zt = (vo2z(1,ior)/(vo2z(1,ior)+abs(delo2)))*vo2z(1,ior)
   vo2z(1,ior) = vo2zt
   ! Sohlschichten
   
   vo2zt = vo2z(nkzs(ior),ior)-dBetO2
   if (vo2zt < 0.0)vo2zt = (vo2z(nkzs(ior),ior)/(vo2z(nkzs(ior),ior)+abs(dBetO2)))*vo2z(nkzs(ior),ior)
   vo2z(nkzs(ior),ior) = vo2zt
   
   vNH4zt = vNH4z(nkzs(ior),ior)+dBetN4
   if (vNH4zt < 0.0)vNH4zt = (vNH4z(nkzs(ior),ior)/(vNH4z(nkzs(ior),ior)+abs(dBetN4)))*vNH4z(nkzs(ior),ior)
   vNH4z(nkzs(ior),ior) = max(0.0001,vNH4zt)
   vNO2z(nkzs(ior),ior) = vNO2z(nkzs(ior),ior)+dBetN2
   vNO3zt = vNO3z(nkzs(ior),ior)+dBetN3
   if (vNO3zt < 0.0)vNO3zt = (vNO3z(nkzs(ior),ior)/(vNO3z(nkzs(ior),ior)+abs(dBetN3)))*vNO3z(nkzs(ior),ior)
   vNO3z(nkzs(ior),ior) = max(0.0001,vNO3zt)
   gelPz(nkzs(ior),ior) = gelPz(nkzs(ior),ior)+dBetP
   hgesPz(mstr,nkzs(ior),ior) = hgesPz(mstr,nkzs(ior),ior) + dBetP
   Siz(nkzs(ior),ior) = Siz(nkzs(ior),ior)+dBetSi
   !########  Berücksichtigung der Sedimentation bei partikulären Inhaltsstoffen #########
   do nkz = 1, nkzs(ior)
      sedorgN(nkz) = max(0.0,hgesNz(mstr,nkz,ior)-akiz(nkz,ior)*Q_NK(ior)-ablz(nkz,ior)*Q_NB(ior)      &
                     -agrz(nkz,ior)*Q_NG(ior)-vnh4z(nkz,ior)-vno2z(nkz,ior)-vno3z(nkz,ior))
      sedorgP(nkz) = max(0.0,hgesPz(mstr,nkz,ior)-akiz(nkz,ior)*Q_PK(ior)-ablz(nkz,ior)*Q_PB(ior)      &
                     -agrz(nkz,ior)*Q_PG(ior)-gelPz(nkz,ior))
   enddo
   do iz = 1,8
      if (iz == 1) then
         U(1:nkzs(ior)) = akiz(1:nkzs(ior),ior)
         xws = sedalk0(ior)
      endif
      if (iz == 2) then
         U(1:nkzs(ior)) = ablz(1:nkzs(ior),ior)
         xws = sedalb0(ior)
      endif
      if (iz == 3) then
         U(1:nkzs(ior)) = agrz(1:nkzs(ior),ior)
         xws = sedalg0(ior)
      endif
      if (iz == 4) then
         U(1:nkzs(ior)) = hchlkz(mstr,1:nkzs(ior),ior)
         xws = sedalk0(ior)
      endif
      if (iz == 5) then
         U(1:nkzs(ior)) = hchlbz(mstr,1:nkzs(ior),ior)
         xws = sedalb0(ior)
      endif
      if (iz == 6) then
         U(1:nkzs(ior)) = hchlgz(mstr,1:nkzs(ior),ior)
         xws = sedalg0(ior)
      endif
      
      if (iz == 7) then
         U(1:nkzs(ior)) = sedorgP(1:nkzs(ior))
         xws = orgCsd0(ior)
      endif
      if (iz == 8) then
         U(1:nkzs(ior)) = sedorgN(1:nkzs(ior))
         xws = orgCsd0(ior)
      endif
      if (iz == 9) then
         U(1:nkzs(ior)) = hCChlkz(mstr,1:nkzs(ior),ior)
         xws = sedalk0(ior)
      endif
      if (iz == 10) then
         U(1:nkzs(ior)) = hCChlbz(mstr,1:nkzs(ior),ior)
         xws = sedalb0(ior)
      endif
      if (iz == 11) then
         U(1:nkzs(ior)) = hCChlgz(mstr,1:nkzs(ior),ior)
         xws = sedalg0(ior)
      endif
      call Lax_Wen(ior,nkzs,U,xws,hcdt,dH2D)
      
      if (iz == 1)akiz(1:nkzs(ior),ior) = U(1:nkzs(ior))
      if (iz == 2)ablz(1:nkzs(ior),ior) = U(1:nkzs(ior))
      if (iz == 3)agrz(1:nkzs(ior),ior) = U(1:nkzs(ior))
      if (iz == 4)hchlkz(mstr,1:nkzs(ior),ior) = U(1:nkzs(ior))
      if (iz == 5)hchlbz(mstr,1:nkzs(ior),ior) = U(1:nkzs(ior))
      if (iz == 6)hchlgz(mstr,1:nkzs(ior),ior) = U(1:nkzs(ior))
      if (iz == 7)hgesPz(mstr,1:nkzs(ior),ior) = U(1:nkzs(ior)) + akiz(1:nkzs(ior),ior)*Q_PK(ior)                &
          +ablz(1:nkzs(ior),ior)*Q_PB(ior)                                 &
          +agrz(1:nkzs(ior),ior)*Q_PG(ior)+gelPz(1:nkzs(ior),ior)
      if (iz == 8)hgesNz(mstr,1:nkzs(ior),ior) = U(1:nkzs(ior)) + akiz(1:nkzs(ior),ior)*Q_NK(ior)                &
          +ablz(1:nkzs(ior),ior)*Q_NB(ior)+agrz(1:nkzs(ior),ior)*Q_NG(ior) &
          +vnh4z(1:nkzs(ior),ior)+vno2z(1:nkzs(ior),ior)                   &
          +vno3z(1:nkzs(ior),ior)
      if (iz == 9)hCChlkz(mstr,1:nkzs(ior),ior) = U(1:nkzs(ior))
      if (iz == 10)hCChlbz(mstr,1:nkzs(ior),ior) = U(1:nkzs(ior))
      if (iz == 11)hCChlgz(mstr,1:nkzs(ior),ior) = U(1:nkzs(ior))
   enddo
   
   ! --- vertikale Diffusion (Crank-Nicolson) ---
   nkz = 1
   a(nkz) = (2.+2.*hcdif1(nkz))
   b(nkz) = -hcdif1(nkz)
   dT(nkz) = (2.-2*hcdif1(nkz))*tempwz(nkz,ior)+hcdif1(nkz)*tempwz(nkz+1,ior)+hcdif1(nkz)*(tempwz(nkz,ior)+tempwz(nkz,ior))
   
   doo(nkz) = (2.-2*hcdif1(nkz))*vo2z(nkz,ior)+hcdif1(nkz)*vo2z(nkz+1,ior)+hcdif1(nkz)*(vo2z(nkz,ior)+vo2z(nkz,ior))
   
   dN4(nkz) = (2.-2*hcdif1(nkz))*vNh4z(nkz,ior)+hcdif1(nkz)*vNH4z(nkz+1,ior)+hcdif1(nkz)*(vNH4z(nkz,ior)+vNH4z(nkz,ior))
   
   dN2(nkz) = (2.-2*hcdif1(nkz))*vNO2z(nkz,ior)+hcdif1(nkz)*vNO2z(nkz+1,ior)+hcdif1(nkz)*(vNO2z(nkz,ior)+vNO2z(nkz,ior))
   
   dN3(nkz) = (2.-2*hcdif1(nkz))*vNO3z(nkz,ior)+hcdif1(nkz)*vNO3z(nkz+1,ior)+hcdif1(nkz)*(vNO3z(nkz,ior)+vNO3z(nkz,ior))
   
   dP(nkz) = (2.-2*hcdif1(nkz))*gelPz(nkz,ior)+hcdif1(nkz)*gelPz(nkz+1,ior)+hcdif1(nkz)*(gelPz(nkz,ior)+gelPz(nkz,ior))
   
   dSi(nkz) = (2.-2*hcdif1(nkz))*Siz(nkz,ior)+hcdif1(nkz)*Siz(nkz+1,ior)+hcdif1(nkz)*(Siz(nkz,ior)+Siz(nkz,ior))
   
   dKi(nkz) = (2.-2*hcdif1(nkz))*akiz(nkz,ior)+hcdif1(nkz)*akiz(nkz+1,ior)+hcdif1(nkz)*(akiz(nkz,ior)+akiz(nkz,ior))
   
   dGr(nkz) = (2.-2*hcdif1(nkz))*agrz(nkz,ior)+hcdif1(nkz)*agrz(nkz+1,ior)+hcdif1(nkz)*(agrz(nkz,ior)+agrz(nkz,ior))
   
   dBl(nkz) = (2.-2*hcdif1(nkz))*ablz(nkz,ior)+hcdif1(nkz)*ablz(nkz+1,ior)+hcdif1(nkz)*(ablz(nkz,ior)+ablz(nkz,ior))
   
   dChl(nkz) = (2.-2*hcdif1(nkz))*chlaz(nkz,ior)+hcdif1(nkz)*chlaz(nkz+1,ior)+hcdif1(nkz)*(chlaz(nkz,ior)+chlaz(nkz,ior))
   dChlk(nkz) = (2.-2*hcdif1(nkz))*hchlkz(mstr,nkz,ior)+hcdif1(nkz)*hchlkz(mstr,nkz+1,ior)     &
                +hcdif1(nkz)*(hchlkz(mstr,nkz,ior)+hchlkz(mstr,nkz,ior))
   dChlg(nkz) = (2.-2*hcdif1(nkz))*hchlgz(mstr,nkz,ior)+hcdif1(nkz)*hchlgz(mstr,nkz+1,ior)     &
                +hcdif1(nkz)*(hchlgz(mstr,nkz,ior)+hchlgz(mstr,nkz,ior))
   dChlb(nkz) = (2.-2*hcdif1(nkz))*hchlbz(mstr,nkz,ior)+hcdif1(nkz)*hchlbz(mstr,nkz+1,ior)     &
                +hcdif1(nkz)*(hchlbz(mstr,nkz,ior)+hchlbz(mstr,nkz,ior))
   dgesP(nkz) = (2.-2*hcdif1(nkz))*hgesPz(mstr,nkz,ior)+hcdif1(nkz)*hgesPz(mstr,nkz+1,ior)     &
                +hcdif1(nkz)*(hgesPz(mstr,nkz,ior)+hgesPz(mstr,nkz,ior))
   dgesN(nkz) = (2.-2*hcdif1(nkz))*hgesNz(mstr,nkz,ior)+hcdif1(nkz)*hgesNz(mstr,nkz+1,ior)     &
                +hcdif1(nkz)*(hgesNz(mstr,nkz,ior)+hgesNz(mstr,nkz,ior))
   dQ_NKz(nkz) = (2.-2*hcdif1(nkz))*hQ_NKz(mstr,nkz,ior)+hcdif1(nkz)*hQ_NKz(mstr,nkz+1,ior)     &
                 +hcdif1(nkz)*(hQ_NKz(mstr,nkz,ior)+hQ_NKz(mstr,nkz,ior))
   dQ_NBz(nkz) = (2.-2*hcdif1(nkz))*hQ_NBz(mstr,nkz,ior)+hcdif1(nkz)*hQ_NBz(mstr,nkz+1,ior)     &
                 +hcdif1(nkz)*(hQ_NBz(mstr,nkz,ior)+hQ_NBz(mstr,nkz,ior))
   dQ_NGz(nkz) = (2.-2*hcdif1(nkz))*hQ_NGz(mstr,nkz,ior)+hcdif1(nkz)*hQ_NGz(mstr,nkz+1,ior)     &
                 +hcdif1(nkz)*(hQ_NGz(mstr,nkz,ior)+hQ_NGz(mstr,nkz,ior))
   dCChlk(nkz) = (2.-2*hcdif1(nkz))*hCChlkz(mstr,nkz,ior)+hcdif1(nkz)*hCChlkz(mstr,nkz+1,ior)     &
                 +hcdif1(nkz)*(hCChlkz(mstr,nkz,ior)+hCChlkz(mstr,nkz,ior))
   dCChlb(nkz) = (2.-2*hcdif1(nkz))*hCChlbz(mstr,nkz,ior)+hcdif1(nkz)*hCChlbz(mstr,nkz+1,ior)     &
                 +hcdif1(nkz)*(hCChlbz(mstr,nkz,ior)+hCChlbz(mstr,nkz,ior))
   dCChlg(nkz) = (2.-2*hcdif1(nkz))*hCChlgz(mstr,nkz,ior)+hcdif1(nkz)*hCChlgz(mstr,nkz+1,ior)     &
                 +hcdif1(nkz)*(hCChlgz(mstr,nkz,ior)+hCChlgz(mstr,nkz,ior))
   
   ! do nkz=2,nkzs(ior)-1  Beginn Schleife
   forall(nkz = 2:nkzs(ior)-1)
      
      c(nkz-1) = -hcdif1(nkz-1)
      a(nkz) = (2.+(hcdif1(nkz-1)+hcdif1(nkz)))
      b(nkz) = -hcdif1(nkz)
      dT(nkz) = hcdif1(nkz-1)*tempwz(nkz-1,ior)+(2.-(hcdif1(nkz-1)+hcdif1(nkz)))*tempwz(nkz,ior)                 &
                +hcdif1(nkz)*tempwz(nkz+1,ior)
      
      doo(nkz) = hcdif1(nkz-1)*vo2z(nkz-1,ior)+(2.-(hcdif1(nkz-1)+hcdif1(nkz)))*vo2z(nkz,ior)                    &
                 +hcdif1(nkz)*vo2z(nkz+1,ior)
      
      dN4(nkz) = hcdif1(nkz-1)*vNH4z(nkz-1,ior)+(2.-(hcdif1(nkz-1)+hcdif1(nkz)))*vNH4z(nkz,ior)                  &
                 +hcdif1(nkz)*vNH4z(nkz+1,ior)
      
      dN2(nkz) = hcdif1(nkz-1)*vNO2z(nkz-1,ior)+(2.-(hcdif1(nkz-1)+hcdif1(nkz)))*vNO2z(nkz,ior)                  &
                 +hcdif1(nkz)*vNO2z(nkz+1,ior)
      
      dN3(nkz) = hcdif1(nkz-1)*vNO3z(nkz-1,ior)+(2.-(hcdif1(nkz-1)+hcdif1(nkz)))*vNO3z(nkz,ior)                  &
                 +hcdif1(nkz)*vNO3z(nkz+1,ior)
      
      dP(nkz) = hcdif1(nkz-1)*gelPz(nkz-1,ior)+(2.-(hcdif1(nkz-1)+hcdif1(nkz)))*gelPz(nkz,ior)                   &
                +hcdif1(nkz)*gelPz(nkz+1,ior)
      
      dSi(nkz) = hcdif1(nkz-1)*Siz(nkz-1,ior)+(2.-(hcdif1(nkz-1)+hcdif1(nkz)))*Siz(nkz,ior)                      &
                 +hcdif1(nkz)*Siz(nkz+1,ior)
      
      dKi(nkz) = hcdif1(nkz-1)*akiz(nkz-1,ior)+(2.-(hcdif1(nkz-1)+hcdif1(nkz)))*akiz(nkz,ior)                    &
                 +hcdif1(nkz)*akiz(nkz+1,ior)
      
      dGr(nkz) = hcdif1(nkz-1)*agrz(nkz-1,ior)+(2.-(hcdif1(nkz-1)+hcdif1(nkz)))*agrz(nkz,ior)                    &
                 +hcdif1(nkz)*agrz(nkz+1,ior)
      
      dBl(nkz) = hcdif1(nkz-1)*ablz(nkz-1,ior)+(2.-(hcdif1(nkz-1)+hcdif1(nkz)))*ablz(nkz,ior)                    &
                 +hcdif1(nkz)*ablz(nkz+1,ior)
      
      dChl(nkz) = hcdif1(nkz-1)*chlaz(nkz-1,ior)+(2.-(hcdif1(nkz-1)+hcdif1(nkz)))*chlaz(nkz,ior)                 &
                  +hcdif1(nkz)*chlaz(nkz+1,ior)
      
      dChlk(nkz) = hcdif1(nkz-1)*hchlkz(mstr,nkz-1,ior)+(2.-(hcdif1(nkz-1)+hcdif1(nkz)))*hchlkz(mstr,nkz,ior)    &
                   +hcdif1(nkz)*hchlkz(mstr,nkz+1,ior)
      dChlg(nkz) = hcdif1(nkz-1)*hchlgz(mstr,nkz-1,ior)+(2.-(hcdif1(nkz-1)+hcdif1(nkz)))*hchlgz(mstr,nkz,ior)    &
                   +hcdif1(nkz)*hchlgz(mstr,nkz+1,ior)
      dChlb(nkz) = hcdif1(nkz-1)*hchlbz(mstr,nkz-1,ior)+(2.-(hcdif1(nkz-1)+hcdif1(nkz)))*hchlbz(mstr,nkz,ior)    &
                   +hcdif1(nkz)*hchlbz(mstr,nkz+1,ior)
      
      dgesP(nkz) = hcdif1(nkz-1)*hgesPz(mstr,nkz-1,ior)+(2.-(hcdif1(nkz-1)+hcdif1(nkz)))*hgesPz(mstr,nkz,ior)    &
                   +hcdif1(nkz)*hgesPz(mstr,nkz+1,ior)
      dgesN(nkz) = hcdif1(nkz-1)*hgesNz(mstr,nkz-1,ior)+(2.-(hcdif1(nkz-1)+hcdif1(nkz)))*hgesNz(mstr,nkz,ior)    &
                   +hcdif1(nkz)*hgesNz(mstr,nkz+1,ior)
      dQ_NKz(nkz) = hcdif1(nkz-1)*hQ_NKz(mstr,nkz-1,ior)+(2.-(hcdif1(nkz-1)+hcdif1(nkz)))*hQ_NKz(mstr,nkz,ior)    &
                    +hcdif1(nkz)*hQ_NKz(mstr,nkz+1,ior)
      dQ_NBz(nkz) = hcdif1(nkz-1)*hQ_NBz(mstr,nkz-1,ior)+(2.-(hcdif1(nkz-1)+hcdif1(nkz)))*hQ_NBz(mstr,nkz,ior)    &
                    +hcdif1(nkz)*hQ_NBz(mstr,nkz+1,ior)
      dQ_NGz(nkz) = hcdif1(nkz-1)*hQ_NGz(mstr,nkz-1,ior)+(2.-(hcdif1(nkz-1)+hcdif1(nkz)))*hQ_NGz(mstr,nkz,ior)    &
                    +hcdif1(nkz)*hQ_NGz(mstr,nkz+1,ior)
      
      dCChlk(nkz) = hcdif1(nkz-1)*hCChlkz(mstr,nkz-1,ior)+(2.-(hcdif1(nkz-1)+hcdif1(nkz)))*hCChlkz(mstr,nkz,ior)    &
                    +hcdif1(nkz)*hCChlkz(mstr,nkz+1,ior)
      dCChlb(nkz) = hcdif1(nkz-1)*hCChlbz(mstr,nkz-1,ior)+(2.-(hcdif1(nkz-1)+hcdif1(nkz)))*hCChlbz(mstr,nkz,ior)    &
                    +hcdif1(nkz)*hCChlbz(mstr,nkz+1,ior)
      dCChlg(nkz) = hcdif1(nkz-1)*hCChlgz(mstr,nkz-1,ior)+(2.-(hcdif1(nkz-1)+hcdif1(nkz)))*hCChlgz(mstr,nkz,ior)    &
                    +hcdif1(nkz)*hCChlgz(mstr,nkz+1,ior)
   end forall
   c(nkzs(ior)-1) = -2.*hcdif1(nkzs(ior)-1)
   a(nkzs(ior)) = (2.+(hcdif1(nkzs(ior)-1)+hcdif1(nkzs(ior)-1)))
   
   dT(nkzs(ior)) = 2.*hcdif1(nkzs(ior)-1)*tempwz(nkzs(ior)-1,ior)+(2.-2*hcdif1(nkzs(ior)-1))*tempwz(nkzs(ior),ior)
   
   doo(nkzs(ior)) = 2.*hcdif1(nkzs(ior)-1)*vo2z(nkzs(ior)-1,ior)+(2.-2*hcdif1(nkzs(ior)-1))*vo2z(nkzs(ior),ior)
   
   dN4(nkzs(ior)) = 2.*hcdif1(nkzs(ior)-1)*vNH4z(nkzs(ior)-1,ior)+(2.-2*hcdif1(nkzs(ior)-1))*vNH4z(nkzs(ior),ior)
   
   dN2(nkzs(ior)) = 2.*hcdif1(nkzs(ior)-1)*vNO2z(nkzs(ior)-1,ior)+(2.-2*hcdif1(nkzs(ior)-1))*vNO2z(nkzs(ior),ior)
   
   dN3(nkzs(ior)) = 2.*hcdif1(nkzs(ior)-1)*vNO3z(nkzs(ior)-1,ior)+(2.-2*hcdif1(nkzs(ior)-1))*vNO3z(nkzs(ior),ior)
   
   dP(nkzs(ior)) = 2.*hcdif1(nkzs(ior)-1)*gelPz(nkzs(ior)-1,ior)+(2.-2*hcdif1(nkzs(ior)-1))*gelPz(nkzs(ior),ior)
   
   dSi(nkzs(ior)) = 2.*hcdif1(nkzs(ior)-1)*Siz(nkzs(ior)-1,ior)+(2.-2*hcdif1(nkzs(ior)-1))*Siz(nkzs(ior),ior)
   
   dKi(nkzs(ior)) = 2.*hcdif1(nkzs(ior)-1)*akiz(nkzs(ior)-1,ior)+(2.-2*hcdif1(nkzs(ior)-1))*akiz(nkzs(ior),ior)
   
   dGr(nkzs(ior)) = 2.*hcdif1(nkzs(ior)-1)*agrz(nkzs(ior)-1,ior)+(2.-2*hcdif1(nkzs(ior)-1))*agrz(nkzs(ior),ior)
   
   dBl(nkzs(ior)) = 2.*hcdif1(nkzs(ior)-1)*ablz(nkzs(ior)-1,ior)+(2.-2*hcdif1(nkzs(ior)-1))*ablz(nkzs(ior),ior)
   
   dChl(nkzs(ior)) = 2.*hcdif1(nkzs(ior)-1)*chlaz(nkzs(ior)-1,ior)+(2.-2*hcdif1(nkzs(ior)-1))*chlaz(nkzs(ior),ior)
   
   dChlk(nkzs(ior)) = 2.*hcdif1(nkzs(ior)-1)*hchlkz(mstr,nkzs(ior)-1,ior)        &
                      +(2.-2*hcdif1(nkzs(ior)-1))*hchlkz(mstr,nkzs(ior),ior)
   dChlg(nkzs(ior)) = 2.*hcdif1(nkzs(ior)-1)*hchlgz(mstr,nkzs(ior)-1,ior)        &
                      +(2.-2*hcdif1(nkzs(ior)-1))*hchlgz(mstr,nkzs(ior),ior)
   dChlb(nkzs(ior)) = 2.*hcdif1(nkzs(ior)-1)*hchlbz(mstr,nkzs(ior)-1,ior)        &
                      +(2.-2*hcdif1(nkzs(ior)-1))*hchlbz(mstr,nkzs(ior),ior)
   dgesP(nkzs(ior)) = 2.*hcdif1(nkzs(ior)-1)*hgesPz(mstr,nkzs(ior)-1,ior)        &
                      +(2.-2*hcdif1(nkzs(ior)-1))*hgesPz(mstr,nkzs(ior),ior)
   dgesN(nkzs(ior)) = 2.*hcdif1(nkzs(ior)-1)*hgesNz(mstr,nkzs(ior)-1,ior)        &
                      +(2.-2*hcdif1(nkzs(ior)-1))*hgesNz(mstr,nkzs(ior),ior)
   dQ_NKz(nkzs(ior)) = 2.*hcdif1(nkzs(ior)-1)*hQ_NKz(mstr,nkzs(ior)-1,ior)        &
                       +(2.-2*hcdif1(nkzs(ior)-1))*hQ_NKz(mstr,nkzs(ior),ior)
   dQ_NBz(nkzs(ior)) = 2.*hcdif1(nkzs(ior)-1)*hQ_NBz(mstr,nkzs(ior)-1,ior)        &
                       +(2.-2*hcdif1(nkzs(ior)-1))*hQ_NBz(mstr,nkzs(ior),ior)
   dQ_NGz(nkzs(ior)) = 2.*hcdif1(nkzs(ior)-1)*hQ_NGz(mstr,nkzs(ior)-1,ior)        &
                       +(2.-2*hcdif1(nkzs(ior)-1))*hQ_NGz(mstr,nkzs(ior),ior)
   dCChlk(nkzs(ior)) = 2.*hcdif1(nkzs(ior)-1)*hCChlkz(mstr,nkzs(ior)-1,ior)       &
                       +(2.-2*hcdif1(nkzs(ior)-1))*hCChlkz(mstr,nkzs(ior),ior)
   dCChlb(nkzs(ior)) = 2.*hcdif1(nkzs(ior)-1)*hCChlbz(mstr,nkzs(ior)-1,ior)       &
                       +(2.-2*hcdif1(nkzs(ior)-1))*hCChlbz(mstr,nkzs(ior),ior)
   dCChlg(nkzs(ior)) = 2.*hcdif1(nkzs(ior)-1)*hCChlgz(mstr,nkzs(ior)-1,ior)        &
                       +(2.-2*hcdif1(nkzs(ior)-1))*hCChlgz(mstr,nkzs(ior),ior)
   
   call trimat_z(a,b,c,dT,doo,dN4,dN2,dN3,dP,dSi,dKi,dGr,dBl,dChl, dCChlk,dCChlb,dCChlg,                     &
                 dchlk,dchlg,dchlb,dgesP,dgesN,dQ_NKz,dQ_NBz,dQ_NGz,tempwz,vo2z,vNH4z,vNO2z,vNO3z,           &
                 gelPz,Siz,akiz,agrz,ablz,chlaz,hchlkz,hchlgz,hchlbz,hgesPz,hgesNz,hQ_NKz,hQ_NBz,hQ_NGz,     &
                 hCChlkz,hCChlbz,hCChlgz,Qmx_NK,Qmx_NB,Qmx_NG,nkzs,mstr,ior,azStrs)
   j = nkzs(ior)
   do nkz = 1,nkzs(ior)
      xD(j) = 0.9998395                               &
            + 0.000067914       * Tempwz(nkz,ior)     &
            - 0.0000090894      * Tempwz(nkz,ior)**2  &
            + 0.00000010171     * Tempwz(nkz,ior)**3  &
            - 0.0000000012846   * Tempwz(nkz,ior)**4  &
            + 0.000000000011592 * Tempwz(nkz,ior)**5  &
            - 5.0125e-14        * Tempwz(nkz,ior)**6
      xD(j) = xD(j)*1000.
      j = j-1
   enddo
end subroutine v_konz
