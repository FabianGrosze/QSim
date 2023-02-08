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

!> @ author Volker Kirchesch
!! @date 21.04.2004
subroutine Sed_DiffK(tiefe,vmitt,rau,H1,H2,hdKorn,DiffK1,DiffK2,DifKP1,DifKP2,poro1,poro2,vvert,vvert1,vvert2    &
                     ,mstr,ior,itags,monats,uhrz, kontroll ,jjj )
   
   use allodim
   implicit none
   
   integer                      :: monats, n, mstr, itags, ior
   real                         :: zwdiffk2, zwdiffk1, z2stern
   real                         :: z1stern, w, vvert, vvert2, vvert1
   real                         :: vis_0, u, ust, uhrz, t
   real                         :: sc, scp, sco, sc0, roh_h2o
   real                         :: re, raun, poros, poro2, poro1
   real                         :: hcon, h2, h1, g, dvis
   real                         :: dkorn, difkp2, difkp1, diffmo, diffkm
   real                         :: diffk2_2, diffk2, diffk1_2, diffk1_1, diffk1
   real                         :: Kappa, K, kVis, nueStern, nue, nueStern_2
   real                         :: alphao2, alphao1
   logical, intent(in)          :: kontroll  !< debugging
   integer, intent(in)          :: jjj       !< debugging
   real, dimension(azStrs,1000) :: hdKorn
   real, dimension(1000)        :: Tiefe, vmitt,rau
   
   external                     :: sedadv
   
   
   raun = 1./rau(ior)
   g = 9.81
   poros = (poro1*H1+poro2*H2)/(H1+H2)
   kVis = 1.e-6
   dVis = 0.001
   roh_H2O = 1000.
   dKorn = hdKorn(mstr,ior)
   ScO = 570.
   ScP = 1827.
   Vis_0 = 0.01 ! molekulare kinematische Viskosität von reinem Wasser in cm2/s
   T = 1.       ! Einheit in Sekunden
   Diffmo = (kVis/SCP)*86400.
   if (Tiefe(ior) == 0.0 .or. abs(vmitt(ior)) == 0.0) then
      DifKP1 = (kVis/SCP)*86400.
      DifKP2 = (kvis/SCP)*86400.
      DiffK1 = (kvis/SCO)*86400.
      DiffK2 = (kvis/SCO)*86400.
   else
      ust = ((raun*sqrt(g))/(tiefe(ior)**0.16667))*abs(vmitt(ior))
      W = Ust
      U = Ust
      K = 5.6e-3*poros**3*dKorn**2*g/((1.-Poros)**2*kVis)
      kappa = K*dVis/(roh_H2O*g)
      Re = W*kappa**0.5/kVis
      nueStern_2 = 12.838 * Re**1.3845/(50.60846**1.3845+Re**1.3845)
      nueStern_2 = nueStern_2/1.e3
      Sc0 = ScO
      do n = 1,2
         if (n == 2)Sc0 = ScP
         DiffKm = (kVis/Sc0)
         z1Stern = 0.0
         hcon = 31.48 * exp(-z1Stern/0.291) + 3.29 * exp(-z1Stern/1.588) + 46.112 * exp(-z1Stern/0.0964) + 0.0077
         nueStern = nueStern_2*hcon
         nue = nueStern*U*W*T + kVis
         
         Sc = Sc0
         if (Re > 10)Sc = 1.
         DiffK1_1 = (nue/Sc)*86400.
         z1Stern = H1/(W*T*(W/U))
         hcon = 31.48 * exp(-z1Stern/0.291) + 3.29 * exp(-z1Stern/1.588) + 46.112 * exp(-z1Stern/0.0964) + 0.0077
         nueStern = nueStern_2*hcon
         nue = nueStern*U*W*T + kVis
         Sc = Sc0
         if (Re > 10)Sc = 1.
         DiffK1_2 = (nue/Sc)*86400.
         DiffK1 = (Diffk1_1 + DiffK1_2)/2.
         if (n == 1) then
            zwDiffK1 = DiffK1      ! Diffusionskoeff. für Sauerstoff
            alphaO1 = DiffK1_2/Diffk1_1
         endif
         z2Stern = (H1+H2)/(W*T*(W/U))
         hcon = 31.48 * exp(-z2Stern/0.291) + 3.29 * exp(-z2Stern/1.588) + 46.112 * exp(-z2Stern/0.0964) + 0.0077
         nueStern = nueStern_2*hcon
         nue = nueStern*U*W*T + kVis
         Sc = Sc0
         if (Re > 10)Sc = 1.
         DiffK2_2 = (nue/Sc)*86400.
         DiffK2 = (DiffK1_2 + DiffK2_2)/2.
         if (n == 1) then
            zwDiffK2 = Diffk2
            alphaO2 = DiffK2_2/DiffK1_2
         endif
         
      enddo  ! Ende Schleife zur Berechnung von DiffK und DiffKP
      DifKP1 = DiffK1
      DifKP2 = DiffK2
      DiffK1 = zwDiffK1
      DiffK2 = zwDiffK2
   endif
   vvert1 = 0.0
   vvert2 = 0.0
   
   if (vvert > 0.0) then
      call SedAdv(vvert,alphaO1,alphaO2,DiffK1,DiffK2,vvert1,vvert2,H1,H2,Diffmo)
   endif
end subroutine Sed_DiffK


subroutine SedAdv(vvert,alphaO1,alphaO2,DiffK1,DiffK2,vvert1,vvert2,H1,H2,Diffmo)

   implicit none
   
   real    :: zvvert2, zvvert1, vvert, vvert2, vvert1
   real    :: h2, h1, diffmo, diffk2, diffk1
   real    :: alphao2, alphao1

   zvvert1 = vvert * alphaO1
   vvert1 = (vvert + zvvert1)/2.
   
   DiffK1 = Diffmo
   zvvert2 = zvvert1 * alphaO2
   vvert2 = (zvvert1 + zvvert2)/2.
   
   DiffK2 = Diffmo
   DiffK1 = DiffK1 + vvert1 * H1
   DiffK2 = DiffK2 +vvert2 * H2
end subroutine SedAdv
