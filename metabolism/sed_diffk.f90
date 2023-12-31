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
subroutine sed_diffk(tiefe_s, vmitt_s, rau_s, h1, h2, hdkorn_s, vvert, &
                     poro1, poro2, diffk1, diffk2, difkp1, difkp2,     &
                     vvert1, vvert2)
   
   use module_alloc_dimensions
   implicit none
   real, intent(in)  :: tiefe_s
   real, intent(in)  :: vmitt_s
   real, intent(in)  :: rau_s
   real, intent(in)  :: h1, h2
   real, intent(in)  :: hdkorn_s
   real, intent(in)  :: vvert
   real, intent(in)  :: poro1, poro2
   real, intent(out) :: diffk1, diffk2
   real, intent(out) :: difkp1, difkp2
    
   real, intent(out) :: vvert1, vvert2
   
   integer :: n
   real    :: zwdiffk2, zwdiffk1, z2stern
   real    :: z1stern, w, vis_0, u, ust, t
   real    :: sc, scp, sco, sc0, roh_h2o
   real    :: re, raun, poros, hcon, dvis
   real    :: diffmo, diffkm, diffk2_2, diffk1_2, diffk1_1
   real    :: kappa, k, kvis, nuestern, nue, nuestern_2
   real    :: alphao2, alphao1, zvvert1, zvvert2
   
   real, parameter :: g = 9.81
   
   raun = 1./rau_s
   poros = (poro1*h1+poro2*h2)/(h1+h2)
   kvis = 1.e-6
   dvis = 0.001
   roh_h2o = 1000.

   sco = 570.
   scp = 1827.
   vis_0 = 0.01 ! molekulare kinematische Viskosität von reinem Wasser in cm2/s
   t = 1.       ! Einheit in Sekunden
   diffmo = (kvis/scp)*86400.
   
   if (tiefe_s == 0.0 .or. abs(vmitt_s) == 0.0) then
      difkp1 = (kvis / scp) * 86400.
      difkp2 = (kvis / scp) * 86400.
      diffk1 = (kvis / sco) * 86400.
      diffk2 = (kvis / sco) * 86400.
   else
      ust = ((raun*sqrt(g))/(tiefe_s**0.16667))*abs(vmitt_s)
      w = ust
      u = ust
      k = 5.6e-3*poros**3*hdkorn_s**2*g/((1.-poros)**2*kvis)
      kappa = k*dvis/(roh_h2o*g)
      re = w*kappa**0.5/kvis
      nuestern_2 = 12.838 * re**1.3845/(50.60846**1.3845+re**1.3845)
      nuestern_2 = nuestern_2/1.e3
      sc0 = sco
      do n = 1,2
         if (n == 2)sc0 = scp
         diffkm = (kvis/sc0)
         z1stern = 0.0
         hcon = 31.48 * exp(-z1stern/0.291) + 3.29 * exp(-z1stern/1.588) + 46.112 * exp(-z1stern/0.0964) + 0.0077
         nuestern = nuestern_2*hcon
         nue = nuestern*u*w*t + kvis
         
         sc = sc0
         if (re > 10)sc = 1.
         diffk1_1 = (nue/sc)*86400.
         z1stern = h1/(w*t*(w/u))
         hcon = 31.48 * exp(-z1stern/0.291) + 3.29 * exp(-z1stern/1.588) + 46.112 * exp(-z1stern/0.0964) + 0.0077
         nuestern = nuestern_2*hcon
         nue = nuestern*u*w*t + kvis
         sc = sc0
         if (re > 10)sc = 1.
         diffk1_2 = (nue/sc)*86400.
         diffk1 = (diffk1_1 + diffk1_2)/2.
         if (n == 1) then
            ! Diffusionskoeff. für Sauerstoff
            zwdiffk1 = diffk1      
            alphao1 = diffk1_2/diffk1_1
         endif
         z2stern = (h1+h2)/(w*t*(w/u))
         hcon = 31.48 * exp(-z2stern/0.291) + 3.29 * exp(-z2stern/1.588) + 46.112 * exp(-z2stern/0.0964) + 0.0077
         nuestern = nuestern_2*hcon
         nue = nuestern*u*w*t + kvis
         sc = sc0
         if (re > 10)sc = 1.
         diffk2_2 = (nue/sc)*86400.
         diffk2 = (diffk1_2 + diffk2_2)/2.
         if (n == 1) then
            zwdiffk2 = diffk2
            alphao2 = diffk2_2/diffk1_2
         endif
         
      enddo 
      
      difkp1 = diffk1
      difkp2 = diffk2
      diffk1 = zwdiffk1
      diffk2 = zwdiffk2
   endif
   
  
   ! --- sedadv ---
   if (vvert <= 0.0) then
      vvert1 = 0.0
      vvert2 = 0.0
   else
      zvvert1 = vvert * alphao1
      vvert1 = (vvert + zvvert1) / 2.
      
      diffk1 = diffmo
      zvvert2 = zvvert1 * alphao2
      vvert2 = (zvvert1 + zvvert2)/2.
      
      diffk2 = diffmo
      diffk1 = diffk1 + vvert1 * h1
      diffk2 = diffk2 + vvert2 * h2
   endif
end subroutine sed_diffk



