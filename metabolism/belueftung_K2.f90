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

!> Berechnung des Belüftungsbeiwerts in 1/d
subroutine belueftung_k2(raus,tiefes,vmitts,rhyds,flaes,tempws,WLages,hwss,wges,iphys,bbeis)
   implicit none
   
   real                  :: wges
   real                  :: raus,tiefes,vmitts,rhyds,flaes,tempws,bbeis
   real                  :: WLages, hwss
   integer               :: iphys
   
   real                  :: FN,G,UST,Slope,Breite,zw10,fkWind,zWmess,wge10,&
                            SC,Wind_Kl,bbeiw
   character(1000)       :: message
   
   FN = 1./raus
   G = 9.81
   UST = ((FN*G**0.5)/tiefes**0.166667)*abs(vmitts)
   Slope = (vmitts/(raus*rhyds**0.6667))**2
   Breite = flaes/tiefes
   zw10 = 10.
   fkWind = 1.
   zWmess = WLages-hWSs
   if (zWmess > 0.0) fkwind = (zw10/zWmess)**0.11
   
   wge10 = 0.0
   if (wges > 0.0) wge10 = wges * fkwind
   
   SC = -0.0308*tempws**3 + 3.0286*tempws**2 - 112.37*tempws + 1845
   if (SC < 1.0) SC = 1.0
   Wind_Kl = (40.94*SC**(-0.5)) * wge10**1.81 * ((1.2/998.)**0.5)
   ! Wind_Kl = 40.94*SC**(-0.5) * wge10**1.81 * (1.2/998.)**0.5  ! original
   if (isnan(Wind_Kl)) then
      print*, "subroutine belueftung_k2: Variable 'wind_kl' became NaN."
      print*, "   wind_kl  = ", wind_kl
      print*, "   sc       = ", sc
      print*, "   wge10    = ", wge10
      print*, "   fkwind   = ", fkwind
      print*, "   wlage    = ", wlages
      print*, "   hws      = ", hwss
      print*, "   wge      = ", wges  
      
      call qerror("subroutine belueftung_k2: Variable 'wind_kl' became NaN.")
   endif

   
   ! verschiedene Belüftungsformeln </AerFormulas>
   select case (iphys) 
      case(1) 
         ! Berechnung nach Kirchesch
         ! k2=79.6*(v*S)^0.32*H^-0.38*B^-0.16+K2wind (mit Wind)
         bbeiw = 79.6*(abs(vmitts)*Slope)**0.32*tiefes**(-0.38)*Breite**(-0.16)
         bbeis = bbeiw+Wind_Kl/tiefes
      
      case(2) 
         ! Berechnung nach Kirchesch
         ! k2=79.6*(v*S)^0.32*H^-0.38*B^-0.16 (ohne Wind)
         bbeis = 79.6*(abs(vmitts)*Slope)**0.32*tiefes**(-0.38)*Breite**(-0.16)
      
      case(3) 
         ! Berechnung nach Wolf (überarbeitete Form)
         ! k2=10.47*v^0.43*H^-1.37*S^0.22+K2wind (Datengrundlage Wolf 1974)
         ! TODO: rechnet nicht nach angegebener Formel
         bbeis = ((3.+40./raus)*abs(vmitts)/tiefes**2)   ! +0.5/tiefes
         bbeiw = 10.47*abs(vmitts)**0.43*tiefes**(-1.37)*Slope**0.22  ! gleiche Datengrundlage wie Wolf (1974)
         bbeis = bbeis+Wind_Kl/tiefes
      
      case(4)
         ! Berechnung nach Melching (1999)
         ! K2=142*(v*S)^0.333*H^-0.66*B^-0.243
         bbeis = 142.*(abs(vmitts)*Slope)**0.333*tiefes**(-0.66)*Breite**(-0.243)
      
      case default
         write(message, "(a,i0)") "subroutine belueftung_k2: Given value for iphy is invalid: ", iphys
         call qerror(message)

   end select
   
   if (isnan(bbeis)) call qerror("subroutine belueftung_k2: Variable 'bbei' became NaN.")
   
   if (bbeis > 20.) bbeis = 20.
   
   ! Temperaturabhängigkeit
   bbeis = bbeis*(1.024**(tempws-20.))
end subroutine belueftung_k2
