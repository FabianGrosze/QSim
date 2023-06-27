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
subroutine belueftung_k2(rau_s, tiefe_s, vmitt_s, rhyd_s, flae_s, tempw_s,  &
                         wlage_s, hws_s, wge_s, iphy, bbei)
   implicit none
   
   ! --- dummy arguments ---
   real,    intent(in)  :: rau_s
   real,    intent(in)  :: tiefe_s
   real,    intent(in)  :: vmitt_s
   real,    intent(in)  :: rhyd_s
   real,    intent(in)  :: flae_s
   real,    intent(in)  :: tempw_s
   real,    intent(in)  :: wlage_s
   real,    intent(in)  :: hws_s
   real,    intent(in)  :: wge_s
   integer, intent(in)  :: iphy
   real,    intent(out) :: bbei
   
   ! --- local variables ---
   real            :: fn, ust, slope, breite, zw10, fkwind, zwmess, wge10
   real            :: sc, wind_kl, bbeiw
   character(1000) :: message
   
   real, parameter  :: g = 9.81
   external :: qerror
   
   fn = 1./rau_s
   ust = ((fn*sqrt(g)) / tiefe_s**0.166667) * abs(vmitt_s)
   slope = (vmitt_s / (rau_s*rhyd_s**0.6667))**2
   breite = flae_s / tiefe_s
   zw10 = 10.
   fkwind = 1.
   zwmess = wlage_s-hws_s
   if (zwmess > 0.0) fkwind = (zw10/zwmess)**0.11
   
   wge10 = 0.0
   if (wge_s > 0.0) wge10 = wge_s * fkwind
   
   sc = -0.0308*tempw_s**3 + 3.0286*tempw_s**2 - 112.37*tempw_s + 1845
   if (sc < 1.0) sc = 1.0
   
   wind_kl = (40.94*sc**(-0.5)) * wge10**1.81 * ((1.2/998.)**0.5)
   
   if (isnan(wind_kl)) then
      print*, "subroutine belueftung_k2: variable 'wind_kl' became nan."
      print*, "   wind_kl  = ", wind_kl
      print*, "   sc       = ", sc
      print*, "   wge10    = ", wge10
      print*, "   fkwind   = ", fkwind
      print*, "   wlage    = ", wlage_s
      print*, "   hws      = ", hws_s
      print*, "   wge      = ", wge_s  
      
      call qerror("subroutine belueftung_k2: variable 'wind_kl' became nan.")
   endif

   
   ! verschiedene Belüftungsformeln </AerFormulas>
   select case (iphy) 
      case(1) 
         ! Berechnung nach Kirchesch
         ! k2=79.6*(v*S)^0.32*H^-0.38*B^-0.16+K2wind (mit Wind)
         bbeiw = 79.6*(abs(vmitt_s)*Slope)**0.32*tiefe_s**(-0.38)*Breite**(-0.16)
         bbei = bbeiw+Wind_Kl/tiefe_s
      
      case(2) 
         ! Berechnung nach Kirchesch
         ! k2=79.6*(v*S)^0.32*H^-0.38*B^-0.16 (ohne Wind)
         bbei = 79.6*(abs(vmitt_s)*Slope)**0.32*tiefe_s**(-0.38)*Breite**(-0.16)
      
      case(3) 
         ! Berechnung nach Wolf (überarbeitete Form)
         ! k2=10.47*v^0.43*H^-1.37*S^0.22+K2wind (Datengrundlage Wolf 1974)
         ! TODO: rechnet nicht nach angegebener Formel
         bbei = ((3.+40./rau_s)*abs(vmitt_s)/tiefe_s**2)   ! +0.5/tiefe_s
         bbeiw = 10.47*abs(vmitt_s)**0.43*tiefe_s**(-1.37)*Slope**0.22  ! gleiche Datengrundlage wie Wolf (1974)
         bbei = bbei+Wind_Kl/tiefe_s
      
      case(4)
         ! Berechnung nach Melching (1999)
         ! K2=142*(v*S)^0.333*H^-0.66*B^-0.243
         bbei = 142.*(abs(vmitt_s)*Slope)**0.333*tiefe_s**(-0.66)*Breite**(-0.243)
      
      case default
         write(message, "(a,i0)") "subroutine belueftung_k2: Given value for iphy is invalid: ", iphy
         call qerror(message)

   end select
   
   if (isnan(bbei)) then
      call qerror("subroutine belueftung_k2: Variable 'bbei' became NaN.")
   endif
   
   bbei = min(20., bbei)
   ! Temperaturabhängigkeit
   bbei = bbei*(1.024**(tempw_s-20.))
   
end subroutine belueftung_k2
