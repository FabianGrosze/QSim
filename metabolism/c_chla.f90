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

!>Berechnung der Chla-Bildung im Zeitschritt
!! @author Volker Kirchesch
!! @date 12.11.2015
subroutine c_chla(roh_Chlz, xup_N, xakres, CChlaz, nkz, tflie, C_Bio,         &
                  CChl_Stern, xChla, xaC, xagrow, isyn, iaus)
                  
   implicit none
   
   integer             :: nkz, isyn, iaus
   real                :: xup_n, xchla, xakres, xagrow, xac
   real                :: up_n, tflie, pchl, hconz, hconv
   real                :: hconn, dchl, c_bio, chla_neu, cchl_stern
   real, dimension(50) :: roh_Chlz, CChlaz
   
   
   up_N = xup_N/C_Bio
   ! up_N Umrechnung von Std. auf sec.
   PChl = roh_Chlz(nkz)*up_N/(tflie*86400.)
   hconz = exp(PChl * tflie*86400.)
   hconN = exp(xakres*tflie)
   
   if (isyn == 1) then     ! Geider (1997)
      hconV = 1./CChl_Stern
      dChl = hconV*xaC*1000.*(exp(xagrow*roh_Chlz(nkz)*tflie)-1.)
      chla_neu = xchla + dChl
      hconz = chla_neu/xChla
      hconz = exp((log(chla_neu) -log(xChla)))
   endif
   CChlaz(nkz) = (hconz/hconN)*(1./CChlaz(nkz))
   CChlaz(nkz) = max(CChl_Stern,1./CChlaz(nkz))

end subroutine c_chla