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
subroutine c_chla(roh_chlz_s, xup_n, xakres, cchlaz_s, tflie, c_bio, &
                  cchl_stern, xchla, xac, xagrow, isyn)
                  
   implicit none
   real,    intent(in)    :: roh_chlz_s
   real,    intent(in)    :: xup_n
   real,    intent(in)    :: xakres
   real,    intent(inout) :: cchlaz_s
   real,    intent(in)    :: tflie
   real,    intent(in)    :: c_bio
   real,    intent(in)    :: cchl_stern
   real,    intent(in)    :: xchla
   real,    intent(in)    :: xac
   real,    intent(in)    :: xagrow
   integer, intent(in)    :: isyn
   
   ! --- local variables ---
   real :: up_n, pchl, hconz, hconv, hconn, dchl, chla_neu
   
   
   up_n = xup_n/c_bio
   ! up_n umrechnung von std. auf sec.
   pchl = roh_chlz_s * up_n / (tflie * 86400.)
   hconz = exp(pchl * tflie * 86400.)
   hconn = exp(xakres * tflie)
   
   if (isyn == 1) then     ! geider (1997)
      hconv = 1./cchl_stern
      dchl = hconv * xac * 1000. * (exp(xagrow * roh_chlz_s * tflie)-1.)
      chla_neu = xchla + dchl
      hconz = chla_neu / xchla
      hconz = exp((log(chla_neu) - log(xchla)))
   endif
   cchlaz_s = (hconz/hconn)  *(1. / cchlaz_s)
   cchlaz_s = max(cchl_stern, 1./cchlaz_s)

end subroutine c_chla
