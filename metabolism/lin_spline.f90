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

!> Berechnung eines linearen Splines
!! @author Volker Kirchesch
!! @date 27.08.2012
subroutine lin_spline (dz_alt,dz_neu,deltaz,n_alt_s,n_neu_s,Y, YY,i_zeiger,iaus,ior)
   implicit none 
   
   integer                :: n_neu_s, n_alt_s, n_alt, n, nn
   integer                :: i_zeiger, ior, iaus
   real                   :: x_neu, x_alt_1, x_alt, dz_neu, dz_alt, deltaz
   integer, dimension(50) :: m
   real,    dimension(50) :: y, yy, aa, bb, ddx
   
   x_neu = 0.0
   x_alt = 0.0
   if (i_zeiger == 0) then  !  i_zeiger=0: altes Gitter dz = 0.25
      n_alt = 2
   else             !  i_zeiger=1: neues Gitter dz = 0.25
      x_neu = -dz_neu
      n_alt = 1
   endif
   
   do n = 1,n_neu_s  ! Schleife neues Gitter
      if (i_zeiger == 0 .and. n == n_neu_s)dz_neu = deltaz
      x_neu = x_neu + dz_neu
      do nn = n_alt,n_alt_s ! Schleife altes Gitter
         if (i_zeiger == 1 .and. nn == n_alt_s)dz_alt = deltaz
         if (nn > 1)x_alt_1 = x_alt
         x_alt = x_alt + dz_alt
         if (x_neu <= x_alt .and. nn > 1) then
            YY(n) = Y(nn-1)+(((y(nn)-y(nn-1))/dz_alt)*(x_neu-x_alt_1))
            ! if(ior==113.and.iaus==1)write(96,*)n_alt_s, n_neu_s,n,nn,x_neu,x_alt
            n_alt = nn
            x_alt = x_alt - dz_alt
            exit
         endif
         if (x_neu <= x_alt .and. nn == 1) then
            YY(n) = Y(nn)
            n_alt = nn
            x_alt = x_alt - dz_alt
            exit
         endif
         cycle
      enddo
      if (ior == 113 .and. iaus == 1)write(96,*)n,nn
      if (x_neu>=x_alt .and. nn>=n_alt_s.and.n <= n_neu_s)YY(n) = Y(n_alt_s)
   enddo
   ! if(x_neu>x_alt)YY(n_neu_s) = Y(n_alt_s)
   if (ior == 113 .and. iaus == 1)write(96,*)n,nn,n_alt_s,n_neu_s,Y(n_alt_s)
end subroutine lin_spline
