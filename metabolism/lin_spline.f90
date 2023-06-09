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
subroutine lin_spline(dz_alt, dz_neu, deltaz, n_alt_s, n_neu_s, y, yy, i_zeiger)
   implicit none 
   
   ! --- dummy arguments ---
   real,    intent(inout)              :: dz_alt
   real,    intent(inout)              :: dz_neu
   real,    intent(in)                 :: deltaz
   integer, intent(in)                 :: n_alt_s
   integer, intent(in)                 :: n_neu_s
   real,    intent(in),  dimension(50) :: y
   real,    intent(out), dimension(50) :: yy
   integer, intent(in)                 :: i_zeiger
   
   ! --- local variables ----
   integer :: n_alt, n, nn
   real    :: x_neu, x_alt_1, x_alt 
   
   if (i_zeiger == 0) then  
      ! altes Gitter dz = 0.25
      x_neu = 0.0
      n_alt = 2
   else       
      ! neues Gitter dz = 0.25
      x_neu = -dz_neu
      n_alt = 1
   endif
   
   ! Schleife neues Gitter
   x_alt = 0.0
   do n = 1, n_neu_s  
      if (i_zeiger == 0 .and. n == n_neu_s) dz_neu = deltaz
      x_neu = x_neu + dz_neu
      
      ! Schleife altes Gitter
      do nn = n_alt,n_alt_s 
         if (i_zeiger == 1 .and. nn == n_alt_s)dz_alt = deltaz
         
         if (nn > 1) x_alt_1 = x_alt
         x_alt = x_alt + dz_alt
         
         if (x_neu <= x_alt .and. nn > 1) then
            yy(n) = y(nn-1) + (((y(nn) - y(nn-1)) / dz_alt) * (x_neu - x_alt_1))
            n_alt = nn
            x_alt = x_alt - dz_alt
            exit
         endif
         
         if (x_neu <= x_alt .and. nn == 1) then
            yy(n) = y(nn)
            n_alt = nn
            x_alt = x_alt - dz_alt
            exit
         endif
      
      enddo
   
      if (x_neu >= x_alt .and. nn >= n_alt_s .and. n <= n_neu_s) then
         yy(n) = y(n_alt_s)
      endif
   enddo

end subroutine lin_spline
