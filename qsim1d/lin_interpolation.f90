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

!> Bestimmung der neuen Gitterwerte durch lineare Interpolation
subroutine lin_interpolation(y_var, y_var_new, hctiefe_new, tiefe_new, nkzs_old,  &
                             nkzs_new, i_zaehlv, itags, monats, mstr, ior)
   implicit none
   
   ! --- dummy arguments ---
   real,    intent(in),  dimension(50) :: y_var
   real,    intent(out), dimension(50) :: y_var_new
   real,    intent(in),  dimension(50) :: hctiefe_new
   real,    intent(in),  dimension(50) :: tiefe_new
   integer, intent(in)                 :: nkzs_old
   integer, intent(in)                 :: nkzs_new
   integer, intent(in)                 :: i_zaehlv ! TODO: unused
   integer, intent(in)                 :: itags    ! TODO: unused
   integer, intent(in)                 :: monats   ! TODO: unused
   integer, intent(in)                 :: mstr     ! TODO: unused
   integer, intent(in)                 :: ior      ! TODO: unused
   
   ! --- local variables ---
   integer :: i_neu, nkz_alt, nkz
   real    :: yachs, stg
   
   i_neu = 1
   do nkz_alt = 1,nkzs_old-1
      do nkz = i_neu,nkzs_new
         if (nkz == 1 .and. tiefe_new(nkz) > hctiefe_new(nkz_alt)) then
            y_var_new(nkz) = y_var(nkz_alt)
            cycle
         endif
        
         if (nkz == nkzs_new .and. tiefe_new(nkz) < hctiefe_new(nkzs_old)) then
            y_var_new(nkz) = y_var(nkzs_old)
            exit
         endif
         
         if (tiefe_new(nkz) <= hctiefe_new(nkz_alt) .and. tiefe_new(nkz)>=hctiefe_new(nkz_alt+1)) then
            Stg = (y_var(nkz_alt+1)-y_var(nkz_alt))/(hctiefe_new(nkz_alt+1)-hctiefe_new(nkz_alt))
            Yachs = y_var(nkz_alt)-Stg*hctiefe_new(nkz_alt)
            y_var_new(nkz) = Stg*tiefe_new(nkz)+Yachs
            cycle
         else
            i_neu = nkz
            exit
         endif
      
      enddo
   enddo
end subroutine lin_interpolation