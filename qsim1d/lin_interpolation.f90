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
subroutine lin_interpolation(y_var,y_var_neu,hctiefe_neu,tiefe_neu,nkzs_alt, &
                             nkzs_neu,i_zaehlv,itags,monats,mstr,ior)
   
   real, dimension(50) :: tiefe_neu, hctiefe_neu, y_var, y_var_neu
   
   i_neu = 1
   do nkz_alt = 1,nkzs_alt-1
      do nkz = i_neu,nkzs_neu
         if (nkz == 1 .and. tiefe_neu(nkz) > hcTiefe_neu(nkz_alt)) then
            y_var_neu(nkz) = y_var(nkz_alt)
            cycle
         endif
         if (nkz == nkzs_neu .and. Tiefe_neu(nkz) < hctiefe_neu(nkzs_alt)) then
            y_var_neu(nkz) = y_var(nkzs_alt)
            exit
         endif
         
         if (tiefe_neu(nkz) <= hcTiefe_neu(nkz_alt) .and. Tiefe_neu(nkz)>=hcTiefe_neu(nkz_alt+1)) then
            Stg = (y_var(nkz_alt+1)-y_var(nkz_alt))/(hcTiefe_neu(nkz_alt+1)-hcTiefe_neu(nkz_alt))
            Yachs = y_var(nkz_alt)-Stg*hcTiefe_neu(nkz_alt)
            y_var_neu(nkz) = Stg*Tiefe_neu(nkz)+Yachs
            cycle
         else
            i_neu = nkz
            exit
         endif
      
      enddo
   enddo
end subroutine lin_interpolation