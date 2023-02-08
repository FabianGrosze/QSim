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
subroutine sys_z_Gitter(mstra,hanze,znkzs,hnkzs,dH2D,iFlRi,htempz,ho2z,hnh4z,        &
                        hno2z,hno3z,hgelPz,hSiz,hakiz,hagrz,hablz,hchlaz,hchlkz,     &
                        hchlgz,hchlbz,hgesPz,hgesNz,hQ_NKz,hQ_NBz,hQ_NGz,hCChlkz,    &
                        hCChlbz,hCChlgz,itags,monats)
   ! znkzs:  Anzahl der Gitterpunkte des alten Gitters
   ! hnkzs:  Anzahl Gitterpunkte des neuen Gitters
   
   use allodim
   implicit none
   
   integer                                :: nkz, nkzs_alt, monats, i_zaehlv, i_var
   integer                                :: i_vars, itags, istep, ire, ior
   integer                                :: ihc_wert, ifak, ibeg
   integer                                :: azStr, nkzs, nkzs_neu, nkz_alt,mstr
   real                                   :: hc_wert, hcdh2d, gstiefe_neu, gstiefe_alt, dtiefe
   real                                   :: dhc_wert, dh2d
   integer, dimension(azStrs)             :: mstra, hanze, iFlRi
   integer, dimension(azStrs,1000)        :: znkzs, hnkzs
   real, dimension(azStrs,50,1000)        :: htempz, ho2z, hnh4z, hno2z, hno3z, hgelPz, hSiz, hakiz, hagrz, hablz, hchlaz
   real, dimension(azStrs,50,1000)        :: hchlkz, hchlgz, hchlbz, hgesPz, hgesNz, hQ_NKz, hQ_NBz, hQ_NGz, hCChlkz
   real, dimension(azStrs,50,1000)        :: hCChlbz, hCChlgz
   real, dimension(50)                    :: y_var, hcTiefe, hcTiefe_neu, tiefe_neu, y_var_neu, dzMasse
   
   external                               :: lin_interpolation
   
   
   do azstr = 1,azstrs  ! Strangschleife
      mstr = mstra(azStr)
      
      ! Rückströmung
      if (iFlRi(mstr) == (-1)) then
         ibeg = hanze(mstr)+1
         iRe = 1
         istep = -1
      endif
      
      if (iFlRi(mstr) > (-1)) then
         ibeg = 1
         iRe = hanze(mstr)+1
         istep = 1
      endif
      
      do ior = ibeg,iRe,istep ! Schleife über die longitudinalen Gitterpunkte
         
         if (hnkzs(mstr,ior) == 1)cycle  !Bei 1D-Modellierung wird die Gitterbelegunsroutine übersprungen
         ! Ermittlung der Tiefe des alten Gitters
         gstiefe_alt = (znkzs(mstr,ior)-1)*dH2D
         ! Ermittlung der Tiefe des neuen Gitters
         gstiefe_neu = (hnkzs(mstr,ior)-1)*dH2D
         
         dTiefe = gsTiefe_neu-gstiefe_alt   ! Tiefenänderung zwischen Tiefe im neuen und alten Zeitschritt
         hcdH2D = dTiefe/(znkzs(mstr,ior)-1)
         ifak = 1
         ! if(nkzs_neu<nkzs_alt)ifak = -1
         nkzs_alt = znkzs(mstr,ior)
         nkzs_neu = hnkzs(mstr,ior)
         hcTiefe_neu(nkzs_alt) = 0.0 ! hcTiefe_neu: TiefenStufen des Zwischengitters
         do nkz = nkzs_alt-1,1,-1
            hcTiefe_neu(nkz) = hcTiefe_neu(nkz+1)+dH2D+hcdH2D
            ! runden
            hc_Wert = hcTiefe_neu(nkz)*1000.
            ihc_Wert = int(hc_wert)
            dhc_Wert = hc_Wert - ihc_Wert
            if (dhc_Wert > 0.5) then
               ! hcTiefe_neu(nkz) = (ihc_Wert + 1)/1000.
            else
               ! hcTiefe_neu(nkz) = ihc_Wert/1000.
            endif
         enddo
         
         do nkz = 1,nkzs_neu
            Tiefe_neu(nkz) = gsTiefe_neu-(nkz-1)*dH2D  ! Tiefe_neu Tiefenstufen des neuen Gitters
            ! Runden
            hc_Wert = Tiefe_neu(nkz)*1000.
            ihc_Wert = int(hc_Wert)
            dhc_Wert = hc_Wert - ihc_Wert
            if (dhc_Wert > 0.5) then
               ! Tiefe_neu(nkz) = (ihc_Wert + 1)/1000.
            else
               ! Tiefe_neu(nkz) = ihc_Wert/1000.
            endif
         enddo
         
         if (nkzs_alt == nkzs_neu)cycle
         i_vars = 23
         i_zaehlv = 1
         do i_var = 1,i_vars
            zeiger_var: select case (i_zaehlv)
               case(1)
                  do nkz = 1,nkzs_alt
                     y_var(nkz) = htempz(mstr,nkz,ior)
                  enddo
               case(2)
                  do nkz = 1,nkzs_neu
                     htempz(mstr,nkz,ior) = y_var_neu(nkz)
                  enddo
                  do nkz = 1,nkzs_alt
                     y_var(nkz) = hO2z(mstr,nkz,ior)
                  enddo
               case(3)
                  do nkz = 1,nkzs_neu
                     hO2z(mstr,nkz,ior) = y_var_neu(nkz)
                  enddo
                  do nkz = 1,nkzs_alt
                     y_var(nkz) = hNH4z(mstr,nkz,ior)
                  enddo
               case(4)
                  do nkz = 1,nkzs_neu
                     hNH4z(mstr,nkz,ior) = y_var_neu(nkz)
                  enddo
                  do nkz = 1,nkzs_alt
                     y_var(nkz) = hNO2z(mstr,nkz,ior)
                  enddo
               case(5)
                  do nkz = 1,nkzs_neu
                     hNO2z(mstr,nkz,ior) = y_var_neu(nkz)
                  enddo
                  do nkz = 1,nkzs_alt
                     y_var(nkz) = hNO3z(mstr,nkz,ior)
                  enddo
               case(6)
                  do nkz = 1,nkzs_neu
                     hNO3z(mstr,nkz,ior) = y_var_neu(nkz)
                  enddo
                  do nkz = 1,nkzs_alt
                     y_var(nkz) = hgelPz(mstr,nkz,ior)
                  enddo
               case(7)
                  do nkz = 1,nkzs_neu
                     hgelPz(mstr,nkz,ior) = y_var_neu(nkz)
                  enddo
                  do nkz = 1,nkzs_alt
                     y_var(nkz) = hSiz(mstr,nkz,ior)
                  enddo
               case(8)
                  do nkz = 1,nkzs_neu
                     hSiz(mstr,nkz,ior) = y_var_neu(nkz)
                  enddo
                  do nkz = 1,nkzs_alt
                     y_var(nkz) = hakiz(mstr,nkz,ior)
                  enddo
               case(9)
                  do nkz = 1,nkzs_neu
                     hakiz(mstr,nkz,ior) = y_var_neu(nkz)
                  enddo
                  do nkz = 1,nkzs_alt
                     y_var(nkz) = hagrz(mstr,nkz,ior)
                  enddo
               case(10)
                  do nkz = 1,nkzs_neu
                     hagrz(mstr,nkz,ior) = y_var_neu(nkz)
                  enddo
                  do nkz = 1,nkzs_alt
                     y_var(nkz) = hablz(mstr,nkz,ior)
                  enddo
               case(11)
                  do nkz = 1,nkzs_neu
                     hablz(mstr,nkz,ior) = y_var_neu(nkz)
                  enddo
                  do nkz = 1,nkzs_alt
                     y_var(nkz) = hchlaz(mstr,nkz,ior)
                  enddo
               case(12)
                  do nkz = 1,nkzs_neu
                     hchlaz(mstr,nkz,ior) = y_var_neu(nkz)
                  enddo
                  do nkz = 1,nkzs_alt
                     y_var(nkz) = hchlkz(mstr,nkz,ior)
                  enddo
               case(13)
                  do nkz = 1,nkzs_neu
                     hchlkz(mstr,nkz,ior) = y_var_neu(nkz)
                  enddo
                  do nkz = 1,nkzs_alt
                     y_var(nkz) = hchlgz(mstr,nkz,ior)
                  enddo
               case(14)
                  do nkz = 1,nkzs_neu
                     hchlgz(mstr,nkz,ior) = y_var_neu(nkz)
                  enddo
                  do nkz = 1,nkzs_alt
                     y_var(nkz) = hchlbz(mstr,nkz,ior)
                  enddo
               case(15)
                  do nkz = 1,nkzs_neu
                     hchlbz(mstr,nkz,ior) = y_var_neu(nkz)
                  enddo
                  do nkz = 1,nkzs_alt
                     y_var(nkz) = hgesPz(mstr,nkz,ior)
                  enddo
               case(16)
                  do nkz = 1,nkzs_neu
                     hgesPz(mstr,nkz,ior) = y_var_neu(nkz)
                  enddo
                  do nkz = 1,nkzs_alt
                     y_var(nkz) = hgesNz(mstr,nkz,ior)
                  enddo
               case(17)
                  do nkz = 1,nkzs_neu
                     hgesNz(mstr,nkz,ior) = y_var_neu(nkz)
                  enddo
                  do nkz = 1,nkzs_alt
                     y_var(nkz) = hQ_NKz(mstr,nkz,ior)
                  enddo
               case(18)
                  do nkz = 1,nkzs_neu
                     hQ_NKz(mstr,nkz,ior) = y_var_neu(nkz)
                  enddo
                  do nkz = 1,nkzs_alt
                     y_var(nkz) = hQ_NBz(mstr,nkz,ior)
                  enddo
               case(19)
                  do nkz = 1,nkzs_neu
                     hQ_NBz(mstr,nkz,ior) = y_var_neu(nkz)
                  enddo
                  do nkz = 1,nkzs_alt
                     y_var(nkz) = hQ_NGz(mstr,nkz,ior)
                  enddo
               case(20)
                  do nkz = 1,nkzs_neu
                     hQ_NGz(mstr,nkz,ior) = y_var_neu(nkz)
                  enddo
                  do nkz = 1,nkzs_alt
                     y_var(nkz) = hCChlkz(mstr,nkz,ior)
                  enddo
               case(21)
                  do nkz = 1,nkzs_neu
                     hCChlkz(mstr,nkz,ior) = y_var_neu(nkz)
                  enddo
                  do nkz = 1,nkzs_alt
                     y_var(nkz) = hCChlbz(mstr,nkz,ior)
                  enddo
               case(22)
                  do nkz = 1,nkzs_neu
                     hCChlbz(mstr,nkz,ior) = y_var_neu(nkz)
                  enddo
                  do nkz = 1,nkzs_alt
                     y_var(nkz) = hCChlgz(mstr,nkz,ior)
                  enddo
               case(23)
                  do nkz = 1,nkzs_neu
                     hCChlgz(mstr,nkz,ior) = y_var_neu(nkz)
                  enddo
            end select zeiger_var
            i_zaehlv = i_zaehlv+1
            
            call lin_interpolation(y_var,y_var_neu,hctiefe_neu,tiefe_neu, &
                                   nkzs_alt, nkzs_neu,i_zaehlv,itags,monats,mstr,ior)
            
         enddo ! Ende Variablenschleife
         !
      enddo ! Ende Schleife über die longitudinalen Gitterpunkte
   enddo ! Ende Strangschleife
   
end subroutine sys_z_Gitter