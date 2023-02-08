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

subroutine sys_gitterTrans(mstr, anze, nkzs, nkzmax, dH2D, tempwz, vo2z, vnh4z,&
                           vno2z, vno3z, gelPz, Siz, akiz, agrz, ablz, chlaz,  &
                           hchlkz, hchlgz, hchlbz, hCChlkz, hCChlbz, hCChlgz,  &
                           hgesPz, hgesNz, hQ_NKz, hQ_NBz, hQ_NGz, Uvert,      &
                           i_point, itags, monats, uhrz)
   ! nkzs:    Anzahl der Gitterpunkte des alten Gitters
   ! nkzmax:  Anzahl Gitterpunkte des neuen Gitters
   
   use allodim
   implicit none
   
   integer                         :: nkz, nkzs_alt, nkzmax, monats, j_neu
   integer                         :: j_alt, i_zaehlv, i_var, i_vars, i_point
   integer                         :: itags, ior
   real                            :: uhrz, hcdh2d, gstiefe_neu, gstiefe_alt, dtiefe, dh2d
   integer                         :: anze, nkzs_neu, nkz_alt,mstr
   integer,dimension(1000)         :: nkzs
   real, dimension(50,1000)        :: tempwz, vo2z, vnh4z, vno2z, vno3z, gelPz, Siz, akiz, agrz, ablz, chlaz, Uvert, hc_array
   real, dimension(azStrs,50,1000) :: hchlkz, hchlgz, hchlbz, hgesPz, hgesNz, hQ_NKz, hQ_NBz, hQ_NGz
   real, dimension(azStrs,50,1000) :: hCChlkz, hCChlbz, hCChlgz
   real, dimension(50)             :: y_var, hcTiefe, hcTiefe_neu, tiefe_neu, y_var_neu, zMasse
   
   external                        :: lin_interpolation
   
   do ior = 1,anze + 1   ! Schleife über die longitudinalen Gitterpunkte
      if (i_point == 0) then
         j_alt = nkzs(ior)
         j_neu = nkzmax
      else
         j_alt = nkzmax
         j_neu = nkzs(ior)
      endif
      
      if (nkzs(ior) == 1)cycle  !Bei 1D-Modellierung wird die Gitterbelegunsroutine übersprungen
      ! Ermittlung der Tiefe des alten Gitters
      gstiefe_alt = (j_alt-1)*dH2D
      ! Ermittlung der Tiefe des neuen Gitters
      gstiefe_neu = (j_neu-1)*dH2D
      
      dTiefe = gsTiefe_neu-gstiefe_alt   ! Tiefenänderung zwischen Tiefe im neuen und alten Zeitschritt
      hcdH2D = dTiefe/(j_alt-1)
      nkzs_alt = j_alt
      nkzs_neu = j_neu
      hcTiefe_neu(nkzs_alt) = 0.0 ! hcTiefe_neu: TiefenStufen des Zwischengitters
      do nkz = nkzs_alt-1,1,-1
         hcTiefe_neu(nkz) = hcTiefe_neu(nkz+1)+dH2D+hcdH2D
      enddo
      do nkz = 1,nkzs_neu
         Tiefe_neu(nkz) = gsTiefe_neu-(nkz-1)*dH2D  ! Tiefe_neu Tiefenstufen des neuen Gitters
      enddo
      i_vars = 24
      i_zaehlv = 1
      do i_var = 1,i_vars
         zeiger_var: select case (i_zaehlv)
            case(1)
               do nkz = 1,nkzs_alt
                  y_var(nkz) = tempwz(nkz,ior)
               enddo
            case(2)
               do nkz = 1,nkzs_neu
                  tempwz(nkz,ior) = y_var_neu(nkz)
               enddo
               do nkz = 1,nkzs_alt
                  y_var(nkz) = vO2z(nkz,ior)
               enddo
            case(3)
               do nkz = 1,nkzs_neu
                  vO2z(nkz,ior) = y_var_neu(nkz)
               enddo
               do nkz = 1,nkzs_alt
                  y_var(nkz) = vNH4z(nkz,ior)
               enddo
            case(4)
               do nkz = 1,nkzs_neu
                  vNH4z(nkz,ior) = y_var_neu(nkz)
               enddo
               do nkz = 1,nkzs_alt
                  y_var(nkz) = vNO2z(nkz,ior)
               enddo
            case(5)
               do nkz = 1,nkzs_neu
                  vNO2z(nkz,ior) = y_var_neu(nkz)
               enddo
               do nkz = 1,nkzs_alt
                  y_var(nkz) = vNO3z(nkz,ior)
               enddo
            case(6)
               do nkz = 1,nkzs_neu
                  vNO3z(nkz,ior) = y_var_neu(nkz)
               enddo
               do nkz = 1,nkzs_alt
                  y_var(nkz) = gelPz(nkz,ior)
               enddo
            case(7)
               do nkz = 1,nkzs_neu
                  gelPz(nkz,ior) = y_var_neu(nkz)
               enddo
               do nkz = 1,nkzs_alt
                  y_var(nkz) = Siz(nkz,ior)
               enddo
            case(8)
               do nkz = 1,nkzs_neu
                  Siz(nkz,ior) = y_var_neu(nkz)
               enddo
               do nkz = 1,nkzs_alt
                  y_var(nkz) = akiz(nkz,ior)
               enddo
            case(9)
               do nkz = 1,nkzs_neu
                  akiz(nkz,ior) = y_var_neu(nkz)
               enddo
               do nkz = 1,nkzs_alt
                  y_var(nkz) = agrz(nkz,ior)
               enddo
            case(10)
               do nkz = 1,nkzs_neu
                  agrz(nkz,ior) = y_var_neu(nkz)
               enddo
               do nkz = 1,nkzs_alt
                  y_var(nkz) = ablz(nkz,ior)
               enddo
            case(11)
               do nkz = 1,nkzs_neu
                  ablz(nkz,ior) = y_var_neu(nkz)
               enddo
               do nkz = 1,nkzs_alt
                  y_var(nkz) = chlaz(nkz,ior)
               enddo
            case(12)
               do nkz = 1,nkzs_neu
                  chlaz(nkz,ior) = y_var_neu(nkz)
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
                  y_var(nkz) = Uvert(nkz,ior)
               enddo
            case(16)
               do nkz = 1,nkzs_neu
                  Uvert(nkz,ior) = y_var_neu(nkz)
               enddo
               do nkz = 1,nkzs_alt
                  y_var(nkz) = hgesPz(mstr,nkz,ior)
               enddo
            case(17)
               do nkz = 1,nkzs_neu
                  hgesPz(mstr,nkz,ior) = y_var_neu(nkz)
               enddo
               do nkz = 1,nkzs_alt
                  y_var(nkz) = hgesNz(mstr,nkz,ior)
               enddo
            case(18)
               do nkz = 1,nkzs_neu
                  hgesNz(mstr,nkz,ior) = y_var_neu(nkz)
               enddo
               do nkz = 1,nkzs_alt
                  y_var(nkz) = hQ_NKz(mstr,nkz,ior)
               enddo
            case(19)
               do nkz = 1,nkzs_neu
                  hQ_NKz(mstr,nkz,ior) = y_var_neu(nkz)
               enddo
               do nkz = 1,nkzs_alt
                  y_var(nkz) = hQ_NBz(mstr,nkz,ior)
               enddo
            case(20)
               do nkz = 1,nkzs_neu
                  hQ_NBz(mstr,nkz,ior) = y_var_neu(nkz)
               enddo
               do nkz = 1,nkzs_alt
                  y_var(nkz) = hQ_NGz(mstr,nkz,ior)
               enddo
            case(21)
               do nkz = 1,nkzs_neu
                  hQ_NGz(mstr,nkz,ior) = y_var_neu(nkz)
               enddo
               do nkz = 1,nkzs_alt
                  y_var(nkz) = hCChlkz(mstr,nkz,ior)
               enddo
            case(22)
               do nkz = 1,nkzs_neu
                  hCChlkz(mstr,nkz,ior) = y_var_neu(nkz)
               enddo
               do nkz = 1,nkzs_alt
                  y_var(nkz) = hCChlbz(mstr,nkz,ior)
               enddo
            case(23)
               do nkz = 1,nkzs_neu
                  hCChlbz(mstr,nkz,ior) = y_var_neu(nkz)
               enddo
               do nkz = 1,nkzs_alt
                  y_var(nkz) = hCChlgz(mstr,nkz,ior)
               enddo
            case(24)
               do nkz = 1,nkzs_neu
                  hCChlgz(mstr,nkz,ior) = y_var_neu(nkz)
               enddo
         end select zeiger_var
         i_zaehlv = i_zaehlv+1
         if (i_zaehlv <= 24) then
            call lin_interpolation(y_var,y_var_neu,hctiefe_neu,tiefe_neu,nkzs_alt, nkzs_neu,i_zaehlv,itags,monats,mstr,ior)
         endif
      enddo ! Ende Variablenschleife
   enddo ! Ende Schleife über die longitudinalen Gitterpunkte
end subroutine sys_gitterTrans

