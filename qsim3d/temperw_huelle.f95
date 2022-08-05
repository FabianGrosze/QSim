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

subroutine temperw_huelle(i)
   ! SUBROUTINE temperw_huelle(ausgeben, temperatur_wa, tiefe_wa, geschw_wa, temperatur_sed, &
   !&                          temperatur_lu, luftfeuchte, wind, strahlung, bewoelkung, wolkentyp, delta_zeit)
   use modell
   use QSimDatenfelder
   implicit none
   !real :: temperatur_lu, luftfeuchte, wind, strahlung, bewoelkung, wolkentyp
   real :: xdtemp_nkz ! delte_temp_3D
   real :: xdtemp_mit ! delte_temp_2D
   integer :: i,j,nk
   !! vermutlich fehlerhaft verwendete Variablen:
   real :: tempmt
   real :: dtempS_mit
   real :: btiefe
   !        call temperw_kern(nkz,xnkzs,xtypw,xschwi,xextk,xhWS,xtempl,xro,xwge,xcloud,xWlage,dH2D, xdtemp_mit                                &
   !                          ,tflie,WUEBK,SPEWKS,PSREFS,xtempwz(1),tempmt,xtempw,btiefe,xTsed,xdtemp(nkz),dtempS_mit,iform_VerdR)
   !    nkz        :   Zähler Tiefenschichten (nkz=1: Oberflächenschicht; nkz=xnkzs: Sohlschicht)
   !    xnkzs      :   Anzahl der Tiefenschichten am Querprofil
   ! ### Die Tiefenschichten müssen immer von 1 bis xnkzs nacheinander aufgerufen werden ####
   !    xtypw      :   Wolkentyp (0-6)
   !    xschwi     :   Globalstrahlung am Querprofil [cal/(cm2*h)]
   !    xextk      :   Lichtextinktion [1/m]
   !    xhWS       :   Wasserspiegellage am Querprofil, Höhe ü. NN [m]
   !    xtempl     :   Lufttemperatur im Zeitschritt [°C]
   !    xro        :   relative Luftfeuchte im Zeitschritt [%]
   !    xwge       :   die in der Höhe zWmess gemessene Windgeschwindigkeit [m/s]
   !    xcloud     :   Bedeckungsgrad in achtel
   !    xWlage     :   Lage der Wetterstation, Höhe ü. NN [m]
   !    dH2D eigentlich delta_z Tiefenschicht-Dicke  z.Z. Wassertiefe ???
   !    xdtemp_mit :   mittlere Temperaturänderung in der Wassersäule [°C]
   !    tflie  Zeitschritt
   !    WUEBK      :   Wärmeübergangskoeffizient
   !    SPEWKS     :   spezifische Wärmekapazität des Sediments
   !    PSREFS     :   Reflektionsanteil der Strahlung an der Sedimentoberfläche
   !    xtempwz    :   Temperatur in der Tiefenschicht nkz am Querprofil [°C] 1 ???
   !    tempmt     :   Mittelwert der Wassertemperatur im Querprofil nach dem Zeitschritt tflie [°C]
   !    xtempw     :   Mittelwert der Wassertemperatur im Querprofil [°C]
   !    btiefe = tiefe
   !    xTsed      :   Sedimenttemperatur [°C]
   !    xdtemp_nkz :   Temperaturänderung in den einzelnen Tiefenschichten [°C/h]
   !    dtempS_mit :   Temperaturänderung durch Sedimenteinfluss (bezogen auf die gesamte Wassersäule) [°C]
   !    IFORM_VERDR:   Schalter für die Auswahl der Verdunstungsformeln
   !    iform_VerdR==1 ! WMO (FGSM-Handbuch)
   !    iform_VerdR==2 ! Sweers (1976) over Land
   !    iform_VerdR==3 ! Rimsha & Donschenko
   !    iform_VerdR==4 ! Priestley-Taylor (1972)
   !    iform_VerdR==5 ! Delclaux et al. (2007)
   
   !> i ist die lokale Knotennummer auf dem jeweiligen Prozessor und läuft von 1 bis part
   iglob = (i+meinrang*part)
   kontroll = (iglob == kontrollknoten)
   !if (kontroll) print*,'temperw_huelle meinrang,i,iglob,wetterstations_nummer,tlmed_T'
   nk = (i-1)*number_plankt_vari ! Ort im Feld der transportierten planktischen Variablen
   tflie = real(deltat)/86400 ! Umwandlung des Zeitschritts von integer sekunden (T-QSim) in real Tage (QSim)
   
   if (num_lev > 1)call qerror("temperw_huelle not ready for 3D")
   if (kontroll) print*,'temperw vorher: temperw, extk, tiefe, temperwz1',planktonic_variable_p(1+nk)  &
       ,transfer_quantity_p(54+(i-1)*number_trans_quant),rb_hydraul_p(2+(i-1)*number_rb_hydraul)  &
       ,plankt_vari_vert_p(1+(1-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev)
   !########################################################################################
   !  das Abarbeiten der einzelnen Schichten erfolgt von
   !  der Oberfläche zur Gewässersohle.(nkz=1: Oberflächenschicht; nkz=xnkzs: Sohlschicht)
   !  übergeben wird die Temperaturänderung dtemp in den einzelnen Schichten
   !########################################################################################
   btiefe = rb_hydraul_p(2+(i-1)*number_rb_hydraul)
   if (btiefe <= min_tief)btiefe = min_tief ! minimale Wassertiefe erhalten
   dH2D = btiefe  ! =tiefe           ! eigentlich delta_z
   
   if ((iform_VerdR < 1) .or. (iform_VerdR > 5)) then
      print*,meinrang,'temperw_huelle iform_VerdR = ',iform_VerdR
      call qerror('iform_VerdR unzulässiger Wert in temperw_huelle')
   endif
   do j = 1,num_lev ! Wassertemperatur tiefenaufgelöst von oben nach unten
      call temperw_kern(                                                      &
                        j                                                     &
                        ,num_lev                                              &
                        ,transfer_quantity_p(67+(i-1)*number_trans_quant)     &
                        ,transfer_quantity_p(64+(i-1)*number_trans_quant)     &
                        ,transfer_quantity_p(54+(i-1)*number_trans_quant)     &
                        ,rb_hydraul_p(3+(i-1)*number_rb_hydraul)              &
                        ,transfer_quantity_p(62+(i-1)*number_trans_quant)     &
                        ,transfer_quantity_p(63+(i-1)*number_trans_quant)     &
                        ,transfer_quantity_p(65+(i-1)*number_trans_quant)     &
                        ,transfer_quantity_p(66+(i-1)*number_trans_quant)     &
                        ,zone(point_zone(iglob))%wettstat%wetterstations_lage &
                        ,dH2D                                                 &
                        ,xdtemp_mit                                           &
                        ,tflie                                                &
                        ,zone(point_zone(iglob))%seditemp%wuebk               &
                        ,zone(point_zone(iglob))%seditemp%spewks              &
                        ,zone(point_zone(iglob))%seditemp%psrefs              &
                        ,plankt_vari_vert_p(j+(1-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev)       &
                        ,tempmt                                               &
                        ,planktonic_variable_p(1+nk)                          &
                        ,btiefe                                               &
                        ,benthic_distribution_p(1+(i-1)*number_benth_distr)   &
                        ,xdtemp_nkz                                           &
                        ,dtempS_mit                                           &
                        ,iform_VerdR                                          &
                        ,kontroll, iglob)
      !   subroutine temperw_kern(nkz,xnkzs,xtypw,xschwi,xextk,xhWS,xtempl,xro,xwge,xcloud,xWlage,dH2D, xdtemp_mit                           &
      !                          ,tflie,WUEBK,SPEWKS,PSREFS,xtempwz1,tempmt,xtempw,btiefe,xTsed,xdtemp_nkz,dtempS_mit,iform_VerdR            &
      !                    ,kontroll ,jjj )
      if (kontroll) print*,meinrang,i,'nach temperw_kern tempw = ',planktonic_variable_p(1+nk),(1+nk)
      !! error check
      if ( isNaN(xdtemp_nkz) ) then
         write(fehler,*)meinrang," isNaN(xdtemp_nkz) ",j,i,iglob
         call qerror(fehler)
      endif
      if ( isNaN(xdtemp_mit) ) then
         write(fehler,*)meinrang," isNaN(xdtemp_mit) ",j,i,iglob
         call qerror(fehler)
      endif
      !! Temperatur change ...
      if (rb_hydraul_p(2+(i-1)*number_rb_hydraul) > min_tief) then ! wet nodes
         planktonic_variable_p(1+nk) = tempmt  ! = planktonic_variable_p(1+nk) + xdtemp_mit ! tempw(1)
         !plankt_vari_vert_p(j+(1-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) =       &
         !plankt_vari_vert_p(j+(1-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) * xdtemp_nkz ! tempwz(j,1)
      else ! dry nodes
         planktonic_variable_p(1+nk) = transfer_quantity_p(62+(i-1)*number_trans_quant) ! water temperature equals air temp.
      endif ! wet nodes
      !keine extra Tiefenauflösung im 3D
      plankt_vari_vert_p(j+(1-1)*num_lev+(i-1)*number_plankt_vari_vert*num_lev) = planktonic_variable_p(1+nk)
   end do ! all j num_lev
   return
end subroutine temperw_huelle
