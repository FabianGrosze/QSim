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
subroutine stoffumsatz()
   use modell
   implicit none
   integer :: i, j , i1, i2, i3, n,k,nk
   logical :: printi, nix, fehler_nan
   integer :: ilast, i1last
   real :: rlast,rcount
   real :: temperatur_lu, luftfeuchte, wind, strahlung, bewoelkung, wolkentyp
   real , allocatable , dimension (:) :: tempw_k_part, tempsed_k_part, tief_part, u_part
   real , allocatable , dimension (:) :: tempsed_k, tempw_k
   if (meinrang == 0)print*,'stoffumsatz start'
   !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Stoffumsätze parallelisiert
   do i = 1,part ! Alle Knoten auf diesem Prozessor
      iglob = (i+meinrang*part)
      nk = (i-1)*number_plankt_vari ! Ort im Feld der transportierten, planktischen Variablen
      if (iglob <= number_plankt_point) then ! Knotennummer existiert (letzter Prozess)
         if (iglob == kontrollknoten)print*,'stoffumsatz kontrollknoten lokal #',i,' auf Prozess #',meinrang,nur_temp,nur_alter
         if (rb_hydraul_p(2+(i-1)*number_rb_hydraul) > min_tief ) then  ! Knoten nass, d.h. kein Stoffumsatz an trockenen Knoten
            if ( .not. nur_temp) then ! wenn nur_temp keine anderen Stoffumsätze
               !------------------------------------------------------------------------ Wasseralter
               if (nur_alter) then
                  call alter(i)
                  if (iglob == kontrollknoten) print*,'stoffumsatz: nur aufenthaltszeit (alter)'
                  cycle ! bei nur_alter nix anderes
               endif
               !------------------------------------------------------------------------ Stofflüsse in/aus Sediment ## unklar ## in Überarbeitung
               !call sedflux_huelle(i)
               !------------------------------------------------------------------------ Konsumenten / Rotatorien
               call konsum_huelle(i)
               do k = 1,number_trans_quant
                  if (isnan(transfer_quantity_p(k+(i-1)*number_trans_quant))) then
                     print*,'nach konsum_huelle: isnan(transfer_quantity_p  node#',iglob,' variable# ',k,' meinrang = ',meinrang
                     if (meinrang == 0)print*,'trans_quant_name:',trans_quant_name(k)
                     !fehler_nan=.true.
                  endif
               end do
               !------------------------------------------------------------------------ Corophium, auch ein Konsument ## in Überarbeitung
               !call coroph_huelle(i)
               !------------------------------------------------------------------------ Muscheln (dreissena), auch Konsumenten
               call dreissen_huelle(i)
               !if(iglob.eq.kontrollknoten) print*,'stoffumsatz: deissena gerade mal weggeschaltet 28okt19'
               !------------------------------------------------------------------------ heterotrophe Nanoflagellaten
               !call hnf_huelle(i)
               !----------------------------------------------------------------------------------------------------------- Algen-Baustein
               do k = 1,number_plankt_vari
                  if (isnan(planktonic_variable_p(k+nk))) then
                     print*,'vor algae_huelle: isnan(planktonic_variable_p  node#',iglob,' variable# ',k,' meinrang = ',meinrang
                     if (meinrang == 0)print*,'planktonic_variable_name:',planktonic_variable_name(k)
                  endif
               end do
               call algae_huelle(i)
               ! algaeski()
               ! algaesbl()
               ! algaesgr()
               do k = 1,number_trans_quant
                  if (isnan(transfer_quantity_p(k+(i-1)*number_trans_quant))) then
                     print*,'nach algae_huelle: isnan(transfer_quantity_p  node#',iglob,' variable# ',k,' meinrang = ',meinrang
                     if (meinrang == 0)print*,'trans_quant_name:',trans_quant_name(k)
                     !fehler_nan=.true.
                  endif
               end do
               do k = 1,number_plankt_vari
                  if (isnan(planktonic_variable_p(k+nk))) then
                     print*,'nach algae_huelle: isnan(planktonic_variable_p  node#',iglob,' variable# ',k,' meinrang = ',meinrang
                     if (meinrang == 0)print*,'planktonic_variable_name:',planktonic_variable_name(k)
                  endif
               end do
               !------------------------------------------------------------------------ benthische Algen
               call albenth_huelle(i)
               !------------------------------------------------------------------------ Makrophythen
               call mphyt_huelle(i)
               !------------------------------------------------------------------------ organischer Kohlenstoff BSB
               if (iglob == kontrollknoten) print*,'stoffumsatz: bac(1) vor orgc_huelle = '  &
                   , planktonic_variable_p(42+(i-1)*number_plankt_vari)
               call orgc_huelle(i)
               do k = 1,number_trans_quant
                  if (isnan(transfer_quantity_p(k+(i-1)*number_trans_quant))) then
                     print*,'nach orgc_huelle: isnan(transfer_quantity_p  node#',iglob,' variable# ',k,' meinrang = ',meinrang
                     if (meinrang == 0)print*,'trans_quant_name:',trans_quant_name(k)
                     !fehler_nan=.true.
                  endif
               end do
               !------------------------------------------------------------------------ Stickstoff in Ammonium, Nitrit und Nitrat
               call ncyc_huelle(i)
               do k = 1,number_trans_quant
                  if (isnan(transfer_quantity_p(k+(i-1)*number_trans_quant))) then
                     print*,'nach ncyc_huelle: isnan(transfer_quantity_p  node#',iglob,' variable# ',k,' meinrang = ',meinrang
                     if (meinrang == 0)print*,'trans_quant_name:',trans_quant_name(k)
                     fehler_nan = .true.
                  endif
               end do
               if (fehler_nan)call qerror("nach ncyc_huelle: isnan(transfer_quantity_p")
               !------------------------------------------------------------------------ PH-Wert (falls gewünscht
               if (ipH == 1) call ph_huelle(i)
            endif ! .not. nur_temp
         end if ! Knoten nass ... Temperw auch an trockenen Knoten !!!
         !------------------------------------------------------------------------ Wasser(+Sediment)-Temperatur
         call temperw_huelle(i) !! übergeben wird die Prozess-lokale Knotennummer !!
         if ((nur_temp) .and. (i == 1))                                                                                 &
             print*,'stoffumsatz: nur temperatursimulation; meinrang, i, iglob, itemp, rb_hydraul_p(2 , min_tief = '  &
             ,meinrang,i,iglob,itemp,rb_hydraul_p(2+(i-1)*number_rb_hydraul),min_tief
         if (rb_hydraul_p(2+(i-1)*number_rb_hydraul) > min_tief ) then  ! Knoten nass, d.h. kein Stoffumsatz an trockenen Knoten
            if ( .not. nur_temp) then
               !------------------------------------------------------------------------ ortho-Phosphat Phosphor
               call po4s_huelle(i)
               !------------------------------------------------------------------------ Silikat
               call silikat_huelle(i)
               !------------------------------------------------------------------------ Sauerstoff
               call oxygen_huelle(i)
               !------------------------------------------------------------------------ Schwebstoff-Bilanz + Sedimentation Min.
               call schweb_huelle(i)
               
               !------------------------------------------------------------------------ coliforme Keime
               if (iColi == 1) then
                  call coliform_huelle(i)
               else
                  planktonic_variable_p(61+(i-1)*number_plankt_vari) = 0.0
               endif
               
               !------------------------------------------------------------------------ Erosion von Schwebstoffe
               if (ieros == 1)call erosion_huelle(i)
               
               !------------------------------------------------------------------------ Schwermetalle
               if (iSchwer == 1)call schwermetalle_huelle(i)
               ! erosion
            end if ! .not. nur_temp
         end if ! Knoten nass
      end if ! Knotennummer existiert(letzter Prozess)
   end do ! Alle Knoten auf diesem Prozess
   !print*,'Stoffumsatz erledigt ',meinrang
   return
end subroutine stoffumsatz
!
!> \page Num_Umsatz Wachstum / Zerfall
!! \section stoumein Einführung
!! Der Stoffumsatz,\n
!! \f[
!! \frac{\partial c_m}{\partial t} = Q ( c_1 \ldots c_m \ldots c_M, x_i, t )
!! \f]\n
!! mit: \n <ul>
!!    <li> \f$ c_m \f$ = m-te Konzentration; Zwischenwert im Sinne des des fractional step algorithm, \n siehe: \ref fracStep \n\n</li>
!!    <li> \f$ t \f$ = Zeit\n\n</li>
!!    <li> \f$ Q_m \f$ = Quelle/Senke der Konzentration m (lokale Veränderungsrate, Stoffumsatz); \n\n</li>
!! </ul>,\n
!! wird explizit in der folgenden Form diskretisiert:\n
!! \f[
!! \frac{ {c_m}^{n+1}-{c_m}^{n} }{ \Delta t}=  Q ( {c_1}^{n} \ldots {c_m}^{n} \ldots {c_M}^{n}, {x_i}^{n}, t^{n} )
!! \f] \n
!! mit: \n <ul>
!!    <li> \f$ n \f$ = Zeitschrittzähler\n\n</li>
!!    <li> \f$ \Delta t \f$ = Zeitschrittweite\n\n</li>
!! </ul>,\n
!! \section abumex Abschätzung Näherungsgüte
!! Bei biologischen Stoffumsetzungsprozesse handelt es sich zumeist um Wachstums- und Zerfallsprozesse,
!! die sich mittels einer Wachstumsrate mathematisch beschreiben lassen:
!! \f[
!! \frac{\partial c_m}{\partial t} = Q  = \mu_m (c_1 \ldots c_M) \cdot c_m
!! \f]\n
!! mit: \n
!! \f$ \mu_m \f$ = Wachstumsrate der m-ten Konzentration, die von anderen Konzentrationen abhängig sein kann.\n\n
!! Durch Normierung entstehen dimensionslose Größen: \f$ t' = \mu \cdot t\f$ und \f$ c' = \frac{c^n}{c^0}\f$. \n
!! Unter der Annahme, dass die Wachstumsrate konstant über die Zeit ist,\n
!! vereinfacht sich die obige Differentialgleichung zu: \f$ \frac{\partial c'}{\partial t'} = c'\f$\n
!! mit der analytische Lösung: \f$ c' = e^{t'} \f$. \n\n
!! Diese Differentialgleichung ist nun ein leicht handhabbarer Test für Diskretisierungsverfahren:
!! <table >
!! <tr><th>Verfahren</th><th> Ansatz    </th><th>   umgeformt            </th></tr>
!! <tr><td> explizit    </td><td> \f$ \frac{c'^{n+1}-c'^{n}}{\Delta t'} = c'^{n}\f$ </td><td> \f$ c'^{n+1}=c'^{n}\cdot (1+\Delta t') \f$ </td></tr>
!! <tr><td> semi-implizit</td><td> \f$ \frac{c'^{n+1}-c'^{n}}{\Delta t'} = 1/2 \cdot ( c'^{n+1} + c'^{n} ) \f$  </td><td>  \f$   c'^{n+1}=c'^{n}\cdot ( (1+\Delta t'/2)/(1-\Delta t'/2) )\f$</td></tr>
!! <tr><td> implizit </td><td>  \f$ \frac{c'^{n+1}-c'^{n}}{\Delta t'} = c'^{n+1} \f$ </td><td>  \f$ c'^{n+1}=c'^{n}\cdot 1/(1-\Delta t') \f$  </td></tr>
!! </table>
!!
!! \image html wachstum_disc_grob.svg "" width=5cm
!! grobe Diskretisierung \f$ \Delta t'=0.2 \f$  Abweichung explizit 8.5%\n\n
!! \image html wachstum_disc_mittel.svg ""  width=5cm
!! mittelfeine Diskretisierung \f$ \Delta t'=0.05 \f$ Abweichung explizit 2.4%\n\n
!! \image html wachstum_disc_fein.svg ""  width=5cm
!! feine Diskretisierung \f$ \Delta t'=0.01 \f$  Abweichung explizit 1.6%\n\n
!!
!! die größten Wachstumsraten treten in der Gewässermikrobiologie beim Algenwachstum auf,
!! das unter optimalen Bedingungen maximale Werte von 2 pro Tag annimmt. Das bedeutet, dass die obigen Abbildungen sich über einen halben Tag erstrecken.
!! der mittelfeine Zeitschritt entspricht dann 2160 Sekunden und der feine 432 Sekunden. Daraus folgert, dass sich die im Gewässergütemodell
!! erwartbaren Wachstums und Zerfallsprozesse bei Zeitschritten von 900s mit guter Genauigkeit und bei Zeitschritten von 300s mit sehr guter Genauigkeit
!! numerisch erfassen lassen.
!!
!! \section Stoffumsetzungsmodule Stoffumsetzungs-Module
!! Das Obengenannte bezieht sich nun nur ganz allgemein auf den Stoffumsatz irgendeiner Konzentration.
!! Eine konkrete Konzentration z. B. der Sauerstoffgehalt verändert sich durch Beiträge aus verschiedenen Stoffumsetzungsprozessen,
!! die in den Modulen des Gewässergütemodells modelliert sind.\n\n
!! Im Abschnitt \ref lnk_ueberblick sind alle von QSim modellierten Prozesse aufgeführt.\n\n
!! Die Zuordnung, welche Konzentrationen von welchem Modul geändert wird, befindet sich in der Liste in: \ref planktische_variablen .
!!
!! \n\n aus Datei stoffumsatz.f95; zurück:\ref numerik
!      ! Lasttest
!      ilast=10000
!      i1last=ilast/proz_anz
!      rcount=0.0
!      rlast = 7
!      do i1=1,i1last !! Rechenlast erzeugen !!
!         rlast = rlast + (1000   + meinrang + real(i1))
!         do i2=1,ilast
!            rlast = rlast + (1000   + meinrang + real(i2))
!            do i3=1,ilast
!               rlast = rlast + (1000   + meinrang + real(i3))
!               rlast = rlast - (1000   + meinrang + real(i3))
!               rcount=rcount+0.00001
!            end do !! i3
!            rlast = rlast - (1000   + meinrang + real(i2))
!         end do !! i2
!         rlast = rlast - (1000   + meinrang + real(i1))
!      end do !! i1
!      print*,'Rechenlast, rcount=',rcount,' meinrang', meinrang,' part=',part
!      1 Prozessor>16:49:34-17:10:42=21:08=1268s  16 Prozess.>17:12:41-17:14:32=01:51=111s*16=1776s  71%
!
!! *\f[
!! {c_m}^{n+1}={c_m}^{n} + \delta t \cdot Q ( {c_1}^{n} \ldots {c_m}^{n} \ldots {c_M}^{n}, {x_i}^{n}, t^{n} )
!! \f] \n
