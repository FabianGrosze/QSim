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
   use module_organic_carbon, only: organic_carbon
   
   
   implicit none
   integer :: i, j , i1, i2, i3, n,k,nk
   logical :: printi, nix, fehler_nan
   integer :: ilast, i1last
   real :: rlast,rcount
   real :: temperatur_lu, luftfeuchte, wind, strahlung, bewoelkung, wolkentyp
   real , allocatable , dimension (:) :: tempw_k_part, tempsed_k_part, tief_part, u_part
   real , allocatable , dimension (:) :: tempsed_k, tempw_k
   
   
   if (meinrang == 0) print*,'stoffumsatz start'
   !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Stoffumsätze parallelisiert
   do i = 1,part ! Alle Knoten auf diesem Prozessor
      iglob = (i+meinrang*part)
      nk = (i-1)*number_plankt_vari ! Ort im Feld der transportierten, planktischen Variablen
      
      if (iglob <= number_plankt_point) then ! Knotennummer existiert (letzter Prozess)
         if (iglob == kontrollknoten) then
            print*,'stoffumsatz kontrollknoten lokal #',i,' auf Prozess #',meinrang,nur_temp,nur_alter
         endif
         
         if (rb_hydraul_p(2+(i-1)*number_rb_hydraul) > min_tief ) then  ! Knoten nass, d.h. kein Stoffumsatz an trockenen Knoten
            
            if (.not. nur_temp) then ! wenn nur_temp keine anderen Stoffumsätze
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
               
               ! heterotrophe Nanoflagellaten [turned off]
               ! call hnf_huelle(i)
               
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
               
               ! macrophytes [turned off]
               call mphyt_huelle(i)
               
               ! organischer Kohlenstoff BSB
               call organic_carbon_wrapper_3d(i)
               
               !------------------------------------------------------------------------ Stickstoff in Ammonium, Nitrit und Nitrat
               call ncyc_huelle(i)
               do k = 1,number_trans_quant
                  if (isnan(transfer_quantity_p(k+(i-1)*number_trans_quant))) then
                     print*,'nach ncyc_huelle: isnan(transfer_quantity_p  node#',iglob,' variable# ',k,' meinrang = ',meinrang
                     if (meinrang == 0) print*, 'trans_quant_name:',trans_quant_name(k)
                     fehler_nan = .true.
                  endif
               end do
               if (fehler_nan)call qerror("nach ncyc_huelle: isnan(transfer_quantity_p")
               
               !------------------------------------------------------------------------ PH-Wert (falls gewünscht
               if (ipH == 1) call ph_wrapper_3d(i)
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
               call phosphate_wrapper_3d(i)
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
               
               ! Schwermetalle
               ! if (iSchwer == 1)call schwermetalle_huelle(i)
               ! erosion
            end if ! .not. nur_temp
         end if ! Knoten nass
      end if ! Knotennummer existiert(letzter Prozess)
   end do ! Alle Knoten auf diesem Prozess
   !print*,'Stoffumsatz erledigt ',meinrang
   return
end subroutine stoffumsatz
