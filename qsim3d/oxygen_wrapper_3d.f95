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

!> Hüllfunktion zum Aufruf von oxygen().
subroutine oxygen_wrapper_3d(i)
   use modell
   use QSimDatenfelder
   use module_metabolism, only: oxygen
   
   implicit none
   integer, intent(in)  :: i !< lokale Knotennummer auf Prozessor; läuft von 1 bis part
   integer              :: j, nk, i2, k
   
   
   iglob = i + meinrang*part ! globale Knotennummer
   i2 = zone(point_zone(iglob))%wettstat%wetterstations_nummer ! ist parallel
   nk = (i-1)*number_plankt_vari ! Ort im Feld der transporterten, planktischen Variablen
   control = (iglob == kontrollknoten)
   
  
   tiefe(1) = rb_hydraul_p(2+(i-1)*number_rb_hydraul) ! Wassertiefe
  
   rhyd(1) = tiefe(1) ! hydraulischer Radius | sinnvollste Annahme im mehrdimensionalen
   flae(1) = tiefe(1)*500.0 !! Breite konstant 500 m ; wird in der Belüftungsformel verwendet,
   ! hat aber keine Entsprechung im Mehrdimensionalen, daher sinnvoller Wert fürs Ästuar
   tflie = real(deltat)/86400. ! Umwandlung des Zeitschritts von integer sekunden (QSim-3D) in real Tage (QSim-1D)
  
   ! TODO: (Schoenung): toc_csb sollte an zentrale Stelle als Parameter definiert werden.
   TOC_CSB = 3.1
   
   ! --- check for nan ---
   do k = 1,number_benth_distr
      if (isnan(benthic_distribution_p(k+(i-1)*number_benth_distr))) then
         print '("before oxygen: isnan(benthic_distribution_p): node #", i0, ", variable #", i0)', iglob, k
         if (meinrang == 0)print*,'benth_distr_name: ', benth_distr_name(k)
      endif
   enddo
   
  
   ! --- metabolism ---
   if (control) then 
      print*,'before oxygen():'
      print*, '  vo2 = ', planktonic_variable_p( 2+nk)
   endif
      
   call oxygen(planktonic_variable_p( 2+nk),                         & ! vO2
               planktonic_variable_p(50+nk),                         & ! zooind_s
               transfer_quantity_p(34+(i-1)*number_trans_quant),     & ! agrNH4_s
               transfer_quantity_p(33+(i-1)*number_trans_quant),     & ! akiNH4_s
               transfer_quantity_p(35+(i-1)*number_trans_quant),     & ! ablNH4_s
               transfer_quantity_p(37+(i-1)*number_trans_quant),     & ! agrNO3_s
               transfer_quantity_p(36+(i-1)*number_trans_quant),     & ! akiNO3_s
               transfer_quantity_p(38+(i-1)*number_trans_quant),     & ! ablNO3_s
               transfer_quantity_p(21+(i-1)*number_trans_quant),     & ! dalggr_s
               transfer_quantity_p(20+(i-1)*number_trans_quant),     & ! dalgki_s
               transfer_quantity_p(22+(i-1)*number_trans_quant),     & ! dalgbl_s
               benthic_distribution_p(13+(i-1)*number_benth_distr),  & ! albewg_s
               benthic_distribution_p(14+(i-1)*number_benth_distr),  & ! albewk_s
               transfer_quantity_p(24+(i-1)*number_trans_quant),     & ! dalgag_s
               transfer_quantity_p(23+(i-1)*number_trans_quant),     & ! dalgak_s
               transfer_quantity_p(25+(i-1)*number_trans_quant),     & ! dalgab_s
               benthic_distribution_p(11+(i-1)*number_benth_distr),  & ! alberg_s
               benthic_distribution_p(12+(i-1)*number_benth_distr),  & ! alberk_s
               benthic_distribution_p(8+(i-1)*number_benth_distr),   & ! hJO2_s
               transfer_quantity_p(1+(i-1)*number_trans_quant),      & ! bsbt_s
               transfer_quantity_p(90 +(i-1)*number_trans_quant),    & ! dC_DenW_s
               TOC_CSB,                                              & ! TOC_CSB
               transfer_quantity_p(32+(i-1)*number_trans_quant),     & ! gO2n_s
               transfer_quantity_p(30+(i-1)*number_trans_quant),     & ! pO2p_s
               transfer_quantity_p(31+(i-1)*number_trans_quant),     & ! pO2r_s
               benthic_distribution_p(24+(i-1)*number_benth_distr),  & ! rO2dr_s
               transfer_quantity_p(44+(i-1)*number_trans_quant),     & ! rO2hnf_s
               strickler( zone(point_zone(iglob))%reib , tiefe(1)),  & ! rau_s
               tiefe(1),                                             & ! tiefe_s
               rhyd(1),                                              & ! rhyd   
               rb_hydraul_p(1+(i-1)*number_rb_hydraul),              & ! vmitt_s
               flae(1),                                              & ! flae   
               zone(point_zone(iglob))%wettstat%wetterstations_lage, & ! wlage_s
               rb_hydraul_p(3+(i-1)*number_rb_hydraul),              & ! hws_s
               wge_T(i2) ,                                           & ! wge_s
               planktonic_variable_p( 1+nk),                         & ! tempw_s
               iPhy,                                                 & ! iPhy
               tflie,                                                & ! tflie
               transfer_quantity_p(40+(i-1)*number_trans_quant),     & ! dalgo_s
               transfer_quantity_p(41+(i-1)*number_trans_quant),     & ! dalgao_s
               transfer_quantity_p(39+(i-1)*number_trans_quant),     & ! algo_s
               benthic_distribution_p(20+(i-1)*number_benth_distr),  & ! abeowg_s
               benthic_distribution_p(22+(i-1)*number_benth_distr),  & ! abeowk_s
               benthic_distribution_p(21+(i-1)*number_benth_distr),  & ! abeorg_s
               benthic_distribution_p(23+(i-1)*number_benth_distr),  & ! abeork_s
               transfer_quantity_p(43+(i-1)*number_trans_quant),     & ! zooro2_s
               benthic_distribution_p(16+(i-1)*number_benth_distr),  & ! hSchlr_s
               benthic_distribution_p(19+(i-1)*number_benth_distr),  & ! O2ein1_s
               control, iglob)
   
   if (control) then 
      print*, 'after oxygen():'
      print*, '  vo2 = ', planktonic_variable_p( 2+nk)
   endif
   
   
   ! --- check for nan ---
   do k = 1,number_plankt_vari
      if (isnan(planktonic_variable_p(k+nk))) then
         print '("before oxygen: isnan(planktonic_variable_p): node #", i0, ", variable #", i0)', iglob, k
         if (meinrang == 0) print*,'planktonic_variable_name: ', planktonic_variable_name(k)
      endif
   enddo
   
   do k = 1,number_trans_quant
      if (isnan(transfer_quantity_p(k+(i-1)*number_trans_quant))) then
         print '("before oxygen: isnan(transfer_quantity_p): node #", i0, ", variable #", i0)', iglob, k
         print '(a,i0)', '  meinrang = ',meinrang
         if (meinrang == 0) print*, 'trans_quant_name: ',trans_quant_name(k)
      endif
   enddo
   
   do k = 1,number_benth_distr
      if (isnan(benthic_distribution_p(k+(i-1)*number_benth_distr))) then
         print '("before oxygen: isnan(benthic_distribution_p): node #", i0, ", variable #", i0)', iglob, k
         if (meinrang == 0)print*,'benth_distr_name:',benth_distr_name(k)
      endif
   enddo
   
   return
end subroutine oxygen_wrapper_3d

