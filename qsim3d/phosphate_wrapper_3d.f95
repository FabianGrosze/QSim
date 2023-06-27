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
subroutine phosphate_wrapper_3d(i)
   use modell
   use QSimDatenfelder
   use module_metabolism, only: phosphate
   use module_aparam
   implicit none
   
   integer, intent(in) :: i
   integer             :: k, nk
   
   iglob =  i + meinrang*part
   control = iglob == kontrollknoten
   nk = (i-1)*number_plankt_vari
   
   ! Umwandlung des Zeitschritts von integer sekunden (T-QSim) in real Tage (QSim)
   tflie = real(deltat)/86400 
   
   if (control) then
      print*, 'before phosphate'
      print*, '   gesp = ', planktonic_variable_p(68+nk)
      print*, '   gelp = ', planktonic_variable_p( 6+nk)
      print*, '   Q_PK = ', planktonic_variable_p(31+nk)
      print*, ''
   endif
   
   call phosphate(                                          & !
      planktonic_variable_p( 6+nk),                         & ! gelP_s
      planktonic_variable_p(68+nk),                         & ! gesP_s
      transfer_quantity_p(2+(i-1)*number_trans_quant),      & ! bsbctP_s
      planktonic_variable_p(8+(i-1)*number_plankt_vari),    & ! aki_s
      planktonic_variable_p(9+(i-1)*number_plankt_vari),    & ! agr_s
      planktonic_variable_p(10+nk),                         & ! abl_s
      transfer_quantity_p(27+(i-1)*number_trans_quant),     & ! dzres1_s
      transfer_quantity_p(28+(i-1)*number_trans_quant),     & ! dzres2_s
      planktonic_variable_p(31+nk),                         & ! Q_PK_s
      planktonic_variable_p(34+(i-1)*number_plankt_vari),   & ! Q_PG_s
      planktonic_variable_p(36+nk),                         & ! Q_PB_s
      benthic_distribution_p(15+(i-1)*number_benth_distr),  & ! resdr_s
      benthic_distribution_p(29+(i-1)*number_benth_distr),  & ! exdrvk_s
      benthic_distribution_p(30+(i-1)*number_benth_distr),  & ! exdrvg_s
      benthic_distribution_p(31+(i-1)*number_benth_distr),  & ! exdrvb_s
      trans_quant_vert_p(1+(6-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert),  & ! up_PG_s
      trans_quant_vert_p(1+(5-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert),  & ! up_PK_s
      trans_quant_vert_p(1+(7-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert),  & ! up_PB_s
      trans_quant_vert_p(1+(24-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert), & ! agrbr_s
      trans_quant_vert_p(1+(23-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert), & ! akibr_s
      trans_quant_vert_p(1+(25-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert), & ! ablbr_s
      trans_quant_vert_p(1+(19-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert), & ! algag_s
      trans_quant_vert_p(1+(18-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert), & ! algak_s
      trans_quant_vert_p(1+(20-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert), & ! algab_s
      benthic_distribution_p(13+(i-1)*number_benth_distr),  & ! albewg_s
      benthic_distribution_p(11+(i-1)*number_benth_distr),  & ! alberg_s
      benthic_distribution_p(14+(i-1)*number_benth_distr),  & ! albewk_s
      benthic_distribution_p(12+(i-1)*number_benth_distr),  & ! alberk_s
      rb_hydraul_p(2+(i-1)*number_rb_hydraul),              & ! tiefe_s
      benthic_distribution_p(32+(i-1)*number_benth_distr),  & ! hJPO4_s
      benthic_distribution_p(6+(i-1)*number_benth_distr),   & ! orgCsd_s
      planktonic_variable_p(58+nk),                         & ! pl0_s
      benthic_distribution_p(26+(i-1)*number_benth_distr),  & ! sedalk_s
      benthic_distribution_p(28+(i-1)*number_benth_distr),  & ! sedalb_s
      benthic_distribution_p(27+(i-1)*number_benth_distr),  & ! sedalg_s
      benthic_distribution_p(38+(i-1)*number_benth_distr),  & ! algdrk_s
      benthic_distribution_p(41+(i-1)*number_benth_distr),  & ! algdrb_s
      benthic_distribution_p(40+(i-1)*number_benth_distr),  & ! algdrg_s
      tflie,                                                & ! tflie
      control, iglob)
   
   
   if (control) then
      print*, 'after phosphate'
      print*, '   gesp = ', planktonic_variable_p(68+nk)
      print*, '   gelp = ', planktonic_variable_p( 6+nk)
      print*, '   Q_PK = ', planktonic_variable_p(31+nk)
      print*, ''
   endif
   
   do k = 1,number_trans_quant
      if (isnan(planktonic_variable_p(k+nk))) then
         print*,'Error in phosphate_wrapper_3d: isnan(transfer_quantity_p)'
         print '(*(a,i0,2x))', 'node: ',     iglob,  &
                               'variable: ', k,      &
                               'meinrang: ', meinrang
         print*,'trans_quant_name:', trans_quant_name(k)
         call qerror("phosphate_wrapper_3d(): isnan(transfer_quantity_p)")
      endif
   enddo
   
   return
end subroutine phosphate_wrapper_3d
