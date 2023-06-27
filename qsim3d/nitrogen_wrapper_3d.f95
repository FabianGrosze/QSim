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

subroutine nitrogen_wrapper_3d(i)
   use modell
   use isotope
   use QSimDatenfelder
   use module_nitrogen, only: nitrifiers, nitrogen
   use module_aparam
   implicit none
   
   integer, intent(in) :: i
   
   integer  :: j, nk
   
   control = iglob == kontrollknoten
   
   iglob = i + meinrang * part
   nk = (i-1) * number_plankt_vari
   
   ! Wassertiefe
   tiefe = rb_hydraul_p(2+(i-1)*number_rb_hydraul)
   
   ! hydraulischer Radius = tiefe | sinnvollste Annahme im mehrdimensionalen
   rhyd(1) = tiefe(1)
   
   ! Strickler Reibungsbeiwert
   rau(1) = strickler(zone(point_zone(iglob))%reib , tiefe(1))
   
   ! Umrechung Zeitschritt
   tflie = real(deltat)/86400.
   
   if (control) then
      print '(a,i0,a)', 'before nitrogen (iglob: ', iglob, ')'
      print*, '   vNH4 = ', planktonic_variable_p( 3+nk)
      print*, '   vNO2 = ', planktonic_variable_p( 4+nk)
      print*, '   vNO3 = ', planktonic_variable_p( 5+nk)
      print*, '   gesN = ', planktonic_variable_p(67+nk)
      print*, '   vx0  = ', planktonic_variable_p(15+nk)
      print*, '   vx02 = ', planktonic_variable_p(16+nk)
      print*, ''
   endif
   
   call nitrifiers(                                            & !
         planktonic_variable_p(15+nk),                         & ! vx0
         planktonic_variable_p(16+nk),                         & ! vx02
         benthic_distribution_p(3+(i-1)*number_benth_distr),   & ! pfl
         planktonic_variable_p(66+nk),                         & ! vph
         planktonic_variable_p( 1+nk),                         & ! tempw
         planktonic_variable_p( 2+nk),                         & ! vo2
         planktonic_variable_p( 3+nk),                         & ! vNH4
         planktonic_variable_p( 4+nk),                         & ! vNO2
         rhyd(1),                                              & ! rhyd
         rau(1),                                               & ! rau
         tiefe(1),                                             & ! tiefe
         rb_hydraul_p(1+(i-1)*number_rb_hydraul),              & ! vmitt
         benthic_distribution_p(36+(i-1)*number_benth_distr),  & ! hJNH4
         tflie,                                                & ! tflie
         transfer_quantity_p(29+(i-1)*number_trans_quant),     & ! susn
         transfer_quantity_p(97+(i-1)*number_trans_quant),     & ! susn2
         transfer_quantity_p(98+(i-1)*number_trans_quant),     & ! pfln1
         transfer_quantity_p(99+(i-1)*number_trans_quant),     & ! pfln2
         benthic_distribution_p(33+(i-1)*number_benth_distr),  & ! sedx0
         benthic_distribution_p(34+(i-1)*number_benth_distr),  & ! bettn
         transfer_quantity_p(32+(i-1)*number_trans_quant),     & ! go2n
         transfer_quantity_p(52+(i-1)*number_trans_quant),     & ! susno
         control, iglob)
   
   
   call nitrogen(                                             & !
         planktonic_variable_p( 3+nk),                        & ! vNH4
         planktonic_variable_p( 5+nk),                        & ! vNO3
         planktonic_variable_p( 4+nk),                        & ! vNO2
         planktonic_variable_p(67+nk),                        & ! gesN
         planktonic_variable_p( 2+nk),                        & ! vO2
         planktonic_variable_p(16+nk),                        & ! vx02
         planktonic_variable_p( 8+nk),                        & ! aki
         planktonic_variable_p( 9+nk),                        & ! agr
         planktonic_variable_p(10+nk),                        & ! abl
         planktonic_variable_p(30+nk),                        & ! Q_NK
         planktonic_variable_p(33+nk),                        & ! Q_NG
         planktonic_variable_p(35+nk),                        & ! Q_NB
         trans_quant_vert_p(1+( 1-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert), & ! up_NK
         trans_quant_vert_p(1+( 2-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert), & ! up_NG
         trans_quant_vert_p(1+( 3-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert), & ! up_NB
         trans_quant_vert_p(1+(23-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert), & ! akibr
         trans_quant_vert_p(1+(24-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert), & ! agrbr
         trans_quant_vert_p(1+(25-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert), & ! ablbr
         trans_quant_vert_p(1+(18-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert), & ! algak
         trans_quant_vert_p(1+(19-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert), & ! algag
         trans_quant_vert_p(1+(20-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert), & ! algab
         benthic_distribution_p(26+(i-1)*number_benth_distr), & ! sedalk
         benthic_distribution_p(27+(i-1)*number_benth_distr), & ! sedalg
         benthic_distribution_p(28+(i-1)*number_benth_distr), & ! sedalb
         algdrk(1),                                           & ! algdrk
         algdrg(1),                                           & ! algdrg
         algdrb(1),                                           & ! algdrb
         transfer_quantity_p(50+(i-1)*number_trans_quant),    & ! abltbr
         benthic_distribution_p(14+(i-1)*number_benth_distr), & ! albewk
         benthic_distribution_p(13+(i-1)*number_benth_distr), & ! albewg
         benthic_distribution_p(12+(i-1)*number_benth_distr), & ! alberk
         benthic_distribution_p(11+(i-1)*number_benth_distr), & ! alberg
         benthic_distribution_p(15+(i-1)*number_benth_distr), & ! resdr
         transfer_quantity_p(27+(i-1)*number_trans_quant),    & ! dzres1
         transfer_quantity_p(28+(i-1)*number_trans_quant),    & ! dzres2
         benthic_distribution_p(29+(i-1)*number_benth_distr), & ! exdrvk
         benthic_distribution_p(30+(i-1)*number_benth_distr), & ! exdrvg
         benthic_distribution_p(31+(i-1)*number_benth_distr), & ! exdrvb
         trans_quant_vert_p(1+(8-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert), & ! up_N2
         benthic_distribution_p(6+(i-1)*number_benth_distr),  & ! orgCsd
         planktonic_variable_p(57+nk),                        & ! nl0
         transfer_quantity_p(47+(i-1)*number_trans_quant),    & ! bsbct
         transfer_quantity_p(29+(i-1)*number_trans_quant),    & ! susn
         transfer_quantity_p(97+(i-1)*number_trans_quant),    & ! susn2
         transfer_quantity_p(98+(i-1)*number_trans_quant),    & ! pfln1
         transfer_quantity_p(99+(i-1)*number_trans_quant),    & ! pfln2
         transfer_quantity_p(3+(i-1)*number_trans_quant),     & ! don
         benthic_distribution_p(36+(i-1)*number_benth_distr), & ! hJNH4
         benthic_distribution_p(35+(i-1)*number_benth_distr), & ! hJNO3
         benthic_distribution_p(47+(i-1)*number_benth_distr), & ! hJN2
         tiefe(1),                                            & ! tflie
         tflie,                                               & ! tflie
         transfer_quantity_p(33+(i-1)*number_trans_quant),    & ! akiNH4
         transfer_quantity_p(34+(i-1)*number_trans_quant),    & ! agrNH4
         transfer_quantity_p(35+(i-1)*number_trans_quant),    & ! ablNH4
         transfer_quantity_p(36+(i-1)*number_trans_quant),    & ! akiNO3
         transfer_quantity_p(37+(i-1)*number_trans_quant),    & ! agrNO3
         transfer_quantity_p(38+(i-1)*number_trans_quant),    & ! ablNO3
         benthic_distribution_p(37+(i-1)*number_benth_distr), & ! hFluN3
         transfer_quantity_p(90 +(i-1)*number_trans_quant),   & ! dC_DenW
         control, iglob)
         
   if (control) then
      print '(a,i0,a)', 'after nitrogen (iglob: ', iglob, ')'
      print*, '   vNH4 = ', planktonic_variable_p( 3+nk)
      print*, '   vNO2 = ', planktonic_variable_p( 4+nk)
      print*, '   vNO3 = ', planktonic_variable_p( 5+nk)
      print*, '   gesN = ', planktonic_variable_p(67+nk)
      print*, '   vx0  = ', planktonic_variable_p(15+nk)
      print*, '   vx02 = ', planktonic_variable_p(16+nk)
      print*, ''
   endif
   
   return
end subroutine nitrogen_wrapper_3d