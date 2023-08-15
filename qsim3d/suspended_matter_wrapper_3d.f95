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
!> Subroutine suspended_matter_wrapper_3d() simulaties suspended Matters
subroutine suspended_matter_wrapper_3d(i)
   use modell
   use qsimdatenfelder
   use module_aparam
   use module_metabolism
   implicit none
   
   integer, intent(in) :: i
   integer :: npla, ntra
   real    :: tiefe_s, rau_s
   
   iglob = i + meinrang * part 
   control = iglob == kontrollknoten

   npla = (i-1)*number_plankt_vari
   ntra = (i-1)*number_trans_quant
   tflie = real(deltat) / 86400.
   tiefe_s = rb_hydraul_p(2+(i-1)*number_rb_hydraul)
   tiefe_s = max(min_tief, tiefe_s)
   
   rau_s = strickler(zone(point_zone(iglob))%reib, tiefe_s)   

   call suspended_matter(                                                       &
               ss_s       = planktonic_variable_p(52+npla),                     &
               ssdr_s     = benthic_distribution_p(4+(i-1)*number_benth_distr), & 
               fssgr_s    = planktonic_variable_p(54+npla),                     &
               aki_s      = planktonic_variable_p(8+npla),                      &
               agr_s      = planktonic_variable_p(9+npla),                      &
               abl_s      = planktonic_variable_p(10+npla),                     &
               zooind_s   = planktonic_variable_p(50+npla),                     &
               tiefe_s    = tiefe_s,                                            &
               rau_s      = rau_s,                                              &
               vmitt_s    = rb_hydraul_p(1+(i-1)*number_rb_hydraul),            &
               tausc_s    = tauscs,                                             &
               dorgss_s   = transfer_quantity_p(19+ntra),                       &
               dkimor_s   = transfer_quantity_p(7+ntra),                        &
               dgrmor_s   = transfer_quantity_p(8+ntra),                        &      
               dblmor_s   = transfer_quantity_p(9+ntra),                        &  
               drfaek_s   = transfer_quantity_p(13+ntra),                       & 
               drfaeg_s   = transfer_quantity_p(14+ntra),                       & 
               drfaeb_s   = transfer_quantity_p(15+ntra),                       &  
               drfaes_s   = transfer_quantity_p(95+ntra),                       & 
               zexki_s    = transfer_quantity_p(16+ntra),                       &
               zexgr_s    = transfer_quantity_p(17+ntra),                       &  
               zexbl_s    = transfer_quantity_p(18+ntra),                       &
               abszo_s    = transfer_quantity_p(6+ntra),                        &
               ischif_s   = zone(point_zone(iglob))%schiff%schifffahrts_zone,   &
               ieros      = ieros,                                              & 
               tflie      = tflie,                                              &
               ssalg_s    = planktonic_variable_p(53+npla),                     & 
               sedss_s    = transfer_quantity_p(5+ntra),                        &  
               sedss_mq_s = sedss_mq(1,1),                                      & 
               control    = control,                                            &
               jjj        = iglob)

end subroutine suspended_matter_wrapper_3d
