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
!> Subroutine schweb_huelle() simulaties suspended Matters
!! \n\n
!! Quelle: schweb_huelle.f95
subroutine schweb_huelle(i)
   use modell
   use QSimDatenfelder
   use aparam
   implicit none
   integer :: i,npla,ntra
   real tiefes,flaes,raus
   ! real sedSS_MQ ! Sedimentation, die auftreten würde ohne Erosion
   iglob = (i+meinrang*part) ! i ist die lokale Knotennummer auf dem jeweiligen Prozessor und läuft von 1 bis part
   kontroll = iglob == kontrollknoten
   npla = (i-1)*number_plankt_vari ! Ort im Feld der transportierten planktischen Variablen
   ntra = (i-1)*number_trans_quant
   tflie = real(deltat)/86400
   tiefes = rb_hydraul_p(2+(i-1)*number_rb_hydraul)
   if (tiefes <= min_tief)tiefes = min_tief ! minimale Wassertiefe erhalten
   raus = strickler( zone(point_zone(iglob))%reib , tiefes )
   ! ischif zone(point_zone(iglob))%schiff%schifffahrts_zone schifffahrt in dieser module::zonen ; 1->Schiffsverkehr  , 0-> kein Schiffsverkehr; MODELLG.txt "F"
   !  SUBROUTINE SCHWEB_kern(zooinds,dorgSSs,sss,ssalgs,tiefes,raus,tflie,VMITTs
   !                ,dkimors,dgrmors,abszos,zexkis,zexgrs,abls,zexbls,dblmor,drfaebs,akis,agrs,ssdrs,drfaeks      &
   !             ,drfaegs,drfaess,fssgrs,sedsss,sedSS_MQs,tauscs,ischifs,ieros,kontroll ,jjj )
   call SCHWEB_kern (                       &
                     planktonic_variable_p(50+npla)          &
                     ,transfer_quantity_p(19+ntra)            &
                     ,planktonic_variable_p(52+npla)          &
                     ,planktonic_variable_p(53+npla)          &
                     ,tiefes                                  &
                     ,raus                                    &
                     ,tflie                                   &
                     ,rb_hydraul_p(1+(i-1)*number_rb_hydraul) &
                     ,transfer_quantity_p(7+ntra)             &
                     ,transfer_quantity_p(8+ntra)             &
                     ,transfer_quantity_p(6+ntra)             &
                     ,transfer_quantity_p(16+ntra)            &
                     ,transfer_quantity_p(17+ntra)            &
                     ,planktonic_variable_p(10+npla)          &
                     ,transfer_quantity_p(18+ntra)            &
                     ,transfer_quantity_p(9+ntra)             &
                     ,transfer_quantity_p(15+ntra)            &
                     ,planktonic_variable_p(8+npla)           &
                     ,planktonic_variable_p(9+npla)           &
                     ,benthic_distribution_p(4+(i-1)*number_benth_distr) &
                     ,transfer_quantity_p(13+ntra)            &
                     ,transfer_quantity_p(14+ntra)            &
                     ,transfer_quantity_p(95+ntra)            &
                     ,planktonic_variable_p(54+npla)          &
                     ,transfer_quantity_p(5+ntra)             &
                     ,sedSS_MQ                                &
                     ,tauscs                                  &
                     ,zone(point_zone(iglob))%schiff%schifffahrts_zone &
                     ,ieros                                   &
                     ,kontroll                                &
                     ,iglob ) !!wy
   return
end subroutine schweb_huelle
