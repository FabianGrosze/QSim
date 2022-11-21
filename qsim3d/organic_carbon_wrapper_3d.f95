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

!> Hüllfunktion für den organischen Kohlenstoff
!! Beschreibung in \ref BSB 
!! ruft organic_carbon() auf
!! siehe auch: \ref lnk_huellen
!! Quelle: organic_carbon_wrapper_3d.f95
subroutine organic_carbon_wrapper_3d(i)
   use modell
   use aparam
   use QSimDatenfelder
   use module_metabolism, only: organic_carbon

   implicit none
   
   integer, intent(in)  :: i !< lokale Knotennummer auf dem jeweiligen Prozessor; läuft von 1 bis part
   real                 :: raus, chnfs, tiefes
   integer              :: k

   iglob = i + meinrang * part
   
   ! überstehende Nummern nicht bearbeiten
   if (iglob > knotenanzahl2D) return 
   
   kontroll = (iglob == kontrollknoten)
   
   ! Strickler Reibungsbeiwert
   tiefes = rb_hydraul_p(2+(i-1)*number_rb_hydraul)
   raus   = strickler(zone(point_zone(iglob))%reib, tiefes) 
   
   ! Umwandlung des Zeitschritts von [integer] Sekunden (QSim3D) in [real] Tage (QSim)
   tflie = real(deltat)/86400. 
   
   ! C-Masse der heterotrophen Nanoflagelaten
   CHNFs  = planktonic_variable_p(48+(i-1)*number_plankt_vari) 
   if (CHNFs <= 0.0) CHNFs = 0.0 ! CHNF=-1 meint keine HNF
   
   
   if (kontroll) then
      print*, 'before organic_carbon'
      print*, '   CHNFs = ', CHNFs
      print*, '   BAC   = ', planktonic_variable_p(42+(i-1)*number_plankt_vari)
      print*, '   obsb  = ', planktonic_variable_p(17+(i-1)*number_plankt_vari)
      print*, '   ocsb  = ', planktonic_variable_p(18+(i-1)*number_plankt_vari)
      print*, '   bsbt  = ', transfer_quantity_p(1+(i-1)*number_trans_quant)
      print*, '   vbsb  = ', planktonic_variable_p(46+(i-1)*number_plankt_vari)
      print*, ''
   endif
   
   
   call organic_carbon(                                          &
            planktonic_variable_p(18+(i-1)*number_plankt_vari) , & ! ocsb
            planktonic_variable_p(17+(i-1)*number_plankt_vari) , & ! obsb
            planktonic_variable_p(37+(i-1)*number_plankt_vari) , & ! CD1
            planktonic_variable_p(38+(i-1)*number_plankt_vari) , & ! CD2
            planktonic_variable_p(39+(i-1)*number_plankt_vari) , & ! CP1
            planktonic_variable_p(40+(i-1)*number_plankt_vari) , & ! CP2
            planktonic_variable_p(41+(i-1)*number_plankt_vari) , & ! CM
            planktonic_variable_p(42+(i-1)*number_plankt_vari) , & ! bac
            planktonic_variable_p(55+(i-1)*number_plankt_vari) , & ! fbsgr
            planktonic_variable_p(56+(i-1)*number_plankt_vari) , & ! frfgr
            planktonic_variable_p(57+(i-1)*number_plankt_vari) , & ! nl0
            planktonic_variable_p(58+(i-1)*number_plankt_vari) , & ! pl0
            chnfs                                              , & ! cHNF
            bvhnf(1)                                           , & ! bvHNF   (module QSimDatenfelder)
            planktonic_variable_p( 1+(i-1)*number_plankt_vari) , & ! tempw
            rb_hydraul_p(2+(i-1)*number_rb_hydraul)            , & ! tiefe
            benthic_distribution_p(3+(i-1)*number_benth_distr) , & ! pfl
            benthic_distribution_p(48+(i-1)*number_benth_distr), & ! jdoc1
            benthic_distribution_p(49+(i-1)*number_benth_distr), & ! jdoc2
            raus                                               , & ! rau
            rb_hydraul_p(1+(i-1)*number_rb_hydraul)            , & ! vmitt
            transfer_quantity_p(10+(i-1)*number_trans_quant)   , & ! bsbHNF
            transfer_quantity_p( 7+(i-1)*number_trans_quant)   , & ! dKiMor
            transfer_quantity_p( 8+(i-1)*number_trans_quant)   , & ! dGrMor
            transfer_quantity_p( 9+(i-1)*number_trans_quant)   , & ! dBlMor
            transfer_quantity_p( 6+(i-1)*number_trans_quant)   , & ! abszo
            planktonic_variable_p(31+(i-1)*number_plankt_vari) , & ! Q_PK
            planktonic_variable_p(34+(i-1)*number_plankt_vari) , & ! Q_PG
            planktonic_variable_p(36+(i-1)*number_plankt_vari) , & ! Q_PB
            planktonic_variable_p(30+(i-1)*number_plankt_vari) , & ! Q_NK
            planktonic_variable_p(33+(i-1)*number_plankt_vari) , & ! Q_NG
            planktonic_variable_p(35+(i-1)*number_plankt_vari) , & ! Q_NB
            transfer_quantity_p(16+(i-1)*number_trans_quant)   , & ! zexKi
            transfer_quantity_p(17+(i-1)*number_trans_quant)   , & ! zexGr
            transfer_quantity_p(18+(i-1)*number_trans_quant)   , & ! zexbl
            transfer_quantity_p(13+(i-1)*number_trans_quant)   , & ! drfaek
            transfer_quantity_p(14+(i-1)*number_trans_quant)   , & ! drfaeg
            transfer_quantity_p(15+(i-1)*number_trans_quant)   , & ! drfaeb
            benthic_distribution_p( 4+(i-1)*number_benth_distr), & ! ssdr
            transfer_quantity_p(11+(i-1)*number_trans_quant)   , & ! hnfbac
            transfer_quantity_p(91+(i-1)*number_trans_quant)   , & ! zBAC
            planktonic_variable_p(10+(i-1)*number_plankt_vari) , & ! abl
            planktonic_variable_p(9+(i-1)*number_plankt_vari)  , & ! agr
            planktonic_variable_p(8+(i-1)*number_plankt_vari)  , & ! aki
            planktonic_variable_p(50+(i-1)*number_plankt_vari) , & ! zooind
            bsbZoo                                             , & ! bsbZoo (in orgc_start hart codiert jetzt direkt aus QSimDatenfelder)
            toc_csb                                            , & ! toc_csb (aus QSimDatenfelder)
            tflie                                              , & ! tflie
            transfer_quantity_p(4+(i-1)*number_trans_quant)    , & ! BAcmua
            transfer_quantity_p(47+(i-1)*number_trans_quant)   , & ! bsbct
            transfer_quantity_p(2+(i-1)*number_trans_quant)    , & ! BSBctP
            transfer_quantity_p(3+(i-1)*number_trans_quant)    , & ! doN
            transfer_quantity_p(1+(i-1)*number_trans_quant)    , & ! bsbt
            benthic_distribution_p(7+(i-1)*number_benth_distr) , & ! BSBbet
            benthic_distribution_p(50+(i-1)*number_benth_distr), & ! orgCsd0
            benthic_distribution_p(6+(i-1)*number_benth_distr) , & ! orgCsd
            benthic_distribution_p(51+(i-1)*number_benth_distr), & ! orgCsd_abb
            transfer_quantity_p(19+(i-1)*number_trans_quant)   , & ! dorgSS
            planktonic_variable_p(46+(i-1)*number_plankt_vari) , & ! vBSB
            planktonic_variable_p(47+(i-1)*number_plankt_vari) , & ! vCSB
            kontroll                                           , & ! kontroll
            iglob)
   
   if (kontroll) then
      print*, 'after organic_carbon'
      print*, '   CHNFs = ', CHNFs
      print*, '   BAC   = ', planktonic_variable_p(42+(i-1)*number_plankt_vari)
      print*, '   obsb  = ', planktonic_variable_p(17+(i-1)*number_plankt_vari)
      print*, '   ocsb  = ', planktonic_variable_p(18+(i-1)*number_plankt_vari)
      print*, '   bsbt  = ', transfer_quantity_p(1+(i-1)*number_trans_quant)
      print*, '   vbsb  = ', planktonic_variable_p(46+(i-1)*number_plankt_vari)
      print*, ''
   endif
   
   
   do k = 1,number_trans_quant
      if (isnan(transfer_quantity_p(k+(i-1)*number_trans_quant))) then
         print*,'Error in organic_carbon_wrapper_3d: isnan(transfer_quantity_p)'
         print '(*(a,i0,2x))', 'node: ',     iglob,  &
                                'variable: ', k,      &
                                'meinrang: ', meinrang
         print*,'trans_quant_name:', trans_quant_name(k)
         call qerror("organic_carbon_wrapper_3d(): isnan(transfer_quantity_p)")
      endif
   end do
    
   return

end subroutine organic_carbon_wrapper_3d
