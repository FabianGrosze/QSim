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

subroutine silicate_wrapper_3d(i)
   use modell
   use QSimDatenfelder
   use aparam
   use module_silicate, only : silicate
   implicit none
   
   integer, intent(in) :: i
   integer             :: j, nk
   
   iglob = i + meinrang * part
   kontroll = iglob == kontrollknoten
   nk = (i-1) * number_plankt_vari
   
   ! Umrechnen des Zeitschritts in Tage
   tflie = real(deltat)/86400.
   
   if (kontroll) then
      print '("before silicate: (Knoten = ", I0, ")")', iglob 
      print*, "   si    = ", planktonic_variable_p( 7+nk)
      print*, "   hJSi  = ", benthic_distribution_p(46+(i-1)*number_benth_distr)
      print*, "   tiefe = ", rb_hydraul_p(2+(i-1)*number_rb_hydraul)
   endif
   
   
   call silicate(                                                                                  &
            planktonic_variable_p(7+nk),                                                           &! si
            benthic_distribution_p(46+(i-1)*number_benth_distr),                                   &! hJSi
            trans_quant_vert_p(1+( 4-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert), &! up_Si
            trans_quant_vert_p(1+(23-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert),&! akibr
            algakz(1,1),                                                                           &! algakz (from QSimDatenfelder)
            benthic_distribution_p(14+(i-1)*number_benth_distr),                                   &! albewk
            rb_hydraul_p(2+(i-1)*number_rb_hydraul),                                               &! tiefe
            tflie,                                                                                 &! tflie
            kontroll, iglob)
            
            
   if (kontroll) then 
      print*, "after silicate:"
      print*, "   si    = ", planktonic_variable_p( 7+nk)
      print*, "   hJSi  = ", benthic_distribution_p(46+(i-1)*number_benth_distr)
      print*, "   tiefe = ", rb_hydraul_p(2+(i-1)*number_rb_hydraul)
   endif
   
   if (isnan(planktonic_variable_p( 7+nk))) then
      write(fehler,'(A,I0)') 'isnan(si)) i = ',iglob
      call qerror(fehler)
   endif
   
   return
end subroutine silicate_wrapper_3d
