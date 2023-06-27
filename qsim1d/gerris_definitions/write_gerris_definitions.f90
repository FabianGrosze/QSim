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
subroutine write_gerris_definitions(cpfad1)
   use module_aparam
   implicit none
   
   character(255), intent(in) :: cpfad1
   
   external :: write_ereigg_param, write_e_extnct_param, write_ereigh_param, write_ergeb2d_param
   external :: write_ergebm_param, write_ergebt_param, write_e_extnct, version_string
   external :: write_modellg_param, write_wetter_param
   
   print*,' Writing definitions:'
   
   print *, '  * AParamParam.xml'
   call write_aparamparam(cpfad1)
   
   print *, '  * EreigGParam.xml'
   call write_ereigg_param(cpfad1)
   
   print *, '  * ModellGParam.xml'
   call write_modellg_param(cpfad1)
   
   print *, '  * E_extnctParam.xml'
   call write_e_extnct_param(cpfad1)
   
   print *, '  * EreigHParam.xml'
   call write_ereigh_param(cpfad1)
   
   print *, '  * Ergeb2DParam.xml'
   call write_ergeb2d_param(cpfad1)
   
   print *, '  * ErgebMParam.xml'
   call write_ergebm_param(cpfad1)
   
   print *, '  * ErgebTParam.xml'
   call write_ergebt_param(cpfad1)
   
   print *, '  * WetterParam.xml'
   call write_wetter_param(cpfad1)
   
   print *, '  * e_extnct.dat'
   call write_e_extnct(cpfad1)
   
   print*, 'Definitions completed.'
end subroutine write_gerris_definitions