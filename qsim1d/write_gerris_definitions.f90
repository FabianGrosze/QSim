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
   use aparam, only: AParamParam

   implicit none
   
   character(255) :: cpfad1
   
   print*,' Writing definitions:'
   !write(cpfad1,*)'.'
   
   print *, '  * AParamParam.xml'
   call AParamParam(cpfad1)
   
   print *, '  * EreigGParam.xml'
   call EreigGParam(cpfad1)
   
   print *, '  * ModellGParam.xml'
   call write_modellg_param(cpfad1)
   
   print *, '  * E_extnctParam.xml'
   call E_extnctParam(cpfad1)
   
   print *, '  * EreigHParam.xml'
   call EreigHParam(cpfad1)
   
   print *, '  * Ergeb2DParam.xml'
   call Ergeb2DParam(cpfad1)
   
   print *, '  * ErgebMParam.xml'
   call ErgebMParam(cpfad1)
   
   print *, '  * ErgebTParam.xml'
   call ErgebTParam(cpfad1)
   
   print *, '  * WetterParam.xml'
   call WetterParam(cpfad1)
   
   print *, '  * e_extnct.dat'
   call write_e_extnct(cpfad1)
   
   print*, 'Definitions completed.'
end subroutine write_gerris_definitions