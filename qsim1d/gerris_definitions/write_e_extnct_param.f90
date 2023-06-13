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

!> Write file e_extnctParam.xml
subroutine write_e_extnct_param(cpfad1)
   implicit none
   
   character(255), intent(in)  :: cpfad1
   
   character(275) :: pfadstring
   character(8)   :: versionstext
   integer        :: u_extn
   
   external :: version_string
   
   call version_string(versionstext)
   
   pfadstring =  trim(adjustl(cpfad1)) // 'E_extnctParam.xml' 
   open(newunit = u_extn, file=pfadstring, encoding='UTF-8')
   
   write(u_extn, '(A)') '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
   write(u_extn, '(3A)') '<GerrisParam FileType="E_extnct" QsimVersion="',versionstext,'">' 
   
   write(u_extn, '(A)') '<ParamSetDef Ident="E_extnct" Text="Absorption" Help="Wellenlängenabhängige Absorptionskoeffizienten verschiedener Stoffe">'
   write(u_extn, '(A)') '  <Parameter Ident="Lambda" Text="Wellenlänge" Help="Wellenlänge" Unit="nm" Format="F5.1" Null="-1" Default="" Min="" Max="" />'
   write(u_extn, '(A)') '  <Parameter Ident="Wasser" Text="Wasser" Help="Absorptionskoeffizienten von Wasser" Unit="" Format="F8.6" Null="-1" Default="" Min="" Max="" />'
   write(u_extn, '(A)') '  <Parameter Ident="KAlg" Text="Kieselalgen" Help="Absorptionskoeffizienten von Kieselalgen" Unit="" Format="F8.6" Null="-1" Default="" Min="" Max="" />'
   write(u_extn, '(A)') '  <Parameter Ident="GAlg" Text="Grünalgen" Help="Absorptionskoeffizienten von Grünalgen" Unit="" Format="F8.6" Null="-1" Default="" Min="" Max="" />'
   write(u_extn, '(A)') '  <Parameter Ident="BAlg" Text="Blaualgen" Help="Absorptionskoeffizienten von Blaualgen" Unit="" Format="F8.6" Null="-1" Default="" Min="" Max="" />'
   write(u_extn, '(A)') '  <Parameter Ident="Humin" Text="Humin" Help="Absorptionskoeffizienten von Humin" Unit="" Format="F8.6" Null="-1" Default="" Min="" Max="" />'
   write(u_extn, '(A)') '  <Parameter Ident="Schweb" Text="Susp. Schwebstoff" Help="Absorptionskoeffizienten von suspendiertem Schwebstoff" Unit="" Format="F8.6" Null="-1" Default="" Min="" Max="" />'
   write(u_extn, '(A)') '  <Parameter Ident="Sonne" Text="Sonnenlicht" Help="Absorptionskoeffizienten von Sonnenlicht" Unit="" Format="F8.6" Null="-1" Default="" Min="" Max="" />'
   write(u_extn, '(A)') '</ParamSetDef>'
   
   write(u_extn, '(A)') '</GerrisParam>'
   
   close(u_extn)
end subroutine write_e_extnct_param