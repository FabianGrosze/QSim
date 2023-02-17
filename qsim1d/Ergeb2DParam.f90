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

!> Writes file `Ergeb2DParam.xml`
subroutine Ergeb2DParam(cpfad1)
   implicit none
   
   character(255), intent(in)  :: cpfad1
   
   character(275) :: pfadstring
   character(8)   :: versionstext
   
   external :: version_string
   
   call version_string(versionstext)
   
   pfadstring = trim(adjustl(cpfad1)) // 'Ergeb2DParam.xml'
   open(unit=1, file=pfadstring, encoding='UTF-8')
   
   write(1, '(A)') '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
   write(1, '(3A)') '<GerrisParam FileType="Ergeb2D" QsimVersion="',versionstext,'">'
   write(1, '(A)') '</GerrisParam>'
   
   close(1)
end subroutine Ergeb2DParam

