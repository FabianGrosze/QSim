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
   
   character (len=275)         :: pfadstring
   character (len = 8)         :: versionstext
   
   call version_string(versionstext)
   
   pfadstring = trim(adjustl(cpfad1)) // 'Ergeb2DParam.xml'
   open(unit=1, file=pfadstring, encoding='UTF-8')
   
   write(1, '(A)') '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
   write(1, '(3A)') '<GerrisParam FileType="Ergeb2D" QsimVersion="',versionstext,'">'  
   write(1, '(A)') '<ParamSetDef Ident="Ergeb2D" Text="2D-Zeitschritt-Ergebnisparameter" Help="Die von QSim in 2D- und Zeitschritt-Auflösung berechneten Parameter">'
   write(1, '(A)') '  <Parameter Ident="VNH4" Text="NH4-N" Unit="mg/l" Format="F6.2" Help="..." Group="1" Quantity="NH4" />'
   write(1, '(A)') '  <Parameter Ident="VNO2" Text="NO2-N" Unit="mg/l" Format="F5.3" Help="..." Group="1" Quantity="NO2" />'
   write(1, '(A)') '  <Parameter Ident="VNO3" Text="NO3-N" Unit="mg/l" Format="F9.6" Help="NO3-N" Group="1" Quantity="NO3" />'
   write(1, '(A)') '  <Parameter Ident="GELP" Text="o-PO4-P" Unit="mg/l" Format="F5.3" Help="o-PO4-P" Group="1" Quantity="GELP" />'
   write(1, '(A)') '  <Parameter Ident="SI" Text="Gelöstes Si" Unit="mg/l" Format="F5.2" Help="Gelöstes Si" Group="1" Quantity="SI" />'
   write(1, '(A)') '  <Parameter Ident="TEMPW" Text="Wassertemperatur" Unit="°C" Format="F5.2" Help="Wassertemperatur" Group="1" Quantity="TEMPW" />'
   write(1, '(A)') '  <Parameter Ident="VO2" Text="Sauerstoffgehalt" Unit="mg/l" Format="F5.2" Help="Sauerstoffgehalt" Group="1" Quantity="O2" />'
   write(1, '(A)') '  <Parameter Ident="CHLA" Text="Chlorophyll" Unit="µg/l" Format="F6.2" Help="Chlorophyll-a-Gehalt" Group="1" Quantity="CHLA" />'
   write(1, '(A)') '  <Parameter Ident="CCHLKI" Text="C:Chlorophyll-a der Kieselalgen" Unit="mg/mg" Format="F6.2" Help="C:Chlorophyll" Group="1" Quantity="CHLA" />'
   write(1, '(A)') '  <Parameter Ident="CCHLBL" Text="C:Chlorophyll-a der Blaualgen" Unit="mg/mg" Format="F6.2" Help="C:Chlorophyll" Group="1" Quantity="CHLA" />'
   write(1, '(A)') '  <Parameter Ident="CCHLGR" Text="C:Chlorophyll-a der Grünalgen" Unit="mg/mg" Format="F6.2" Help="C:Chlorophyll" Group="1" Quantity="CHLA" />'
   write(1, '(A)') '  <Parameter Ident="gesP" Text="gesamt Phosphor" Unit="mg/l" Format="F6.3" Help="gesamt Phosphor" Group="1" Quantity="gesP" />'
   write(1, '(A)') '  <Parameter Ident="gesN" Text="gesamt Stickstoff" Unit="mg/l" Format="F6.3" Help="gesamt Stickstoff" Group="1" Quantity="gesN" />'
   write(1, '(A)') '</ParamSetDef>'
   write(1, '(A)') '</GerrisParam>'
   
   close(1)
end subroutine Ergeb2DParam

