!---------------------------------------------------------------------------------------
!
!   QSim - Programm zur Simulation der Wasserqualität
!
!   Copyright (C) 2020 Bundesanstalt für Gewässerkunde, Koblenz, Deutschland, http://www.bafg.de
!
!   Dieses Programm ist freie Software. Sie können es unter den Bedingungen der 
!   GNU General Public License, Version 3,
!   wie von der Free Software Foundation veröffentlicht, weitergeben und/oder modifizieren. 
!   Die Veröffentlichung dieses Programms erfolgt in der Hoffnung, daß es Ihnen von Nutzen sein wird, 
!   aber OHNE IRGENDEINE GARANTIE, sogar ohne die implizite Garantie der MARKTREIFE oder der VERWENDBARKEIT FÜR EINEN BESTIMMTEN ZWECK. 
!   Details finden Sie in der GNU General Public License.
!   Sie sollten ein Exemplar der GNU General Public License zusammen mit diesem Programm erhalten haben. 
!   Falls nicht, siehe http://www.gnu.org/licenses/.  
!   
!	Programmiert von:
!	1979 bis 2018 Volker Kirchesch
!	seit 2011 Jens Wyrwa, Wyrwa@bafg.de
!
!---------------------------------------------------------------------------------------

 subroutine E_extnctParam(cpfad1,j1)

!  Ausgabe der Definition von E_extnct

  character(255)  :: cpfad1
  character (len=275)         :: pfadstring

  write(pfadstring,'(2A)')trim(adjustl(cpfad1(1:j1))),'E_extnctParam.xml' 
  open(unit=1, file=pfadstring, encoding='UTF-8')

  WRITE(1, '(A)') '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
  WRITE(1, '(A)') '<GerrisParam FileType="E_extnct" QsimVersion="14.02">'  
  WRITE(1, '(A)') '<ParamSetDef Ident="E_extnct" Text="Absorption" Help="Wellenlängenabhängige Absorptionskoeffizienten verschiedener Stoffe">'
  WRITE(1, '(A)') '  <Parameter Ident="Lambda" Text="Wellenlänge" Help="Wellenlänge" Unit="nm" Format="F5.1" Null="-1" Default="" Min="" Max="" />'
  WRITE(1, '(A)') '  <Parameter Ident="Wasser" Text="Wasser" Help="Absorptionskoeffizienten von Wasser" Unit="" Format="F8.6" Null="-1" Default="" Min="" Max="" />'
  WRITE(1, '(A)') '  <Parameter Ident="KAlg" Text="Kieselalgen" Help="Absorptionskoeffizienten von Kieselalgen" Unit="" Format="F8.6" Null="-1" Default="" Min="" Max="" />'
  WRITE(1, '(A)') '  <Parameter Ident="GAlg" Text="Grünalgen" Help="Absorptionskoeffizienten von Grünalgen" Unit="" Format="F8.6" Null="-1" Default="" Min="" Max="" />'
  WRITE(1, '(A)') '  <Parameter Ident="BAlg" Text="Blaualgen" Help="Absorptionskoeffizienten von Blaualgen" Unit="" Format="F8.6" Null="-1" Default="" Min="" Max="" />'
  WRITE(1, '(A)') '  <Parameter Ident="Humin" Text="Humin" Help="Absorptionskoeffizienten von Humin" Unit="" Format="F8.6" Null="-1" Default="" Min="" Max="" />'
  WRITE(1, '(A)') '  <Parameter Ident="Schweb" Text="Susp. Schwebstoff" Help="Absorptionskoeffizienten von suspendiertem Schwebstoff" Unit="" Format="F8.6" Null="-1" Default="" Min="" Max="" />'
  WRITE(1, '(A)') '  <Parameter Ident="Sonne" Text="Sonnenlicht" Help="Absorptionskoeffizienten von Sonnenlicht" Unit="" Format="F8.6" Null="-1" Default="" Min="" Max="" />'
  WRITE(1, '(A)') '</ParamSetDef>'
  WRITE(1, '(A)') '</GerrisParam>'
  
  CLOSE(1)

 END subroutine E_extnctParam
