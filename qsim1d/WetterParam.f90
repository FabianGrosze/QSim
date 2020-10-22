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

 subroutine WetterParam(cpfad1,j1)

!  Ausgabe der Definition von Wetter

  character(255)  :: cpfad1
  character (len=275)         :: pfadstring 

  write(pfadstring,'(2A)')trim(adjustl(cpfad1(1:j1))),'WetterParam.xml' 
  open(unit=1, file=pfadstring, encoding='UTF-8')

  
  WRITE(1, '(A)') '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
  WRITE(1, '(A)') '<GerrisParam FileType="Wetter" QsimVersion="14.02">'  
  WRITE(1, '(A)') '<ParamSetDef Ident="Wetter" Text="Wetterdaten" Help="Parameter des Wetters">'
  WRITE(1, '(A)') '  <Parameter Ident="GStrahl" Text="Globalstrahlung" Help="Tagessumme der Globalstrahlung oder Globalstrahlungsintensität" Unit="J/cm² oder J/(cm²*h)" Format="F7.2" Null="-1" Default="" Min="" Max="" />'
  WRITE(1, '(A)') '  <Parameter Ident="MaxTemp" Text="Max.Lufttemp." Help="Maximale Lufttemperatur im Messzeitraum" Unit="°C" Format="F6.2" Null="-99.99" Default="" Min="" Max="" />'
  WRITE(1, '(A)') '  <Parameter Ident="MinTemp" Text="Min.Lufttemp." Help="Minimale Lufttemperatur im Messzeitraum" Unit="°C" Format="F6.2" Null="-99.99" Default="" Min="" Max="" />'
  WRITE(1, '(A)') '  <Parameter Ident="Feuchte" Text="Rel.Feuchte" Help="Relative Luftfeuchtigkeit" Unit="%" Format="F6.2" Null="-1" Default="" Min="" Max="" />'
  WRITE(1, '(A)') '  <Parameter Ident="Wind" Text="Windgeschw." Help="Windgeschwindigkeit" Unit="m/s" Format="F6.2" Null="-1" Default="" Min="" Max="" />'
  WRITE(1, '(A)') '  <Parameter Ident="Wdichte" Text="Bewölkungsdichte" Help="Bewölkungsdichte" Unit="" Format="F4.1" Null="-1" Default="" Min="" Max="" />'
  WRITE(1, '(A)') '  <Parameter Ident="Wtyp" Text="Wolkentyp" Help="Wolkentyp" Unit="" Format="F4.1" Null="-1" Default="" Min="" Max="" />'
  WRITE(1, '(A)') '</ParamSetDef>'
  WRITE(1, '(A)') '</GerrisParam>'
  
  CLOSE(1)

 END subroutine WetterParam
