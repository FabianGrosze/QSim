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

subroutine EreigGParam(cpfad1,j1)  

  character(255) :: cpfad1
  character (len=275)         :: pfadstring

  write(pfadstring,'(2A)')trim(adjustl(cpfad1(1:j1))),'EreigGParam.xml'
  open(unit=1, file=pfadstring, encoding='UTF-8')
  
  WRITE(1, '(A)') '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
  WRITE(1, '(A)') '<GerrisParam FileType="EreigG" QsimVersion="14.06">'  
  WRITE(1, '(A)') '<ParamSetDef Ident="EreigG" Text="Randbedingungs-Parameter" Help="Parameter der Randbedingungen">'
  WRITE(1, '(A)') '  <Parameter Ident="OBSB" Text="C-BSB5" IsLoad="0" Unit="mg/l" Format="F6.2" Null="-1" Help="C-BSB5" Min="0" Max="999.99" Default="" Module="Alle" />'
  WRITE(1, '(A)') '  <Parameter Ident="OCSB" Text="CSB" IsLoad="0" Unit="mg/l" Format="F6.2" Null="-1" Help="CSB" Min="0" Max="999.99" Default="" Module="Alle" />'
  WRITE(1, '(A)') '  <Parameter Ident="VNH4" Text="NH4-N" IsLoad="1" Unit="mg/l" Format="F6.2" Null="-1" Help="NH4-N Gehalt" Min="0" Max="999.99" Default="" Module="Alle" />'
  WRITE(1, '(A)') '  <Parameter Ident="VNO2" Text="Nitrit-N" IsLoad="1" Unit="mg/l" Format="F6.3" Null="-1" Help="Nitrit-Stickstoffgehalt" Min="0" Max="99.999" Default="" Module="Alle" />'
  WRITE(1, '(A)') '  <Parameter Ident="VNO3" Text="Nitrat-N" IsLoad="1" Unit="mg/l" Format="F5.2" Null="-1" Help="Nitrat-Stickstoffgehalt" Min="0" Max="99.99" Default="" Module="Alle" />'
  WRITE(1, '(A)') '  <Parameter Ident="GESN" Text="Gesamt-N" IsLoad="1" Unit="mg/l" Format="F5.2" Null="-1" Help="Gesamt-Stickstoff" Min="0" Max="99.99" Default="" Module="Alle" />'
  WRITE(1, '(A)') '  <Parameter Ident="VX0" Text="Nitrosomonas" IsLoad="0" Unit="mg/l" Format="F8.5" Null="-1" Help="suspendierte Nitrosomonasbiomasse" Min="0" Max="99.99999" Default="" Module="Alle" />'
  WRITE(1, '(A)') '  <Parameter Ident="VX02" Text="Nitrobacter" IsLoad="0" Unit="mg/l" Format="F8.5" Null="-1" Help="suspendierte Nitrobacterbiomasse" Min="0" Max="99.99999" Default="" Module="Alle" />'
  WRITE(1, '(A)') '  <Parameter Ident="GELP" Text="Gelöster Phosphor" IsLoad="1" Unit="mg/l" Format="F6.3" Null="-1" Help="gelöster Phosphorgehalt" Min="0" Max="99.999" Default="" Module="Alle" />'
  WRITE(1, '(A)') '  <Parameter Ident="GESP" Text="Gesamt-Phosphor" IsLoad="1" Unit="mg/l" Format="F5.2" Null="-1" Help="Gesamt-Phosphor" Min="0" Max="99.99" Default="" Module="Alle" />'
  WRITE(1, '(A)') '  <Parameter Ident="SI" Text="Silizium" IsLoad="0" Unit="mg/l" Format="F5.2" Null="-1" Help="gelöster Siliziumgehalt" Min="0" Max="99.99" Default="" Module="Alle" />'
  WRITE(1, '(A)') '  <Parameter Ident="CHLA" Text="Chlorophyll-a" IsLoad="0" Unit="µg/l" Format="F6.2" Null="-1" Help="Chlorophyll-a-Gehalt" Min="0" Max="999.99" Default="" Module="Alle" />'
  WRITE(1, '(A)') '  <Parameter Ident="VKIGR" Text="Kieselalgen" IsLoad="0" Unit="0-1" Format="F5.2" Null="-1" Help="Anteil der Kieselalgen am Gesamt-Chlorophyll-a" Min="0" Max="99.99" Default="" Module="Alle" />'
  WRITE(1, '(A)') '  <Parameter Ident="ANTBL" Text="Blaualgen" IsLoad="0" Unit="0-1" Format="F5.2" Null="-1" Help="Anteil der Blaualgen am Gesamt-Chlorophyll-a" Min="0" Max="99.99" Default="" Module="Alle" />'
  WRITE(1, '(A)') '  <Parameter Ident="ZOOIND" Text="Rotatorien" IsLoad="0" Unit="Ind/l" Format="F7.1" Null="-1" Help="Rotatoriendichte" Min="0" Max="99999.9" Default="" Module="Alle" />'
  WRITE(1, '(A)') '  <Parameter Ident="VPH" Text="pH-Wert" IsLoad="0" Unit="" Format="F5.2" Null="-1" Help="pH-Wert" Min="0" Max="99.99" Default="" Module="Alle" />'
  WRITE(1, '(A)') '  <Parameter Ident="MW" Text="m-Wert" IsLoad="0" Unit="mmol/l" Format="F5.2" Null="-1" Help="m-Wert" Min="0" Max="99.99" Default="" Module="Alle" />'
  WRITE(1, '(A)') '  <Parameter Ident="CA" Text="Calcium" IsLoad="0" Unit="mg/l" Format="F5.1" Null="-1" Help="Calciumkonzentration" Min="0" Max="999.9" Default="" Module="Alle" />'
  WRITE(1, '(A)') '  <Parameter Ident="LF" Text="Leitfähigkeit" IsLoad="0" Unit="µS/cm" Format="F8.1" Null="-1" Help="Leitfähigkeit" Min="0" Max="999999.9" Default="" Module="Alle" />'
  WRITE(1, '(A)') '  <Parameter Ident="SS" Text="Schwebstoff" IsLoad="0" Unit="mg/l" Format="F7.2" Null="-1" Help="Schwebstoffgehalt" Min="0" Max="9999.99" Default="" Module="Alle" />'
  WRITE(1, '(A)') '  <Parameter Ident="TEMPW" Text="Wassertemperatur" IsLoad="0" Unit="°C" Format="F5.2" Null="-9.99" Help="Wassertemperatur" Min="-9.98" Max="99.99" Default="" Module="Alle,Temperatur,Coli" />'
  WRITE(1, '(A)') '  <Parameter Ident="VO2" Text="Sauerstoff" IsLoad="0" Unit="mg/l" Format="F5.2" Null="-1" Help="Sauerstoffgehalt" Min="0" Max="99.99" Default="" Module="Alle" />'
  WRITE(1, '(A)') '  <Parameter Ident="CHNF" Text="Nanoflagellaten" IsLoad="0" Unit="Ind/ml" Format="F8.1" Null="-1" Help="Heterotrophe Nanoflagellaten" Min="0" Max="999999.9" Default="" Module="Alle" />'
  WRITE(1, '(A)') '  <Parameter Ident="BVHNF" Text="HNF Biovolumen" IsLoad="0" Unit="µm³" Format="F6.1" Null="-1" Help="Biovolumen der HNF" Min="0" Max="9999.9" Default="" Module="Alle" />'
  WRITE(1, '(A)') '  <Parameter Ident="COLI" Text="Coliforme" IsLoad="0" Unit="Ind/100 ml" Format="E9.2" Null="-1.0" Help="Fäkalcoliforme Bakterien" Min="0.0" Max="999999999999999.9" Default="" Module="Alle,Temperatur,Coli" />'
  WRITE(1, '(A)') '  <Parameter Ident="EWAERM" Text="Wärme" IsLoad="0" Unit="MJ/s" Format="F7.1" Null="-9999.9" Help="Menge der eingeleiteten Wärme (Kraftwerk)" Min="-9999.8" Max="99999.9" Default="" Module="Alle,Temperatur" />'
  WRITE(1, '(A)') '  <Parameter Ident="TRACER" Text="Tracermenge" IsLoad="0" Unit="µg/l" Format="F9.3" Null="-1." Help="Tracerkonzentration" Min="0" Max="99999.999" Default="" Module="Tracer" />'
  WRITE(1, '(A)') '  <Parameter Ident="KONSS" Text="konserv. Substanz (Cl etc." IsLoad="0" Unit="mg/l" Format="F7.1" Null="-1" Help="Tracerkonzentration" Min="0" Max="99999.9" Default="" Module="KonsS" />'
  WRITE(1, '(A)') '  <Parameter Ident="gsZn" Text="Zn-ges." IsLoad="0" Unit="µg/l" Format="F9.2" Null="-1" Help="" Min="0.01" Max="999999.99" Default="" Module="Schwer" />'
  WRITE(1, '(A)') '  <Parameter Ident="glZn" Text="Zn-gel." IsLoad="0" Unit="µg/l" Format="F9.2" Null="-1" Help="" Min="0.01" Max="99999.99" Default="" Module="Schwer" />'
  WRITE(1, '(A)') '  <Parameter Ident="gsCad" Text="Cd-ges." IsLoad="0" Unit="µg/l" Format="F8.4" Null="-1" Help="" Min="0.0001" Max="999.9999" Default="" Module="Schwer" />'
  WRITE(1, '(A)') '  <Parameter Ident="glCad" Text="Cd-gel." IsLoad="0" Unit="µg/l" Format="F8.4" Null="-1" Help="" Min="0.0001" Max="999.9999" Default="" Module="Schwer" />'
  WRITE(1, '(A)') '  <Parameter Ident="gsCu" Text="Cu-ges." IsLoad="0" Unit="µg/l" Format="F7.3" Null="-1" Help="" Min="0.001" Max="999.999" Default="" Module="Schwer" />'
  WRITE(1, '(A)') '  <Parameter Ident="glCu" Text="Cu-gel." IsLoad="0" Unit="µg/l" Format="F7.3" Null="-1" Help="" Min="0.001" Max="999.999" Default="" Module="Schwer" />'
  WRITE(1, '(A)') '  <Parameter Ident="gsNi" Text="Ni-ges." IsLoad="0" Unit="µg/l" Format="F7.3" Null="-1" Help="" Min="0.001" Max="999.999" Default="" Module="Schwer" />'
  WRITE(1, '(A)') '  <Parameter Ident="glNi" Text="Ni-gel." IsLoad="0" Unit="µg/l" Format="F7.3" Null="-1" Help="" Min="0.001" Max="999.999" Default="" Module="Schwer" />'
  WRITE(1, '(A)') '</ParamSetDef>'
  WRITE(1, '(A)') '<TpEquations>'
  WRITE(1, '(A)') '  <TpEquation Value="1" Text="CIP (Semi-Lagrange-Verfahren)" Help="Semi-Lagrange-Verfahren" />'
  WRITE(1, '(A)') '  <TpEquation Value="2" Text="LAX-WENDROFF-Verfahren (finite Differenzen-Verfahren)" Help="finite Differenzen-Verfahren" />'
  WRITE(1, '(A)') '  <TpEquation Value="3" Text="QUICKEST-Verfahren (finite Volumen-Verfahren)" Help="finite Volumen-Verfahren (hier: Masseerhaltend)" />'
  WRITE(1, '(A)') '</TpEquations>'
  WRITE(1, '(A)') '<DlEquations>'
  WRITE(1, '(A)') '  <DlEquation Value="1" Text="nach Deng(2001): Dx = (0.15/8*Dy)*(v/u*)^2*(B/H)^1.67" Help="Dx = (0.15/8*Dy)*(v/u*)^2*(B/H)^1.67" />'
  WRITE(1, '(A)') '  <DlEquation Value="2" Text="nach Li(1998): Dx = 0.2*(v/u*)^1.2*(B/H)^1.3*H*u* " Help="Dx = 0.2*(v/u*)^1.2*(B/H)^1.3*H*u*" />'
  WRITE(1, '(A)') '  <DlEquation Value="3" Text="nach Iwasa and Aya(1991): Dx = 2*(B/H)^1.5*H*u* " Help="Dx = 2*(B/H)^1.5*H*u*" />'
  WRITE(1, '(A)') '  <DlEquation Value="4" Text="nach Elder(1959): Dx = 5.93*H*u*" Help="Dx = 5.93*H*u*" />'
  WRITE(1, '(A)') '</DlEquations>'
  WRITE(1, '(A)') '<AerFormulas>'
  WRITE(1, '(A)') '<AerFormula Value="1" Text="k2=79.6*(v*S)^0.32*H^-0.38*B^-0.16+K2wind (mit Wind)" Help="Berechnung nach Kirchesch" />´'
  WRITE(1, '(A)') '<AerFormula Value="2" Text="k2=79.6*(v*S)^0.32*H^-0.38*B^-0.16 (ohne Wind)" Help="Berechnung nach Kirchesch" />'
  WRITE(1, '(A)') '<AerFormula Value="3" Text="##Formelfehler## k2=10.47*v^0.43*H^-1.37*S^0.22+K2wind (Dantengrundlage Wolf 1974)" Help="Berechnung nach Wolf (überarbeitete Form)" />'
  WRITE(1, '(A)') '<AerFormula Value="4" Text="K2=142*(v*S)^0.333*H^-0.66*B^-0.243 (Melching 1999)" Help="Berechnung nach Melching" />'
  WRITE(1, '(A)') '</AerFormulas>'
  WRITE(1, '(A)') '<HmFormulas>'
  WRITE(1, '(A)') '<HmFormula Value="1" Text="Ansatz nach DWA-Modell" Help="" />'
  WRITE(1, '(A)') '<HmFormula Value="2" Text="Verteilungskoeff. nach Deltares (2010)" Help="" />'
  WRITE(1, '(A)') '<HmFormula Value="3" Text="noch nicht belegt" Help="" />'
  WRITE(1, '(A)') '</HmFormulas>'
  WRITE(1, '(A)') '<EvapoFormulas>'
  WRITE(1, '(A)') '  <EvapoFormula Value="1" Text="Windabhängigk. nach WMO" Help="fwind = 0,13 + 0,0936 * uw [m/s]" />'
  WRITE(1, '(A)') '  <EvapoFormula Value="2" Text="nach Sweers (1976)" Help="fwind = 0,153 + 0,063 * uw [m/s]" />'
  WRITE(1, '(A)') '  <EvapoFormula Value="3" Text="nach Rimsha und Donschenko" Help="fwind = 0,211 + 0,103 * uw [m/s])" />'
  WRITE(1, '(A)') '  <EvapoFormula Value="4" Text="ohne Wind nach Priestley-Taylor (1972)" Help="ohne Wind nach Priestley-Taylor (1972)" />'
  WRITE(1, '(A)') '  <EvapoFormula Value="5" Text="ohne Wind nach Delclaux et al. (2007)" Help="ohne Wind nach Delclaux et al. (2007)" />'
  WRITE(1, '(A)') '</EvapoFormulas>'
  WRITE(1, '(A)') '</GerrisParam>'
 
  CLOSE(1)

 END subroutine EreigGParam

