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

!> Write file EreigGParam.xml
subroutine EreigGParam(cpfad1)
   implicit none
   
   character(255), intent(in)  :: cpfad1
   
   character(275) :: pfadstring
   character(8)   :: versionstext
   integer        :: u_erg
   
   external :: version_string
   
   call version_string(versionstext)
   
   pfadstring = trim(adjustl(cpfad1)) // 'EreigGParam.xml'
   open(newunit = u_erg, file = pfadstring, encoding = 'UTF-8')
   
   write(u_erg, '(A)') '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
   call version_string(versionstext)
   write(u_erg, '(3A)') '<GerrisParam FileType="EreigG" QsimVersion="',versionstext,'">'  
   
   write(u_erg, '(A)') '<ParamSetDef Ident="EreigG" Text="Randbedingungs-Parameter" Help="Parameter der Randbedingungen">'
   write(u_erg, '(A)') '  <Parameter Ident="OBSB" Text="C-BSB5" IsLoad="0" Unit="mg/l" Format="F6.2" Null="-1" Help="C-BSB5" Min="0" Max="999.99" Default="" Module="Alle" />'
   write(u_erg, '(A)') '  <Parameter Ident="OCSB" Text="CSB" IsLoad="0" Unit="mg/l" Format="F6.2" Null="-1" Help="CSB" Min="0" Max="999.99" Default="" Module="Alle" />'
   write(u_erg, '(A)') '  <Parameter Ident="VNH4" Text="NH4-N" IsLoad="1" Unit="mg/l" Format="F6.2" Null="-1" Help="NH4-N Gehalt" Min="0" Max="999.99" Default="" Module="Alle" />'
   write(u_erg, '(A)') '  <Parameter Ident="VNO2" Text="Nitrit-N" IsLoad="1" Unit="mg/l" Format="F6.3" Null="-1" Help="Nitrit-Stickstoffgehalt" Min="0" Max="99.999" Default="" Module="Alle" />'
   write(u_erg, '(A)') '  <Parameter Ident="VNO3" Text="Nitrat-N" IsLoad="1" Unit="mg/l" Format="F5.2" Null="-1" Help="Nitrat-Stickstoffgehalt" Min="0" Max="99.99" Default="" Module="Alle" />'
   write(u_erg, '(A)') '  <Parameter Ident="GESN" Text="Gesamt-N" IsLoad="1" Unit="mg/l" Format="F5.2" Null="-1" Help="Gesamt-Stickstoff" Min="0" Max="99.99" Default="" Module="Alle" />'
   write(u_erg, '(A)') '  <Parameter Ident="VX0" Text="Nitrosomonas" IsLoad="0" Unit="mg/l" Format="F8.5" Null="-1" Help="suspendierte Nitrosomonasbiomasse" Min="0" Max="99.99999" Default="" Module="Alle" />'
   write(u_erg, '(A)') '  <Parameter Ident="VX02" Text="Nitrobacter" IsLoad="0" Unit="mg/l" Format="F8.5" Null="-1" Help="suspendierte Nitrobacterbiomasse" Min="0" Max="99.99999" Default="" Module="Alle" />'
   write(u_erg, '(A)') '  <Parameter Ident="GELP" Text="Gelöster Phosphor" IsLoad="1" Unit="mg/l" Format="F6.3" Null="-1" Help="gelöster Phosphorgehalt" Min="0" Max="99.999" Default="" Module="Alle" />'
   write(u_erg, '(A)') '  <Parameter Ident="GESP" Text="Gesamt-Phosphor" IsLoad="1" Unit="mg/l" Format="F5.2" Null="-1" Help="Gesamt-Phosphor" Min="0" Max="99.99" Default="" Module="Alle" />'
   write(u_erg, '(A)') '  <Parameter Ident="SI" Text="Silizium" IsLoad="0" Unit="mg/l" Format="F5.2" Null="-1" Help="gelöster Siliziumgehalt" Min="0" Max="99.99" Default="" Module="Alle" />'
   write(u_erg, '(A)') '  <Parameter Ident="CHLA" Text="Chlorophyll-a" IsLoad="0" Unit="µg/l" Format="F6.2" Null="-1" Help="Chlorophyll-a-Gehalt" Min="0" Max="999.99" Default="" Module="Alle" />'
   write(u_erg, '(A)') '  <Parameter Ident="VKIGR" Text="Kieselalgen" IsLoad="0" Unit="0-1" Format="F5.2" Null="-1" Help="Anteil der Kieselalgen am Gesamt-Chlorophyll-a" Min="0" Max="99.99" Default="" Module="Alle" />'
   write(u_erg, '(A)') '  <Parameter Ident="ANTBL" Text="Blaualgen" IsLoad="0" Unit="0-1" Format="F5.2" Null="-1" Help="Anteil der Blaualgen am Gesamt-Chlorophyll-a" Min="0" Max="99.99" Default="" Module="Alle" />'
   write(u_erg, '(A)') '  <Parameter Ident="ZOOIND" Text="Rotatorien" IsLoad="0" Unit="Ind/l" Format="F7.1" Null="-1" Help="Rotatoriendichte" Min="0" Max="99999.9" Default="" Module="Alle" />'
   write(u_erg, '(A)') '  <Parameter Ident="VPH" Text="pH-Wert" IsLoad="0" Unit="" Format="F5.2" Null="-1" Help="pH-Wert" Min="0" Max="99.99" Default="" Module="Alle" />'
   write(u_erg, '(A)') '  <Parameter Ident="MW" Text="m-Wert" IsLoad="0" Unit="mmol/l" Format="F5.2" Null="-1" Help="m-Wert" Min="0" Max="99.99" Default="" Module="Alle" />'
   write(u_erg, '(A)') '  <Parameter Ident="CA" Text="Calcium" IsLoad="0" Unit="mg/l" Format="F5.1" Null="-1" Help="Calciumkonzentration" Min="0" Max="999.9" Default="" Module="Alle" />'
   write(u_erg, '(A)') '  <Parameter Ident="LF" Text="Leitfähigkeit" IsLoad="0" Unit="µS/cm" Format="F8.1" Null="-1" Help="Leitfähigkeit" Min="0" Max="999999.9" Default="" Module="Alle" />'
   write(u_erg, '(A)') '  <Parameter Ident="SS" Text="Schwebstoff" IsLoad="0" Unit="mg/l" Format="F7.2" Null="-1" Help="Schwebstoffgehalt" Min="0" Max="9999.99" Default="" Module="Alle" />'
   write(u_erg, '(A)') '  <Parameter Ident="TEMPW" Text="Wassertemperatur" IsLoad="0" Unit="°C" Format="F5.2" Null="-9.99" Help="Wassertemperatur" Min="-9.98" Max="99.99" Default="" Module="Alle,Temperatur,Coli" />'
   write(u_erg, '(A)') '  <Parameter Ident="VO2" Text="Sauerstoff" IsLoad="0" Unit="mg/l" Format="F5.2" Null="-1" Help="Sauerstoffgehalt" Min="0" Max="99.99" Default="" Module="Alle" />'
   write(u_erg, '(A)') '  <Parameter Ident="CHNF" Text="Nanoflagellaten" IsLoad="0" Unit="Ind/ml" Format="F8.1" Null="-1" Help="Heterotrophe Nanoflagellaten" Min="0" Max="999999.9" Default="" Module="Alle" />'
   write(u_erg, '(A)') '  <Parameter Ident="BVHNF" Text="HNF Biovolumen" IsLoad="0" Unit="µm³" Format="F6.1" Null="-1" Help="Biovolumen der HNF" Min="0" Max="9999.9" Default="" Module="Alle" />'
   write(u_erg, '(A)') '  <Parameter Ident="COLI" Text="Coliforme" IsLoad="0" Unit="Ind/100 ml" Format="E9.2" Null="-1.0" Help="Fäkalcoliforme Bakterien" Min="0.0" Max="999999999999999.9" Default="" Module="Alle,Temperatur,Coli" />'
   write(u_erg, '(A)') '  <Parameter Ident="EWAERM" Text="Wärme" IsLoad="0" Unit="MJ/s" Format="F7.1" Null="-9999.9" Help="Menge der eingeleiteten Wärme (Kraftwerk)" Min="-9999.8" Max="99999.9" Default="" Module="Alle,Temperatur" />'
   write(u_erg, '(A)') '  <Parameter Ident="TRACER" Text="Tracermenge" IsLoad="0" Unit="µg/l" Format="F9.3" Null="-1." Help="Tracerkonzentration" Min="0" Max="99999.999" Default="" Module="Tracer" />'
   write(u_erg, '(A)') '  <Parameter Ident="KONSS" Text="konserv. Substanz (Cl etc." IsLoad="0" Unit="mg/l" Format="F7.1" Null="-1" Help="Tracerkonzentration" Min="0" Max="99999.9" Default="" Module="KonsS" />'
   write(u_erg, '(A)') '  <Parameter Ident="gsZn" Text="Zn-ges." IsLoad="0" Unit="µg/l" Format="F9.2" Null="-1" Help="" Min="0.01" Max="999999.99" Default="" Module="Schwer" />'
   write(u_erg, '(A)') '  <Parameter Ident="glZn" Text="Zn-gel." IsLoad="0" Unit="µg/l" Format="F9.2" Null="-1" Help="" Min="0.01" Max="99999.99" Default="" Module="Schwer" />'
   write(u_erg, '(A)') '  <Parameter Ident="gsCad" Text="Cd-ges." IsLoad="0" Unit="µg/l" Format="F8.4" Null="-1" Help="" Min="0.0001" Max="999.9999" Default="" Module="Schwer" />'
   write(u_erg, '(A)') '  <Parameter Ident="glCad" Text="Cd-gel." IsLoad="0" Unit="µg/l" Format="F8.4" Null="-1" Help="" Min="0.0001" Max="999.9999" Default="" Module="Schwer" />'
   write(u_erg, '(A)') '  <Parameter Ident="gsCu" Text="Cu-ges." IsLoad="0" Unit="µg/l" Format="F7.3" Null="-1" Help="" Min="0.001" Max="999.999" Default="" Module="Schwer" />'
   write(u_erg, '(A)') '  <Parameter Ident="glCu" Text="Cu-gel." IsLoad="0" Unit="µg/l" Format="F7.3" Null="-1" Help="" Min="0.001" Max="999.999" Default="" Module="Schwer" />'
   write(u_erg, '(A)') '  <Parameter Ident="gsNi" Text="Ni-ges." IsLoad="0" Unit="µg/l" Format="F7.3" Null="-1" Help="" Min="0.001" Max="999.999" Default="" Module="Schwer" />'
   write(u_erg, '(A)') '  <Parameter Ident="glNi" Text="Ni-gel." IsLoad="0" Unit="µg/l" Format="F7.3" Null="-1" Help="" Min="0.001" Max="999.999" Default="" Module="Schwer" />'
   write(u_erg, '(A)') '</ParamSetDef>'
   
   write(u_erg, '(A)') '<TpEquations>'
   write(u_erg, '(A)') '  <TpEquation Value="1" Text="CIP (Semi-Lagrange-Verfahren)" Help="Semi-Lagrange-Verfahren" />'
   write(u_erg, '(A)') '  <TpEquation Value="2" Text="LAX-WENDROFF-Verfahren (finite Differenzen-Verfahren)" Help="finite Differenzen-Verfahren" />'
   write(u_erg, '(A)') '  <TpEquation Value="3" Text="QUICKEST-Verfahren (finite Volumen-Verfahren)" Help="finite Volumen-Verfahren (hier: Masseerhaltend)" />'
   write(u_erg, '(A)') '</TpEquations>'
   
   write(u_erg, '(A)') '<DlEquations>'
   write(u_erg, '(A)') '  <DlEquation Value="1" Text="nach Deng(2001): Dx = (0.15/8*Dy)*(v/u*)^2*(B/H)^1.67" Help="Dx = (0.15/8*Dy)*(v/u*)^2*(B/H)^1.67" />'
   write(u_erg, '(A)') '  <DlEquation Value="2" Text="nach Li(1998): Dx = 0.2*(v/u*)^1.2*(B/H)^1.3*H*u* " Help="Dx = 0.2*(v/u*)^1.2*(B/H)^1.3*H*u*" />'
   write(u_erg, '(A)') '  <DlEquation Value="3" Text="nach Iwasa and Aya(1991): Dx = 2*(B/H)^1.5*H*u* " Help="Dx = 2*(B/H)^1.5*H*u*" />'
   write(u_erg, '(A)') '  <DlEquation Value="4" Text="nach Elder(1959): Dx = 5.93*H*u*" Help="Dx = 5.93*H*u*" />'
   write(u_erg, '(A)') '</DlEquations>'
   
   write(u_erg, '(A)') '<AerFormulas>'
   write(u_erg, '(A)') '<AerFormula Value="1" Text="k2=79.6*(v*S)^0.32*H^-0.38*B^-0.16+K2wind (mit Wind)" Help="Berechnung nach Kirchesch" />´'
   write(u_erg, '(A)') '<AerFormula Value="2" Text="k2=79.6*(v*S)^0.32*H^-0.38*B^-0.16 (ohne Wind)" Help="Berechnung nach Kirchesch" />'
   write(u_erg, '(A)') '<AerFormula Value="3" Text="##Formelfehler## k2=10.47*v^0.43*H^-1.37*S^0.22+K2wind (Dantengrundlage Wolf 1974)" Help="Berechnung nach Wolf (überarbeitete Form)" />'
   write(u_erg, '(A)') '<AerFormula Value="4" Text="K2=142*(v*S)^0.333*H^-0.66*B^-0.243 (Melching 1999)" Help="Berechnung nach Melching" />'
   write(u_erg, '(A)') '</AerFormulas>'
   
   write(u_erg, '(A)') '<HmFormulas>'
   write(u_erg, '(A)') '<HmFormula Value="1" Text="Ansatz nach DWA-Modell" Help="" />'
   write(u_erg, '(A)') '<HmFormula Value="2" Text="Verteilungskoeff. nach Deltares (2010)" Help="" />'
   write(u_erg, '(A)') '<HmFormula Value="3" Text="noch nicht belegt" Help="" />'
   write(u_erg, '(A)') '</HmFormulas>'
   
   write(u_erg, '(A)') '<EvapoFormulas>'
   write(u_erg, '(A)') '  <EvapoFormula Value="1" Text="Windabhängigk. nach WMO" Help="fwind = 0,13 + 0,0936 * uw [m/s]" />'
   write(u_erg, '(A)') '  <EvapoFormula Value="2" Text="nach Sweers (1976)" Help="fwind = 0,153 + 0,063 * uw [m/s]" />'
   write(u_erg, '(A)') '  <EvapoFormula Value="3" Text="nach Rimsha und Donschenko" Help="fwind = 0,211 + 0,103 * uw [m/s])" />'
   write(u_erg, '(A)') '  <EvapoFormula Value="4" Text="ohne Wind nach Priestley-Taylor (1972)" Help="ohne Wind nach Priestley-Taylor (1972)" />'
   write(u_erg, '(A)') '  <EvapoFormula Value="5" Text="ohne Wind nach Delclaux et al. (2007)" Help="ohne Wind nach Delclaux et al. (2007)" />'
   write(u_erg, '(A)') '</EvapoFormulas>'
   
   write(u_erg, '(A)') '</GerrisParam>'
   
   close(u_erg)
end subroutine EreigGParam

