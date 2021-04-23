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

  subroutine AParamParam(cpfad1,j1) 

!  Ausgabe der Definition von AParam

  character(200)  :: cpfad1  
  character (len=275)         :: pfadstring

  write(pfadstring,'(2A)')trim(adjustl(cpfad1(1:j1))),'AParamParam.xml'
  open(unit=200, file=pfadstring, encoding='UTF-8')
  
  WRITE(200, '(A)')'<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
  WRITE(200, '(A)') '<GerrisParam FileType="AParam" QsimVersion="14.02">'  
  WRITE(200, '(A)') '<ParamSetDef Ident="AParam" Text="Allg. Parameter" Null="-1" Help="Allgemeine Simulations-Parameter" Scope="Modell" IsGrouped="0">'
  WRITE(200, '(A)') '  <Parameter Ident="AGCHL" Text="Kohlenstoff/Chlorophyll Grünalgen (dunkeladaptiert) bei 20°C" Unit="mgC/mgChla" Format="F4.1" Null="-1" Help="" Default="12.4" Min="0" Max="99.9" Gruppe="Grünalgen" Kategorie="C Biomasse" />'
  WRITE(200, '(A)') '  <Parameter Ident="AGGMAX" Text="Max. Wachstumsrate d. Grünalgen bei 20°C" Unit="1/d" Format="F5.2" Null="-1" Help="" Default="1.2" Min="0" Max="99.99" Gruppe="Grünalgen" Kategorie="Wachstum" />'
  WRITE(200, '(A)') '  <Parameter Ident="IKg" Text="Lichtsättigung für kohlenstoffspez. Photosynthese der Grünalgen bei 20°C" Unit="µE/(m2*s)" Format="F6.2" Null="-1" Help="" Default="58.6" Min="0" Max="999.99" Gruppe="Grünalgen" Kategorie="Licht" />' 
  WRITE(200, '(A)') '  <Parameter Ident="AGKSN" Text="Halbsättigung Grünalgen N" Unit="mg/l" Format="F5.3" Null="-1" Help="" Default="0.007" Min="0" Max="9.999" Gruppe="Grünalgen" Kategorie="N" />'
  WRITE(200, '(A)') '  <Parameter Ident="AGKSP" Text="Halbsättigung Grünalgen P" Unit="mg/l" Format="F6.4" Null="-1" Help="" Default="0.023" Min="0" Max="9.9999" Gruppe="Grünalgen" Kategorie="P" />'
  WRITE(200, '(A)') '  <Parameter Ident="AGREMI" Text="Grundrespiration d. Grünalgen bei 20°C" Unit="1/d" Format="F5.3" Null="-1" Help="" Default="0.11" Min="0.05" Max="9.999" Gruppe="Grünalgen" Kategorie="Wachstum" />'
  WRITE(200, '(A)') '  <Parameter Ident="frmuge" Text="Anteil der vom Wachstum abhängigigen Respiration (Grünalgen)" Unit=" -" Format="F5.2" Null="-1" Help="" Default="0.067" Min="0" Max="99.99" Gruppe="Grünalgen" Kategorie="Wachstum" />'
  WRITE(200, '(A)') '  <Parameter Ident="BSBGR" Text="C-BSB5-Erhöhung Grünalgen" Unit="mg/mgC" Format="F6.4" Null="-1" Help="" Default="0.54" Min="0" Max="9.9999" Gruppe="Grünalgen" Kategorie="C Biomasse" />'
  WRITE(200, '(A)') '  <Parameter Ident="CSBGR" Text="CSB-Erhöhung Grünalgen" Unit="mg/mgC" Format="F6.4" Null="-1" Help="" Default="3.1" Min="0" Max="9.9999" Gruppe="Grünalgen" Kategorie="C Biomasse" />'
  WRITE(200, '(A)') '  <Parameter Ident="QMX_NG" Text="max. N-Gehalt der Grünalgenzelle" Unit="mg/mgBio" Format="F7.5" Null="-1" Help="" Default="0.11" Min="0" Max="9.99999" Gruppe="Grünalgen" Kategorie="N" />'
  WRITE(200, '(A)') '  <Parameter Ident="QMX_PG" Text="max. P-Gehalt der Grünalgenzelle" Unit="mg/mgBio" Format="F7.5" Null="-1" Help="" Default="0.012" Min="0" Max="9.99999" Gruppe="Grünalgen" Kategorie="P" />'
  WRITE(200, '(A)') '  <Parameter Ident="QMN_NG" Text="min. N-Gehalt der Grünalgenzelle" Unit="mg/mgBio" Format="F7.5" Null="-1" Help="" Default="0.02" Min="0" Max="9.99999" Gruppe="Grünalgen" Kategorie="N" />'
  WRITE(200, '(A)') '  <Parameter Ident="QMN_PG" Text="min. P-Gehalt der Grünalgenzelle" Unit="mg/mgBio" Format="F7.5" Null="-1" Help="" Default="0.0016" Min="0" Max="9.99999" Gruppe="Grünalgen" Kategorie="P" />'
  WRITE(200, '(A)') '  <Parameter Ident="UPMXNG" Text="max. N-Aufnahmerate der Grünalgen" Unit="mgN/(mgBio*d)" Format="F5.3" Null="-1" Help="" Default="0.18" Min="0" Max="9.999" Gruppe="Grünalgen" Kategorie="N" />'
  WRITE(200, '(A)') '  <Parameter Ident="UPMXPG" Text="max. P-Aufnahmerate der Grünalgen" Unit="mgP/(mgBio*d)" Format="F5.3" Null="-1" Help="" Default="0.69" Min="0" Max="9.999" Gruppe="Grünalgen" Kategorie="P" />'
  WRITE(200, '(A)') '  <Parameter Ident="OPGRMI" Text="RQ respiratorischer Quotient für Grünalgen" Unit="molC/mol O2" Format="F4.2" Null="-1" Help="" Default="0.67" Min="0" Max="9.99" Gruppe="Grünalgen" Kategorie="O" />'
  WRITE(200, '(A)') '  <Parameter Ident="OPGRMA" Text="PQ photosynthetischer Quotient für Grünalgen" Unit="molO2/molC" Format="F4.2" Null="-1" Help="" Default="1.9" Min="0" Max="9.99" Gruppe="Grünalgen" Kategorie="O" />'
  WRITE(200, '(A)') '  <Parameter Ident="ASGRE" Text="Sediment Grünalgen" Unit="0-1" Format="F5.2" Null="-1" Help="" Default="0.5" Min="0" Max="99.99" Gruppe="Grünalgen" Kategorie="Wachstum" />'
  WRITE(200, '(A)') '  <Parameter Ident="TOPTG" Text="optimal Temperatur für Grünalgenwachstum" Unit="°C" Format="F5.2" Null="-1" Help="" Default="30.2" Min="0" Max="99.99" Gruppe="Grünalgen" Kategorie="Temperatur" />'
  WRITE(200, '(A)') '  <Parameter Ident="KTEMP_GR" Text="empirische Konstante KT(µ) für Temperaturabhängigkeit (Exponent)" Unit="1/°C" Format="F7.5" Null="-1" Help="" Default="0.0041" Min="0" Max="9.99999" Gruppe="Grünalgen" Kategorie="Temperatur" />'
  WRITE(200, '(A)') '  <Parameter Ident="AKCHL" Text="Kohlenstoff/Chlorophyll Kieselalgen (dunkeladaptiert) bei 20°C" Unit="mgC/mgChla" Format="F4.1" Null="-1" Help="" Default="13.3" Min="0" Max="99.9" Gruppe="Kieselalgen" Kategorie="C Biomasse" />'
  WRITE(200, '(A)') '  <Parameter Ident="AKGMAX" Text="Max. Wachstumsate d. Kieselalgen bei 20°C" Unit="1/d" Format="F5.2" Null="-1" Help="" Default="1.6" Min="0" Max="99.99" Gruppe="Kieselalgen" Kategorie="Wachstum" />'
  WRITE(200, '(A)') '  <Parameter Ident="IKke" Text="Lichtsättigung für kohlenstoffspz.Photosynthese der Kieselalgen bei 20°C" Unit="µE/(m2*s)" Format="F6.2" Null="-1" Help="" Default="43.9" Min="0" Max="999.99" Gruppe="Kieselalgen" Kategorie="Licht" />'
  WRITE(200, '(A)') '  <Parameter Ident="AKKSN" Text="N-Halbsättigung Kieselalgen" Unit="mg/l" Format="F5.3" Null="-1" Help="" Default="0.007" Min="0" Max="9.999" Gruppe="Kieselalgen" Kategorie="N" />'
  WRITE(200, '(A)') '  <Parameter Ident="AKKSP" Text="P-Halbsättigung Kieselalgen" Unit="mg/l" Format="F6.4" Null="-1" Help="" Default="0.023" Min="0" Max="9.9999" Gruppe="Kieselalgen" Kategorie="P" />'
  WRITE(200, '(A)') '  <Parameter Ident="AKKSSI" Text="Si-Halbsättigung Kieselalgen" Unit="mg/l" Format="F5.3" Null="-1" Help="" Default="0.08" Min="0" Max="9.999" Gruppe="Kieselalgen" Kategorie="Si" />'
  WRITE(200, '(A)') '  <Parameter Ident="AKREMI" Text="Grundrespiration d. Kieselalgen bei 20°C" Unit="1/d" Format="F5.3" Null="-1" Help="" Default="0.085" Min="0" Max="9.999" Gruppe="Kieselalgen" Kategorie="Wachstum" />'
  WRITE(200, '(A)') '  <Parameter Ident="frmuke" Text="Anteil der vom Wachstum abhängigigen Respiration für Kieselalgen" Unit=" -" Format="F5.2" Null="-1" Help="" Default="0.2" Min="0" Max="99.99" Gruppe="Kieselalgen" Kategorie="Wachstum" />'
  WRITE(200, '(A)') '  <Parameter Ident="BSBKI" Text="C-BSB5-Erhöhung Kieselalgen" Unit="mg/mgC" Format="F6.4" Null="-1" Help="" Default="0.73" Min="0" Max="9.9999" Gruppe="Kieselalgen" Kategorie="C Biomasse" />'
  WRITE(200, '(A)') '  <Parameter Ident="CSBKI" Text="CSB-Erhöhung Kieselalgen" Unit="mg/mgC" Format="F6.4" Null="-1" Help="" Default="3.1" Min="0" Max="9.9999" Gruppe="Kieselalgen" Kategorie="C Biomasse" />'
  WRITE(200, '(A)') '  <Parameter Ident="QMX_NK" Text="max. N-Gehalt der Kieselalgenzelle" Unit="mg/mgBio" Format="F7.5" Null="-1" Help="" Default="0.052" Min="0" Max="9.99999" Gruppe="Kieselalgen" Kategorie="N" />'
  WRITE(200, '(A)') '  <Parameter Ident="QMX_PK" Text="max. P-Gehalt der Kieselalgenzelle" Unit="mg/mgBio" Format="F7.5" Null="-1" Help="" Default="0.009" Min="0" Max="9.99999" Gruppe="Kieselalgen" Kategorie="P" />'
  WRITE(200, '(A)') '  <Parameter Ident="QMX_SK" Text="max. Si-Gehalt der Kieselalgenzelle" Unit="mg/mgBio" Format="F7.5" Null="-1" Help="" Default="0.18" Min="0" Max="9.99999" Gruppe="Kieselalgen" Kategorie="Si" />'
  WRITE(200, '(A)') '  <Parameter Ident="QMN_NK" Text="min. N-Gehalt der Kieselalgenzelle" Unit="mg/mgBio" Format="F7.5" Null="-1" Help="" Default="0.012" Min="0" Max="9.99999" Gruppe="Kieselalgen" Kategorie="N" />'
  WRITE(200, '(A)') '  <Parameter Ident="QMN_PK" Text="min. P-Gehalt der Kieselalgenzelle" Unit="mg/mgBio" Format="F7.5" Null="-1" Help="" Default="0.0011" Min="0" Max="9.99999" Gruppe="Kieselalgen" Kategorie="P" />'
  WRITE(200, '(A)') '  <Parameter Ident="QMN_SK" Text="min. Si-Gehalt der Kieselalgenzelle" Unit="mg/mgBio" Format="F7.5" Null="-1" Help="" Default="0.18" Min="0" Max="9.99999" Gruppe="Kieselalgen" Kategorie="Si" />'
  WRITE(200, '(A)') '  <Parameter Ident="UPMXNK" Text="max. N-Aufnahmerate der Kieselalgen" Unit="mgN/(mgBio*d)" Format="F5.3" Null="-1" Help="" Default="0.31" Min="0" Max="9.999" Gruppe="Kieselalgen" Kategorie="N" />'
  WRITE(200, '(A)') '  <Parameter Ident="UPMXPK" Text="max. P-Aufnahmerate der Kieselalgen" Unit="mgP/(mgBio*d)" Format="F5.3" Null="-1" Help="" Default="0.62" Min="0" Max="9.999" Gruppe="Kieselalgen" Kategorie="P" />'
  WRITE(200, '(A)') '  <Parameter Ident="UPMXSK" Text="max. Si-Aufnahmerate der Kieselalgen" Unit="mgSi/(mgBio*d)" Format="F5.3" Null="-1" Help="" Default="2.5" Min="0" Max="9.999" Gruppe="Kieselalgen" Kategorie="Si" />'
  WRITE(200, '(A)') '  <Parameter Ident="OPKIMI" Text="RQ respiratorischer Quotient für Kieselalgen" Unit="mol C/mol O2" Format="F4.2" Null="-1" Help="" Default="0.57" Min="0" Max="9.99" Gruppe="Kieselalgen" Kategorie="O" />'
  WRITE(200, '(A)') '  <Parameter Ident="OPKIMA" Text="PQ photosynthetischer Quotient für Kieselalgen" Unit="mol O2/mol C" Format="F4.2" Null="-1" Help="" Default="2.1" Min="0" Max="9.99" Gruppe="Kieselalgen" Kategorie="O" />'
  WRITE(200, '(A)') '  <Parameter Ident="ASKIE" Text="Sediment Kieselalgen" Unit="0-1" Format="F5.2" Null="-1" Help="Sedimentierbarer Anteil für Kieselalgen" Default="0.5" Min="0" Max="99.99" Gruppe="Kieselalgen" Kategorie="Wachstum" />'
  WRITE(200, '(A)') '  <Parameter Ident="TOPTK" Text="optimal Temperatur für Kieselalgenwachstum" Unit="°C" Format="F5.2" Null="-1" Help="" Default="20.3" Min="20.00" Max="28.00" Gruppe="Kieselalgen" Kategorie="Temperatur" />'
  WRITE(200, '(A)') '  <Parameter Ident="KTEMP_Ki" Text="empirische Konstante KT(µ) für Temperaturabhängigkeit (Exponent)" Unit="1/°C" Format="F7.5" Null="-1" Help="" Default="0.0065" Min="0.002" Max="0.009" Gruppe="Kieselalgen" Kategorie="Temperatur" />'
  WRITE(200, '(A)') '  <Parameter Ident="ABCHL" Text="Kohlenstoff/Chlorophyll Blaualgen" Unit="mgC/mgChla" Format="F5.1" Null="-1" Help="" Default="68." Min="0" Max="999.9" Gruppe="Blaualgen" Kategorie="C Biomasse" />'
  WRITE(200, '(A)') '  <Parameter Ident="ABGMAX" Text="Max. Wachstumsrate d. Blaualgen bei 20°C" Unit="1/d" Format="F5.2" Null="-1" Help="" Default="1.2" Min="0" Max="99.99" Gruppe="Blaualgen" Kategorie="Wachstum" />'
  WRITE(200, '(A)') '  <Parameter Ident="IKbe" Text="Lichtsättigung für kohlenstoffspez.Photosynthese der Blaualgen bei 20°C" Unit="µE/m2*s)" Format="F6.2" Null="-1" Help="" Default="99.1" Min="0" Max="999.99" Gruppe="Blaualgen" Kategorie="Licht" />'
  WRITE(200, '(A)') '  <Parameter Ident="ABKSN" Text="N-Halbsättigung Blaualgen" Unit="mg/l" Format="F5.3" Null="-1" Help="" Default="0.007" Min="0" Max="9.999" Gruppe="Blaualgen" Kategorie="N" />'
  WRITE(200, '(A)') '  <Parameter Ident="ABKSP" Text="P-Halbsättigung Blaualgen" Unit="mg/l" Format="F6.4" Null="-1" Help="" Default="0.023" Min="0" Max="9.9999" Gruppe="Blaualgen" Kategorie="P" />'
  WRITE(200, '(A)') '  <Parameter Ident="ABREMI" Text="Grundrespiration d. Blaualgen bei 20°C" Unit="1/d" Format="F5.3" Null="-1" Help="" Default="0.085" Min="0" Max="9.999" Gruppe="Blaualgen" Kategorie="Wachstum" />'
  WRITE(200, '(A)') '  <Parameter Ident="frmube" Text="Anteil der vom Wachstum abhängigigen Respiration (Blaulalgen)" Unit=" -" Format="F5.2" Null="-1" Help="" Default="0.2" Min="0" Max="99.99" Gruppe="Blaualgen" Kategorie="Wachstum" />'
  WRITE(200, '(A)') '  <Parameter Ident="BSBBL" Text="C-BSB5-Erhöhung Blaualgen" Unit="mg/mgC" Format="F6.4" Null="-1" Help="" Default="0.44" Min="0" Max="9.9999" Gruppe="Blaualgen" Kategorie="C Biomasse" />'
  WRITE(200, '(A)') '  <Parameter Ident="CSBBL" Text="CSB-Erhöhung Blaualgen" Unit="mg/µmgC" Format="F6.4" Null="-1" Help="" Default="3.1" Min="0" Max="9.9999" Gruppe="Blaualgen" Kategorie="C Biomasse" />'
  WRITE(200, '(A)') '  <Parameter Ident="QMX_NB" Text="max. N-Gehalt der Blaualgenzelle" Unit="mg/mgBio" Format="F7.5" Null="-1" Help="" Default="0.088" Min="0" Max="9.99999" Gruppe="Blaualgen" Kategorie="N" />'
  WRITE(200, '(A)') '  <Parameter Ident="QMX_PB" Text="max. P-Gehalt der Blaualgenzelle" Unit="mg/mgBio" Format="F7.5" Null="-1" Help="" Default="0.007" Min="0" Max="9.99999" Gruppe="Blaualgen" Kategorie="P" />'
  WRITE(200, '(A)') '  <Parameter Ident="QMN_NB" Text="min. N-Gehalt der Blaualgenzelle" Unit="mg/mgBio" Format="F7.5" Null="-1" Help="" Default="0.026" Min="0" Max="9.99999" Gruppe="Blaualgen" Kategorie="N" />'
  WRITE(200, '(A)') '  <Parameter Ident="QMN_PB" Text="min. P-Gehalt der Blaualgenzelle" Unit="mg/mgBio" Format="F7.5" Null="-1" Help="" Default="0.0009" Min="0" Max="9.99999" Gruppe="Blaualgen" Kategorie="P" />'
  WRITE(200, '(A)') '  <Parameter Ident="UPMXNB" Text="max. N-Aufnahmerate der Blaualgen" Unit="mgN/(mgBio*d)" Format="F5.3" Null="-1" Help="" Default="0.31" Min="0" Max="9.999" Gruppe="Blaualgen" Kategorie="N" />'
  WRITE(200, '(A)') '  <Parameter Ident="UPMXPB" Text="max. P-Aufnahmerate der Blaualgen" Unit="mgP/(mgBio*d)" Format="F5.3" Null="-1" Help="" Default="0.62" Min="0" Max="9.999" Gruppe="Blaualgen" Kategorie="P" />'
  WRITE(200, '(A)') '  <Parameter Ident="OPBLMI" Text="RQ respiratorischer Quotient für Blaualgen" Unit="mol C/mol O2" Format="F4.2" Null="-1" Help="" Default="0.68" Min="0" Max="9.99" Gruppe="Blaualgen" Kategorie="O" />'
  WRITE(200, '(A)') '  <Parameter Ident="OPBLMA" Text="PQ photosynthetischer Quotient für Blaualgen" Unit="mol O2/mol C" Format="F4.2" Null="-1" Help="" Default="1.85" Min="0" Max="9.99" Gruppe="Blaualgen" Kategorie="O" />'
  WRITE(200, '(A)') '  <Parameter Ident="ASBLE" Text="Sediment Blaualgen" Unit="0-1" Format="F5.2" Null="-1" Help="" Default="0" Min="0" Max="99.99" Gruppe="Blaualgen" Kategorie="Wachstum" />'
  WRITE(200, '(A)') '  <Parameter Ident="TOPTB" Text="optimal Temperatur für Blaualgenwachstum" Unit="°C" Format="F5.2" Null="-1" Help="Fadenbildend: 23.7; Kolonienbildend: 31.8" Default="26" Min="0" Max="99.99" Gruppe="Blaualgen" Kategorie="Temperatur" />'
  WRITE(200, '(A)') '  <Parameter Ident="KTEMP_Bl" Text="empirische Konstante KT(µ) für Temperaturabhängigkeit (Exponent)" Unit="1/°C" Format="F7.5" Null="-1" Help="µ = µmax*exp(-kT(µ)*(T-Topt)^2), Fadenbildend: 0.0069; Kolonienbildend: 0.0081" Default="0.0081" Min="0" Max="99.99" Gruppe="Blaualgen" Kategorie="Temperatur" />'
  WRITE(200, '(A)') '  <Parameter Ident="ifix" Text="Luftstickstofffixierer (0/1)" Unit="" Format="I2" Null="-1" Help="Luftstickstofffixierer(0:Nein/1:Ja)" Default="0" Min="0" Max="1" Gruppe="Blaualgen" Kategorie="Wachstum" />'
  WRITE(200, '(A)') '  <Parameter Ident="IRMAX" Text="max. Gewichtsspez. Algenaufnahmerate d. Rotatorien" Unit="µgC-2/3*d-1" Format="F5.2" Null="-1" Help="Max. Ingestionsrate für Rotatorien" Default="2.9" Min="0" Max="99.99" Gruppe="Rotatorien" Kategorie="Grazing" />'
  WRITE(200, '(A)') '  <Parameter Ident="FOPTR" Text="Halbsättigungskonstante für Futteraufnahme d. Rotatorien" Unit="mg/l" Format="F5.2" Null="-1" Help="Optimale Futterkonzentration für Rotatorienwachstum" Default="0.80" Min="0" Max="99.99" Gruppe="Rotatorien" Kategorie="Grazing" />'
  WRITE(200, '(A)') '  <Parameter Ident="GROT" Text="Gewicht Rotatorie" Unit="µg" Format="F5.2" Null="-1" Help="Gewicht einer Rotatorie" Default="0.3" Min="0" Max="99.99" Gruppe="Rotatorien" Kategorie="Grazing" />'
  WRITE(200, '(A)') '  <Parameter Ident="ZRESG" Text="Grundrespiration Rotatorien" Unit="1/d" Format="F5.3" Null="-1" Help="Grundrespiration der Rotatorien" Default="0.09" Min="0" Max="9.999" Gruppe="Rotatorien" Kategorie="Grazing" />'
  WRITE(200, '(A)') '  <Parameter Ident="ZAKI" Text="Filtrierbarkeit Kieselalgen" Unit="0-1" Format="F5.2" Null="-1" Help="Filtrierbarkeit der Kieselalgen durch Rotatorien" Default="0.6" Min="0" Max="99.99" Gruppe="Rotatorien" Kategorie="Grazing" />'
  WRITE(200, '(A)') '  <Parameter Ident="ZAGR" Text="Filtrierbarkeit Grünalgen" Unit="0-1" Format="F5.2" Null="-1" Help="Filtrierbarkeit der Grünalgen durch Rotatorien" Default="0.8" Min="0" Max="99.99" Gruppe="Rotatorien" Kategorie="Grazing" />'
  WRITE(200, '(A)') '  <Parameter Ident="ZABL" Text="Filtrierbarkeit Blaualgen" Unit="0-1" Format="F5.2" Null="-1" Help="Filtrierbarkeit der Blaualgen durch Rotatorien" Default="0.1" Min="0" Max="99.99" Gruppe="Rotatorien" Kategorie="Grazing" />'
  WRITE(200, '(A)') '  <Parameter Ident="YNMAX1" Text="Max. Wachstum Nitrosomonas" Unit="1/d" Format="F4.2" Null="-1" Help="Max. Wachstumsrate der Nitrosomonas" Default="0.58" Min="0" Max="9.99" Gruppe="Nitrosomonas" Kategorie="Wachstum" />'
  WRITE(200, '(A)') '  <Parameter Ident="STKS1" Text="Halbsättigung Nitrosomonas" Unit="mgNH4-N/l" Format="F5.2" Null="-1" Help="Halbsättigungskonstante für Nitrosomonas" Default="0.49" Min="0" Max="99.99" Gruppe="Nitrosomonas" Kategorie="Wachstum" />'
  WRITE(200, '(A)') '  <Parameter Ident="ANITR1" Text="Absterberate Nitrosomonas" Unit="1/d" Format="F4.2" Null="-1" Help="Absterberate für Nitrosomonas" Default="0.09" Min="0" Max="9.99" Gruppe="Nitrosomonas" Kategorie="Wachstum" />'
  WRITE(200, '(A)') '  <Parameter Ident="BNMX1" Text="Max. Umsatz Nitrosomonas" Unit="gNH4-N/(m²*l)" Format="F5.2" Null="-1" Help="Max. Umsatzrate sessiler Nitrosomonas" Default="2.4" Min="0" Max="99.99" Gruppe="Nitrosomonas" Kategorie="Wachstum" />'
  WRITE(200, '(A)') '  <Parameter Ident="BNKS1" Text="Halbsätt. sessiler Nitrosomonas" Unit="mg/l" Format="F5.2" Null="-1" Help="Halbsättigungskonstante der sessilen Nitrosomonas" Default="3.7" Min="0" Max="99.99" Gruppe="Nitrosomonas" Kategorie="Wachstum" />'
  WRITE(200, '(A)') '  <Parameter Ident="YNMAX2" Text="Max. Wachstum Nitrobacter" Unit="1/d" Format="F4.2" Null="-1" Help="Max. Wachstumsrate der Nitrobacter" Default="0.33" Min="0" Max="9.99" Gruppe=" Nitrobacter" Kategorie="Wachstum" />'
  WRITE(200, '(A)') '  <Parameter Ident="STKS2" Text="Halbsättigung Nitrobacter" Unit="mgNO2-N/l" Format="F5.2" Null="-1" Help="Halbsättigungskonstante für Nitrobacter" Default="0.35" Min="0" Max="99.99" Gruppe=" Nitrobacter" Kategorie="Wachstum" />'
  WRITE(200, '(A)') '  <Parameter Ident="ANITR2" Text="Absterberate Nitrobacter" Unit="1/d" Format="F4.2" Null="-1" Help="Absterberate für Nitrobacter" Default="0.11" Min="0" Max="9.99" Gruppe=" Nitrobacter" Kategorie="Wachstum" />'
  WRITE(200, '(A)') '  <Parameter Ident="BNMX2" Text="Max. Umsatz Nitrobacter" Unit="gNO2-N/(m2*l)" Format="F5.2" Null="-1" Help="Max. Umsatzrate sessiler Nitrobacter" Default="4.9" Min="0" Max="99.99" Gruppe=" Nitrobacter" Kategorie="Wachstum" />'
  WRITE(200, '(A)') '  <Parameter Ident="BNKS2" Text="Halbsätt. sessiler Nitrobacter" Unit="mg/l" Format="F5.2" Null="-1" Help="Halbsättigungskonstante der sessilen Nitrobacter" Default="1.2" Min="0" Max="99.99" Gruppe=" Nitrobacter" Kategorie="Wachstum" />'
  WRITE(200, '(A)') '  <Parameter Ident="KNH4e" Text="NH4-Umsatzgeschw. im Sediment" Unit="m/d" Format="F5.2" Null="-1" Help="NH4-Umsatzgeschw. im Sediment" Default="0.31" Min="0" Max="99.99" Gruppe="Sediment" Kategorie="N" />'
  WRITE(200, '(A)') '  <Parameter Ident="KapN3e" Text="Denitrifikationsgeschw. im Sediment" Unit="m/d" Format="F5.2" Null="-1" Help="Denitrifikationsgeschw.im Sediment" Default="0.81" Min="0" Max="99.99" Gruppe="Sediment" Kategorie="N" />'
  WRITE(200, '(A)') '  <Parameter Ident="HyP1" Text="Hydrolyserate für die leicht abbaubaren partikulären organischen C-Verbindungen " Unit="d-1" Format="F6.3" Null="-1" Help="Hydrolyserate für die leicht abbaubaren partikulären organischen C-Verbindungen " Default="0.12" Min="0" Max="99.999" Gruppe="Sediment" Kategorie="C Biomasse" />'
  WRITE(200, '(A)') '  <Parameter Ident="hymxDe" Text="Hydrolyserate für die leicht abbaubaren gelösten organischen C-Verbindungen " Unit="d-1" Format="F6.3" Null="-1" Help="Hydrolyserate für die leicht abbaubaren gelösten organischen C-Verbindungen " Default="18" Min="0" Max="99.999" Gruppe="Sediment" Kategorie="C Biomasse" />'
  WRITE(200, '(A)') '  <Parameter Ident="KsD1" Text="Halbsättigungskonstante für die Hydrolyse der leicht abbaubaren gelöster organischer C-Verbindungen" Unit="mgC/l" Format="F6.3" Null="-1" Help="Halbsättigungskonstante für die Hydrolyse der leicht abbaubaren gelöster organischer C-Verbindungen" Default="0.25" Min="0" Max="99.999" Gruppe="Sediment" Kategorie="C Biomasse" />'
  WRITE(200, '(A)') '  <Parameter Ident="KsD2" Text="Halbsättigungskonstante für die Hydrolyse der schwer abbaubaren gelöster organischer C-Verbindungen" Unit="mgC/l" Format="F6.3" Null="-1" Help="Halbsättigungskonstante für die Hydrolyse der schwer abbaubaren gelöster organischer C-Verbindungen" Default="2.5" Min="0" Max="99.999" Gruppe="Sediment" Kategorie="C Biomasse" />'
  WRITE(200, '(A)') '  <Parameter Ident="KsM" Text="Halbsättigungskonst. für den Abbau monomerer C-Verbindungen" Unit="mgC/l" Format="F6.3" Null="-1" Help="Halbsättigungskonst. für den Abbau monomerer C-Verbindungen" Default="0.1" Min="0" Max="99.999" Gruppe="Sediment" Kategorie="C Biomasse" />'
  WRITE(200, '(A)') '  <Parameter Ident="upBAC" Text="max. Aufnahmerate monomerer C-Verbindungen d. Bakterien" Unit="d-1" Format="F6.3" Null="-1" Help="max. Aufnahmerate monomerer C-Verbindungen d. Bakterien bei 25°C (Topt)" Default="4.8" Min="0" Max="99.999" Gruppe="Bakterien" Kategorie="Wachstum" />'
  WRITE(200, '(A)') '  <Parameter Ident="YBAC" Text="Ertragskoeffizient für Bakterienbiomasse" Unit=" -" Format="F6.3" Null="-1" Help="Ertragskoeffizient für Bakterienbiomasse" Default="0.25" Min="0" Max="99.999" Gruppe="Bakterien" Kategorie="Wachstum" />'
  WRITE(200, '(A)') '  <Parameter Ident="rsGBAC" Text="Grundrespiration het. Bakterien" Unit="d-1" Format="F6.3" Null="-1" Help="Grundrespiration het. Bakterien" Default="0.03" Min="0" Max="99.999" Gruppe="Bakterien" Kategorie="Wachstum" />'
  WRITE(200, '(A)') '  <Parameter Ident="FOPTD" Text="Opt. Futterkonz. Dreissena" Unit="mgC/l" Format="F5.2" Null="-1" Help="Optimale Futterkonzentration für Dreissena-Wachstum" Default="1.2" Min="0" Max="99.99" Gruppe="Muscheln" Kategorie="Grazing" />'
  WRITE(200, '(A)') '  <Parameter Ident="UPHNF" Text="max. Aufnahmerate der HNF" Unit="d-1" Format="F5.2" Null="-1" Help="Aufnahmerate heterotropher Nanoflagelaten" Default="1.61" Min="0" Max="99.99" Gruppe="HNF" Kategorie="Grazing" />'
  WRITE(200, '(A)') '  <Parameter Ident="BACKS" Text="Halbsättigungsk. für BaK.-Aufnahme durch HNF" Unit="mgC/l" Format="F6.4" Null="-1" Help="Halbsättigungsk. für BaK.-Aufnahme durch HNF" Default="0.0143" Min="0" Max="9.9999" Gruppe="HNF" Kategorie="Grazing" />'
  WRITE(200, '(A)') '  <Parameter Ident="ALAMDA" Text="Absorptionskoeff. für Gelbstoffe bei 440 nm" Unit="-" Format="F5.3" Null="-1" Help="Absorptionskoeff. für Gelbstoffe bei 440 nm" Default="0.75" Min="0" Max="9.999" Gruppe="Wasser" Kategorie="Licht" />'
  WRITE(200, '(A)') '  <Parameter Ident="FPOC1e" Text="leichtabbaubarer Anteil d. Sedimentkohlenstoffs" Unit=" - " Format="F5.2" Null="-1" Help="leichtabbaubarer Anteil d. Sedimentkohlenstoffs" Default="0.65" Min="0" Max="99.99" Gruppe="Sediment" Kategorie="C Biomasse" />'
  WRITE(200, '(A)') '  <Parameter Ident="FPOC2e" Text="schwerabbaubarer Anteil d. Sedimentkohlenstoffs" Unit=" - " Format="F5.2" Null="-1" Help="schwerabbaubarer Anteil d. Sedimentkohlenstoffs" Default="0.15" Min="0" Max="99.99" Gruppe="Sediment" Kategorie="C Biomasse" />'
  WRITE(200, '(A)') '  <Parameter Ident="SorpCape" Text="SorptionsKapazität für Phosphor" Unit="mgP/gTG" Format="F6.2" Null="-99.99" Help="SorptionsKapazität für Phosphor" Default="28" Min="0.0" Max="99.99" Gruppe="Sediment" Kategorie="P" />'
  WRITE(200, '(A)') '  <Parameter Ident="KLange" Text="Langmuirkoeffizient für Phosphorsorption" Unit="l/mgP" Format="F6.3" Null="-1" Help="Langmuirkoeffizient für Phosphorsorption" Default="0.7" Min="0" Max="99.999" Gruppe="Sediment" Kategorie="P" />'
  WRITE(200, '(A)') '  <Parameter Ident="KdNH3e" Text="Partitionskoeffizient für Ammonium" Unit="l/kg" Format="F5.2" Null="-9.99" Help="-1.-> Wert wird berechnet" Min="-1" Max="99.99" Gruppe="Sediment" Kategorie="N" />'
  WRITE(200, '(A)') '  <Parameter Ident="RateCde" Text="Grundmortalitätsrate coliformer Bakterien bei 20°C" Unit="1/d" Format="F6.3" Null="-1." Help="Grundmortalitätsrate coliformer Bakterien bei 20°C" Min="0.0" Max="10." Gruppe="Hygiene" Kategorie="Coliform" />'
  WRITE(200, '(A)') '  <Parameter Ident="etaCde" Text="Temperaturkoeffizient" Unit="-" Format="F5.2" Null="-1." Help="Temperaturkoeffizient" Min="1." Max="3." Gruppe="Hygiene" Kategorie="Coliform" />'
  WRITE(200, '(A)') '  <Parameter Ident="RateCIe" Text="Inaktivierungskoeffizient im Licht" Unit="m2*MJ-1" Format="F5.2" Null="-1." Help="Inaktivierungskoeffizient im Licht" Min="0.0" Max="99.99" Gruppe="Hygiene" Kategorie="Coliform" />'
  WRITE(200, '(A)') '  <Parameter Ident="xnueCe" Text="dimensionsloser Parameter" Unit="-" Format="F6.2" Null="-1." Help="dimensionsloser Parameter zur Beschreibung der Inaktiv. im Licht" Min="1.0" Max="999.99" Gruppe="Hygiene" Kategorie="Coliform" />'
  WRITE(200, '(A)') '  <Parameter Ident="RateCGe" Text="Verlustrate durch Grazing" Unit="d-1" Format="F5.3" Null="-.1" Help="Coliforme Verlustrate durch Grazing" Min="0.0" Max="9.999" Gruppe="Hygiene" Kategorie="Coliform" />'
  WRITE(200, '(A)') '  <Parameter Ident="RateCSe" Text="Verlustrate durch Sedimentation" Unit="d-1" Format="F5.3" Null="-.1" Help="Coliforme Verlustrate durch Sedimentation" Min="0" Max="9.999" Gruppe="Hygiene" Kategorie="Coliform" />'
  WRITE(200, '(A)') '</ParamSetDef>'
  WRITE(200, '(A)') '</GerrisParam>'
  
  CLOSE(1)

 END subroutine AParamParam
