 subroutine Ergeb2DParam(cpfad1,j1)

!  Ausgabe der Definition von Ergeb2D

   character(255) :: cpfad1
   character (len=275)         :: pfadstring

  write(pfadstring,'(2A)')trim(adjustl(cpfad1(1:j1))),'Ergeb2DParam.xml'
  open(unit=1, file=pfadstring)
  
  WRITE(1, '(A)') '<?xml version="1.0" encoding="ISO-8859-1" standalone="yes"?>'
  WRITE(1, '(A)') '<GerrisParam FileType="Ergeb2D" QsimVersion="13.10">'  
  WRITE(1, '(A)') '<ParamSetDef Ident="Ergeb2D" Text="2D-Zeitschritt-Ergebnisparameter" Help="Die von QSim in 2D- und Zeitschritt-Auflösung berechneten Parameter">'
  WRITE(1, '(A)') '  <Parameter Ident="VNH4" Text="NH4-N" Unit="mg/l" Format="F6.2" Help="..." Group="1" Quantity="NH4" />'
  WRITE(1, '(A)') '  <Parameter Ident="VNO2" Text="NO2-N" Unit="mg/l" Format="F5.3" Help="..." Group="1" Quantity="NO2" />'
  WRITE(1, '(A)') '  <Parameter Ident="VNO3" Text="NO3-N" Unit="mg/l" Format="F9.6" Help="NO3-N" Group="1" Quantity="NO3" />'
  WRITE(1, '(A)') '  <Parameter Ident="GELP" Text="o-PO4-P" Unit="mg/l" Format="F5.3" Help="o-PO4-P" Group="1" Quantity="GELP" />'
  WRITE(1, '(A)') '  <Parameter Ident="SI" Text="Gelöstes Si" Unit="mg/l" Format="F5.2" Help="Gelöstes Si" Group="1" Quantity="SI" />'
  WRITE(1, '(A)') '  <Parameter Ident="TEMPW" Text="Wassertemperatur" Unit="°C" Format="F5.2" Help="Wassertemperatur" Group="1" Quantity="TEMPW" />'
  WRITE(1, '(A)') '  <Parameter Ident="VO2" Text="Sauerstoffgehalt" Unit="mg/l" Format="F5.2" Help="Sauerstoffgehalt" Group="1" Quantity="O2" />'
  WRITE(1, '(A)') '  <Parameter Ident="CHLA" Text="Chlorophyll" Unit="µg/l" Format="F6.2" Help="Chlorophyll-a-Gehalt" Group="1" Quantity="CHLA" />'
  WRITE(1, '(A)') '  <Parameter Ident="CCHLKI" Text="C:Chlorophyll-a der Kieselalgen" Unit="mg/mg" Format="F6.2" Help="C:Chlorophyll" Group="1" Quantity="CHLA" />'
  WRITE(1, '(A)') '  <Parameter Ident="CCHLBL" Text="C:Chlorophyll-a der Blaualgen" Unit="mg/mg" Format="F6.2" Help="C:Chlorophyll" Group="1" Quantity="CHLA" />'
  WRITE(1, '(A)') '  <Parameter Ident="CCHLGR" Text="C:Chlorophyll-a der Grünalgen" Unit="mg/mg" Format="F6.2" Help="C:Chlorophyll" Group="1" Quantity="CHLA" />'
  WRITE(1, '(A)') '  <Parameter Ident="gesP" Text="gesamt Phosphor" Unit="mg/l" Format="F6.3" Help="gesamt Phosphor" Group="1" Quantity="gesP" />'
  WRITE(1, '(A)') '  <Parameter Ident="gesN" Text="gesamt Stickstoff" Unit="mg/l" Format="F6.3" Help="gesamt Stickstoff" Group="1" Quantity="gesN" />'
  WRITE(1, '(A)') '</ParamSetDef>'
  WRITE(1, '(A)') '</GerrisParam>'
  
  CLOSE(1)

 END subroutine Ergeb2DParam

