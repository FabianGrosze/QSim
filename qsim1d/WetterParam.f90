 subroutine WetterParam(cpfad1,j1)

!  Ausgabe der Definition von Wetter

  character(255)  :: cpfad1
  character (len=275)         :: pfadstring 

  write(pfadstring,'(2A)')trim(adjustl(cpfad1(1:j1))),'WetterParam.xml' 
  open(unit=1, file=pfadstring)

  
  WRITE(1, '(A)') '<?xml version="1.0" encoding="ISO-8859-1" standalone="yes"?>'
  WRITE(1, '(A)') '<GerrisParam FileType="Wetter" QsimVersion="13.10">'  
  WRITE(1, '(A)') '<ParamSetDef Ident="Wetter" Text="Wetterdaten" Help="Parameter des Wetters">'
  WRITE(1, '(A)') '  <Parameter Ident="GStrahl" Text="Globalstrahlung" Help="Globalstrahlung im Messzeitraum" Unit="J/(cm²*d)" Format="F7.2" Null="-1" Default="" Min="" Max="" />'
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
