 subroutine E_extnctParam(cpfad1,j1)

!  Ausgabe der Definition von E_extnct

  character(255)  :: cpfad1
  character (len=275)         :: pfadstring

  write(pfadstring,'(2A)')trim(adjustl(cpfad1(1:j1))),'E_extnctParam.xml' 
  open(unit=1, file=pfadstring)

  WRITE(1, '(A)') '<?xml version="1.0" encoding="ISO-8859-1" standalone="yes"?>'
  WRITE(1, '(A)') '<GerrisParam FileType="E_extnct" QsimVersion="13.10">'  
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
