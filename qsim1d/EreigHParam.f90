 subroutine EreigHParam(cpfad1,j1)

!  Ausgabe der Definition von EreigH

  character(255) :: cpfad1
  character (len=275)         :: pfadstring

  write(pfadstring,'(2A)')trim(adjustl(cpfad1(1:j1))),'EreigHParam.xml' 
  open(unit=1, file=pfadstring)
  
  WRITE(1, '(A)') '<?xml version="1.0" encoding="ISO-8859-1" standalone="yes"?>'
  WRITE(1, '(A)') '<GerrisParam FileType="EreigH" QsimVersion="13.10">'  
  WRITE(1, '(A)') '<ParamSetDef Ident="EreigH" Text="Hydraulische Ereignisdaten" Help="Die von Qsim verwendeten hydraulischen Ereignisdaten">'
  WRITE(1, '(A)') '  <Parameter Ident="W" Text="W" Unit="m ü.NN" Format="F9.4" Help="Wasserstand" Quantity="W" />'
  WRITE(1, '(A)') '  <Parameter Ident="Q" Text="Q" Unit="m3/s" Format="F13.6" Help="Durchfluss" Quantity="Q" />'
  WRITE(1, '(A)') '  <Parameter Ident="GESCHW" Text="v" Unit="m/s" Format="F8.5" Help="Fließgeschwindigkeit" Quantity="GESCHW" />'
  WRITE(1, '(A)') '  <Parameter Ident="A" Text="A" Unit="m2" Format="F7.1" Help="Fläche" Quantity="A" />'
  WRITE(1, '(A)') '  <Parameter Ident="VOL" Text="Volumen" Unit="m3" Format="F8.0" Help="Wasservolumen" Quantity="VOL" />'
  WRITE(1, '(A)') '  <Parameter Ident="TMITTL" Text="Tm" Unit="m" Format="F7.4" Help="Mittlere Tiefe" Quantity="TMITTL" />'
  WRITE(1, '(A)') '  <Parameter Ident="RHYD" Text="r-hyd" Unit="m" Format="F7.3" Help="Hydraulischer Radius" Quantity="RHYD" />'
  WRITE(1, '(A)') '  <Parameter Ident="BLNGE" Text="Boeschung" Unit="m" Format="F7.2" Help="Böschungslänge" Quantity="BLNGE" />'
  WRITE(1, '(A)') '  <Parameter Ident="BUADIFF" Text="Bu.A.diff." Unit="m2" Format="F7.1" Help="Buhnenschatten-Fläche" Quantity="BUADIFF" />'
  WRITE(1, '(A)') '  <Parameter Ident="BUWFL" Text="Bu.wuchsfl." Unit="m2" Format="F7.1" Help="Buhnen-Aufwuchsfläche" Quantity="BUWFL" />'
  WRITE(1, '(A)') '  <Parameter Ident="BUTMITTL" Text="Bu.Tm" Unit="m" Format="F7.4" Help="Mittlere Tiefe im Buhnenfeld" Quantity="BUTMITTL" />'
  WRITE(1, '(A)') '  <Parameter Ident="BUBLNGE" Text="Bu.Boeschung" Unit="m" Format="F6.2" Help="Böschungslänge im Buhnenfeld" Quantity="BUBLNGE" />'
  WRITE(1, '(A)') '  <Parameter Ident="BUSOHLE" Text="Bu.sohle" Unit="m" Format="F6.2" Help="Sohlbreite im Buhnenfeld" Quantity="BUSOHLE" />'
  WRITE(1, '(A)') '  <Parameter Ident="BUGESCHW" Text="Bu.v" Unit="m/s" Format="F8.5" Help="Fließgeschwindigkeit im Buhnenfeld" Quantity="BUGESCHW" />'
  WRITE(1, '(A)') '  <Parameter Ident="BULIAKTV" Text="L.Bu.aktiv" Unit="" Format="I2" Help="Linke-Buhne-benetzt-Indikator" Quantity="BULIAKTV" />'
  WRITE(1, '(A)') '  <Parameter Ident="BUREAKTV" Text="R.Bu.aktiv" Unit="" Format="I2" Help="Rechte-Buhne-benetzt-Indikator" Quantity="BUREAKTV" />'
  WRITE(1, '(A)') '</ParamSetDef>'
  WRITE(1, '(A)') '</GerrisParam>'
  
  CLOSE(1)

 END subroutine EreigHParam

