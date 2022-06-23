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

!> Write file `EreigHParam.xml`
 subroutine EreigHParam(cpfad1)
   implicit none
   character(255), intent(in)  :: cpfad1
   
   character (len=275)         :: pfadstring
   character (len = 8)         :: versionstext
   
   call version_string(versionstext)
   
   pfadstring =  trim(adjustl(cpfad1)) // 'EreigHParam.xml' 
   open(unit=1, file=pfadstring, encoding='UTF-8')
  
   write(1, '(A)') '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
   write(1, '(3A)') '<GerrisParam FileType="EreigH" QsimVersion="',versionstext,'">' 
   write(1, '(A)') '<ParamSetDef Ident="EreigH" Text="Hydraulische Ereignisdaten" Help="Die von Qsim verwendeten hydraulischen Ereignisdaten">'
   write(1, '(A)') '  <Parameter Ident="W" Text="W" Unit="m ü.NN" Format="F9.4" Help="Wasserstand" Quantity="W" />'
   write(1, '(A)') '  <Parameter Ident="Q" Text="Q" Unit="m3/s" Format="F13.6" Help="Durchfluss" Quantity="Q" />'
   write(1, '(A)') '  <Parameter Ident="GESCHW" Text="v" Unit="m/s" Format="F8.5" Help="Fließgeschwindigkeit" Quantity="GESCHW" />'
   write(1, '(A)') '  <Parameter Ident="A" Text="A" Unit="m2" Format="F7.1" Help="Fläche" Quantity="A" />'
   write(1, '(A)') '  <Parameter Ident="VOL" Text="Volumen" Unit="m3" Format="F8.0" Help="Wasservolumen" Quantity="VOL" />'
   write(1, '(A)') '  <Parameter Ident="TMITTL" Text="Tm" Unit="m" Format="F7.4" Help="Mittlere Tiefe" Quantity="TMITTL" />'
   write(1, '(A)') '  <Parameter Ident="RHYD" Text="r-hyd" Unit="m" Format="F7.3" Help="Hydraulischer Radius" Quantity="RHYD" />'
   write(1, '(A)') '  <Parameter Ident="BLNGE" Text="Boeschung" Unit="m" Format="F7.2" Help="Böschungslänge" Quantity="BLNGE" />'
   write(1, '(A)') '  <Parameter Ident="BUADIFF" Text="Bu.A.diff." Unit="m2" Format="F7.1" Help="Buhnenschatten-Fläche" Quantity="BUADIFF" />'
   write(1, '(A)') '  <Parameter Ident="BUWFL" Text="Bu.wuchsfl." Unit="m2" Format="F7.1" Help="Buhnen-Aufwuchsfläche" Quantity="BUWFL" />'
   write(1, '(A)') '  <Parameter Ident="BUTMITTL" Text="Bu.Tm" Unit="m" Format="F7.4" Help="Mittlere Tiefe im Buhnenfeld" Quantity="BUTMITTL" />'
   write(1, '(A)') '  <Parameter Ident="BUBLNGE" Text="Bu.Boeschung" Unit="m" Format="F6.2" Help="Böschungslänge im Buhnenfeld" Quantity="BUBLNGE" />'
   write(1, '(A)') '  <Parameter Ident="BUSOHLE" Text="Bu.sohle" Unit="m" Format="F6.2" Help="Sohlbreite im Buhnenfeld" Quantity="BUSOHLE" />'
   write(1, '(A)') '  <Parameter Ident="BUGESCHW" Text="Bu.v" Unit="m/s" Format="F8.5" Help="Fließgeschwindigkeit im Buhnenfeld" Quantity="BUGESCHW" />'
   write(1, '(A)') '  <Parameter Ident="BULIAKTV" Text="L.Bu.aktiv" Unit="" Format="I2" Help="Linke-Buhne-benetzt-Indikator" Quantity="BULIAKTV" />'
   write(1, '(A)') '  <Parameter Ident="BUREAKTV" Text="R.Bu.aktiv" Unit="" Format="I2" Help="Rechte-Buhne-benetzt-Indikator" Quantity="BUREAKTV" />'
   write(1, '(A)') '</ParamSetDef>'
   write(1, '(A)') '</GerrisParam>'
   close(1)
end subroutine EreigHParam

