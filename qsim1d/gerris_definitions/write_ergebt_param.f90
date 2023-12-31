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

!> Write `ErgebTParam.xml`
subroutine write_ergebt_param(cpfad1)

   implicit none
   character(255), intent(in) :: cpfad1
   
   character(275) :: pfadstring
   character(8)   :: versionstext
   integer        :: u_erg
   
   external :: version_string
   
   call version_string(versionstext)
  
   pfadstring = trim(adjustl(cpfad1)) // 'ErgebTParam.xml'
   open(newunit = u_erg, file = pfadstring, encoding='UTF-8')
  
   write(u_erg, '(A)') '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
   write(u_erg, '(3A)') '<GerrisParam FileType="ErgebT" QsimVersion="',versionstext,'">'  
   write(u_erg, '(A)') '<ParamSetDef Ident="ErgebT" Text="Zeitschritt-Ergebnisparameter" Help="Die von QSim in Zeitschritt-Auflösung berechneten Parameter">'
   write(u_erg, '(A)') '  <Parameter Ident="VBSB" Text="C-BSB5" Unit="mg/l" Format="F6.2" Help="C-BSB5" Group="1" Quantity="BSB5" />'
   write(u_erg, '(A)') '  <Parameter Ident="VCSB" Text="CSB" Unit="mg/l" Format="F6.2" Help="CSB" Group="1" Quantity="CSB" />'
   write(u_erg, '(A)') '  <Parameter Ident="VNH4" Text="NH4-N" Unit="mg/l" Format="F6.2" Help="NH4-N" Group="1" Quantity="NH4" />'
   write(u_erg, '(A)') '  <Parameter Ident="VNO2" Text="NO2-N" Unit="mg/l" Format="F6.3" Help="NO2-N" Group="1" Quantity="NO2" />'
   write(u_erg, '(A)') '  <Parameter Ident="VNO3" Text="NO3-N" Unit="mg/l" Format="F9.6" Help="NO3-N" Group="1" Quantity="NO3" />'
   write(u_erg, '(A)') '  <Parameter Ident="GSNY" Text="Gesamt N" Unit="mg/l" Format="F5.2" Help="Gesamt N" Group="1" Quantity="GSNY" />'
   write(u_erg, '(A)') '  <Parameter Ident="GELP" Text="o-PO4-P" Unit="mg/l" Format="F6.3" Help="o-PO4-P" Group="1" Quantity="GELP" />'
   write(u_erg, '(A)') '  <Parameter Ident="GSPY" Text="Gesamt P" Unit="mg/l" Format="F5.2" Help="Gesamt P" Group="1" Quantity="GSPY" />'
   write(u_erg, '(A)') '  <Parameter Ident="SI" Text="Gelöstes Si" Unit="mg/l" Format="F5.2" Help="Gelöstes Si" Group="1" Quantity="SI" />'
   write(u_erg, '(A)') '  <Parameter Ident="CHLA" Text="Chlorophyll-a-Gehalt" Unit="µg/l" Format="F6.2" Help="Chlorophyll-a-Gehalt" Group="1" Quantity="CHLA" />'
   write(u_erg, '(A)') '  <Parameter Ident="ZOOIND" Text="Zooplanktondichte" Unit="Ind/l" Format="F7.1" Help="Zooplanktondichte" Group="1" Quantity="ZOOIND" />'
   write(u_erg, '(A)') '  <Parameter Ident="VPH" Text="pH-Wert" Unit="" Format="F5.2" Help="pH-Wert" Group="1" Quantity="PH" />'
   write(u_erg, '(A)') '  <Parameter Ident="MW" Text="m-Wert" Unit="mmol/l" Format="F5.2" Help="m-Wert" Group="1" Quantity="MW" />'
   write(u_erg, '(A)') '  <Parameter Ident="CA" Text="Ca-Konzentration" Unit="mg/l" Format="F5.1" Help="Ca-Konzentration" Group="1" Quantity="CA" />'
   write(u_erg, '(A)') '  <Parameter Ident="LF" Text="Leitfähigkeit" Unit="µS/cm" Format="F8.1" Help="Leitfähigkeit" Group="1" Quantity="LF" />'
   write(u_erg, '(A)') '  <Parameter Ident="SSALG" Text="Schwebstoffgehalt" Unit="mg/l" Format="F6.2" Help="Schwebstoffgehalt" Group="1" Quantity="SSALG" />'
   write(u_erg, '(A)') '  <Parameter Ident="TEMPW" Text="Wassertemperatur" Unit="°C" Format="F5.2" Help="Wassertemperatur" Group="1" Quantity="TEMPW" />'
   write(u_erg, '(A)') '  <Parameter Ident="VO2" Text="Sauerstoffgehalt" Unit="mg/l" Format="F5.2" Help="Sauerstoffgehalt" Group="1" Quantity="O2" />'
   write(u_erg, '(A)') '  <Parameter Ident="CHNF" Text="Heterotrophe Nanoflagellaten" Unit="Ind/ml" Format="F8.1" Help="Heterotrophe Nanoflagellaten" Group="1" Quantity="CHNF" />'
   write(u_erg, '(A)') '  <Parameter Ident="COLIY" Text="Fäkalcoliforme Bakterien" Unit="Ind/100 ml" Format="E9.2" Help="Fäkalcoliforme Bakterien" Group="1" Quantity="COLIY" />'
   write(u_erg, '(A)') '  <Parameter Ident="DLY" Text="longitudinale Dispersion" Unit="m/s2" Format="F7.2" Help="longitudinale Dispersion" Group="1" Quantity="DLY" />'
   write(u_erg, '(A)') '  <Parameter Ident="SEDHG" Text="Sedimentdicke" Unit="mm" Format="F12.6" Help="Sedimentdicke" Group="1" Quantity="SEDHG" />'
   write(u_erg, '(A)') '  <Parameter Ident="TRACER" Text="Tracerkonzentration" Unit="µg/l" Format="F9.3" Help="" Group="1" Quantity="TRACER" />'
   write(u_erg, '(A)') '  <Parameter Ident="BSBtY" Text="Sauerstoffverbrauch d. Kohlenstoffabbau in der Wassersäule" Unit="mgO2/(l*h)" Format="F8.6" Help="" Group="Raten (O2)" Quantity="" />'
   write(u_erg, '(A)') '  <Parameter Ident="susNOy" Text="Sauerstoffverbr. d. Nitrifik. in der Wassersäule" Unit="mgO2/(l*h)" Format="F8.6" Help="" Group="Raten (O2)" Quantity="" />'
   write(u_erg, '(A)') '  <Parameter Ident="O2ei1y" Text="physikalischer Sauerstoffeintrag" Unit="mgO2/(l*h)" Format="F8.6" Help="" Group="Raten (O2)" Quantity="" />'
   write(u_erg, '(A)') '  <Parameter Ident="dalgoy" Text="BruttoO2-Prod. d. Algen" Unit="mgO2/(l*h)" Format="F8.6" Help="" Group="Raten (O2)" Quantity="" />'
   write(u_erg, '(A)') '  <Parameter Ident="cchlky" Text="C:Chla-Verhältnis Kieselalgen" Unit="mg/mg" Format="F6.2" Help="" Group="1" Quantity="cchlky" />'
   write(u_erg, '(A)') '  <Parameter Ident="cchlkg" Text="C:Chla-Verhältnis Grünalgen" Unit="mg/mg" Format="F6.2" Help="" Group="1" Quantity="cchlgy" />'
   write(u_erg, '(A)') '  <Parameter Ident="cchlkb" Text="C:Chla-Verhältnis Blaualgen" Unit="mg/mg" Format="F6.2" Help="" Group="1" Quantity="cchlby" />'
   write(u_erg, '(A)') '  <Parameter Ident="zoro2y" Text="O2-Verbr. durch Rotatorien" Unit="mgO2/(l*h)" Format="F8.6" Help="" Group="Raten (O2)" Quantity="" />'
   write(u_erg, '(A)') '  <Parameter Ident="schlry" Text="O2-Flux in das Sediment" Unit="mgO2/(l*h)" Format="F10.8" Help="" Group="Raten (O2)" Quantity="" />'
   write(u_erg, '(A)') '  <Parameter Ident="bettny" Text="AmmoniumFlux Wasser/Sediment (-)" Unit="mgN/(l*h)" Format="F8.6" Help="" Min="0" Group="Raten (N)" Quantity="" />'
   write(u_erg, '(A)') '  <Parameter Ident="BVBSBY" Text="C-BSB5 im Buhnenfeld" Unit="mg/l" Format="F6.2" Help="C-BSB5 im Buhnenfeld" Group="2" Quantity="BBSB5Y" />'
   write(u_erg, '(A)') '  <Parameter Ident="BVCSBY" Text="CSB im Buhnenfeld" Unit="mg/l" Format="F6.2" Help="CSB im Buhnenfeld" Group="2" Quantity="BCSBY" />'
   write(u_erg, '(A)') '  <Parameter Ident="BNH4Y" Text="NH4-N im Buhnenfeld" Unit="mg/l" Format="F6.2" Help="NH4-N im Buhnenfeld" Group="2" Quantity="BNH4Y" />'
   write(u_erg, '(A)') '  <Parameter Ident="BNO2Y" Text="NO2-N im Buhnenfeld" Unit="mg/l" Format="F6.3" Help="NO2-N im Buhnenfeld" Group="2" Quantity="BNO2Y" />'
   write(u_erg, '(A)') '  <Parameter Ident="BNO3Y" Text="NO3-N im Buhnenfeld" Unit="mg/l" Format="F9.6" Help="NO3-N im Buhnenfeld" Group="2" Quantity="BNO3Y" />'
   write(u_erg, '(A)') '  <Parameter Ident="BGSNY" Text="Gesamt N im Buhnenfeld" Unit="mg/l" Format="F5.2" Help="Gesamt N im Buhnenfeld" Group="2" Quantity="BGSNY" />'
   write(u_erg, '(A)') '  <Parameter Ident="BGELPY" Text="o-PO4-P im Buhnenfeld" Unit="mg/l" Format="F5.3" Help="o-PO4-P im Buhnenfeld" Group="2" Quantity="BGELPY" />'
   write(u_erg, '(A)') '  <Parameter Ident="BGSPY" Text="Gesamt P im Buhnenfeld" Unit="mg/l" Format="F5.2" Help="Gesamt P im Buhnenfeld" Group="2" Quantity="BGSPY" />'
   write(u_erg, '(A)') '  <Parameter Ident="BSIY" Text="Gelöstes Si im Buhnenfeld" Unit="mg/l" Format="F5.2" Help="Gelöstes Si im Buhnenfeld" Group="2" Quantity="BSIY" />'
   write(u_erg, '(A)') '  <Parameter Ident="BCHLAY" Text="Chlorophyll-a-Gehalt im Buhnenfeld" Unit="µg/l" Format="F6.2" Help="Chlorophyll-a-Gehalt im Buhnenfeld" Group="2" Quantity="BCHLAY" />'
   write(u_erg, '(A)') '  <Parameter Ident="BZOOIY" Text="Zooplanktondichte im Buhnenfeld" Unit="Ind/l" Format="F7.1" Help="Zooplanktondichte im Buhnenfeld" Group="2" Quantity="BZOOIY" />'
   write(u_erg, '(A)') '  <Parameter Ident="BPHY" Text="pH-Wert im Buhnenfeld" Unit="" Format="F5.2" Help="pH-Wert im Buhnenfeld" Group="2" Quantity="BPHY" />'
   write(u_erg, '(A)') '  <Parameter Ident="BMWY" Text="m-Wert im Buhnenfeld" Unit="mmol/l" Format="F5.2" Help="m-Wert im Buhnenfeld" Group="2" Quantity="BMWY" />'
   write(u_erg, '(A)') '  <Parameter Ident="BCAY" Text="Ca-Konzentration im Buhnenfeld" Unit="mg/l" Format="F5.1" Help="Ca-Konzentration im Buhnenfeld" Group="2" Quantity="BCAY" />'
   write(u_erg, '(A)') '  <Parameter Ident="BLFY" Text="Leitfähigkeit im Buhnenfeld" Unit="µS/cm" Format="F6.1" Help="Leitfähigkeit im Buhnenfeld" Group="2" Quantity="BLFY" />'
   write(u_erg, '(A)') '  <Parameter Ident="BSSALY" Text="Schwebstoffgehalt im Buhnenfeld" Unit="mg/l" Format="F6.2" Help="Schwebstoffgehalt im Buhnenfeld" Group="2" Quantity="BSSALY" />'
   write(u_erg, '(A)') '  <Parameter Ident="BTEMPY" Text="Wassertemperatur im Buhnenfeld" Unit="°C" Format="F5.2" Help="Wassertemperatur im Buhnenfeld" Group="2" Quantity="BTEMPY" />'
   write(u_erg, '(A)') '  <Parameter Ident="BO2Y" Text="Sauerstoffgehalt im Buhnenfeld" Unit="mg/l" Format="F5.2" Help="Sauerstoffgehalt im Buhnenfeld" Group="2" Quantity="BO2Y" />'
   write(u_erg, '(A)') '  <Parameter Ident="BHNFY" Text="Heterotrophe Nanoflagellaten im Buhnenfeld" Unit="Ind/ml" Format="F8.1" Help="Heterotrophe Nanoflagellaten im Buhnenfeld" Group="2" Quantity="BHNFY" />'
   write(u_erg, '(A)') '  <Parameter Ident="BCOLIY" Text="Fäkalcoliforme Bakterien im Buhnenfeld" Unit="Ind/100 ml" Format="F8.0" Help="Fäkalcoliforme Bakterien im Buhnenfeld" Group="2" Quantity="BCOLIY" />'
   write(u_erg, '(A)') '  <Parameter Ident="TAU2" Text="Austauschzeit" Unit="h" Format="F7.3" Help="" Group="2" Quantity="TAU2" />'
   write(u_erg, '(A)') '  <Parameter Ident="bTRACER" Text="" Unit="µg/l" Format="F9.3" Help="" Group="2" Quantity="BTRACER" />'
   write(u_erg, '(A)') '</ParamSetDef>'
   write(u_erg, '(A)') '</GerrisParam>'
   
   close(u_erg)
end subroutine write_ergebt_param

