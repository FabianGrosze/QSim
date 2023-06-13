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

!> Write file `ModellGParam.xml`
!!
!! This file is used to organise the GUI Gerris.
subroutine write_modellg_param(cpfad1)
   implicit none
   character(255), intent(in) :: cpfad1
   
   character(275) :: filepath
   character(8)   :: versionstext
   integer        :: u_xml
   
   external       :: version_string
   
   
   call version_string(versionstext)
   
   filepath =  trim(adjustl(cpfad1)) // 'ModellGParam.xml'
   open(newunit = u_xml, file = filepath, encoding='UTF-8')
  
   write(u_xml, '(A)') '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
   write(u_xml, '(3A)') '<GerrisParam FileType="ModellG" QsimVersion="',versionstext,'">'  
   
   ! Wehr
   write(u_xml, '(A)') '<ParamSetDef Id="QW" Text="Wehr" Help="Parameter des Wehres" Scope="Strang">'
   write(u_xml, '(A)') '  <Parameter Ident="IstAktiv" Text="Aktiv" Unit="" Format="I1" Null="0" Help="Wehr nicht berücksichtigen: 0; Wehr berücksichtigen: 1" Min="" Max="" Default="0" />'
   write(u_xml, '(A)') '  <Parameter Ident="Breite" Text="Breite" Unit="m" Format="F7.2" Null="-1" Help="Breite des Wehres >0 : Wehrbelüftung" Min="0" Max="9999.999" Default="" />'
   write(u_xml, '(A)') '  <Parameter Ident="Hoehe" Text="Höhe" Unit="m" Format="F7.2" Null="-1" Help="Falls Breite >0 Wehrhöhe aus Wasserspiegeldifferenz OW/UW berechnen: -1; ansonsten Höhe >0" Min="0" Max="999.999" Default="" />'
   write(u_xml, '(A)') '</ParamSetDef>'
   
   ! Schiffsverkehr
   write(u_xml, '(A)') '<ParamSetDef Id="QF" Text="Schiffsverkehr" Help="Schiffsverkehr auf den Gewässerabschnitten" Scope="Abschnitt">'
   write(u_xml, '(A)') '  <Parameter Ident="VSCHIFF" Text="Schiffsgeschwindigkeit" Unit="m/s" Format="F5.2" Null="-1" Help="" Min="0" Max="99.99" Default="1.5" />'
   write(u_xml, '(A)') '  <Parameter Ident="UPROP" Text="Drehzahl des Propellers" Unit="U/s" Format="F5.2" Null="-1" Help="" Min="0" Max="99.99" Default="3.33" />'
   write(u_xml, '(A)') '</ParamSetDef>'
   
   ! Dreissena
   write(u_xml, '(A)') '<ParamSetDef Id="QD" Text="Dreissena: Biomasse" Help="Dreissena-Bewuchs in den Gewässer-Abschnitten" Scope="Abschnitt">'
   write(u_xml, '(A)') '  <Parameter Ident="MBoesch0" Text="Biomasse 0.Kohorte Böschung" Unit="g/m²" Format="F7.2" Null="-1" Help="Dreissena-Biomasse der 0. Kohorte (Schalenlänge kl. 8 mm) im Abschnitt an der Böschung" Min="" Max="" Default="" />'
   write(u_xml, '(A)') '  <Parameter Ident="MSohle0" Text="Biomasse 0.Kohorte Sohle" Unit="g/m²" Format="F7.2" Null="-1" Help="Dreissena-Biomasse der 0. Kohorte im Abschnitt an der Sohle" Min="" Max="" Default="" />'
   write(u_xml, '(A)') '  <Parameter Ident="Gewicht0" Text="Mittl. Muschelgewicht 0.Kohorte" Unit="mgC/Ind" Format="F7.3" Null="-1" Help="Gewicht einer Muschel als Mittelwert der 0. Kohorte" Min="" Max="" Default="" />'
   write(u_xml, '(A)') '  <Parameter Ident="MBoesch1" Text="Biomasse 1.Kohorte Böschung" Unit="g/m²" Format="F7.2" Null="-1" Help="Dreissena-Biomasse der 1. Kohorte (Schalenlänge gr.= 8 mm) im Abschnitt an der Böschung" Min="" Max="" Default="" />'
   write(u_xml, '(A)') '  <Parameter Ident="MSohle1" Text="Biomasse 1.Kohorte Sohle" Unit="g/m²" Format="F7.2" Null="-1" Help="Dreissena-Biomasse der 1. Kohorte im Abschnitt an der Sohle" Min="" Max="" Default="" />'
   write(u_xml, '(A)') '  <Parameter Ident="Gewicht1" Text="Mittl. Muschelgewicht 1.Kohorte" Unit="mgC/Ind" Format="F7.3" Null="-1" Help="Gewicht einer Muschel als Mittelwert der 1. Kohorte." Min="" Max="" Default="" />'
   write(u_xml, '(A)') '</ParamSetDef>'
   
   ! Laichperiode
   write(u_xml, '(A)') '<ParamSetDef Ident="QL" Text="Dreissena: Laichperiode" Help="Laichperiode" Scope="Strang">'
   write(u_xml, '(A)') '  <Parameter Ident="StartTag" Text="Starttag" Unit="" Format="I2" Null="-1" Help="Tag des Beginns der Laichperiode" Min="1" Max="31" Default="" />'
   write(u_xml, '(A)') '  <Parameter Ident="StartMonat" Text="Startmonat" Unit="" Format="I2" Null="-1" Help="Monat des Beginns der Laichperiode" Min="1" Max="12" Default="" />'
   write(u_xml, '(A)') '  <Parameter Ident="Dauer" Text="Dauer [d]" Unit="d" Format="I2" Null="-1" Help="Dauer der Laichperiode in Tagen" Min="0" Max="99" Default="" />'
   write(u_xml, '(A)') '</ParamSetDef>'
   
   ! Wetterstation
   write(u_xml, '(A)') '<ParamSetDef Id="QT" Text="Wetterstation" Help="Wetterstations-Zuordnung" Scope="Abschnitt">'
   write(u_xml, '(A)') '  <Parameter Ident="WStation" Text="Wetterstation" Unit="" Format="I4" Null="-1" Help="Zugehörige Wetterstation" Min="" Max="" Default="" />'
   write(u_xml, '(A)') '  <Parameter Ident="WLage" Text="Lage" Unit="mNHN" Format="F7.1" Null="-1" Help="Lage der Wetterstation" Min="" Max="" Default="" />'
   write(u_xml, '(A)') '</ParamSetDef>'
   
   ! Buhnen
   write(u_xml, '(A)') '<ParamSetDef Id="QU" Text="Buhnen" Help="Parameter für Buhnenfelder" Scope="Abschnitt">'
   write(u_xml, '(A)') '  <Parameter Ident="DLB" Text="Long. Disp.koeff." Unit="m²/s" Format="F7.2" Null="-1" Help="Longitudinaler Dispersionskoeffizient" Min="" Max="" Default="" />'
   write(u_xml, '(A)') '  <Parameter Ident="TAU2B" Text="max. Austauschzeit" Unit="h" Format="F7.2" Null="-1" Help="Austauschzeit zwischen Buhnen und Hauptstrom" Min="" Max="" Default="" />'
   write(u_xml, '(A)') '  <Parameter Ident="ALPHAB" Text="Alpha" Unit="-" Format="F6.2" Null="-1" Help="Exponent beim -Breite/Tiefe-Term- zur Berechnung der longitudinalen Dispersion (nicht bei Elder) Deng: 1.67; Li: 1.3; Iwasa: 1.5" Min="" Max="" Default="" />'
   write(u_xml, '(A)') '  <Parameter Ident="POMsedb" Text="Anteil organisches Material" Unit="%" Format="F6.2" Null="-1" Help="Anteil des organischen Materials im Sediment" Min="" Max="" Default="-1." />'
   write(u_xml, '(A)') '</ParamSetDef>'
   
   ! Vegetation
   write(u_xml, '(A)') '<ParamSetDef Id="QO" Text="Anteil der Vegetationstypen" Help="Anteil der Vegetationstypen" Scope="Abschnitt">'
   write(u_xml, '(A)') '  <Parameter Ident="VTYP1" Text="Niedervegetation (links)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />'
   write(u_xml, '(A)') '  <Parameter Ident="VTYP2" Text="Buschwerk (links)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />'
   write(u_xml, '(A)') '  <Parameter Ident="VTYP3" Text="Weichholzaue (links)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />'
   write(u_xml, '(A)') '  <Parameter Ident="VTYP4" Text="Laubwald (links)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />'
   write(u_xml, '(A)') '  <Parameter Ident="VTYP5" Text="Nadelwald (links)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />'
   write(u_xml, '(A)') '  <Parameter Ident="VTYP6" Text="Bebauung (links)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />'
   write(u_xml, '(A)') '  <Parameter Ident="VALTBL" Text="Höhe Bebauung (links)" Unit="m" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />'
   write(u_xml, '(A)') '  <Parameter Ident="EDUFBL" Text="Uferabstand Bebauung (links)" Unit="m" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />'
   write(u_xml, '(A)') '  <Parameter Ident="VTYP7" Text="Niedervegetation (rechts)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />'
   write(u_xml, '(A)') '  <Parameter Ident="VTYP8" Text="Buschwerk (rechts)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />'
   write(u_xml, '(A)') '  <Parameter Ident="VTYP9" Text="Weichholzaue (rechts)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />'
   write(u_xml, '(A)') '  <Parameter Ident="VTYP10" Text="Laubwald (rechts)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />'
   write(u_xml, '(A)') '  <Parameter Ident="VTYP11" Text="Nadelwald (rechts)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />'
   write(u_xml, '(A)') '  <Parameter Ident="VTYP12" Text="Bebauung (rechts)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />'
   write(u_xml, '(A)') '  <Parameter Ident="VALTBR" Text="Höhe Bebauung (rechts)" Unit="m" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />'
   write(u_xml, '(A)') '  <Parameter Ident="EDUFBR" Text="Uferabstand Bebaung (rechts)" Unit="m" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />'
   write(u_xml, '(A)') '  <Parameter Ident="VTYP13" Text="Laubwald (beidseitig)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />'
   write(u_xml, '(A)') '  <Parameter Ident="VTYP14" Text="Nadelwald (beidseitig)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />'
   write(u_xml, '(A)') '</ParamSetDef>'
   
   ! Kenngrößen Temperatur und Sediment
   write(u_xml, '(A)') '<ParamSetDef Id="QS" Text="Kenngrössen für Sedimenttemperatur" Help="Kenngrößen für die Gewässerabschnitten" Scope="Abschnitt">'
   write(u_xml, '(A)') '  <Parameter Ident="SPEWKS" Text="Spezifische Wärmekapazität Sediment" Unit="KJ/(kg*K)" Format="F6.2" Null="-1" Help="Ton: 0.83; Sand: 0.88" Min="0.8" Max="4.5" Default="-1" />'
   write(u_xml, '(A)') '  <Parameter Ident="WUEBK" Text="Wärmeübergangskoeffizient" Unit="KJ/(K*m2*h)" Format="F7.2" Null="-1" Help="" Min="0" Max="1000" Default="-1." />'
   write(u_xml, '(A)') '  <Parameter Ident="PSREFS" Text="Reflektionsanteil der Strahlung an der Sedimentoberfläche" Unit="-" Format="F5.2" Null="-1" Help="" Min="0" Max="1" Default="-1." />'
   write(u_xml, '(A)') '  <Parameter Ident="EXTKS" Text="Extinktionskoeffizient für PAR" Unit="-" Format="F5.2" Null="-1" Help="Nur bei Temperaturmodellierung erforderlich" Min="" Max="" Default="-1." />'
   write(u_xml, '(A)') '</ParamSetDef>'
   
   ! Erosion
   write(u_xml, '(A)') '<ParamSetDef Id="QE" Text="Erosions-Parameter" Help="Kenngrößen für die Gewässerabschnitte" Scope="Abschnitt">'
   write(u_xml, '(A)') '  <Parameter Ident="tau_krit" Text="kritische Sohlschubspannung" Unit="N/m²" Format="F7.3" Null="-1" Help="kritische Sohlschubspannung, ab der Erosion auftritt" Max="" Default="9999.99" />'
   write(u_xml, '(A)') '  <Parameter Ident="M_eros" Text="Erodibilitätskonstante" Unit="kg/(m²s)" Format="F7.3" Null="-1" Help="" Min="" Max="" Default="0." />'
   write(u_xml, '(A)') '  <Parameter Ident="n_eros" Text="Exponent in der Erosionsformel" Unit="-" Format="F7.3" Null="-1" Help="Exponent in der Erosionsformel, potenziert den relativen Sohlspannungsüberschuss" Min="" Max="" Default="1." />'
   write(u_xml, '(A)') '  <Parameter Ident="sed_roh" Text="Dichte des liegenden Sediments" Unit="kg/m³" Format="F7.3" Null="-1" Help="" Min="" Max="" Default="2650.0" />'
   write(u_xml, '(A)') '</ParamSetDef>'
   
   write(u_xml, '(A)') '</GerrisParam>'
   
   close(u_xml)
end subroutine write_modellg_param
