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

 subroutine ModellGParam(cpfad1,j1)

!  Ausgabe der Definition von ModellG


  character(255)  :: cpfad1
  character (len=275)         :: pfadstring
  character (len = 8)         :: versionstext

  write(pfadstring,'(2A)')trim(adjustl(cpfad1(1:j1))),'ModellGParam.xml' 
  open(unit=1, file=pfadstring, encoding='UTF-8')
  
  WRITE(1, '(A)') '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
  call version_string(versionstext)
  WRITE(1, '(3A)') '<GerrisParam FileType="ModellG" QsimVersion="',versionstext,'">'  
  WRITE(1, '(A)') '<ParamSetDef Ident="QL" Text="Laichperiode" Help="Laichperiode" Scope="Strang">'
  WRITE(1, '(A)') '  <Parameter Ident="StartTag" Text="Start-Tag" Unit="" Format="I2" Null="-1" Help="Tag des Beginns der Laichperiode" Min="1" Max="31" Default="" />'
  WRITE(1, '(A)') '  <Parameter Ident="StartMonat" Text="Start-Monat" Unit="" Format="I2" Null="-1" Help="Monat des Beginns der Laichperiode" Min="1" Max="12" Default="" />'
  WRITE(1, '(A)') '  <Parameter Ident="Dauer" Text="Dauer" Unit="" Format="I2" Null="-1" Help="Dauer der Laichperiode in Tagen" Min="0" Max="9999" Default="" />'
  WRITE(1, '(A)') '</ParamSetDef>'
  WRITE(1, '(A)') '<ParamSetDef Id="QW" Text="Wehr" Help="Parameter des Wehres" Scope="Strang">'
  WRITE(1, '(A)') '  <Parameter Ident="IstAktiv" Text="Aktiv" Unit="" Format="I1" Null="0" Help="Wehr nicht berücksichtigen: 0; Wehr berücksichtigen: 1" Min="" Max="" Default="0" />'
  WRITE(1, '(A)') '  <Parameter Ident="Breite" Text="Breite [m]" Unit="m" Format="F7.2" Null="-1" Help="Breite des Wehres >0 : Wehrbelüftung" Min="0" Max="9999.999" Default="" />'
  WRITE(1, '(A)') '  <Parameter Ident="Hoehe" Text="Höhe [m]" Unit="m" Format="F7.2" Null="-1" Help="Falls Breite >0 Wehrhöhe aus Wasserspiegeldifferenz OW/UW berechnen: -1; ansonsten Höhe >0" Min="0" Max="999.999" Default="" />'
  WRITE(1, '(A)') '</ParamSetDef>'
  WRITE(1, '(A)') '<ParamSetDef Id="QM" Text="Makrophyten" Help="Makrophyten-Wachstum" Scope="Strang">'
  WRITE(1, '(A)') '  <Parameter Ident="StartTag" Text="Start-Tag" Unit="" Format="I2" Null="-1" Help="Tag des Wachstumsbeginns der Makrophyten" Min="1" Max="31" Default="" />'
  WRITE(1, '(A)') '  <Parameter Ident="StartMonat" Text="Start-Monat" Unit="" Format="I2" Null="-1" Help="Monat des Wachstumsbeginns der Makrophyten" Min="1" Max="12" Default="" />'
  WRITE(1, '(A)') '  <Parameter Ident="MaxTag" Text="Max.-Tag" Unit="" Format="I2" Null="-1" Help="Tag, an dem die Makrophytenbiomasse ihr Maximum hat" Min="1" Max="31" Default="" />'
  WRITE(1, '(A)') '  <Parameter Ident="MaxMonat" Text="Max.-Monat" Unit="" Format="I2" Null="-1" Help="Monat, in dem die Makrophytenbiomasse ihr Maximum hat" Min="1" Max="12" Default="" />'
  WRITE(1, '(A)') '  <Parameter Ident="EndTag" Text="Ende-Tag" Unit="" Format="I2" Null="-1" Help="Tag, an dem die Makrophytenbiomasse ihr Minimum erreicht hat. Hier endet das Makrophytenwachstum" Min="1" Max="31" Default="" />'
  WRITE(1, '(A)') '  <Parameter Ident="EndMonat" Text="Ende-Monat" Unit="" Format="I2" Null="-1" Help="Monat, in dem die Makrophytenbiomasse ihr Minimum erreicht hat" Min="1" Max="12" Default="" />'
  WRITE(1, '(A)') '</ParamSetDef>'
  WRITE(1, '(A)') '<ParamSetDef Id="QP" Text="Dichte der Makrophyten" Help="Makrophyten-Dichte" Scope="Abschnitt">'
  WRITE(1, '(A)') '  <Parameter Ident="PflMin" Text="min. Dichte (Winter)" Unit="g/m²" Format="F7.2" Null="-1" Help="Minimale Dichte der Makrophyten im Winter" Min="" Max="" Default="" />'
  WRITE(1, '(A)') '  <Parameter Ident="PflMax" Text="max. Dichte (Sommer)" Unit="g/m²" Format="F7.2" Null="-1" Help="Maximale Dichte der Makrophyten im Sommer" Min="" Max="" Default="" />'
  WRITE(1, '(A)') '</ParamSetDef>'
  WRITE(1, '(A)') '<ParamSetDef Id="QF" Text="Schiffsverkehr" Help="Schiffsverkehr auf den Gewässer-Abschnitten" Scope="Abschnitt">'
  WRITE(1, '(A)') '  <Parameter Ident="VSCHIFF" Text="Schiffsgeschwindigkeit" Unit="m/s" Format="F5.2" Null="-1" Help="" Min="0" Max="99.99" Default="1.5" />'
  WRITE(1, '(A)') '  <Parameter Ident="UPROP" Text="Drehzahl des Propellers" Unit="U/s" Format="F5.2" Null="-1" Help="" Min="0" Max="99.99" Default="3.33" />'
  WRITE(1, '(A)') '</ParamSetDef>'
  WRITE(1, '(A)') '<ParamSetDef Id="QD" Text="Dreissena" Help="Dreissena-Bewuchs in den Gewässer-Abschnitten" Scope="Abschnitt">'
  WRITE(1, '(A)') '  <Parameter Ident="MBoesch0" Text="Biomasse 0.Koh. Böschung" Unit="g/m²" Format="F7.2" Null="-1" Help="Dreissena-Biomasse der 0. Kohorte (Schalenlänge kl. 8 mm) im Abschnitt an der Böschung" Min="" Max="" Default="" />'
  WRITE(1, '(A)') '  <Parameter Ident="MSohle0" Text="Biomasse 0.Koh. Sohle" Unit="g/m²" Format="F7.2" Null="-1" Help="Dreissena-Biomasse der 0. Kohorte im Abschnitt an der Sohle" Min="" Max="" Default="" />'
  WRITE(1, '(A)') '  <Parameter Ident="Gewicht0" Text="Mittl. Muschelgewicht 0.Koh." Unit="mgC" Format="F7.3" Null="-1" Help="Gewicht einer Muschel als Mittelwert der 0. Kohorte" Min="" Max="" Default="" />'
  WRITE(1, '(A)') '  <Parameter Ident="MBoesch1" Text="Biomasse 1.Koh. Böschung" Unit="g/m²" Format="F7.2" Null="-1" Help="Dreissena-Biomasse der 1. Kohorte (Schalenlänge gr.= 8 mm) im Abschnitt an der Böschung" Min="" Max="" Default="" />'
  WRITE(1, '(A)') '  <Parameter Ident="MSohle1" Text="Biomasse 1.Koh. Sohle" Unit="g/m²" Format="F7.2" Null="-1" Help="Dreissena-Biomasse der 1. Kohorte im Abschnitt an der Sohle" Min="" Max="" Default="" />'
  WRITE(1, '(A)') '  <Parameter Ident="Gewicht1" Text="Mittl. Muschelgewicht 1.Koh." Unit="mgC" Format="F7.3" Null="-1" Help="Gewicht einer Muschel als Mittelwert der 1. Kohorte." Min="" Max="" Default="" />'
  WRITE(1, '(A)') '</ParamSetDef>'
  WRITE(1, '(A)') '<ParamSetDef Id="QC" Text="Corophium" Help="Corophium-Vorkommen in den Gewässer-Abschnitten" Scope="Abschnitt">'
  WRITE(1, '(A)') '  <Parameter Ident="DBoesch" Text="Ind.dichte Böschung" Unit="Ind/m²" Format="F8.1" Null="-1" Help="Individuen-Dichte im Abschnitt an der Böschung" Min="" Max="" Default="" />'
  WRITE(1, '(A)') '  <Parameter Ident="DSohle" Text="Ind.dichte Sohle" Unit="Ind/m²" Format="F8.1" Null="-1" Help="Individuen-Dichte im Abschnitt an der Sohle" Min="" Max="" Default="" />'
  WRITE(1, '(A)') '</ParamSetDef>'
  WRITE(1, '(A)') '<ParamSetDef Id="QB" Text="Benth.Algen" Help="Benth.Algen-Vorkommen in den Gewässer-Abschnitten" Scope="Abschnitt">'
  WRITE(1, '(A)') '  <Parameter Ident="GGruen" Text="Gewicht Grünalgen" Unit="g/m²" Format="F7.1" Null="-1" Help="Trockengewicht der benthischen Grünalgen" Min="" Max="" Default="-1" />'
  WRITE(1, '(A)') '  <Parameter Ident="GKiesel" Text="Gewicht Kieselalgen" Unit="g/m²" Format="F7.1" Null="-1" Help="Trockengewicht der benthischen Kieselalgen" Min="" Max="" Default="-1" />'
  WRITE(1, '(A)') '</ParamSetDef>'
  WRITE(1, '(A)') '<ParamSetDef Id="QT" Text="Wetterstation" Help="Wetterstations-Zuordnung" Scope="Abschnitt">'
  WRITE(1, '(A)') '  <Parameter Ident="WStation" Text="Wetterstation" Unit="" Format="I4" Null="-1" Help="Zugehörige Wetterstation" Min="" Max="" Default="" />'
  WRITE(1, '(A)') '  <Parameter Ident="WLage" Text="Lage der Station" Unit="m üb. NN" Format="F7.1" Null="-1" Help="" Min="" Max="" Default="" />'
  WRITE(1, '(A)') '</ParamSetDef>'
  WRITE(1, '(A)') '<ParamSetDef Id="QV" Text="2D-Modellierung" Help="Abschnitte mit 2D-Modellierung" Scope="Abschnitt">'
  WRITE(1, '(A)') '</ParamSetDef>'
  WRITE(1, '(A)') '<ParamSetDef Id="QU" Text="Buhnen" Help="Parameter für Buhnenfelder" Scope="Abschnitt">'
  WRITE(1, '(A)') '  <Parameter Ident="DLB" Text="Long. Disp.koeff." Unit="m²/s" Format="F7.2" Null="-1" Help="Longitudinaler Dispersionskoeffizient" Min="" Max="" Default="" />'
  WRITE(1, '(A)') '  <Parameter Ident="TAU2B" Text="max. Austauschzeit" Unit="h" Format="F7.2" Null="-1" Help="Austauschzeit zwischen Buhnen und Hauptstrom" Min="" Max="" Default="" />'
  WRITE(1, '(A)') '  <Parameter Ident="ALPHAB" Text="Alpha" Unit="" Format="F6.2" Null="-1" Help="Exponent beim -Breite/Tiefe-Term- zur Berechnung der longitudinalen Dispersion (nicht bei Elder) Deng: 1.67; Li: 1.3; Iwasa: 1.5" Min="" Max="" Default="" />'
  WRITE(1, '(A)') '  <Parameter Ident="POMsedb" Text="Anteil org. Materials" Unit="%" Format="F6.2" Null="-1" Help="Anteil des organischen Materials im Sediment" Min="" Max="" Default="-1." />'
  WRITE(1, '(A)') '</ParamSetDef>'
  WRITE(1, '(A)') '<ParamSetDef Id="QO" Text="Anteil der Vegetationstypen" Help="Anteil der Vegetationstypen" Scope="Abschnitt">'
  WRITE(1, '(A)') '  <Parameter Ident="VTYP1" Text="Niedervegetation (links)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />'
  WRITE(1, '(A)') '  <Parameter Ident="VTYP2" Text="Buschwerk (links)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />'
  WRITE(1, '(A)') '  <Parameter Ident="VTYP3" Text="Weichholzaue (links)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />'
  WRITE(1, '(A)') '  <Parameter Ident="VTYP4" Text="Laubwald (links)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />'
  WRITE(1, '(A)') '  <Parameter Ident="VTYP5" Text="Nadelwald (links)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />'
  WRITE(1, '(A)') '  <Parameter Ident="VTYP6" Text="Bebauung (links)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />'
  WRITE(1, '(A)') '  <Parameter Ident="VALTBL" Text="Höhe d. Bebauung (links)" Unit="m" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />'
  WRITE(1, '(A)') '  <Parameter Ident="EDUFBL" Text="Uferabstand d. Bebauung (links)" Unit="m" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />'
  WRITE(1, '(A)') '  <Parameter Ident="VTYP7" Text="Niedervegetation (rechts)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />'
  WRITE(1, '(A)') '  <Parameter Ident="VTYP8" Text="Buschwerk (rechts)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />'
  WRITE(1, '(A)') '  <Parameter Ident="VTYP9" Text="Weichholzaue (rechts)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />'
  WRITE(1, '(A)') '  <Parameter Ident="VTYP10" Text="Laubwald (rechts)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />'
  WRITE(1, '(A)') '  <Parameter Ident="VTYP11" Text="Nadelwald (rechts)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />'
  WRITE(1, '(A)') '  <Parameter Ident="VTYP12" Text="Bebauung (rechts)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />'
  WRITE(1, '(A)') '  <Parameter Ident="VALTBR" Text="Höhe d. Bebauung (rechts)" Unit="m" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />'
  WRITE(1, '(A)') '  <Parameter Ident="EDUFBR" Text="Uferabstand d. Bebaung (rechts)" Unit="m" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />'
  WRITE(1, '(A)') '  <Parameter Ident="VTYP13" Text="Laubwald (beidseitig)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />'
  WRITE(1, '(A)') '  <Parameter Ident="VTYP14" Text="Nadelwald (beidseitig)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />'
  WRITE(1, '(A)') '</ParamSetDef>'
  WRITE(1, '(A)') '<ParamSetDef Id="QZ" Text="Sediment-Kenngrößen" Help="Sediment-Kenngrößen in den Gewässer-Abschnitten" Scope="Abschnitt">'
  WRITE(1, '(A)') '  <Parameter Ident="POMsed" Text="Anteil org. Materials" Unit="%" Format="F6.2" Null="-1" Help="Anteil des organischen Materials im Sediment" Min="" Max="" Default="-1" />'
  WRITE(1, '(A)') '  <Parameter Ident="BedGSed" Text="Bedeckungsgrad der Sohle mit Sediment (0-1)" Unit="-" Format="F5.2" Null="-1" Help="" Min="0" Max="1" Default="-1." />'
  WRITE(1, '(A)') '  <Parameter Ident="VVERTZ" Text="volumenbezogene Eindringgeschwindigkeit ins Sediment" Unit="mm/h" Format="F9.4" Null="-1" Help="" Min="" Max="" Default="-1." />'

  WRITE(1, '(A)') '</ParamSetDef>'
  WRITE(1, '(A)') '<ParamSetDef Id="QS" Text="Kenngrössen für Temperatur/Sedimenttemperatur" Help="Kenngrößen für die Gewässerabschnitten" Scope="Abschnitt">'
  WRITE(1, '(A)') '  <Parameter Ident="SPEWKS" Text="Spez. WärmeKapazität Sediment" Unit="KJ/(kg*K)" Format="F6.2" Null="-1" Help="Ton: 0.83; Sand: 0.88" Min="0.8" Max="4.5" Default="-1" />'
  WRITE(1, '(A)') '  <Parameter Ident="WUEBK" Text="Wärmeübergangskoeffizient" Unit="KJ/(K*m2*h)" Format="F7.2" Null="-1" Help="" Min="0" Max="1000" Default="-1." />'
  WRITE(1, '(A)') '  <Parameter Ident="PSREFS" Text="Reflektionsanteil der Strahlung an der Sedimentoberfläche" Unit="-" Format="F5.2" Null="-1" Help="" Min="0" Max="1" Default="-1." />'
  WRITE(1, '(A)') '  <Parameter Ident="EXTKS" Text="Extinktionskoeffizient für PARS (nur bei Temperaturmodellierung erforderlich!)" Unit="-" Format="F5.2" Null="-1" Help="" Min="" Max="" Default="-1." />'
  WRITE(1, '(A)') '</ParamSetDef>'

!  WRITE(1, '(A)') '<ParamSetDef Id="Q1" Text="Platzhalter1" Help="Platzhalter1" Scope="Abschnitt">'
!  WRITE(1, '(A)') '</ParamSetDef>'
!  WRITE(1, '(A)') '<ParamSetDef Id="Q2" Text="Platzhalter2" Help="Platzhalter2" Scope="Abschnitt">'
!  WRITE(1, '(A)') '</ParamSetDef>'
!  WRITE(1, '(A)') '<ParamSetDef Id="Q3" Text="Platzhalter3" Help="Platzhalter3" Scope="Abschnitt">'
!  WRITE(1, '(A)') '</ParamSetDef>'
!  WRITE(1, '(A)') '<ParamSetDef Id="Q4" Text="Platzhalter4" Help="Platzhalter4" Scope="Abschnitt">'
!  WRITE(1, '(A)') '</ParamSetDef>'
  WRITE(1, '(A)') '</GerrisParam>'
  
  CLOSE(1)

 END subroutine ModellGParam
