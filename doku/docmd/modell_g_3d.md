ModellG.3D.txt  {#lnk_modell_g3}
==============

Die Datei <a href="./exp/MODELLG.3D.txt" target="_blank">ModellG.3D.txt</a>
dient der zonenweisen Zuordnung von Parametern, Rand- und Anfangsbedingungen.

Sie ist hervorgegangen aus der Datei 
<a href="./exp/ModellG_beispiel.txt" target="_blank">ModellG.txt</a>
in QSim-1D, welche eine Zuordnung zu Abschnitten in Strängen des 1D-Modells 
erlaubte.


Welcher Knoten zu welcher <b>Zone</b> gehört, ist in der Datei points 
(siehe \ref lnk_datenmodell) festgelegt.
Die Zonen-Einteilung des Modellgebiets wird z. Z. mit dem
Netzerstellungswerkzeug JANET vorgenommen. Diese Zonen-Einteilung geschieht vor 
der hydraulischen Simulation und wird vom Gütemodell übernommen.


Beispiele:

3D: <a href="./exp/MODELLG.3D.txt" target="_blank">MODELLG.3D.txt</a>
Zeilen, die mit # beginnen, sind Kommentarzeilen und werden von QSim3D überlesen.

1D: <a href="./exp/ModellG_beispiel.txt" target="_blank">ModellG_beispiel.txt</a>
Die mit # gekennzeichneten Zeilen dienen der Erläuterung und müssen entfernt 
werden um die Datei als Eingabe für QSim1D zu verwenden.

Die Datei MODELLG.3D.txt gliedert sich in einen Kopfbereich und nachfolgende 
Datenblöcke für die einzelnen Zonen (Zonen-Blöcke).


## Kopfbereich

Der Kopfbereich besteht aus drei Zeilen.

| Position | Inhalt | 
| -------- | ------ |
| Kopf, 1.Zeile | Versions-Zeile\n Die Zeile wird von Gerris benutzt und von QSim-3D nicht ausgewertet. | 
| Kopf, 2.Zeile | Modellname und ggf. weitere Angaben, maximal 255 Zeichen.\n
  Die Zeile wird von Gerris benutzt und von QSim-3D nicht ausgewertet. | 
| Kopf, 3.Zeile | Anzahl der Zonen des Modells, d.h. Anzahl der in der Datei folgenden Zonenblöcke | 


## Zonenblock

Vor jedem Zonenblock muss sich eine Leerzeile befinden.

Für jede Zone des Modells muss ein eigener Block vorhanden sein, der aus 
folgenden Zeilen besteht:

Die unten aufgeführten Kennbuschstaben müssen in der ersten Spalte stehen.

| Kenn- \n Buchstabe | Inhalt | Bemerkung | 1D <-> 3D | 
| ------------------ | ------ | --------- | --------- |
|  ' '  | <b>Zonennummer</b>, "Bezeichnung" |  
         Die erste Zeile eines Zonenblocks muss die Zonen-nummer enthalten. Die 
		 "Bezeichnung" dient zur Orientierung der Bearbeiter*in |  | 
|  T  | anfangsKm(unbenutzt),endKm(unbenutzt)  <b>Wetterstations-Nummer</b>, Wetterstations-Lage (mueNHN) | Wetterstations-Nummer gemäß der Datei WETTER.txt (siehe \ref lnk_datenmodell) | 3D identisch 1D | 
|  I  | Nummer des Randes, der zur <b>Initialisierung </b> dieser Zone dient  | 
           Randnummer entsprechend der Datei EREIGG.txt (siehe \ref lnk_datenmodell) | in 1D nicht vorhanden | 
|  R  | Reibungsbeiwert "Rauheit" für o.g. Zone als ks-Wert nach Nikuradse (Sandrauheit) in m
            |  casu-Werte müssen z. Z noch von Hand übertragen werden. SCHISM kennt keine Zonen. | in 1D nicht vorhanden | 
|  M  | Macrophyten  |  Makrophyten-Wachstum   | 3D identisch 1D [noch nicht implementiert] | 
|  P  | Macrophytendichte  |   | [noch nicht implementiert] | 
|  F  | Schiffsverkehr  |    |  wenn F vorhanden, ganze Zone Schiffsverkehr \ref schifffahrts_zone =1 | 
|  D  | Dreissena (Muscheln)  |  Dreissena-Bewuchs in den Gewässer-Abschnitten  | [noch nicht implementiert] | 
|  C  | Corophium (Schlickkrebs)  |  Corophium-Vorkommen in den Gewässer-Abschnitten  | [noch nicht implementiert] | 
|  B  | benthische Algen  |  Benth.Algen-Vorkommen in den Gewässer-Abschnitten  | [noch nicht implementiert] | 
|  V  | tiefenaufgelöste Berechnung  |  Abschnitte mit 2D-tiefenaufgelöster-Modellierung | [noch nicht implementiert] | 
|  U  | Buhnen  |  Parameter für Buhnenfelder  | [noch nicht implementiert] | 
|  L  | Laichperiode  |  Laichperiode   | [noch nicht implementiert] | 
|  O  | Verschattung durch Uferbewuchs ?  |  Anteil der Vegetationstypen | [noch nicht implementiert] | 
|  Z  |  Sediment-Kenngrößen  |  Belegungen für Stoffumsatz im Sediment </td>
<td>aPOM(irrelevant),ePOM(irrelevant),\ref sedom,\ref bedgs,\ref sedvvert ,(optional) \ref kornd ,\ref burial  | 
|  S  |  Sedimenttemperatur   |  Kenngrössen für Temperatur/Sedimenttemperatur  | [noch nicht implementiert] | 
|  W  |  Wehr   |  Parameter des Wehres  | in 3D nicht sinnvoll | 
|  E  |  Erosion   |  kritische Sohlschubspannung, Erodibilitätskonstante, Exponent, Dichte |   | 
|    |     |    |  | 


Die Datei ist ähnlich wie ModellG.txt für QSim aufgebaut, die in der
<a href="./pdf/Schnittstelle_QSIM2004.pdf" target="_blank">
Schnittstellenbeschreibung 2004 Gerris-QSim</a> \n
<a href="./pdf/schnittstellenbeschreibung2006_gerris.pdf" target="_blank">
Schnittstellenbeschreibung 2006 Gerris-QSim</a>\n
genauer erläutert wird.\n\n
Aktuelle Parameter siehe: ModellGParam().

# Wetter
Die Wetter Randbedingungen (meteorologischen Bedingungen) sind zeitabhängig. \n
Sie sind in Gerris über die Schaltfläche "Wetter" bearbeitbar.\n
Details dazu in: \ref lnk_wetter_rb \n

## T Wetterstation
In der Datei \ref lnk_modell_g3 wird lediglich vorgegeben, welche Wetterstation 
zur jeweiligen Zone gehört. 

<code>\verbatim
<ParamSetDef Id="QT" Text="Wetterstation" Help="Wetterstations-Zuordnung" Scope="Abschnitt">
<Parameter Ident="WStation" Text="Wetterstation" Unit="" Format="I4" Null="-1" Help="Zugehörige Wetterstation" Min="" Max="" Default="" />
<Parameter Ident="WLage" Text="Lage der Station" Unit="m üb. NN" Format="F7.1" Null="-1" Help="" Min="" Max="" Default="" />
\endverbatim</code>

# Gebiets-Eigenschaften
In der Gerris-Benutzeroberfläche werden sie unter der Schaltfläche 
"Strang-Optionen" geführt.

## R Reibungsbeiwert
nur in 3D:\n
Es wird der ks-Wert nach Nikuradse (Sandrauheit) eingelesen.

## I Initialisierung
nur in 3D:\n
Initialisierung für die jeweilige Zone mittels Randbedingung.\n
d.h. es wird eine Randnummer eingelesen.\n
Alle Berechnungspunkte der jeweiligen Zone bekommen die Werte die am 
zugeordneten Rand zum Berechnungs-Startzeitpunkt vorliegen.

## Z Sediment
...
<code>\verbatim
<ParamSetDef Id="QZ" Text="Sediment-Kenngrößen" Help="Sediment-Kenngrößen in den Gewässer-Abschnitten" Scope="Abschnitt">'
  <Parameter Ident="POMsed" Text="Anteil org. Materials" Unit="%" Format="F6.2" Null="-1" Help="Anteil des organischen Materials im Sediment" Min="" Max="" Default="-1" />
  <Parameter Ident="BedGSed" Text="Bedeckungsgrad der Sohle mit Sediment (0-1)" Unit="-" Format="F5.2" Null="-1" Help="" Min="0" Max="1" Default="-1." />
  <Parameter Ident="VVERTZ" Text="volumenbezogene Eindringgeschwindigkeit ins Sediment" Unit="mm/h" Format="F9.4" Null="-1" Help="" Min="" Max="" Default="-1." />
\endverbatim</code>

## S Sedimenttemperatur
...
<code>\verbatim
<ParamSetDef Id="QS" Text="Kenngrössen für Temperatur/Sedimenttemperatur" Help="Kenngrößen für die Gewässerabschnitten" Scope="Abschnitt">'
  <Parameter Ident="SPEWKS" Text="Spez. WärmeKapazität Sediment" Unit="KJ/(kg*K)" Format="F6.2" Null="-1" Help="Ton: 0.83; Sand: 0.88" Min="0.8" Max="4.5" Default="-1" />
  <Parameter Ident="WUEBK" Text="Wärmeübergangskoeffizient" Unit="KJ/(K*m2*h)" Format="F7.2" Null="-1" Help="" Min="0" Max="1000" Default="-1." />
  <Parameter Ident="PSREFS" Text="Reflektionsanteil der Strahlung an der Sedimentoberfläche" Unit="-" Format="F5.2" Null="-1" Help="" Min="0" Max="1" Default="-1." />
  <Parameter Ident="EXTKS" Text="Extinktionskoeffizient für PARS (nur bei Temperaturmodellierung erforderlich!)" Unit="-" Format="F5.2" Null="-1" Help="" Min="" Max="" Default="-1." />
\endverbatim</code>

## L Laichperiode
...
<code>\verbatim
<ParamSetDef Ident="QL" Text="Laichperiode" Help="Laichperiode" Scope="Strang">'
  <Parameter Ident="StartTag" Text="Start-Tag" Unit="" Format="I2" Null="-1" Help="Tag des Beginns der Laichperiode" Min="1" Max="31" Default="" />
  <Parameter Ident="StartMonat" Text="Start-Monat" Unit="" Format="I2" Null="-1" Help="Monat des Beginns der Laichperiode" Min="1" Max="12" Default="" />
  <Parameter Ident="Dauer" Text="Dauer" Unit="" Format="I2" Null="-1" Help="Dauer der Laichperiode in Tagen" Min="0" Max="9999" Default="" />
\endverbatim</code>

## D Dreissena-Muschel
Im 3D gibt es keine Böschungen, daher wird dort die Muscheldichte Null gesetzt.
<code>\verbatim
<ParamSetDef Id="QD" Text="Dreissena" Help="Dreissena-Bewuchs in den Gewässer-Abschnitten" Scope="Abschnitt">
  <Parameter Ident="mboesch0" Text="Biomasse 0.Koh. Böschung" Unit="g/m²" Format="F7.2" Null="-1" Help="Dreissena-Biomasse der 0. Kohorte (Schalenlänge kl. 8 mm) im Abschnitt an der Böschung" Min="" Max="" Default="" />
  <Parameter Ident="msohle0" Text="Biomasse 0.Koh. Sohle" Unit="g/m²" Format="F7.2" Null="-1" Help="Dreissena-Biomasse der 0. Kohorte im Abschnitt an der Sohle" Min="" Max="" Default="" />
  <Parameter Ident="gewicht0" Text="Mittl. Muschelgewicht 0.Koh." Unit="mgC" Format="F7.3" Null="-1" Help="Gewicht einer Muschel als Mittelwert der 0. Kohorte" Min="" Max="" Default="" />
  <Parameter Ident="mboesch1" Text="Biomasse 1.Koh. Böschung" Unit="g/m²" Format="F7.2" Null="-1" Help="Dreissena-Biomasse der 1. Kohorte (Schalenlänge gr.= 8 mm) im Abschnitt an der Böschung" Min="" Max="" Default="" />
  <Parameter Ident="msohle1" Text="Biomasse 1.Koh. Sohle" Unit="g/m²" Format="F7.2" Null="-1" Help="Dreissena-Biomasse der 1. Kohorte im Abschnitt an der Sohle" Min="" Max="" Default="" />
  <Parameter Ident="gewicht1" Text="Mittl. Muschelgewicht 1.Koh." Unit="mgC" Format="F7.3" Null="-1" Help="Gewicht einer Muschel als Mittelwert der 1. Kohorte." Min="" Max="" Default="" />
\endverbatim</code>

## M Makrophyten
...
<code>\verbatim
<ParamSetDef Id="QM" Text="Makrophyten" Help="Makrophyten-Wachstum" Scope="Strang">
  <Parameter Ident="StartTag" Text="Start-Tag" Unit="" Format="I2" Null="-1" Help="Tag des Wachstumsbeginns der Makrophyten" Min="1" Max="31" Default="" />
  <Parameter Ident="StartMonat" Text="Start-Monat" Unit="" Format="I2" Null="-1" Help="Monat des Wachstumsbeginns der Makrophyten" Min="1" Max="12" Default="" />
  <Parameter Ident="MaxTag" Text="Max.-Tag" Unit="" Format="I2" Null="-1" Help="Tag, an dem die Makrophytenbiomasse ihr Maximum hat" Min="1" Max="31" Default="" />
  <Parameter Ident="MaxMonat" Text="Max.-Monat" Unit="" Format="I2" Null="-1" Help="Monat, in dem die Makrophytenbiomasse ihr Maximum hat" Min="1" Max="12" Default="" />
  <Parameter Ident="EndTag" Text="Ende-Tag" Unit="" Format="I2" Null="-1" Help="Tag, an dem die Makrophytenbiomasse ihr Minimum erreicht hat. Hier endet das Makrophytenwachstum" Min="1" Max="31" Default="" />
  <Parameter Ident="EndMonat" Text="Ende-Monat" Unit="" Format="I2" Null="-1" Help="Monat, in dem die Makrophytenbiomasse ihr Minimum erreicht hat" Min="1" Max="12" Default="" />
\endverbatim</code>

## P Makrophyten-Dichte
...
<code>\verbatim
<ParamSetDef Id="QP" Text="Dichte der Makrophyten" Help="Makrophyten-Dichte" Scope="Abschnitt">'
  <Parameter Ident="PflMin" Text="min. Dichte (Winter)" Unit="g/m²" Format="F7.2" Null="-1" Help="Minimale Dichte der Makrophyten im Winter" Min="" Max="" Default="" />
  <Parameter Ident="PflMax" Text="max. Dichte (Sommer)" Unit="g/m²" Format="F7.2" Null="-1" Help="Maximale Dichte der Makrophyten im Sommer" Min="" Max="" Default="" />
\endverbatim</code>

## F Schiffsverkehr
...
<code>\verbatim
<ParamSetDef Id="QF" Text="Schiffsverkehr" Help="Schiffsverkehr auf den Gewässer-Abschnitten" Scope="Abschnitt">'
  <Parameter Ident="VSCHIFF" Text="Schiffsgeschwindigkeit" Unit="m/s" Format="F5.2" Null="-1" Help="" Min="0" Max="99.99" Default="1.5" />
  <Parameter Ident="UPROP" Text="Drehzahl des Propellers" Unit="U/s" Format="F5.2" Null="-1" Help="" Min="0" Max="99.99" Default="3.33" />
<ParamSetDef Id="QC" Text="Corophium" Help="Corophium-Vorkommen in den Gewässer-Abschnitten" Scope="Abschnitt">'
  <Parameter Ident="DBoesch" Text="Ind.dichte Böschung" Unit="Ind/m²" Format="F8.1" Null="-1" Help="Individuen-Dichte im Abschnitt an der Böschung" Min="" Max="" Default="" />
  <Parameter Ident="DSohle" Text="Ind.dichte Sohle" Unit="Ind/m²" Format="F8.1" Null="-1" Help="Individuen-Dichte im Abschnitt an der Sohle" Min="" Max="" Default="" />
\endverbatim</code>

## B Benthische Algen
...
<code>\verbatim
<ParamSetDef Id="QB" Text="Benth.Algen" Help="Benth.Algen-Vorkommen in den Gewässer-Abschnitten" Scope="Abschnitt">'
  <Parameter Ident="GGruen" Text="Gewicht Grünalgen" Unit="g/m²" Format="F7.1" Null="-1" Help="Trockengewicht der benthischen Grünalgen" Min="" Max="" Default="-1" />
  <Parameter Ident="GKiesel" Text="Gewicht Kieselalgen" Unit="g/m²" Format="F7.1" Null="-1" Help="Trockengewicht der benthischen Kieselalgen" Min="" Max="" Default="-1" />
\endverbatim</code>

## C Corophium
...
<code>\verbatim
<ParamSetDef Id="QC" Text="Corophium" Help="Corophium-Vorkommen in den Gewässer-Abschnitten" Scope="Abschnitt">'
  <Parameter Ident="DBoesch" Text="Ind.dichte Böschung" Unit="Ind/m²" Format="F8.1" Null="-1" Help="Individuen-Dichte im Abschnitt an der Böschung" Min="" Max="" Default="" />
  <Parameter Ident="DSohle" Text="Ind.dichte Sohle" Unit="Ind/m²" Format="F8.1" Null="-1" Help="Individuen-Dichte im Abschnitt an der Sohle" Min="" Max="" Default="" />
\endverbatim</code>

## O Vegetationstypen
...
<code>\verbatim
<ParamSetDef Id="QO" Text="Anteil der Vegetationstypen" Help="Anteil der Vegetationstypen" Scope="Abschnitt">
  <Parameter Ident="VTYP1" Text="Niedervegetation (links)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />
  <Parameter Ident="VTYP2" Text="Buschwerk (links)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />
  <Parameter Ident="VTYP3" Text="Weichholzaue (links)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />
  <Parameter Ident="VTYP4" Text="Laubwald (links)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />
  <Parameter Ident="VTYP5" Text="Nadelwald (links)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />
  <Parameter Ident="VTYP6" Text="Bebauung (links)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />
  <Parameter Ident="VALTBL" Text="Höhe d. Bebauung (links)" Unit="m" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />
  <Parameter Ident="EDUFBL" Text="Uferabstand d. Bebauung (links)" Unit="m" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />
  <Parameter Ident="VTYP7" Text="Niedervegetation (rechts)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />
  <Parameter Ident="VTYP8" Text="Buschwerk (rechts)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />
  <Parameter Ident="VTYP9" Text="Weichholzaue (rechts)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />
  <Parameter Ident="VTYP10" Text="Laubwald (rechts)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />
  <Parameter Ident="VTYP11" Text="Nadelwald (rechts)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />
  <Parameter Ident="VTYP12" Text="Bebauung (rechts)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />
  <Parameter Ident="VALTBR" Text="Höhe d. Bebauung (rechts)" Unit="m" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />
  <Parameter Ident="EDUFBR" Text="Uferabstand d. Bebaung (rechts)" Unit="m" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />
  <Parameter Ident="VTYP13" Text="Laubwald (beidseitig)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />
  <Parameter Ident="VTYP14" Text="Nadelwald (beidseitig)" Unit="%" Format="F6.2" Null="-1" Help="" Min="" Max="" Default="0" />
\endverbatim</code>

## W V U - Keine Entsprechung im 3D
...
<code>\verbatim
<ParamSetDef Id="QW" Text="Wehr" Help="Parameter des Wehres" Scope="Strang">'
  <Parameter Ident="IstAktiv" Text="Aktiv" Unit="" Format="I1" Null="0" Help="Wehr nicht berücksichtigen: 0; Wehr berücksichtigen: 1" Min="" Max="" Default="0" />
  <Parameter Ident="Breite" Text="Breite [m]" Unit="m" Format="F7.2" Null="-1" Help="Breite des Wehres >0 : Wehrbelüftung" Min="0" Max="9999.999" Default="" />
  <Parameter Ident="Hoehe" Text="Höhe [m]" Unit="m" Format="F7.2" Null="-1" Help="Falls Breite >0 Wehrhöhe aus Wasserspiegeldifferenz OW/UW berechnen: -1; ansonsten Höhe >0" Min="0" Max="999.999" Default="" />
</ParamSetDef>'
<ParamSetDef Id="QV" Text="2D-Modellierung" Help="Abschnitte mit 2D-Modellierung" Scope="Abschnitt">'
</ParamSetDef>'
<ParamSetDef Id="QU" Text="Buhnen" Help="Parameter für Buhnenfelder" Scope="Abschnitt">'
  <Parameter Ident="DLB" Text="Long. Disp.koeff." Unit="m²/s" Format="F7.2" Null="-1" Help="Longitudinaler Dispersionskoeffizient" Min="" Max="" Default="" />
  <Parameter Ident="TAU2B" Text="max. Austauschzeit" Unit="h" Format="F7.2" Null="-1" Help="Austauschzeit zwischen Buhnen und Hauptstrom" Min="" Max="" Default="" />
  <Parameter Ident="ALPHAB" Text="Alpha" Unit="" Format="F6.2" Null="-1" Help="Exponent beim -Breite/Tiefe-Term- zur Berechnung der longitudinalen Dispersion (nicht bei Elder) Deng: 1.67; Li: 1.3; Iwasa: 1.5" Min="" Max="" Default="" />
  <Parameter Ident="POMsedb" Text="Anteil org. Materials" Unit="%" Format="F6.2" Null="-1" Help="Anteil des organischen Materials im Sediment" Min="" Max="" Default="-1." />
</ParamSetDef>'
\endverbatim</code>

## E Erosionsparameter
<code>\verbatim
 WRITE(1, '(A)') '<ParamSetDef Id="QE" Text="Erosions-Parameter" Help="Kenngrößen für die Gewässerabschnitte" Scope="Abschnitt">'
 WRITE(1, '(A)') '  <Parameter Ident="tau_krit" Text="kritische Sohlschubspannung ab der Erosion auftritt"       Unit="N/m²"      Format="F7.3" Null="-1" Help="" Max="" Default="9999.99" />'
 WRITE(1, '(A)') '  <Parameter Ident="M_eros"   Text="Erodibilitätskonstante"                                    Unit="kg/(m²*s)" Format="F7.3" Null="-1" Help="" Min="" Max="" Default="0." />'
 WRITE(1, '(A)') '  <Parameter Ident="n_eros"   Text="Exponent in der Erosionsformel, potenziert den relativen Sohlspannungsüberschuss" Unit="-" Format="F7.3" Null="-1" Help="" Min="" Max="" Default="1." />'
 WRITE(1, '(A)') '  <Parameter Ident="sed_roh"  Text="Dichte des liegenden Sediments"                            Unit="kg/m³"     Format="F7.3" Null="-1" Help="" Min="" Max="" Default="2650.0" />'
 WRITE(1, '(A)') '</ParamSetDef>'
\endverbatim</code>


Textquelle: modell_g_3d.md ; Codesources: modellg.f95 ;  
zurück: \ref lnk_datenmodell oder \ref lnk_lokale_parameter
