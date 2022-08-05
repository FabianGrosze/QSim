Wetter-Randbedingungen  {#lnk_wetter_rb}
=======================

Die Wetter-Randbedingungen (meteorologischen Bedingungen) werden für die 
\ref lnk_wtemp und das Strahlungsklima, das die Algen (\ref lnk_phy_licht ) 
erfahren, benötigt.

## Wetter-Parameter
Diese Wetterdaten werden bei der eingabe() von Subroutine wetter_readallo0() 
aus der Datei <a href="./exp/WETTER.txt" target="_blank">WETTER.txt</a> gelesen.

|Spalte WETTER.txt | 1D-Name | 3D-Stationswert | Nr. \ref lnk_uebergabewerte | Beschreibung | Einheit |
| --- | --- | --- | --- | --- | --- |
| 1 | \anchor glob glob |   |  -  | Globalstrahlung von strahlg() berechnet | cal/(cm2*h) | 
| 2 | tlmax  | \anchor tlmax_T tlmax_T |  -  | Tagesmaximum der Lufttemperatur (bei der Verwendung von Tagesmittelwerten sonst Zeitwert) | Grad Celsius  | 
| 3 | tlmin  | \anchor tlmin_T tlmin_T |  -  | Tagesminimum der Lufttemperatur (nur bei der Verwendung von Tagesmittelwerten verwendet) | Grad Celsius  | 
| 4 | \anchor RO RO	   | \anchor ro_T ro_T   | 63 | Luftfeuchte         |  %  | 
| 5 | \anchor WGE WGE  | \anchor wge_T wge_T | 65 | Windgeschwindigkeit |  m/s  | 
| 6 | \ref cloud cloud | \anchor cloud_T cloud_T | 66 | Bewölkungsdichte | Achtel | 
| 7 | \ref typw typw   | \anchor typw_T typw_T   | 67 | Wolkentyp | siehe <a href="./pdf/QSimDoku_5Strahlung.pdf" target="_blank">Dokumentation Strahlung</a> | 
| - | \ref templ templ | \anchor tlmed_T tlmed_T | 62 | aktuelle Lufttemperatur aus temperl_wetter()  |  Grad Celsius  | 
| - | \anchor SCHWI SCHWI | \anchor schwi_T schwi_T | 64 | Globalstrahlung an der Wasseroberflaeche unter Beruecksichtigung der Reflektion an der Wasseroberflaeche aus strahlg_wetter() | cal/(cm2*h) | 


Der Eintrag der Wetter-Randbedingungen in die Felder der \ref lnk_uebergabewerte 
wird von temperw_huelle() nur deswegen vorgenommen, damit diese Werte an jeder
 Berechnungsstützstelle (Knoten oder Element) ausgegeben werden können.

## Zuordnung im Modell
Die Zuordnung der 3D-Stationswerte zu den Berechnungsstützstellen geschieht über 
das Feld \ref wetterstations_nummer, das basierend auf den Angaben in den 
T-Zeilen von <a href="./exp/MODELLG.3D.txt" target="_blank">MODELLG.3D.txt</a>,
zu jedem Zonen-zähler einen Wetterstations-Zähler bereitstellt.
Und jede Berechnungsstützstelle weiß (\ref knoten_zone oder \ref element_zone 
\ref point_zone) in welcher Zone sie liegt.

## Aufbereitung der Randvorgaben für den jeweiligen Zeitschritt
Die aktuellen Wetterdaten für Waermebilanz im jeweiligen Zeitschritt werden in
randbedingungen_setzen() ermittelt. Dabei werden die folgenden Subroutinen 
verwendet:

call wettles_wetter()  ! ersetzt wettles(), interpoliert Wetterdaten für den 
aktuellen Zeitpunkt *Änderungen in 13.3 nicht geprüft* 

call temperl_wetter()  ! ersetzt Temperl(), berechnet die aktuelle 
Lufttemperatur und legt sie in \ref tlmed_T ab. 
*Änderungen in 13.3 nicht geprüft*

call strahlg_wetter()  ! dient dem Aufruf von strahlg();
                         berechnet aus der Globalstrahlung \ref glob  den 
						 Strahlungsanteil \ref SCHWI, der im Gewässer ankommt. 
						 *Änderungen in 13.3 nicht geprüft*

## Wärmeeinleitungen
Im 1-dimensionalen QSim besteht die Möglichkeit Wärmeeinleitungen z.B. durch 
Kraftwerke direkt in der Temperaturberechnung zu berücksichtigen. 
Im mehrdimensionalen QSim3D muss dies über Randbedingungen vorgegeben werden. 
Das heißt einen Ausströmrand an dem der Volumenstrom entnommen wird und einem 
Einströmrand, an dem das erwärmte Wasser ins Gewässer (Modellgebiet) 
zurückfließt.

## Geänderte Feldindizierung QSim1D - QSim3D
In QSim1D werden die Wetterdaten unter der Stations-Kennnummer abgespeichert. 
In QSim3D unter dem Stationszähler.

Dadurch können auch hohe Kenn-Nummern verwendet werden, ohne unnötig 
Speicherplatz zu verschwenden.\n
Um die QSim-Routinen benutzen zu können ist das Feld iWSta erhalten geblieben; 
es speichert in QSim3D aber den Zähler.
Für die Kennnummer aus WETTER.txt ist ein neues Feld "Wetterstationskennung" 
eingeführt worden.

Textquelle: wetter.md ; Codesources: wetter.f95 ;  
zurück: \ref lnk_wtemp oder \ref lnk_modellerstellung
 