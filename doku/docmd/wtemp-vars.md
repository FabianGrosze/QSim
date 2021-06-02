Wassertemperatur - Formelzeichen/Variablennamen {#lnk_wtemp_vars}
========================================

## Liste der Formelzeichen und Variablennamen, Wassertemperatur:(Stand QSim xx.yy) ##

| Formelzeichen/ \n Variablen-Name   | Bedeutung	| Einheit | Wert | Variablenname \n Quellcode | Referenz | Herkunft    | 
| -------- | -------- | -------- | -------- | -------- | -------- | -------- |
| \f$ T_w \f$    | Wassertemperatur  		| °C  			| - | temperw	| | x |
| \f$ t \f$      | Zeit      				| h 			| - | tflie    	| | e |
| \f$ q_S \f$    | Wärmestromdichte \ref Strahlung | kJ/(h*m²)	| - | -     | | b |
| \f$ q_V \f$    | Wärmestromdichte Verdunstung (latente Wärme) | kJ/(h*m²)	| - | - | | b |
| \f$ q_K \f$    | Wärmestromdichte Konvektion | kJ/(h*m²)	| - | -         | | b |
| \f$ q_U \f$    | Wärmestromdichte Sohle 	| kJ/(h*m²) 	| - | -         | | b |
| \f$ q_{US}\f$  | Wärmestromdichte Reflektion Sohle | kJ/(h*m²) | - | -	| | b |
| \f$ q_E \f$    | Wärmestromdichte Einleitung | kJ/(h*m²) 	| - | -    		| | b |
| \f$ c_w \f$    | spez. Wärmekapazität Wasser | 4,1868  kJ/(kg*K) | speWKW  | | v |
| \f$ h \f$      | mittlere Wassertiefe 	|  m 			| - | tiefe   	| | e |
| \f$ \rho_w \f$ | Dichte des Wassers 		| 1000 kg/m3  	| - | rho		| | v |
| -------- | -------- | -------- | -------- | -------- | -------- | | 
| *a* | empirische Konstante | m/s*hPa; W/(m2 * mbar) | 0,13; 3,69 | - | WMO (1966), Sweers (1976) | | 
| *b* | empirische Konstante | m/s*hPa; W/(m2 * mbar) | 0,0936; 2,66 | - | WMO (1966), Sweers (1976) | | 
| \f$v_{wind} \f$  | Windgeschwindigkeit | m/s |  |  |  | | 
| \f$ p_S \f$ | Sättigungsdampfdruck bei der Wassertemperatur an der Wasseroberfläche | mbar |  |  |  | | 
| \f$ p_D \f$ | Partialdampfdruck bei der Lufttemperatur, gemessen am trockenen Thermometer | mbar |  |  |  | | 
| \f$ p_(L,Ort) \f$ | Luftdruck bezogen auf Ortshöhe | mbar |  |  |  | | 
| \f$ pL,Meer \f$ | Luftdruck bezogen auf Meereshöhe | mbar |  |  |  | | 
| *c* | Umrechnungsfaktor aus der Umrechnung der hV von mm/d in m/h | - | 1/24.000 |  |  | | 
| \f$ \f$ |  |  |  |  |  | | 



Herkunft:
+ x - Bilanzvariable QSim
+ b - berechnet, Zwischengröße / Übergabevariable
+ e - Eingabe
+ v - Vorgabe im Quellcode gesetzt

&nbsp
aus Datei: baustein-pars.md;

Code in Datei: baustein.f90 
