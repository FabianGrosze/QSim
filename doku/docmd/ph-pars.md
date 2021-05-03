pH - Formelzeichen/Variablennamen {#lnk_ph_pars}
==================================

## Liste der Formelzeichen und Variablennamen des pH-Bausteins ##

| Formelzeichen/ \n Variablen-Name | Bedeutung | Einheit | Wert | Variablenname \n Quellcode | Herkunft | 
| ------ | --------| -------| --------| ------- | ------- | 
| \f$ pH \f$  | pH-Wert (negativer dekadischer Logarithmus der Protonenkonzentration) | mol/l | - | vph   |  x |
| \f$ Ca \f$  | Calcium   | mg/l | - | Ca  | x |
| \f$ \Delta Ca \f$ | Calcium-Änderung pro Zeitschritt| mg/l	| - | dca | b |
| \f$ lf \f$        | Leitfähigkeit	| \f$ \mu S/cm\f$|  - | lf | x |
| \f$ m \f$      	| m-Wert Säurekapazität		| mmol/l		| - | mw    		| x |
| \f$ p \f$      	| p-Wert Basenkapazität		| mmol/l		| - | pw    		| x |
| \f$ DIC \f$   	| gelöster inorganischer Kohlenstoff |mol/l | - | c				| b |
| \f$ CO_2 \f$   	| gel. Kohlendioxid + Kohlensäure | mol/l  	| - | moco2			| b |
| \f$ HCO_3^- \f$   | Hydrogencarbonat  		| mol/l    		| - | mohco3		| b |
| \f$ CO_3^{2-} \f$ | Carbonat  				| mol/l    		| - | moco3			| b |
| \f$ k_1 \f$    	| Säurekonstante  			| mol/l    		| - | k1			| b |
| \f$ k_2 \f$    	| Säurekonstante  			| mol/l    		| - | k2			| b |
| \f$ k_{ca} \f$ 	| Löslichkeitsprodukt Calcium * Carbonat | \f$ mol^2/l^2 \f$| - | kca	| b |
| \f$ H^+ \f$    	| Protonenkonzentration		| mol/l    		| - | h				| b |
| \f$ OH^- \f$   	| Hydroxid  				| mol/l    		| - | oh			| b |
| \f$ T \f$      	| abs. Wassertemperatur  	| K      		| - | abst=tempw+273.16| x |
| \f$ t \f$ , \f$ \Delta t \f$| Zeit, Zeitschritt | d 			| - | - , tflie 	| e |
| \f$ \Delta N \f$  | oxidiertes Ammonium pro Zeitschritt | mgN/l | - | susns		| b |
| \f$ pk_w\f$ 		| Dissoziationskonstante Wasser | - 		| - | pkw 			| b |
| \f$ hk \f$ 		| Leitfähigkeitskorrektur 	| - 			| - | hk 			| b |
| \f$ stind \f$ 	| Alter						| min 			| - | stind 		| x |
| \f$ ssalgs\f$ 	| gesamt-Schwebstoff		| mg/l 			| - | ssalg 		| x |
| \f$C_{aki}\f$,\f$C_{agr}\f$,\f$C_{abl}\f$|Kohlenstoffgehalte Algen| gC/gBio| 0,48 |caki,cagr,cabl|v |
|\f$C_{dr}\f$	|Kohlenstoffgehalt Muscheln	| gC/gBio	| 0,38|cdr			| v |
|\f$C_{rot}\f$ |Kohlenstoffgehalt Konsumenten| gC/gBio	| 0,45 | crot | v |

Herkunft:
+ x - Bilanzvariable QSim
+ b - berechnet, Zwischengröße / Übergabevariable
+ e - Eingabe
+ v - Vorgabe im Quellcode gesetzt


aus Datei ph-pars.md
