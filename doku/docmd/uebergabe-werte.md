Übergabewerte {#lnk_uebergabewerte}
=============

Mit Übergabewerten sind alle Feldgrößen gemeint, die nur zwischen den Modulen 
ausgetauscht werden.
Diese Variablen, zumeist Konzentrationen, werden im nächsten Zeitschritt neu 
berechnet und müssen daher nicht transportiert werden.

Des Weiteren enthält QSim auch Übergabewerte, die zwischen den Modulen übergeben 
werden, aber nicht im Raum verteilt sind.

siehe dazu auch: \ref lnk_ueberblick

## Übergabe Konzentrationen, tiefengemittelt \anchor tiefengemittelte_übergabe_variable 

Die QSim-3D Nummer bezieht sich auf die Datenfelder modell::transfer_quantity 
und  modell::transfer_quantity_p

Die QSim-1D Namen werden in QSim3D im module_QSimDatenfelder.f95 vereinbart. 

| QSim3D Nr.| QSim1D Name	 | Beschreibung | Dimension	| von -> nach |
|-----------|----------------|--------------|-----------|-------------|
|  1 | \anchor bsbt bsbt	 | Sauerstoffverbrauch durch Kohlenstoffabbau | mgO2/l pro Zeitschritt	| orgc -> ph, oxygen |
|  2 | \anchor bsbctp bsbctp | ortho-Phophat-Phosphor-Freisetzung beim Abbau org. Kohlenstoffverbidungen | mgP/l je Zeitschritt 	| orgc -> po4s |
|  3 | \anchor don don		 | Ammoniumfreisetzung beim Abbau org. Kohlenstoffverbidungen | mgN/l je Zeitschritt | orgc -> ncyc |
|  4 | \anchor bacmua bacmua | Ausgabe:Summe Aufnahme+Respirations Rate heterotrophe Bakterien | 1/d | orgc -> Ausgabe |
|  5 | 			| 			| 				|  |
|  6 | \anchor abszo abszo   | Absterberate Zooplankton | Ind. je Liter und Zeitschritt | konsum -> orgc |
|  7 | \anchor dkimor dkimor | Abnahme infolge \ref lnk_phy_Mortalitaet, Kiesel-Algen-Biomasse | mgBio/l je Zeitschritt | algaeski -> orgc |
|  8 | \anchor dgrmor dgrmor | Abnahme infolge \ref lnk_phy_Mortalitaet, Grünalgen	| mgBio/l je Zeitschritt | algaesgr -> orgc |
|  9 | \anchor dblmor dblmor | Abnahme infolge \ref lnk_phy_Mortalitaet, Blaualgen | mgBio/l je Zeitschritt | algaesbl -> orgc |
| 10 | \anchor bsbhnf bsbhnf | Absterben und Exkretion Heterotropher Nanoflagelaten	| mgC/l je Zeitschritt | hnf -> orgc |
| 11 | \anchor hnfbac hnfbac | Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen, die in jedem Zeitschritt infolge Wegfraß durch heterotrophe Nanoflagelaten verloren geht | mgC/l je Zeitschritt |  hnf -> orgc |
| 12 | 			| 	 		|  				| 	|
| 13 | \anchor drfaek drfaek | Ausscheidungen der Dreissena-Muscheln infolge Konsums von Kieselalgen | mgBio/l je Zeitschritt | dreissen -> orgc,schweb |
| 14 | \anchor drfaeg drfaeg | Ausscheidungen der Dreissena-Muscheln infolge Konsums von Grünalgen   | mgBio/l je Zeitschritt | dreissen -> orgc,schweb |
| 15 | \anchor drfaeb drfaeb | Ausscheidungen der Dreissena-Muscheln infolge Konsums von Blaualgen   | mgBio/l je Zeitschritt | dreissen -> orgc,schweb |
| 16 | \anchor zexki zexki	 | Ausscheidungen der Rotatorien infolge Konsums von Kieselalgen | mgBio/l je Zeitschritt | konsum -> orgc |
| 17 | \anchor zexgr zexgr	 | Ausscheidungen der Rotatorien infolge Konsums von Grünalgen 	 | mgBio/l je Zeitschritt | konsum -> orgc |
| 18 | \anchor zexbl zexbl	 | Ausscheidungen der Rotatorien infolge Konsums von Blaualgen 	 | mgBio/l je Zeitschritt | konsum -> orgc |
| 19 | \anchor dorgss dorgss | Abnahme! von suspendierten Sedimenten infolge Abbau von C-Verbindungen| mg SS /l je Zeitschritt  	| orgc -> schweb 	|
| 20 | \anchor dalgki dalgki | Zunahme infolge \ref lnk_phy_wachstum, Kiesel-Algen-Biomasse | mgBio/l je Zeitschritt | algaeski -> oxygen	|
| 21 | \anchor dalggr dalggr | Zunahme infolge \ref lnk_phy_wachstum, Grün-Algen 			  | mgBio/l je Zeitschritt | algaesgr -> oxygen	|
| 22 | \anchor dalgbl dalgbl | Zunahme infolge \ref lnk_phy_wachstum, Blau-Algen 			  | mgBio/l je Zeitschritt | algaesbl -> oxygen	|
| 23 | \anchor dalgak dalgak | Abnahme infolge \ref lnk_phy_Respiration, Kiesel-Algen-Biomasse | mgBio/l je Zeitschritt | algaeski -> oxygen |
| 24 | \anchor dalgag dalgag | Abnahme infolge \ref lnk_phy_Respiration, Grün-Algen 			 | mgBio/l je Zeitschritt | algaesgr -> oxygen |
| 25 | \anchor dalgab dalgab | Abnahme infolge \ref lnk_phy_Respiration, Blau-Algen 			 | mgBio/l je Zeitschritt | algaesbl -> oxygen |
| 26 | \anchor vco2 vco2	 | Kohlendioxyd | mg/l | algaeski -> ph |
| 27 | \anchor dzres1 dzres1 | Grund-Respiration des Zoo-Planktons	| mgBio/(l*d) | konsum -> ph,ncyc,po4s|
| 28 | \anchor dzres2 dzres2 | Fraßabhängige Respirationsrate des Zoo-Planktons | mgBio/(l*d) | konsum -> ph,ncyc,po4s|
| 29 | \anchor susn susn	 | Durch SUSPendierte NITRIFikanten OXIDIERTE AMMONIUMMENGE | ? | ncyc -> ph |
| 30 | \anchor po2p po2p	 | Sauerstoffproduktion durch Makrophyten 				 | mgO/l je Zeitschritt | mphyt -> oxygen |
| 31 | \anchor po2r po2r	 | Sauerstoffverbrauch durch Makrophyten 				 | mgO/l je Zeitschritt | mphyt -> oxygen |
| 32 | \anchor go2n go2n	 | O2-Verbrauch durch Nitrifikation NH4N -> NO2N -> NO3N | mgO/l je Zeitschritt | ncyc -> oxygen  |
| 33 | \anchor akinh4 akinh4 | Ammoniumaufnahme der kiesel-Algen, tiefengem. 		 | mgN/l je Zeitschritt	| ncyc -> oxygen  |
| 34 | \anchor agrnh4 agrnh4 | Ammoniumaufnahme der gruen-Algen, tiefengem. 		 | mgN/l je Zeitschritt | ncyc -> oxygen  |
| 35 | \anchor ablnh4 ablnh4 | Ammoniumaufnahme der blau-Algen, tiefengem. 			 | mgN/l je Zeitschritt | ncyc -> oxygen  |
| 36 | \anchor akino3 akino3 | Nitrataufnahme der kiesel-Algen 	| mgN/l je Zeitschritt  | ncyc -> oxygen |
| 37 | \anchor agrno3 agrno3 | Nitrataufnahme der gruen-Algen 	| mgN/l je Zeitschritt  | ncyc -> oxygen |
| 38 | \anchor ablno3 ablno3 | Nitrataufnahme der blau-Algen 	| mgN/l je Zeitschritt  | ncyc -> oxygen |
| 39 | \anchor salgo salgo	 | Summe Sauerstoffeintrag Algen  ** | mgO2/l je Zeitschritt | oxygen -> Ausgabe |
| 40 | \anchor dalgo dalgo 	 | Sauerstoffproduktion der Gruen-, Kiesel-, und Blaualgen | mgO2/l je Zeitschritt | oxygen -> Ausgabe |
| 41 | \anchor dalgao dalgao | Respiration (Sauerstoffverbrauch) der Gruen-, Kiesel-, und Blaualgen | mgO2/l je Zeitschritt | oxygen -> Ausgabe |
| 42 | \anchor ir ir		 | Ingestionsrate der Rotatorien  	| mg/(l*h) 	| konsum -> algaeski,algaesgr,algaesbl |
| 43 | \anchor zooro2 zooro2 | Sauerstoffverbrauch durch Zooplanktonrespiration | mgO2/l je Zeitschritt | oxygen -> Ausgabe |
| 44 | \anchor ro2hnf ro2hnf | Sauerstoffverbrauch durch Respiration HNF 		| mgO2/l je Zeitschritt	| hnf -> oxygen |
| 45 | \anchor saett saett	 | Sauerstoff Sättigungs-Konzentration 				| mgO2/l  			| oxygen -> Ausgabe |
| 46 | \anchor fluxt1 fluxt1 | Wärmefluss tiefenintegriert ??? wohl nur Rückgabewert  | ??				| temperw -> Ausgabe |
| 47 | \anchor bsbct bsbct	 | mineralisierter Kohlenstoffgehalt in der Wassersäule	  | mgC/l je Zeitschritt   | orgc -> ncyc |
| 48 | \anchor akitbr akitbr | Nettowachstum Kieselalgen (Bruttowachstum-Respiration) | mgBio/l je Zeitschritt | algaeski -> ncyc,po4s,silikat,Ausgabe |
| 49 | \anchor agrtbr agrtbr | Nettowachstum Grünalgen (Bruttowachstum-Respiration)   | mgBio/l je Zeitschritt | algaesgr -> ncyc,po4s,silikat,Ausgabe |
| 50 | \anchor abltbr abltbr | Nettowachstum Blaualgen (Bruttowachstum-Respiration)   | mgBio/l je Zeitschritt | algaesbl -> ncyc,po4s,silikat,Ausgabe |
| 51 | \anchor sgo2n sgo2n	 | Aufsummierter Nitrifikationssauerstoffverbrauch ??	  | ?? 	| ncyc -> Ausgabe |
| 52 | \anchor susno susno	 | Ausgabewert Sauerstoffverbr. d. Nitrifik. in der Wassersäule | mgO2/(l*h) | ncyc -> Ausgabe |
| 53 | \anchor algzok algzok | Kiesel-Algen-Konsum des Zoo-Planktons  				 | mgBiom./l je Zeitschritt	| konsum -> algaeski |
| 54 | \anchor extk extk	 | (Licht) Extinktionskoeffizient 1D:aufsummiert zu XFIG | 1/m	| algaeski -> temperw,albenth,mphyt |
| 55 | \anchor tpki tpki	 | wird zu XNAEHR, Naehrstoffabhängigkeit bezogen auf Hellphase, aufsummiert | - | algaeski -> Ausgabe |
| 56 | \anchor akmuea akmuea | Bruttowachstumsrate der Kieselalgen akgrow, aufsummiert zu XAKMUA	| 1/d  | algaeski -> Ausgabe |
| 57 | \anchor ftaaus ftaaus | Temperaturabhaengigkeit der Wachstumsrate fta 			| -	| algaeski -> Ausgabe |
| 58 | \anchor fiaus fiaus	 | wird zu XFIK, Lichtabhängigkeit der Algenproduktion bezogen auf Hellphase, aufsummiert| - | algaeski -> Ausgabe |
| 59 | \anchor fheaus fheaus | Lichthemmung der Kieselalgen, svhemk, aufsummiert zu XFHEK		|  -  | algaeski -> Ausgabe |
| 60 | \anchor akraus akraus | Respirationsrate der Kieselalgen, akres, aufsummiert zu XAKRAU	| 1/d | algaeski -> Ausgabe |
| 61 | \anchor dz2d dz2d	 | vertikalen Dispersionskoeffizient, inaktiv in tiefengemittelten Sim.	| m²/s | k_eps -> algaeski |
| 62 | \anchor templ templ	 | Lufttemperatur  					  | °C | temperl -> temperw |
| 63 | \anchor ro ro		 | Luftfeuchte aus \ref ro_T 		  | %	 | update_weather -> temperw |
| 64 | \anchor schwi schwi	 | Globalstrahlung an der Wasseroberflaeche unter Beruecksichtigung der Reflektion an der Wasseroberflaeche \ref schwi_T | [cal/(cm2*h)| update_weather -> temperw |
| 65 | \anchor wge wge		 | Windgeschwindigkeit aus \ref wge_T | m/s	| update_weather -> temperw |
| 66 | \anchor cloud cloud	 | Bewölkungsdichte aus \ref cloud_T  | 1/8	| update_weather -> temperw |
| 67 | \anchor typw typw	 | Wolkentyp aus \ref typw_T 		  | -	| update_weather -> temperw |
| 68 | 			| 				 					|			  | update_weather -> temperw |
| 69 | 			| 				 					|			  |    |
| 70 | 			| 					 				|			  |    |
| 71 | 			| 			 						|			  |    |
| 72 | \anchor algzog algzog | Gruen-Algen-Konsum des Zoo-Planktons | mgBio/l je Zeitschritt | konsum -> algaesgr |
| 73 | \anchor algzob algzob | Blau-Algen-Konsum des Zoo-Planktons 	| mgBio/l je Zeitschritt | konsum -> algaesbl |
| 74 | \anchor zhnf zhnf	 | Zooplankton frisst HNF 				| mgC/l  je Zeitschritt	 | konsum -> hnf |
| 75 | \anchor hnfza hnfza	 | HNFza(ior) = (zHNF(ior)/CHNF(ior))*24. | *unstimmig ??* 		 | konsum -> Ausgabe |
| 76 | \anchor rmuas rmuas	 | = mueRot-respRg , Netto-Wachstumsrate des Zooplanktons, aufsummiert zu XRMUE| 1/d | konsum -> Ausgabe |
| 77 | \anchor rakr rakr	 | = iras(ior)*respaR , Aktive Respirationsrate des Zooplanktons ?fraßabhängig? , aufsummiert zu XRAKR| 1/d | konsum -> Ausgabe |
| 78 | \anchor rbar rbar	 | = respRg, Basale Respirationsrate des Zooplanktons, aufsummiert zu XRBAR| 1/d | konsum -> Ausgabe |
| 79 | \anchor iras iras	 | Ingestionsrate des Zooplanktons, aufsummiert zu XIR			| 1/d 			 | konsum -> Ausgabe |
| 80 | \anchor tpgr tpgr	 | wird zu XNAEHR, Naehrstoffabhängigkeit bezogen auf Hellphase, aufsummiert| -	 | algaesgr -> Ausgabe |
| 81 | \anchor tpbl tpbl	 | wird zu XNAEHR, Naehrstoffabhängigkeit bezogen auf Hellphase, aufsummiert| -	 | algaesbl -> Ausgabe |
| 82 | \anchor figaus figaus | wird zu XFIK, Lichtabhängigkeit der Algenproduktion bezogen auf Hellphase, aufsummiert| - | algaesgr -> Ausgabe |
| 83 | \anchor fibaus fibaus | wird zu XFIK, Lichtabhängigkeit der Algenproduktion bezogen auf Hellphase, aufsummiert| - | algaesbl -> Ausgabe |
| 84 | \anchor agmuea agmuea | Brutto-Wachstumsrate Grünalgen aggrow, aufsummiert zu XAGMUA | 1/d | algaesgr -> Ausgabe |
| 85 | \anchor abmuea abmuea | Brutto-Wachstumsrate Blaualgen abgrow, aufsummiert zu XABMUA	| 1/d | algaesbl -> Ausgabe |
| 86 | \anchor fhegas fhegas | Lichthemmung der Grünalgen, svhemg, aufsummiert zu XFHEG		| -	  | algaesgr -> Ausgabe |
| 87 | \anchor fhebas fhebas | Lichthemmung der Blaualgen, svhemb, aufsummiert zu XFHEB		| -	  | algaesbl -> Ausgabe |
| 88 | \anchor agreau agreau | Respirationsrate der Grünalgen, agres, aufsummiert zu XAGREA	| 1/d | algaesgr -> Ausgabe |
| 89 | \anchor abreau abreau | Respirationsrate der Blaualgen, abres, aufsummiert zu XABREA	| 1/d | algaesbl -> Ausgabe |
| 90 | \anchor dc_denw dc_denw | C-Abbau durch Denitrifikation in der Wassersäule 	| mgC/l je Zeitschritt | ncyc -> oxygen |
| 91 | \anchor zbac zbac	 | Zooplankton frisst Bakterien	| mgC/l je Zeitschritt | konsum -> orgc |
| 92 | 			| 									| 	 			|    |
| 93 | 			| 									| 	 			|    |
| 94 | 			| 							 		| 	 			|    |
| 95 | \anchor drfaes drfaes | Ausscheidungen der Dreissena-Muscheln infolge Konsums von Schwebstoffen | mgBio/l je Zeitschritt | dreissen -> orgc,schweb |
| 96 | \anchor drhnf drHNF	 | Dreissena-Muscheln fressen HNF | mgC/l  je Zeitschritt | dreissen -> hnf |


## Tiefenaufgelöste Übergabe Konzentrationen \anchor tiefenaufgelöste_übergabe_variable

Die QSim-3D Nummer bezieht sich auf die Datenfelder modell::trans_quant_vert und  
modell::trans_quant_vert_p

Die QSim-1D Namen werden in QSim3D im module_QSimDatenfelder.f95 vereinbart.

| Nr.| Name	                 | Beschreibung 				| Dimension | von -> nach |
|----|-----------------------|------------------------------|-----------|-------------|
|  1 | \anchor up_nkz up_nkz | N-Aufnahmerate der Algengruppe kiesel  | | algaeski -> ncyc |
|  2 | \anchor up_ngz up_ngz | N-Aufnahmerate der Algengruppe gruen   | |  -> ncyc |
|  3 | \anchor up_nbz up_nbz | N-Aufnahmerate der Algengruppe blau    | |  -> ncyc |
|  4 | \anchor up_siz up_siz | Si-Aufnahmerate der Algengruppe kiesel | mgSi/(mgBio*d ) | algaeski -> silikat |
|  5 | \anchor up_pkz up_pkz | P-Aufnahmerate der Algengruppen kiesel | mgP/(mgBio*d)   | algaeski -> po4s |
|  6 | \anchor up_pgz up_pgz | P-Aufnahmerate der Algengruppen gruen  | mgP/(mgBio*d)   | algaesgr-> po4s |
|  7 | \anchor up_pbz up_pbz | P-Aufnahmerate der Algengruppen blau   | mgP/(mgBio*d)   | algaesbl-> po4s |
|  8 | \anchor up_n2z up_n2z | Aufnahmerate von Luftstickstoff durch Blaualgen   |      | algaesbl-> ncyc |
|  9 | \anchor aknh4z aknh4z | Ammoniumaufnahme Algengruppe kiesel    | | -> |
| 10 | \anchor agnh4z agnh4z | Ammoniumaufnahme Algengruppe gruen     | | -> |
| 11 | \anchor abnh4z abnh4z | Ammoniumaufnahme Algengruppe blau      | | -> |
| 12 | \anchor dalgkz dalgkz | Algen-Wachstum Algengruppe kiesel | mgBiom./l je Zeitschritt | -> |
| 13 | \anchor dalggz dalggz | Algen-Wachstum Algengruppe gruen  | mgBiom./l je Zeitschritt | -> |
| 14 | \anchor dalgbz dalgbz | Algen-Wachstum Algengruppe blau   | mgBiom./l je Zeitschritt | -> |
| 15 | \anchor akno3z akno3z | Nitrataufnahme Algengruppe kiesel | | -> |
| 16 | \anchor agno3z agno3z | Nitrataufnahme Algengruppe gruen  | | -> |
| 17 | \anchor abno3z abno3z | Nitrataufnahme Algengruppe blau   | | -> |
| 18 | \anchor algakz algakz | Respirierte Algenbiomasse kiesel  | mgBio/l je Zeitschritt | -> |
| 19 | \anchor algagz algagz | Respirierte Algenbiomasse grün    | mgBio/l je Zeitschritt | -> |
| 20 | \anchor algabz algabz | Respirierte Algenbiomasse blau    | mgBio/l je Zeitschritt | -> |
| 21 | \anchor vz1 vz1		 | lokale Sauerstoffzehrung/produktion (tiefenprofil ausserhalb oxygen)    | | -> |
| 22 | \anchor dtemp dtemp	 | lokaler Wärmeeintrag, tiefenaufgelöst (tiefenprofil ausserhalb temperw) | | -> |
| 23 | \anchor akibrz akibrz | Brutto-Zuwachs Kiesel-Algen-Biomasse | mgBio/l je Zeitschritt | -> |
| 24 | \anchor agrbrz agrbrz | Brutto-Zuwachs Grün-Algen-Biomasse   | mgBio/l je Zeitschritt | -> |
| 25 | \anchor ablbrz ablbrz | Brutto-Zuwachs Blau-Algen-Biomasse 	| mgBio/l je Zeitschritt | -> |
| 26 | \anchor algzkz algzkz | Kiesel-Algen-Konsum durch Zoo-Plankton | mg/l | -> |
| 27 | \anchor algzgz algzgz | Grün-Algen-Konsum durch Zoo-Plankton   | mg/l | -> |
| 28 | \anchor algzbz algzbz | Blau-Algen-Konsum durch Zoo-Plankton   | mg/l | -> |


## Übergabe Werte \anchor übergabe_wert
Die QSim-3D Nummer bezieht sich auf die Datenfelder modell::transfer_value und  
modell::transfer_value_p

Die QSim-1D Namen werden in QSim3D im module_QSimDatenfelder.f95 vereinbart.

WIRD AUFGELÖST ####

| Nr.| Name			| Beschreibung 						| Dimension 	| von -> nach 	|
|----|--------------|-----------------------------------|---------------|---------------|
|  1 | 			    | leer   							|		        |       		|
|  2 | \anchor nzoo nzoo	 | Stickstoffanteil in der Rotatorienbiomasse | mgN/mgBiom.	| ini_ueber -> ncyc,orgc |
|  3 | \anchor pZoo pZoo	 | Phosphoranteil in der Rotatorienbiomasse   | mgP/mgBiom.	| ini_ueber -> po4s,orgc |
|  4 | \anchor bk1 bk1		 | parameter bk1 orgc_modul  | | -> |
|  5 | \anchor bk2 bk2		 | parameter bk2 orgc_module | | -> |
|  6 | \anchor saettk Saettk | ??? 						 | | algeski -> Rückgabewert ?? |
|  7 | \anchor tauscs tauscs | Schiffseinfluss qsim.f90: tauscs = 1.25 | |  ->  |
|  8 | \anchor saettg Saettg | ??? 						 | | algesgr -> Rückgabewert ?? |
|  9 | \anchor saettb Saettb | ??? 						 | | algesbl -> Rückgabewert ?? |
| 10 | \anchor it_h it_h	 | Anzahl der Zeitschritte während der Hellphase (unbenutzt) |-| strahlg -> algaeski |

<b> \ref lnk_globale_parameter von APARAM.txt </b>

<hr>
** Im Programmcode werden die Sauerstoffänderungsraten nach meinem Dafürhalten 
als Differenzen je Zeitschritt verwendet. die Gerris-Oberfläche spricht aber von 
Raten je Stunde ???Jens

Variablendefinition in module_modell.f95

siehe auch: stofftransport()

aus Datei ubergabe-werte.md ; Code in: uebergabe_werte.f95; 
zurück: \ref lnk_datenstruktur
