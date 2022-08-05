Benthische Verteilungen {#lnk_var_benthisch}
========================

In den Datenfeldern modell::benthic_distribution und 
modell::benthic_distribution_p werden alle Variablen gespeichert, die 
Eigenschaften der Sohle oder des Sediments beschreiben. 
Allgemeiner gesprochen handelt es sich um Verteilungen, die in jeder Vertikalen 
nur einmal auftreten und die nicht vom fließenden Wasser transportiert werden; 
so dass hier auch Eigenschaften der Gewässeroberfläche gespeichert werden 
können.

## benthische Verteilungen ##

Die QSim-3D Nummer bezieht sich auf die Datenfelder modell::benthic_distribution 
und  modell::benthic_distribution_p

Die QSim-1D Namen werden in QSim3d im module_QSimDatenfelder.f95 vereinbart.

| Nr.| Name			           | Beschreibung 			    | Dimension	   | Wertebereich |
|----|-------------------------|----------------------------|--------------|--------------|
| 1 | \anchor tsed tsed 	| Temperatur des Sediments					| °C		|
| 2 | \anchor sised sised	| Siliziumgehalt im Sediment				| mg/m² ??	|
| 3 | \anchor pfl pfl		| Pflanzentrockengewicht					| g/m² 		|
| 4 | \anchor ssdr ssdr		| Schwebstoffaufnahme durch Dreissena 		| mg/l je Zeitschritt	|
| 5 | \anchor ks ks			| ## außer Betrieb ## jetzt in: zone(:)%reib ##  Sandrauheit nach Nikuradse, hydraulischer Reibungsbeiwert	| m		|
| 6 | \anchor orgcsd orgcsd	| Gesamtmasse Kohlenstoff, die je Zeitschritt sedimentiert 	|  mgC/l je Zeitschritt |
| 7 | \anchor bsbbet bsbbet	| Sauerstoffverbrauch durch Organismen auf Makrophyten	??? (wohl unbenutzt) 	| Ausgabekonzentration		|
| 8	| \anchor hjo2 hjo2		| Sauerstofffluss ins Sediment				|  g O2/(m²*d)	|
| 9	| \anchor cmatki cmatki	| Abspülung benthischer kiesel-Algen ??		| 	|
|10	| \anchor cmatgr cmatgr	| Abspülung benthischer gruen-Algen ??		| 	|
|11	| \anchor alberg alberg	| Respiration benthischer gruen-Algen 		|  mgBio/l je Zeitschritt	|
|12	| \anchor alberk alberk	| Respiration benthischer kiesel-Algen 		|  mgBio/l je Zeitschritt	|
|13	| \anchor albewg albewg	| Wachstum benthischer gruen-Algen 		|  mgBio/l je Zeitschritt	|
|14	| \anchor albewk albewk	| Wachstum benthischer kiesel-Algen 		|  mgBio/l je Zeitschritt	|
|15	| \anchor resdr resdr	| Respirationsrate benthischer Filtrierer (Dreissena-Muscheln) 	| mgBio/l je Zeitschritt		|
|16 | \anchor hschlr hschlr	| (Ausgabe) Sauerstoffverlust im Wasser infolge Sauerstofffluss ins Sediment | mgO/(l*h) |
|17 | \anchor so2ein so2ein	| (Ausgabe) max. möglicher Sauerstoffgewinn im Wasser aus der Oberflächenbelüftung | mgO/l je Zeitschritt |
|18 | \anchor do2o2d do2o2d	| Aufnahmerate Sauerstoff aus Oberflächenbelüftung		| mgO/(l*d) |
|19 | \anchor o2ein1 o2ein1	| (Ausgabe) tatsächlicher Sauerstoffgewinn im Wasser aus der Oberflächenbelüftung |  mgO/l  je Zeitschritt |
|20	| \anchor abeowg abeowg	| Sauerstoffproduktion (Wachstum) benthischer Grünalgen   | mgO/l je Zeitschritt 	|
|21 | \anchor abeorg abeorg	| Sauerstoffverbrauch (Respiration) benthischer Grünalgen | mgO/l je Zeitschritt	|
|22	| \anchor abeowk abeowk	| Sauerstoffproduktion (Wachstum)  benthischer Kieselalgen | mgO/l je Zeitschritt 	|
|23 | \anchor abeork abeork	| Sauerstoffverbrauch (Respiration) benthischer Kieselalgen	| mgO/l je Zeitschritt	|
|24 | \anchor ro2dr ro2dr	| Respiration Dreissena-Muscheln		| mgO/l je Zeitschritt 	|
|25 | \anchor siruek siruek	| Rückgelöste Menge Silikat-Silizium	| 	|
|26 | \anchor sedalk sedalk	| Sedimentierte Menge an Kiesel-Algen 	| mgBio/l je Zeitschritt	|
|27 | \anchor sedalg sedalg	| Sedimentierte Menge an Grün-Algen 	| mgBio/l je Zeitschritt	|
|28 | \anchor sedalb sedalb	| Sedimentierte Menge an Blau-Algen 	| mgBio/l je Zeitschritt	|
|29 | \anchor exdrvk exdrvk	| exkretierte Biomasse der Muscheln beim Verzehr von Kiesel-Algen | mgBio/l je Zeitschritt 	|
|30 | \anchor exdrvg exdrvg	| exkretierte Biomasse der Muscheln beim Verzehr von Grün-Algen |  mgBio/l je Zeitschritt	|
|31 | \anchor exdrvb exdrvb	| exkretierte Biomasse der Muscheln beim Verzehr von Blau-Algen | mgBio/l je Zeitschritt	|
|32	| \anchor hjpo4 hjpo4	| Phosphat-Freisetzung aus dem Sediment | gP/(m²*d)	|
|33	| \anchor sedx0 sedx0	| sedimentierte Nitrosomonasbiomasse, nur Ausgabewert |  in µg/l	|
|34 | \anchor bettn bettn	| OXYDIERTE STICKSTOFFMENGE AM GEWAESSERBETT 	| 	|
|35	| \anchor hjno3 hjno3	| Nitrat-Freisetzung aus dem Sediment 			|  gN/(m²*d) 	|
|36	| \anchor hjnh4 hjnh4	| Ammonium-Freisetzung aus dem Sediment 		|  gN/(m²*d)	|
|37 | \anchor hflun3 hflun3	| Ausgabe NitratFlux Wasser/Sediment 			|  in mgN/(l*h)	|
|38 | \anchor algdrk algdrk	| \ref lnk_dreissena (Muscheln) Fraßrate Dreissena	|  in mg/l	 	|
|39 | \anchor algcok algcok	| Kiesel-Algen Konsum durch Corophium ? 	| 	|
|40 | \anchor algdrg algdrg	| grün-Algen-Konsum-bentisch (Muscheln)		|  in mg/l 	|
|41	| \anchor algdrb algdrb	| blau-Algen-Konsum-bentisch (Muscheln)		|  in mg/l 	|
|42 | \anchor algcog algcog	| grün-Algen Konsum durch Corophium ? 		| 	|
|43 | \anchor algcob algcob	| blau-Algen Konsum durch Corophium ? 		| 	|
|44 | \anchor kst kst 		| ## ausser Betrieb ## Strickler-Beiwert aus Nikuradse Sandrauheit umgerechnet	| (m**(1/3))/s	|
|45 | \anchor utau utau 	| Sohlschubspannung 						| N/m²	|
|46 | \anchor hjsi hjsi 	| Silizium-Flux aus dem Sediment 			| gSi/(m²*d)	|
|47 | \anchor hjn2 hjn2 	| N2 Flux vom Sediment in den Wasserkörper	| gN/(m²*d)	|
|48	| \anchor jdoc1 jdoc1 	| Flux gelöster org. Kohlenstoffe aus dem Sediment, leicht abbaubar	| g ??? (m²*d)	|
|49	| \anchor jdoc2 jdoc2 	| Flux gelöster org. Kohlenstoffe aus dem Sediment, schwer abbaubar	| g ??? (m²*d)	|
|50 | \anchor orgcsd0 orgcsd0 | teil des? sedimentierten organ. Material |  mgC/l je Zeitschritt ??|
|51	| \anchor orgcsd_abb orgcsd_abb	| sedimentiertes biologisch abbaubares organ. Material |  mgC/l je Zeitschritt ??|
|52	| \anchor sedalg_mq sedalg_mq	| ?? 		|  ?? |
|53	| \anchor sedalk0 sedalk0       | ?? 		|  ?? |
|54	| \anchor coroi coroi	        | Corophium Böschung  |  ?? |
|55	| \anchor corois corois	        | Corophium Sohle	  |  ?? |
|56	| \anchor zdreis zdreis	        | Dreissenabiomasse pro Fläche Sohle (0. Kohorte) |  gBio/m² |
|57	|  zdreis 	| (1. Kohorte) 		|   |
|58	|  zdreis 	| (2. Kohorte) 		|   |
|59	|  zdreis 	| (3. Kohorte) 		|   |
|60	| \anchor zdrei zdrei	| Dreissenabiomasse pro Fläche Böschung (0. Kohorte) | gBio/m² |
|61	|  zdrei 	| (1. Kohorte) 		|   |
|62	|  zdrei 	| (2. Kohorte) 		|   |
|63	|  zdrei 	| (3. Kohorte) 		|   |
|64	| \anchor gewdr gewdr	| Gewicht einer Dreissena-Muschel (0. Kohorte) | mg |
|65	|  gewdr 	| (1. Kohorte) |  |
|66	|  gewdr 	| (2. Kohorte) |  |
|67	|  gewdr 	| (3. Kohorte) |  |
|68	| \anchor dlmax dlmax	| Dreissena Larven ??		|  ?? |
|69	| \anchor dlmaxs dlmaxs	| Dreissena Larven ??  		|  ?? |
|70	| \anchor gwdmax gwdmax	| Dreissena Larven ??  		|  ?? |
|71	| \anchor sgwmue sgwmue	| Dreissena Larven ??  		|  ?? |
|72	| \anchor abegm2 abegm2	| Biomasse benthischer Grünalgen  	| gBio/m³ |
|73	| \anchor abekm2 abekm2 | Biomasse benthischer Kieselalgen 	| gBio/m³ |

Variablendefinition in module_modell.f95

*siehe dazu auch:* stofftransport()

aus Datei: var_benthisch.md ; Code in: benthische_verteilungen.f95; 
zurück: \ref lnk_datenstruktur 
