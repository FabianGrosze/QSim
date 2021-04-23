!
!----------------------------------------------------------------------------------------- uebergabekonzentrationen
!> \page uebergabe_werte Übergabe-Werte
!! damit sind alle Feldgrößen gemeint, 
!! die nur zwischen den modulen ausgetauscht werden.
!! Diese Variablen, zumeist Konzentrationen, werden im nächsten Zeitschritt neu berechnet
!! und müssen daher nicht transportiert werden.\n\n
!! Desweiteren enthält es auch Übergabewerte, die zwischen den Modulen übergeben werden, 
!! aber nicht im Raum verteilt sind.
!! \n\n
!! siehe dazu auch: \ref lnk_ueberblick
!! \n\n
!! <h2> Übergabe Konzentrationen, tiefengemittelt</h2> \anchor tiefengemittelte_übergabe_variable 
!! Die QSim-3D Nummer bezieht sich auf die Datenfelder modell::transfer_quantity und  modell::transfer_quantity_p\n 
!! Die QSim-1D Namen werden in QSim3D im module_QSimDatenfelder.f95 vereinbart.\n 
!!<table >
!!<tr><th> QSim3D Nr.</th><th> QSim1D Name	</th><th> Beschreibung 								</th><th> Dimension 			</th><th> von -> nach </th></tr>
!!<tr><td>  1 </td><td> \anchor bsbt bsbt	</td><td> Sauerstoffverbrauch durch Kohlenstoffabbau 				</td><td> mgO2/l pro Zeitschritt	</td><td> orgc -> ph, oxygen </td></tr>
!!<tr><td>  2 </td><td> \anchor bsbctp bsbctp	</td><td> ortho-Phophat-Phosphor-Freisetzung beim Abbau org. Kohlenstoffverbidungen </td><td> mgP/l je Zeitschritt 	</td><td> orgc -> po4s </td></tr>
!!<tr><td>  3 </td><td> \anchor don don		</td><td> Ammoniumfreisetzung beim Abbau org. Kohlenstoffverbidungen 		</td><td> mgN/l je Zeitschritt 		</td><td> orgc -> ncyc </td></tr>
!!<tr><td>  4 </td><td> \anchor bacmua bacmua	</td><td> Ausgabe:Summe Aufnahme+Respirations Rate heterotrophe Bakterien 	</td><td> 1/d 				</td><td> orgc -> Ausgabe </td></tr>
!!<tr><td>  5 </td><td> 			</td><td> 									</td><td> 				</td><td>  </td></tr>
!!<tr><td>  6 </td><td> \anchor abszo abszo	</td><td> Absterberate Zooplankton  						</td><td> Ind. je Liter und Zeitschritt </td><td> konsum -> orgc </td></tr>
!!<tr><td>  7 </td><td> \anchor dkimor dkimor	</td><td> Abnahme infolge \ref Algen-Mortalität, Kiesel-Algen-Biomasse 		</td><td> mgBio/l je Zeitschritt 	</td><td> algaeski -> orgc </td></tr>
!!<tr><td>  8 </td><td> \anchor dgrmor dgrmor	</td><td> Abnahme infolge \ref Algen-Mortalität, Grünalgen 			</td><td> mgBio/l je Zeitschritt 	</td><td> algaesgr -> orgc </td></tr>
!!<tr><td>  9 </td><td> \anchor dblmor dblmor	</td><td> Abnahme infolge \ref Algen-Mortalität, Blaualgen 			</td><td> mgBio/l je Zeitschritt 	</td><td> algaesbl -> orgc </td></tr>
!!<tr><td> 10 </td><td> \anchor bsbhnf bsbhnf	</td><td> Absterben und Exkretion Heterotropher Nanoflagelaten 			</td><td> mgC/l je Zeitschritt     	</td><td> hnf -> orgc </td></tr>
!!<tr><td> 11 </td><td> \anchor hnfbac hnfbac	</td><td> Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen, die in jedem Zeitschritt infolge Wegfraß durch heterotrophe Nanoflagelaten verloren geht </td><td> mgC/l je Zeitschritt </td><td>  hnf -> orgc </td></tr>
!!<tr><td> 12 </td><td> 			</td><td> 	 								</td><td>  				</td><td> 	</td></tr>
!!<tr><td> 13 </td><td> \anchor drfaek drfaek	</td><td> Ausscheidungen der Dreissena-Muscheln infolge Konsums von Kieselalgen </td><td> mgBio/l je Zeitschritt 	</td><td> dreissen -> orgc,schweb </td></tr>
!!<tr><td> 14 </td><td> \anchor drfaeg drfaeg	</td><td> Ausscheidungen der Dreissena-Muscheln infolge Konsums von Grünalgen   </td><td> mgBio/l je Zeitschritt 	</td><td> dreissen -> orgc,schweb </td></tr>
!!<tr><td> 15 </td><td> \anchor drfaeb drfaeb	</td><td> Ausscheidungen der Dreissena-Muscheln infolge Konsums von Blaualgen   </td><td> mgBio/l je Zeitschritt 	</td><td> dreissen -> orgc,schweb </td></tr>
!!<tr><td> 16 </td><td> \anchor zexki zexki	</td><td> Ausscheidungen der Rotatorien infolge Konsums von Kieselalgen 	</td><td> mgBio/l je Zeitschritt 	</td><td> konsum -> orgc 	</td></tr>
!!<tr><td> 17 </td><td> \anchor zexgr zexgr	</td><td> Ausscheidungen der Rotatorien infolge Konsums von Grünalgen 		</td><td> mgBio/l je Zeitschritt 	</td><td> konsum -> orgc 	</td></tr>
!!<tr><td> 18 </td><td> \anchor zexbl zexbl	</td><td> Ausscheidungen der Rotatorien infolge Konsums von Blaualgen 		</td><td> mgBio/l je Zeitschritt 	</td><td> konsum -> orgc 	</td></tr>
!!<tr><td> 19 </td><td> \anchor dorgss dorgss	</td><td> Abnahme! von suspendierten Sedimenten infolge Abbau von C-Verbindungen</td><td> mg SS /l je Zeitschritt  	</td><td> orgc -> schweb 	</td></tr>
!!<tr><td> 20 </td><td> \anchor dalgki dalgki	</td><td> Zunahme infolge \ref Algen-Wachstum, Kiesel-Algen-Biomasse 		</td><td> mgBio/l je Zeitschritt 	</td><td> algaeski -> oxygen 	</td></tr>
!!<tr><td> 21 </td><td> \anchor dalggr dalggr	</td><td> Zunahme infolge \ref Algen-Wachstum, Grün-Algen 			</td><td> mgBio/l je Zeitschritt 	</td><td> algaesgr -> oxygen 	</td></tr>
!!<tr><td> 22 </td><td> \anchor dalgbl dalgbl	</td><td> Zunahme infolge \ref Algen-Wachstum, Blau-Algen 			</td><td> mgBio/l je Zeitschritt 	</td><td> algaesbl -> oxygen 	</td></tr>
!!<tr><td> 23 </td><td> \anchor dalgak dalgak	</td><td> Abnahme infolge \ref Algen-Respiration, Kiesel-Algen-Biomasse		</td><td> mgBio/l je Zeitschritt 	</td><td> algaeski -> oxygen 	</td></tr>
!!<tr><td> 24 </td><td> \anchor dalgag dalgag	</td><td> Abnahme infolge \ref Algen-Respiration, Grün-Algen 			</td><td> mgBio/l je Zeitschritt 	</td><td> algaesgr -> oxygen 	</td></tr>
!!<tr><td> 25 </td><td> \anchor dalgab dalgab	</td><td> Abnahme infolge \ref Algen-Respiration, Blau-Algen 			</td><td> mgBio/l je Zeitschritt 	</td><td> algaesbl -> oxygen 	</td></tr>
!!<tr><td> 26 </td><td> \anchor vco2 vco2	</td><td> Kohlendioxyd 								</td><td> mg/l				</td><td> algaeski -> ph  	</td></tr>
!!<tr><td> 27 </td><td> \anchor dzres1 dzres1	</td><td> Grund-Respiration des Zoo-Planktons					</td><td> mgBio/(l*d)			</td><td> konsum -> ph,ncyc,po4s</td></tr>
!!<tr><td> 28 </td><td> \anchor dzres2 dzres2	</td><td> Fraßabhängige Respirationsrate des Zoo-Planktons 			</td><td> mgBio/(l*d)			</td><td> konsum -> ph,ncyc,po4s</td></tr>
!!<tr><td> 29 </td><td> \anchor susn susn	</td><td> Durch SUSPendierte NITRIFikanten OXIDIERTE AMMONIUMMENGE 		</td><td> ? 				</td><td> ncyc -> ph 		</td></tr>
!!<tr><td> 30 </td><td> \anchor po2p po2p	</td><td> Sauerstoffproduktion durch Makrophyten 				</td><td> mgO/l je Zeitschritt 		</td><td> mphyt -> oxygen 	</td></tr>
!!<tr><td> 31 </td><td> \anchor po2r po2r	</td><td> Sauerstoffverbrauch durch Makrophyten 				</td><td> mgO/l je Zeitschritt 		</td><td> mphyt -> oxygen 	</td></tr>
!!<tr><td> 32 </td><td> \anchor go2n go2n	</td><td> O2-Verbrauch durch Nitrifikation NH4N -> NO2N -> NO3N 		</td><td> mgO/l je Zeitschritt 		</td><td> ncyc -> oxygen  	</td></tr>
!!<tr><td> 33 </td><td> \anchor akinh4 akinh4	</td><td> Ammoniumaufnahme der kiesel-Algen, tiefengem. 			</td><td> mgN/l je Zeitschritt  	</td><td> ncyc -> oxygen 	</td></tr>
!!<tr><td> 34 </td><td> \anchor agrnh4 agrnh4	</td><td> Ammoniumaufnahme der gruen-Algen, tiefengem. 				</td><td> mgN/l je Zeitschritt  	</td><td> ncyc -> oxygen 	</td></tr>
!!<tr><td> 35 </td><td> \anchor ablnh4 ablnh4	</td><td> Ammoniumaufnahme der blau-Algen, tiefengem. 				</td><td> mgN/l je Zeitschritt  	</td><td> ncyc -> oxygen 	</td></tr>
!!<tr><td> 36 </td><td> \anchor akino3 akino3	</td><td> Nitrataufnahme der kiesel-Algen 					</td><td> mgN/l je Zeitschritt  	</td><td> ncyc -> oxygen 	</td></tr>
!!<tr><td> 37 </td><td> \anchor agrno3 agrno3	</td><td> Nitrataufnahme der gruen-Algen 					</td><td> mgN/l je Zeitschritt  	</td><td> ncyc -> oxygen 	</td></tr>
!!<tr><td> 38 </td><td> \anchor ablno3 ablno3	</td><td> Nitrataufnahme der blau-Algen 					</td><td> mgN/l je Zeitschritt  	</td><td> ncyc -> oxygen 	</td></tr>
!!<tr><td> 39 </td><td> \anchor salgo salgo	</td><td> Summe Sauerstoffeintrag Algen  ** 					</td><td> mgO2/l je Zeitschritt		</td><td> oxygen -> Ausgabe 	</td></tr>
!!<tr><td> 40 </td><td> \anchor dalgo dalgo 	</td><td> Sauerstoffproduktion der Gruen-, Kiesel-, und Blaualgen 		</td><td> mgO2/l je Zeitschritt 	</td><td> oxygen -> Ausgabe 	</td></tr>
!!<tr><td> 41 </td><td> \anchor dalgao dalgao 	</td><td> Respiration (Sauerstoffverbrauch) der Gruen-, Kiesel-, und Blaualgen 	</td><td> mgO2/l je Zeitschritt 	</td><td> oxygen -> Ausgabe 	</td></tr>
!!<tr><td> 42 </td><td> \anchor ir ir		</td><td> Ingestionsrate der Rotatorien  					</td><td> mg/(l*h) 			</td><td> konsum -> algaeski,algaesgr,algaesbl </td></tr>
!!<tr><td> 43 </td><td> \anchor zooro2 zooro2	</td><td> Sauerstoffverbrauch durch Zooplanktonrespiration 			</td><td> mgO2/l je Zeitschritt 	</td><td> oxygen -> Ausgabe </td></tr>
!!<tr><td> 44 </td><td> \anchor ro2hnf ro2hnf	</td><td> Sauerstoffverbrauch durch Respiration HNF 				</td><td> mgO2/l je Zeitschritt 	</td><td> hnf -> oxygen </td></tr>
!!<tr><td> 45 </td><td> \anchor saett saett	</td><td> Sauerstoff Sättigungs-Konzentration 					</td><td> mgO2/l  			</td><td> oxygen -> Ausgabe </td></tr>
!!<tr><td> 46 </td><td> \anchor fluxt1 fluxt1	</td><td> Wärmefluss tiefenintegriert ??? wohl nur Rückgabewert 		</td><td> ??				</td><td> temperw -> Ausgabe </td></tr>
!!<tr><td> 47 </td><td> \anchor bsbct bsbct	</td><td> mineralisierter Kohlenstoffgehalt in der Wassersäule		  	</td><td> mgC/l je Zeitschritt		</td><td> orgc -> ncyc </td></tr>
!!<tr><td> 48 </td><td> \anchor akitbr akitbr	</td><td> Nettowachstum Kieselalgen (Bruttowachstum-Respiration) 		</td><td> mgBio/l je Zeitschritt 	</td><td> algaeski -> ncyc,po4s,silikat,Ausgabe </td></tr>
!!<tr><td> 49 </td><td> \anchor agrtbr agrtbr	</td><td> Nettowachstum Grünalgen (Bruttowachstum-Respiration) 			</td><td> mgBio/l je Zeitschritt 	</td><td> algaesgr -> ncyc,po4s,silikat,Ausgabe </td></tr>
!!<tr><td> 50 </td><td> \anchor abltbr abltbr	</td><td> Nettowachstum Blaualgen (Bruttowachstum-Respiration) 			</td><td> mgBio/l je Zeitschritt 	</td><td> algaesbl -> ncyc,po4s,silikat,Ausgabe </td></tr>
!!<tr><td> 51 </td><td> \anchor sgo2n sgo2n	</td><td> Aufsummierter Nitrifikationssauerstoffverbrauch ??	 		</td><td> ?? 				</td><td> ncyc -> Ausgabe </td></tr>
!!<tr><td> 52 </td><td> \anchor susno susno	</td><td> Ausgabewert Sauerstoffverbr. d. Nitrifik. in der Wassersäule   	</td><td> mgO2/(l*h)			</td><td> ncyc -> Ausgabe </td></tr>
!!<tr><td> 53 </td><td> \anchor algzok algzok	</td><td> Kiesel-Algen-Konsum des Zoo-Planktons  				</td><td> mgBiom./l je Zeitschritt  	</td><td> konsum -> algaeski </td></tr>
!!<tr><td> 54 </td><td> \anchor extk extk	</td><td> (Licht) Extinktionskoeffizient 1D:aufsummiert zu XFIG 		</td><td> 1/m 				</td><td> algaeski -> temperw,albenth,mphyt </td></tr>
!!<tr><td> 55 </td><td> \anchor tpki tpki	</td><td> wird zu XNAEHR, Naehrstoffabhängigkeit bezogen auf Hellphase, aufsummiert</td><td> -				</td><td> algaeski -> Ausgabe </td></tr>
!!<tr><td> 56 </td><td> \anchor akmuea akmuea	</td><td> Bruttowachstumsrate der Kieselalgen akgrow, aufsummiert zu XAKMUA	</td><td> 1/d  				</td><td> algaeski -> Ausgabe </td></tr>
!!<tr><td> 57 </td><td> \anchor ftaaus ftaaus	</td><td> Temperaturabhaengigkeit der Wachstumsrate fta 			</td><td> -				</td><td> algaeski -> Ausgabe </td></tr>
!!<tr><td> 58 </td><td> \anchor fiaus fiaus	</td><td> wird zu XFIK, Lichtabhängigkeit der Algenproduktion bezogen auf Hellphase, aufsummiert</td><td> - 		</td><td> algaeski -> Ausgabe </td></tr>
!!<tr><td> 59 </td><td> \anchor fheaus fheaus	</td><td> Lichthemmung der Kieselalgen, svhemk, aufsummiert zu XFHEK		</td><td>  -  				</td><td> algaeski -> Ausgabe </td></tr>
!!<tr><td> 60 </td><td> \anchor akraus akraus	</td><td> Respirationsrate der Kieselalgen, akres, aufsummiert zu XAKRAU	</td><td> 1/d  				</td><td> algaeski -> Ausgabe </td></tr>
!!<tr><td> 61 </td><td> \anchor dz2d dz2d	</td><td> vertikalen Dispersionskoeffizient, inaktiv in tiefengemittelten Sim.	</td><td> m²/s				</td><td> k_eps -> algaeski </td></tr>
!!<tr><td> 62 </td><td> \anchor templ templ	</td><td> Lufttemperatur  							</td><td> °C 				</td><td> temperl -> temperw </td></tr>
!!<tr><td> 63 </td><td> \anchor ro ro		</td><td> Luftfeuchte aus \ref ro_T 						</td><td> %				</td><td> update_weather -> temperw </td></tr>
!!<tr><td> 64 </td><td> \anchor schwi schwi	</td><td> Globalstrahlung an der Wasseroberflaeche unter Beruecksichtigung der Reflektion an der Wasseroberflaeche \ref schwi_T </td><td> [cal/(cm2*h)</td><td> update_weather -> temperw </td></tr>
!!<tr><td> 65 </td><td> \anchor wge wge		</td><td> Windgeschwindigkeit aus \ref wge_T 					</td><td> m/s				</td><td> update_weather -> temperw </td></tr>
!!<tr><td> 66 </td><td> \anchor cloud cloud	</td><td> Bewölkungsdichte aus \ref cloud_T 					</td><td> 1/8				</td><td> update_weather -> temperw </td></tr>
!!<tr><td> 67 </td><td> \anchor typw typw	</td><td> Wolkentyp aus \ref typw_T 						</td><td> -				</td><td> update_weather -> temperw </td></tr>
!!<tr><td> 68 </td><td> 			</td><td> 				 					</td><td>				</td><td> update_weather -> temperw </td></tr>
!!<tr><td> 69 </td><td> 			</td><td> 				 					</td><td>				</td><td>    </td></tr>
!!<tr><td> 70 </td><td> 			</td><td> 					 				</td><td>				</td><td>    </td></tr>
!!<tr><td> 71 </td><td> 			</td><td> 			 						</td><td>				</td><td>    </td></tr>
!!<tr><td> 72 </td><td> \anchor algzog algzog	</td><td> Gruen-Algen-Konsum des Zoo-Planktons 					</td><td> mgBio/l je Zeitschritt 	</td><td> konsum -> algaesgr </td></tr>
!!<tr><td> 73 </td><td> \anchor algzob algzob	</td><td> Blau-Algen-Konsum des Zoo-Planktons 					</td><td> mgBio/l je Zeitschritt  	</td><td> konsum -> algaesbl </td></tr>
!!<tr><td> 74 </td><td> \anchor zhnf zhnf	</td><td> Zooplankton frisst HNF 						</td><td> mgC/l  je Zeitschritt		</td><td> konsum -> hnf </td></tr>
!!<tr><td> 75 </td><td> \anchor hnfza hnfza	</td><td> HNFza(ior) = (zHNF(ior)/CHNF(ior))*24. 				</td><td> *unstimmig*			</td><td> konsum -> Ausgabe </td></tr>
!!<tr><td> 76 </td><td> \anchor rmuas rmuas	</td><td> = mueRot-respRg , Netto-Wachstumsrate des Zooplanktons, aufsummiert zu XRMUE</td><td> 1/d			</td><td> konsum -> Ausgabe </td></tr>
!!<tr><td> 77 </td><td> \anchor rakr rakr	</td><td> = iras(ior)*respaR , Aktive Respirationsrate des Zooplanktons ?fraßabhängig? , aufsummiert zu XRAKR</td><td> 1/d </td><td> konsum -> Ausgabe </td></tr>
!!<tr><td> 78 </td><td> \anchor rbar rbar	</td><td> = respRg, Basale Respirationsrate des Zooplanktons, aufsummiert zu XRBAR</td><td> 1/d 			</td><td> konsum -> Ausgabe </td></tr>
!!<tr><td> 79 </td><td> \anchor iras iras	</td><td> Ingestionsrate des Zooplanktons, aufsummiert zu XIR			</td><td> 1/d 				</td><td> konsum -> Ausgabe </td></tr>
!!<tr><td> 80 </td><td> \anchor tpgr tpgr	</td><td> wird zu XNAEHR, Naehrstoffabhängigkeit bezogen auf Hellphase, aufsummiert</td><td> -				</td><td> algaesgr -> Ausgabe </td></tr>
!!<tr><td> 81 </td><td> \anchor tpbl tpbl	</td><td> wird zu XNAEHR, Naehrstoffabhängigkeit bezogen auf Hellphase, aufsummiert</td><td> -				</td><td> algaesbl -> Ausgabe </td></tr>
!!<tr><td> 82 </td><td> \anchor figaus figaus	</td><td> wird zu XFIK, Lichtabhängigkeit der Algenproduktion bezogen auf Hellphase, aufsummiert</td><td> - 		</td><td> algaesgr -> Ausgabe </td></tr>
!!<tr><td> 83 </td><td> \anchor fibaus fibaus	</td><td> wird zu XFIK, Lichtabhängigkeit der Algenproduktion bezogen auf Hellphase, aufsummiert</td><td> - 		</td><td> algaesbl -> Ausgabe </td></tr>
!!<tr><td> 84 </td><td> \anchor agmuea agmuea	</td><td> Brutto-Wachstumsrate Grünalgen aggrow, aufsummiert zu XAGMUA 		</td><td> 1/d				</td><td> algaesgr -> Ausgabe </td></tr>
!!<tr><td> 85 </td><td> \anchor abmuea abmuea	</td><td> Brutto-Wachstumsrate Blaualgen abgrow, aufsummiert zu XABMUA		</td><td> 1/d				</td><td> algaesbl -> Ausgabe </td></tr>
!!<tr><td> 86 </td><td> \anchor fhegas fhegas	</td><td> Lichthemmung der Grünalgen, svhemg, aufsummiert zu XFHEG		</td><td> -				</td><td> algaesgr -> Ausgabe </td></tr>
!!<tr><td> 87 </td><td> \anchor fhebas fhebas	</td><td> Lichthemmung der Blaualgen, svhemb, aufsummiert zu XFHEB		</td><td> -				</td><td> algaesbl -> Ausgabe </td></tr>
!!<tr><td> 88 </td><td> \anchor agreau agreau	</td><td> Respirationsrate der Grünalgen, agres, aufsummiert zu XAGREA		</td><td> 1/d				</td><td> algaesgr -> Ausgabe </td></tr>
!!<tr><td> 89 </td><td> \anchor abreau abreau	</td><td> Respirationsrate der Blaualgen, abres, aufsummiert zu XABREA		</td><td> 1/d				</td><td> algaesbl -> Ausgabe </td></tr>
!!<tr><td> 90 </td><td> \anchor dc_denw dc_denw </td><td> C-Abbau durch Denitrifikation in der Wassersäule 			</td><td> mgC/l je Zeitschritt 		</td><td> ncyc -> oxygen </td></tr>
!!<tr><td> 91 </td><td> \anchor zbac zbac	</td><td> Zooplankton frisst Bakterien 						</td><td> mgC/l je Zeitschritt		</td><td> konsum -> orgc </td></tr>
!!<tr><td> 92 </td><td> 			</td><td> 									</td><td> 	 			</td><td>    </td></tr>
!!<tr><td> 93 </td><td> 			</td><td> 									</td><td> 	 			</td><td>    </td></tr>
!!<tr><td> 94 </td><td> 			</td><td> 							 		</td><td> 	 			</td><td>    </td></tr>
!!<tr><td> 95 </td><td> \anchor drfaes drfaes	</td><td> Ausscheidungen der Dreissena-Muscheln infolge Konsums von Schwebstoffen </td><td> mgBio/l je Zeitschritt 	</td><td> dreissen -> orgc,schweb </td></tr>
!!<tr><td> 96 </td><td> \anchor drhnf drHNF	</td><td> Dreissena-Muscheln fressen HNF  					</td><td> mgC/l  je Zeitschritt		</td><td> dreissen -> hnf </td></tr>
!!</table>\n\n
!! <h2> tiefenaufgelöste Übergabe Konzentrationen </h2> \anchor tiefenaufgelöste_übergabe_variable
!! Die QSim-3D Nummer bezieht sich auf die Datenfelder modell::trans_quant_vert und  modell::trans_quant_vert_p\n 
!! Die QSim-1D Namen werden in QSim3D im module_QSimDatenfelder.f95 vereinbart.\n 
!!<table >
!!<tr><th> Nr.</th><th> Name			</th><th> Beschreibung 				</th><th> Dimension </th><th> von -> nach </th></tr>
!!<tr><td>  1 </td><td> \anchor up_nkz up_nkz	</td><td> N-Aufnahmerate der Algengruppe kiesel </td><td> </td><td> algaeski -> ncyc </td></tr>
!!<tr><td>  2 </td><td> \anchor up_ngz up_ngz	</td><td> N-Aufnahmerate der Algengruppe gruen   </td><td> </td><td>  -> ncyc </td></tr>
!!<tr><td>  3 </td><td> \anchor up_nbz up_nbz	</td><td> N-Aufnahmerate der Algengruppe blau    </td><td> </td><td>  -> ncyc </td></tr>
!!<tr><td>  4 </td><td> \anchor up_siz up_siz	</td><td> Si-Aufnahmerate der Algengruppe kiesel    </td><td>  mgSi/(mgBio*d )</td><td> algaeski -> silikat </td></tr>
!!<tr><td>  5 </td><td> \anchor up_pkz up_pkz	</td><td> P-Aufnahmerate der Algengruppen kiesel    </td><td> mgP/(mgBio*d) </td><td> algaeski -> po4s </td></tr>
!!<tr><td>  6 </td><td> \anchor up_pgz up_pgz	</td><td> P-Aufnahmerate der Algengruppen gruen    </td><td> mgP/(mgBio*d) </td><td>  algaesgr-> po4s </td></tr>
!!<tr><td>  7 </td><td> \anchor up_pbz up_pbz	</td><td> P-Aufnahmerate der Algengruppen blau    </td><td> mgP/(mgBio*d)</td><td>  algaesbl-> po4s </td></tr>
!!<tr><td>  8 </td><td> \anchor up_n2z up_n2z	</td><td> Aufnahmerate von Luftstickstoff durch Blaualgen   </td><td>  </td><td>  algaesbl-> ncyc </td></tr>
!!<tr><td>  9 </td><td> \anchor aknh4z aknh4z	</td><td> Ammoniumaufnahme Algengruppe kiesel    </td><td> </td><td>  ->  </td></tr>
!!<tr><td>  10</td><td> \anchor agnh4z agnh4z	</td><td> Ammoniumaufnahme Algengruppe gruen    </td><td> </td><td>  ->  </td></tr>
!!<tr><td>  11</td><td> \anchor abnh4z abnh4z	</td><td> Ammoniumaufnahme Algengruppe blau    </td><td> </td><td>  ->  </td></tr>
!!<tr><td>  12</td><td> \anchor dalgkz dalgkz	</td><td> Algen-Wachstum Algengruppe kiesel    </td><td> mgBiom./l je Zeitschritt </td><td>  ->  </td></tr>
!!<tr><td>  13</td><td> \anchor dalggz dalggz	</td><td> Algen-Wachstum Algengruppe gruen    </td><td> mgBiom./l je Zeitschritt </td><td>  ->  </td></tr>
!!<tr><td>  14</td><td> \anchor dalgbz dalgbz	</td><td> Algen-Wachstum Algengruppe blau    </td><td> mgBiom./l je Zeitschritt </td><td>  ->  </td></tr>
!!<tr><td>  15</td><td> \anchor akno3z akno3z	</td><td> Nitrataufnahme Algengruppe kiesel    </td><td> </td><td>  ->  </td></tr>
!!<tr><td>  16</td><td> \anchor agno3z agno3z	</td><td> Nitrataufnahme Algengruppe gruen    </td><td> </td><td>  ->  </td></tr>
!!<tr><td>  17</td><td> \anchor abno3z abno3z	</td><td> Nitrataufnahme Algengruppe blau    </td><td> </td><td>  ->  </td></tr>
!!<tr><td>  18</td><td> \anchor algakz algakz	</td><td> Respirierte Algenbiomasse kiesel  </td><td> mgBio/l je Zeitschritt  </td><td>  ->  </td></tr>
!!<tr><td>  19</td><td> \anchor algagz algagz	</td><td> Respirierte Algenbiomasse grün   </td><td> mgBio/l je Zeitschritt  </td><td>  ->  </td></tr>
!!<tr><td>  20</td><td> \anchor algabz algabz	</td><td> Respirierte Algenbiomasse blau  </td><td>  mgBio/l je Zeitschritt </td><td>  ->  </td></tr>
!!<tr><td>  21</td><td> \anchor vz1 vz1		</td><td> lokale Sauerstoffzehrung/produktion  (tiefenprofil ausserhalb oxygen)   </td><td> </td><td>  ->  </td></tr>
!!<tr><td>  22</td><td> \anchor dtemp dtemp	</td><td> lokaler Wärmeeintrag, tiefenaufgelöst (tiefenprofil ausserhalb temperw)   </td><td> </td><td>  ->  </td></tr>
!!<tr><td>  23</td><td> \anchor akibrz akibrz	</td><td> Brutto-Zuwachs Kiesel-Algen-Biomasse  </td><td> mgBio/l je Zeitschritt </td><td>  ->  </td></tr>
!!<tr><td>  24</td><td> \anchor agrbrz agrbrz	</td><td> Brutto-Zuwachs Grün-Algen-Biomasse    </td><td> mgBio/l je Zeitschritt </td><td>  ->  </td></tr>
!!<tr><td>  25</td><td> \anchor ablbrz ablbrz	</td><td> Brutto-Zuwachs Blau-Algen-Biomasse 	</td><td> mgBio/l je Zeitschritt </td><td>  ->  </td></tr>
!!<tr><td>  26</td><td> \anchor algzkz algzkz	</td><td> Kiesel-Algen-Konsum durch Zoo-Plankton</td><td> mg/l </td><td>  ->  </td></tr>
!!<tr><td>  27</td><td> \anchor algzgz algzgz	</td><td> Grün-Algen-Konsum durch Zoo-Plankton </td><td> mg/l </td><td>  ->  </td></tr>
!!<tr><td>  28</td><td> \anchor algzbz algzbz	</td><td> Blau-Algen-Konsum durch Zoo-Plankton </td><td> mg/l </td><td>  ->  </td></tr>
!!</table>\n\n
!! <h2> Übergabe Werte </h2> \anchor übergabe_wert
!! Die QSim-3D Nummer bezieht sich auf die Datenfelder modell::transfer_value und  modell::transfer_value_p\n 
!! Die QSim-1D Namen werden in QSim3D im module_QSimDatenfelder.f95 vereinbart.\n \n
!! WIRD AUFGELÖST ####\n\n
!!<table >
!!<tr><th> Nr.</th><th> Name			</th><th> Beschreibung 						</th><th> Dimension 	</th><th> von -> nach 	</th></tr>
!!<tr><td>  1 </td><td> 			</td><td> leer 							</td><td>		</td><td>   		</td></tr>
!!<tr><td>  2 </td><td> \anchor nzoo nzoo	</td><td> Stickstoffanteil in der Rotatorienbiomasse 		</td><td> mgN/mgBiom.	</td><td> ini_ueber -> ncyc,orgc </td></tr>
!!<tr><td>  3 </td><td> \anchor pZoo pZoo	</td><td> Phosphoranteil in der Rotatorienbiomasse 		</td><td> mgP/mgBiom.	</td><td> ini_ueber -> po4s,orgc </td></tr>
!!<tr><td>  4 </td><td> \anchor bk1 bk1		</td><td> parameter bk1 orgc_modul 				</td><td> </td><td>  ->  </td></tr>
!!<tr><td>  5 </td><td> \anchor bk2 bk2		</td><td> parameter bk2 orgc_module 				</td><td> </td><td>  ->  </td></tr>
!!<tr><td>  6 </td><td> \anchor saettk Saettk	</td><td> ??? 							</td><td> </td><td>  algeski -> Rückgabewert ?? </td></tr>
!!<tr><td>  7 </td><td> \anchor tauscs tauscs	</td><td> Schiffseinfluss qsim.f90: tauscs = 1.25  		</td><td> </td><td>  ->  </td></tr>
!!<tr><td>  8 </td><td> \anchor saettg Saettg	</td><td> ??? 							</td><td> </td><td>  algesgr -> Rückgabewert ?? </td></tr>
!!<tr><td>  9 </td><td> \anchor saettb Saettb	</td><td> ??? 							</td><td> </td><td>  algesbl -> Rückgabewert ?? </td></tr>
!!<tr><td> 10 </td><td> \anchor it_h it_h	</td><td> Anzahl der Zeitschritte während der Hellphase (unbenutzt) </td><td>-</td><td> strahlg -> algaeski </td></tr>
!!</table>\n\n
!!
!! <b> \ref globaleParameter von APARAM.txt </b>
!! \n\n
!! <hr>
!! ** Im Programmcode werden die Sauerstoffänderungsraten nach meinem Dafürhalten als Differenzen je Zeitschritt 
!! verwendet. die Gerris-Oberfläche spricht aber von Raten je Stunde ???Jens\n
!! \n\n
!! Variablendefinition in module_modell.f95\n
!! aus Datei uebergabe_werte.f95; zurück: \ref lnk_Datentechnik \n 
!! siehe dazu auch: stofftransport()

!----+-----+----
!> <h1>Verteilen der Datenstrukturen auf die parallelen Prozesse</h1>
!! .... to be done\n 
      subroutine ueber_parallel()
      use modell
      use QSimDatenfelder
      implicit none
      integer :: allostat

      call MPI_Bcast(number_trans_quant_points,1,MPI_INT,0,mpi_komm_welt,ierr)
!      print*,meinrang,' ueber_parallel, number_trans_quant_points,number_trans_quant='  &
!     &      ,number_trans_quant_points,number_trans_quant

      call broadcast_parameter()
!      print*,meinrang,' broadcast_parameter'

      allocate (transfer_quantity_p(number_trans_quant*part), stat = allostat )
      if(allostat.ne.0)then
         write(fehler,*)' Rueckgabewert   von   allocate transfer_quantity_p :', allostat
         call qerror(fehler)
      end if 
!      print*,meinrang,'allocate (transfer_quantity_p    number_trans_quant,number_trans_quant_points,part=' &
!     &      ,number_trans_quant,part

      allocate (trans_quant_vert_p(number_trans_quant_vert*part*num_lev_trans), stat = allostat )
      if(allostat.ne.0)then
         write(fehler,*)' allocate (trans_quant_vert_p failed :', allostat
         call qerror(fehler)
      end if 
!      print*,meinrang,' allocate (trans_quant_vert_p  -  number_trans_quant_vert,num_lev_trans,part'  &
!     &      ,number_trans_quant_vert,num_lev_trans,part

      !call mpi_barrier (mpi_komm_welt, ierr)
      !print*,meinrang,' ueber_parallel vor scatter_ueber'
      call scatter_ueber()
      !call mpi_barrier (mpi_komm_welt, ierr)
      !print*,meinrang,' ueber_parallel nach scatter_ueber'

      RETURN
      END subroutine ueber_parallel

!----+-----+----
!> <h1>Verteilen der Datenstrukturen auf die parallelen Prozesse</h1>
!! .... to be done\n 
      subroutine scatter_ueber()
      use modell
      implicit none

      call MPI_Bcast(transfer_value_p,number_trans_val,MPI_FLOAT,0,mpi_komm_welt,ierr)
      if(ierr.ne.0)then
         write(fehler,*)' MPI_Bcast(transfer_value_p failed :',ierr
         call qerror(fehler)
      end if 
      call mpi_barrier (mpi_komm_welt, ierr)
      !print*,meinrang,'scatter_ueber: MPI_Bcast(transfer_value_p,  number_trans_val,part=',number_trans_val,part
      !call mpi_barrier (mpi_komm_welt, ierr)
      !print*,meinrang,"size(transfer_quantity_p)=",size(transfer_quantity_p)
      !call mpi_barrier (mpi_komm_welt, ierr)
      !if(meinrang.eq.0)print*,"size(transfer_quantity)=",size(transfer_quantity)
      !call mpi_barrier (mpi_komm_welt, ierr)

      call MPI_Scatter(transfer_quantity, part*number_trans_quant, MPI_FLOAT,  &
     &                 transfer_quantity_p, part*number_trans_quant, MPI_FLOAT, 0, mpi_komm_welt, ierr)
      if(ierr.ne.0)then
         write(fehler,*)' MPI_Scatter(transfer_quantity failed :',ierr
         call qerror(fehler)
      end if 
      call mpi_barrier (mpi_komm_welt, ierr)
      !print*,meinrang,' scatter_ueber nach MPI_Scatter(transfer_quantity'

      call MPI_Scatter(trans_quant_vert, number_trans_quant_vert*part*num_lev_trans, MPI_FLOAT,  &
     &                 trans_quant_vert_p, number_trans_quant_vert*part*num_lev_trans, MPI_FLOAT, 0, mpi_komm_welt, ierr)
      if(ierr.ne.0)then
         write(fehler,*)' MPI_Scatter(trans_quant_vert failed :', ierr
         call qerror(fehler)
      end if 
!      print*,meinrang,'scatter_ueber: MPI_Scatter(trans_quant_vert,ierr,number_trans_quant_vert,part,num_lev_trans'  &
!     &      ,ierr,number_trans_quant_vert,part,num_lev_trans
      call mpi_barrier (mpi_komm_welt, ierr)
      !print*,meinrang,' scatter_ueber nach MPI_Scatter(trans_quant_vert,'

      RETURN
      END subroutine scatter_ueber

!----+-----+----
!> <h1>wieder-einsammeln der Datenstrukturen von den parallelen Prozesse</h1>
!! \n 
      subroutine gather_ueber()
      use modell
      implicit none
      integer :: i

      call MPI_Gather(transfer_quantity_p, part*number_trans_quant, MPI_FLOAT,  &
      transfer_quantity, part*number_trans_quant, MPI_FLOAT, 0, mpi_komm_welt, ierr)
      if(ierr.ne.0)then
         write(fehler,*)' MPI_Gather(transfer_quantity failed :', ierr
         call qerror(fehler)
      end if 

      call MPI_Gather(trans_quant_vert_p, number_trans_quant_vert*part*num_lev_trans, MPI_FLOAT,  &
      trans_quant_vert, number_trans_quant_vert*part*num_lev_trans, MPI_FLOAT, 0,mpi_komm_welt, ierr)
      if(ierr.ne.0)then
         write(fehler,*)' MPI_Gather(trans_quant_vert failed :', ierr
         call qerror(fehler)
      end if 

      RETURN
      END subroutine gather_ueber

!----+-----+----
!> Initialisierung der nicht-transportierten Übergabe-Konzentrationen und Werte.
!! \n\n
      subroutine ini_ueber(nk)
         use modell                                                 
         implicit none
         integer nk,i,n,as,j,l,k
         real, parameter  :: bk1 = 0.51 , bk2 = 0.02  !! Konstanten wie in orgc gesetzt
if(meinrang.eq.0)then ! nur auf Prozessor 0 bearbeiten

!--------------------------------------------- Übergabe Konzentrationen
         number_trans_quant_points=nk

         if(num_lev_trans.ne.num_lev)then
            write(fehler,*)'Anzahl der Levels num_lev_trans und num_lev(planktonic) müssen gleich sein',num_lev_trans,num_lev
            call qerror(fehler)
         endif

!--------single (global) transfer values
         do j=1,number_trans_val ! initialise
            write(trans_val_name(j),'(18x)')
         end do
         trans_val_name( 1)= "               nix" ! leer
         trans_val_name( 2)= "              nZoo"
         trans_val_name( 3)= "              pZoo"
         trans_val_name( 4)= "               bk1"
         trans_val_name( 5)= "               bk2"
         trans_val_name( 6)= "            Saettk"
         trans_val_name( 7)= "            tauscs" ! Schiffseinfluss     qsim.f90: tauscs = 1.25
         trans_val_name( 8)= "            saettg"
         trans_val_name( 9)= "            saettb"
         trans_val_name(10)= "              it_h" ! Anzahl der Zeitschritte während der Hellphase
!algae_huelle.f95:      saettg = transfer_value_p(8)    ! ???
!algae_huelle.f95:      saettb = transfer_value_p(9)    ! ???
        !trans_val_name()= "               " ! 

         do j=1,number_trans_val ! initialise
            transfer_value_p(j)= 0.0 !!!####!0.0
         end do
         do j=1,number_trans_val ! default: no output
            output_trans_val(j)=.false.
         end do

         !#! transfer_value_p(2)=
         !#! transfer_value_p(3)=
         transfer_value_p(4)=0.51 !! real, parameter  :: bk1 = 0.51 , bk2 = 0.02
         transfer_value_p(5)=0.02 !! Konstanten wie in orgc gesetzt
         !saettk    transfer_value_p(6)=  !! Rückgabewert Algen
         transfer_value_p(7)= 1.25 ! Schiffseinfluss     qsim.f90: tauscs = 1.25
         ! saettg   transfer_value_p(8)=
         ! saettb   transfer_value_p(9)=
         !#! transfer_value_p(10)=

!------- depth averaged quantities
         do j=1,number_trans_quant ! initialise
            write(trans_quant_name(j),'(18x)')
         end do
         trans_quant_name( 1)= "              bsbt"
         trans_quant_name( 2)= "            bsbctP" ! Phosphorfreisetzung orgc
         trans_quant_name( 3)= "               doN" ! Stickstofffreisetzung orgc
         trans_quant_name( 4)= "            BACmua" ! Ausgabekonzentration Summe Aufnahme+Respirationsrate heterotrophe Bakterien
         trans_quant_name( 5)= "        empty_five" ! 
         trans_quant_name( 6)= "             abszo" ! Absterberate Zooplankton
         trans_quant_name( 7)= "            dkimor" ! Absterberate Kieselalgen
         trans_quant_name( 8)= "            dgrmor" ! Absterberate Grünlalgen
         trans_quant_name( 9)= "            dblmor" ! Absterberate Blaualgen
         trans_quant_name(10)= "            BSBHNF" ! 
         trans_quant_name(11)= "            HNFBAC" ! 
         trans_quant_name(12)= "      empty_twelve" ! 
         trans_quant_name(13)= "            drfaek" ! 
         trans_quant_name(14)= "            drfaeg" ! 
         trans_quant_name(15)= "            drfaeb" ! 
         trans_quant_name(16)= "             zexki" ! 
         trans_quant_name(17)= "             zexgr" ! 
         trans_quant_name(18)= "             zexbl" ! 
         trans_quant_name(19)= "            dorgSS" ! 
         trans_quant_name(20)= "            dalgki" ! Zuwachs Kiesel-Algen
         trans_quant_name(21)= "            dalggr" ! Zuwachs Grün-Algen
         trans_quant_name(22)= "            dalgbl" ! Zuwachs Blau-Algen
         trans_quant_name(23)= "            dalgak" ! Respiration Kiesel-Algen
         trans_quant_name(24)= "            dalgag" ! Respiration Grün-Algen
         trans_quant_name(25)= "            dalgab" ! Respiration Blau-Algen
         trans_quant_name(26)= "              vco2" ! Konzentration Kohlendioxyd
         trans_quant_name(27)= "            dzres1" ! Grund-Respiration Konsumenten
         trans_quant_name(28)= "            dzres2" ! Fraßabhängige Respirationsrate Konsumenten
         trans_quant_name(29)= "              susn" ! Durch SUSPendierte NITRIFikanten OXIDIERTE AMMONIUMMENGE
         trans_quant_name(30)= "              PO2P" ! Sauerstoffproduktion durch Makrophyten in mgO2/(l*h)
         trans_quant_name(31)= "              PO2R" ! Sauerstoffverbrauch durch Makrophyten mgO2/(l*h)
         trans_quant_name(32)= "              go2n" ! FUER DIE STICKSTOFFOXYDATION VERBRAUCHTE SAUERSTOFFMENGE in mgO2/(l*h)
         trans_quant_name(33)= "            akinh4" ! Ammoniumaufnahme der kiesel-Algen, tiefengem.
         trans_quant_name(34)= "            agrnh4" ! Ammoniumaufnahme der gruen-Algen, tiefengem.
         trans_quant_name(35)= "            ablnh4" ! Ammoniumaufnahme der blau-Algen, tiefengem.
         trans_quant_name(36)= "            akino3" ! Nitrataufnahme der kiesel-Algen
         trans_quant_name(37)= "            agrno3" ! Nitrataufnahme der gruen-Algen
         trans_quant_name(38)= "            ablno3" ! Nitrataufnahme der blau-Algen
         trans_quant_name(39)= "             salgo" ! Summe Sauerstoffeintrag Algen 
         trans_quant_name(40)= "             dalgo" ! Sauerstoffproduktion der Gruen-, Kiesel-, und Blaualgen
         trans_quant_name(41)= "            dalgao" ! Respiration (Sauerstoffverbrauch) der Gruen-, Kiesel-, und Blaualge
         trans_quant_name(42)= "                ir" ! Ingestionsrate der Rotatorien in mg/(l*h) | konsum()
         trans_quant_name(43)= "            zooro2" ! Sauerstoffverbrauch durch Zooplanktonrespiration
         trans_quant_name(44)= "            rO2HNF" ! Respiration HNF ???
         trans_quant_name(45)= "             SAETT" ! Sauerstoff Sättigungs-Konzentration in mgO2/l 
         trans_quant_name(46)= "            FluxT1" ! Wärmefluss tiefenintegriert ??? wohl Rückgabewert 
         trans_quant_name(47)= "             bsbct" ! mineralisierter Kohlenstoffgehalt in der Wassersäule | Rückgabewert
         trans_quant_name(48)= "            akitbr" ! 
         trans_quant_name(49)= "            agrtbr" ! 
         trans_quant_name(50)= "            abltbr" ! 
         trans_quant_name(51)= "             sgo2n" ! Aufsummierter Nitrifikationssauerstoffverbrauch, nur Ausgabe 
         trans_quant_name(52)= "             susno" ! Sauerstoffverbr. d. Nitrifik. in der Wassersäule in mgO2/(l*h)
         trans_quant_name(53)= "            algzok" ! kiesel-Algen-Konsum Zoo-Plankton in mg/l
         trans_quant_name(54)= "           ???extk" ! mittlerer Extinktionskoeffizient
         trans_quant_name(55)= "              tpki" ! Ausgabeparameter algaeski()
         trans_quant_name(56)= "            akmuea" ! Ausgabeparameter algaeski()
         trans_quant_name(57)= "            ftaaus" ! Ausgabeparameter algaeski() fta
         trans_quant_name(58)= "             fiaus" ! Ausgabeparameter algaeski() Pmit/(Pmax*3.6)
         trans_quant_name(59)= "            fheaus" ! Ausgabeparameter algaeski() svhemk
         trans_quant_name(60)= "            akraus" ! Ausgabeparameter algaeski() F53
         trans_quant_name(61)= "              Dz2D" ! min. vertikalen Dispersionskoeffizient
         trans_quant_name(62)= "             templ" ! Lufttemperatur
         trans_quant_name(63)= "                RO" ! Luftfeuchte
         trans_quant_name(64)= "             SCHWI" ! Globalstrahlung
         trans_quant_name(65)= "               WGE" ! Windgeschwindigkeit
         trans_quant_name(66)= "             cloud" ! Bewölkungsdichte
         trans_quant_name(67)= "              typw" ! Wolkentyp
         trans_quant_name(68)= "  empty_sixtyeight" ! 
         trans_quant_name(69)= "          empty_69" ! 
         trans_quant_name(70)= "          empty_70" ! 
         trans_quant_name(71)= "          empty_71" ! 
         trans_quant_name(72)= "            algzog" ! gruen-Algen-Konsum Zoo-Plankton in mg/l
         trans_quant_name(73)= "            algzob" ! blau-Algen-Konsum Zoo-Plankton in mg/l
         trans_quant_name(74)= "              zHNF" ! Aufnahmerate der HNF  
         trans_quant_name(75)= "             HNFza" ! HNFza(ior) = (zHNF(ior)/CHNF(ior))*24.
         trans_quant_name(76)= "             rmuas" !  = mueRot-respRg ! Nettowachstumsrate Rotatorien ?
         trans_quant_name(77)= "              rakr" !  = iras(ior)*respaR ! Fraßabhängige Respiration ?
         trans_quant_name(78)= "              rbar" !  = respRg ! GRund?-Respiration ?
         trans_quant_name(79)= "              iras" !  Ausgabe Ingestionsrate
         trans_quant_name(80)= "              tpgr" ! Ausgabeparameter algaesgr()
         trans_quant_name(81)= "              tpbl" ! Ausgabeparameter algaesbl()
         trans_quant_name(82)= "            figaus" ! Ausgabeparameter algaesgr() Pmit/(Pmax*3.6)
         trans_quant_name(83)= "            fibaus" ! Ausgabeparameter algaesbl() Pmit/(Pmax*3.6)
         trans_quant_name(84)= "            agmuea" ! Ausgabeparameter algaesgr()
         trans_quant_name(85)= "            abmuea" ! Ausgabeparameter algaesbl()
         trans_quant_name(86)= "            fhegas" ! Ausgabeparameter algaeski() svhemg
         trans_quant_name(87)= "            fhebas" ! Ausgabeparameter algaeski() svhemb
         trans_quant_name(88)= "            agreau" ! Ausgabe agbcm
         trans_quant_name(89)= "            abreau" ! Ausgabe von abbcm (Chlorophyll-a zu Kohlenstoff der blau-Algen) algaesbl() 
         trans_quant_name(90)= "           dC_DenW" ! C-Abbau durch Denitrifikation in der Wassersäule
         trans_quant_name(91)= "              zBAC" ! Aufnahmerate der Bakterien
         trans_quant_name(92)= "          empty_92" ! 
         trans_quant_name(93)= "          empty_93" ! 
         trans_quant_name(94)= "          empty_94" ! 
         trans_quant_name(95)= "            drfaes" ! Ausscheidungen der Dreissena-Muscheln infolge Konsums von Schwebstoffen
         trans_quant_name(96)= "             drHNF" ! Dreissena-Muscheln fressen HNF
        !trans_quant_name()= "              " ! 

         ! allocate transfer_quantity and initialise
         !!! allocate (transfer_quantity(number_trans_quant*number_trans_quant_points), stat = as )
         print*,"ini_ueber allocate (transfer_quantity part*proz_anz=",part*proz_anz,part,proz_anz
         allocate (transfer_quantity(number_trans_quant*part*proz_anz), stat = as )
         if(as.ne.0)then
            write(fehler,*)' Rueckgabewert   von   allocate transfer_quantity :', as
            call qerror(fehler)
         end if 
         print*,meinrang,'allocate (transfer_quantity(    number_trans_quant,number_trans_quant_points=' &
     &         ,number_trans_quant,number_trans_quant_points

         do i=1,number_trans_quant_points ! alle knoten
            do j=1,number_trans_quant ! initialisierung aller konzentrationen zunächt auf Null
               transfer_quantity(j+(i-1)*number_trans_quant)= 0.0 !!!####!0.0
            end do
         end do
         do j=1,number_trans_quant ! default: no output
            output_trans_quant(j)=.false.
         end do

!--------vertically distributed quantities
         do j=1,number_trans_quant_vert ! initialise
            write(trans_quant_vert_name(j),'(18x)')
         end do
         trans_quant_vert_name( 1)= "            up_NKz"
         trans_quant_vert_name( 2)= "            up_NGz" ! N-Aufnahmerate der Algengruppe gruen 
         trans_quant_vert_name( 3)= "            up_NBz" ! N-Aufnahmerate der Algengruppe blau
         trans_quant_vert_name( 4)= "            up_Siz" ! Si (Silizium) -Aufnahmerate der Kieselalgen
         trans_quant_vert_name( 5)= "            up_PKz" ! P (Phosphor) -Aufnahmerate der Algengruppen kiesel
         trans_quant_vert_name( 6)= "            up_PGz" ! P-Aufnahmerate der Algengruppen gruen
         trans_quant_vert_name( 7)= "            up_PBz" ! P-Aufnahmerate der Algengruppen blau
         trans_quant_vert_name( 8)= "            up_N2z" ! Aufnahmerate von Luftstickstoff durch Blaualgen
         trans_quant_vert_name( 9)= "            aknh4z" ! 
         trans_quant_vert_name(10)= "            agnh4z" ! 
         trans_quant_vert_name(11)= "            abnh4z" ! 
         trans_quant_vert_name(12)= "            dalgkz" ! 
         trans_quant_vert_name(13)= "            dalggz" ! 
         trans_quant_vert_name(14)= "            dalgbz" ! 
         trans_quant_vert_name(15)= "            akno3z" ! 
         trans_quant_vert_name(16)= "            agno3z" ! 
         trans_quant_vert_name(17)= "            abno3z" ! 
         trans_quant_vert_name(18)= "            algakz" ! Respirierte Algenbiomasse kiesel
         trans_quant_vert_name(19)= "            algagz" ! Respirierte Algenbiomasse grün
         trans_quant_vert_name(20)= "            algabz" ! Respirierte Algenbiomasse blau
         trans_quant_vert_name(21)= "               vz1" ! lokale Sauerstoffzehrung/produktion tiefenaufgelöst (tiefenprofil ausserhalb oxygen)
         trans_quant_vert_name(22)= "             dtemp" ! lokaler Wärmeeintrag tiefenaufgelöst (tiefenprofil ausserhalb temperw)
         trans_quant_vert_name(23)= "            akibrz" ! Wachstum? Kiesel-Algen-Biomasse
         trans_quant_vert_name(24)= "            agrbrz" ! Wachstum ? Grün-Algen-Biomasse
         trans_quant_vert_name(25)= "            ablbrz" ! Wachstum ? Blau-Algen-Biomasse
         trans_quant_vert_name(26)= "            algzkz" ! Kiesel-Algen-Konsum durch Zoo-Plankton in mg/l 
         trans_quant_vert_name(27)= "            algzgz" ! Grün-Algen-Konsum durch Zoo-Plankton in mg/l 
         trans_quant_vert_name(28)= "            algzbz" ! Blau-Algen-Konsum durch Zoo-Plankton in mg/l 

         allocate (trans_quant_vert(part*proz_anz*number_trans_quant_vert*num_lev_trans), stat = as )
         !allocate (trans_quant_vert(number_trans_quant*number_trans_quant_points*num_lev_trans), stat = as )
         if(as.ne.0)then
            write(fehler,*)' allocate (trans_quant_vert failed :', as
            call qerror(fehler)
         end if 

         do i=1,number_trans_quant_points
            do j=1,number_trans_quant_vert
               do k=1,num_lev_trans
                  trans_quant_vert(k+(j-1)*num_lev_trans+(i-1)*number_trans_quant_vert*num_lev_trans)= 0.0 !!!####!0.0 ! initialisierung aller konzentrationen zunächt auf Null
               end do ! alle k levels
            end do ! alle j quantities
            !do k=1,num_lev_trans
            !   trans_quant_vert(k+(1-1)*num_lev_trans+(i-1)*num_lev_trans*number_trans_quant_vert)=7.7 ! up_NKz probehalber
            !end do ! alle k levels
         end do ! alle i Knoten
         do j=1,number_trans_quant_vert ! default: no output
            output_trans_quant_vert(j)=.false.
         end do

end if !! nur prozessor 0
      END subroutine ini_ueber

!----+-----+----
!> \page globaleParameter globale Modellparameter
!! Dies sind empirische Parameter, die als allgemeingültig für \n
!! <ul>
!! <li>das gesamte Modellgebiet und</li>
!! <li>den gesamten Berechnungs-Zeitraum</li>
!! </ul>
!! angesehen werden.
!! \n\n
!! <h2> Extinktionskoeffizienten </h2> 
!! Die \ref extnct_rb sind in der Gerris-Benutzeroberfläche nicht bearbeitbar.\n
!! Die Datei <a href="./exp/e_extnct.dat" target="_blank">e_extnct.dat</a> ist Teil der QSim-Installation.
!! 
!! <h2> Biologische Parameter </h2> 
!! In der Gerris-Benutzeroberfläche werden sie unter der Schaltfläche "QSim-Parameter" geführt.\n
!! Diese globalen Biologischen Parameter werden von QSim aus der Datei APARAM.txt gelesen. Es handelt sich dabei um folgende:
!!<table >
!!<tr><th>Position in APARAM.txt </th><th>QSim-Name</th><th> Beschreibung </th><th> Einheit </th><th> Zitat aus AParamParam.xml </th></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 1 :</td><td>agchl,aggmax,IKge,agksn,agksp </td></tr>
!!<tr><td>  | </td><td> \anchor agchl agchl	</td><td> Kohlenstoff/Chlorophyll Gr▒nalgen (dunkeladaptiert) bei 20°C </td><td> mgC/mgChla </td><td>  </td></tr>
!!<tr><td>  | </td><td> \anchor aggmax aggmax	</td><td> Max. Wachstumsrate d. Grünalgen  </td><td> 1/d </td><td>   Format= F5.2  Null= -1  Help= maximale Produktionsrate für Grünalgen  </td></tr>
!!<tr><td>  | </td><td> \anchor ikge ikge	</td><td> Lichtsättigung für Photosynthese der Grünalgen bei 20°C </td><td> µE/(m2*s) </td><td>   Format= F6.2  Null= -1  </td></tr> 
!!<tr><td>  | </td><td> \anchor agksn agksn	</td><td> Halbsättigungskonstante Grünalgen N </td><td> mg/l </td><td>   Format= F5.3  Null= -1  Help= Halbsättigungskonstante für N bei Grünalgen  </td></tr>
!!<tr><td>  | </td><td> \anchor agksp agksp	</td><td> Halbsättigungskonstante Grünalgen P </td><td> mg/l </td><td>   Format= F6.4  Null= -1  Help= Halbsättigungskonstante für P bei Grünalgen  </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 2 :</td><td>agremi,frmuge,bsbgr,csbgr,Qmx_NG </td></tr>
!!<tr><td>  | </td><td> \anchor agremi agremi	</td><td> Grundrespiration d. Grünalgen </td><td> 1/d </td><td>   Format= F5.3  Null= -1  </td></tr>
!!<tr><td>  | </td><td> \anchor frmuge frmuge	</td><td> Anteil der vom Wachstum abhängigigen Respiration (Grünalgen) </td><td>  - </td><td>   Format= F5.2  Null= -1   </td></tr>
!!<tr><td>  | </td><td> \anchor bsbgr bsbgr	</td><td> C-BSB5-Erhöhung Grünalgen </td><td> mg/mgC </td><td>   Format= F6.4  Null= -1  Help= C-BSB5-Erhöhung durch Grünalgen   </td></tr>
!!<tr><td>  | </td><td> \anchor csbgr csbgr	</td><td> CSB-Erhöhung Grünalgen </td><td> mg/mgC </td><td>   Format= F6.4  Null= -1  Help= CSB-Erhöhung durch Grünalgen  </td></tr>
!!<tr><td>  | </td><td> \anchor qmx_ng qmx_ng	</td><td> max. N-Gehalt der Grünalgenzelle </td><td> mg/mgBio </td><td>   Format= F7.5  Null= -1  Help= Stickstoffgehalt der Grünalgenbiomasse  </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 3 :</td><td>Qmx_PG,Qmn_NG,Qmn_PG,upmxNG,upmxPG </td></tr>
!!<tr><td>  | </td><td> \anchor qmx_pg qmx_pg	</td><td> max. P-Gehalt der Grünalgenzelle </td><td> mg/mgBio </td><td>   Format= F7.5  Null= -1  Help= Phosphorgehalt der Grünalgenbiomasse  </td></tr>
!!<tr><td>  | </td><td> \anchor qmn_ng qmn_ng	</td><td> min. N-Gehalt der Grünalgenzelle </td><td> mg/mgBio </td><td>   Format= F7.5  Null= -1   </td></tr>
!!<tr><td>  | </td><td> \anchor qmn_pg qmn_pg	</td><td> min. P-Gehalt der Grünalgenzelle </td><td> mg/mgBio </td><td>   Format= F7.5  Null= -1   </td></tr>
!!<tr><td>  | </td><td> \anchor upmxng upmxng	</td><td> max. N-Aufnahmerate der Grünalgen </td><td> 1/d </td><td>   Format= F5.3  Null= -1    </td></tr>
!!<tr><td>  | </td><td> \anchor upmxpg upmxpg	</td><td> max. P-Aufnahmerate der Grünalgen </td><td> 1/d </td><td>   Format= F5.3  Null= -1   </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 4 :</td><td>opgrmi,opgrma,asgre,ToptG,TmaxG</td></tr>
!!<tr><td>  | </td><td> \anchor opgrmi opgrmi	</td><td> Min. O2-Prod. Grünalgen </td><td> mg/mgBio </td><td>   Format= F4.2  Null= -1  Help= minimale Sauerstoffproduktion durch Grünalgen   </td></tr>
!!<tr><td>  | </td><td> \anchor opgrma opgrma	</td><td> Max. O2-Prod. Grünalgen </td><td> mg/mgBio </td><td>   Format= F4.2  Null= -1  Help= maximale Sauerstoffproduktion durch Grünalgen   </td></tr>
!!<tr><td>  | </td><td> \anchor asgre asgre	</td><td> Sediment Grünalgen </td><td> 0-1 </td><td>   Format= F5.2  Null= -1  Help= Sedimentierbarer Anteil für Grünalgen   </td></tr>
!!<tr><td>  | </td><td> \anchor toptg toptg	</td><td> optimal Temperatur für Grünalgenwachstum </td><td> °C </td><td>   Format= F5.2  Null= -1  Help= optimal Temperatur für Grünalgenwachstum   </td></tr>
!!<tr><td>  | </td><td> \anchor tmaxg tmaxg	</td><td> Letal-Temperatur für Grünalgenwachstum </td><td> °C </td><td>   Format= F5.2  Null= -1  Help= Letal-Temperatur für Grünalgenwachstum   </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 5 :</td><td>akchl,akgmax,IKke,akksn,akksp </td></tr>
!!<tr><td>  | </td><td> \anchor akchl akchl	</td><td> Kohlenstoff/Chlorophyll Kieselalgen (dunkeladaptiert) bei 20°C </td><td> mgC/mgChla </td><td>    </td></tr>
!!<tr><td>  | </td><td> \anchor akgmax akgmax	</td><td> Max. Wachstumsate d. Kieselalgen  </td><td> 1/d </td><td>   Format= F5.2  Null= -1  Help= maximale Wachstumsrate für Kieselalgen   </td></tr>
!!<tr><td>  | </td><td> \anchor ikke IKke	</td><td> Lichtsättigung für Photosynthese der Kieselalgen bei 20°C </td><td> µE/(m2*s) </td><td>   Format= F6.2  Null= -1  </td></tr>
!!<tr><td>  | </td><td> \anchor akksn akksn	</td><td> N-Halbsättigungskonstante Kieselalgen </td><td> mg/l </td><td>   Format= F5.3  Null= -1  Help= Halbsättigungskonstante für N bei Kieselalgen  </td></tr>
!!<tr><td>  | </td><td> \anchor akksp akksp	</td><td> P-Halbsättigungskonstante Kieselalgen </td><td> mg/l </td><td>   Format= F6.4  Null= -1  Help= Halbsättigungskonstante für P bei Kieselalgen  </td></tr>
!!<tr><td bgcolor= "#888888" ></td>Zeile 6 :</td><td>akkssi,akremi,frmuke,bsbki,csbki </td></tr>
!!<tr><td>  | </td><td> \anchor akkssi akkssi	</td><td> Si-Halbsättigungskonstante Kieselalgen </td><td> mg/l </td><td>   Format= F5.3  Null= -1  Help= Halbsättigungskonstante für SI bei Kieselalgen  </td></tr>
!!<tr><td>  | </td><td> \anchor akremi akremi	</td><td> Grundrespiration d. Kieselalgen </td><td> 1/d </td><td>   Format= F5.3  Null= -1  Help= minimale Respirationsrate für Kieselalgen  </td></tr>
!!<tr><td>  | </td><td> \anchor frmuke frmuke	</td><td> Anteil der vom Wachstum abhängigigen Respiration (Kieselalgen) </td><td>  - </td><td>   Format= F5.2  Null= -1  Help=   Default=  </td></tr>
!!<tr><td>  | </td><td> \anchor bsbki bsbki	</td><td> C-BSB5-Erhöhung Kieselalgen </td><td> mg/mgC </td><td>   Format= F6.4  Null= -1  Help= C-BSB5-Erhöhung durch Kieselalgen   </td></tr>
!!<tr><td>  | </td><td> \anchor csbki csbki	</td><td> CSB-Erhöhung Kieselalgen </td><td> mg/mgC </td><td>   Format= F6.4  Null= -1  Help= CSB-Erhöhung durch Kieselalgen   </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 7 :</td><td>Qmx_NK,Qmx_PK,Qmx_SK,Qmn_NK,Qmn_PK </td></tr>
!!<tr><td>  | </td><td> \anchor qmx_nk qmx_nk	</td><td> max. N-Gehalt der Kieselalgenzelle </td><td> mgN/mgBio </td><td>   Format= F7.5  Null= -1  Help= Stickstoffgehalt der Kieselalgenbiomasse   </td></tr>
!!<tr><td>  | </td><td> \anchor qmx_pk qmx_pk	</td><td> max. P-Gehalt der Kieselalgenzelle </td><td> mgP/mgBio </td><td>   Format= F7.5  Null= -1  Help= Phosphorgehalt der Kieselalgenbiomasse  </td></tr>
!!<tr><td>  | </td><td> \anchor qmx_sk qmx_sk	</td><td> max. Si-Gehalt der Kieselalgenzelle </td><td> mgSi/mgBio </td><td>   Format= F7.5  Null= -1  Help= Silikatgehalt der Kieselalgenbiomasse  </td></tr>
!!<tr><td>  | </td><td> \anchor qmn_nk qmn_nk	</td><td> min. N-Gehalt der Kieselalgenzelle </td><td> mgN/mgBio </td><td>   Format= F7.5  Null= -1   </td></tr>
!!<tr><td>  | </td><td> \anchor qmn_pk qmn_pk	</td><td> min. P-Gehalt der Kieselalgenzelle </td><td> mgP/mgBio </td><td>   Format= F7.5  Null= -1   </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 8 :</td><td>Qmn_SK,upmxNK,upmxPK,upmxSK,opkimi </td></tr>
!!<tr><td>  | </td><td> \anchor qmn_sk qmn_sk	</td><td> min. Si-Gehalt der Kieselalgenzelle </td><td> mgSi/mgBio </td><td>   Format= F7.5  Null= -1  </td></tr>
!!<tr><td>  | </td><td> \anchor upmxnk upmxnk	</td><td> max. N-Aufnahmerate der Kieselalgen </td><td> 1/d </td><td>   Format= F5.3  Null= -1   </td></tr>
!!<tr><td>  | </td><td> \anchor upmxpk upmxpk	</td><td> max. P-Aufnahmerate der Kieselalgen </td><td> 1/d </td><td>   Format= F5.3  Null= -1   </td></tr>
!!<tr><td>  | </td><td> \anchor upmxsk upmxsk	</td><td> max. Si-Aufnahmerate der Kieselalgen </td><td> 1/d </td><td>   Format= F5.3  Null= -1  </td></tr>
!!<tr><td>  | </td><td> \anchor opkimi opkimi	</td><td> Min. O2-Prod. Kieselalgen </td><td> mgO2/mgBio </td><td>   Format= F4.2  Null= -1  Help= minimale Sauerstoffproduktion durch Kieselalgen  </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 9 :</td><td> opkima, askie, ToptK, kTemp_Ki, abchl </td></tr>
!!<tr><td>  | </td><td> \anchor opkima opkima	</td><td> Max. O2-Prod. Kieselalgen </td><td> mg/mgBio </td><td>   Format= F4.2  Null= -1  Help= maximale Sauerstoffproduktion durch Kieselalgen  </td></tr> 
!!<tr><td>  | </td><td> \anchor askie askie	</td><td> Sediment Kieselalgen </td><td> 0-1 </td><td>   Format= F5.2  Null= -1  Help= Sedimentierbarer Anteil für Kieselalgen   </td></tr> 
!!<tr><td>  | </td><td> \anchor toptk toptk	</td><td> optimal Temperatur für Kieselalgenwachstum </td><td> °C </td><td> Cyclotella meneghiniana: 27.9°C  Default= 20  Min= 0  Max= 99.99 </td></tr>
!!<tr><td>  | </td><td> \anchor ktemp_ki ktemp_ki </td><td> empirische Konstante KT(µ) für Temperaturabhängigkeit (Exponent) </td><td> 1/°C </td><td> Format= F7.5  Null= -1  
!! Help= Cyclotella meneghiniana: 0.003  Default= 0.0056  Min= 0  Max= 9.99999  Gruppe= Kieselalgen  Kategorie= Temperatur  </td></tr>
!!<tr><td>  | </td><td> \anchor abchl abchl	</td><td> "Kohlenstoff/Chlorophyll Blaualgen </td><td> mgC/mgChla </td><td>   </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 10 :</td><td>abgmax,IKbe,abksn,abksp,abremi </td></tr>
!!<tr><td>  | </td><td> \anchor abgmax abgmax	</td><td> Max. Wachstumsrate d. Blaualgen  </td><td> 1/d </td><td>   Format= F5.2  Null= -1  Help= maximale Wachstumsrate für Blaualgen</td></tr>
!!<tr><td>  | </td><td> \anchor ikbe ikbe	</td><td> Lichtsättigung für Photosynthese der Blaualgen bei 20°C </td><td> µE/m2*s) </td><td>   Format= F6.2  Null= -1 </td></tr>
!!<tr><td>  | </td><td> \anchor abksn abksn	</td><td> N-Halbsättigung Blaualgen </td><td> mg/l </td><td>   Format= F5.3  Null= -1  Help= Halbsättigungskonstante für N bei Blaualgen  </td></tr>
!!<tr><td>  | </td><td> \anchor abksp abksp	</td><td> P-Halbsättigung Blaualgen </td><td> mg/l </td><td>   Format= F6.4  Null= -1  Help= Halbsättigungskonstante für P bei Blaualgen  </td></tr>
!!<tr><td>  | </td><td> \anchor abremi abremi	</td><td> Grundrespiration d. Blaualgen </td><td> 1/d </td><td>   Format= F5.3  Null= -1  Help= minimale Respirationsrate für Blaualgen  </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 11 :</td><td>frmube,bsbbl,csbbl,Qmx_NB,Qmx_PB </td></tr>
!!<tr><td>  | </td><td> \anchor frmube frmube	</td><td> Anteil der vom Wachstum abhängigigen Respiration (Blaulalgen) </td><td>  - </td><td>   Format= F5.2  Null= -1  </td></tr>
!!<tr><td>  | </td><td> \anchor bsbbl bsbbl	</td><td> C-BSB5-Erhöhung Blaualgen </td><td> mg/mgC </td><td>   Format= F6.4  Null= -1  Help= C-BSB5-Erhöhung durch Blaualgen  </td></tr>
!!<tr><td>  | </td><td> \anchor csbbl csbbl	</td><td> CSB-Erhöhung Blaualgen </td><td> mg/mgCS </td><td>   Format= F6.4  Null= -1  Help= CSB-Erhöhung durch Blaualgen  </td></tr>
!!<tr><td>  | </td><td> \anchor qmx_nb qmx_nb	</td><td> max. N-Gehalt der Blaualgenzelle </td><td> mg/mgBio </td><td>   Format= F7.5  Null= -1  Help= Stickstoffgehalt der Blaualgenbiomasse  </td></tr>
!!<tr><td>  | </td><td> \anchor qmx_pb qmx_pb	</td><td> max. P-Gehalt der Blaualgenzelle </td><td> mgP/mgBio </td><td>   Format= F7.5  Null= -1  Help= Phosphorgehalt der Blaualgenbiomasse  </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 12 :</td><td>Qmn_NB,Qmn_PB,upmxNB,upmxPB,opblmi </td></tr>
!!<tr><td>  | </td><td> \anchor qmn_nb qmn_nb	</td><td> min. N-Gehalt der Blaualgenzelle </td><td> mgN/mgBio </td><td>   Format= F7.5  Null= -1  </td></tr>
!!<tr><td>  | </td><td> \anchor qmn_pb qmn_pb	</td><td> min. P-Gehalt der Blaualgenzelle </td><td> mgP/mgBio </td><td>   Format= F7.5  Null= -1  </td></tr>
!!<tr><td>  | </td><td> \anchor upmxnb upmxnb	</td><td> max. N-Aufnahmerate der Blaualgen </td><td> 1/d </td><td>   Format= F5.3  Null= -1  </td></tr>
!!<tr><td>  | </td><td> \anchor upmxpb upmxpb	</td><td> max. P-Aufnahmerate der Blaualgen </td><td> 1/d </td><td>   Format= F5.3  Null= -1  </td></tr>
!!<tr><td>  | </td><td> \anchor opblmi opblmi	</td><td> Min. O2-Prod. Blaualgen </td><td> mg/mgBio </td><td>   Format= F4.2  Null= -1  Help= minimale Sauerstoffproduktion durch Blaualgen  </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 13 :</td><td>opblma,asble,ToptB,TmaxB,ifix</td></tr>
!!<tr><td>  | </td><td> \anchor opblma opblma	</td><td> Max. O2-Prod. Blaualgen </td><td> mg/mgBio </td><td>   Format= F4.2  Null= -1  Help= maximale Sauerstoffproduktion durch Blaualgen  </td></tr>
!!<tr><td>  | </td><td> \anchor asble asble	</td><td> Sediment Blaualgen </td><td> 0-1 </td><td>   Format= F5.2  Null= -1  Help= Sedimentierbarer Anteil für Blaualgen  </td></tr>
!!<tr><td>  | </td><td> \anchor toptb toptb	</td><td> optimal Temperatur für Blaualgenwachstum </td><td> °C </td><td>   Format= F5.2  Null= -1  Help= optimal Temperatur für Blaualgenwachstum  </td></tr>
!!<tr><td>  | </td><td> \anchor tmaxb tmaxb	</td><td> Letal-Temperatur für Blaualgenwachstum </td><td> °C </td><td>   Format= F5.2  Null= -1  Help= Letal-Temperatur für Blaualgenwachstum  </td></tr>
!!<tr><td>  | </td><td> \anchor ifix ifix	</td><td> Luftstickstofffixierer (0/1) </td><td>  </td><td>   Format= I2  Null= -1  Help= Luftstickstofffixierer(0:Nein/1:Ja)  </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 14 :</td><td>irmaxe,FopIRe,GRote,zresge,zakie </td></tr>
!!<tr><td>  | </td><td> \anchor irmaxe irmaxe	</td><td> max. Gewichtsspez. Algenaufnahmerate d. Rotatorien </td><td> µgC*µgC-2/3*d-1 ??? </td><td>   Format= F5.2  Null= -1  Help= Max. Ingestionsrate für Rotatorien  </td></tr>
!!<tr><td>  | </td><td> \anchor fopire fopire	</td><td> Halbsättigungskonstante für Futteraufnahme d. Rotatorien </td><td> mg/l </td><td>   Format= F5.2  Null= -1  Help= Optimale Futterkonzentration für Rotatorienwachstum  </td></tr>
!!<tr><td>  | </td><td> \anchor grote grote	</td><td> durchschnittliches Gewicht einer Rotatorie </td><td> µg </td><td>   Format= F5.2  Null= -1  Help= Gewicht einer Rotatorie  </td></tr>
!!<tr><td>  | </td><td> \anchor zresge zresge	</td><td> Grundrespiration Rotatorien </td><td> 1/d </td><td>   Format= F5.3  Null= -1  Help= Grundrespiration der Rotatorien  </td></tr>
!!<tr><td>  | </td><td> \anchor zakie zakie	</td><td> Filtrierbarkeit Kieselalgen </td><td> 0-1 </td><td>   Format= F5.2  Null= -1  Help= Filtrierbarkeit der Kieselalgen durch Rotatorien  </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 15 :</td><td>zagre,zable,ynmx1e,stks1e,anitrie </td></tr>
!!<tr><td>  | </td><td> \anchor zagre zagre	</td><td> Filtrierbarkeit Grünalgen </td><td> 0-1 </td><td>   Format= F5.2  Null= -1  Help= Filtrierbarkeit der Grünalgen durch Rotatorien  </td></tr>
!!<tr><td>  | </td><td> \anchor zable zable	</td><td> Filtrierbarkeit Blaualgen </td><td> 0-1 </td><td>   Format= F5.2  Null= -1  Help= Filtrierbarkeit der Blaualgen durch Rotatorien  </td></tr>
!!<tr><td>  | </td><td> \anchor ynmx1e ynmx1e (YNMAX1)	</td><td> Max. Wachstum Nitrosomonas </td><td> 1/d </td><td>   Format= F4.2  Null= -1  Help= Max. Wachstumsrate der Nitrosomonas  </td></tr>
!!<tr><td>  | </td><td> \anchor stks1e stks1e	</td><td> Halbsättigung Nitrosomonas </td><td> mgNH4-N/l </td><td>   Format= F5.2  Null= -1  Help= Halbsättigungskonstante für Nitrosomonas  </td></tr>
!!<tr><td>  | </td><td> \anchor anitrie anitrie	</td><td> Absterberate Nitrosomonas </td><td> 1/d </td><td>   Format= F4.2  Null= -1  Help= Absterberate für Nitrosomonas  </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 16 :</td><td>bnmx1e,bnks1e,ynmx2e,stks2e,anitri2e</td></tr>
!!<tr><td>  | </td><td> \anchor bnmx1e bnmx1e (BNMX1)	</td><td> Max. Umsatz Nitrosomonas </td><td> gNH4-N/(m²*l) </td><td>   Format= F5.2  Null= -1  Help= Max. Umsatzrate sessiler Nitrosomonas  </td></tr>
!!<tr><td>  | </td><td> \anchor bnks1e bnks1e	</td><td> Halbsätt. sessiler Nitrosomonas </td><td> mg/l </td><td>   Format= F5.2  Null= -1  Help= Halbsättigungskonstante der sessilen Nitrosomonas  </td></tr>
!!<tr><td>  | </td><td> \anchor ynmx2e ynmx2e (YNMAX2)	</td><td> Max. Wachstum Nitrobacter </td><td> 1/d </td><td>   Format= F4.2  Null= -1  Help= Max. Wachstumsrate der Nitrobacter  </td></tr>
!!<tr><td>  | </td><td> \anchor stks2e stks2e	</td><td> Halbsättigung Nitrobacter </td><td> mgNO2-N/l </td><td>   Format= F5.2  Null= -1  Help= Halbsättigungskonstante für Nitrobacter  </td></tr>
!!<tr><td>  | </td><td> \anchor anitri2e anitri2e</td><td> Absterberate Nitrobacter </td><td> 1/d </td><td>   Format= F4.2  Null= -1  Help= Absterberate für Nitrobacter  </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 17 :</td><td>bnmx2e,bnks2e,KNH4e,KapN3e,hyPe </td></tr>
!!<tr><td>  | </td><td> \anchor bnmx2e bnmx2e (BNMX2)	</td><td> Max. Umsatz Nitrobacter </td><td> gNO2-N/(m2*l) </td><td>   Format= F5.2  Null= -1  Help= Max. Umsatzrate sessiler Nitrobacter  </td></tr>
!!<tr><td>  | </td><td> \anchor bnks2e bnks2e	</td><td> Halbsätt. sessiler Nitrobacter </td><td> mg/l </td><td>   Format= F5.2  Null= -1  Help= Halbsättigungskonstante der sessilen Nitrobacter  </td></tr>
!!<tr><td>  | </td><td> \anchor knh4e knh4e	</td><td> NH4-Umsatzgeschw. im Sediment </td><td> m/d </td><td>   Format= F5.2  Null= -1  Help= Saar: 0.28;  Havel: 0.19   </td></tr>
!!<tr><td>  | </td><td> \anchor kapn3e kapn3e	</td><td> Denitrifikationsgeschw. im Sediment </td><td> m/d </td><td>   Format= F5.2  Null= -1  Help= Saar: 0.06;  Havel: 0.15  </td></tr>
!!<tr><td>  | </td><td> \anchor hype hype	</td><td> Hydrolyserate für die leicht abbaubaren partikulären organischen C-Verbindungen  </td><td> 1/d </td><td>   Format= F6.3  </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 18 :</td><td>hymxDe,KsD1e,KsD2e,KsMe,upBACe </td></tr>
!!<tr><td>  | </td><td> \anchor hymxde hymxde	</td><td> maximale Hydrolyserate für die leicht abbaubaren gelösten organischen C-Verbindungen  </td><td> 1/d </td><td>   Format= F6.3  Null= -1  </td></tr>
!!<tr><td>  | </td><td> \anchor ksd1e ksd1e	</td><td> Halbsättigungskonstante für die Hydrolyse der leicht abbaubaren gelöster organischer C-Verbindungen </td><td> mgC/l </td><td>   Format= F6.3  Null= -1  H</td></tr>
!!<tr><td>  | </td><td> \anchor ksd2e ksd2e	</td><td> Halbsättigungskonstante für die Hydrolyse der schwer abbaubaren gelöster organischer C-Verbindungen </td><td> mgC/l </td><td>   Format= F6.3  Null= -1  </td></tr>
!!<tr><td>  | </td><td> \anchor ksme ksme	</td><td> Halbsättigungskonst. für den Abbau monomerer C-Verbindungen (?? für die Aufnahme von Kohlenstoff durch heterotrophen Bakterien??) </td><td> mgC/l </td><td> 
!!   Format= F6.3  Null= -1  </td></tr>
!!<tr><td>  | </td><td> \anchor upbace upbace	</td><td> max. Aufnahmerate monomerer C-Verbindungen d. Bakterien </td><td> 1/d </td><td>   Format= F6.3  Null= -1  </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 19 :</td><td>YBACe,rsGBACe,FoptDe,upHNFe,BACkse </td></tr>
!!<tr><td>  | </td><td> \anchor ybace ybace	</td><td> Ertragskoeffizient für Bakterienbiomasse </td><td>  - </td><td>   Format= F6.3  Null= -1  </td></tr>
!!<tr><td>  | </td><td> \anchor rsgbace rsgbace	</td><td> Grundrespiration het. Bakterien </td><td> 1/d </td><td>   Format= F6.3  Null= -1  </td></tr>
!!<tr><td>  | </td><td> \anchor foptde foptde	</td><td> Opt. Futterkonz. Dreissena </td><td> mgC/l </td><td>   Format= F5.2  Null= -1  Help= Optimale Futterkonzentration für Dreissena-Wachstum  </td></tr>
!!<tr><td>  | </td><td> \anchor uphnfe uphnfe	</td><td> max. Aufnahmerate der HNF </td><td> 1/d </td><td>   Format= F5.2  Null= -1  Help= Aufnahmerate heterotropher Nanoflagelaten  </td></tr>
!!<tr><td>  | </td><td> \anchor backse backse	</td><td> Halbsättigungsk. für BaK.-Aufnahme durch HNF </td><td> mgC/l </td><td>   Format= F6.4  Null= -1  </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 20 :</td><td>alamda,fPOC1e,fPOC2e,SorpCape,Klange</td></tr>
!!<tr><td>  | </td><td> \anchor alamda alamda	</td><td> Absorptionskoeff. für Gelbstoffe bei 440 nm </td><td> - </td><td>   Format= F5.3  Null= -1   </td></tr>
!!<tr><td>  | </td><td> \anchor fpoc1e fpoc1e	</td><td> leichtabbaubarer Anteil d. Sedimentkohlenstoffs </td><td>  -  </td><td>   Format= F5.2  Null=   Help= (Literaturwert: 0.65  </td></tr>
!!<tr><td>  | </td><td> \anchor fpoc2e fpoc2e	</td><td> schwerabbaubarer Anteil d. Sedimentkohlenstoffs </td><td>  -  </td><td>   Format= F5.2  Null=   Help= (Literaturwert: 0.15  </td></tr>
!!<tr><td>  | </td><td> \anchor sorpcape sorpcape</td><td> SorptionsKapazität für Phosphor </td><td> mgP/gTG </td><td>   Format= F6.2  Null= -1  Help= (Literaturwerte: Maxmalwert: 2.5; Eingabe: -1 -> Wert wird berechnet  </td></tr>
!!<tr><td>  | </td><td> \anchor klange klange	</td><td> Langmuirkoeffizient für Phosphorsorption </td><td> l/mgP </td><td>   Format= F6.3  Null= -1  </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 21 :</td><td>KdNh3e,RateCde,etaCde,RateCIe,xnueCe</td></tr>
!!<tr><td>  | </td><td> \anchor kdnh3e kdnh3e	</td><td> Partitionskoeffizient für Ammonium </td><td> l/kg </td><td>   Format= F5.2  Null= -1  Help= -1.-> Wert wird berechnet  </td></tr>
!!<tr><td>  | </td><td> \anchor ratecde	ratecde	</td><td>Grundmortalitätsrate coliformer Bakterien bei 20°C </td><td> 1/d </td><td>  Help="Grundmortalitätsrate coliformer Bakterien bei 20°C" Min="0.0" Max="10." Gruppe="Hygiene" Kategorie="Coliform"   </td></tr>
!!<tr><td>  | </td><td> \anchor etacde	etacde	</td><td>Temperaturkoeffizient </td><td> - </td><td>  Help="Temperaturkoeffizient" Min="1." Max="3." Gruppe="Hygiene" Kategorie="Coliform"   </td></tr>
!!<tr><td>  | </td><td> \anchor ratecie	ratecie	</td><td>Inaktivierungskoeffizient im Licht </td><td> m2*MJ-1 </td><td>  Help="Inaktivierungskoeffizient im Licht" Min="0.0" Max="99.99" Gruppe="Hygiene" Kategorie="Coliform"   </td></tr>
!!<tr><td>  | </td><td> \anchor xnuece	xnuece	</td><td>dimensionsloser Parameter </td><td> - </td><td>  Help="dimensionsloser Parameter zur Beschreibung der Inaktiv. im Licht" Min="1.0" Max="999.99" Gruppe="Hygiene" Kategorie="Coliform"   </td></tr>
!!<tr><td bgcolor="#888888"></td>Zeile 22 :</td><td>RateCGe,RateCSe</td></tr>
!!<tr><td>  | </td><td> \anchor ratecge	ratecge	</td><td>Verlustrate durch Grazing </td><td> d-1 </td><td> Help="Coliforme Verlustrate durch Grazing" Min="0.0" Max="9.999" Gruppe="Hygiene" Kategorie="Coliform"   </td></tr>
!!<tr><td>  | </td><td> \anchor ratecse	ratecse	</td><td>Verlustrate durch Sedimentation </td><td> d-1 </td><td>  Help="Coliforme Verlustrate durch Sedimentation" Min="0" Max="9.999" Gruppe="Hygiene" Kategorie="Coliform"   </td></tr>
!!</table>
!!\n\n
!! Beispiel:\n
!! <a href="./exp/APARAM_200314.txt" target="_blank">APARAM.txt, Version vom 20.03.2014</a>\n
!! <a href="./exp/AParam_kommentiert.txt" target="_blank">APARAM.txt (kommentiert) v12.40</a>\n
!! <a href="./exp/aparam_gerris.xls" target="_blank">Parameterliste Volker 19dez12</a>\n
!! <a href="./exp/aparam_gerris_13.10.xls" target="_blank">überarbeitete Prameterliste 20dez12 wy</a>\n\n
!! die aktuelle <a href="./exp/AParamParam.xml" target="_blank"> AParamParam.xml </a> enthält die Parameterdefinitionen im
!! xml-Format. Diese Datei dient der Synchronisation mit der Benutzeroberfläche Gerris \ref Gerris .\n\n
!!\n\n
!! <a href="./pdf/Schnittstelle_QSIM.pdf" target="_blank">Schnittstellenbeschreibung Gerris-QSim</a>\n
!!
!! \n\n zurück: \ref Modellerstellung  Quelle: uebergabe_werte.f95
!----+-----+----
!> <h1>broadcast_parameter() alle prozessoren bekommen die Parameter aus aparam.txt</h1>
!! nicht mehr über transfer_parameter_p sondern direkt aus module_QSimDatenfelder.f95
!! aus qsim13.3:
!! <code>\verbatim
!!       read(55,5500,iostat=read_error)agchl,aggmax,IKge,agksn,agksp 
!!       read(55,5502,iostat=read_error)agremi,frmuge,bsbgr,csbgr,Qmx_NG 
!!       read(55,5504,iostat=read_error)Qmx_PG,Qmn_NG,Qmn_PG,upmxNG,upmxPG 
!!       read(55,5506,iostat=read_error)opgrmi,opgrma,asgre,ToptG,kTemp_Gr
!!       read(55,5507,iostat=read_error)akchl,akgmax,IKke,akksn,akksp 
!!       read(55,5508,iostat=read_error)akkssi,akremi,frmuke,bsbki,csbki 
!!       read(55,5510,iostat=read_error)Qmx_NK,Qmx_PK,Qmx_SK,Qmn_NK,Qmn_PK 
!!       read(55,5512,iostat=read_error)Qmn_SK,upmxNK,upmxPK,upmxSK,opkimi 
!!       read(55,5514,iostat=read_error)opkima,askie,ToptK,kTemp_Ki,abchl 
!!       read(55,5516,iostat=read_error)abgmax,IKbe,abksn,abksp,abremi 
!!       read(55,5518,iostat=read_error)frmube,bsbbl,csbbl,Qmx_NB,Qmx_PB 
!!       read(55,5520,iostat=read_error)Qmn_NB,Qmn_PB,upmxNB,upmxPB,opblmi 
!!       read(55,5522,iostat=read_error)opblma,asble,ToptB,kTemp_Bl,ifix
!!       read(55,5524,iostat=read_error)irmaxe,FopIRe,GRote,zresge,zakie 
!!       read(55,5526,iostat=read_error)zagre,zable,ynmx1e,stks1e,anitrie 
!!       read(55,5530,iostat=read_error)bnmx1e,bnks1e,ynmx2e,stks2e,anitrie
!!       read(55,5528,iostat=read_error)bnmx2e,bnks2e,KNH4e,KapN3e,hyPe 
!!       read(55,5533,iostat=read_error)hymxDe,KsD1e,KsD2e,KsMe,upBACe 
!!       read(55,5535,iostat=read_error)YBACe,rsGBACe,FoptDe,upHNFe,BACkse 
!!       read(55,5538,iostat=read_error)alamda,fPOC1e,fPOC2e,SorpCape,Klange
!!       read(55,5540,iostat=read_error)KdNh3e
!! \endverbatim</code>
!! aus module_uebergabe_werte.f95



      subroutine broadcast_parameter()
      use modell
      use QSimDatenfelder
      implicit none

!----------------------------------------------------------------- APARAM.txt
      call MPI_Bcast(agchl,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(aggmax,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(IKge,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(agksn,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(agksp,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 

      call MPI_Bcast(agremi,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(frmuge,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(bsbgr,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(csbgr,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(Qmx_NG ,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 

      call MPI_Bcast(Qmx_PG,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(Qmn_NG,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(Qmn_PG,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(upmxNG,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(upmxPG,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
  
      call MPI_Bcast(opgrmi,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(opgrma,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(asgre,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(ToptG,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(kTemp_Gr,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 

      call MPI_Bcast(akchl,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(akgmax,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(IKke,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(akksn,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(akksp ,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 

      call MPI_Bcast(akkssi,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(akremi,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(frmuke,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(bsbki,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(csbki ,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 

      call MPI_Bcast(Qmx_NK,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(Qmx_PK,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(Qmx_SK,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(Qmn_NK,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(Qmn_PK ,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 

      call MPI_Bcast(Qmn_SK,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(upmxNK,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(upmxPK,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(upmxSK,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(opkimi ,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 

      call MPI_Bcast(opkima,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(askie,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(ToptK,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(kTemp_Ki,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(abchl ,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 

      call MPI_Bcast(abgmax,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(IKbe,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(abksn,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(abksp,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(abremi ,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 

      call MPI_Bcast(frmube,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(bsbbl,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(csbbl,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(Qmx_NB,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(Qmx_PB ,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 

      call MPI_Bcast(Qmn_NB,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(Qmn_PB,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(upmxNB,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(upmxPB,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(opblmi ,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 

      call MPI_Bcast(opblma,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(asble,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(ToptB,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(kTemp_Bl,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(ifix,1,MPI_INT,0,mpi_komm_welt,ierr) !

      call MPI_Bcast(irmaxe,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(FopIRe,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(GRote,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(zresge,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(zakie ,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 

      call MPI_Bcast(zagre,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(zable,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(ynmx1e,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(stks1e,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(anitrie ,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 

      call MPI_Bcast(bnmx1e,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(bnks1e,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(ynmx2e,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(stks2e,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(anitri2e,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 

      call MPI_Bcast(bnmx2e,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(bnks2e,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(KNH4e,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(KapN3e,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(hyPe ,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 

      call MPI_Bcast(hymxDe,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(KsD1e,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(KsD2e,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(KsMe,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(upBACe ,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 

      call MPI_Bcast(YBACe,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(rsGBACe,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(FoptDe,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(upHNFe,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(BACkse ,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 

      call MPI_Bcast(alamda,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(fPOC1e,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(fPOC2e,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(SorpCape,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(Klange,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 

      call MPI_Bcast(KdNh3e,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
      call MPI_Bcast(ratecde,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
      call MPI_Bcast(etacde,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
      call MPI_Bcast(ratecie,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
      call MPI_Bcast(xnuece,1,MPI_FLOAT,0,mpi_komm_welt,ierr)

      call MPI_Bcast(ratecge,1,MPI_FLOAT,0,mpi_komm_welt,ierr)
      call MPI_Bcast(ratecse,1,MPI_FLOAT,0,mpi_komm_welt,ierr)

!-----------------------------------------------------------------weitere (ini_algae)
      call MPI_Bcast(Cagr,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(Caki,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(Cabl,1,MPI_FLOAT,0,mpi_komm_welt,ierr) ! 
      call MPI_Bcast(CZoo,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !

      call MPI_Bcast(a1Ki,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
      call MPI_Bcast(a2Ki,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
      call MPI_Bcast(a3Ki,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !

      call MPI_Bcast(a1Bl,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
      call MPI_Bcast(a2Bl,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
      call MPI_Bcast(a3Bl,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !

      call MPI_Bcast(a1Gr,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
      call MPI_Bcast(a2Gr,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
      call MPI_Bcast(a3Gr,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !

!-----------------------------------------------------------------weitere (orgc_start)
      call MPI_Bcast(TOC_CSB,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
      call MPI_Bcast(bsbZoo,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
!-----------------------------------------------------------------weitere (naehr2_start)
      call MPI_Bcast(nZoo,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !
      call MPI_Bcast(pZoo,1,MPI_FLOAT,0,mpi_komm_welt,ierr) !

      RETURN
      END subroutine broadcast_parameter

!
!> <h1>SUBROUTINE aparam_lesen()</h1>
!! Beschreibung siehe: \ref globaleParameter \n
!! Quelle: module_uebergabe_werte.f95
      SUBROUTINE aparam_lesen()
      use modell
      use QSimDatenfelder
      implicit none
      character(500) dateiname
      integer :: io_error, j,i
      logical :: logi
      real dummy

      !print*,'Parameter sollen gelesen werden'
      write(dateiname,'(2A)')trim(modellverzeichnis),'APARAM.txt'
      open ( unit =55 , file = dateiname, status ='old', action ='read ', iostat = io_error )
      !open(unit=55, DEFAULTFILE=cpfad, file='APARAM.txt') 
      if(io_error.ne.0)call qerror('open_error APARAM.txt ... Datei vorhanden?')
      rewind (55) 

      if(zeile(55))read(ctext,*,iostat=io_error)agchl,aggmax,IKge,agksn,agksp 
      if(io_error.ne.0) goto 198
      if(zeile(55))read(ctext,*,iostat=io_error)agremi,frmuge,bsbgr,csbgr,Qmx_NG 
      if(io_error.ne.0) goto 198
      if(zeile(55))read(ctext,*,iostat=io_error)Qmx_PG,Qmn_NG,Qmn_PG,upmxNG,upmxPG 
      if(io_error.ne.0) goto 198
      if(zeile(55))read(ctext,*,iostat=io_error)opgrmi,opgrma,asgre,ToptG,kTemp_Gr
      if(io_error.ne.0) goto 198
      if(zeile(55))read(ctext,*,iostat=io_error)akchl,akgmax,IKke,akksn,akksp 
      if(io_error.ne.0) goto 198
      if(zeile(55))read(ctext,*,iostat=io_error)akkssi,akremi,frmuke,bsbki,csbki 
      if(io_error.ne.0) goto 198
      if(zeile(55))read(ctext,*,iostat=io_error)Qmx_NK,Qmx_PK,Qmx_SK,Qmn_NK,Qmn_PK 
      if(io_error.ne.0) goto 198
      if(zeile(55))read(ctext,*,iostat=io_error)Qmn_SK,upmxNK,upmxPK,upmxSK,opkimi 
      if(io_error.ne.0) goto 198
!      if(zeile(55))read(ctext,*,iostat=io_error)opkima,askie,ToptK,TmaxK,abchl 
      if(zeile(55))read(ctext,*,iostat=io_error)opkima,askie,ToptK,kTemp_Ki,abchl 
      if(io_error.ne.0) goto 198
      if(zeile(55))read(ctext,*,iostat=io_error)abgmax,IKbe,abksn,abksp,abremi 
      if(io_error.ne.0) goto 198
      if(zeile(55))read(ctext,*,iostat=io_error)frmube,bsbbl,csbbl,Qmx_NB,Qmx_PB 
      if(io_error.ne.0) goto 198
      if(zeile(55))read(ctext,*,iostat=io_error)Qmn_NB,Qmn_PB,upmxNB,upmxPB,opblmi 
      if(io_error.ne.0) goto 198
      if(zeile(55))read(ctext,*,iostat=io_error)opblma,asble,ToptB,kTemp_Bl,ifix
      if(io_error.ne.0) goto 198
      if(zeile(55))read(ctext,*,iostat=io_error)irmaxe,FopIRe,GRote,zresge,zakie 
      if(io_error.ne.0) goto 198
      if(zeile(55))read(ctext,*,iostat=io_error)zagre,zable,ynmx1e,stks1e,anitrie 
      if(io_error.ne.0) goto 198
      if(zeile(55))read(ctext,*,iostat=io_error)bnmx1e,bnks1e,ynmx2e,stks2e,anitri2e
      if(io_error.ne.0) goto 198
      if(zeile(55))read(ctext,*,iostat=io_error)bnmx2e,bnks2e,KNH4e,KapN3e,hyPe 
      if(io_error.ne.0) goto 198
      if(zeile(55))read(ctext,*,iostat=io_error)hymxDe,KsD1e,KsD2e,KsMe,upBACe 
      if(io_error.ne.0) goto 198
      if(zeile(55))read(ctext,*,iostat=io_error)YBACe,rsGBACe,FoptDe,upHNFe,BACkse 
      if(io_error.ne.0) goto 198
      if(zeile(55))read(ctext,*,iostat=io_error)alamda,fPOC1e,fPOC2e,SorpCape,Klange
      if(io_error.ne.0) goto 198
      ratecde = 0.0 ! coliform evt. nicht angegeben
      etacde = 0.0
      ratecie = 0.0
      xnuece = 0.0
      ratecge = 0.0
      ratecse = 0.0
      if(zeile(55))then
         read(ctext,*,iostat=io_error)KdNh3e
         if(io_error.ne.0) goto 198
         read(ctext,*,iostat=io_error)dummy,ratecde,etacde,ratecie,xnuece
         if(io_error.eq.0)then
            print*,'APARAM.txt coliform parameters present'
            if(zeile(55))read(ctext,*,iostat=io_error)ratecge,ratecse
            !if(io_error.ne.0) goto 198
		 else
            print*,'APARAM.txt coliform parameters missing'
         endif ! coliformpresent
      end if !zeile 21

      if(kontrollknoten.gt.0)then
         print*,'agchl,aggmax,IKge,agksn,agksp =',agchl,aggmax,IKge,agksn,agksp
         print*,'agremi,frmuge,bsbgr,csbgr,Qmx_NG =',agremi,frmuge,bsbgr,csbgr,Qmx_NG
         print*,'Qmx_PG,Qmn_NG,Qmn_PG,upmxNG,upmxPG =',Qmx_PG,Qmn_NG,Qmn_PG,upmxNG,upmxPG
         print*,'opgrmi,opgrma,asgre,ToptG,kTemp_Gr =',opgrmi,opgrma,asgre,ToptG,kTemp_Gr
         print*,'akchl,akgmax,IKke,akksn,akksp =',akchl,akgmax,IKke,akksn,akksp
         print*,'akkssi,akremi,frmuke,bsbki,csbki =',akkssi,akremi,frmuke,bsbki,csbki
         print*,'Qmx_NK,Qmx_PK,Qmx_SK,Qmn_NK,Qmn_PK =',Qmx_NK,Qmx_PK,Qmx_SK,Qmn_NK,Qmn_PK
         print*,'Qmn_SK,upmxNK,upmxPK,upmxSK,opkimi =',Qmn_SK,upmxNK,upmxPK,upmxSK,opkimi
         print*,'opkima,askie,ToptK,kTemp_Ki,abchl =',opkima,askie,ToptK,kTemp_Ki,abchl 
         print*,'abgmax,IKbe,abksn,abksp,abremi =',abgmax,IKbe,abksn,abksp,abremi
         print*,'frmube,bsbbl,csbbl,Qmx_NB,Qmx_PB =',frmube,bsbbl,csbbl,Qmx_NB,Qmx_PB
         print*,'Qmn_NB,Qmn_PB,upmxNB,upmxPB,opblmi =',Qmn_NB,Qmn_PB,upmxNB,upmxPB,opblmi
         print*,'opblma,asble,ToptB,kTemp_Bl,ifix =',opblma,asble,ToptB,kTemp_Bl,ifix
         print*,'irmaxe,FopIRe,GRote,zresge,zakie =',irmaxe,FopIRe,GRote,zresge,zakie
         print*,'zagre,zable,ynmx1e,stks1e,anitrie =',zagre,zable,ynmx1e,stks1e,anitrie
         print*,'bnmx1e,bnks1e,ynmx2e,stks2e,anitri2e =',bnmx1e,bnks1e,ynmx2e,stks2e,anitri2e
         print*,'bnmx2e,bnks2e,KNH4e,KapN3e,hyPe =',bnmx2e,bnks2e,KNH4e,KapN3e,hyPe
         print*,'hymxDe,KsD1e,KsD2e,KsMe,upBACe =',hymxDe,KsD1e,KsD2e,KsMe,upBACe
         print*,'YBACe,rsGBACe,FoptDe,upHNFe,BACkse =',YBACe,rsGBACe,FoptDe,upHNFe,BACkse
         print*,'alamda,fPOC1e,fPOC2e,SorpCape,Klange =',alamda,fPOC1e,fPOC2e,SorpCape,Klange
         print*,'KdNh3e,ratecde,etacde,ratecie,xnuece =',KdNh3e,ratecde,etacde,ratecie,xnuece
         print*,'ratecge,ratecse =',ratecge,ratecse
      endif
      print*,'read successfully 107 parameters from APARAM.txt.'

      if( agchl .lt. 0.0)call qerror('APARAM.txt agchl negativ (nicht zulässig in QSim3D)')
      if( aggmax .lt. 0.0)call qerror('APARAM.txt aggmax negativ (nicht zulässig in QSim3D)')
      if( IKge .lt. 0.0)call qerror('APARAM.txt IKge negativ (nicht zulässig in QSim3D)')
      if( agksn .lt. 0.0)call qerror('APARAM.txt agksn negativ (nicht zulässig in QSim3D)')
      if( agksp .lt. 0.0)call qerror('APARAM.txt agksp negativ (nicht zulässig in QSim3D)')

      if( agremi .lt. 0.0)call qerror('APARAM.txt agremi negativ (nicht zulässig in QSim3D)')
      if( frmuge .lt. 0.0)call qerror('APARAM.txt frmuge negativ (nicht zulässig in QSim3D)')
      if( bsbgr .lt. 0.0)call qerror('APARAM.txt bsbgr negativ (nicht zulässig in QSim3D)')
      if( csbgr .lt. 0.0)call qerror('APARAM.txt csbgr negativ (nicht zulässig in QSim3D)')
      if( Qmx_NG .lt. 0.0)call qerror('APARAM.txt Qmx_NG negativ (nicht zulässig in QSim3D)')

      if( Qmx_PG .lt. 0.0)call qerror('APARAM.txt Qmx_PG negativ (nicht zulässig in QSim3D)')
      if( Qmn_NG .lt. 0.0)call qerror('APARAM.txt Qmn_NG negativ (nicht zulässig in QSim3D)')
      if( Qmn_PG .lt. 0.0)call qerror('APARAM.txt Qmn_PG negativ (nicht zulässig in QSim3D)')
      if( upmxNG .lt. 0.0)call qerror('APARAM.txt upmxNG negativ (nicht zulässig in QSim3D)')
      if( upmxPG .lt. 0.0)call qerror('APARAM.txt upmxPG negativ (nicht zulässig in QSim3D)')

      if( opgrmi .lt. 0.0)call qerror('APARAM.txt opgrmi negativ (nicht zulässig in QSim3D)')
      if( opgrma .lt. 0.0)call qerror('APARAM.txt opgrma negativ (nicht zulässig in QSim3D)')
      if( asgre .lt. 0.0)call qerror('APARAM.txt asgre negativ (nicht zulässig in QSim3D)')
      if( ToptG .lt. 0.0)call qerror('APARAM.txt ToptG negativ (nicht zulässig in QSim3D)')
      if( kTemp_Gr .lt. 0.0)call qerror('APARAM.txt kTemp_Gr negativ (nicht zulässig in QSim3D)')

      if( akchl .lt. 0.0)call qerror('APARAM.txt akchl negativ (nicht zulässig in QSim3D)')
      if( akgmax .lt. 0.0)call qerror('APARAM.txt akgmax negativ (nicht zulässig in QSim3D)')
      if( IKke .lt. 0.0)call qerror('APARAM.txt IKke negativ (nicht zulässig in QSim3D)')
      if( akksn .lt. 0.0)call qerror('APARAM.txt akksn negativ (nicht zulässig in QSim3D)')
      if( akksp .lt. 0.0)call qerror('APARAM.txt akksp negativ (nicht zulässig in QSim3D)')

      if( akkssi .lt. 0.0)call qerror('APARAM.txt akkssi negativ (nicht zulässig in QSim3D)')
      if( akremi .lt. 0.0)call qerror('APARAM.txt akremi negativ (nicht zulässig in QSim3D)')
      if( frmuke .lt. 0.0)call qerror('APARAM.txt frmuke negativ (nicht zulässig in QSim3D)')
      if( bsbki .lt. 0.0)call qerror('APARAM.txt bsbki negativ (nicht zulässig in QSim3D)')
      if( csbki .lt. 0.0)call qerror('APARAM.txt csbki negativ (nicht zulässig in QSim3D)')

      if( Qmx_NK .lt. 0.0)call qerror('APARAM.txt Qmx_NK negativ (nicht zulässig in QSim3D)')
      if( Qmx_PK .lt. 0.0)call qerror('APARAM.txt Qmx_PK negativ (nicht zulässig in QSim3D)')
      if( Qmx_SK .lt. 0.0)call qerror('APARAM.txt Qmx_SK negativ (nicht zulässig in QSim3D)')
      if( Qmn_NK .lt. 0.0)call qerror('APARAM.txt Qmn_NK negativ (nicht zulässig in QSim3D)')
      if( Qmn_PK .lt. 0.0)call qerror('APARAM.txt Qmn_PK negativ (nicht zulässig in QSim3D)')

      if( Qmn_SK .lt. 0.0)call qerror('APARAM.txt Qmn_SK negativ (nicht zulässig in QSim3D)')
      if( upmxNK .lt. 0.0)call qerror('APARAM.txt upmxNK negativ (nicht zulässig in QSim3D)')
      if( upmxPK .lt. 0.0)call qerror('APARAM.txt upmxPK negativ (nicht zulässig in QSim3D)')
      if( upmxSK .lt. 0.0)call qerror('APARAM.txt upmxSK negativ (nicht zulässig in QSim3D)')
      if( opkimi .lt. 0.0)call qerror('APARAM.txt opkimi negativ (nicht zulässig in QSim3D)')

      if( opkima .lt. 0.0)call qerror('APARAM.txt opkima negativ (nicht zulässig in QSim3D)')
      if( askie .lt. 0.0)call qerror('APARAM.txt askie negativ (nicht zulässig in QSim3D)')
      if( ToptK .lt. 0.0)call qerror('APARAM.txt ToptK negativ (nicht zulässig in QSim3D)')
      if( kTemp_Ki .lt. 0.0)call qerror('APARAM.txt kTemp_Ki negativ (nicht zulässig in QSim3D)')
      if( abchl .lt. 0.0)call qerror('APARAM.txt abchl negativ (nicht zulässig in QSim3D)')
 
      if( abgmax .lt. 0.0)call qerror('APARAM.txt abgmax negativ (nicht zulässig in QSim3D)')
      if( IKbe .lt. 0.0)call qerror('APARAM.txt IKbe negativ (nicht zulässig in QSim3D)')
      if( abksn .lt. 0.0)call qerror('APARAM.txt abksn negativ (nicht zulässig in QSim3D)')
      if( abksp .lt. 0.0)call qerror('APARAM.txt abksp negativ (nicht zulässig in QSim3D)')
      if( abremi .lt. 0.0)call qerror('APARAM.txt abremi negativ (nicht zulässig in QSim3D)')

      if( frmube .lt. 0.0)call qerror('APARAM.txt frmube negativ (nicht zulässig in QSim3D)')
      if( bsbbl .lt. 0.0)call qerror('APARAM.txt bsbbl negativ (nicht zulässig in QSim3D)')
      if( csbbl .lt. 0.0)call qerror('APARAM.txt csbbl negativ (nicht zulässig in QSim3D)')
      if( Qmx_NB .lt. 0.0)call qerror('APARAM.txt Qmx_NB negativ (nicht zulässig in QSim3D)')
      if( Qmx_PB .lt. 0.0)call qerror('APARAM.txt Qmx_PB negativ (nicht zulässig in QSim3D)')

      if( Qmn_NB .lt. 0.0)call qerror('APARAM.txt Qmn_NB negativ (nicht zulässig in QSim3D)')
      if( Qmn_PB .lt. 0.0)call qerror('APARAM.txt Qmn_PB negativ (nicht zulässig in QSim3D)')
      if( upmxNB .lt. 0.0)call qerror('APARAM.txt upmxNB negativ (nicht zulässig in QSim3D)')
      if( upmxPB .lt. 0.0)call qerror('APARAM.txt upmxPB negativ (nicht zulässig in QSim3D)')
      if( opblmi .lt. 0.0)call qerror('APARAM.txt opblmi negativ (nicht zulässig in QSim3D)')

      if( opblma .lt. 0.0)call qerror('APARAM.txt opblma negativ (nicht zulässig in QSim3D)')
      if( asble .lt. 0.0)call qerror('APARAM.txt asble negativ (nicht zulässig in QSim3D)')
      if( ToptB .lt. 0.0)call qerror('APARAM.txt ToptB negativ (nicht zulässig in QSim3D)')
      if( kTemp_Bl .lt. 0.0)call qerror('APARAM.txt kTemp_Bl negativ (nicht zulässig in QSim3D)')
      if( ifix .lt. 0.0)call qerror('APARAM.txt ifix negativ (nicht zulässig in QSim3D)')

      if( irmaxe .lt. 0.0)print*,'irmaxe wird berechnet in konsum()'
      if( FopIRe .lt. 0.0)print*,'FopIRe wird berechnet in konsum()'
      if( GRote .lt. 0.0)call qerror('APARAM.txt GRote negativ (nicht zulässig in QSim3D)')
      if( zresge .lt. 0.0)call qerror('APARAM.txt zresge negativ (nicht zulässig in QSim3D)')
      if( zakie .lt. 0.0)call qerror('APARAM.txt zakie negativ (nicht zulässig in QSim3D)')

      if( zagre .lt. 0.0)call qerror('APARAM.txt zagre negativ (nicht zulässig in QSim3D)')
      if( zable .lt. 0.0)call qerror('APARAM.txt zable negativ (nicht zulässig in QSim3D)')
      if( ynmx1e .lt. 0.0)call qerror('APARAM.txt ynmx1e negativ (nicht zulässig in QSim3D)')
      if( stks1e .lt. 0.0)call qerror('APARAM.txt stks1e negativ (nicht zulässig in QSim3D)')
      if( anitrie .lt. 0.0)call qerror('APARAM.txt anitrie negativ (nicht zulässig in QSim3D)')

      if( bnmx1e .lt. 0.0)call qerror('APARAM.txt bnmx1e negativ (nicht zulässig in QSim3D)')
      if( bnks1e .lt. 0.0)call qerror('APARAM.txt bnks1e negativ (nicht zulässig in QSim3D)')
      if( ynmx2e .lt. 0.0)call qerror('APARAM.txt ynmx2e negativ (nicht zulässig in QSim3D)')
      if( stks2e .lt. 0.0)call qerror('APARAM.txt stks2e negativ (nicht zulässig in QSim3D)')
      if( anitri2e .lt. 0.0)call qerror('APARAM.txt anitri2e negativ (nicht zulässig in QSim3D)')

      if( bnmx2e .lt. 0.0)call qerror('APARAM.txt bnmx2e negativ (nicht zulässig in QSim3D)')
      if( bnks2e .lt. 0.0)call qerror('APARAM.txt bnks2e negativ (nicht zulässig in QSim3D)')
      if( KNH4e .lt. 0.0)call qerror('APARAM.txt KNH4e negativ (nicht zulässig in QSim3D)')
      if( KapN3e .lt. 0.0)call qerror('APARAM.txt KapN3e negativ (nicht zulässig in QSim3D)')
      if( hyPe .lt. 0.0)call qerror('APARAM.txt hyPe negativ (nicht zulässig in QSim3D)')

      if( hymxDe .lt. 0.0)call qerror('APARAM.txthymxDe  negativ (nicht zulässig in QSim3D)')
      if( KsD1e .lt. 0.0)call qerror('APARAM.txt KsD1e negativ (nicht zulässig in QSim3D)')
      if( KsD2e .lt. 0.0)call qerror('APARAM.txt KsD2e negativ (nicht zulässig in QSim3D)')
      if( KsMe .lt. 0.0)call qerror('APARAM.txt KsMe negativ (nicht zulässig in QSim3D)')
      if( upBACe .lt. 0.0)call qerror('APARAM.txt upBACe negativ (nicht zulässig in QSim3D)')

      if( YBACe .lt. 0.0)call qerror('APARAM.txt YBACe negativ (nicht zulässig in QSim3D)')
      if( rsGBACe .lt. 0.0)call qerror('APARAM.txt rsGBACe negativ (nicht zulässig in QSim3D)')
      if( FoptDe .lt. 0.0)call qerror('APARAM.txt FoptDe negativ (nicht zulässig in QSim3D)')
      if( upHNFe .lt. 0.0)call qerror('APARAM.txt upHNFe negativ (nicht zulässig in QSim3D)')
      if( BACkse .lt. 0.0)call qerror('APARAM.txt BACkse negativ (nicht zulässig in QSim3D)')

      if( alamda .lt. 0.0)call qerror('APARAM.txt alamda negativ (nicht zulässig in QSim3D)')
      if( fPOC1e .lt. 0.0)call qerror('APARAM.txt fPOC1e negativ (nicht zulässig in QSim3D)')
      if( fPOC2e .lt. 0.0)call qerror('APARAM.txt fPOC2e negativ (nicht zulässig in QSim3D)')
      if( SorpCape .lt. 0.0)call qerror('APARAM.txt SorpCape negativ (nicht zulässig in QSim3D)')
      if( Klange .lt. 0.0)call qerror('APARAM.txt Klange negativ (nicht zulässig in QSim3D)')

      if( KdNh3e .lt. 0.0)print*,'Partitionskoeffizient für Ammonium KdNh3e wird berechnet in sedflux()'
      if( ratecde .lt. 0.0)call qerror('APARAM.txt ratecde negativ (nicht zulässig in QSim3D)')
      if( etacde .lt. 0.0)call qerror('APARAM.txt etacde negativ (nicht zulässig in QSim3D)')
      if( ratecie .lt. 0.0)call qerror('APARAM.txt ratecie negativ (nicht zulässig in QSim3D)')
      if( xnuece .lt. 0.0)call qerror('APARAM.txt xnuece negativ (nicht zulässig in QSim3D)')

      if( ratecge .lt. -1.0)call qerror('APARAM.txt ratecge kleiner -1 (nicht zulässig in QSim3D)')
      if( ratecse .lt. -1.0)call qerror('APARAM.txt ratecse kleiner -1 (nicht zulässig in QSim3D)')

      close (55)
      RETURN

  198 continue
      print*,io_error,trim(ctext)
      call qerror('Lesefehler APARAM.txt')

      END subroutine aparam_lesen

!----+-----+----

