Planktische, transportierte Variablen {#lnk_var_planktisch}
=====================================

Bei diesen Variablen handelt es sich zumeist um Konzentrationen von gelösten 
Wasserinhaltsstoffen oder um Güteparameter, die als gleichmäßig im Volumen 
verteilt betrachtet werden können und nur vernachlässigbare Relativbewegung im 
Wasser ausführen; z.B. Temperatur, Sauerstoff-Kohlenstoffverhältnis oder Anzahl 
der Rotatorien.
 
Die planktischen Variablen werden transportiert (advektiert und diffundiert). 
Siehe dazu \ref lnk_stofftransport_3d.
Dadurch werden sie auch in den nächsten Zeitschritt mitgenommen 
(Im Gegensatz zu: \ref lnk_uebergabewerte ).

Es definiert Felder für tiefengemittelte (planktonic_variable_p)
und vertikal aufgelöste Variablen (plankt_vari_vert, plankt_vari_vert_p).\n
Mit dem Suffix _p werden diejenigen Felder bezeichnet, welche für die parallele 
Programmbearbeitung benötigten werden. 
Sie sind auf allen (beim parallelen Rechnen zusammenwirkenden) Prozessen 
definiert (allokiert) und enthalten nur jene Knoten, die von dem zugehörigen 
Prozezess bearbeitet werden.
Die Gesamtfelder (Variablennamen ohne _p), enthalten alle Knoten, 
sind aber nur auf dem Prozess 0 angelegt (allokiert). 

Für die Parallelisierung mit MPI war es erforderlich, diese Felder einfach zu 
indizieren. Diese Feldnummern werden bei jedem Zugriff aus Knotennummer, 
Variablennummer und ggf. Tiefenschichtnummer explizit berechnet und nicht 
implizit aus der Mehrfachdimensionierung wie sonst in Fortran üblich.
 
Den Zusammenhang zu den in QSim1D verwendeten Variablennamen wird in den 
nachstehenden Tabellen aufgelistet und wird darüber hinaus auch vom Programm in 
den Feldern modell::planktonic_variable_name und modell::plankt_vari_vert_name 
mitgeführt. Die Namen werden in ini_planktkon0() gesetzt.


# Tiefengemittelte Konzentrationen {#lnk_tiefengemittelte_plankt_var} 

Die QSim-3D Nummer bezieht sich auf die Datenfelder modell::planktonic_variable 
und  modell::planktonic_variable_p
 
Die QSim-1D Namen werden in QSim3D im module_QSimDatenfelder.f95 vereinbart.\n 

| Nr. QSim-3D | Name QSim-1D | Beschreibung 					| Dimension	   |
|-------------|--------------|----------------------------------|--------------|
|  1 | \anchor tempw tempw	 | Wasser-Temperatur				| Grad Celsius |
|  2 | \anchor vo2 vo2		 | Sauerstoff	 					| mg O2 / l	   |
|  3 | \anchor vnh4 vnh4	 | Ammonium-Sticktoff				| mg NH4-N / l |
|  4 | \anchor vno2 vno2	 | Nitrit-Sticktoff					| mg NO2-N / l |
|  5 | \anchor vno3 vno3	 | Nitrat-Sticktoff					| mg NO3-N / l |
|  6 | \anchor gelp gelp	 | gelöster ortho-Phosphat-Phosphor	| mg PO4-P / l |
|  7 | \anchor si si		 | Silikat-Silizium					| mg Si / l	   |
|  8 | \anchor aki aki		 | Kiesel-Algen \ref Biomasse		| mgBio/l 	   |
|  9 | \anchor agr agr		 | Gruen-Algen \ref Biomasse		| mgBio/l 	   |
| 10 | \anchor abl abl		 | Blau-Algen \ref Biomasse			| mgBio/l 	   |
| 11 | \anchor chla chla	 | Gesamt Chlorophyll-a				| µgChla/l 	   |
| 12 | \anchor chlaki chlaki | Chlorophyll-a Kiesela.			| µgChla/l 	   |
| 13 | \anchor chlagr chlagr | Chlorophyll-a Grüna.				| µgChla/l 	   |
| 14 | \anchor chlabl chlabl | Chlorophyll-a Blaua.				| µgChla/l 	   |
| 15 | \anchor vx0 vx0		 | Biomasse der Nitrosomonas		| mgBio/l	   |
| 16 | \anchor vx02 vx02	 | Biomasse der Nitrobacter			| mgBio/l	   |
| 17 | \anchor obsb obsb	 | C-BSB5 ohne lebende Organismen, biologischer Sauerstoffbedarf in 5 Tage der organischen Kohlenstoffverbindungen | mgO2/l	|
| 18 | \anchor ocsb ocsb	 | C-CSB, chemischer Sauerstoffbedarf der organischen Kohlenstoffverbindungen			| mgO2/l	|
| 19 | \anchor vkigr vkigr	 | Anteil Kiesela. an Gesamtalgenmasse	|  -	   |
| 20 | \anchor antbl antbl	 | Anteil Blau an Gesamtalgenmasse		|  -       |
| 21 | \anchor svhemk svhemk | Lichtinhibition Kieselalgenwachstum 	|		   |
| 22 | \anchor svhemg svhemg | Lichtinhibition Grünalgenwachstum	|		   |
| 23 | \anchor svhemb svhemb | Lichtinhibition Blaualgenwachstum	|		   |
| 24 | \anchor akbcm akbcm | Kohlenstoff zu Chlorophyll-a Verhältnis in Kiesel-Algen | mg C/ mg Chla	|
| 25 | \anchor agbcm agbcm | Kohlenstoff zu Chlorophyll-a Verhältnis in Grün-Algen   | mg C/ mg Chla	|
| 26 | \anchor abbcm abbcm | Kohlenstoff zu Chlorophyll-a Verhältnis in Blau-Algen	 | mg C/ mg Chla	|
| 27 | \anchor akiiv akiiv	| entallen								|	 	|
| 28 | \anchor agriv agriv	| entallen								|	 	|
| 29 | \anchor abliv abliv	| entallen								|		|
| 30 | \anchor q_nk q_nk	| Stickstoffanteil der Algenbiomasse kiesel	 | mg N /mg bio	|
| 31 | \anchor q_pk q_pk	| Phosphoranteil der Algenbiomasse kiesel	 | mg P /mg bio	|
| 32 | \anchor q_sk q_sk	| Siliziumanteil der Algenbiomasse kiesel	 | mg Si/mg bio	|
| 33 | \anchor q_ng q_ng	| Stickstoffanteil der Algenbiomasse gruen	 | mg N /mg bio	|
| 34 | \anchor q_pg q_pg	| Phosphoranteil der Algenbiomasse gruen	 | mg P /mg bio	|
| 35 | \anchor q_nb q_nb	| Stickstoffanteil der Algenbiomasse blau	 | mg N /mg bio	|
| 36 | \anchor q_pb q_pb	| Phosphoranteil der Algenbiomasse blau		 | mg P /mg bio	|
| 37 | \anchor cd1 cd1		| leicht abbaubare gelöste organische C-Verbindungen | mg C / l	|
| 38 | \anchor cd2 cd2		| schwer abbaubare gelöste organische C-Verbindungen | mg C / l	|
| 39 | \anchor cp1 cp1		| leicht abbaubare partikuläre organische C-Verbindungen | mg C / l	|
| 40 | \anchor cp2 cp2		| schwer abbaubare partikuläre organische C-Verbindungen | mg C / l	|
| 41 | \anchor cm cm		| monomolekularen organischen C-Verbindungen			 | mg C / l	|
| 42 | \anchor bac bac		| Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen	| mg C / l	|
| 43 | \anchor o2bsb o2bsb	| Sauerstoff-Kohlenstoffverhältnis beim C-Abbau				| mgO2/mgC	|
| 44 | \anchor bl01 bl01	| schwerabbaubare Kohlenstoffverbindungen als Sauerstoffäquivalent 	| mg O2 / l 	|
| 45 | \anchor bl02 bl02	| leichtabbaubare Kohlenstoffverbindungen als Sauerstoffäquivalent 	| mg O2 / l 	|
| 46 | \anchor vbsb vbsb	| C-BSB5 incl. lebender Organismen, messbar, Randwert (BSB5 ohne Sauerstoffbedarf Nitrifikation)| mg O2 / l	|
| 47 | \anchor vcsb vcsb	| C-CSB, messbar, Randwert			| mg O2 / l	|
| 48 | \anchor chnf chnf	| C-Masse der heterotrophen Nanoflagelaten 	| mg C / l |
| 49 | \anchor bvhnf bvhnf	| Biovolumen der HNF ??						| µm3 ??   |
| 50 | \anchor zooind zooind | Anzahl der Rotatorien					| Ind/l	   |
| 51 | \anchor abrzo abrzo1	| leer ?							    	|		   |
| 52 | \anchor ssalg ssalg	| GESAMTSCHWEBSTOFFE ss+Algen+Rotatorien	| mg/l	   |
| 53 | \anchor ss ss		| mineralischer Schwebstoffgehalt	 		| mg/l	   |
| 54 | \anchor fssgr fssgr	| zu schweb()  								|		   |
| 55 | \anchor fbsgr fbsgr	| Faktor zur Berechnung der ablagerungsfreien Grenzkonzentration von zehrungsfähigem, organischem Material ???? siehe: \ref lnk_Sediorgc | - |
| 56 | \anchor frfgr frfgr	| Faktor zur Berechnung der ablagerungsfreien Grenzkonzentration von refraktärem, organischem Material ???? siehe: \ref lnk_Sediorgc | - |
| 57 | \anchor nl0 nl0 | N/C Verhältnis von Stickstoff zu Kohlenstoff in organischem Material | mg N / mg C	|
| 58 | \anchor pl0 pl0 | P/C Verhältnis von Phosphor zu Kohlenstoff in organischem Material	| mg P / mg C	|
| 59 | \anchor stind stind	| ph() ???? Minutenzähler; Versuch einer Altersvariablen ? |		|
| 60 | \anchor dlarvn dlarvn | Anzahl der Dreissena-Larven		| Ind/l 	|
| 61 | \anchor coli coli	| Fäkalcoliforme Bakterien			| Ind/100 ml |
| 62 | \anchor mw mw		| Säurekapazität (m-Wert) 			| mmol/l	|
| 63 | \anchor pw pw		| Basenkapazität (p-wert)			| mmol/l	|
| 64 | \anchor ca ca		| Calciumkonzentration				| mg Ca /l	|
| 65 | \anchor lf lf		| Leitfähigkeit						| µS/cm		|
| 66 | \anchor vph vph 	    | pH-Wert							|	-		|
| 67 | \anchor gesn gesn	| Gesamt-Stickstoff					| mg N / l	|
| 68 | \anchor gesp gesp	| Gesamt-Phosphor  				   | mg PO4-P / l |
| 69 | \anchor skmor skmor	| Silizium in schwebenden, abgestorbenen Kieselalgen |		|
| 70 | \anchor doscf doscf	| zu coliform() ?				 	|		|
| 71 | \anchor tracer tracer | passiver Tracer (nur QSim3D)		| -		|
| 72 | \anchor salz salz   	| Salzgehalt (nur QSim3D)			| Psu   |
| 73 | \anchor alter_decay alter_decay	| Altersvariable Zerfall  (nur QSim3D)	| d		|
| 74 | \anchor alter_arith alter_arith	| Altersvariable arithmetisches Mittel  (nur QSim3D) | d	|
| 75 | \anchor alter_growth alter_growth | Altersvariable Wachstum  (nur QSim3D)			| d		|
| 76 | \anchor tgzoo tgzoo	| Gewicht einer einzelnen Rotatorie (Zooplankton) (noch nicht aktiv) | µg |
| 77 | \anchor akmor_1 akmor_1	| max.?? Algenmortalität??	Kiesel | ?		|
| 78 | \anchor agmor_1 agmor_1	| 	Grün | ?		|
| 79 | \anchor abmor_1 abmor_1	| 	Blau | ?		|

<b> \anchor Biomasse Biomasse </b> \n
Der Begriff "Biomasse" wird möglicherweise nur in Bezug auf QSim in dieser Form 
verwendet. Eine Begriffsklärung ist in Arbeit (April2021).
Er wird vorläufig beibehalten, weil sich andere Größen (Nährstoffgehalte, 
Schwebstoffmassen) darauf beziehen.

<i>Transport eigentlich unnötig: fbsgr, frfgr</i>


# Tiefenaufgelöste Konzentrationen {#tiefenaufgelöste_planktische_variable}

Die QSim-3D Nummer bezieht sich auf die Datenfelder modell::plankt_vari_vert und 
modell::plankt_vari_vert_p

Die QSim-1D Namen werden in QSim3D im module_QSimDatenfelder.f95 vereinbart.\n 

| Nr.| Name			           | Beschreibung 			    | Dimension	   |
|----|-------------------------|----------------------------|--------------|
|  1 | \anchor tempwz tempwz   | Wasser-Temperatur		    | Grad Celsius |
|  2 | \anchor vo2z vo2z	   | Sauerstoff 			    | mg O2 / l	   |
|  3 | \anchor vnh4z vnh4z	   | Ammonium-Stickstoff 		| mg NH4-N / l |
|  4 | \anchor vno2z vno2z	   | Nitrit-Stickstoff		    | mg NO2-N / l |
|  5 | \anchor vno3z vno3z	   | Nitrat-Stickstoff		    | mg NO3-N / l |
|  6 | \anchor gelpz gelpz	   | gelöster Phosphor		    | mg PO4-P / l |
|  7 | \anchor siz siz		   | Silikat-Silizium		    | mg Si / l	   |
|  8 | \anchor akiz akiz	   | Biomasse Kiesel-Algen	    | mgBio/l	   |
|  9 | \anchor agrz agrz	   | Biomasse Gruen-Algen		| mgBio/l	   |
| 10 | \anchor ablz ablz	   | Biomasse Blau-Algen		| mgBio/l	   |
| 11 | \anchor chlaz chlaz	   | Chlorophyll-a			    | µgChla/l	   |
| 12 | \anchor hchlkz hchlkz   | Chlorophyll in Kieselalgen	| µgChla/l     |
| 13 | \anchor hchlgz hchlgz   | Chlorophyll in Gruenalgen	| µgChla/l     |
| 14 | \anchor hchlbz hchlbz   | Chlorophyll in Blaualgen	| µgChla/l 	   |
| 15 | \anchor hgespz hgespz   | gesamt Phosphor 	        |              |
| 16 | \anchor hgesnz hgesnz   | gesamt Stickstoff	        |              |
| 17 | \anchor hq_nkz hq_nkz   | Stickstoffanteil der Algenbiomasse kiesel  | mg N /mg bio	|
| 18 | \anchor hq_ngz hq_ngz   | Stickstoffanteil der Algenbiomasse grün 	| mg N /mg bio	|
| 19 | \anchor hq_nbz hq_nbz   | Stickstoffanteil der Algenbiomasse blau 	| mg N /mg bio	|
| 20 | \anchor hcchlkz hcchlkz | c-chla Verhältnis Kiesel entspricht \ref akbcm | mg C/ mg Chla	|	 
| 21 | \anchor hcchlgz hcchlgz | c-chla Verhältnis grün entspricht \ref agbcm	| mg C/ mg Chla	|
| 22 | \anchor hcchlbz hcchlbz | c-chla Verhältnis blau entspricht \ref abbcm	| mg C/ mg Chla	|

<code>\verbatim
      subroutine transportz(anze,deltat,izeits,isub_dt,hvmitt,elen,flag &
     &,\ref tempwz,\ref vnh4z,\ref vno2z,\ref vno3z,\ref vo2z,\ref gelpz,
	 \ref siz,\ref akiz,\ref agrz                &
     &,ablz,chlaz,nkzs,dh2d,i2ds,iwsim,mstr,r1,r2,r3                    &
     &,htempz,ho2z,hnh4z,hno2z,hno3z,hgelpz,hsiz,hakiz,hagrz            &
     &,hablz,hchlaz,iflri,dl,imac,uvert,tflie,jpoin1,itags,monats       &
     &,iwied,uhrz,iverfahren)                                            
            
      call advdiff(anze,elen,vmitt,uvert,dl,flag,ktrans,u,temp0,tempn   &
     &,deltat,sumdet,itime,izeits,mstr,iwied,iwahld,nkz,nkzs,tflie,iflri&
     &,jpoin1,itags,monats,isub_dt,imac,iverfahren,uhrz)                             
\endverbatim</code>
\n\n

Ein Teil der planktische Bilanzvariablen (\subpage lnk_partik_planktik) liegen 
partikulär vor, werden aber als im Wasser gleichverteilte Konzentrationen 
gerechent.

aus var_planktisch.md; zurück zu \ref lnk_datenstruktur oder 
\ref lnk_stofftransport_3d
