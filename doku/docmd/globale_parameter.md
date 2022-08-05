Globale Modellparameter  {#lnk_globale_parameter}
=======================

Dies sind empirische Parameter, die als allgemeingültig für \n

- das gesamte Modellgebiet und
- den gesamten Berechnungs-Zeitraum

angesehen werden.

# Extinktionskoeffizienten
Die \ref lnk_extnct_rb sind in der Gerris-Benutzeroberfläche nicht bearbeitbar.

Die Datei <a href="./exp/e_extnct.dat" target="_blank">e_extnct.dat</a> ist Teil 
der QSim-Installation.

# Biologische Parameter
In der Gerris-Benutzeroberfläche werden sie unter der Schaltfläche 
"QSim-Parameter" geführt.

Diese globalen Biologischen Parameter werden von QSim aus der Datei APARAM.txt 
gelesen. Es handelt sich dabei um folgende:

| Position in APARAM.txt | QSim-Name | Beschreibung | Einheit | Zitat aus AParamParam.xml | 
| ---------------------- | --------- | ------------ | ------- | ------------------------- | 
| Zeile 1 : | agchl,aggmax,IKge,agksn,agksp |       |         |                           |
| | \anchor agchl agchl   | Kohlenstoff/Chlorophyll Grünalgen (dunkeladaptiert) bei 20°C |  mgC/mgChla |  |
| | \anchor aggmax aggmax | Max. Wachstumsrate d. Grünalgen | 1/d | Format= F5.2  Null= -1  Help= maximale Produktionsrate für Grünalgen | 
| | \anchor ikge ikge     | Lichtsättigung für Photosynthese der Grünalgen bei 20°C      |  µE/(m2*s) | Format= F6.2  Null= -1 | 
| | \anchor agksn agksn   |  Halbsättigungskonstante Grünalgen N |  mg/l | Format= F5.3  Null= -1  Help= Halbsättigungskonstante für N bei Grünalgen  | 
|  |  \anchor agksp agksp   |  Halbsättigungskonstante Grünalgen P |  mg/l |    Format= F6.4  Null= -1  Help= Halbsättigungskonstante für P bei Grünalgen  | 
| Zeile 2 :| agremi,frmuge,bsbgr,csbgr,Qmx_NG |  | | |
| |  \anchor agremi agremi   |  Grundrespiration d. Grünalgen |  1/d |    Format= F5.3  Null= -1  | 
|  |  \anchor frmuge frmuge   |  Anteil der vom Wachstum abhängigigen Respiration (Grünalgen) |   - |    Format= F5.2  Null= -1   | 
|  |  \anchor bsbgr bsbgr   |  C-BSB5-Erhöhung Grünalgen |  mg/mgC |    Format= F6.4  Null= -1  Help= C-BSB5-Erhöhung durch Grünalgen   | 
| |  \anchor csbgr csbgr   |  CSB-Erhöhung Grünalgen |  mg/mgC |    Format= F6.4  Null= -1  Help= CSB-Erhöhung durch Grünalgen  | 
| |  \anchor qmx_ng qmx_ng   |  max. N-Gehalt der Grünalgenzelle |  mg/mgBio |    Format= F7.5  Null= -1  Help= Stickstoffgehalt der Grünalgenbiomasse  | 
| Zeile 3 :| Qmx_PG,Qmn_NG,Qmn_PG,upmxNG,upmxPG | | | |
| |  \anchor qmx_pg qmx_pg   |  max. P-Gehalt der Grünalgenzelle |  mg/mgBio |    Format= F7.5  Null= -1  Help= Phosphorgehalt der Grünalgenbiomasse  | 
| |  \anchor qmn_ng qmn_ng   |  min. N-Gehalt der Grünalgenzelle |  mg/mgBio |    Format= F7.5  Null= -1   | 
| |  \anchor qmn_pg qmn_pg   |  min. P-Gehalt der Grünalgenzelle |  mg/mgBio |    Format= F7.5  Null= -1   | 
| |  \anchor upmxng upmxng   |  max. N-Aufnahmerate der Grünalgen |  1/d |    Format= F5.3  Null= -1    | 
| |  \anchor upmxpg upmxpg   |  max. P-Aufnahmerate der Grünalgen |  1/d |    Format= F5.3  Null= -1   | 
| Zeile 4 :| opgrmi,opgrma,asgre,ToptG,TmaxG| | | | 
| |  \anchor opgrmi opgrmi   |  Min. O2-Prod. Grünalgen |  mg/mgBio |    Format= F4.2  Null= -1  Help= minimale Sauerstoffproduktion durch Grünalgen   | 
| |  \anchor opgrma opgrma   |  Max. O2-Prod. Grünalgen |  mg/mgBio |    Format= F4.2  Null= -1  Help= maximale Sauerstoffproduktion durch Grünalgen   | 
| |  \anchor asgre asgre   |  Sediment Grünalgen |  0-1 |    Format= F5.2  Null= -1  Help= Sedimentierbarer Anteil für Grünalgen   | 
| |  \anchor toptg toptg   |  optimal Temperatur für Grünalgenwachstum |  °C |    Format= F5.2  Null= -1  Help= optimal Temperatur für Grünalgenwachstum   | 
| |  \anchor tmaxg tmaxg   |  Letal-Temperatur für Grünalgenwachstum |  °C |    Format= F5.2  Null= -1  Help= Letal-Temperatur für Grünalgenwachstum   | 
| Zeile 5 :| akchl,akgmax,IKke,akksn,akksp | | | | 
| |  \anchor akchl akchl   |  Kohlenstoff/Chlorophyll Kieselalgen (dunkeladaptiert) bei 20°C |  mgC/mgChla |     | 
| |  \anchor akgmax akgmax   |  Max. Wachstumsate d. Kieselalgen  |  1/d |    Format= F5.2  Null= -1  Help= maximale Wachstumsrate für Kieselalgen   | 
| |  \anchor ikke IKke   |  Lichtsättigung für Photosynthese der Kieselalgen bei 20°C |  µE/(m2*s) |    Format= F6.2  Null= -1  | 
| |  \anchor akksn akksn   |  N-Halbsättigungskonstante Kieselalgen |  mg/l |    Format= F5.3  Null= -1  Help= Halbsättigungskonstante für N bei Kieselalgen  | 
| |  \anchor akksp akksp   |  P-Halbsättigungskonstante Kieselalgen |  mg/l |    Format= F6.4  Null= -1  Help= Halbsättigungskonstante für P bei Kieselalgen  | 
| Zeile 6 :| akkssi,akremi,frmuke,bsbki,csbki | | | | 
| |  \anchor akkssi akkssi   |  Si-Halbsättigungskonstante Kieselalgen |  mg/l |    Format= F5.3  Null= -1  Help= Halbsättigungskonstante für SI bei Kieselalgen  | 
|  |  \anchor akremi akremi   |  Grundrespiration d. Kieselalgen |  1/d |    Format= F5.3  Null= -1  Help= minimale Respirationsrate für Kieselalgen  | 
| |  \anchor frmuke frmuke   |  Anteil der vom Wachstum abhängigigen Respiration (Kieselalgen) |   - |    Format= F5.2  Null= -1  Help=   Default=  | 
| |  \anchor bsbki bsbki   |  C-BSB5-Erhöhung Kieselalgen |  mg/mgC |    Format= F6.4  Null= -1  Help= C-BSB5-Erhöhung durch Kieselalgen   | 
| |  \anchor csbki csbki   |  CSB-Erhöhung Kieselalgen |  mg/mgC |    Format= F6.4  Null= -1  Help= CSB-Erhöhung durch Kieselalgen   | 
| Zeile 7 :| Qmx_NK,Qmx_PK,Qmx_SK,Qmn_NK,Qmn_PK | | | | 
| |  \anchor qmx_nk qmx_nk   |  max. N-Gehalt der Kieselalgenzelle |  mgN/mgBio |    Format= F7.5  Null= -1  Help= Stickstoffgehalt der Kieselalgenbiomasse   | 
| |  \anchor qmx_pk qmx_pk   |  max. P-Gehalt der Kieselalgenzelle |  mgP/mgBio |    Format= F7.5  Null= -1  Help= Phosphorgehalt der Kieselalgenbiomasse  | 
| |  \anchor qmx_sk qmx_sk   |  max. Si-Gehalt der Kieselalgenzelle |  mgSi/mgBio |    Format= F7.5  Null= -1  Help= Silikatgehalt der Kieselalgenbiomasse  | 
| |  \anchor qmn_nk qmn_nk   |  min. N-Gehalt der Kieselalgenzelle |  mgN/mgBio |    Format= F7.5  Null= -1   | 
| |  \anchor qmn_pk qmn_pk   |  min. P-Gehalt der Kieselalgenzelle |  mgP/mgBio |    Format= F7.5  Null= -1   | 
| Zeile 8 :| Qmn_SK,upmxNK,upmxPK,upmxSK,opkimi | | | | 
| |  \anchor qmn_sk qmn_sk   |  min. Si-Gehalt der Kieselalgenzelle |  mgSi/mgBio |    Format= F7.5  Null= -1  | 
| |  \anchor upmxnk upmxnk   |  max. N-Aufnahmerate der Kieselalgen |  1/d |    Format= F5.3  Null= -1   | 
| |  \anchor upmxpk upmxpk   |  max. P-Aufnahmerate der Kieselalgen |  1/d |    Format= F5.3  Null= -1   | 
| |  \anchor upmxsk upmxsk   |  max. Si-Aufnahmerate der Kieselalgen |  1/d |    Format= F5.3  Null= -1  | 
|  |  \anchor opkimi opkimi   |  Min. O2-Prod. Kieselalgen |  mgO2/mgBio |    Format= F4.2  Null= -1  Help= minimale Sauerstoffproduktion durch Kieselalgen  | 
| Zeile 9 :|  opkima, askie, ToptK, kTemp_Ki, abchl | | | | 
|  |  \anchor opkima opkima   |  Max. O2-Prod. Kieselalgen |  mg/mgBio |    Format= F4.2  Null= -1  Help= maximale Sauerstoffproduktion durch Kieselalgen  | 
| |  \anchor askie askie   |  Sediment Kieselalgen |  0-1 |    Format= F5.2  Null= -1  Help= Sedimentierbarer Anteil für Kieselalgen   | 
| |  \anchor toptk toptk   |  optimal Temperatur für Kieselalgenwachstum |  °C |  Cyclotella meneghiniana: 27.9°C  Default= 20  Min= 0  Max= 99.99 | 
| |  \anchor ktemp_ki ktemp_ki |  empirische Konstante KT(µ) für Temperaturabhängigkeit (Exponent) |  1/°C |  Format= F7.5  Null= -1 Help= Cyclotella meneghiniana: 0.003  Default= 0.0056  Min= 0  Max= 9.99999  Gruppe= Kieselalgen  Kategorie= Temperatur  | 
| |  \anchor abchl abchl   |  "Kohlenstoff/Chlorophyll Blaualgen |  mgC/mgChla |    | 
| Zeile 10 :| abgmax,IKbe,abksn,abksp,abremi | | | | 
| |  \anchor abgmax abgmax   |  Max. Wachstumsrate d. Blaualgen  |  1/d |    Format= F5.2  Null= -1  Help= maximale Wachstumsrate für Blaualgen| 
| |  \anchor ikbe ikbe   |  Lichtsättigung für Photosynthese der Blaualgen bei 20°C |  µE/m2*s) |    Format= F6.2  Null= -1 | 
| |  \anchor abksn abksn   |  N-Halbsättigung Blaualgen |  mg/l |    Format= F5.3  Null= -1  Help= Halbsättigungskonstante für N bei Blaualgen  | 
| |  \anchor abksp abksp   |  P-Halbsättigung Blaualgen |  mg/l |    Format= F6.4  Null= -1  Help= Halbsättigungskonstante für P bei Blaualgen  | 
| |  \anchor abremi abremi   |  Grundrespiration d. Blaualgen |  1/d |    Format= F5.3  Null= -1  Help= minimale Respirationsrate für Blaualgen  | 
| Zeile 11 :| frmube,bsbbl,csbbl,Qmx_NB,Qmx_PB | | | | 
| |  \anchor frmube frmube   |  Anteil der vom Wachstum abhängigigen Respiration (Blaulalgen) |   - |    Format= F5.2  Null= -1  | 
| |  \anchor bsbbl bsbbl   |  C-BSB5-Erhöhung Blaualgen |  mg/mgC |    Format= F6.4  Null= -1  Help= C-BSB5-Erhöhung durch Blaualgen  | 
| |  \anchor csbbl csbbl   |  CSB-Erhöhung Blaualgen |  mg/mgCS |    Format= F6.4  Null= -1  Help= CSB-Erhöhung durch Blaualgen  | 
| |  \anchor qmx_nb qmx_nb   |  max. N-Gehalt der Blaualgenzelle |  mg/mgBio |    Format= F7.5  Null= -1  Help= Stickstoffgehalt der Blaualgenbiomasse  | 
| |  \anchor qmx_pb qmx_pb   |  max. P-Gehalt der Blaualgenzelle |  mgP/mgBio |    Format= F7.5  Null= -1  Help= Phosphorgehalt der Blaualgenbiomasse  | 
| Zeile 12 :| Qmn_NB,Qmn_PB,upmxNB,upmxPB,opblmi | | | | 
| |  \anchor qmn_nb qmn_nb   |  min. N-Gehalt der Blaualgenzelle |  mgN/mgBio |    Format= F7.5  Null= -1  | 
| |  \anchor qmn_pb qmn_pb   |  min. P-Gehalt der Blaualgenzelle |  mgP/mgBio |    Format= F7.5  Null= -1  | 
| |  \anchor upmxnb upmxnb   |  max. N-Aufnahmerate der Blaualgen |  1/d |    Format= F5.3  Null= -1  | 
| |  \anchor upmxpb upmxpb   |  max. P-Aufnahmerate der Blaualgen |  1/d |    Format= F5.3  Null= -1  | 
| |  \anchor opblmi opblmi   |  Min. O2-Prod. Blaualgen |  mg/mgBio |    Format= F4.2  Null= -1  Help= minimale Sauerstoffproduktion durch Blaualgen  | 
| Zeile 13 :| opblma,asble,ToptB,TmaxB,ifix | | | | 
| |  \anchor opblma opblma   |  Max. O2-Prod. Blaualgen |  mg/mgBio |    Format= F4.2  Null= -1  Help= maximale Sauerstoffproduktion durch Blaualgen  | 
| |  \anchor asble asble   |  Sediment Blaualgen |  0-1 |    Format= F5.2  Null= -1  Help= Sedimentierbarer Anteil für Blaualgen  | 
| |  \anchor toptb toptb   |  optimal Temperatur für Blaualgenwachstum |  °C |    Format= F5.2  Null= -1  Help= optimal Temperatur für Blaualgenwachstum  | 
| |  \anchor tmaxb tmaxb   |  Letal-Temperatur für Blaualgenwachstum |  °C |    Format= F5.2  Null= -1  Help= Letal-Temperatur für Blaualgenwachstum  | 
| |  \anchor ifix ifix   |  Luftstickstofffixierer (0/1) |   |    Format= I2  Null= -1  Help= Luftstickstofffixierer(0:Nein/1:Ja)  | 
| Zeile 14 :| irmaxe,FopIRe,GROT,zresge,zakie | | | | 
| |  \anchor irmaxe irmaxe   |  max. Gewichtsspez. Algenaufnahmerate d. Rotatorien |  µgC*µgC-2/3*d-1 ??? |    Format= F5.2  Null= -1  Help= Max. Ingestionsrate für Rotatorien  | 
| |  \anchor fopire fopire   |  Halbsättigungskonstante für Futteraufnahme d. Rotatorien |  mg/l |    Format= F5.2  Null= -1  Help= Optimale Futterkonzentration für Rotatorienwachstum  | 
| |  \anchor GROT GROT   |  durchschnittliches Gewicht einer Rotatorie |  µg |    Format= F5.2  Null= -1  Help= Gewicht einer Rotatorie  | 
| |  \anchor zresge zresge   |  Grundrespiration Rotatorien |  1/d |    Format= F5.3  Null= -1  Help= Grundrespiration der Rotatorien  | 
| |  \anchor zakie zakie   |  Filtrierbarkeit Kieselalgen |  0-1 |    Format= F5.2  Null= -1  Help= Filtrierbarkeit der Kieselalgen durch Rotatorien  | 
| Zeile 15 :| zagre,zable,ynmx1e,stks1e,anitrie | | | | 
| |  \anchor zagre zagre   |  Filtrierbarkeit Grünalgen |  0-1 |    Format= F5.2  Null= -1  Help= Filtrierbarkeit der Grünalgen durch Rotatorien  | 
| |  \anchor zable zable   |  Filtrierbarkeit Blaualgen |  0-1 |    Format= F5.2  Null= -1  Help= Filtrierbarkeit der Blaualgen durch Rotatorien  | 
| |  \anchor ynmx1e ynmx1e (YNMAX1)   |  Max. Wachstum Nitrosomonas |  1/d |    Format= F4.2  Null= -1  Help= Max. Wachstumsrate der Nitrosomonas  | 
| |  \anchor stks1e stks1e   |  Halbsättigung Nitrosomonas |  mgNH4-N/l |    Format= F5.2  Null= -1  Help= Halbsättigungskonstante für Nitrosomonas  | 
| |  \anchor anitrie anitrie   |  Absterberate Nitrosomonas |  1/d |    Format= F4.2  Null= -1  Help= Absterberate für Nitrosomonas  | 
| Zeile 16 :| bnmx1e,bnks1e,ynmx2e,stks2e,anitri2e | | | | 
| |  \anchor bnmx1e bnmx1e (BNMX1)   |  Max. Umsatz Nitrosomonas |  gNH4-N/(m²*l) |    Format= F5.2  Null= -1  Help= Max. Umsatzrate sessiler Nitrosomonas  | 
| |  \anchor bnks1e bnks1e   |  Halbsätt. sessiler Nitrosomonas |  mg/l |    Format= F5.2  Null= -1  Help= Halbsättigungskonstante der sessilen Nitrosomonas  | 
| |  \anchor ynmx2e ynmx2e (YNMAX2)   |  Max. Wachstum Nitrobacter |  1/d |    Format= F4.2  Null= -1  Help= Max. Wachstumsrate der Nitrobacter  | 
| |  \anchor stks2e stks2e   |  Halbsättigung Nitrobacter |  mgNO2-N/l |    Format= F5.2  Null= -1  Help= Halbsättigungskonstante für Nitrobacter  | 
| |  \anchor anitri2e anitri2e|  Absterberate Nitrobacter |  1/d |    Format= F4.2  Null= -1  Help= Absterberate für Nitrobacter  | 
| Zeile 17 :| bnmx2e,bnks2e,KNH4e,KapN3e,hyPe | | | | 
| |  \anchor bnmx2e bnmx2e (BNMX2)   |  Max. Umsatz Nitrobacter |  gNO2-N/(m2*l) |    Format= F5.2  Null= -1  Help= Max. Umsatzrate sessiler Nitrobacter  | 
| |  \anchor bnks2e bnks2e   |  Halbsätt. sessiler Nitrobacter |  mg/l |    Format= F5.2  Null= -1  Help= Halbsättigungskonstante der sessilen Nitrobacter  | 
| |  \anchor knh4e knh4e   |  NH4-Umsatzgeschw. im Sediment |  m/d |    Format= F5.2  Null= -1  Help= Saar: 0.28;  Havel: 0.19   | 
| |  \anchor kapn3e kapn3e   |  Denitrifikationsgeschw. im Sediment |  m/d |    Format= F5.2  Null= -1  Help= Saar: 0.06;  Havel: 0.15  | 
| |  \anchor hype hype   |  Hydrolyserate für die leicht abbaubaren partikulären organischen C-Verbindungen  |  1/d |    Format= F6.3  | 
| Zeile 18 :| hymxDe,KsD1e,KsD2e,KsMe,upBACe | | | | 
| |  \anchor hymxde hymxde   |  maximale Hydrolyserate für die leicht abbaubaren gelösten organischen C-Verbindungen  |  1/d |    Format= F6.3  Null= -1  | 
| |  \anchor ksd1e ksd1e   |  Halbsättigungskonstante für die Hydrolyse der leicht abbaubaren gelöster organischer C-Verbindungen |  mgC/l |    Format= F6.3  Null= -1  H| 
| |  \anchor ksd2e ksd2e   |  Halbsättigungskonstante für die Hydrolyse der schwer abbaubaren gelöster organischer C-Verbindungen |  mgC/l |    Format= F6.3  Null= -1  | 
| |  \anchor ksme ksme   |  Halbsättigungskonst. für den Abbau monomerer C-Verbindungen (?? für die Aufnahme von Kohlenstoff durch heterotrophen Bakterien??) |  mgC/l | Format= F6.3  Null= -1  | 
| |  \anchor upbace upbace   |  max. Aufnahmerate monomerer C-Verbindungen d. Bakterien |  1/d |    Format= F6.3  Null= -1  | 
| Zeile 19 :| YBACe,rsGBACe,FoptDe,upHNFe,BACkse | | | | 
| |  \anchor ybace ybace   |  Ertragskoeffizient für Bakterienbiomasse |   - |    Format= F6.3  Null= -1  | 
| |  \anchor rsgbace rsgbace   |  Grundrespiration het. Bakterien |  1/d |    Format= F6.3  Null= -1  | 
| |  \anchor foptde foptde   |  Opt. Futterkonz. Dreissena |  mgC/l |    Format= F5.2  Null= -1  Help= Optimale Futterkonzentration für Dreissena-Wachstum  | 
| |  \anchor uphnfe uphnfe   |  max. Aufnahmerate der HNF |  1/d |    Format= F5.2  Null= -1  Help= Aufnahmerate heterotropher Nanoflagelaten  | 
| |  \anchor backse backse   |  Halbsättigungsk. für BaK.-Aufnahme durch HNF |  mgC/l |    Format= F6.4  Null= -1  | 
| Zeile 20 :| alamda,fPOC1e,fPOC2e,SorpCape,Klange | | | | 
| |  \anchor alamda alamda   |  Absorptionskoeff. für Gelbstoffe bei 440 nm |  - |    Format= F5.3  Null= -1   | 
| |  \anchor fpoc1e fpoc1e   |  leichtabbaubarer Anteil d. Sedimentkohlenstoffs |   -  |    Format= F5.2  Null=   Help= (Literaturwert: 0.65  | 
| |  \anchor fpoc2e fpoc2e   |  schwerabbaubarer Anteil d. Sedimentkohlenstoffs |   -  |    Format= F5.2  Null=   Help= (Literaturwert: 0.15  | 
| |  \anchor sorpcape sorpcape|  SorptionsKapazität für Phosphor |  mgP/gTG |    Format= F6.2  Null= -1  Help= (Literaturwerte: Maxmalwert: 2.5; Eingabe: -1 -> Wert wird berechnet  | 
| |  \anchor klange klange   |  Langmuirkoeffizient für Phosphorsorption |  l/mgP |    Format= F6.3  Null= -1  | 
| Zeile 21 :| KdNh3e,RateCde,etaCde,RateCIe,xnueCe | | | | 
| |  \anchor kdnh3e kdnh3e   |  Partitionskoeffizient für Ammonium |  l/kg |    Format= F5.2  Null= -1  Help= -1.-> Wert wird berechnet  | 
| |  \anchor ratecde   ratecde   | Grundmortalitätsrate coliformer Bakterien bei 20°C |  1/d |   Help="Grundmortalitätsrate coliformer Bakterien bei 20°C" Min="0.0" Max="10." Gruppe="Hygiene" Kategorie="Coliform"   | 
| |  \anchor etacde   etacde   | Temperaturkoeffizient |  - |   Help="Temperaturkoeffizient" Min="1." Max="3." Gruppe="Hygiene" Kategorie="Coliform"   | 
| |  \anchor ratecie   ratecie   | Inaktivierungskoeffizient im Licht |  m2*MJ-1 |   Help="Inaktivierungskoeffizient im Licht" Min="0.0" Max="99.99" Gruppe="Hygiene" Kategorie="Coliform"   | 
| |  \anchor xnuece   xnuece   | dimensionsloser Parameter |  - |   Help="dimensionsloser Parameter zur Beschreibung der Inaktiv. im Licht" Min="1.0" Max="999.99" Gruppe="Hygiene" Kategorie="Coliform"   | 
| Zeile 22 :| RateCGe,RateCSe | | | | 
| |  \anchor ratecge   ratecge   | Verlustrate durch Grazing |  d-1 |  Help="Coliforme Verlustrate durch Grazing" Min="0.0" Max="9.999" Gruppe="Hygiene" Kategorie="Coliform"   | 
| |  \anchor ratecse   ratecse   | Verlustrate durch Sedimentation |  d-1 |   Help="Coliforme Verlustrate durch Sedimentation" Min="0" Max="9.999" Gruppe="Hygiene" Kategorie="Coliform"   | 


Beispiel:

<a href="./exp/APARAM_200314.txt" target="_blank">APARAM.txt, Version vom 20.03.2014</a>

<a href="./exp/AParam_kommentiert.txt" target="_blank">APARAM.txt (kommentiert) v12.40</a>

<a href="./exp/aparam_gerris.xls" target="_blank">Parameterliste Volker 19dez12</a>

<a href="./exp/aparam_gerris_13.10.xls" target="_blank">überarbeitete Prameterliste 20dez12 wy</a>

die aktuelle <a href="./exp/AParamParam.xml" target="_blank"> AParamParam.xml </a> 
enthält die Parameterdefinitionen im xml-Format. Diese Datei dient der 
Synchronisation mit der Benutzeroberfläche Gerris \ref lnk_gerris .


<a href="./pdf/Schnittstelle_QSIM.pdf" target="_blank">Schnittstellenbeschreibung Gerris-QSim</a>

Textquelle: globale_parameter.md ; Codesources: uebergabe_werte.f95 ;  
zurück: \ref lnk_modellerstellung
 