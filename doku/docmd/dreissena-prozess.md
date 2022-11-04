Dreissena - Prozesse {#lnk_dreissena_prozesse}
===================== 
Die Entwicklung von Dreissena polymorpha wird von folgenden Faktoren 
beeinflusst:

* Futterkonzentration
* ...
\n\n

# Futterkonzentration

## Filtrierbare Futterkonzentration
Als für Dreissena filtrierbar wird ein Anteil der Schwebstoffe 
\f$F_{SS}\f$ (ssc) und Algen (gruppenspezifisch) angenommen.

\f{equation}{ F_{SS} = SS \cdot 0,1 \cdot 0,4 = SS \cdot 0,04 \f}

\f$ F_{SS} \f$:   Filtrierbarer Anteil der Schwebstoffe [...] \n

Mit dem Faktor 0,1 wird der Anteil organischer Schwebstoffe berechnet und mit
dem Faktor 0,4 der für Dreissena verwertbare Anteil davon.

Die verwertbaren Algen der Gruppen der Grün-, Kiesel- und Blaualgen 
F_{Grün}, F_{Kiesel}, F_{Blau} (Fgr, Fki, Fbl) werden unter Verwendung des 
Algengruppenanteiles A (agr, aki, abl), deren gruppenspezifischen
C:Biomasse-Verhältnissen und ~ Präferenzfaktoren Prä (pgr, pki, pbl) berechnet:

\f{equation}{F_{Grün} = A_{Grün} \cdot C_{Grün} \cdot Prä_{Grün} ⁡= 
    A_{Grün} \cdot 0,48 \cdot 1 \f}
\f{equation}{F_{Kiesel} = A_{Kiesel} \cdot C_{Kiesel} \cdot Prä_{Kiesel} ⁡= 
    A_{Kiesel} \cdot 0,48 \cdot 1 \f}
\f{equation}{F_{Blau} = A_{Blau} \cdot C_{Blau} \cdot Prä_{Blau}  ⁡= 
    A_{Blau} \cdot 0,096 \f}
	

Die Summe des für Dreissena zur Verfügung stehenden Futters F_{Dreis} ergibt 
sich durch
 
\f{equation}{F_{Dreis} = F_{SS} + F_{Grün} + F_{Kiesel} + F_{Blau} \f}

<!-- Quellcode -->

	! Berechnung der filtrierbaren Futterkonzentration 
	! 
	ssorg = ss(ior)*0.1 
	ssc = ssorg*0.4 
	ssorg = 0.0 
	! 
	agrc = agr(ior)*0.48 
	akic = aki(ior)*0.48 
	ablc = abl(ior)*0.48 

	if((aki(ior)+agr(ior)+abl(ior)).eq.0.0)then 
	hconvk = 0.0 
	hconvg = 0.0 
	hconvb = 0.0 
	goto 568 
	endif 
	hconvk = aki(ior)*pki/(aki(ior)*pki+agr(ior)*pgr+abl(ior)*pbl) 
	hconvg = agr(ior)*pgr/(aki(ior)*pki+agr(ior)*pgr+abl(ior)*pbl) 
	hconvb = abl(ior)*pgr/(aki(ior)*pki+agr(ior)*pgr+abl(ior)*pbl) 
	568 hconvs = 0.0 

\n\n

## Einfluss der Futterkonzentration

Der Faktor \f$F_{Dreis}\f$(Futter) (hconf) ergibt sich aus der zur Verfügung 
stehenden Futterkonzentration \f$F_{Dreis}\f$ im Verhältnis zur optimalen 
Futterkonzentration \f$F_{Dreis,opt}\f$(FoptD).

\f{equation}{F_{Dreis}(Futter) = \frac{F_{Dreis}}{F_{Dreis,opt}} = 
   \frac{F_{Dreis}}{1,2} \f}

Der Futterfaktor kann maximal den Wert 1 annehmen und wird auf null gesetzt 
wenn er einen Wert von weniger als 0,01 annimmt, da dies die limitierende 
Futterkonzentration für Dreissena ist (vorgegebener Wert).

<!-- Quellcode -->

	f_lim = 0.01 ! Kohlenstoffgehalt, bei dem Dreissena die Futteraufnahme einstellt
	 hconf = food/foptd
	 if(food.gt.foptd)hconf = 1. 
	 if(food<=f_lim)hconf = 0.0 

\n\n

## Einfluss von Schwebstoff 

Der Gesamt-Schwebstoffgehalt \f$SS_{ges}\f$ (ssalg) des Wassers beeinflusst die 
Filtrierrate über den Faktor \f$F_{Dreis}(SSges)\f$:

\f{equation}{F_{Dreis}(SS_{ges}) = 3,267 \cdot e^{-0,37 \cdot SS_{ges}} \f}


_(keine Variablenbezeichnung in Code)_


![Faktor $F_{Dreis}(SS_{ges})$ in Abhängigkeit des Schwebstoffgehaltes.](img/dreiss_ss.png)

\n\n
<!-- Quellcode -->

	fr = fr*3.267*exp(-0.037*ssalg(ior))

\n\n

## Fäkal- und Ertragsanteil

Der Fäkalanteil \f$F_{Dreis}(Fäkal)\f$ (qfec) wird bei der Berechnung der 
Assimilationsrate aus der Aufnahmerate gebraucht und ist von dem Futter-Faktor 
\f$F_{Dreis}(Futter)\f$ abhängig. Eins minus dem Fäkalanteil entspricht dem 
Ertragsanteil \f$F_{Dreis}(Ertrag)\f$:

\f{equation}{F_{Dreis}(Fäkal) = \alpha_{Fäkal} \cdot 
     e^{\beta_{Fäkal} \cdot F_{Dreis}(Futter} = 
	 0,35 \cdot e^{0,88 \cdot F_{Dreis}(Futter)} \f}

\f{equation}{F_{Dreis}(Ertrag) = 1 - F_{Dreis}(Fäkal) \f}
 
<!-- Quellcode -->

	qfec = 0.315*exp(0.88*hconf)
	assr = (1.-qfec)*up 
	assrs = (1.-qfec)*ups 

\n\n

## Faktor für Temperaturabhängigkeit

Der Faktor zur Temperaturabhängigkeit wird wiederholt (Filtrierrate, 
Aufnahmerate) entsprechend dieser Formel gebraucht, ohne dass im Code ein 
eigener Variablenname hierfür verwendet wird. Der Übersicht halber wird der 
Faktor \f$F_{Dreis}(T)\f$ hier eingeführt als:
 
\f{equation}{F_{Dreis}(T) = e^{-0,00605 \cdot (20 - T)^2} \f}

![Faktor $F_{Dreis}(T)$ in Abhängigkeit der Wassertemperatur.](img/dreiss_wt.png)
\n\n

## Temperaturabhängigkeit der Wachstums- und Respirationsrate

Die Temperaturabhängigkeit fließt in QSim in einen Beeinflussungsfaktor 
\f$Fmue,Dreis(T)\f$ (hcont) ein, der auf das Wachstum und die Respiration von 
Dreissena wirkt. Bei Optimaltemperatur hat der Faktor den Wert von 1 und wird 
bei Temperaturen unter- oder oberhalb der Optimaltemperatur zunehmend kleiner 
und somit sein Einfluss größer. Er wird nach folgender Formel berechnet:

\f{equation}{F_{mue,Dreis}(T) = \left(\frac{T_{max} ⁡  - T}{T_{max} - T_{opt}} 
   \right) \cdot e^{\left(1- \left(\frac{T_{max} ⁡  - T}{T_{max} - T_{opt}} 
   \right) \right)^x} \f}

wobei:

\f{equation}{x = \left(\frac{w}{20} \cdot \left(1 + sqrt\left(1 + \frac{40}{w}
  \right) \right)\right)^2 \f}

und

w = log(zqz10) * (ztmax - ztopt) 

<!-- Quellcode -->

	w = log(zqz10)*(ztmax-ztopt) 
	x = ((w/20.)*(1.+sqrt(1.+40./w)))**2 
	hcont = (((ztmax-tempw(ior))/(ztmax-ztopt))*exp(1.- &
	&(ztmax-tempw(ior))/(ztmax-ztopt)))**x 

\n\n


## Einfluss des Individuengewichts

Das Individuengewicht von Dreissena \f$Gew_{Dreis}\f$ (gewdr) hat Einfluss auf 
Filtrier- und Aufnahmerate sowie auf die Grundrespirationsrate. Es werden die 
Faktoren \f$F_{Filt,Dreis}(Gew), F_{Aufn,Dreis}(Gew) und F_{Resp,Dreis}(Gew)\f$ 
berechnet:

Filtration
\f{equation}{F_{Filt,Dreis}(Gew) = 9,24 \cdot Gew_{Dreis}^{-0,392} \; \f}  
(keine Variable dafür verwendet)

Aufnahme
\f{equation}{F_{Aufn,Dreis}(Gew) = 0,249 \cdot Gew_{Dreis}^{-0,615} \; \f}   
 
und (idr):
\f{equation}{idr = F_{Aufn,Dreis}(Gew) \cdot F_{Dreis}(T) \f}

Respiration
\f{equation}{F_{Resp,Dreis}(Gew) = Gew_{Dreis}^{-0,25} \; \f}
(keine Variable dafür verwendet)


<!-- Quellcode -->

	fr = 9.24*gewdr(ior,ndr)**(-0.392)
	idr = (0.249*gewdr(ior,ndr)**(-0.615)) &
	&*exp(-0.00605*(20.0-tempw(ior))**2) 
	rres = rres0*gewdr(ior,ndr)**(-0.25)


\n\n

## Einfluss von Chelicorophium auf die Ingestions- und Filtrierrate von Dreissena

Aufgrund der Dichte von Chelicorophium auf Sohle und Böschung 
\f$DichteBö_{Coro}\f$ und \f$DichteSo_{Coro}\f$(Coro, CoroS) wird mit zwei 
Zwischenschritten ein Faktor F_{Dreis} (Coro) (fcom) berechnet:

\f{equation}{F_{Dreis}(CoroBö) = \frac{90000 - (DichteBö_{Coro} - 10000)}{90000} =
  1,111 - \frac{DichteBö_{Coro}}{90000} \;\f}

  aber: min. 0, max. 1

\f{equation}{F_{Dreis}(CoroSo) = \frac{90000 - (DichteSo_{Coro} - 10000)}{90000} =
  1,111 - \frac{DichteSo_{Coro}}{90000} \;\f}
    
  aber: min. 0, max. 1
  
\f{equation}{F_{Dreis}(Coro) = \frac{ABö_{Coro} \cdot F_{Dreis}(CoroBö) + 
      ASo_{Coro} \cdot F_{Dreis}(CoroSo)}{ABö{Coro} + ASo_{Coro}} \;\f}
  
mit
\f{equation}{ABö_{Coro} = DichteBö_{Coro} \cdot 2 \cdot Länge_{Bö} \cdot Länge_{Absch} \f}
 	siehe Chelicorophium (hconc1)
\f{equation}{ASo_{Coro} = DichteSo_{Coro} \cdot Länge_{So} \cdot Länge_{Absch} \f}	
 	siehe Chelicorophium (hconc2)

Die Berechnung des Faktors für Chelicorophium auf Böschung und Sohle 
\f$F_{Dreis}(CoroBö)\f$ (fco) bzw. \f$F_{Dreis}(CoroSo)\f$ (fcos) erfolgt 
getrennt, und anschließend wird ein mittlerer Wert entsprechend des 
Flächen-Anteils und der Dichte auf Sohle und Böschung (hconc1, hconc2) 
berechnet. Die Berechnung des Faktors F_{Dreis}(Coro) kann anschaulich so 
vereinfacht werden:

\f{equation}{F_{Dreis}(Coro) = 1,111 - \frac{DichteGes_{Coro}}{90000} \; \f}
  aber: max. 1, falls < 0,1*: 0


![Abhängigkeit des Faktors F_{Dreiss}(Coro) von mittlerer 
Chelicorophiumdichte auf Sohle und Böschung.](img/fdrei_coroph.png)
\n\n

Zu einer Beeinflussung der Filtrierrate kommt es also erst ab einer 
Chelicorophium-Abundanz von mehr als 10.000 Individuen pro m². So sinkt der 
Faktor beispielsweise bei einer Abundanz von 11.000 Ind./m² auf 0,989 und bei 
einer Abundanz von 15.000 Ind./m² auf 0,944.

<!-- Quellcode -->

	!Einfluss von Corophium auf die Ingest.- und Filtrierrate 
	! 
	if(coroi(ior).eq.0.0.and.corois(ior).eq.0.0)then 
	fco = 1. 
	fcos = 1. 
	fcom = 1. 
	else
	fco = (90000.-(coroi(ior)-10000.))/90000. 
	if(fco.gt.1.)fco = 1. 
	if(fco.lt.0.0)fco = 0.0 
  
	fcos = (90000.-(corois(ior)-10000.))/90000. 
	if(fcos.gt.1.)fcos = 1. 
	if(fcos.lt.0.0)fcos = 0.0 
  
	hconc1 = 2.*lboem(ior)*elen(ior)*coroi(ior) 
	hconc2 = bsohlm(ior)*elen(ior)*corois(ior) 
	fcom = (hconc1*fco+hconc2*fcos)/(hconc1+hconc2) 

\n\n

## Dreissena Biomasse

Aus den Dreissenadichten auf Böschung und Sohle, \f$DichteBö_{Dreis}\f$ und 
\f$DichteSo_{Dreis}\f$ (zdrei bzw. zdreis) werden die im betrachteten Abschnitt 
absolut vorhandene Dreissena-Anzahl \f$ABö_{Dreis}\f$ und \f$ASo_{Dreis}\f$ 
(zdrei=Yc, zdreis=Ycs) berechnet, indem die Böschungs- und Sohllängen 
\f$Länge_{Bö}\f$ und \f$Länge_{So}\f$ (lboem, bsohlm) sowie die Abschnittlänge 
\f$Länge_{Absch}\f$ (elen) verwendet werden.

\f{equation}{ABö_{Dreis} = DichteBö_{Dreis} \cdot 2 \cdot Länge_{Bö} \cdot Länge_{Absch} \f}

\f{equation}{ASo_{Dreis} = DichteSo_{Dreis} \cdot Länge_{So} \cdot Länge_{Absch} \f}


<!-- Quellcode -->

	!.....max. Dreissena-Dichte 
	! drpr = zdreis(ior)/(bsohlm(ior)*elen(ior)) 
	! 
	do 214 ndr=1,nndr 
	zdrei(ior,ndr) = zdrei(ior,ndr)*(2.*lboem(ior)*elen(ior)) 
	zdreis(ior,ndr) = zdreis(ior,ndr)*(bsohlm(ior)*elen(ior)) 
	yc = zdrei(ior,ndr) 
	ycs = zdreis(ior,ndr) 

\n\n

## Änderung der Dreissena Biomasse

Der Zuwachs der Dreissenabiomassen auf Böschung und Sohle 
\f$d_{Dreis} (dYc, dYcs)\f$ ergibt sich aus der Assimilationsrate 
\f$Ass_{Dreis}\f$ (assr, assrs) abzüglich der Respirations- und Exkretionsraten 
\f$Resp_{Dreis}\f$ (respc, respcs) und \f$Exk_{Dreis}\f$ (exdr, exdrs) mal der 
Fließzeit \f$T_{Flie}\f$. Die Dreissenabiomassen nach der Fließzeit entsprechen 
der Summe aus Ausgangsbiomassen ABö_{Dreis} bzw. ASo_{Dreis} (Yc bzw. Ycs) und 
jeweiligem Zuwachs.

\f{equation}{dBö_{Dreis} = (AssBö_{Dreis} - Re⁡spBö_{Dreis} - 
  ExkBö_{Dreis}) \cdot T_{Flie} \f}

\f{equation}{ABö_{Dreis,TFlie} = dBö_{Dreis} + ABö_{Dreis} \f}
\f{equation}{dSo_{Dreis} = (AssSo_{Dreis} - Re⁡spSo_{Dreis} - ExkSo_{Dreis}) 
  \cdot T_{Flie} \f}
\f{equation}{ASo_{Dreis,TFlie} = dSo_{Dreis} + ASo_{Dreis} \f}

\n\n

## Wachstumsrate

Die Wachstumsrate von Dreissena \f$\mu_{Dreis}\f$ (drmue = drmas) berechnet sich 
durch

\f{equation}{\mu_{Dreis} = idr - idr \cdot F_{Dreis}(Fäkal) - idr \cdot 
  F_{Dreis}(Ertrag) \cdot 0,064 - drakr - drbar \f}

mit:

\f{equation}{drakr = (F_{Dreis}(Ertrag)) \cdot Re⁡sp_{akt,Dreis} \cdot idr \f} 

\f{equation}{drbar = Resp_{Grund,Dreis} \cdot F_{mue,Dreis}(T) \f}
 

idr: \f$F_{Dreis}(Gew) \cdot F_{Dreis}(T)\f$, siehe Faktoren der 
Dreissena-Entwicklung

Eine Vereinfachung der Gleichung ergibt:

\f{equation}{\mu_{Dreis} = idr - idr \cdot F_{Dreis} (Fäkal) - idr \cdot 
  F_{Dreis}(Ertrag) \cdot 0,064 - F_{Dreis}(Ertrag) \cdot Re⁡sp_{akt,Dreis} \cdot idr - 
  Re⁡sp_{Grund,Dreis} \cdot F_{mue,Dreis}(T) \cdot \mu_{Dreis} \\
  = idr \cdot (1 - F_{Dreis}(Fäkal)) + F_{Dreis}(Ertrag) \cdot (-idr \cdot 
  0,064 - Re⁡sp_{akt,Dreis} \cdot idr) - Re⁡sp_{Grund,Dreis} \cdot 
  F_{mue,Dreis}(T) \cdot \mu_{Dreis} \\
  = F_{Dreis}(Ertrag) \cdot (idr - idr \cdot 0,064 - Re⁡sp_{akt,Dreis} \cdot idr) - 
  Re⁡sp_{Grund,Dreis} \cdot F_{mue,Dreis}(T) \f}

\f{equation}{\mu_{Dreis} = F_{Dreis}(Ertrag) \cdot (idr \cdot (0,936 - 
  Re⁡sp_{akt,Dreis})) - Re⁡sp_{Grund,Dreis} \cdot F_{mue,Dreis}(T) \f}

\f{equation}{\mu_{Dreis} = F_{Dreis}(Ertrag) \cdot (F_{Dreis}(T) \cdot 
  F_{Dreis}(Gew) \cdot (0,936 - 0,29)) - 0,0015 \cdot F_{mue,Dreis}(T) \f}

\f{equation}{\mu_{Dreis} = F_{Dreis}(Ertrag) \cdot F_{Dreis}(T) \cdot 
  F_{Dreis}(Gew) \cdot 0,637 - 0,0015 \cdot  F_{mue,Dreis}(T) \f}   

\n\n

## Dreissena-Individuen

Um die Dreissena-Individuenzahl auf Böschung plus Sohle \f$Ind_{Dreis,TFlie}\f$ 
(dreisn=dreisg) zu bestimmen, werden die Dreissenabiomassen \f$ABö_{Dreis}\f$ 
bzw. \f$ASo_{Dreis}\f$ (ursprünglich Yc bzw. Ycs, diverse Umbenennungen im Code, 
zuletzt zdrei, zdreis bzw. hconds, hcondb) durch das Individuengewicht 
\f$Gew_{Dreis,TFlie1}\f$ (modifiziert durch Zuwachs und Verlust aufgrund von 
Larvenbildung, s.o.) geteilt. Diese Berechnung erfolgt für jede Kohorte 
(in Gleichungen hier nicht so dargestellt).

\f{equation}{Ind_{Dreis,TFlie} = \frac{(ASo_{Dreis} + ABö_{Dreis}) \cdot 1000}{
  Gew_{Dreis,TFlie1}} \f}  

Falls es festgesetzte Larven gibt, wird für die Kohorte mit Index 1 ein neues 
mittleres Dreissenengewicht \f$Gew_{Dreis,neu}\f$ (gewdts) unter 
Berücksichtigung der festgesetzten Larven (Annahme: 8,6E-5 mgC/Ind) berechnet:
 
\f{equation}{Gew_{Dreis,neu} = \frac{Ind_{Dreis,TFlie} \cdot Gew_{Dreis,TFlie1} + 
  dLarven_{Dreis,festg} \cdot Vol_{Absch} \cdot 1000 \cdot 8,6 \cdot 10^{-5}}{
  Ind_{Dreis,TFlie} + dLarven_{Dreis,festg} \cdot Vol_{Absch} \cdot 1000} \f} 

\f{equation}{Gew_(Dreis,neu) = \frac{(ASo_{Dreis} + ABö_{Dreis}) + 
  dLarven_{Dreis,festg} \cdot Vol_{Absch} \cdot 8,6 \cdot 10^{-5}}{
  \frac{(ASo_{Dreis} + ABö_{Dreis}}{Gew_{Dreis,TFlie1}} + 
  dLarven_{Dreis,festg} \cdot Vol_{Absch}}  \f}

Falls \f$Gew_{Dreis,neu}\f$ größer 0,0246 wird \f$dLarven_{Dreis,festg}\f$ 
neu berechnet

\f{equation}{dLarven_{Dreis,festg} = dLarven_{Dreis,festg} \cdot 
  e^{-0,1 \cdot 20 \cdot T_{Flie}} \\
  = dLarven_{Dreis,festg} \cdot e^{-2 \cdot T_{Flie}} \f}
  
und mit dem neuen \f$dLarven_{Dreis,festg}\f$  Wert wiederum 
\f$Gew_{Dreis,neu}\f$ ermittelt:

\f{equation}{Gew_{Dreis,neu} = \frac{Ind_{Dreis,TFlie} \cdot 
  Gew_{Dreis,TFlie1} + dLarven_{Dreis,festg} \cdot Vol_{Absch} \cdot 1000 \cdot 
  8,6 \cdot 10^{-5}}{Ind_{Dreis,TFlie} + dLarven_{Dreis,festg} \cdot 
  Vol_{Absch} \cdot 1000} \f} 

\note Neue Muscheln werden aufgrund der bestehenden Verhältnisse von 
Populationen auf Böschung und Sohle hierauf verteilt, noch nicht ausformuliert.

Zu der Individuenzahl der Dreissenen wird konsequenter Weise anschließend die 
Individuenzahl der festgesetzten Larven addiert:
 
\f{equation}{Ind_{Dreis,neu} = Ind_{Dreis,TFlie} + dLarven_{Dreis,festg} \cdot 
   Vol_{Absch} \cdot 1000 \f}

Falls das Individuen-Gewicht für die Kohorte mit Index 1 \f$ Gew_{Dreis,Ko1} \f$  
die 1,6 überschreitet, werden mittleres Gewicht und Abundanz der Dreissenen neu 
bestimmt. Dann werden die Dreissenen der Kohorte mit Index 1 zur Kohorte mit 
Index 2 gezählt und für diese ein neues mittleres Dreissena-Gewicht ermittelt. 
Die Biomassen auf Sohle und Böschung sowie das mittlere Gewicht der Kohorte mit 
dem Index 1 hingegen werden Null gesetzt.

\f{equation}{Gew_{Dreis,neu,Ko2} = \frac{Gew_{Dreis,Ko1} \cdot 
  Ind_{Dreis,TFlie,Ko1} + Gew_{Dreis,Ko2} \cdot Ind_{Dreis,TFlie,Ko2}}{
  Ind_{Dreis,TFlie,Ko1} + Ind_{Dreis,TFlie,Ko2} } \f}  

\f{equation}{Gew_{Dreis,neu,Ko1} = 0 \f}  
 
\f{equation}{ABö_{Dreis,neu,Ko1} = 0 \f}  

und  

\f{equation}{ASo_{Dreis,neu,Ko1} = 0 \f}  
 
\f$ABö_{Dreis,neu,Ko2}\f$ = neue Muscheln werden aufgrund des alten 
 Verhältnisses von Böschung zu \\
\f$ASo_{Dreis,neu,Ko2}\f$ = Sohle auf die Populationen verteilt, noch nicht 
 ausformuliert

<!-- Quellcode -->

   if (ndr == 2 .and. gewdr(ior,1) > 1.6) then
	  !
	  ddrein = ((zdrei(ior,1)+zdreis(ior,1))*1000.)/gewdr(ior,1)
	  dreing = dreisn+ddrein
	  !
	  gewdr(ior,2) = (dreisn*gewdr(ior,2)+ddrein*gewdr(ior,1))          &
				     /(dreisn+ddrein)
	  !
	  gewdr(ior,1) = 0.0
	  zdrei(ior,1) = 0.0
	  zdreis(ior,1) = 0.0
   endif

\n\n

## 	Mortalität

Die Mortalitätsrate \f$dMort_{Dreis}\f$ hängt vom Gewicht der Dreissenen ab, 
sie wird folgendermaßen formuliert:

Falls \f$Gew_{Dreis} < 0,0246: dMort_{Dreis} = 0,1 \f$ \n
Falls \f$Gew_{Dreis} > 0,0246: dMort_{Dreis} = 0,0157 \cdot Gew_{Dreis}^{-0,502} \f$   \n
Falls \f$Gew_{Dreis} = 0: dMort_{Dreis}=0 \f$  \n
*diese Setzung erfolgt erst nach dem Rausschreiben (drmor) \n
Was ist mit: falls gleich 0,0246?*

\note Anschließend wird noch die Dreissenendichte pro m² ausgerechnet (fehlt noch)
 
<!-- Quellcode -->
 
	if (gewdr(ior,ndr) < 0.0246)dmorg = 0.1
    if (gewdr(ior,ndr) > 0.0246)                                      &
	    dmorg = 0.0157*gewdr(ior,ndr)**(-0.502)
    ! nur ausschreiben
    drmor(ior,ndr) = dmorg
    if (gewdr(ior,ndr) == 0.0)drmor(ior,ndr) = 0.0
 
    dreinm = dreing*(1.-exp(-dmorg*tflie))
    dreing = dreing-dreinm
    if (dreing < 0.0)dreing = 0.0
 
    if (ndr == 1 .and. dlafes > 0.0.and.                                &
	    (zdrei(ior,ndr)+zdreis(ior,ndr)) == 0.0) then
	   hcond1 = zdrei(ior,2)/(zdrei(ior,2)+zdreis(ior,2))
	   hcond2 = zdreis(ior,2)/(zdrei(ior,2)+zdreis(ior,2))
	   goto 351
    endif

*weiterer Code*   

<!-- Quellcode -->

    continue
    if (gewdr(ior,ndr) < 0.0246)dmorg = 0.1
    if (gewdr(ior,ndr) > 0.0246)                                      & 
	   dmorg = 0.0157*gewdr(ior,ndr)**(-0.502)
    ! nur ausschreiben
    drmor(ior,ndr) = dmorg
    if (gewdr(ior,ndr) == 0.0)drmor(ior,ndr) = 0.0
  
    dreinm = dreing*(1.-exp(-dmorg*tflie))
    dreing = dreing-dreinm
    if (dreing < 0.0)dreing = 0.0
  
    if (ndr == 1 .and. dlafes > 0.0.and.                                & 
	   (zdrei(ior,ndr)+zdreis(ior,ndr)) == 0.0) then
 	  hcond1 = zdrei(ior,2)/(zdrei(ior,2)+zdreis(ior,2))
 	  hcond2 = zdreis(ior,2)/(zdrei(ior,2)+zdreis(ior,2))
 	  goto 351
    endif
 
    if ((zdrei(ior,ndr)+zdreis(ior,ndr)) == 0.0 .and. ndr == 1) then
 	  zdrei(ior,ndr) = 0.0
 	  zdreis(ior,ndr) = 0.0
 	  goto 217
    endif
 
    if ((zdrei(ior,ndr)+zdreis(ior,ndr)) == 0.0 .and. ndr == 2           &
	   .and. ddrein == 0.0) then
	  zdrei(ior,ndr) = 0.0
	  zdreis(ior,ndr) = 0.0
	  goto 217
    endif
 
    if ((zdrei(ior,ndr)+zdreis(ior,ndr)) == 0.0 .and. ndr == 2           &
	   .and. ddrein > 0.0) then
	  hcond1 = hcondb(1)/(hcondb(1)+hconds(1))
	  hcond2 = hconds(1)/(hcondb(1)+hconds(1))
	  goto 351
    endif
 
    hcond1 = hcondb(ndr)/(hcondb(ndr)+hconds(ndr))
    hcond2 = hconds(ndr)/(hcondb(ndr)+hconds(ndr))

*weiterer Code **

<!-- Quellcode -->

         if ((zdrei(ior,ndr)+zdreis(ior,ndr)) == 0.0 .and. ndr == 2           &
             .and. ddrein == 0.0) then
            zdrei(ior,ndr) = 0.0
            zdreis(ior,ndr) = 0.0
            goto 217
         endif
         
         if ((zdrei(ior,ndr)+zdreis(ior,ndr)) == 0.0 .and. ndr == 2           &
             .and. ddrein > 0.0) then
            hcond1 = hcondb(1)/(hcondb(1)+hconds(1))
            hcond2 = hconds(1)/(hcondb(1)+hconds(1))
            goto 351
         endif
         
         hcond1 = hcondb(ndr)/(hcondb(ndr)+hconds(ndr))
         hcond2 = hconds(ndr)/(hcondb(ndr)+hconds(ndr))
         
         351 zdrei(ior,ndr) = (dreing*gewdr(ior,ndr)/1000.)*hcond1
         zdreis(ior,ndr) = (dreing*gewdr(ior,ndr)/1000.)*hcond2
         
      217 continue
      
      do ndr = 1,nndr
         zdrei(ior,ndr) = zdrei(ior,ndr)/(2.*lboem(ior)*elen(ior))
         if (bsohlm(ior) <= 0.0) then
            zdreis(ior,ndr) = 0.0
            cycle
         endif
         zdreis(ior,ndr) = zdreis(ior,ndr)/(bsohlm(ior)*elen(ior))
      enddo
      
      211 dlmax(ior) = dlmax(ior)/(2.*lboem(ior)*elen(ior))
      dlmaxs(ior) = dlmaxs(ior)/(bsohlm(ior)*elen(ior))
      
       111 continue
   
      999 dlarvn(anze+1) = dlarvn(anze)
      jahr_tst1 = jahrs
   
      return
    end

\n\n

# Filtrierrate, Aufnahmerate, Assimilationsrate, Exkretionsrate 
In diesem Kapitel werden die Berechnungen für die verschiedenen Raten 
(Aufnahmerate, Exkretionsrate etc…) dargestellt.

## Filtrierrate

Alle Berechnungen werden getrennt für beide Dreissena-Kohorten durchgeführt, 
hier wird jedoch nur jeweils einmal die allgemeine Formel dargestellt. \n
Die Filtrierrate von Dreissena \f$Filt_{Dreis}\f$ (fr) [m³/g/d] berechnet sich 
aus den Faktoren für Gewicht, Temperatur, Schwebstoff und Corophium mal einem 
Umrechnungsfaktor (wg. Einheiten):

\f{equation}{Filt_{Dreis} = F_{Filt,Dreis}(Gew) \cdot F_{Dreis}(T) \cdot 
   F_{Dreis}(SSges) \cdot F_{Dreis}(Coro) \cdot \frac{24}{1000} \f}

Aus der Filtrierrate wird nach Berechnung des Zuwachses der Dreissena (s.u.) 
aus der absoluten Dreissenenabundanz im Abschnitt 
\f$ABö_{Dreis} + ASo_{Dreis}\f$ (yc + ycs) das filtrierte Wasservolumen 
\f$FiltVol_{Dreis}\f$ (fh2ovol) berechnet, in Prozent als 
\f$FiltVolProz_{Dreis}\f$.

\f{equation}{FiltVol_{Dreis} = Filt_{Dreis} \cdot (ABö_{Dreis,T_{Flie}} + 
  ASo_{Dreis,T_{Flie}}) \cdot T_{Flie} \f} 

\f{equation}{FiltVolProz_{Dreis} = \frac{FiltVol_{Dreis}}{Vol_Absch} \f} 

mit:

\f{equation}{Vol_{Absch} = Länge_{Absch} \cdot Querschnittsfläche_{Absch} \f} 

<!-- Quellcode -->

	fh2ovol = fr*yc+fr*ycs 
	fh2ovol = fh2ovol*tflie 
	vofkop(ndr) = (fh2ovol/vol)*100.

\n\n

## Aufnahmerate

Die Aufnahmerate der Dreissena \f$Auf_{Dreis}\f$ (up, ups) [gC/m²] ergibt sich 
aus den jeweiligen Dreissena-Abundanzen auf Böschung bzw. Sohle mal der 
verschiedenen Faktoren für Individuengewicht, Temperatur, Chelicorophiumdichte 
und Futterkonzentration (s.o., Faktoren der Dreissenaentwicklung). Alle 
Berechnungen werden getrennt für beide Dreissena-Kohorten durchgeführt, hier 
wird jedoch nur jeweils einmal die allgemeine Formel dargestellt.

\f{equation}{AufBö_{Dreis} = F_{Aufn,Dreis}(Gew) \cdot F_{Dreis}(T) \cdot 
  F_{Dreis}(CoroBö) \cdot F_{Dreis}(Futter) \cdot ABö_{Dreis} \cdot AufSo_{Dreis} \\ 
  = F_{Aufn,Dreis}(Gew) \cdot F_{Dreis}(T) \cdot F_{Dreis}(CoroSo) \cdot 
  F_{Dreis}(Futter) \cdot ASo_{Dreis} \cdot Auf_{rel,Dreis} \\ 
  = F_{Aufn,Dreis}(Gew) \cdot F_{Dreis}(T) \cdot F_{Dreis}(Coro) \cdot 
  F_{Dreis}(Futter) \f}   

Die Gesamt-Aufnahmerate \f$AufBö_{Dreis} + AufSo_{Dreis}\f$ (up, ups) wird 
schließlich auf die Fließzeit und das Wasservolumen des aktuellen 
Gewässer-Abschnittes \f$Vol_{Abschn}\f$ (vol) bezogen:

\f{equation}{AufVol_{Dreis} = \frac{(AufBö_{Dreis} + AufSo_{Dreis}) \cdot 
  T_{Flie}}{Vol_{Abschn}} (uptm3) \f}     

\f$AufVol_{Dreis}\f$ wiederum wird weiterverwendet, um die Dreissena-Aufnahme 
der verschiedenen Algengruppen zu berechnen (adrg, adrk, adrb):

\f{equation}{AufGrün_{Dreis} = \frac{AufVol_{Dreis} \cdot F_{Dreis}(Grün)}{
  C_{Grün}} = \frac{AufVol_{Dreis} \cdot F_{Dreis}(Grün)}{0,48} \f} 
  
\f{equation}{AufKiesel_{Dreis} = \frac{AufVol_{Dreis} \cdot F_{Dreis}(Kiesel)}{
  C_{Kiesel}} = \frac{AufVol_{Dreis} \cdot F_{Dreis}(Kiesel)}{0,48} \f} 
  
\f{equation}{AufBlau_{Dreis} = \frac{AufVol_{Dreis} \cdot F_{Dreis}(Blau)}{
  C_{Blau}} = \frac{AufVol_{Dreis} \cdot F_{Dreis}(Blau)}{0,48} \f} 


\f{equation}{AufAlg_{Dreis}⁡ = AufGrün_{Dreis} + AufKiesel_{Dreis} + 
  AufBlau_{Dreis}  (draup Zeile 382) \f}


mit:

\f$F_{Dreis}(Gruppe)\f$: Faktor der jeweiligen Algengruppe, gibt deren Anteil 
an filtrierbarem Algenfutter (auf TG bezogen) an (hcong, hconk, hconb), siehe 
filtrierbare Futterkonzentration

andere C als vorbelegt (siehe Parameter Dreissena)
\n\n
Aus den Aufnahmeraten der einzelnen Algengruppen und des Schwebstoffes wird 
deren Faecesanteil (mal \f$F_{Dreis}\f$(Fäkal) (fec) ermittelt (Aufnahmerate 
minus Faecesanteil ergibt Assimilationsrate):

\f{equation}{FaecesGrün_{Dreis} = AufGrün_{Dreis} \cdot F_{Dreis}(Fäkal) \f} (drfecg)
\f{equation}{FaecesKiesel_{Dreis} = AufKiesel_{Dreis} \cdot F_{Dreis}(Fäkal) \f} (drfeck)
\f{equation}{FaecesBlau_{Dreis} = AufBlau_{Dreis} \cdot F_{Dreis}(Fäkal) \f} (drfecb)
\f{equation}{FaecesSS_{Dreis} = AufSS_{Dreis} \cdot F_{Dreis}(Fäkal) \f} (drfecs)

Später wird die Menge filtrierter Algen der einzelnen Gruppen und der 
Schwebstoffe \f$FiltSS_{Dreis}\f$ (filss) berechnet, indem ihre filtrierbare 
Biomasse \f$A_{Gruppe}\f$ mal \f$Prä_{Gruppe}\f$ in mg/l (aki, agr, abl mal pki, 
pgr, pbl) mit dem durch Dreissena filtrierten Anteil des Wassers im 
Gewässerabschnitt \f$FiltVolProz_{Dreis}\f$ (vofkop/100) multipliziert wird:

\f{equation}{FiltGrün_{Dreis} = A_{Grün} \cdot Prä_{Grün⁡ } \cdot 
  FiltVolProz_{Dreis} = A_{Grün} \cdot FiltVolProz_{Dreis} \f}
\f{equation}{FiltKiesel_{Dreis} = A_{Kiesel} \cdot Prä_{Kiesel} ⁡ \cdot 
  FiltVolProz_{Dreis} = A_{Kiesel} \cdot FiltVolProz_{Dreis} \f}
\f{equation}{FiltBlau_{Dreis} = A_{Blau} \cdot Prä_{Blau⁡ } \cdot 
  FiltVolProz_{Dreis} = A_{Blau} \cdot FiltVolProz_{Dreis} \f}
\f{equation}{FiltSS_{Dreis} = SS \cdot FiltVolProz_{Dreis} \f}

für Futter wird SS-Anteil von 0.04 berechnet.

aber: falls (\f$AufBlau_{Dreis}\f$ > 0 und \f$AufBlau_{Dreis}\f$ > \f$FiltBlau_{Dreis}\f$) \n
oder (\f$AufGrün_{Dreis} > 0\f$ und \f$AufGrün_{Dreis} > FiltGrün_{Dreis}\f$) \n
oder (\f$AufKiesel_{Dreis} > 0\f$ und \f$AufKiesel_{Dreis} > FiltKiesel_{Dreis}\f$):

\f{equation}{FiltVolProz_{Dreis}=\frac{AufGruppe_{Dreis}}{FiltGruppe_{Dreis}} \cdot 
  FiltVolProz_{Dreis} \f}

mit Gruppe: diejenige, die falls-Bedingung (zuerst) erfüllt

\f$FiltBlau_{Dreis} = AufBlau_{Dreis}\f$
\f$FiltGrün_{Dreis} = AufGrün_{Dreis}\f$
\f$FiltKiesel_{Dreis} = AufKiesel_{Dreis}\f$

 
![](img/dreiss_aufnahmerate_ss_oben.png)
![Aufnahme und Filtration von Futter in Abhängigkeit des Gesamt-Schwebstoffgehaltes bei sonst konstanten Bedingungen (Algenkonz.=opt. Futterkonz., Dreissenagewicht 40 mgC, 20°C, kein Corophium, Vol 24000m³, Boden 7560m², Dreissena-Dichte 100), oben: mit Faktor 24/1000 nur bei Filtrierrate, unten: mit gleichem Faktor bei beiden Raten.](img/dreiss_aufnahmerate_ss_unten.png)

\n\n


Der Anteil der durch Dreissena filtrierten Algen der jeweiligen Gruppen wird 
jeweils auf Chlorophyll a umgerechnet (dchlg, dchlk, dchlb): 

\f{equation}{dGrün_{Chl,Dreis} = FiltGrün_{Dreis} \cdot Chl:C_{Grün} ⁡ \cdot 
  C_{Grün} \cdot 1000 \f}
\f{equation}{dKiesel_{Chl,Dreis} = FiltKiesel_{Dreis} \cdot Chl:C{_Kiesel} ⁡ \cdot 
  C_{Kiesel} \cdot 1000  \f}
\f{equation}{dBlau_{Chl,Dreis} = FiltBlau_{Dreis} \cdot Chl:C_{Blau} ⁡ \cdot 
  C_{Blau} \cdot 1000  \f} 

### Summe über Kohorten

Die unter Aufnahmerate beschriebene Berechnung der filtrierten Algen (nach 
Gruppen) und Schwebstoffe sowie des ausgeschiedenen Faeces (ebenfalls nach 
Algengruppen) und des filtrierten Wassers werden in Zeilen 407ff über beide 
Kohorten summiert (ndr: Kohortenindex). Zusätzlich wird die Summe über die 
Aufnahme aller Algengruppen (draup) gebildet. Dies hat eine allgemeine 
Umbenennung der Variablen zur Folge.

<!-- Quellcode -->

	drfecg(ndr) = qfec*adrg(ndr) 
	drfeck(ndr) = qfec*adrk(ndr) 
	drfecb(ndr) = qfec*adrb(ndr) 
	drfecs(ndr) = qfec*drss(ndr) 
	vofkop(ndr) = (fh2ovol/vol)*100. 
	! 
	filaki(ndr) = aki(ior)*pki*fh2ovol/vol 
	filagr(ndr) = agr(ior)*pgr*fh2ovol/vol 
	filabl(ndr) = abl(ior)*pbl*fh2ovol/vol 
	filss(ndr) = ss(ior)*fh2ovol/vol 
	! 
	if(adrk(ndr).gt.0.0.and.adrk(ndr).gt.filaki(ndr))goto 501 
	if(adrg(ndr).gt.0.0.and.adrg(ndr).gt.filagr(ndr))goto 502 
	if(adrb(ndr).gt.0.0.and.adrb(ndr).gt.filabl(ndr))goto 503 
	goto 515 
	! 
	501 filaki(ndr) = adrk(ndr) 
	filagr(ndr) = adrg(ndr) 
	filabl(ndr) = adrb(ndr) 
	vofkop(ndr) = (filaki(ndr)/(aki(ior)*pki))*100. 
	goto 515 
	! 
	502 filaki(ndr) = adrk(ndr) 
	filagr(ndr) = adrg(ndr) 
	filabl(ndr) = adrb(ndr) 
	vofkop(ndr) = (filagr(ndr)/(agr(ior)*pgr))*100. 
	goto 515 
	! 
	503 filaki(ndr) = adrk(ndr) 
	filagr(ndr) = adrg(ndr) 
	filabl(ndr) = adrb(ndr) 
	vofkop(ndr) = (filabl(ndr)/(abl(ior)*pbl))*100. 
	! 
	515 dchlg(ndr) = 0.0
	dchlk(ndr) = 0.0
	dchlb(ndr) = 0.0
	
	if(agbcm(ior)>0.0)dchlg(ndr) = filagr(ndr)*1000.*cagr/agbcm(ior) 
	if(akbcm(ior)>0.0)dchlk(ndr) = filaki(ndr)*1000.*caki/akbcm(ior) 
	if(abbcm(ior)>0.0)dchlb(ndr) = filabl(ndr)*1000.*cabl/abbcm(ior) 
	! 
	filhnf(ndr) = chnf(ior)*fh2ovol/vol 
	! 
	! 
	214 continue 
	! 
	algdrg(ior) = 0.0 
	algdrk(ior) = 0.0 
	algdrb(ior) = 0.0 
	ssdr(ior) = 0.0 
	drfaek(ior) = 0.0 
	drfaeg(ior) = 0.0 
	drfaeb(ior) = 0.0 
	drfaes(ior) = 0.0 
	draup = 0.0 
	volfdr(ior) = 0.0 
	! 
	drhnf(ior) = 0.0 
	! 
	! 
	do ndr=1,nndr 
	! 
	algdrg(ior) = algdrg(ior)+filagr(ndr) 
	algdrk(ior) = algdrk(ior)+filaki(ndr) 
	algdrb(ior) = algdrb(ior)+filabl(ndr) 
	ssdr(ior) = ssdr(ior)+filss(ndr) 
	drfaeg(ior) = drfaeg(ior)+drfecg(ndr) 
	drfaek(ior) = drfaek(ior)+drfeck(ndr) 
	drfaeb(ior) = drfaeb(ior)+drfecb(ndr) 
	drfaes(ior) = drfaes(ior)+drfecs(ndr) 
	
	volfdr(ior) = volfdr(ior)+vofkop(ndr) 
	
	draup = draup+adrg(ndr)+adrk(ndr)+adrb(ndr) 
	drhnf(ior) = drhnf(ior)+filhnf(ndr) 
	enddo
	
	!....Ausgabe 
	hnfdra(ior) = 0.0 
	if(chnf(ior)>0.0)hnfdra(ior) = (drhnf(ior)/chnf(ior))*24. 
	
	!.....Schwebstoffaufnahme durch Dreissena wird vorläufig auf Null gesetzt
	ssdr(ior) = 0.0 
	! 
	if((algdrg(ior)+algdrk(ior)+algdrb(ior)).eq.0.0)then 
	drpfec(ior) = 0.0 
	else 
	drpfec(ior) = 1.-(draup/(algdrg(ior)+algdrk(ior)+algdrb(ior))) 
	endif
	
	drpfec(ior) = drpfec(ior)*100. 
	if(draup.eq.0.0)drpfec(ior) = 0.0 
	if(drpfec(ior).eq.0.0)volfdr(ior) = 0.0 
	if(drpfec(ior).lt.0.0)drpfec(ior) = 0.0 

\n\n


## Assimilationsrate

Die Assimilationsraten \f$AssBö_{Dreis}\f$ und \f$AssSo_{Dreis}\f$ (assr, assrs) 
berechnen sich aus dem Ertragsanteil des Futters (1 - Fäkalrate 
\f$F_{Dreis}\f$(Fäkal) = \f$F_{Dreis}\f$(Ertrag)) mal der Aufnahmerate.

\f{equation}{AssBö_{Dreis} = (F_{Dreis}(Ertrag)) \cdot AufBö_{Dreis} \f} (assr)
\f{equation}{AssSo_{Dreis} = (F_{Dreis}(Ertrag)) \cdot AufSo_{Dreis} \f} (assrs)


<!-- Quellcode -->

	! Berechnung der Assimilationsrate 
	! 
	qfec = 0.315*exp(0.88*hconf) 
	assr = (1.-qfec)*up 
	assrs = (1.-qfec)*ups 
	! 

\n\n

## Exkretionsrate

Die Exkretionsrate wird als fester Anteil der Assimilationsrate berechnet:
 
\f{equation}{ExkBö_{Dreis} = 0,064 \cdot AssBö_{Dreis} \f}
\f{equation}{ExkSo_{Dreis} = 0,064 \cdot AssSo_{Dreis} \f}
 
mit: 

0,064: Exkretionsrate qex, siehe Parameter Dreissena

Die Gesamt-Exkretion ExkBö_{Dreis} + ExkSo_{Dreis} (exdr + exdrs) wird 
schließlich auf das Wasservolumen des aktuellen Gewässer-Abschnittes 
\f$Vol_{Abschn}\f$ (vol) bezogen (Zeilen 288ff):

\f{equation}{ExkVol_{Dreis} = \frac{ExkBö_{Dreis} + ExkSo_{Dreis}}{Vol_{Abschn}} \f}  
(excm3, exdrvz = excm3 * TFlie)

\f$ExkVol_{Dreis}\f$ wiederum wird auf die Fließzeit bezogen (exdrvz) 
weiterverwendet, um die Dreissena-Exkretion der verschiedenen Algengruppen zu 
berechnen (exdrvg, exdrvk, exdrvb):

\f{equation}{ExkGrün_{Dreis} = \frac{(ExkVol_{Dreis} \cdot T_{Flie} \cdot 
  F_{Dreis}(Grün)}{C_{Grün}} = \frac{ExkVol_{Dreis} \cdot T_{Flie} \cdot 
  F_{Dreis}(Grün)}{0,483} \f}

\f{equation}{ExkKiesel_{Dreis} = \frac{ExkVol_{Dreis} \cdot T_{Flie} \cdot 
  F_{Dreis}(Kiesel)}{C_{Kiesel}} = \frac{ExkVol_{Dreis} \cdot T_{Flie} \cdot 
  F_{Dreis}(Kiesel)}{0,379} \f}  
 
\f{equation}{ExkBlau_{Dreis} = \frac{ExkVol_{Dreis} \cdot T_{Flie} \cdot 
  F_{Dreis}(Blau)}{C_{Blau}} = \frac{ExkVol_{Dreis} \cdot T_{Flie} \cdot 
  F_{Dreis}(Blau)}{0,48} \f} 
 
mit
\f$F_{Dreis}(Gruppe)\f$: Faktor der jeweiligen Algengruppe, gibt deren Anteil 
an filtrierbarem Algenfutter (auf TG bezogen) an (hcong, hconk, hconb), siehe 
filtrierbare Futterkonzentration \n
andere CGruppe als vorbelegt (siehe Parameter Dreissena)

<!-- Quellcode -->

	! Berechnung der Exkretionsrate 
	exdr = 0.064*(1.-qfec)*up 
	exdrs = 0.064*(1.-qfec)*ups 
	exdrvz = exdrvz+excm3*tflie 
	exdrvg(ior) = exdrvz*hconvg/0.48 
	exdrvk(ior) = exdrvz*hconvk/0.48 
	exdrvb(ior) = exdrvz*hconvb/0.48 

\n\n

## Pseudofaecesanteil

Der Anteil des Pseudofaeces \f$PsFäDreis\f$ wird aus der Aufnahme- 
(\f$AufAlg_{Dreis,ges}\f$) und Filtrationsrate (\f$FiltAlg_{Dreis,ges}\f$) 
aller Algen (Basis: Summe der Kohorten ‚Dreis,ges’) berechnet:
 
\f{equation}{PsFä_{Dreis} = 1 - \frac{AufAlg_{Dreis,ges}}{FiltAlg_{Dreis,ges}} \f} 
  
aber: = 0 falls \f$FiltAlg{Dreis,ges} = 0\f$ oder  \f$AufAlg_{Dreis,ges} = 0\f$ 

(min: 0: diese Bedingung kann eigentlich entfallen, da weiter oben Filt = Auf 
gesetzt, falls Auf > Filt, siehe Aufnahmerate)

mit:

\f{equation}{FiltAlg_{Dreis,ges} ⁡   = FiltGrün_{Dreis,ges} + 
  FiltKiesel_{Dreis,ges} + FiltBlau_{Dreis,ges} \f}
 
\f$AufAlg_{Dreis}\f$: siehe Aufnahmerate

Falls der Pseudofaecesanteil Null ist, wird die Filtrationsrate von Dreissena 
ebenfalls Null gesetzt.

<!-- Quellcode -->

	if((algdrg(ior)+algdrk(ior)+algdrb(ior)).eq.0.0)then 
	drpfec(ior) = 0.0 
	else 
	drpfec(ior) = 1.-(draup/(algdrg(ior)+algdrk(ior)+algdrb(ior))) 
	endif
	
	drpfec(ior) = drpfec(ior)*100. 
	if(draup.eq.0.0)drpfec(ior) = 0.0 
	if(drpfec(ior).eq.0.0)volfdr(ior) = 0.0 
	if(drpfec(ior).lt.0.0)drpfec(ior) = 0.0 

\n\n

# Larvenentwicklung 

## Zeitpunkt der Larvenentwicklung

Die maximale Dreissenabiomasse auf Böschung und Sohle, \f$ABö_{Dreis,max}\f$ 
und \f$ASo_{Dreis,max}\f$, werden unter Verwendung der maximalen 
Dreissena-Dichten (dlmax und dlmaxs) berechnet, indem die Böschungs- und 
Sohllängen \f$Länge_{Bö}\f$ und \f$Länge_{So}\f$ (lboem, bsohlm) sowie die 
Abschnittlänge \f$Länge{Absch}\f$ (elen) verwendet werden.

Anschließend wird der Tag des Jahres \f$Tag_{StartL1}\f$ (NRla1a), an dem die 
Laichperiode startet, aus dem im Modelleditor angegebenen Starttag und 
Startmonat berechnet. Auch für den aktuellen Tag der Modellierung wird der Tag 
des Jahres \f$Tag-{akt}\f$ (NRS) berechnet.

Das Ende der Laichsaison \f$Tag_{EndeL1}\f$ (NRla1e) ergibt sich aus 
\f$Tag_{StartL1}\f$ plus der Dauer der Laichsaison \f$Dauer_{Laich,Dreis}\f$ 
(laid1, ebenfalls im Modelleditor für den Abschnitt definiert).

<!-- Quellcode -->

	! Berechnung der Larvenbildung 
	! 
	ddlarn = 0.0 
	dlamor = 0.0 
	dlafes = 0.0 
	! 
	dlmax(ior) = dlmax(ior)*(2.*lboem(ior)*elen(ior)) 
	dlmaxs(ior) = dlmaxs(ior)*(bsohlm(ior)*elen(ior)) 
	
	if(lait1.eq.0.and.laim1.eq.0)goto 211 
	if(drft.ge.laid1)goto 116 
	if(ilang.eq.0.or.jahr_tst1.lt.jahrs)then 
	drrt = 0.0 
	goto 211 
	endif 
	if(monats.gt.2)goto 23 
	nrs = itags+31*(monats-1) 
	goto 29 
	23 nrs = (itags+31*(monats-1)-int(0.4*monats+2.3)) 
	! 
	29 if(laim1.gt.2)goto 25 
	nrla1a = lait1+31*(laim1-1) 
	goto 26 
	25 nrla1a = (lait1+31*(laim1-1)-int(0.4*laim1+2.3)) 
	! 
	26 nrla1e = nrla1a+laid1 
	
	if(nrs.lt.nrla1a.or.nrs.ge.nrla1e)then 
	drrt = 0.0 
	goto 113 
	endif 
	! 
	! 
	drrt1 = 0.0 
	drrt3 = 30. 
	drrt2 = drrt3/2. 
	
	
	drrt11 = 0.0 
	drrt33 = laid1-drrt3 
	drrt22 = drrt33/2. 
	! 

\n\n

## Gewichtsverlust durch Reproduktion

Während des Wachstums der Dreissenen in der Laichperiode wird ein konstanter 
Anteil angenommen, der für die Reproduktion aufgewendet wird. In Abhängigkeit 
des aktuellen relativen Zeitpunktes drrt erfolgt die Berechnung durch:
 
\f{equation}{
 Larven_{Dreis,mue} = 
 \begin{cases}
  % case 1: drrt ≤ drrt3
  \frac{Re⁡pr_{Dreis,mue} \cdot 0,6}{0,5 \cdot drrt3} = 0,0208 & drrt ≤ drrt3 \\
  % case 2: drrt > drrt3⁡
  \frac{Re⁡pr_{Dreis,mue} \cdot 0,4}{0,5 \cdot drrt33} = 
  \frac{0,416}{Dauer_{Laich,Dreis} - 30} & {drrt > drrt3⁡}
 \end{cases}
\f}

Bei der Berechung des Faktors \f$F_{Dreis}(GewVerl)\f$ (fdrrt) ist der aktuelle 
Zeitpunkt drrt (bzw. drrtt = drrt+drrt3) entscheidend: 

Falls Drrt <= drrt2: \n
\f{equation}{F_{Dreis}(GewVerl) = \frac{(drrt - drrt3)^2 \cdot 
  Larven_{Dreis,mue}}{(drrt - drrt2)^2 + (drrt - drrt3)^2} \f}
  
Sonst falls Drrt <= drrt3: \n  
\f{equation}{F_{Dreis}(GewVerl) = \frac{(drrt - drrt1)^2 \cdot
  Larven_{Dreis,mue}}{(drrt - drrt2)^2 + (drrt - drrt1)^2} \f}
  
Sonst falls Drrtt <= drrt22: \n 
\f{equation}{F_{Dreis}(GewVerl) = \frac{(drrtt - drrt33)^2 \cdot 
  Larven_{Dreis,mue}}{(drrtt - drrt22)^2 + (drrtt - drrt33)^2} \f}
  
Sonst falls Drrtt > drrt22: \n 
\f{equation}{F_{Dreis}(GewVerl) = \frac{(drrtt - drrt11)^2 \cdot
  Larven_{Dreis,mue}}{(drrtt - drrt22)^2 + (drrtt - drrt11)^2 } \f}

 
![Relativer Tag drrt und Larvenbildung (vorläufige Abb.).](img/dreiss_relTag_Larvenbildung.png)
\n\n


Mit dem Faktor \f$F_{Dreis}(GewVerl)\f$ wird schließlich die Gewichtsabnahme der 
Dreissenen aufgrund der Reproduktion berechnet:

\f{equation}{Gew_{Dreis,TFlie1} = Gew_{Dreis,TFlie} - Gew_{Dreis,max⁡} \cdot 
  T_{Flie} \cdot F_{Dreis}(GewVerl) \f}

<!-- Quellcode:-->

	!.....Annahme Gewichtverlust der Adulten durch Reproduktion 
	! 
	! 
	if(drrt.le.drrt3)spwmx = flai*0.6/(0.5*drrt3) 
	if(drrt.gt.drrt3)spwmx = flai*0.4/(0.5*(laid1-drrt3)) 
	! 
	if(dlmax(ior).eq.0.0.and.dlmaxs(ior).eq.0.0)then 
	dlmax(ior) = zdrei(ior,2) 
	dlmaxs(ior) = zdreis(ior,2) 
	gwdmax(ior) = gewdr(ior,2) 
	sgwmue(ior) = 0.0 
	endif 
	! 
	if(drrt.gt.drrt3)goto 221 
	! 
	if(drrt.gt.drrt2)goto 220 
	fdrrt = ((drrt-drrt1)**2)/((drrt-drrt2)**2+(drrt-drrt1)**2) 
	fdrrt = fdrrt*spwmx 
	goto 250 
	220 fdrrt = ((drrt-drrt3)**2)/((drrt-drrt2)**2+(drrt-drrt3)**2) 
	fdrrt = fdrrt*spwmx 
	goto 250 
	! 
	! zweite Kurve 
	! 
	221 drrtt = drrt-drrt3 
	if(drrtt.gt.drrt22)goto 225 
	fdrrt = ((drrtt-drrt11)**2)/((drrtt-drrt22)**2+(drrtt-drrt11)**2) 
	fdrrt = fdrrt*spwmx 
	goto 250 
	225 fdrrt = ((drrtt-drrt33)**2)/((drrtt-drrt22)**2+(drrtt-drrt33)**2) 
	fdrrt = fdrrt*spwmx 
	! 
	250 continue 
	do 216 ndr=2,nndr 
	gewdr(ior,ndr) = gewdr(ior,ndr)-(gwdmax(ior)*tflie*fdrrt) 
	! 
	

\n\n

## Gewichtsverlust und Larvenbildung

Anschließend wird die Zahl der aufgrund des Gewichtsverlustes gebildeten Eier 
errechnet, wobei der Anteil weiblicher Dreissenen einfließt.

\f{equation}{Ei_{Dreis,GewVerl} = \frac{A_{Dreis,max⁡} \cdot F_{Dreis}(GewVerl)}
  {3,35 \cdot 10^6} \cdot 0,75 \cdot F_{weib,Dreis} \f} (dEi)

Daraus schlüpfen Larven \f$Larven_{Dreis,TFlie}\f$ (ddlarn) in Abhängigkeit 
des Anteils gesunder Individuen:
 
\f{equation}{dLarven_{Dreis,GewVerl} = Ei_{Dreis,TFlie} \cdot F_{gesund,Dreis} \f}
 
Mit:\n
\f$F_{gesund,Dreis}\f$: Anteil gesunder Individuen, gesetzt 0,25

	
<!-- Quellcode:-->

	!+++Berechnung der gebildeten Larven im Zeitschritt 
	! aus dem Gewichtsverlust der Weibchen+++++ 
	! C-Gehalt einer Eizelle: 3.35e-9 g 
	! 
	dei = (dlmax(ior)+dlmaxs(ior))*tflie*fdrrt/3.35e-9 
	dei = dei*0.75 
	ddlarn = dei*fgesund*fweib

\n\n
 
## Zuwachs und Larvenbildung

Aufgrund des Zuwachses werden ebenfalls Dreissena-Larven gebildet (deimue):

\f{equation}{dLarven_{Dreis,mue} = \frac{(d_{Dreis}Bö + d_{Dreis}So) \cdot 
  Re⁡pr_{Dreis,mue}}{3,35 \cdot 10^{-10}} \cdot 0,75 \cdot 
  F_{gesund,Dreis} \cdot F_{weib,Dreis} \f} 

\f{equation}{dLarven_{Dreis,mue} = \frac{(d_{Dreis}Bö + d_{Dreis}So) \cdot 
  0,52}{3,35 \cdot 10^{-10}} \cdot 0,75 \cdot 0,5 \cdot 0,25 = 
  (d_{Dreis}Bö + d_{Dreis}So) \cdot 14.55 \cdot 10^6 \f} 

Die gesamt-Larvenbildung wird folgendermaßen errechnet:
 
\f{equation}{dLarven_{Dreis} = \frac{dLarven_{Dreis,GewVerl} + 
  dLarven_{Dreis,mue}}{Vol_{Absch} \cdot 1000}  \; \f}
(Zeilen 497 und 504, ddlarn)
 
\f{equation}{Gew_{Dreis,TFlie} = Gew_{Dreis} - dGew_{Dreis} \cdot 0,52 \f}

\f{equation}{d_{Dreis,mue,rel} = \frac{d_{Dreis}Bö}{2 \cdot Länge_{Bö} \cdot 
  Länge_{Absch}} \cdot 0,52 + \frac{d_{Dreis}Bö}{Länge_{So} \cdot 
  Länge_{Absch}} \cdot 0,52 \f}

\f{equation}{d_{Dreis,mue,rel} = \left(\frac{ZuwachsBö_{Dreis}}{Fläche_{Bö,Absch}} +
  \frac{ZuwachsSo_{Dreis}}{Fläche_{So,Absch}} \right) \cdot 0,52 \f} 
 
\f{equation}{sgwmue1 = sgwmue + d_{Dreis,mue,rel} \f}

sgwmue: Übergabewert, alter Wert wird Routine übergeben 

\f{equation}{dfemue = \frac{sgwmue + d_{Dreis,mue,rel}}{tdpla} \cdot T_{Flie} =
  \frac{sgwmue + d_{Dreis,mue,rel}}{2} \cdot T_{Flie} \f}

\note Z.554, Wachstum festgesetzter Larven?

Mit\n
Tdpla = 2

\f{equation}{sgwmue2 = sgwmue1 - dfemue = sgwmue + d_{Dreis,mue,rel} - 
  \frac{sgwmue + d_{Dreis,mue,rel}}{2} \cdot T_{Flie} \f}

\f{equation}{sgwmue2 = (sgwmue + d_{Dreis,mue,rel}) \cdot \left(1 - 
  \frac{T_{Flie}}{2} \right) \f}

\f{equation}{ABö_{Dreis,TFlie} = ABö_{Dreis} - ABö_(Dreis,max⁡ ) \cdot 
  T_{Flie} \cdot F_{Dreis}(GewVerl) - dBö_{Dreis} \cdot 0,52 \f} (zdrei)
  
\f{equation}{ASo_{Dreis,TFlie} = ASo_{Dreis} - ASo_(Dreis,max⁡ ) \cdot 
  T_{Flie} \cdot F_{Dreis}(GewVerl) - dSo_{Dreis} \cdot 0,52 \f} (zdreis)

\n\n

## Mortalität der Larven
\f{equation}{Mort_{Dreis,Larven} = Larven_{Dreis} \cdot (1 - 
  e^{Mort_{Dreis,Larven1} \cdot T_{Flie}})  \f}

\f{equation}{Mort_{Dreis,Larven} = Larven_{Dreis} \cdot (1 - 
  e^{4,13 \cdot T_{Flie}})  \f}
 
Mit \n
\f$Mort_{Dreis,Larven1}\f$: Mortalitätsrate Dreissenalarven (k1mor), gesetzt 4,13

LarvenDreis: 



\n\n

## Festsetzen der Larven

Zunächst wird wie unter „Gewichtsverlust durch Reproduktion“ beschrieben ein 
Faktor \f$F_{Dreis}(GewVerl2)\f$ berechnet, wobei lediglich statt dem relativen 
Tag drrt der relative Tag drrft eingesetzt wird.

\note genaue Herkunft des Wertes drrft muss noch geklärt werden: Drft wird in 
QSim außer bei Aufruf von Dreissen.for nicht verwendet, Dreissen.for wird es 
drft=drft+TFlie gesetzt, falls ior = 1, ansonsten auch keine Verwendung oder 
Setzung bis auf Berechnung von FDreis(GewVerl2), ist also wahrscheinlich 
entweder 0 oder TFlie (i.A. 1/24), dLarvenDreis,Fests entspräche dann 0,0166 
für drft = 1/24 bzw. 0,0168 für drft = 0

Anschließend wird die Zahl der festgesetzen Eier \f$dLarven_{Dreis,Fests}\f$ 
(dlafes, vgl. dlarn) errechnet, wobei wieder die Anteile weiblicher und 
gesunder Dreissenen einfließen:
 
\f{equation}{dLarven_{Dreis,Fests} = \frac{A_{Dreis,max} \cdot F_{Dreis}(GewVerl2)}{
  3,35 \cdot 10^6} \cdot 0,75 \cdot F_{weib,Dreis} \cdot F_{gesund,Dreis} \f} 
 
(dlafes, analog zu dei und ddlarn (\f$dLarven_{Dreis,GewVerl}\f$))

\note Kommentar im Code: Larvengewicht beim Festsetzen: 8.6E-5 mgC, wird hier 
aber nicht verwendet, statt dessen Gewicht der Eier, s.o.!

mit

\f$A_{Dreis,max}\f$: \f$ABö_{Dreis,max} + ASo_{Dreis,max}\f$
\f$F_{weib,Dreis}\f$: Anteil weiblicher Individuen, gesetzt 0,5
\f$F_{gesund,Dreis}\f$: Anteil gesunder Individuen, gesetzt 0,25

\f{equation}{dfemue1 = \left(\frac{dfemue \cdot (2 \cdot Länge_{Bö} \cdot 
  Länge_{Absch}) \cdot ABö_{Dreis}}{ABö_{Dreis} + ASo_{Dreis}} + 
  \frac{dfemue \cdot (Länge_{So} \cdot Länge_{Absch}) \cdot ASo_{Dreis}}{
  ABö_{Dreis} + ASo_{Dreis}} \right) \cdot 0,75 \cdot F_{Dreis,ges} \cdot 
  F_{Dreis,weibl} \f}  

\f{equation}{dfemue1 = (Fläche_{Bö,Absch} \cdot ABöProz_{Dreis} + 
  Fläche_{So,Absch} \cdot ASoProzent_{Dreis} ) \cdot dfemue \cdot 27,98 \cdot 10^6  \f} 
 
\note Diese Berechnung ist schwer nachvollziehbar, prüfen! Warum wird der 
Anteil der Dreissenenpopulation (auf Böschung bzw. Sohle) an Gesamtpopulation 
mit der jeweiligen Fläche (Böschung bzw. Sohle) multipliziert? Was soll das 
anschaulich sein?

\f{equation}{dLarven_{Dreis,Festg} = (dfemue1 + dLarven_{Dreis,Fests}) \cdot 
  \frac{e^{Mort_{Dreis,fest1}}}{Vol_{Absch} \cdot 1000} = 
  (dfemue1 + dLarven_{Dreis,Fests}) \cdot \frac{e^{-8,26}}{Vol_{Absch} \cdot 1000}  \f} 

 
\n\n

##  Ergebnis Larven

Schließlich wird die Larvenzahl nach der Fließzeit berechnet durch 
(Vereinfachen der Gleichungen sinnvoll):

\f{equation}{Larven_{Dreis,TFlie} = Larven_{Dreis} + dLarven_{Dreis,GewVerl} - 
  Mort_{Dreis,Larven} - dLarven_{Dreis,Festg}  \f} 

\f{equation}{Larven_{Dreis,TFlie} = Larven_{Dreis} + dLarven_{Dreis,GewVerl} - 
  Larven_{Dreis} \cdot (1-e^{-Mort_{Dreis,Larven1} \cdot T_{Flie}}) - 
  dLarven_{Dreis,Festg} \f}

\f{equation}{Larven_{Dreis,TFlie} = Larven_{Dreis} \cdot e^{-4,13 \cdot T_{Flie}} +
  dLarven_{Dreis,GewVerl} - dLarven_{Dreis,Festg} \f}
  
Aber: min. 0

mit

\f$dLarven_{Dreis,GewVerl} \f$: (dlarn) \n
\f$dLarven_{Dreis,Festg}   \f$: (dlafes) \n
\f$Larven_{Dreis}          \f$: Dreissenalarven (dlafn) \n

\f$Mort_{Dreis,Larven} = Larven_{Dreis} \cdot (1 - e^{-Mort_{Dreis,Larven1} \cdot 
  T_{Flie}}) \f$ , s.o., 
  
\note Vielleicht wäre es sinnvoller, einfach \f$ F_{Dreis,Larven}(Mort) = 
e^{-Mort_{Dreis,Larven1 \cdot T_{Flie}}}\f$ als Mortalitätsfaktor einzuführen? 
Dann könnte folgende vereinfachte Form der Gleichung verwendet werden:
 
\f{equation}{Larven_{Dreis,TFlie} = Larven_{Dreis} \cdot F_{Dreis,Larven}(Mort) + 
  dLarven_{Dreis,GewVerl} - dLarven_{Dreis,Festg} \f}

<!-- Quellcode -->

	 ! Larvenbildung aus Zuwachs im Zeitschritt
	 dEimue = ((dyc*flai+dycs*flai)/3.35e-9)*0.75*fgesund*fweib
	 if (dEimue < 0.0)dEimue = 0.0
	 ddlarn = ddlarn+dEimue
	 gewdr(ior,ndr) = gewdr(ior,ndr)-dgewdr*0.52
	 dgwmue = (dyc / (2.*lboem(ior)*elen(ior)))*0.52  &
			+ (dycs / (bsohlm(ior)*elen(ior)))*0.52
	 sgwmue(ior) = sgwmue(ior)+dgwmue
	 
	 ddlarn = ddlarn/(vol*1000.)
	 zdrei(ior,ndr) = zdrei(ior,ndr)-(dlmax(ior)*tflie*fdrrt)
	 zdrei(ior,ndr) = zdrei(ior,ndr)-dyc*0.52
	 zdreis(ior,ndr) = zdreis(ior,ndr)-(dlmaxs(ior)*tflie*fdrrt)
	 zdreis(ior,ndr) = zdreis(ior,ndr)-dycs*0.52


Mortalität der Larven

Wiederholung des Blocks zur Berechnung des Jahresganges (Zeitpunkt der 
Larvenbildung). \n
Nur mit drft (übergebener Wert+TFlie) statt drrt \n
Das Ergebnis der Berechnung wird wie oben \f$F_{Dreis}\f$(GewVerl) 
\f$F_{Dreis}\f$(Fests) (fdrrt) bezeichnet.

<!-- Quellcode -->

      if (nrs>=nrla1a .and. itime_hoch == 1)stdpla = stdpla+tflie
      dlamor = dlarvn(ior)*(1.-exp(-klmor*tflie))
      if (stdpla < tdpla) then
         drft = 0.0
         itime_hoch = 0
         goto 114
      endif
      
      if (itime_hoch == 1) then
         drft = drft+tflie
         itime_hoch = 0
      endif
      
      if (drft <= drrt3)spwmx = flai*0.6/(0.5*drrt3)
      if (drft > drrt3)spwmx = flai*0.4/(0.5*(laid1-drrt3))
      
      if (drft > drrt3)goto 325
      
      if (drft > drrt2)goto 322
      fdrrt = ((drft-drrt1)**2)/((drft-drrt2)**2+(drft-drrt1)**2)
      fdrrt = fdrrt*spwmx
      goto 350
      322 fdrrt = ((drft-drrt3)**2)/((drft-drrt2)**2+(drft-drrt3)**2)
      fdrrt = fdrrt*spwmx
      goto 350
      
      ! zweite Kurve
      325 drftt = drft-drrt3
      if (drftt > drrt22)goto 320
      fdrrt = ((drftt-drrt11)**2)/((drftt-drrt22)**2+(drftt-drrt11)**2)
      fdrrt = fdrrt*spwmx
      goto 350
      320 fdrrt = ((drftt-drrt33)**2)/((drftt-drrt22)**2+(drftt-drrt33)**2)
      fdrrt = fdrrt*spwmx


Auch die folgenden Berechnungsschritte werden sinngemäß noch einmal 
durchlaufen, wobei andere Variablennamen verwendet werden (dlafes <-> dEi)

<!-- Quellcode -->

      ! Larvengewicht beim Festsetzen: 8.6e-8 gC; 8.6e-5 mgC
      dlafes = (dlmax(ior)+dlmaxs(ior))*tflie*fdrrt/3.35e-9
      dlafes = dlafes*0.75*fgesund*fweib
      116 dfemue = sgwmue(ior)/(tdpla*1./tflie)
      if (zdreis(ior,2) == 0.0 .and. zdrei(ior,2) == 0.0) then
         dfemue = 0.0
         dfmue = 0.0
         dfmues = 0.0
         sgwmue(ior) = sgwmue(ior)-dfemue
         goto 316
      endif
      
      sgwmue(ior) = sgwmue(ior)-dfemue
      dfmue = dfemue*(2.*lboem(ior)*elen(ior))*                         &
              (zdrei(ior,2)/(zdrei(ior,2)+zdreis(ior,2)))
      dfmues = dfemue*(bsohlm(ior)*elen(ior))*                          &
               (zdreis(ior,2)/(zdrei(ior,2)+zdreis(ior,2)))
      316 dfemue = ((dfmue+dfmues)/3.35e-9)*0.75*fgesund*fweib
      dlafes = dlafes+dfemue
      dlafes = (dlafes*exp(-klmorg))/(vol*1000.)
      114 dlarvn(ior) = dlarvn(ior)+ddlarn-dlamor-dlafes
      if (dlarvn(ior) < 0.0)dlarvn(ior) = 0.0

\n\n

 
# QSim-Veröffentlichungen, die den Dreissena-Baustein beschreiben und/oder anwenden: 

- Seredszus, Fabian (1998): Populationsdynamische Grundlagen zur mathematischen 
Modellierung der Filtrierleistung und Populationsdynamik von Muscheln. 
Literaturstudie erstellt im Auftrag der BfG, 71 Seiten.

 

\n\n

Textquelle: dreissena-prozess.md; Codesource: dreissen.f90; zurück: \ref lnk_dreissena