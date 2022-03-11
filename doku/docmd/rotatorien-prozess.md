Rotatorien - Prozesse {#lnk_rotatorien_equ}
=====================

# Bilanzgleichung #
Die zeitliche Änderung der Rotatorienkonzentration \f$ Rot \f$ ergibt sich aus
der [Wachstumsrate](#lnk_rot-wachstum) \f$\mu\f$, der [Grundrespirationsrate](#lnk_rot-respG) 
\f$resp_G\f$ und der [Mortatlitätsrate](#lnk_rot-mort) \f$ mort \f$. Die Bilanzgleichung der Rotatorien lautet:
	\f[ 
		\frac{d}{dt}Rot = (\mu-resp_\mathrm{G}-mort)\cdot Rot
	\f]
	

# Wachstumsrate {#lnk_rot-wachstum}

Rotatorien decken ihren Energiebedarf durch den Fraß von Algen. Die so aufgenommene
Biomasse kann jedoch nur zu einem Teil in eigene Biomasse umgewandelt werden.
Die Wachstumsrate \f$\mu\f$ errechnet sich daher aus der Ingestionsrate \f$I\f$ und einem 
Ertragskoeffizienten \f$\eta\f$:
	\f[
		\mu = I \cdot \eta
	\f]

## Ingestionsrate	
Die Ingestionsrate \f$ I \f$ der Rotatorien wird beeinflusst durch die Wassertemperatur und
das verfügbare Nahrungsangebot. Modelliert wird sie über die maximale Ingestionsrate
\f$ I_{\max} \f$ bei 20°C  und den Temperatureinfluss \f$ f_\mathrm{Temp,I} \f$ sowie den
Nahrungseinfluss \f$ f_\mathrm{Nahr} \f$.
    \f[
        I = I_\max \cdot f_\mathrm{Temp,I} \cdot f_\mathrm{Nahr}
    \f]



Die Beschreibung des Temperatureinflusses erfolgt über die van't-Hoff'schen Regel, 
die besagt, dass  bei einer Erhöhung der Temperatur \f$T\f$ um 10°C eine
Umsatzrate um den Faktor \f$ Q_{10} \f$ zunimmt.  Für den Temperaturfaktor 
\f$ f_\mathrm{Temp,I} \f$ gilt daher:
	\f[
		f_\mathrm{Temp,I} = Q_{10,I}^{(T-20)/10}
	\f]
    

Darüber hinaus ist die Ingestionsrate abhängig von der verfügbaren Algenkonzentration.
Die im Wasser vorhandenen Algen sind für Rotatorien nicht vollständig 
filtrierbar und stehen daher auch nicht vollständig für die Ingestion zur Verfügung.
Jede Algengruppe \f$ A_i \f$ weist dabei eine spezifische Filtrierbarkeit \f$ \Phi_i \leq 1 \f$ auf. 
Die filtrierbare Konzentration aller Algen\f$ A_\mathrm{Filtr} \f$ berechnet sich als

	\f[
		A_\mathrm{Filtr} = \sum_i A_i \Phi_i
	\f]
    
Die Beschreibung des Nahrungseinflusses erfolgt über eine Monod-Funktion. Dabei
beschreibt die Halbsättigungskonsten \f$ K_m \f$ diejenige Algenkonzentration, bei 
der die halbe maximal mögliche Umsatzrate errreicht wird. 
Der Nahrungseinfluss \f$ f_\mathrm{Nahr} \f$ lautet damit:

	\f[
		f_\mathrm{Nahr} = \frac{A_\mathrm{Filtr}}{K_m + A_\mathrm{Filtr} }.
	\f]


## Ertragskoeffizient
	
Die ingestierte Biomasse kann von den Rotatorien nur teilweise assimiliert werden; ein gewisser
Anteil wird wieder als Faeces ausgeschieden.  
Der Assimilationsanteil \f$ ASS \f$ nimmt mit steigender Algenkonzentration ab
und kann wie folgt beschrieben werden:
	\f[
		ASS = ASS_\max \cdot \exp \left( -\alpha_\mathrm{ASS} \cdot f_\mathrm{Beute} \right)
	\f]
Dabei ist \f$ ASS_\max \f$ der maximal mögliche Assimilationsanteil bei 20°C Wassertemperatur und \f$ \alpha_\mathrm{ASS} \f$ der Assimilationskoeffizient.

Ein Teil der assimilierten Biomasse wird zur Aufrechterhaltung des Betriebsstoffwechels und Energieversorgung benötigt. Dieser Anteil \f$RESP\f$ wird als aktive Respiration bezeichnet und wird in QSim mit einem konstanten Wert modelliert.

Der übrige Teil steht den Rotatorien für das Wachstum zur Verfügung und der Ertragskoeffizient lautet demnach:
	\f[
		\eta = ASS \cdot (1 - RESP)
	\f]



# Grundrespirationsrate # {#lnk_rot-respG}
Unabhängig von der aktiven Respiration wird zur Deckung des Grundenergiebedarfs körpereigene Substanz veratmet. Diese Grundrespirationsrate \f$ resp_G \f$ ist unabhängig von der
Nahrungsaufnahme und wird nur duch die Temperatur \f$ T \f$ beeinflusst. Der van't-Hoff'schen Regel folgend gilt
	\f[
		resp_\mathrm{G} = resp_\mathrm{G,0} \cdot Q_{10, respG} ^{
		(T-20)/10},
	\f]
wobei \f$ resp_\mathrm{G,0} \f$ die Grundrespirationsrate bei 20°C Wassertemperatur bezeichnet.
	
	
# Mortalitätsrate # {#lnk_rot-mort}
In QSim werden (bisher) keine Prädatoren für Zooplankton modelliert. Es wird lediglich eine natürliche
Absterberate in Abhängigkeit von der Temperatur und dem Sauerstoffgehalt beachtet.
 
Der Temperatureinfluss wird mit der van't-Hoff'schen Regel beschrieben und lautet
	\f[
		f_\mathrm{Temp,mort} = Q_{10,mort}^{(T-20)/10}
	\f]
	
Für die Sauerstoffabhängigkeit wird angenommen, dass oberhalb einer kritischen Sauerstoffkonzentration \f$ [O_2]_{krit} \f$ keine Beeinflussung erfolgt, unterhalb jedoch eine Erhöhung der Absterberate bis zu einem Maximalwert hin erfolgt. Der Sauerstoffeinfluss lautet
	\f[ 
		f_\mathrm{O_2} = \max \left(0,  \min \left(\frac{[O_2]}{[O_2]_\mathrm{krit}}, 1 \right)\right).
	\f]

Die Mortalitätsrate lautet damit
	\f[
		mort = mort_{\max} \cdot f_\mathrm{Temp,mort} \cdot \exp \left(
		-\alpha_\mathrm{mort} \cdot f_\mathrm{O_2}\right).
	\f]
Dabei ist \f$ mort_{\max} \f$ der maximale Absterberate bei 20°C Wassertemperatur und
\f$ \alpha_\mathrm{mort} \f$ der Mortalitätskoeffizient.
	

<hr>
aus Datei: rotatorien-equ.md; 

Code in Datei konsum_kern.f90

