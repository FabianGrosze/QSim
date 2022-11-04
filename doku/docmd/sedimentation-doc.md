Sedimentation {#lnk_sedimentation}
========

\warning Der Text stammt aus einer Vorgängerversion der Dokumentation und ist 
nicht mit dem Code gegengecheckt. Er sollte mit Vorsicht genossen werden.

Die Konzentrationsverringerung partikulärer Inhaltsstoffe im Wasserkörper durch 
Sedimentation hängt von dem turbulenzabhängigen sedimentierbaren Anteil des 
jeweiligen Stoffes und der ebenso turbulenzabhängigen Sinkgeschwindigkeit ab. 

Um das Sedimentationsverhalten beschreiben zu können, muss die 
Sinkgeschwindigkeitsverteilung der einzelnen partikulären Stoffgruppen 
(Detritus, planktische Algen, Zooplankton und Gesamtschwebstoffe) im ruhenden 
Medium z.B. aus Standversuchen bekannt sein. Eine mathematische Beschreibung 
der Verteilung lässt sich mit folgendem Ansatz erreichen:

\f{equation}{
  Q(w_s) = \frac{1}{1 + A \cdot e^{-B + log(w_s)}}
\f}

\f$Q(w_s)\f$: Summenhäufigkeit der Sinkgeschwindigkeit [-] \n
\f$A, B\f$:   Stoffspezifische Konstanten  \n
\f$w_s\f$: 	  Sinkgeschwindigkeit der jeweiligen Stoffgruppe [\f$\ms\f$] \n
\n\n

Für einen bestimmten Turbulenzzustand (Sohlschubspannungsgeschwindigkeit) lässt 
sich mit Hilfe der Gleichung \f$w_{s,Gr} = 0,625 \cdot u^{2,1}_*\f$  die 
Geschwindigkeit ermitteln, unterhalb der keine Sedimentation erfolgt 
(Grenzsinkgeschwindigkeit). 

\f$u_*\f$:	Umfangsgeschwindigkeit [\f$\ms\f$] \n
\n\n

Setzt man \f$w_{s,Gr}\f$ in Gleichung xx ein, so lässt sich der nicht 
sedimentierfähige Anteil (\f$Q_{Gr}\f$) bzw. nach Umstellen der Gleichung xx 
für \f$ \frac{1 + Q_{Gr}}{2} \f$ die mittlere Sinkgeschwindigkeit des 
sedimentierfähigen Anteils im 
ruhenden Medium bestimmen. Die Umrechnung der Sinkgeschwindigkeit in ruhendem 
Medium auf die Sinkgeschwindigkeit bei Turbulenz (wst) erfolgt mit folgendem 
Ansatz:

\f{equation}{
  w_{st} = w_s \cdot 1,14 \cdot e^{-188 \cdot u_*}
\f}

Schließlich errechnet sich die Sedimentationsrate der jeweiligen Stoffgruppe zu: 

\f{equation}{
  C_{sed} = C_0 \cdot \alpha_{sed} \cdot Q_{Gr} \cdot \left(1 - 
   \frac{1}{e^{\frac{P_{sed} \cdot w_{st} \cdot \Delta t}{H}}} \right)
\f}

\f$C_0\f$:	Konzentration der jeweiligen Stoffgruppe [g*m-3] \n
\f$C_{sed}\f$:	Sedimentationsrate der jeweiligen Stoffgruppe [g*m-3*d-1] \n
\f$\alpha_{sed}\f$:	sedimentierfähiger Anteil in ruhendem Medium [-] \n
\f$P_{sed}\f$:	Proportionalitätsfaktor [-] \n
\f$H\f$:	Wassertiefe [m] \n
\n\n

Bei der Berechnung der Sinkgeschwindigkeiten wird keine Temperaturabhängigkeit 
berücksichtigt.


\n\n

Textquelle: sedimentation-doc.md ; Codesource: Sedimentation.f90

