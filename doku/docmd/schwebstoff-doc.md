Schwebstoff {#lnk_schweb}
=================

\warning Der Text stammt aus einer früheren Version der Dokumentation und 
wurde nicht mit dem Code gegengecheckt

Im Modell wird der Schwebstoffanteil, der durch die lebende organische Substanz 
verursacht wird, in den jeweiligen Bausteinen berücksichtigt. Deshalb muss der 
Schwebstoffgehalt an den Modellrändern zu Beginn der Simulation um diesen Anteil 
verringert werden. Der Gesamtschwebstoffgehalt am Ende des Zeitschritts 
errechnet sich aus den anorganischen Schwebstoffen, dem Detritus und der Algen- 
und Rotatorienbiomasse.

Die Massenbilanzgleichung für die anorganischen Schwebstoffe sowie für den 
Detritus lautet:

\f{equation}{
 \begin{split}
 \frac{dSS}{dt} = up_{ROT} \cdot (1 - ass_{Rot}) \cdot 
 ROT + m_{ROT} \cdot ROT + \sum_{j=1}^3{m_{A,j} \cdot A_j} + \\
 \Delta SS_{Corg} + (1 - ass_{DR}) \cdot DR - SS_{sed} 
 \end{split}
\f}

\f$SS\f$:	Gehalt an anorganischen Schwebstoffen und Detritus [g*m-3] \n
\f$\Delta SS_{Corg}\f$	Änderung des Gehalts an partikulären organischen Kohlenstoffverbindungen
	bei der Hydrolyse [g*m-3*d-1] \n
\f$ass_{DR}\f$	Assimilationsanteil der durch Dreissena aufgenommenen Nahrung [-] \n
\f$DR\f$	Biomasse einer einzelnen Dreissena [g] \n
\f$SS_{sed}\f$: sedimentierte Menge an anorganischen Schwebstoffen und an Detritus
	[g*m-3*d-1] \n

Der Gesamtschwebstoffgehalt ergibt sich dann aus:
 
\f{equation}{SS_G = SS + A_j + ROT \f}

## Schwebstoff aus UnTRIM²/SediMorph
Es ist möglich, in QSim3D den in UnTRIM²/SediMorph berechneten Schwebstoff zu 
verwenden. Eine Anleitung dazu findet sich unter \subpage lnk_SPM_UnTRIM2 .

aus Datei schwebstoff-doc.md ;
zurück: \ref lnk_ueberblick