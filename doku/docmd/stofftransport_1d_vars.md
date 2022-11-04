Stofftransport QSim1D - Formelzeichen/Variablennamen {#lnk_transport_1d_vars}
====================================================

## Liste der Formelzeichen und Variablennamen, Stofftransport QSim1d:(Stand QSim xx.yy)

| Formelzeichen/ \n Variablen-Name | Bedeutung | Einheit | Wert | Variablennamen \n Quellcode | Herkunft |
|----------------|------------|--------------|---------|---------|---------|
| _C_            | Stoffkonzentration           | µg/l |  | U, U_neu, S1, S2, S3, S4,Uneu_1 |  | 	
| \f$C'\f$       | räumliche Ableitung der Stoffkonzentration | µg/l |  | CUx |  | 
| _i_            | Gitterpunkt                  |   |  | i, ior, m1, m2, m3 |  | 
| \f$\Delta x\f$ | Länge des Querprofils        | m |  | elen,elenl, dx, dx_p1, dx_m1 |  | 
| \f$D_L\f$      | Longitudinaler Dispersionskoeffizient | m²/s |  | DL	 |  | 
| \f$Co\f$       | Courant Zahl	                |   |  | Crr, Crr1, Crr2, Crm,Crnt | 	 | 
| \f$\overline{Co}\f$ | mittlere Courant Zahl zweier benachbarter Gitterpunkt |  |  | Cour_mit |  | 
| \f$\overline{x}\f$  | mittlere Länge zweier benachbarter Gitterpunkte | m |  | dx_Stern |  | 
| \f$\Delta t\f$ | Iterationslänge              | s |  | deltat|  | 
| \f$\overline\nu\f$  | mittelere Fließgeschwindigkeit zweier benachbarter Gitterpunkte | m/s |  | vr |  | 
| \f$u^*\f$      | Schubspannungsgeschwindigkeit | m/s |  | ust |  | 
| \f$k_{st}\f$   | Rauheitsbeiwert              | m1/3/s |  | rau |  | 
| _T_            | Querschnittstiefe            | m	|  | tiefe |  | 
| \f$\nu\f$      | mittlere Fließgeschwindigkeit | m/s |  | vmitt |  | 
| _B_            | Querschnittsbreite           | m |  | Breite |  | 
| _A_            | Querschnittsfläche           | m\f$^2\f$|  | flae	 |  | 
| \f$\varepsilon_l\f$ | Transversaler Mischungskoeffizient | m²/s |  | lat_K | | 

\n\n

Herkunft:
+ x - Bilanzvariable QSim 
+ b - berechnet, Zwischengröße/Übergabevariable 
+ e - Eingabe 
+ v - Vorgabe im Quellcode gesetzt 

\n\n

Textquelle: stofftransport_1d_vars.md; Codesource: stofftransport.f90; 
zurück: \ref lnk_stofftransport_1d