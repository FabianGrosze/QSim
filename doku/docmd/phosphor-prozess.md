Phosphor - Prozesse {#lnk_phosphor_prozesse}
===================== 

In QSim werden der Gesamt-Phosphorgehalt und der Phosphatgehalt bilanziert.

## Teilprozesse ##
Folgende Teilprozesse wirken sich auf den Gesamt-Phosphorgehalt aus:
1. 

Folgende Teilprozesse wirken sich auf den Phosphatgehalt aus:


Die Bilanzgleichung für den Gesamt-Phosphorgehalt, \f$ \f$, lautet:

\f[ \frac{dP_{ges}}{dt} = C_{org, sed} \cdot Y_{C:P} -
                           A_{sed, i} * Q_{P,A_i}\f] 
<!-- 
            gesPt = gesP(ior)                                               &
                    - orgCsd(mstr,ior) * pl0(ior)                           &
                    - sedalk(ior) * Q_PK(ior)                               & 
                    - sedalb(ior) * Q_PB(ior)                               &
                    - sedalg(ior) * Q_PG(ior)                               &
                    + Psed                                                  &
                    - algdrk(ior) * Q_PK(ior)                               &
                    - algdrg(ior) * Q_PG(ior)                               &
                    - algdrb(ior) * Q_PB(ior)                               &    
                    - (albewg(ior) - alberg(ior)) * Qmx_PG                  &
                    - (albewk(ior) - alberk(ior)) * Qmx_PK 
-->

\f$ P_{ges} \f$:    Gesamt-Phosphorgehalt im Gewässer [mg P/L] \n 
\f$ C_{org, sed} \f$:     [ ] \n
\f$ Y_{C:P} \f$:     [ ] \n
\f$ A_{sed, i} \f$:     [ ] \n
\f$  \f$:     [ ] \n
\f$  \f$:     [ ] \n
\f$  \f$:     [ ] \n
\f$  \f$:     [ ] \n


\f[ \frac{PO4}{dt} =  \f]

\f$  \f$:     [ ] \n
\f$  \f$:     [ ] \n

 
## QSim-Veröffentlichungen, die den ...-Baustein beschreiben und/oder anwenden: 
...
 

\n\n

Textquelle: phosphor-prozess.md; Codesource: ncyc.f90; zurück: \ref lnk_phosphor