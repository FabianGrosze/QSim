Suspended sediment {#lnk_schweb}
=================

\warning This text is from an older version of the documentation. The content 
has not been checked with the current code

In the model, the share of suspended matter caused by the living organic matter  
is taken into account in the respective modules. Therefore, 
the suspended matter content at the model boundaries has to be reduced by this 
fraction at the beginning of the simulation. 
The concentration of total suspended matter at the end of each time step 
is calculated from the concentrations of inorganic suspended matter, detritus, 
algal and rotifer biomass. 

The mass balance for inorganic suspended matter and for detritus is 
calculated as:

\f{equation}{
 \begin{split}
 \frac{dSS}{dt} = up_{ROT} \cdot (1 - ass_{Rot}) \cdot 
 ROT + m_{ROT} \cdot ROT + \sum_{j=1}^3{m_{A,j} \cdot A_j} + \\
 \Delta SS_{Corg} + (1 - ass_{DR}) \cdot DR - SS_{sed} 
 \end{split}
\f}

\f$SS\f$: Concentration of inorganic suspended matter and detritus [\f$ \gmq \f$] \n
\f$\Delta SS_{Corg}\f$	Change in the concentration of particulate organic 
  matter due to hydrolysis [\f$\gmqd\f$] \n
\f$ass_{DR}\f$ Assmiliation share of food taken up by Dreissena [-] \n
\f$DR\f$ Biomass of a single *Dreissena* [\f$g\f$] \n
\f$SS_{sed}\f$: Deposited amount of inorganic suspended matter and detritus [\f$\gmqd\f$] \n

The total suspended matter concentration then is:
 
\f{equation}{SS_G = SS + A_j + ROT \f}

## Suspended matter from UnTRIM²/SediMorph
Within QSim3D, it is possibel to use the suspended matter concentration 
calculated by UnTRIM²/SediMorph. An instruction to do this can be found 
[here](\subpage lnk_SPM_UnTRIM2) .

Text source schwebstoff-doc.md ; \n
Go back to: \ref lnk_ueberblick