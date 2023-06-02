Sediment fluxes {#lnk_sediment}
===============

\warning At the moment, this module is switched off. That is, there is no 
nutrient flux between the water column and the sediment. The text is from 
an older version of the QSim documentation.

The sediment module `sedflux()` which has been included in the water quality 
model since QSim version 12.30 (2010) onwards describes the nutrient fluxes 
between sediment and water body and the and the oxygen consumption in the 
sediment of a waterway. It is based on the the model of DITORO (2001).
According to this model, the description of a sediment flux model includes 
three sub-processes, namely the deposition of particulate organic matter 
(carbon, nitrogen and phosphorus) from the water column, 
mineralisation (diagenesis) of the deposited material, and finally the reaction 
of the resulting intermediate products in the sediment, and 
a partial flow of the substances back into the overlying water.

Furthermore the diffusion of dissolved organic compounds into the sediment and 
their oxidation influence the nutrient fluxes and oxygen consumption in the 
sediment. The sediment module integrated into QSim divides the sediment into two 
layers: an aerobic layer H1, the thickness of which is determined by the oxygen 
consumption in this layer and which is usually only a few millimetres thick, 
and an underlying anaerobic layer H2 with a constant thickness of 10 cm. 
The module is described in detail in 
BfG Report 1843 (2016, DOI: 10.5675/BfG-1843).

Given below is an example of the dissolved organic carbon compounds:

<center>
 \image html Sedi_orgC.png ""
</center>

Further details on the module are described in the following sections:

- \subpage lnk_sediment_prozesse : Explanation of the processes implemented 
   within the module

- \subpage lnk_sediment_vars : List of formula symbols and variables

- \subpage lnk_sediment_umsetzung : Details on the code and its numeric 
   implementation 


Text source: sediment-doc.md; Code sources: SedFlux.f90, sedflux_huelle.f95; 
go back: \ref lnk_weitere_stoffe or \ref index 

<!-- #todo: search for and add reference Ditoro, 2001 to the reference list -->
<!-- #todo: BfG Bericht verlinken --> 
