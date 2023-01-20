Nitrogen {#lnk_stickstoff}
=======================

The application of fertilisers, inputs via sewage treatment plants, air 
pollution as well as the degradation of drained wetlands lead to excessive 
nitrogen emissions into water bodies. Some of this nitrogen is converted 
and/or retained in the water body (retention). Different processes 
can contribute to nutrient retention in rivers and floodplains. A distinction 
is made between temporary retention and permanent removal from the ecosystem. 

In QSim, the change in total nitrogen, ammonium nitrogen, nitrite nitrogen and 
nitrate nitrogen is calculated by changes in discharges and turnover processes. 
At the beginning of each simulation, the total nitrogen concentration at  
the upper boundary and the tributaries is reduced by the inorganic nitrogen
components and by the nitrogen fractions bound in algal and zooplankton biomass. 
The remaining nitrogen is the organic fraction of detritus and dissolved organic
compounds.

For N retention, QSim currently only considers the sedimentation of organic 
material and the retention via benthic filter feeders (uptake into benthic 
biomass).
A release of dissolved N components (NH4 flux from the sediment, which leads to
negative retention values) as well as the NO3 fluxes into or out of the sediment
that are produced by benthic nitrification and/or denitrification are currently 
not taken into account. 

Total nitrogen content and inorganic nitrogen are further changed along the 
river by point sources and diffuse discharges.
Suspended nitrifiers (nitrosomonas and nitrobacter) are needed to describe the
nitrification. Their growth rate depends on water temperature, 
oxygen concentration and the supply of ammonium nitrogen (Nitrosomonas) and
nitrite nitrogen (nitrobacter). The nitrifiers' growth is inhibited by the
pH-dependent concentrations of ammonia (affecting the growth rate of the
nitrobacter) and nitrous acid (affecting the growth rate of Nitrosomonas). 
Further factors of a concentration change of both nitrifiers are sedimentation 
and mortality.


Additional processes are taken into account when balancing ammonium nitrogen. 
These include the ammonium uptake by algae and the supply of ammonium
from the decomposition of organic matter.
The oxidation of nitrite serves as a source term for nitrate nitrogen, the 
uptake by algae as sink term.

Details on the nitrogen module can be found in the following sections:

- \subpage lnk_stickstoff_prozesse : Explanation of the processes implemented 
   within the nitrogen module

- \subpage lnk_stickstoff_vars : List of formula symbols and variables 

- \subpage lnk_stickstoff_umsetzung : Details on the code and its numeric 
   implementation 

\n\n

Text source: stickstoff-doc.md ; Code source: nitrogen.f90
