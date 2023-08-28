Oxygen {#lnk_sauerstoff}
=========

The oxygen dissolved in the water (dissolved oxygen) serves as an 
indicator for the condition of a water body. Since oxygen is a vital factor for 
many organisms, oxygen concentration also is a proxy for the quality of a water 
body. The oxygen concentration of a water body determines the potential for 
colonization of this habitat by various organisms.

Dissolved oxygen also affects matter turnover and mass balances, e.g. of 
nutrients.  

Oxygen concentration depends on water temperature, as the physical solubility 
of oxygen in water decreases with an increase in temperature. Further components 
affect the oxygen concentration in a water body, which in QSim are balanced in a 
two-stage calculation procedure.

In every time step, an interim value for the oxygen concentration ist calculated 
as a consequence of biochemical oxygen consumption and production. \n
The following processes are included in this step:

<!-- in Liste Links einfügen via * [Wort](\ref lnk_prozess); -->
* oxygen production due to photosynthesis by diatoms, green algae and cyanobacteria
* oxygen consumption due to respiration by diatoms, green algae and cyanobacteria
* oxygen consumption due to microbial oxidation of organic matter
* oxygen consumption due to nitrification in the water body
* oxygen consumption due to respiration by zooplankton (rotifers) and benthic 
  filter feeders (Dreissena) 
<!-- früher noch in der Liste: Sauerstoffverbrauch im Sediment, Photosynthese 
durch Makrophyten, Respiration HNF; Bausteine derzeit abgeschaltet -->

In a second step, (de-)oxygenation at the water surface is calculated. This 
oxygen exchange across the water surface is calculated based on 
the temperature dependent oxygen saturation concentration and the previously 
calculated interim value of oxygen concentration in the water body.

In water bodies with weirs and weir spillways, additionally, the impact of 
these constructions is included in the calculation of dissolved oxygen.

The change in oxygen concentration due to point sources is calculated via the 
mixing of two volumes at the inflow point.

Further Information:

- \subpage lnk_sauerstoff_prozesse : Explanation of the processes implemented 
   within the module

- \subpage lnk_sauerstoff_vars : List of formula symbols and variables 

- \subpage lnk_sauerstoff_umsetzung : Details on the code and its numeric 
   implementation 

\n\n 

Text source: sauerstoff-doc.md; Code sources: module_oxygen.f90, oxygen.f90, 
oxygen_saturation_concentration.f90 and oxygen_wrapper_3d.f95; \n
go back to: \ref index
