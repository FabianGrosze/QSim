Overview  {#lnk_ueberblick}
=========
 
 
What is simulated?
-------------------

<!-- In der Liste könnte man auch auf die Subpages verlinken
und die Subpages entsprechend anordnen...-->
QSim uses modules to simulate the individual mass transfer processes that 
take place in flowing water. These are:
- The module for the \ref lnk_waerme,
- 6 bio-chemical modules to describe \ref lnk_oxygen,  
  \ref lnk_ph and \ref lnk_orgC, and the \ref lnk_nutrients \ref lnk_nitrogen, 
  \ref lnk_phosphor und \ref lnk_silikat.
- 1 biological module for the simulation of \ref lnk_primaer : 
  [pelagic phytoplankton (algae)](\ref lnk_phytoplankton); \n
  [benthic algae](\ref lnk_albenth) and 
  [macrophytes (aquatic plants)](\ref lnk_makrophyt) are currently switched off.
- 2 biological modules for recording the first trophic level 
  [\ref lnk_consumers](\ref lnk_consumers) : [zooplankton](\ref lnk_rotatoria) 
  as well as [benthic filter feeders (mussels)](\ref lnk_dreissena)
- as well as further modules for: \ref lnk_heavy_metals , 
  \ref lnk_float and \ref lnk_stay time.
  
The [sediment module](\ref lnk_sediment), which calculates early diagenetic 
processes that cause oxygen, carbon and nutrient fluxes, is currently switched 
off.  

Other modules that are currently switched off are \ref lnk_coliform,
\ref lnk_hnf and \ref lnk_corophium .

A short overview of the biogeochemical modules can be found in
<a href="http://bibliothek.bafg.de/webopac/index.asp?detsuche_systematik=online+492" target="_blank">Schöl et al. 2014</a>,
more detailed information is available in the sub-chapters of the individual 
modules.

The simulation of the advective and diffusive transport of matter is 
described in the section [matter transport](\ref lnk_stofftransport_3d).
Also [conservative tracers](\ref lnk_tracer) can be simulated with QSim.

You can find a brief introduction to the water quality simulation with 
QSim in German on the
<a href="http://www.bafg.de/DE/08_Ref/U2/01_mikrobiologie/QSIM/qsim_node.html" target="_blank">
website of the Federal Institute of Hydrology</a>, under 
<a href="http://bibliothek.bafg.de/webopac/index.asp?detsuche_systematik=online+321" target="_blank">
QSim Übersicht</a>.

QSim is the core for simulating water quality.
It is integrated into a software environment which also includes hydraulic 
drivers, the graphical user interface Gerris and others: 
\subpage lnk_qsim_aufbau .

If you want to work with QSim yourself, this section will help you
\ref lnk_download continue.

The following chapter \subpage lnk_doxygen_info gives a few tips on how
to read and use this documentation portal.

The history of QSim development is given in the chapter 
\subpage lnk_geschichte_qsim


Text source: ueberblick.md ; go back to <a href="index.html">main</a>
Text source: ueberblick.md ; go back to: \ref index
 