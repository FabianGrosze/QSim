Chelicorophium {#lnk_corophium}
==============

\warning This module is currently switched off. That is, Corophium is not 
simulated. This is an old documentation.

In QSim, the removal of phytoplankton by benthic filter feeders is done in the 
Dreissena and chelicorophium modules. Both species, the zebra mussel 
Dreissena polymorpha and the mud crab chelicorophium curvispinum, are so-called 
active filter feeders, which actively swirl the fine organic material 
and filter out fine and very fine particulate organic matter and thus also the 
planktonic algae (floating in the water) from the water body. 
Both benthic filter feeders are neozoans that reach very high densities in 
running waters and can dominate the benthic community. The 
modelling of benthic filter feeders in QSim is not intended to describe the 
development of benthic filter feeders in a water body, but to quantify their 
feeding pressure on the phytoplankton.

In contrast to the zebra mussel, the population development of the 
mud crab is modelled using simple assumptions based on Rajagopal et. 
al. (1999) (\cite Rajagopal_1999). Only their filtration rate is 
calculated over the year. In other modules such as the oxygen 
or carbon module, chelicorophium is not included.

Three generations are mapped during one annual cycle, the growth of 
of chelicorophium depends only on its initial individual density at the start 
of the model. No other influencing factors such as water temperature or food 
availability were implemented in the module. The loss rates are also 
unaffected by external factors. For the simulation runs 
the density of individuals for chelicorophium is entered for the 01.01. of 
each year. The density is differentiated into embankment and channel bed. 
When entering the values, the modelled water body can be divided into different 
sections in order to set the distribution of the chelicorophium abundances for 
embankment and channel bed along the river.

The start and subsequent generations die off at different times, 
the last generation forms the basis for the next year's start generation. 
year. Due to the high dominance of chelicorophium in the Danube river, the 
module was added within a EU study on the Danube (BfG-1740, 2013) in order to 
reproduce the algal development with QSim.

Further information:
- \subpage lnk_corophium_prozesse
- \subpage lnk_corophium_vars
- \subpage lnk_corophium_umsetzung


![_Chelicorophium curvispinum von Ralf Rombach, BfG/U4..](img/Corophium_curvispinum_10_RalfRombach_BfGU4.jpg)
\n\n


Text source: corophium-doc.md ; Code source: coroph.f90 ; 
go back to: \ref lnk_konsumenten oder \ref lnk_ueberblick
