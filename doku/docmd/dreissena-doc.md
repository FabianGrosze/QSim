Dreissena {#lnk_dreissena}
==========

In QSim, the removal of phytoplankton by benthic filter feeders is implemented 
in the Dreissena module. The zebra mussel *Dreissena polymorpha* is a so-called 
active filter-feeder, the species actively swirls fine organic material and 
and filters fine and very fine particulate organic matter, and thus also the 
planktic algae (floating in the water) from the water body. 
*Dreissena polymorpha* is a neozoan that ca reach very high densities in running 
waters and dominate the benthic community. 
The modelling of benthic filter feeders in QSim does not serve to describe the 
development of benthic filter feeders in a water body, but to quantify their 
feeding pressure on phytoplankton and suspended matter.

The population development of Dreissena has been modelled in QSim based on the 
literature study by Seredszus (1998). Two generations are modelled with a 
0. and 1. cohort. The population development depends on temperature, food and  
spawning processes. In addition, there is a weight-dependent mortality rate of 
dreissena. In the simulation runs, at the start of the model, the following 
values are entered for the 01.01.: biomass (g/m²) of the mussels and the mean 
weight of a mussel (mg C) both for the embarkement and the channel bed. 
The modelled water body can be subdivided into different sections in order to 
distribution of mussel abundance for embarkement and channel bed along the 
river. 

The 0. cohort includes all animals with a mean shell length up to 8 mm 
and the 1. cohort includes all animals with a mean length greater than 8 mm. 
The adult animals (1st cohort) produce larvae, which form the 0th cohort after 
becoming entranched at the river bed. The 0. cohort is of importance at the 
earliest four weeks after the first spawning date, i.e. from the end of June 
each year. Thus, the density of individuals and the associated feeding pressure 
of the zebra mussel increases over the year. 
The different cohorts were introduced in order to simulate the size-dependent 
filtration performance of the mussels. The Dreissena module was 
developed to map algal development along the Moselle in 1994 with QSim 
(Schöl et al. 1999). 

![<i>Dreissena polymorpha</i>](img/Dreissena_sp5_RalfRombach_BfGU4.jpg)

<small>Quelle: Photo by Ralf Rombach/U4</small>
\n\n

Further information:
- \subpage lnk_dreissena_prozesse
- \subpage lnk_dreissena_vars
- \subpage lnk_dreissena_umsetzung


<hr>
Text source: dreissena-doc.md ; Code source: dreissen.f90 ; 
go back to: \ref lnk_konsumenten
