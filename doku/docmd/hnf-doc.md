Heterotrophic Nanoflagellates (HNF)  {#lnk_hnf}
==================================

\warning This module is currently switched off, that is, HNF are not simulated.

Heterotrophic nanoflagellates (HNF) are unicellular animal organisms with a 
cell size of 2 to 20 µm that feed on bacteria. Besides ciliates and rotifers, 
HNF can play a decisive role in shaping the zooplankton community in
large rivers (Bergfeld et al. 2009). In QSim, the microbial food web is modelled 
by examining the development of heterotrophic nanoflagellates (HNF) and their 
influence on bacteria. The chrysomonad genus Spumella was chosen as a model 
organism for HNF in QSim because it is the dominant genus in the River Rhine 
(WEITERE & ARNDT 2002, PRAST ET AL. 2003). The grazing rate of the HNF by 
metazoans and benthic filter feeders is calculated from the filtered water 
volume per time step analogous to the grazing rate of algae. The assumption is 
made that all prey organisms contained in the filtered water volume are ingested 
without a preference for certain prey organisms.

Further details on the module are described on the following sections:
- \subpage lnk_hnf_prozesse : Explanation of the processes implemented 
   within the HNF module
- \subpage lnk_hnf_vars : List of formula symbols and variables
- \subpage lnk_hnf_umsetzung : Details on the code and its numeric 
   implementation 


\n\n

Text source: hnf-doc.md ; Code source: hnf.f90

