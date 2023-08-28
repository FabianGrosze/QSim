Organic carbon {#lnk_orgC}
=================

The total organic carbon (TOC) content in water consists of dissolved (DOC) 
and particulate (POC) components. POC is further differentiated into living 
biomass and non-living organic matter (= detritus). 

The division of the organic fractions and their interconnection in QSim is 
implemented in the organic carbon module. As carbon fractions there are 
particulate poorly and easily degradable, dissolved poorly and easily degradable 
and monomeric and refractory carbon compounds (BILLEN 1991). The 
refractory carbon compounds are only a carbon sink. 
To complete the overall balance of POC, the living biomass is included, 
which is calculated in the primary and secondary production modules.
<!-- #mf: Referenz für Billen fehlt -->

When calculating concentration changes in the individual fractions,
excluding monomeric fractions, dead algal and rotifer biomass as well as 
rotifer and dreissena faeces are sources. They are distributed among the 
carbon fractions with a fixed ratio.
Sedimentation is a sink for the detritus.

Carbon is transformed to the dissolved fractions by hydrolysis of the 
particulate fractions. The dissolved fraction is then transformed 
to the monomeric fraction by hydrolysis and by exoenzymatic activity. 
The exoenzymatic activity is coupled to the concentration of the 
bacterial biomass, because monomeric substances are the substrate of 
heterotrophic bacterial respiration. 
The conversion of monomeric carbon is carried out by bacteria, which procude 
biomass and carbon dioxide while consuming oxygen. That is, in this approach, 
the bacterial biomass (BM) is explicitly calculated. From the terminal step 
the biochemical oxygen demand (BOD) is derived.  

<center> 
 \image html orgc.klein.png ""
 <a href="./img/orgc.ppt" target="_blank">Download des Schaubilds als .ppt</a>
 \image latex orgc.png "Bilanz des organischen Kohlenstoffs" width=0.95\textwidth
</center>
<!-- #mf: Frage an Andreas & Tanja: passt das Bild? Bzw. TExt und Bild gegenchecken -->

Details on the carbon module can be found in the following sections:

- \subpage lnk_orgC_prozesse : Explanation of the processes implemented within 
   the carbon module 

- \subpage lnk_orgC_vars : List of formula symbols and variables 

- \subpage lnk_orgc_umsetzung : Details on the code and its numeric 
   implementation 

\n\n

Text source: kohlenstoff-doc.md ; Code sources: module_organic_carbon.f90, 
organic_carbon.f90 and organic_carbon_wrapper_3d.f95 
