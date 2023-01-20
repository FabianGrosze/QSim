Phosphorus {#lnk_phosphor}
=======================

The application of fertilisers, inputs via sewage treatment plants, air 
pollution as well as the degradation of drained wetlands lead to excessive 
phosphorus emissions into water bodies. Some of this phosphorus is converted 
and/or retained in the water body (retention). Different processes 
can contribute to nutrient retention in rivers and floodplains. 

The plant nutrient phosphorus occurs in the water body as inorganic or 
organically bound particulate or dissolved phosphorus. 

In the model, a distinction is made between total phosphorus (TP, sum of 
dissolved and particulate phosphorus) and dissolved reactive phosphate (SRP, 
for "soluble reactive phosphorus"). 

TP and SRP enter the watercourse via tributaries and discharges at the model 
boundaries.

The change in total phosphorus takes place via sedimentation of particulate 
bound phosphorus (algae and detritus). The exchange of inorganic phosphate 
(PO4-P) with the sediment is currently switched off. 

While SRP immediately is available to algae, this is not the case for 
organically bound phosphorus, which is only available after bacterial 
degradation (respiration of heterotrophic bacteria). 
In the model, decomposition of algal and rotatoria biomass releases SRP. The
amount of released phosphorus is calculated from the respired biomasses and the
phosphorus content of algae and detritus.

Details on the phosphorus module are given in the following sections:

- \subpage lnk_phosphor_prozesse : Explanation of the processes implemented 
   within the phosphorus module

- \subpage lnk_phosphor_vars : List of formula symbols and variables  

- \subpage lnk_phosphor_umsetzung : Details on the code and its numeric 
   implementation

\n\n

Text source: phosphor-doc.md ; Code source: ncyc.f90 
