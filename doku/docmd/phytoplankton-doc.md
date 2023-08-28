Phytoplankton {#lnk_phytoplankton}
================

Phytoplankton are the algae suspended in the water, which are of particular 
importance as the most important primary producers in the water body. Algae 
have the ability to perform oxygen-forming photosynthesis, in which 
carbon dioxide acts as a carbon source, light as a source of energy and 
water as an electron donor (reducing agent). Photosynthesis can be 
simplified with the following elemental formula:

\f[
 6\,\text{CO}_2 + 6\,\text{H}_2\text{O} \rightarrow \text{C}_6\text{H}_{12}\text{O}_6 + 6\,\text{O}_2 \quad \text{--2902 kJ}
\f]

Primary producers are at the beginning of the food chain and form the nutrional 
basis for planktonic and benthic herbivorous animals and for the bacteria and 
fungi involved in biochemical degradation (destructors). They also are essential
suppliers of oxygen, which is vital for most organisms. 
Biomass and growth of phytoplankton in flowing waters play an important role for  
the food web and its representation in the model. 
This can be associated with a significant proportion of the primary production 
and thus the energy and material flows. 
<a href="https://www.gewaesser-bewertung.de/index.php?article_id=460&clang=0}{www.gewaesser-bewertung.de"> Website: Typologie nach WRRL, 03.12.2021</a>. 
Phytoplanktonr-rich water especially are water bodies with a large catchment, 
and especially slow flowing and/or regulated rivers.

Only part of the visible radiation can be used for photosynthesis by 
phytoplankton. The proportion of usable light in the model is derived from 
global radiation. The calculation of the underwater light climate is often an
essential part of the phytoplankton component of quality models. 
For the sake of simplicity, a dependency of the light spectrum on water depth 
is not (any longer) taken into account in the present model verison. 

In QSim, photosynthesis rate and algal growth rate depend on temperature, 
light intensity and nutrient supply. To simplify matters growth rate is assumed 
to be proportional to photosynthesis rate in the model. Depending on the light
intensity, the photosynthetic rate (_P-I_ curve) in the model is separated into 
three areas. In the light-limited area, the photosynthetic rate increases almost
linearly with increasing light intensity. In the light-saturated range, it 
converges towards its maximum, and at high light intensities the photosynthetic 
rate decreases again (light inhibition or photoinhibition). The described 
correlation of light dependence of growth is mapped specifically for each algae 
group in the model.

It is further assumed in the model that the most abundant elements in the algal 
biomas, that is, carbon (C), oxygen (O\f$_2\f$) and hydrogen (H) are unlimited, 
whereas the nutrients nitrogen (N), phosphorus (P) and, in the case of diatoms, silicon (Si) can limit growth. 

For their metabolism, phytoplankton obtains energy from the decomposition of 
organic carbon compounds, which consumes oxygen. The model distinguishes between 
basic respiration and light-dependent respiration.

In addition to respiration, algae in the model experience losses by 
zooplankton and zoobenthic organisms grazing (*Dreissena*, *Corophium*), 
sedimentation and mortality due to a lack of resources and lethal physical or 
chemical environmental conditions.

When describing photosynthesis, we refer to gross photosynthesis as the total 
of primary carbon fixation. Net photosynthesis is the gross photosynthesis 
minus internal respiratory losses and mortality.
<!-- (Wirklich inklusive der Mortalität? Das höre ich so zum ersten 
Mal.) -->
Whereas external losses from other modules are neglected.

QSim separated three algal classes. In the large German rivers, the most 
relevant algal groups are: Chlorophyceae (green algae), bacillariophyceae 
(diatoms) and cyanophyceae (blue-green algae or cyanobacteria).

To quantify phytoplankton, chlorophyll-a is used, since it is commonly used 
and relatively easy to measure. The percentage algal group composition (related 
to Chl-a) can be determined by direct measurements (probes) or by 
conversion of microscopic-quantitative data. 

Details on the phytoplankton module are given in the following sections:

- \subpage lnk_phyto_prozesse : Explanation of the processes implemented 
   within the phytoplankton module

- \subpage lnk_phyto_vars : List of formula symbols and variables

- \subpage lnk_phyto_umsetzung : Details on the code and its numeric 
   implementation 

\n\n

Text source: phytoplankton-doc.md ; Code sources: albenth.f90, algaesbl.f90,
algaesgr.f90
go back to: \ref lnk_primaer oder \ref lnk_ueberblick
