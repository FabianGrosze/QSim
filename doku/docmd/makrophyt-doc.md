Macrophytes {#lnk_makrophyt}
================

\warning The module is currently switched off, i.e. makrophytes are not
simulated. This is an old documentation.

The growth of makrophytes is done in a simplified way in QSim.

For the macrophytes as a whole (no differentiation between growth forms), 
a seasonal cycle of annual biomass progression is assumed. In the settings,
seasonal start and end as well as the maximum biomass achievable under optimal
conditions (and the minimum biomass outside of the season) are entered section 
by section. The ideal seasonal pattern itself follows a simple, predefined 
scheme.

Relative to the assumed ideal course, the growth of the macrophytes is 
calculated as a function of the available light, whereby the total 
absorption of the water only has a limited effect. 
A fixed optimal light intensity is assumed for the macrophytes as a 
compensatory light intensity.

In the model, makrophytes release oxygen when growing. This oxygen release 
depends on plant biomass and light intensity as well as a temperature factor. 

A direct nutrient, turbidity or flow velocity dependency of macrophyte growth 
is not currently represented in the model. There are also no direct interactions 
with nutrient pools, sedimentation or other organisms. 

In the past it has also been possible to calculate the growth of biofilm on 
the macrophytes. 

\note The current macrophyte module is rudimentary and mainly calculates
the oxygen production by macrophytes in a simplified approach.

Details on the macrophyte module are given in the following sections:

- \subpage lnk_makrophyt_prozesse : Explanation of the processes implemented 
   within the macrophyte module

- \subpage lnk_makrophyt_vars : List of formula symbols and variables

- \subpage lnk_makrophyt_umsetzung : Details on the code and its numeric 
   implementation 

\n\n

Text source: makrophyt-doc.md ; Code sources: mphyt.f90;
go back to zu \ref lnk_ueberblick
