Heat budget {#lnk_waerme}
=============

![](Waermehaushalts.png "")

The heat balance of a body of water determines the temperature distribution 
within the water body.

To calculate the heat balance, the following local heat inputs and outputs 
(heat flows) are implemented in the model:

- Shortwave radiation <br> 
(*incident solar radiation, light*)
- Longwave radiation <br> 
(*outgoing longwave radiation and ingoing longwave radiation*)
- Evaporation/condensation <br> 
(* depending on the vapor pressure difference between the water surface and 
the overlying air, water evaporates from the water body into the air or 
condenses from the air into the water body. In the process, either heat of 
evaporation is removed from the water or heat of condensation is added.*)
- Convection <br>
(*Heat conductivity between the water surface and the air*)
- Thermal conduction sediment <br>
(*Heating of the sediment by incident radiation or loss of heat from the 
sediment to the water body*)
- Inflows/discharge <br>
(*Natural inflows and anthropogenic heat discharges as linear or point sources*)

Heat transfer into the body of water, for example through biological processes
(respiration) or frictional heat is very small and is neglected.
That is, other than the above listed processes are assumed as irrelevant for 
the heat balance.

Input and export of heat into the water is a result of the 
\ref lnk_wetter_rb and the [inflow boundary data](\ref lnk_boundary conditions).
<!-- #mf: check if link to inflow boundary data (inflow RBs) is correct -->

The following subchapters describe the calculation of 
- \subpage lnk_strahlung and
- \subpage lnk_wtemp


Text source: waerme-doc.md ; code sources: temperw.f90, temperw_huelle.f95  ; 
go back: \ref lnk_ueberblick
