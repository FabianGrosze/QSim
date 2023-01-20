Water temperature {#lnk_wtemp}
================

The water temperature results from the heat balance of a body of water. It is an
important factor for water quality, as it influences e.g. chemical reaction 
rates or growth rates of organisms and thus matter turnover processes.

In QSim, the following processes are taken into account for calculating the 
water temperature:

* [Radiation](\ref lnk_strahlung);
* [Evaporation](\ref lnk_verdunstung);
* [Convection](\ref lnk_konvektion); 
* Direct heat exchange with the sediment;
* Heating of the sediment via the radiation that penetrates the water body;
* Heating of the water body by radiation that is reflected from the sediment;
* [Tributaries and linear or point discharges.](\ref lnk_waermeeinleitung)

<!-- #mf: prüfen, dass auch tatsäächl. alle im Wtemp-Code auftauchen und verwendet werden + 
ob hier in Liste etwas fehlt -->

For the modelling the heat balance, the following meteorological data are 
needed: daily mean values of relative humidity, wind speed and cloud cover 
as well as the daily values of minimum and maximum air temperature.

Further details on the water temperature module can be found in the following 
sections: 
- \subpage lnk_wtemp_prozesse
- \subpage lnk_wtemp_vars
- \subpage lnk_wtemp_umsetzung


Text source: wtemp-doc.md ; Code sources: TEMPERW.f90, temperw_huelle.f95 ;
Go back to: \ref lnk_waerme
