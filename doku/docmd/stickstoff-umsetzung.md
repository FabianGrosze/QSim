Nitrogen - Implementation {#lnk_stickstoff_umsetzung}
========================= 

## Origin ##

## Interface ##
see nitrogen.f90

## Nitrogen partitioning inflow {#lnk_stickstoff_aufteilung} 
Q_NK = Qmx_NK \n
Q_NG = Qmx_NG \n
Q_NB = Qmx_NB \n
nl0  *noch unklar* \n
alle anderen stickstoff-relevanten Transportkonzentrationen VNH4, VNO2, VNO3, 
VX0, VX02 und gesN werden vorgegeben.

Text source: stickstoff-umsetzung.md; Code sources: module_nitrogen.f90,
nitrifiers.f90, nitrogen.f90 und nitrogen_wrapper_3d.f95 ; 
go back to \ref lnk_stickstoff
