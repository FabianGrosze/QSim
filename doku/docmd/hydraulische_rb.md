Datenfelder offline: Hydraulik-Randbedingungen {#lnk_hydraul_rb}
=============================================

Die hydraulischen Randbedingungen werden als \ref lnk_transport_numerik offline 
von holen_trans() eingelesen/berechnet.

Die QSim-3D Nummern beziehen sich auf das Datenfeld rb_hydraul, 
resp. rb_hydraul_p

Die QSim-1D Namen werden in QSim3D im module_QSimDatenfelder.f95 vereinbart.


| Nr. QSim-3D | Name QSim-1D | Beschreibung | Dimension | Wertebereich |
| ----------- | ------------ | ------------ | --------- | ------------ |
| 1 | \anchor vmitt vmitt | Geschwindigkeitsbetrag | m/s     | 0,0 ... 3,0 |
| 2 | \anchor tiefe tiefe | Wassertiefe            | m       | 0,0 ...     |
| 3 | \anchor wsp wsp     | Wasserspiegellage      | m ü NHN |             |

Textquelle: hydraulische_rb.md ; Codesources: module_modell.f95 ;  
zurück: \ref lnk_transport_numerik
 