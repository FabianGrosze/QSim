Mass transport  {#lnk_stofftransport}
=================

Substances dissolved in flowing waters as well as planktonic organisms, which 
move negligibly in relation to the surrounding water, are 
transported with the flow velocity. This leads not only to convective 
displacement, but also to dispersive mixing. 
These are based on a series of physical effects and lead to the fact that the 
concentration is reduced with increasing flow distance, but at the same time 
that the volume of water mixed with substances increases (Figure 1). 
Depending on the type of substance involved, the water quality can be affected
significantly, but also the natural functions of aquatic ecosystems. 
Accordingly, the transport of substances in water bodies is a relevant key 
process, which is implemented in QSim in the "transport module". 

By solving the 1D advection-dispersion equation, the concentration changes 
averaged over the cross-section are calculated. For the calculation of
the advection fraction, one of three different numerical methods can be selected
(Cubic interpolated Pseudo-Particle - CIP, Lax-Wendroff method and Quadratic 
Upstream Interpolation for Convective Kinematics with Estimated Upstream Terms 
- QUICKEST), while the dispersion fraction can be determined with the 
Crank-Nicolson or McCormack method. For the selection of the dispersion 
coefficient, four equations are available. In addition, in waterways 
characterised by jetties (*Buhne*), the possibility exists of taking lateral 
exchange processes into account in order to reflect the influence of the 
stillwater zones on the concentration of matter. 

![Schematic representation of the advective and dispersive processes that affect matter transport.](img/transport_schema_adv_disp.png)

Details on matter transport are (currently) described in separate pages for 1d
and 3d:

- \subpage lnk_stofftransport_1d
with
  - \subpage lnk_transport_1d_vars
  - \subpage lnk_transport_1d_umsetzung
- \subpage lnk_stofftransport_3d

Text source: stofftransport.md ; Code sources: stofftransport.f95 ;  \n
Go back to: \ref index
 
