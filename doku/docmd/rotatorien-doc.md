Rotifers {#lnk_rotatorien}
==========

QSim simulates rotifers instead of the total of zooplankton.

Rotifers feed on phytoplankton and thereby put grazing pressure on the 
phytoplankton in the model.

Like all organisms, rotifers cannot fully utilise their food,
so they excrete part of their food as faeces.
From the assimilated part of their food they cover their energy requirements 
for basic maintenance and growth.
maintenance and growth.
The model takes into account a natural die-off rate, which can increase when 
there is a lack of oxygen in the water.
All turnover rates are also temperature-dependent, so that an increase in the
ambient temperature leads to an acceleration of the rates.

The intrinsic motion of the rotifers is negligible compared to the flow velocity 
of the watercourse, so that in the model they are passively drifted with the 
current. 


Further Information:
- \subpage lnk_rotatorien_equ
- \subpage lnk_rotatorien_pars
- \subpage lnk_rotatorien_num


Publications:
- [Schoel et al., 2002](./pdf/Schoel_et_al_2002rhein.pdf)
- [Schoel et al., 1999](./pdf/Schoel_et_al_1999mosel-saar.pdf)


<hr>
Text source: rotatorien-doc.md ; Code source: konsum_kern.f90 ; 
go back to: \ref lnk_konsumenten


