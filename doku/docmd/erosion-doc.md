Erosion {#lnk_erosion}
================

Erosion occurs if the bottom friction exceeds a critical value \f$\tau_{crit}\f$

Bottom friction:
\f{equation}{
  \tau =\frac{\rho_S \cdot v^2 \cdot g}{(K_{St})^2  \cdot h^{1/3} }
\f}
with: \n
\f$ \tau\f$:  bottom friction [N/m²] \n
\f$\rho_S\f$: Sediment density [kg/m³] \n
\f$h\f$: water depth [m] \n
\f$v\f$: velocity (depth averaged, absolute value) [m/s] \n
\f$K_{St}\f$: friction coefficient (Gauckler/Manning/Strickler, invers of Mannings's n) [m**(1/3)/s] \n


Erosion mass flux:
\f{equation}{
  M = M_{eros} \cdot (\frac{\tau - \tau_{crit}}{\tau_{crit}})^{n_{eros}}
\f}
with: \n
\f$ M \f$:  erosion mass flux [Kg/m²] \n
\f$ M_{eros} \f$: erodibility Parameter (empirical) [Kg/m²] \n
\f$ \tau_{crit}\f$: critical bottom friction (empirical) [N/m²] \n
\f$n_{eros}\f$: erodibility exponent (empirical) [-] \n

\n\n

Codesource: erosion_kern.f90 ; 
Text source: erosion-doc.md ;
go back to: \ref lnk_erosion_sedimentation 