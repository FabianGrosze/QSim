Importing distributions of suspended matter and salt  {#lnk_schwebstoff_salz}
================================================

Some properties of the water, such as the salinity, the concentration of 
suspended sediments and also the temperature change its density so significantly 
that they can have an effect on the flow. That is why they are often taken 
into account within hydraulic simulation.

QSim3D already takes its transport information offline from an hydraulic driver.
This makes it easy to also use concentration of suspended matter and salt 
content from these drivers, if they have been calculated. For water temperature, 
this is currently not planned.

# Suspended matter concentration {#lnk_schwebstoff}
Currently, it is implemented to use the concentration of suspended matter from 
another driver: \n
The subroutine schwebstoff_salz_sichten() checks if a subfolder 
./trueb exists within the current model directory.
If none does exist, no suspended matter distributions will be read.

In the subfolder trueb, the files are searched for those starting with 'd'
(e.g. ./trueb/d86400 ).
The number behind the letter d within the filename is the time in seconds 
after the start of the simulation.
The subroutine schwebstoff_salz() then interpolates the distribution of 
suspended matter for the current simulation time step (stoffumsatz-time steps).

The suspended matter files have to be in the Elcirc .gr3 format.
This format also serves for the definition of the SCHISM simulation mesh.
At the location where the height of the knots is given within the mesh files, 
now the concentration of suspended matter has to be written in ??? mg/l ???.
That way it is possible to use the same tools for the anaylsis of the 
suspended matter and the simulation mesh (e.g. with the mesh generator Janet).

A given suspended matter concentration is read into the variable 
\ref ss and thereby represents the ??? additional ??? share of suspended 
matter.
The \ref lnk_phy_licht , which is available for the \ref lnk_phytoplankton 
to do photosynthesis, is extenuated further by other substances within the 
water.

# Salt concentration {#lnk_salz}
*Currently not implemented.*

Text source: schwebstoff_salz.f95 \n
Go back to: \ref lnk_weitere_stoffe
