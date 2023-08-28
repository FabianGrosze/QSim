Troubleshooting {#lnk_troubleshoot}
==================
<!-- in the long run, it would be good to have this in a forum,
for now, it will be good to use RocketChat and maybe a word doc? 
or an e-mail to qsim@bafg.de? -->

# GERRIS

When trying to solve problems, there are several places where to take a look:
- simulation protocol: right click on an event > Protokoll (= protocol)
- log files: in the simulation directory you will find several log files 
  (Preproc.log, QSim.log, QSimConsole.log)
- Gerris checks for some common mistakes and flags errors and warnings with a 
  red exclamation mark. Make sure to check and whereever possible, solve all 
  these warnigns
- Model editor warnings: in the model editor, warnings are shown at the bottom 
  of the window

## Paths
- Have you set all the path correctly (e.g. the path to the QSim executable, 
  which can be set globally and specifically for one event)
- check that the software versions fit to each other. The best is to use the 
  latest (stable) software versions of each program (HYDRAX, QSim, Gerris)
- Check for missing files (e.g. e_extnct.dat on your local disc)
 

# Input data
- check that your data are within the common min/max range; the file 
  AParamParam.xml may give you an indication of common ranges for QSim 
  parameters
- check that your QSim parameter values fit to your QSim version. The current 
  version of QSim parameter values can be found here:
  `z:\Projekte\QSim\Parameter_und_Umrechnungen\Parameterlisten\` 
  <!-- we should include a current list within the doc. portal, best would be 
   to have one that is automatically created/read + under the link on Z there 
   are so many lists that it will be hard to find the right one -->
- make sure to read the section on input data in the 
 [Gerris Handbook]((pdf/Anleitung_Gerris_extern_Stand-2021-04-15.pdf))
- check that there are no "missing values" identifiers in your data set, as e.g.
  NA or NaN
- meteorology: 
	- make sure that the unit of your global radiation data is correct 
	- be sure if you use daily values or timed values/time step values and that 
	  you use the right setting for your values (the easiest is to start with 
	  daily values)
- water quality data: do all variables have a value at the upper boundary (e.g.
   the algal composition)
- is the ratio between variables in an OK range (e.g. CBSB:CSB)   

# Model set up
- Make sure to use a correct transport scheme. We recommend QUICKEST + Li or 
  Deng
- Check which equation you have set for evaporation (Sweers is the default)
- Check which section settings ("Strangoptionen") are set to active/non-active.
  You can get a quick overview (right click > "Eingangsdaten bearbeiten" > 
  "Strangoptionen", then on the left "Alle"
   - especially check this for the weather stations: does every stretch have a 
     weather station assigned to it?!

# Simulation general 
- check that you have used the right MQ event in HYDRAX. Maybe you need to 
  re-run it

Text source: troubleshoot.md