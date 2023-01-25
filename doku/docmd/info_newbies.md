Information for people new to QSim {#lnk_newbies}
===================================

# Written information on QSim
You can get some first information from the 
[BfG website](https://www.bafg.de/DE/08_Ref/U2/QSIM/qsim_node.html) and from the
[QSim information sheet from 2018](https://doi.bafg.de/BfG/2015/BfG_QSim.pdf), 
which is available in German and English.

An in-depth documentation will be available in this documentation portal.
Currently, this portal is only available internally. 

# QSim Code
Changes in the QSim code are tracked in the version control application git.
The [QSim repository](https://gitlab.lan.bafg.de/qsim/) is hosted online on a 
GitLab instance of the BfG. To get invited to the repository, please contact 
[Jens Wyrwa](http://www.bafg.de/DE/08_Ref/U2/05_Mitarbeiter/wyrwa_j/wyrwa_node.html).
So called "issues" (bugs and wishes to fix/extend/change QSim) are tracked in 
the [issue list](https://gitlab.lan.bafg.de/qsim/qsim/-/issues?sort=created_date&state=opened) 
on GitLab. On the main page of the repository, you'll find 
information on the latest changes of the code (see section readme.md). Be 
careful, because there are several development branches on the repository.
The version that you will use in the beginning is the "main" branch, which 
can be selected in a drop-down menu.

# Executables
The different compiled versions and thus executable QSim software are located 
in this directory: `z:\Projekte\QSim\Versionen_u_Setups\QSim\`
(currently main14.08, best to chose "_ohnesedflux").

To run QSim, it is necessary to use a hydrodynamic model with it.
For QSim1D, we use the BfG-owned model HYDRAX. The documentation of HYDRAX 
can be found [here](https://doi.bafg.de/BfG/2021/HYDRAX2021.pdf).
The latest version of HYDRAX and its source code can be found here:
`z:\Projekte\QSim\Versionen_u_Setups\Hydrax_mit_Code\Hydrax_2020_version527_mit Code\`

Gerris is a graphical user interface, which integrates the BfG models HYDRAX and 
QSim. The documentation of Gerris (Anleitung Gerris.docx) is located here:
`Z:\Projekte\QSim\Dokumentation_und_Handbuecher\Gerris`. The latest version of 
Gerris is located here: `Z:\Projekte\QSim\Versionen_u_Setups\Gerris`. 
The Gerris-zip-files include HYDRAX, but you should carefully check the version 
of QSim and replace it by the latest version if necessary. 

# Tutorial/Video
We have recorded a lecture on first steps with the model chain 
Gerris/HYDRAX/QSim: 
`Z:\Projekte\QSim\Dokumentation_und_Handbuecher\Gerris\2020-07-28 GERRIS Schulung`.

# Rights and access
Overview of rights/access that you may need when working with QSim:
- GitLab (see above)
- Funktionspostfach QSim-Support@bafg.de 
- Funktionspostfach qsim@bafg.de
- `Z:/Projekte/QSim`
- Rocket chat groups: QSim, QSim-Code, HPC, rrrr, R-Fragen-U2

# Who is responsible?
The following table lists who is responsible for what within the QSim group: \n
*tbd* <!-- Link zu unserer Tabelle aus QSim-Intern? -->

# QSim and R
- Within U2 and G1, we have an active R-group that is, among others, 
  developing R-scripts around QSim. An overview and notes of the meetings can be 
  found here: `Z:\U\U2\R-Werkzeuge\Skripte\QSim` and in the 
  [U2-Wiki](http://voss-wiki.bafg.de/instanzen/u2wiki/doku.php?id=start) 
  (*to be uploaded*). Some of the packages that are being developed are hosted on 
  [GitLab](https://gitlab.lan.bafg.de/qsim), too. These include a package to run 
  QSim from r (rQSim), a package and a shiny app to validate QSim results
  (QSimValidatoR). If you would like to contribute to the packages or use them,
  best write into one of the RocketChat channels (QSim or R-Fragen-U2)	

# QSim Wikis
We also have two wikis related to QSim:
- The [validation wiki](http://voss-wiki.bafg.de/instanzen/qsim_validierung/),
  which compiles test runs of QSim that are done before the release of new 
  QSim executables
- A wiki for the [model applications in the estuary](http://voss-wiki.bafg.de/instanzen/modellwiki/?animal=modellwiki)
  This wiki serves to document decisions made when setting up model instances,
  e.g. when compiling input data or when compiling the model results.

Text source: info_newbies.md