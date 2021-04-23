!---------------------------------------------------------------------------------------
!
!   QSim - Programm zur Simulation der Wasserqualität
!
!   Copyright (C) 2020 Bundesanstalt für Gewässerkunde, Koblenz, Deutschland, http://www.bafg.de
!
!   Dieses Programm ist freie Software. Sie können es unter den Bedingungen der 
!   GNU General Public License, Version 3,
!   wie von der Free Software Foundation veröffentlicht, weitergeben und/oder modifizieren. 
!   Die Veröffentlichung dieses Programms erfolgt in der Hoffnung, daß es Ihnen von Nutzen sein wird, 
!   aber OHNE IRGENDEINE GARANTIE, sogar ohne die implizite Garantie der MARKTREIFE oder der VERWENDBARKEIT FÜR EINEN BESTIMMTEN ZWECK. 
!   Details finden Sie in der GNU General Public License.
!   Sie sollten ein Exemplar der GNU General Public License zusammen mit diesem Programm erhalten haben. 
!   Falls nicht, siehe http://www.gnu.org/licenses/.  
!   
!	Programmiert von:
!	1979 bis 2018 Volker Kirchesch
!	seit 2011 Jens Wyrwa, Wyrwa@bafg.de
!
!--------------------------------------------------------------------------------------------------------------

!> Hauptprogramm QSim3D 
!! Beschreibung siehe:\ref index \n
      Program QSim3D 
!
      use netcdf
      use modell                                                   
      use QSimDatenfelder
      !use netcdf
      !use mpi                                               
      implicit none
      include 'netcdf.inc'
      integer :: i,ni,j,k,n, system_error
      character(300) systemaufruf
      logical :: raus, jetzt_ausgeben, only

      !----initialize parallel computing:
      call parallel_ini()

      ! ----- write start message, find model directory, start progess display (file "fortschritt") which blocks concurrent runs in the same directory
      call fortschritt(1,0.0) ! also gets type of hydraulic driver 

      !------- reading input, allocation, initialisation etc.
      call eingabe()        ! --- input
      call initialisieren() ! --- initialize

      !----preparing parallel computing:
      call parallel_vorbereiten()

      !! mirror inital values
      call ausgeben()
      call ganglinien_zeitschritt(1)

!==== Stat of time-loop, time (zeitpunkt) in seconds (integer) ============= ============= ============= ============= =============
      do izeit=1,zeitschrittanzahl !------------------------------------------------- proceed in time

         call zeitschritt_halb(.true.) ! --- increment time and compute boundary-values in the middle of the timestep
         call MPI_Bcast(zeitpunkt,1,MPI_INT,0,mpi_komm_welt,ierr);call MPI_Bcast(izeit,1,MPI_INT,0,mpi_komm_welt,ierr)
         call zeitsekunde()
         call fortschritt(0,real(izeit)/real(zeitschrittanzahl)) ! update progess display
         call mpi_barrier (mpi_komm_welt, ierr)

         !------------------------------------------------- set Boundary-Conditions (incl. Weather and Flow)
         call randbedingungen_setzen()

         !-------------------------------------------------- suspended matter module 
         call schwebstoff_salz()    ! currently only reading distribuions from input
         call mpi_barrier (mpi_komm_welt, ierr)

         !------------------------------------------------- all metabolic processes 
         call stoffumsatz()     !!             <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

         !------------------------------------------------- transport all concentrations (advection-diffusion) ... 
         call stofftransport()  !!             <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< if (hydro_trieb.ne. 3) für SCHISM erstmal nicht #########

         !------------------------------------------------- finish time step
         call zeitschritt_halb(.false.)
         call mpi_barrier (mpi_komm_welt, ierr)

         !------------------------------------------------- output ... 
         if(jetzt_ausgeben()) call ausgeben() !! output concentration fields if required
         call ganglinien_zeitschritt(izeit+1) !! store values for time series
         call mpi_barrier (mpi_komm_welt, ierr)

      end do
!==== End of time-loop   ============= ============= ============= ============= =============
      write(*,*)meinrang,' End of time-loop'

      !------------------------------------------------- 
      !call mpi_barrier (mpi_komm_welt, ierr)
      !call gather_planktkon()
      !call gather_benthic()
      !call gather_ueber()
      !call mpi_barrier (mpi_komm_welt, ierr)
      !------------------------------------------------- 
      call ganglinien_schliessen() !! write and close time series files

      call ausgeben() !! output at the end
      !call aus_grd() !! aufsummierte Überstaudauer für Janet ausgeben
      if(hydro_trieb.eq. 3)call check_err( nf_close(ncid) ) ! close SCHISM netCDF files
      call mpi_barrier (mpi_komm_welt, ierr)

      call fortschritt(-1,0.0) !! write closing message, delete file "fortschritt"
      call mpi_finalize(ierr)
      if (ierr.ne.0)then
         print*,'mpi_finalize(ierr).ne.0'
         call exit(7)
      !else
      !   print*,'mpi_finalized'
      end if

      call exit(0)
      END program QSim3D
!--------------------------------------------------------------------------------------------------------------
!> \page lnk_Grundlagen Grundlegende Annahmen eines deterministischen Modells 
!! <ol>
!!  <li> Es werden nur die für den Sauerstoffgehalt mengenmäßig bedeutendsten Stoffumsetzungsprozesse simuliert. 
!!       Diese laufen auf der Ebene der Gewässermikrobiologie ab.\n\n</li>
!!  <li> Alle für die Güte relevanten Stoffe und Organismen werden als gleichmäßig im Wasser verteilte Variablen 
!!       (Konzentrationen) oder als gleichmäßig über die Sohle verteilte Belegungen simuliert. \n\n</li>
!!  <li> Jede Veränderungen einer Variable ist eindeutig von anderen Variablen abhängig (determiniert); 
!!       und anhand mathematischer Gleichungen im Modell festgelegt. Z. B. hängt der Zuwachs an Algen 
!!       eindeutig von Licht, Temperatur, Nährstoffen und Fraßdruck ab. \n</li>
!! </ol>
!! 
!! \section QSim-1D Verbindung zwischen QSim-1D und QSim-3D
!! Die beiden Programme der QSim-Familie unterscheiden sich in der räumlichen Auflösung des Wasserkörpers; 
!! die Detail erläutert der Abschnitt \subpage Dimension. 
!! Die datentechnische Realisierung finden Sie im Abschnitt \ref hüllen. 
!! Einen Einblick in die Entstehung der QSim-Familie gibt der Abschnitt \ref about_tiqusim.
!! 
!> \page lnk_konzept Aufbau von QSim 
!! \image html qsim_konzept.png "konzeptioneller Aufbau der Gütemodelle QSim-1D und QSim-3D"
!! Das Ineinandergreifen der Komponenten der QSim-Gütemodelle ist in obigem Schaubild zusammengestellt.
!! Beide beziehen die Informationen über den Strömungsvorgang aus einem als sog. "hydraulischen Treiber"
!! (in Grün dargestellt), der seine Ergebnisse für ein zu berechnendes Güte-Ereignis abspeichert. 
!! Die QSim-Programme laufen nach dem hydraulischen Treiber und lesen dessen Ergebnisse ein.
!! \n\n
!! Zur Simulation der lokalen biochemischen und biologischen Stoffumsetzungsprozesse werden in beiden QSim's 
!! die selben Module, die als Fortran Subroutinen realisiert sind, verwendet.
!! \n\n
!! Die unterschiedliche räumliche Auflösung führt dazu, dass äquivalente Ergebnisauswertungen 
!! in QSim-3D mindestens eine Dimension größer sind. 
!! \n\n
!! Bedient werden beide QSim's mit Gerris (\ref Gerris).
!! 
!> \page lnk_Kopplung Offline Kopplung von Strömungssimulation und Stoff-Transport
!! Dabei kommunizieren die Strömungssimulation und das Gütemodell offline, 
!! d. h. mittels fest abgespeicherte Dateien.
!! Der hydraulische Treiber ist eine eigenständige Software, welche die Strömung simuliert und seine Ergebnisse abspeichert.
!! Das Gütemodell ist eine ebenfalls eigenständige Software, die nach dem hydraulischen Treiber 
!! gestartet wird und dessen Ergebnisse "offline" einließt.
!! Voraussetzung für diese Informationsübertragung nur in die eine Richtung ist, dass die im Gütemodell betrachteten Wasserinhaltsstoffe
!! keinen (oder nur vernachlässigbaren) Einfluss auf den Strömungsvorgang haben. 
!! \n\n
!! Die offline-Kopplung hat sich in QSim bewährt. 
!! Viel Rechenzeit lässt sich dadurch sparen, dass in der Praxis viele Gütesimulationen auf der Basis 
!! einzelner hydraulischer Berechnungen durchgeführt werden. 
!! Müsste für jede Gütesimulationen eine eigene hydraulische Berechnung angefertigt werden, wäre eine online-Kopplung schneller.
!! Ausserdem ergibt die offline-Kopplung eine klare Schnittstelle in der interdisziplinären Zusammenarbeit.
!! \n\n
!! QSim-3D verwendet die folgenden hydraulischen Treiber: \n
!! <a href="http://www.wasserimunterricht.de/wyrwa/casu12.html"  target="_blank">casu</a>,\n UnTRIM und \n
!! <a href="http://voss-wiki.bafg.de/instanzen/schismwiki/doku.php/start" target="_blank">SCHISM</a> (in Arbeit).
!! \n\n
!! Der Stofftransport-Baustein in QSim übernimmt das Berechnungsnetz vom hydraulischen Treiber;
!! an dessen Struktur muss auch der Transportalgorithmus speziell angepasst werden.
!! Da die verwendeten hydraulischen Treiber zur Simulation des Transports von Salz und suspendierten Sedimenten
!! über eigene Löser der Advektions-Diffusions-Gleichung (Transportgleichung) verfügen,
!! ergibt sich die Möglichkeit, durch Übergabe von verfahrensabhängigen Transportinformationen
!! im Gütemodell einiges an Rechenaufwand einzusparen.
!! Details werden im Abschnitt \ref Stofftransport näher ausgeführt. 
!! \n\n
!! aus Datei: QSim3D.f95 ; zurück:\ref index

!--------------------------------------------------------------------------------------------------------------

!> \page lnk_simulation Simulations-Verfahren
!! 
!! \section EinfNum Grundgleichung
!! In QSim werden alle güte-relevanten Variablen incl. der planktisch lebenden Organismen
!! als im Wasser gelöste Konzentrationen modelliert\n
!! Die Veränderungen dieser Konzentrationen wird summarisch mit der untenstehenden partiellen Differential-Gleichung beschrieben:\n
!! \f[ 
!!   \underbrace {\frac{\partial c_m}{\partial t}}_{lokale Aenderung} = 
!!   \underbrace {Q_m ( c_1 \ldots c_m \ldots c_M, x_i, t )  }_{Stoffumsatz}
!!   \underbrace {
!! - \underbrace {v_i \frac{\partial c_m}{\partial x_i} }_{Advektion}
!! + \underbrace {\frac{\partial}{\partial x_i} D_{ij} \frac{\partial c_m}{\partial x_j}}_{Diffusion}
!!    }_{Stofftransport}
!! \f]
!! Dieser Grundgleichung beschreibt die zeitliche Änderung der m-ten Konzentration \f$ c_m \f$ an einem 
!! festen Punkt im Raum. Diese Konzentrationsbilanzen stellen den Verfahrenskern von QSim dar; 
!! datentechnisch werden diese Konzentrationen als \ref planktische_variablen gespeichert. 
!! Unter dem vorstehenden Link findet sich eine Auflistung, aus der hervorgeht um welche "Stoffe" es sich dabei handelt.
!! \n
!! Darüber hinaus bilanziert QSim auch \ref benthische_verteilungen
!! bei denen allerdings der Stofftransport-Anteil der obigen Gleichung entfällt.
!!
!! \section aenderUrsachen Ursachen der Konzentrations-Änderungen
!!        \subsection metabol Stoffumsatz
!!            Der gesamte Stoffumsatz, also alle biologischen Stoffwechselvorgänge und chemischen Reaktionen
!!            werden in der obigen Gleichung in einer Änderungsrate \f$ Q_m \f$ zusammengefasst.
!!            Diese hängt von anderen (ebenfalls als Konzentrationen modellierten) Variablen  
!!            sowie von Ort (x) und Zeit (t) ab. 
!!            \n\n
!!            Die voranstehende Gleichung kann auch als Massenbilanz des "Stoffes" m gelesen werden. 
!!            Dann ist \f$ Q_m \f$ je nach Vorzeichen ein Quelle oder Senke.
!!            Der Index m zählt über alle 1 bis M (z.Zt. M=76) transportierten Konzentrationen. (\subpage planktische_variablen)
!!            \n\n
!!            Die Prozessbeschreibungen, die hier formal in der Änderungsrate \f$ Q_m \f$ zusammengefasst wurden, die aber den Kern des Gütemodells ausmachen,
!!            werden in den einzelnen Modulbeschreibungen im Detail erläutert.
!!        \n\n
!!        \subsection transport Stofftransport
!!            Die Verfrachtung (Advektion) und Vermischung (Diffusion) der Konzentrationen
!!            infolge des Fließvorgangs des Wassers wird zusammenfassend als <b>\ref Stofftransport</b> benannt.
!!            \n\n
!!            Die Advektion bewirkt, dass das Strömungsfeld \f$ v_i \f$ eine Konzentration \f$ c_m \f$ von anderenorts 
!!            (\f$ x_i \f$) herantransportiert, wo diese andere Werte aufweist. 
!!            Der Index i zählt über alle drei Raumrichtungen; sein doppeltes Auftreten besagt, dass über ihn zu summieren ist.
!!            \n\n
!!            Die Diffusion wird hier mithilfe eines Diffusionstensors \f$ D_{ij} \f$ beschrieben. Diese bewirkt, dass sich
!!            räumliche Konzentrationsunterschiede ausgleichen.
!!            Die Indezes i und j zählen über alle drei Raumrichtungen;
!!            ihr doppeltes Auftreten besagt, dass über beide zu summieren ist.
!!            \n\n
!!            Die numerische Näherung des Transportprozesses in der Kopplung mit einem ebenfalls simulierten Strömungsfeld, 
!!            beschreibt der Abschnitt \ref Stofftransport.
!! 
!! \section fracStep Trennung der numerischen Lösung
!! Die voranstehenden Grundgleichung wird für ihre numerische Lösung getrennt. In QSim (sowohl 1D als auch 3D) wird zuerst 
!! der Stoffumsatz berechnet und dann der Transport. 
!! Dies sei anhand eines Wassertropfens erläutert, der während eines Zeitschritts auf einem gewissen Weg
!! im Wasserkörper fortbewegt wird:
!! Während diesem Zeitintervall laufen in besagtem Wassertropfen biochemische Stoffumwandlungsvorgänge ab.
!! Die Simulation in QSim läuft nun so ab, dass der Wasertropfen quasi am Ort festgehalten wird, an dem er sich zu Beginn des Zeitschritts befand.
!! Dort wird zunächst der Stoffumsatz unter den an diesem Ort herrschenden Bedingungen simuliert. 
!! Danach wird der Tropfen vom Transport-Algorithmus an den Ort gesetzt, den er am Ende des Zeitschritts erreicht.
!!
!! \section numerik Algorithmische Umsetzung
!! \n\n
!! \image html NumericalAspects_WyrwaSchoel_final5.png
!! Die numerische Lösung der o.g. partiellen Differentialgelichung für die Änderung einer Konzentration geschieht auf folgendem Wege:
!! \n\n
!! In einem Fractional-step-Verfahren werden die Stoffumsätze und der Stofftransport nacheinnander simuliert, 
!! ohne im gleichen Zeitschritt aufeinander rückwirken zu können.
!! \n\n
!! \image html NumericalAspects_WyrwaSchoel_final6.png
!! dabei können die Einzelprozesse mit unterschiedliche Zeitschritt-weiten abgebildet werden, die allerdings ganzzahlige Vielfache voneinader sein müssen.
!! \n\n
!! Die Stoffumsätze (aufgelistet in: \ref lnk_ueberblick) werden numerisch als \ref Num_Umsatz modelliert.
!! Die numerische Umsetzung der \ref Stofftransport - Modellierung wird im Vortrag
!! <a href="http://bibliothek.bafg.de/dokumente/Online%20377.ppt" target="_blank">
!! Numerical aspects in offline coupling of biochemical reaction modules with advection-diffusion simulations</a>
!! näher erläutert.
!!  \n\n 
!! Auf Details wurde im Vortrag
!! <a href="http://bibliothek.bafg.de/dokumente/Online%20377.ppt" target="_blank">
!! Numerical aspects in offline coupling of biochemical reaction modules with advection-diffusion simulations</a>
!! eingegangen. Die Näherungsgüte dieses auch als "fractional step" bezeichneten Verfahrens diskutiert die Dokumentation
!! <a href="./pdf/transportdoku_3.pdf" target="_blank"> ???? noch nicht geschrieben ??? </a>
!! \n\n
!! aus Datei QSim3D.f95 ; zurück zu \ref index


!--------------------------------------------------------------------------------------------------------------

!> \page Dimension 1, 2, 3 Dimensionen
!! Die Bezeichnung 1D und 3D der beiden QSim's steht für die ein-dimensionale 
!! respektive drei-dimensionale Raumauflösung der Modelle. 
!! Dies ist deswegen erklärungsbedürftig, weil mit beiden Berechnungswerkzeugen unterschiedliche 
!! zwei-dimensionale Simulationen ausgeführt werden können. \n\n
!! QSim-<b>1D</b> basiert auf einer querschnittsgemittelten Hydraulik (HYDRAX). 
!! Die horizontale Erstreckung eines Gewässers wird dadurch immer als Linie erfasst, 
!! welche im Modell als Abschnitte diskretisiert werden, die jeweils einem Querprofil zugeordnet sind. 
!! Ein Gewässernetz kann fernerhin noch in mehrere Stränge unterteilt sein, wobei jeder Strang viele Abschnitte enthalten kann. 
!! Im Gütemodell kann dann eine Tiefenauflösung
!! der Variablen gewählt werden, um vor allem Temperaturschichtungen simulieren zu können. 2D bei QSim-1D meint daher
!! <b>2D-breitengemittelt</b>.\n\n
!! QSim-<b>3D</b> basiert auf hydraulischen Treibern, welche die horizontale Erstreckung der hier behandelten flachen Gewässer
!! mit einer flächigen Auflösung/Diskretisierung anhand von Drei- und Vierecksnetzen erfassen. 
!! Wird die Wassertiefe mit nur einer Schicht modelliert, gelangt man zu <b>2D-tiefengemittelten</b> Simulationen.
!! Erst durch die Hinzunahme der schon in QSim-1D angelegten Tiefenauflösung gelangt man zu wirklich drei-dimensionalen 
!! Raumauflösungen, die aber nur in geschichteten Wasserkörpern erforderlich sind.
!! \n\n
!! aus Datei: QSim3D.f95 

!--------------------------------------------------------------------------------------------------------------

!>\page about_doc Über diese Dokumentation
!! Das spezifische des Dokumentationsportals besteht darin, dass es mithilfe des Dokumentationsgenarators 
!! <a href="http://www.doxygen.org/index.html" target="_blank">Doxygen </a>
!! direkt aus dem Quell-Code generiert wird.  
!! \n\n
!! Dies hat den Vorteil, dass die Beschreibung des Berechnungsverfahrens zusammen mit dessen programmtechnischer 
!! Realisierung (QSim-3D ist in Fortran95 codiert) gemeinsam in einer Datei erstellt wird. D. h. die selbe Datei, 
!! die der Compiler zur Erzeugung des ausführbaren Programms verwendet, dient auch dem Dokumentationsgenarator 
!! zur Erzeugung der Dokumentation. Der Entwickler muss keine seperate Dokumentation pflegen, sondern hat sie in 
!! der Datei direkt neben dem entsprechenden Abschnitt unmittelbar vorliegen. 
!! \n\n
!! Der Vorteil für den Anwender besteht
!! darin, dass bei weitergehenden Fragen auch direkt im Programmcode das tatsächlich ausgeführte Berechnungsverfahren
!! im Detail nachgeschaut werden kann. 
!! In der Regel finden Sie untenstehend auf den Web-Seiten dieses Portals auch den Link zu derjenigen 
!! Programmdatei aus der sie erzeugt wurde. 
!! Diese hier wurde z.B. aus der Datei QSim3D.f95 generiert, welche das Hauptprogramm enthält.
!! \n\n
!! Darüber hinaus ermöglicht 
!! <a href="http://www.doxygen.org/index.html" target="_blank">Doxygen </a> die Einbindung von Links auf externe Quellen.
!! Diese Dokumentation wird nicht nur zur direkten Beschreibung der Programmquellen genutzt, sondern auch als 
!! zentrale Stelle, von der aus alle weiteren Informationen zu QSim erreicht werden können. Daher ist 
!! die Bezeichnung "Dokumentations-Portal" gewählt worden.
!! \n\n aus Datei: QSim3D.f95 ;  zurück:\ref index

!--------------------------------------------------------------------------------------------------------------

!> \page lnk_validierung Validierung, Beispiele
!! <ul>
!! <li>das alte <a href="http://oss-hpc01.bafg.de/wyrwa/vali/index.html"  target="_blank">Validierungs Dokument</a> 
!!     wird momentan (Dez15) in ein <a href= "http://voss-wiki.bafg.de/instanzen/qsim_validierung/doku.php?id=start" target="_blank">Validierungs-Wiki</a> überführt\n\n </li>
!! <li>Anwendungs-Beispiele befinden sich im <a href="http://voss-wiki.bafg.de/instanzen/modellwiki/doku.php?id=start" target="_blank">Modell-Wiki</a>\n\n </li>
!! <li>Fachliche Fragen, bei denen \subpage klaerung besteht ...\n\n </li>
!! </ul>
!! \n\n 
!! aus Datei QSim3D.f95; zurück zu \ref index

!--------------------------------------------------------------------------------------------------------------

!> \page lnk_literatur Literatur
!!
!! \section wiss wissenschaftliche Veröffentlichungen mit Bezug zu QSim
!! <ul>
!!  <li><a href="http://bibliothek.bafg.de/webopac/index.asp?detsuche_systematik=online+492" target="_blank">
!!      Modelling Water Quality in the Elbe and its Estuary Large Scale and Long Term Applications with Focus on the Oxygen Budget of the Estuary, 
!!      Schöl et al. 2014</a> \ref lnk_litfuss \n\n</li>
!!  <li><a href="http://bibliothek.bafg.de/webopac/index.asp?detsuche_systematik=?????" target="_blank">
!!      Oxygen - Conceptual Model and QSim Approach, 
!!      Schöl et al. 2013</a> \ref lnk_litfuss \n\n</li>
!!  <li><a href="http://bibliothek.bafg.de/webopac/index.asp?detsuche_systematik=online+118" target="_blank"> 
!!      Influence of global change on phytoplankton and nutrient cycling in the Elbe River,
!!      Quiel et al. 2011</a> \ref lnk_litfuss \n\n</li>
!!  <li><a href="http://bibliothek.bafg.de/webopac/index.asp?detsuche_systematik=?????" target="_blank">
!!      MODELLING THE EFFECTS OF THERMAL STRATIFICATION ON THE OXYGEN BUDGET OF AN IMPOUNDED RIVER,
!!      Becker et al. 2009</a> \ref lnk_litfuss \n\n</li>
!!  <li><a href="http://bibliothek.bafg.de/index.asp?detsuche_systematik=sep+8989" target="_blank">
!!      Modelling the chlorophyll a content of the river Rhine - 
!!      interrelation between riverine algal production and population biomass of grazers, rotifers and the zebra mussel, Dreissena polymorpha,
!!      Schöl et al. 2002</a> \ref lnk_litfuss \n\n</li>
!!  <li><a href="http://bibliothek.bafg.de/dokumente/II,230-43.1999.pdf" target="_blank">
!!      Das Gewässergütemodell QSIM - ein Instrument zur Simulation und Prognose des Stoffhaushalts 
!!      und der Planktondynamik von Fließgewässern.
!!      Kirchesch, V. und Schöl, A. 1999</a> \ref lnk_litfuss \n\n</li>
!!  <li><a href="http://bibliothek.bafg.de/webopac/index.asp?detsuche_systematik=A+224" target="_blank">
!!      Mixing in inland and coastal waters,
!!      Fischer et al. 1979</a> \ref lnk_litfuss \n\n</li>
!!  <li><a href="http://bibliothek.bafg.de/webopac/index.asp?detsuche_systematik=A+7042" target="_blank">
!!      Turbulence in open-Channel Flow,
!!      I. Nezu und H. Nakagawa 1993</a> \ref lnk_litfuss \n\n</li>
!! </ul>
!! <a href="http://bibliothek.bafg.de/webopac/index.asp?detsuche_systematik=online+321" target="_blank"> 
!! QSim Übersicht</a> \ref lnk_litfuss und in der \n
!! http://dx.doi.org/10.1002/rra.1260 \n
!!
!! \section andere Beispiele anderer Projekte:
!! <a href="http://oss.deltares.nl/web/delft3d/publications" target="_blank">Delft3D</a> \n
!! <a href="http://www.opentelemac.org/index.php/publications" target="_blank">Telemac</a> \n
!! <a href="http://ccrm.vims.edu/schism/schism_pubs.html" target="_blank">SCHISM (ex SELFE)</a> \n
!! 
!! \section lnk_litfuss *
!! <h3>Entschuldigung,</h3>
!! unsere Bibliothekssoftware ist leider schon etwas älter.
!! Daher erhalten Sie beim Anklicken des ersten Literaturverweises nach dem Öffnen ihres Browsers
!! nur die Suchmaske unserer Bibliothek. \n \n
!! Bitte klicken Sie den gewählten Literatur-Link noch einmal, um zur gewünschten Publikation zu gelangen.
!! Alle weiteren Literatur-Links funktionieren in derselben Browser-Sitzung dann beim ersten Anklicken.
!! \n\n
!! Wenn im Systematik-Feld ????? stehen, wurde die Publikation noch nicht in den Katalog der BfG-Bibliothek übernommen.
!! \n\n
!! aus Datei: QSim3D.f95 ; zurück mit dem Browser-Button <-

!--------------------------------------------------------------------------------------------------------------

!> \page  klaerung Klärungsbedarf
!! Dieses Dokumentationsportal dient in erster Linie dem Zweck, die Funktion von QSim-3D in der aktuellen
!! Fassung zu beschreiben. \n
!! Bei Testrechnungen und in der Anwendung ergeben sich z.T. Fragen dahingehend, ob das
!! angewendete Berechnungverfahren auch fachlich (hinreichend) zutreffend ist.\n
!! Um solche fachlichen Fragestellungen einer spätere Bearbeitung zuführen zu können, werden sie in einem eigenen 
!! <a href="http://voss-wiki.bafg.de/instanzen/qsim-fragen/doku.php?id=start" target="_blank">Klärungs-Wiki</a>
!! gesammelt.
!! \n
!! Bisher hier im Dokumentationsportal notierte Fragen sind:
!! <ol>
!! <li>\subpage sauerkohl \n\n</li>
!! <li>\subpage kalterwind \n\n</li>
!! </ol>
!! aus Datei QSim3D.f95 ; zurück zu \ref lnk_validierung

!--------------------------------------------------------------------------------------------------------------
 
!> \page lnk_download Download
!! <h3>Versionierungs-System</h3>
!! <ul><li>Im Versionierungssystem <a href="./html/QSim1D_GitBucket.html">GitBucket</a> ist
!! der aktuelle Quellcode von QSim verfügbar:\n
!! Dazu existiert eine Anleitung zur <a href="./pdf/QSIM_GIT.pdf" target="_blank">Weiterentwicklung von QSim mit GIT</a> \n</li></ul>
!! <h3>ausführbare Programme</h3>
!! <ul><li> QSim1D Windows-executabels  Z:\\U\\U2\\QSim\\Versionen_u_Setups\\QSim (im BfG-Netzwerk)\n </li> 
!! <li> QSim3D Linux-executabels  /home/Wyrwa/bin (HPC im BfG-Netzwerk) \n </li> </ul>
!! <h3>Dokumentation</h3>
!! <ul><li> <a href="./taz/qsim_doku.taz"> ergänzendes Material zur Dokumentation</a> \n </li></ul>
!! <h3>Archiv</h3>
!! <ul><li> <a href="./taz/qsimcode_13_40originalVolker.zip"> QSim1D-source-code (15. Oktober 2018)</a> letzte Version von Volker Kirchesch\n</li></ul>
!!
!! aus Datei QSim3D.f95 ; zurück zu \ref index
 
!--------------------------------------------------------------------------------------------------------------

!> \page Programmstart  Programmstart
!! nach Abschluss der \ref Modellerstellung kann das Programm QSim3D gestartet werden. Dazu gibt es die folgenden Möglichkeiten: 
!!
!! \section manuell Linux-Konsole
!! \subsection seriell seriell
!! Nachdem der sourcecode von Qsim-3D compiliert wurde und das daraus entstandene ausführbare Programm (executabel)
!! z.B. in der Datei /home/Wyrwa/bin/qsim3d abgelegt wurde; \n
!! kann es dann von der Kommandozeile einer Konsole eines Linux-Rechners aus direkt ausgeführt werden.\n
!! Z.B. auf dem voss-mod01 mit dem Kommando:
!! \code wyrwa@voss-mod01:~> /home/Wyrwa/bin/qsim3d /srv/wyrwa/annu/ 769 \endcode
!! Der erste, obligatorische  Parameter ist das Modellverzeichnis 
!! (mit abschließendem betriebssystemspezifischem Verzeichnistrenner).
!! \n Der zweite, optionale Parameter ist eine Kontrollknotennummer für eine erweiterte Bildschirmausgabe.
!! \n Das Programm wird damit seriell, d. h. alle Operationen werden nacheinander auf einem Prozessor ausgeführt.
!!
!! \subsection parallel parallel
!! QSim3D ist mit dem Message Passing Interface (MPI) parallelisiert, um auf Parallelrechnern mit verteiltem Speicher ausführbar zu sein;
!! siehe: \ref Parallelisierung. Es wird dann das Kommando mpiexec verwendet, um QSim3d auf einer Gruppe von Prozessoren zu starten.
!!
!! \subsection batch-queue batch-queue
!! Teure Hochleistungsrechner wie der HPC (high-performance-cluster) der BfG stehen in der Regel nicht einzelnen Kollegen alleine zur Verfügung.
!! Um die Berechnungsaufträge von vielen Benutzern abzuarbeiten, werden sogenannte batch-queues (elektronische Warteschlangen) verwendet.
!! Der Benutzer übergibt seinen Berechnungsauftrag durch Start eines qsub-scripts (torque) an einen Scheduler.
!! Der Auftrag wird dann abgearbeitet, sobald die angeforderten Kapazitäten auf dem HPC verfügbar sind.
!! Sowohl QSim3D als auch qsub verfügen über Möglichkeiten den Benutzer über das Berechnungsende per Email zu informieren.
!!
!! \section automatisch Benutzeroberfläche Gerris
!! Für den Programmstart mithilfe von \ref Gerris ist es erforderlich, vorher eine ssh-Verbindung 
!! zwischen dem eigenen Arbeits-PC und dem server, auf dem gerechnet werden soll, herzustelllen.
!! Details dazu enthalten die Dokumente: 
!! <a href="./pdf/Gerris3D_nachvollzogenJens.pdf" target="_blank">Gerris3D_nachvollzogen</a> 
!! und <a href="./pdf/Schluesselkonvertierung.pdf" target="_blank">Schluesselkonvertierung</a>
!! \n\n 
!! Gerris startet auf dem Server allerdings nur ein script, in dem der maschinenspezifische Programmaufruf enthalten ist. 
!! Z.Z. (aug18) ist dies hpc-worker.bafg.de:/home/Wyrwa/bin/qsim3d_gerris.bash.\n
!! Dieses ruft wiederum ein qsub-script auf, das im jeweiligen Modellverzeichnis angelegt werden muss, 
!! um die Simulation des dort liegenden Modells/Ereignises in die Warteschlange (batch-queue) des hpc-servers einzureihen.
!! 
!! \section fortschritt fortschritt
!! Während des Rechenlaufs wird im Modellverzeichnis eine Datei \p fortschritt abgelegt, die nur eine Zahl
!! zwischen 0 und 1 enthält, und angibt, welcher Anteil der erforderlichen Zeitschritte bereits durchlaufen wurde.
!! Beim Beenden von QSim-3D wird diese Datei gelöscht. Das Vorhandensein von \p fortschritt verhindert, dass weitere
!! Rechenläufe gestartet werden, die dasselbe Modellverzeichnis verwenden. \n Bei unkontrollierten Programmabbrüchen
!! wird \p fortschritt nicht gelöscht. Bei einem Neustart würde dies das Anlaufen von QSim3d verhindern; 
!! d.h. \p fortschritt muss nach unkontrollierten Abbrüchen vor dem Neustart manuell entfernt werden.
!! \n\n
!! <hr>Es existiert eine Überlegung QSim3D mithilfe einer in php codierten <a href="./taz/webstart_tiqu26apr16.taz">Web-Anwendung</a> zu starten.  
!! (z. Z. im Versuchsstadium)
!! \n\n
!! aus Datei QSim3D.f95 ; zurück zu \ref lnk_technisch oder \ref index

!--------------------------------------------------------------------------------------------------------------

!> \page about_tiqusim Geschichte
!! QSim-3D basiert auf <a href="http://www.bafg.de/DE/08_Ref/U2/01_mikrobiologie/QSIM/qsim_node.html" target="_blank">QSim-1D</a>, 
!! das 1979 startete und von 
!! <a href="http://www.bafg.de/DE/08_Ref/U2/05_Mitarbeiter/kirchesch_v/kirchesch_node.html" target="_blank">Volker Kirchesch </a>
!! programmiert wurde und wird.
!! \n\n
!! Die ersten 
!! <a href="http://www.bafg.de/DE/08_Ref/U2/01_mikrobiologie/QSim_Praxis/qsim_praxis_node.html" target="_blank">Anwendungen</a>
!! von QSim-1D betrafen 1980 die Fulda und 1983 die Saar. Seitdem wird QSim durchgehend für Gewässergüte-Untersuchungen an 
!! deutschen Wasserstraßen eingesetzt.
!! \n\n
!! Im Jahr 2000 beginnt die Erstellung der Benutzeroberfläche Gerris, mit der sich QSim
!! effektiver und übersichtlicher bedienen läßt.
!! \n\n
!! Im August 2011 begann Jens Wyrwa mit der Programmierung von QSim-3D,
!! damals unter dem Projektnamen T-QSim (T für Tidengewässer), weil sich die Gewässergüte 
!! in tidebeeinflussten Ästuaren mit ihren Nebenrinnen und Wattflächen 
!! nicht mehr querschnitts- oder breitengemittelt erfassen läßt.
!! \n\n
!! \image html Taufe1.0.0.jpg
!! Am 16. September 2014 findet die Softwaretaufe von QSim-3D statt. Zu diesem Anlaß wurde der neue Name eingeführt.
!! Nach der Einbindung der Algen- und Konsumenten-Module simuliert QSim-3D erste plausible Sauerstoffgehalte.
!! Bei dem kleinen, feierlichen Umtrunk stellt sich Frau Dr. Tanja Bergfeld-Wiedemann spontan als Taufpatin 
!! zur Verfügung und beschriftet die Sicherungs-CD der Version 1.0.0.
!! \n\n 
!! aus Datei QSim3D.f95; zurück: \ref index

!--------------------------------------------------------------------------------------------------------------

!!!!!!!!!!! Internationalisierung ??????
!! so nicht ...
!#endif
!#ifdef ESPANOL
!#include "main.es.h"
!#endif
! sondern mir \~German ... gemäß Anleitung:
! https://stackoverflow.com/questions/46662062/multiple-languages-in-doxygen-docs-c
! http://www.stack.nl/~dimitri/doxygen/manual/config.html#cfg_output_language
! http://www.stack.nl/~dimitri/doxygen/manual/commands.html#cmdtilde


