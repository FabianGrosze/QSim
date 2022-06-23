! --------------------------------------------------------------------------- !
!  QSim - Programm zur Simulation der Wasserqualität                          !
!                                                                             !
!  Copyright (C) 2022                                                         !
!  Bundesanstalt für Gewässerkunde                                            !
!  Koblenz (Deutschland)                                                      !
!  http://www.bafg.de                                                         !
!                                                                             !
!  Dieses Programm ist freie Software. Sie können es unter den Bedingungen    !
!  der GNU General Public License, Version 3, wie von der Free Software       !
!  Foundation veröffentlicht, weitergeben und/oder modifizieren.              !
!                                                                             !
!  Die Veröffentlichung dieses Programms erfolgt in der Hoffnung, dass es     !
!  Ihnen von Nutzen sein wird, aber ohne irgendeine Garantie, sogar ohne die  !
!  implizite Garantie der Makrtreife oder der Verwendbarkeit für einen        !
!  bestimmten Zweck.                                                          !
!                                                                             !
!  Details finden Sie in der GNU General Public License.                      !
!  Sie sollten ein Exemplar der GNU General Public License zusammen mit       !
!  diesem Programm erhalten haben.                                            !
!  Falls nicht, siehe http://www.gnu.org/licenses/.                           !
!                                                                             !
!  Programmiert von                                                           !
!  1979 bis 2018   Volker Kirchesch                                           !
!  seit 2011       Jens Wyrwa, Wyrwa@bafg.de                                  !
! --------------------------------------------------------------------------- !
!> \page Stofftransport Stofftransport
!!
!! <h2>Konzept: Konzentrations-Transport</h2>
!! In QSim werden im Wasser gelöste Stoffe, planktisch lebende Organismen und deren Eigenschaften
!! als Konzentrationen modelliert. D. h. es wird angenommen,
!! dass selbige nur vernachlässigbare Bewegungen gegenüber dem umgebenden Wasser ausführen.
!! QSim arbeite mit 72 tiefengemittelten und 11 tiefenaufgelösten Transportkonzentrationen
!! deren Bedeutung in \ref planktische_variablen näher beschrieben wird.
!! \n\n
!! <h2>Advektions- Diffusons-Gleichung</h2>
!! Die gesamte Änderung einer Konzentration wird aufgespalten in die Änderung infolge Stoffumsatz und diejenige infolge
!! Transport durch das fließende Wasser. In diesem Zweig geht es jetzt nur noch um den Transportanteil:
!! \f[
!!   \underbrace {\frac{\partial c_m}{\partial t}}_{Aenderung infolge Transport} =
!! - \underbrace {v_i \frac{\partial c_m}{\partial x_i} }_{Advektion}
!! + \underbrace {\frac{\partial}{\partial x_i} D_{ij} \frac{\partial c_m}{\partial x_j}}_{Diffusion}
!! \f]
!! Die obige Gleichung (Transportgleichung) für die m-te Konzentration \f$ c_m \f$
!! gilt allgemein für die Advektion durch ein 3-dimensionales Strömungsfeld \f$ v_i \f$.
!! Mit t wird die Zeit bezeichnet und mit \f$ x_i \f$ ein Ort im 3-dimensionalen Raum.
!! Der Index i zählt über alle drei Raumrichtungen; sein doppeltes Auftreten besagt, dass über ihn zu summieren ist.
!! Die Vermischung wird vom Diffusionstensors \f$ D_{ij} \f$ bewirkt, der genauso wie der Strömungsvektor von Ort und Zeit abhängig sein kann.
!! \n\n
!! <h2>räumliche Integration</h2>
!! Die Integration der Transportgleichung in eine tiefen-, breiten- und querschnitts-gemittelte Formulierung
!! wird in der speziell dazu erstellten <a href="./pdf/transport_dimensionsreduziert2016.pdf" target="_blank">Ausarbeitung</a> hergeleitet.
!! \n\n
!! <h2>antreibendes Strömungsfeld</h2>
!! Das in der Transportgleichung verwendete Geschwindigkeitsfeld wird für QSim vorab mithilfe einer numerischen Strömungssimulation
!! (CFD, Computational Fluid Dynamics) von einem sogenanntes HN(Hydro-Numerisches)-Modell berechnet, das in diesem Zusammenhang auch als
!! "hydraulischer Treiber" bezeichnet wird. Bei der Verbindung von Strömungssimulation und Gütemodell handelt es sich um eine \ref lnk_Kopplung .\n\n
!! QSim-1D verwendet als hydraulischen Treiber Hydrax.\n\n
!! QSim-3D kann die Ergebnisse verschiedener hydraulischer Treiber nutzen:\n
!! <ul>
!!    <li><a href="http://www.wasserimunterricht.de/wyrwa/casu12.html"  target="_blank">casu</a>.
!!        Die von o.g. HN-Modellen binär, verlustbehaftet komprimierten Transportinformationen
!!        werden von holen_trans() mit der C++ Funktion trans_read_() gelesen. \ref Transport_casu</li>\n\n
!!    <li>UnTRIM (BAW) \ref Transport_Untrim</li>\n\n
!!    <li>SCHISM <a href="http://voss-wiki.bafg.de/instanzen/schismwiki/doku.php/start" target="_blank">Wiki</a> (BfG),
!!        <a href="http://ccrm.vims.edu/schism/" target="_blank">Homepage</a> (VIMS) \ref Transport_SCHISM</li>\n\n
!! </ul>
!!
!! <h2>numerischen Näherungslösungen</h2>
!! Die näherungsweise Lösungen der Advektions- Diffusons-Gleichung (Transportgleichung) werden im Abschnitt \ref numerik vorgestellt.
!!
!! <h2>datentechnische Umsetzung</h2>
!! siehe \subpage hydraul_rb
!!  \n\n
!! zurück: lnk_simulation;  Quelle: stofftransport.f95
!> \page Netz Netz / Orts-Diskretisierung
!! Sowohl in QSim1D als auch in QSim3D wird die horizontalen Diskretisierung, also des Netzes (3D) oder der Gewässer-Stränge (1D)
!! vom Hydraulischen Treiber übernommen. Im 3D werden benutzt:
!! <ul>
!!    <li> <a href="http://www.wasserimunterricht.de/wyrwa/casu12.html"  target="_blank">casu</a>, \ref Transport_casu</li>
!!    <li> UnTRIM (Datenübergabe von der BAW), \ref Transport_Untrim</li>
!!    <li> SCHISM in Arbeit, \ref Transport_SCHISM</li>
!! </ul>
!! \n\n
!! Die Verwendung einer auf die Stofftransportsimulation abgestimmten Diskretisierung bietet Vorteile bei
!! der Genauigkeit und der Rechenzeit. Dazu erforderlich ist aber eine Interpolation der hydraulischen Ergebnisse
!! auf das Güte-Netz. Die Implementierung dieser Interpolation ist nicht ganz unaufwändig und der dadurch erzielbare
!! Vorteil nicht sicher einschätzbar. Bisher realisiert wurde in dieser Frage nur eine einfache Vergröberung von Deltares.\n
!! siehe auch: \ref Stofftransport
!! \n\n
!! Zum Lesen des Netzes aus dem Modellverzeichnis bedient sich netz_lesen() der Subroutinen points(), elements() und edges()
!! \n\n
!! aus Datei stofftransport.f95 ; zurück zu \ref Modellerstellung
!
!> \page Transportinformationen Transportinformationen
!!
!! <ol>
!! <li> \subpage Transport_casu  </li>
!! <li> \subpage Transport_Untrim  </li>
!! <li> \subpage Transport_SCHISM  </li>
!! </ol>
!! \n\n aus Datei stofftransport.f95; zurück zu \ref Modellerstellung oder \ref lnk_Datentechnik
!
!! Steinbruch Numerik
!!
!! Advektion Advektion
!! \f[
!! c_{m,k}(t+\Delta t) = A_{kl}(\Delta t^{adv}){c}_{m,l}^{diff,react} \quad ; \quad
!! {c}_{m,k}^{diff,react}=D_{kl}(\Delta t^{diff}){c}_{m,l}^{react} \quad ; \quad
!! {c}_{m,k}^{react}= Q_m(c_1(t) \ldots c_M(t), \underline {x}, t, \Delta t^{react})
!! \f]
!!  \n\n
!! zurück: \ref Stofftransport ;  Quelle: stofftransport.f95
!!
!!  massenerhalt Massenerhaltung
!! nicht konservatives ELM-Verfahren.\n
!! Verfahren zur Abschätzung der Massenerhaltung: ... to be continued
!!  \n\n
!! zurück: \ref Stofftransport ;  Quelle: stofftransport.f95
!! Diffusion Diffusion
!! \f[
!! \frac{\partial c_m}{\partial t} =
!! - \underbrace {v_i \frac{\partial c_m}{\partial x_i} }_{Advektion}
!! + \underbrace {\frac{\partial}{\partial x_i} D_{ij} \frac{\partial c_m}{\partial x_j}}_{Diffusion}
!! + \underbrace {Q_m ( c_1 \ldots c_m \ldots c_M, x_i, t )  }_{Stoffumsatz}
!! - \underbrace {w_{s,3} \frac{\partial c_m}{\partial x_3} }_{Sinken}
!! \f]
!!  \n\n
!! zurück: \ref Stofftransport  ;  Quelle: stofftransport.f95
!!! <hr> \n <i>folgender Abschnitt z. Z. noch in Bearbeitung</i>
!! \n
!! \section pde mathematische Beschreibung als partielle Differentialgleichung
!! In QSIM werden im Wasser gelöste Stoffe und planktisch lebende Organismen, die näherungsweise passiv im Wasser treiben, als Konzentrationen modelliert\n\n
!! Die Veränderungen dieser Konzentrationen werden mit der untenstehenden Transport-Gleichung beschrieben.
!! \n\n
!! \f[
!! \frac{\partial c_m}{\partial t} =
!! - \underbrace {v_i \frac{\partial c_m}{\partial x_i} }_{Advektion}
!! + \underbrace {\frac{\partial}{\partial x_i} D_{ij} \frac{\partial c_m}{\partial x_j}}_{Diffusion}
!! + \underbrace {Q_m ( c_1 \ldots c_m \ldots c_M, x_i, t )  }_{Stoffumsatz}
!! - \underbrace {w_{s,3} \frac{\partial c_m}{\partial x_3} }_{Sinken}
!! \f]
!! \n\n
!! \f[
!! \frac{\partial c_m}{\partial t} =
!! - \underbrace {v_i \frac{\partial c_m}{\partial x_i} }_{advection}
!! + \underbrace {\frac{\partial}{\partial x_i} D_{ij} \frac{\partial c_m}{\partial x_j}}_{diffusion}
!! + \underbrace {Q_m ( c_1 \ldots c_m \ldots c_M, x_i, t )  }_{source}
!! - \underbrace {w_{s,3} \frac{\partial c_m}{\partial x_3} }_{settling/rise}
!! \f]
!! \n\n
!! \f[
!! c_m(t+\Delta t,\underline {x}) = c_m(t,{\underline {x}}_{orig}) + {\Delta c}_m^{diff} + {\Delta c}_m^{react}
!! \f]
!! \n\n
!! \f[
!! c_{m,k}(t+\Delta t) = A_{kl}(\Delta t^{adv}){c}_{m,l}^{diff,react} \quad ; \quad
!! {c}_{m,k}^{diff,react}=D_{kl}(\Delta t^{diff}){c}_{m,l}^{react} \quad ; \quad
!! {c}_{m,k}^{react}= Q_m(c_1(t) \ldots c_M(t), \underline {x}, t, \Delta t^{react})
!! \f]
!! \n\n
!! \f[  n_1 \cdot \Delta t^{adv} = n_2 \cdot \Delta t^{diff} = \Delta t^{react}
!! \f]
!! \n\n
!! \f[
!! \frac{\partial c_m}{\partial t} = Q_m ( c_1 \ldots c_m \ldots c_M, x_i, t )  - w_{s,3} \frac{\partial c_m}{\partial x_3} - v_i \frac{\partial c_m}{\partial x_i} + \frac{\partial}{\partial x_i} D_{ij} \frac{\partial c_m}{\partial x_j}
!! \f]
!! mit: \n <ul>
!!    <li> \f$ c_m \f$ = m-te Konzentration \n\n</li>
!!    <li> \f$ t \f$ = Zeit\n\n</li>
!!    <li> \f$ x_i \f$ = Ortskoordinaten in den mit i indizierten 3 Raumrichtungen\n\n</li>
!!    <li> \f$ v_i \f$ = Strömungsgeschwindigkeiten in den mit i indizierten 3 Raumrichtungen (Einsteinsche Summationskonvention); \n
!!         siehe dazu >> \subpage advect \n\n</li>
!!    <li> \f$ D_{ij} \f$ = Diffusivitätstensor (symetrisch), \n
!!         meist als isotrop angenommen \f$ D_{ij} = d \cdot \delta_{ij} \f$ mit \f$ d\f$=Diffusivität und \f$\delta_{ij}\f$=Einheitstensor; \n
!!         siehe dazu >> \subpage diff \n\n</li>
!!    <li> \f$ Q_m \f$ = Quelle/Senke der Konzentration m (lokale Veränderungsrate, Stoffumsatz); \n
!!          siehe dazu >> \ref Num_Umsatz \n\n</li>
!!    <li> \f$ w_{s,3} \f$ = Sinkgeschwindigkeit in der vertikalen (3) Raumrichtung;\n
!!          siehe dazu >> \subpage sinken \n\n</li>
!! </ul>
!! \n\n
!! \f[
!! A_{ij} c_{j}^{n+1} = B_{ij} c_{j}^{n} + R_i
!! \f]
!! mit: \n <ul>
!!    <li> \f$ c_{j}^{n} \f$ Konzentration am Knoten j zum Zeitpunkt n \n\n</li>
!!    <li> \f$ c_{j}^{n+1} \f$ Konzentration am Knoten j zum auf n folgenden Zeitpunkt \n\n</li>
!! </ul>
!! \n\n
!! \f[
!! c^{n+1}_{m,i} = \sum_{k=1}^4 c^{n}_{m,Nr_i(k)}  \cdot w_i(k)
!! \f]
!! \n\n
!! \f[
!! c_{m,i}^{n+1a} = A_{ij} c_{m,j}^{n}
!! \f]
!! \f[
!! c_{m,i}^{n+1d} = D_{ij} c_{m,j}^{n}
!! \f]
!!! \n\n
!! \f[
!! c_{m,i}^{n+1q}( c_{1,i} \ldots c_{m,i} \ldots c_{M,i}, x_i, t )
!! \f]
!! \n\n
!! \f[
!! \frac{c_{i}^{n+1}-c_{j}^{n}}{\Delta t} =
!! \f]
!! mit: \n <ul>
!!    <li> \f$ c_{j}^{n} \f$ Konzentration am Knoten j zum Zeitpunkt n \n\n</li>
!!    <li> \f$ c_{j}^{n+1} \f$ Konzentration am Knoten j zum auf n folgenden Zeitpunkt \n\n</li>
!! </ul>
!! \n\n
!! \f[
!! c^{n+1}_i = \sum_{k=1}^4 c^{n}_{Nr_i(k)} \cdot w_i(k) + R_i
!! \f]
!! \n\n
!! \section fractional numerische Näherungslösung für o. g. partielle Differentialgleichung
!! Die numerische Näherungs-Lösung dieser Transportgleichungen an diskreten Berechnungspunkten im Raum und zu diskreten Zeitpunkten
!! erfolgt in zwei aufeinanderfolgen Schritten (<b>fractional step algorithm</b>):\n\n
!! In einem ersten Schritt werden \ref Num_Umsatz und \ref sinken (im Weiteren als Stoffumsatz oder lokale Änderung bezeichnet) berechnet.
!! Daraus ergeben sich Konzentrationsfelder für den nächsten Zeitpunkt (als ob das Wasser steht).\n
!! Diese (Zwischen-) Konzentrationsfelder werden dann in einem zweiten Schritt der \ref advect und \ref diff (im Weiteren als Transport bezeichnet) unterzogen.\n\n
!! Diese Teilung der numerischen Näherungs-Lösung auf der obersten Ebene in Stoffumsatz und Transport ist zulässig unter der Voraussetzung,
!! dass die Bedingungen für den Stoffumsatz entlang des Transportweges näherungsweise gleich bleiben.
!! D. h., dass sich Konzentrationszusammensetzung und Randbedingungen (z.B. Sonneneinstrahlung)
!! entlang des in einem Zeitschritt zurückgelegten Transportweges nur unwesentlich ändern.
!! Dies dürfte in Tidengewässern bei hinreichend kleinen Zeitschritten der Fall sein.\n\n
!! Für die Berechnungen in den beiden Schritten werden jeweils eigene numerische Näherungsverfahren eingesetzt,
!! die in den jeweiligen Unterkapiteln (\ref Num_Umsatz und \ref sinken, sowie \ref advect und \ref diff) beschrieben sind.
!!
!! <hr> ####\n<i>folgender Abschnitt z. Z. noch in Bearbeitung</i> \n
!! SUBROUTINE  stofftransport(), allo_trans(), holen_trans(), transinfo_schritte(), transinfo_sichten() \n
!! trans_read_() und lossy_input_() in trans_read.c\n
!! \n\n
!! Die Veränderungen dieser Konzentrationen wird mittels der untenstehenden Transport-Gleichung beschrieben:
!! \n\n
!! \f[
!! \frac{\partial c_m}{\partial t} =
!! - \underbrace {v_i \frac{\partial c_m}{\partial x_i} }_{Advektion}
!! + \underbrace {\frac{\partial}{\partial x_i} D_{ij} \frac{\partial c_m}{\partial x_j}}_{Diffusion}
!! + \underbrace {Q_m ( c_1 \ldots c_m \ldots c_M, x_i, t )  }_{Stoffumsatz}
!! - \underbrace {w_{s,3} \frac{\partial c_m}{\partial x_3} }_{Sinken}
!! \f]
!! \n\n
!! \f[
!! \frac{\partial c_m}{\partial t} = Q_m ( c_1 \ldots c_m \ldots c_M, x_i, t )  - w_{s,3} \frac{\partial c_m}{\partial x_3} - v_i \frac{\partial c_m}{\partial x_i} + \frac{\partial}{\partial x_i} D_{ij} \frac{\partial c_m}{\partial x_j}
!! \f]
!! mit: \n <ul>
!!    <li> \f$ c_m \f$ = m-te Konzentration \n\n</li>
!!    <li> \f$ t \f$ = Zeit\n\n</li>
!!    <li> \f$ x_i \f$ = Ortskoordinaten in den mit i indizierten 3 Raumrichtungen\n\n</li>
!!    <li> \f$ v_i \f$ = Strömungsgeschwindigkeiten in den mit i indizierten 3 Raumrichtungen (Einsteinsche Summationskonvention); \n
!!         siehe dazu >> \subpage advect \n\n</li>
!!    <li> \f$ D_{ij} \f$ = Diffusivitätstensor (symetrisch), \n
!!         meist als isotrop angenommen \f$ D_{ij} = d \cdot \delta_{ij} \f$ mit \f$ d\f$=Diffusivität und \f$\delta_{ij}\f$=Einheitstensor; \n
!!         siehe dazu >> \subpage diff \n\n</li>
!!    <li> \f$ Q_m \f$ = Quelle/Senke der Konzentration m (lokale Veränderungsrate, Stoffumsatz); \n
!!          siehe dazu >> \ref Num_Umsatz \n\n</li>
!!    <li> \f$ w_{s,3} \f$ = Sinkgeschwindigkeit in der vertikalen (3) Raumrichtung;\n
!!          siehe dazu >> \subpage sinken \n\n</li>
!! </ul>
!! \n\n
!! eindimensional
!! \n\n
!! \f[
!! \frac{\partial c}{\partial t} = - v \frac{\partial c}{\partial x} + \frac{\partial}{\partial x} D \frac{\partial c}{\partial x} + Quelle/Senke(Stoffumsatz)
!! \f]
!! \n\n
!! \f[
!! \frac{\partial v}{\partial t} = - v \frac{\partial v}{\partial x} + \frac{\partial}{\partial x} D \frac{\partial v}{\partial x} - Wasserspiegel(Druck)gradient + Sohlreibung
!! \f]
!! \f$\frac{\partial c}{\partial x}\f$ ist groß
!! \f$\frac{\partial v}{\partial x}\f$ ist klein
!! \n\n
!! Der Transport einer Konzentrationen im fließenden Wasser
!! setzt sich aus \ref advect und \ref diff zusammen. D. h. die in einem imaginären Wassertropfen gelösten Inhalte werden
!! mit der mittleren Strömungsgeschwindigkeit entlang einer Bahnlinie verdriftet (\ref advect) und
!! durch die Turbulente Wirbelbewegung mit benachbarten "Wassertropfen" vermischt (\ref diff).
!! \n\n
!! Die komplett ausgeschriebene Transportgleichung inclusive der aus dem Stoffumsatz stammenden Quellen und Senken
!! finden Sie im Abschnitt \ref numerik .
!! \n\n
!! Der Fractional Step Algorithmus, der in T-QSim realisiert ist (siehe \ref numerik), geht dabei von der Annahme aus, dass
!! die Konzentrationsänderungen, die während eines Zeitschritts infolge
!! von stoffumsatz() stattfinden, mit hinreichender Genauigkeit vor dem Transportvorgang berechnet werden können.
!! Damit diese Näherung zutrifft, muss der stoffumsatz() am Strombahnurspung
!! annähernd genauso stattfinden wie entlang des Transportweges.
!!  \n\n
!! Die Verwendung von unterschiedlichen Netzen für die Strömungsberechnung und die Gütesimulation
!! ist zunächst nicht weiterverfolgt worden; würde sie angewendet, müsste eine Zwischeninterpolatin erfolgen
!! z. B. mit dem Programm "Raster" von Herrn Wirth.
!!  \n\n
!! zurück: \ref numerik ;  Quelle: stofftransport.f95
!----+-----+----
!> Subroutine stofftransport() macht jetzt nur noch die Verzweigung zu den unterschiedlichen Hydraulischen Treibern und Transportalgorithmen
subroutine stofftransport()
   use modell
   implicit none
   integer :: i, n, j
   do i = 1,part ! all i elements/nodes on this process
      iglob = i + meinrang * part
      if (kontrollknoten == iglob)print*,iglob,meinrang,i,part," vor ph2hplus lf,ph = ",  &
          planktonic_variable_p(65+(i-1)*number_plankt_vari),planktonic_variable_p(66+(i-1)*number_plankt_vari)
   end do
   call ph2hplus()
   call mpi_barrier (mpi_komm_welt, ierr)
   
   select case (hydro_trieb)
      case(1) ! casu-transinfo
         call gather_planktkon()
         if (meinrang == 0) then !! nur prozessor 0
            if (kontrollknoten >= 1) print*,'0  vor stofftransport_casu: ph,lf = ',  &
                planktonic_variable(66+(kontrollknoten-1)*number_plankt_vari),  &
                planktonic_variable(65+(kontrollknoten-1)*number_plankt_vari)
            call stofftransport_casu()
            if (kontrollknoten >= 1) print*,'0 nach stofftransport_casu: ph,lf = ',  &
                planktonic_variable(66+(kontrollknoten-1)*number_plankt_vari),  &
                planktonic_variable(65+(kontrollknoten-1)*number_plankt_vari)
         end if !! nur prozessor 0
         call scatter_planktkon()
      case(2) ! Untrim² netCDF
         call gather_planktkon()
         if (meinrang == 0) then !! nur prozessor 0
            call stofftransport_untrim()
            if (kontrollknoten >= 1) print*,'nach stofftransport_untrim: lf,ph = ',  &
                planktonic_variable(65+(kontrollknoten-1)*number_plankt_vari),  &
                planktonic_variable(66+(kontrollknoten-1)*number_plankt_vari)
         end if !! nur prozessor 0
         call scatter_planktkon()
      case(3) ! SCHISM netCDF
         !! call stofftransport_schism() !parallel and 3D
         case default
         call qerror('stofftransport: Hydraulischer Antrieb unbekannt')
   end select
   call mpi_barrier (mpi_komm_welt, ierr)
   call hplus2ph()
   call mpi_barrier (mpi_komm_welt, ierr)
   call gather_planktkon() ! syncronize non-parallel fields to paralell ones again
   do i = 1,part ! all i elements/nodes on this process
      iglob = (i+meinrang*part)
      if (kontrollknoten == iglob)print*,iglob,meinrang,i,part," nach hplus2ph lf,ph = ",  &
          planktonic_variable_p(65+(i-1)*number_plankt_vari),planktonic_variable_p(66+(i-1)*number_plankt_vari)
   end do
   call mpi_barrier (mpi_komm_welt, ierr)
   if (meinrang == 0) then !! nur prozessor 0
      do j = 1,number_plankt_point ! alle j Knoten
         call tiefenprofil(j)  !! 2D depth avaraged
      end do ! alle j Berechnungsstützstellen
   end if !! nur prozessor 0
   call mpi_barrier (mpi_komm_welt, ierr)
   return
end subroutine stofftransport
!----+-----+----
!> allokieren der Felder für die Transportinformationen.
!! \n\n
subroutine allo_trans()
   use modell
   implicit none
   integer :: alloc_status,j
   if (meinrang == 0) then ! prozess 0 only
      allocate( p(number_plankt_point), stat = alloc_status ) !, tief(number_plankt_point)
      if (alloc_status /= 0) then
         write(fehler,*)' Rueckgabewert   von   allocate p :', alloc_status
         call qerror(fehler)
      end if
      do j = 1,number_plankt_point ! alle j Berechnungsstützstellen
         p(j) = -777.777
      end do ! alle j Berechnungsstützstellen
      allocate( u(number_plankt_point), dir(number_plankt_point), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' Rueckgabewert   von   allocate u :', alloc_status
         call qerror(fehler)
      end if
      allocate( vel_x(number_plankt_point), vel_y(number_plankt_point), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' Rueckgabewert   von   allocate u :', alloc_status
         call qerror(fehler)
      end if
      allocate (inflow(number_plankt_point), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' Rueckgabewert   von   allocate inflow :', alloc_status
         call qerror(fehler)
      end if
      select case (hydro_trieb)
         case(1) ! casu-transinfo
            !call allo_trans(knotenanzahl2D) !! Felder für Transportinformationen und Strömungsfeld allocieren
            allocate (intereck(4*number_plankt_point), stat = alloc_status )
            if (alloc_status /= 0) then
               write(fehler,*)' Rueckgabewert   von   allocate  intereck(4* :', alloc_status
               call qerror(fehler)
            end if
            allocate (wicht(4*number_plankt_point), stat = alloc_status )
            if (alloc_status /= 0) then
               write(fehler,*)' Rueckgabewert   von   allocate  wicht(4* :', alloc_status
               call qerror(fehler)
            end if
            allocate (w(number_plankt_point), stat = alloc_status )
            if (alloc_status /= 0) then
               write(fehler,*)' Rueckgabewert   von   allocate w :', alloc_status
               call qerror(fehler)
            end if
            allocate (ur_x(number_plankt_point),ur_y(number_plankt_point),ur_z(number_plankt_point), stat = alloc_status )
            if (alloc_status /= 0)call qerror('allocate (ur_ failed')
         case(2) ! Untrim² netCDF
            !call allo_trans(n_elemente) !! Felder für Transportinformationen und Strömungsfeld allocieren
            allocate( el_vol(number_plankt_point), el_area(number_plankt_point), stat = alloc_status )
            if (alloc_status /= 0)call qerror('allocate (el_vol(number_plankt_point), el_area(number_plankt_point)) failed')
            allocate( ed_flux(kantenanzahl), ed_area(kantenanzahl), stat = alloc_status )
            if (alloc_status /= 0)call qerror('allocate( ed_area(kantenanzahl) ) failed')
            allocate( ed_vel_x(kantenanzahl), ed_vel_y(kantenanzahl), stat = alloc_status )
            if (alloc_status /= 0)call qerror('allocate( ed_vel(kantenanzahl) ) failed')
            allocate (wicht(5*number_plankt_point), stat = alloc_status )
            if (alloc_status /= 0) then
               write(fehler,*)' Rueckgabewert   von   allocate  wicht(5* :', alloc_status
               call qerror(fehler)
            end if
            allocate (cu(number_plankt_point), stat = alloc_status )
         case(3) ! SCHISM netCDF
            print*,'####### allo_trans SCHISM macht noch nix spezielles VORSICHT #######'
            case default
            call qerror('allo_trans: Hydraulischer Antrieb unbekannt')
      end select
   end if ! only prozessor 0
   return
end subroutine allo_trans
!----+-----+----
!> calculate proton concentration from pH
!! \n\n
subroutine ph2hplus()
   use modell
   implicit none
   real*8 mue,ph,lf,hplus,hk,lgh
   integer i
   real(8) :: mue, ph, lf, hplus, hk
   integer :: i
   
   do i = 1,part ! all i elements/nodes on this process
      iglob = i + meinrang * part
      lf = planktonic_variable_p(65 + (i-1) * number_plankt_vari)
      ph = planktonic_variable_p(66 + (i-1) * number_plankt_vari)
      mue = sqrt(max(0.0, 1.7e-5 * lf))
      hk = mue / (2. + 2.8 * mue)
      hplus = 10**(hk - ph)
      planktonic_variable_p(66+(i-1)*number_plankt_vari) = hplus
      if (iglob == kontrollknoten) print*, meinrang, i, ' ph2hplus: ph, hplus, part, number_plankt_vari = ',  &
          ph, hplus, part, number_plankt_vari
   end do ! all i elements/nodes on this process
   
   return
end subroutine ph2hplus
!----+-----+----
!> calculate pH from proton concentration
!! \n\n
subroutine hplus2ph()
   use modell
   implicit none
   real(8) :: mue, ph, lf, hplus, hk
   integer :: i
   
   do i = 1,part ! all i elements/nodes on this process
      iglob = i + meinrang * part
      lf = planktonic_variable_p(65 + (i-1) * number_plankt_vari)
      hplus = planktonic_variable_p(66 + (i-1) * number_plankt_vari)
      mue = sqrt(max(0.0, 1.7e-5 * lf))
      hk = mue / (2. + 2.8 * mue)
      ph = hk - log10(hplus)
      planktonic_variable_p(66+(i-1)*number_plankt_vari) = ph
      if (iglob == kontrollknoten) print*, meinrang, i, ' hplus2ph: ph, hplus, part, number_plankt_vari = ',  &
          ph, hplus, part, number_plankt_vari
   end do ! alle j elements/nodes
   
   return
end subroutine hplus2ph
