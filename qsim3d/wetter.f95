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
!> \page wetter_rb Wetter-Randbedingungen
!!
!! Die Wetter-Randbedingungen (meteorologischen Bedingungen) werden für die \ref lnk_wtemp
!! und das Strahlungsklima, das die Algen (\ref Licht_algen ) erfahren, benötigt.
!! \n
!! <h2>Wetter-Parameter</h2>
!! Diese Wetterdaten werden bei der eingabe() von Subroutine wetter_readallo0() aus der Datei
!! <a href="./exp/WETTER.txt" target="_blank">WETTER.txt</a> gelesen.
!!
!!<table >
!!<tr><th>Spalte WETTER.txt</th><th> 1D-Name </th><th> 3D-Stationswert       </th><th> Nr. \ref uebergabe_werte    </th><th> Beschreibung       </th><th> Einheit </th></tr>
!!<tr><td>1</td><td>\anchor glob glob   </td><td>             </td><td> -             </td><td>
!!                                                                                                              Globalstrahlung von strahlg() berechnet </td><td> cal/(cm2*h) </td></tr>
!!<tr><td>2</td><td> tlmax       </td><td> \anchor tlmax_T tlmax_T   </td><td> -            </td><td>
!!                                                            Tagesmaximum der Lufttemperatur (bei der Verwendung von Tagesmittelwerten sonst Zeitwert) </td><td>  Grad Celsius  </td></tr>
!!<tr><td>3</td><td> tlmin        </td><td> \anchor tlmin_T tlmin_T   </td><td> -            </td><td>
!!                                                              Tagesminimum der Lufttemperatur (nur bei der Verwendung von Tagesmittelwerten verwendet)</td><td>  Grad Celsius  </td></tr>
!!<tr><td>4</td><td>\anchor RO RO   </td><td> \anchor ro_T ro_T      </td><td> 63             </td><td> Luftfeuchte       </td><td> % </td></tr>
!!<tr><td>5</td><td>\anchor WGE WGE    </td><td> \anchor wge_T wge_T      </td><td> 65             </td><td> Windgeschwindigkeit    </td><td> m/s </td></tr>
!!<tr><td>6</td><td>\ref cloud cloud </td><td> \anchor cloud_T cloud_T   </td><td> 66             </td><td> Bewölkungsdichte    </td><td> Achtel </td></tr>
!!<tr><td>7</td><td>\ref typw typw    </td><td> \anchor typw_T typw_T      </td><td> 67             </td><td> Wolkentyp       </td><td>
!!                                   siehe <a href="./pdf/QSimDoku_5Strahlung.pdf" target="_blank">Dokumentation Strahlung</a> </td></tr>
!!<tr><td>-</td><td>\ref templ templ </td><td> \anchor tlmed_T tlmed_T    </td><td> 62             </td><td> aktuelle Lufttemperatur aus temperl_wetter()    </td><td> Grad Celsius </td></tr>
!!<tr><td>-</td><td>\anchor SCHWI SCHWI </td><td> \anchor schwi_T schwi_T   </td><td> 64             </td><td>
!!                 Globalstrahlung an der Wasseroberflaeche unter Beruecksichtigung der Reflektion an der Wasseroberflaeche aus strahlg_wetter()   </td><td> cal/(cm2*h) </td></tr>
!!</table>\n
!! Der Eintrag der Wetter-Randbedingungen in die Felder der \ref uebergabe_werte wird von temperw_huelle() nur deswegen vorgenommen,
!! damit diese Werte an jeder Berechnungsstützstelle (Knoten oder Element) ausgegeben werden können. \n
!!
!! <h2>Zuordnung im Modell</h2>
!! Die Zuordnung der 3D-Stationswerte zu den Berechnungsstützstellen geschieht über das Feld
!! \ref wetterstations_nummer, das basierend auf den Angaben in den T-Zeilen von <a href="./exp/MODELLG.3D.txt" target="_blank">MODELLG.3D.txt</a>,
!! zu jedem Zonen-zähler einen Wetterstations-Zähler bereitstellt.
!! Und jede Berechnungsstützstelle weiß (\ref knoten_zone oder \ref element_zone \ref point_zone) in welcher Zone sie liegt.
!!
!! <h2>Aufbereitung der Randvorgaben für den jeweiligen Zeitschritt</h2>
!! Die aktuellen Wetterdaten für Waermebilanz im jeweiligen Zeitschritt werden in randbedingungen_setzen() ermittelt. Dabei werden die folgenden Subroutinen verwendet:\n
!! call wettles_wetter()  ! ersetzt wettles(), interpoliert Wetterdaten für den aktuellen Zeitpunkt ### Änderungen in 13.3 nicht geprüft ###\n
!! call temperl_wetter()  ! ersetzt Temperl(), berechnet die aktuelle Lufttemperatur und legt sie in \ref tlmed_T ab. ### Änderungen in 13.3 nicht geprüft ###\n
!! call strahlg_wetter()  ! dient dem Aufruf von strahlg();
!!                          berechnet aus der Globalstrahlung \ref glob  den Strahlungsanteil \ref SCHWI, der im Gewässer ankommt. ### Änderungen in 13.3 nicht geprüft ###
!!
!! <h2>Wärmeeinleitungen</h2>
!! Im 1-dimensionalen QSim besteht die Möglichkeit Wärmeeinleitungen z.B. durch Kraftwerke direkt in der Temperaturberechnung
!! zu berücksichtigen. Im mehrdimensionalen QSim3D muss dies über Randbedingungen vorgegeben werden. D. h. einen Ausströmrand
!! an dem der Volumenstrom entnommen wird und einem Einströmrand, an dem das erwärmte Wasser ins Gewässer (Modellgebiet) zurückfließt.
!!
!! <h2>geänderte Feldindizierung QSim1D - QSim3D</h2>
!! In QSim1D werden die Wetterdaten unter der Stations-Kennnummer abgespeichert. In QSim3D unter dem Stationszähler.\n
!! Dadurch können auch hohe Kenn-Nummern verwendet werden, ohne unnötig Speicherplatz zu verschwenden.\n
!! Um die QSim-Routinen benutzen zu können ist das Feld iWSta erhalten geblieben; es speichert in QSim3D aber den Zähler.
!! Für die Kennnummer aus WETTER.txt ist ein neues Feld "Wetterstationskennung" eingeführt worden.
!! \n\n
!! zurück: \ref zuflussranddaten, \ref lnk_wtemp oder \ref Modellerstellung ; Quelle: wetter.f95
! QSIM.f90:
!~~~~~~~~~~~~~~ Weg der Ufervegetationsparameter von Eingabe MODELLG bis TEMPERW
!1190      open(unit=103, DEFAULTFILE=cpfad, file='MODELLG.txt')
!1309      if(ckenn.eq.'O')then ... read(77,1040)aVeg(mstr,mV),eVeg(mstr,mV),(VTYPA(mstr,mV,iV)       &
!         &,iV=1,6),VALTAL(mstr,mV),EDUFAL(mstr,mV)                          &
!         &,(VTYPA(mstr,mV,iV),iV=7,12),VALTAR(mstr,mV),EDUFAR(mstr,mV)      &
!         &,(VTYPA(mstr,mV,iV),iV=13,14)
!1542      EDUFLH(mstr,mSta) = EDUFAL(mstr,mV)
!3932      EDUFBL(jR) = EDUFLH(mstr,ior)
!4015      call strahlg(glob,uhrz,sa,su,schwi,tflie,geol,tdj,geob,dk         &
!         &,cloud,schwia,imet,mstr,IDWe,itags,monats,VTYP,VALTBL,EDUFBL      &
!         &,VALTBR,EDUFBR,breite,anze,ifehl,ifhStr)
!
!~~~~~~~~~~~~~~ Alle an der Wärmebilanz beteiligten Subroutinen ??
!1074  740 open(unit=86, DEFAULTFILE=cpfad, file='WETTER.txt')
!1759      9999 CONTINUE ! Beginn eines neuen Zeitschritts
!2527      call sasu(itags,monats,geob,geol,sa,su,zg,zlk,dk,tdj)
!2546      call wettles(itags,monats,jahrs,uhrz,uhrn,itagw,monatw,jahrw,uhrzw&
!         &,wertw,glob,tlmax,tlmin,ro,wge,cloud,typw,mwetts                  &
!         &,imet,iWSta,iwetts)
!3689      do 8888 azStr = 1,azStrs.... !Strangschleife
!4015      call strahlg(glob,uhrz,sa,su,schwi,tflie,geol,tdj,geob,dk         &
!         &,cloud,schwia,imet,mstr,IDWe,itags,monats,VTYP,VALTBL,EDUFBL      &
!         &,VALTBR,EDUFBR,breite,anze,ifehl,ifhStr)
!4020      call Temperl(SA,SU,Uhrz,TEMPL,mstr,IDWe,TLMAX,TLMIN,anze,imet)
!5803      call temperw(RO,TEMPL,TEMPW,SCHWI,WGE,TIEFE,TFLIE                 &
!         &,vmitt,flag,elen,ior,anze,etemp,ewaerm,typ,qeinl,vabfl            &
!         &,jiein,cloud,typw,iwied,uhrz,ilbuhn,nwaerm,fkm,nkzs               &
!         &,tempwz,dH2D,iorLa,iorLe,ieinLs,flae,qeinlL,etempL                &
!         &,mstr,IDWe,ilang,dtemp,FluxT1,extk,itags,monats,Tsed              &
!         &,Wlage,hWS,iRHKW)
!---------------------------------------------------------------------------------------------------------------
!> Wetter Randbedingungen auf allen Prozessen allocieren und verteilen\n
!! \n\n
subroutine wetter_parallel()  ! called from all processes randbedingungen_parallel()
   use modell
   implicit none
   integer :: alloc_status
   call MPI_Bcast(IWETTs_T,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(IMET_T,1,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(mwettmax_T,1,MPI_INT,0,mpi_komm_welt,ierr)
   if (meinrang == 0)print*,'meinrang, IWETTs_T, IMET_T, mwettmax_T'
   print*, meinrang, IWETTs_T, IMET_T, mwettmax_T
   call mpi_barrier (mpi_komm_welt, ierr)
   if (meinrang /= 0) then ! alle Prozesse ausser 0
      allocate (Wetterstationskennung_T(IWETTs_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' allocate faile in wetter_parallel Wetterstationskennung_T(IWETTs_T) :'  &
                                                                                             , meinrang, alloc_status
         call qerror(fehler)
      end if
      allocate (iWSta_T(IWETTs_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' allocate faile in wetter_parallel iWSta :', alloc_status
         call qerror(fehler)
      end if
      allocate (mwetts_T(IWETTs_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' allocate faile in wetter_parallel mwetts :', alloc_status
         call qerror(fehler)
      end if
      allocate (itagw_T(IWETTs_T,mwettmax_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' allocate faile in wetter_parallel itagw :', alloc_status
         call qerror(fehler)
      end if
      allocate (monatw_T(IWETTs_T,mwettmax_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' allocate faile in wetter_parallel monatw :', alloc_status
         call qerror(fehler)
      end if
      allocate (jahrw_T(IWETTs_T,mwettmax_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' allocate faile in wetter_parallel jahrw :', alloc_status
         call qerror(fehler)
      end if
      allocate (uhrzw_T(IWETTs_T,mwettmax_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' allocate faile in wetter_parallel uhrzw_T:', alloc_status
         call qerror(fehler)
      end if
      allocate (zeitpunktw(IWETTs_T,mwettmax_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' allocate faile in wetter_parallel zeitpunktw:', alloc_status
         call qerror(fehler)
      end if
      allocate (wertw_T(IWETTs_T,7,mwettmax_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' allocate faile in wetter_parallel wertw :', alloc_status
         call qerror(fehler)
      end if
      !     allokieren der Felder für die Momentan-Werte
      allocate (glob_T(IWETTs_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' allocate faile in wetter_parallel glob_T :', alloc_status
         call qerror(fehler)
      end if
      allocate (tlmax_T(IWETTs_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' allocate faile in wetter_parallel tlmax_T :', alloc_status
         call qerror(fehler)
      end if
      allocate (tlmin_T(IWETTs_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' allocate faile in wetter_parallel tlmin_T :', alloc_status
         call qerror(fehler)
      end if
      allocate (tlmed_T(IWETTs_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' allocate faile in wetter_parallel tlmin_T :', alloc_status
         call qerror(fehler)
      end if
      allocate (ro_T(IWETTs_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' allocate faile in wetter_parallel ro_T :', alloc_status
         call qerror(fehler)
      end if
      allocate (wge_T(IWETTs_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' allocate faile in wetter_parallel wge_T :', alloc_status
         call qerror(fehler)
      end if
      allocate (cloud_T(IWETTs_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' allocate faile in wetter_parallel cloud_T :', alloc_status
         call qerror(fehler)
      end if
      allocate (typw_T(IWETTs_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' allocate faile in wetter_parallel typw_T :', alloc_status
         call qerror(fehler)
      end if
      allocate (schwi_T(IWETTs_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' allocate faile in wetter_parallel strahlung :', alloc_status
         call qerror(fehler)
      end if
   end if !! alle Prozesse ausser 0
   call MPI_Bcast(Wetterstationskennung_T,IWETTs_T,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(iWSta_T,IWETTs_T,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(mwetts_T,IWETTs_T,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(itagw_T,IWETTs_T*mwettmax_T,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(monatw_T,IWETTs_T*mwettmax_T,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(jahrw_T,IWETTs_T*mwettmax_T,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(uhrzw_T,IWETTs_T*mwettmax_T,MPI_FLOAT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(zeitpunktw,IWETTs_T*mwettmax_T,MPI_INT,0,mpi_komm_welt,ierr)
   call MPI_Bcast(wertw_T,IWETTs_T*7*mwettmax_T,MPI_FLOAT,0,mpi_komm_welt,ierr)
   !call MPI_Bcast(,,MPI_,0,mpi_komm_welt,ierr)
   return
end subroutine wetter_parallel
!----+-----+----
!> Dient der eingabe() von  Wetterdaten aus <a href="./exp/WETTER.txt" target="_blank">WETTER.txt</a>.\n
!! In QSim-3D können die selben Dateien verwendet werden wie in QSim-1D.\n
!! Die Wetterdaten sind die wesentlichen Randbedingungen für die Berechnung der Wärmebilanz mittels temperw_huelle(),
!! deren Resultat die Temperaturverteilung im Wasserkörper ist.\n\n
!! \n\n
subroutine wetter_readallo0()  ! called only from process 0 (eingabe)
   use modell
   implicit none
   character(300) dateiname, text
   character(300) systemaufruf
   !      integer iWETTs, IMET, iWSta(20), mwetts(20), itagw(20,10000), monatw(20,10000), jahrw(20,10000)
   !      real wertw(7,10000), uhrzw(20,10000)
   integer :: alloc_status , dealloc_status, flag, open_error, io_error ,i,j
   integer :: ifehl_T, ifhStr_T, ixw_T, mwett_T, iWETT_T
   real hcTmx2_T, dummreal
   character(300) txt
   integer ANZT,sysa
   real geob,geol,sa,su,zg,zlk,dk
   logical logi,existing_station
   real t1, t2, dlt  !delta
   if (meinrang == 0) then ! nur auf Prozessor 0 bearbeiten
      write(dateiname,'(2A)')trim(modellverzeichnis),'WETTER.txt'
      open(unit = 86, file = dateiname, status = 'old', action = 'read ', iostat = open_error )
      if (open_error /= 0) then
         write(fehler,*)'Die Datei WETTER.txt läßt sich nicht öffnen'
         call qerror(fehler)
      end if ! open_error.ne.0
      rewind (86)
      ! vier Zeilen Dateikopf:Versionsname, Modellnahme, Ereignisnahme, Anzahl der Wetterstationen(Datenblöcke)IWETTs
      read(86,'(A40)', iostat = io_error) VERSION_T
      if (io_error /= 0) then
         write(fehler,*)'io_error VERSION_T subroutine wetter'
         call qerror(fehler)
      end if ! open_error.ne.0
      read(86,'(A40)', iostat = io_error) MODNAME_T
      if (io_error /= 0) then
         write(fehler,*)'io_error MODNAME subroutine wetter'
         call qerror(fehler)
      end if ! open_error.ne.0
      !print*,'MODNAME=', MODNAME
      read(86,'(A40)', iostat = io_error)ERENAME_T
      if (io_error /= 0) then
         write(fehler,*)'io_error ERENAME subroutine wetter'
         call qerror(fehler)
      end if ! io_error.ne.0
      print*,'WETTER.txt MODELLNAME:',trim(MODNAME_T), '  EREIGNISNAME:', trim(ERENAME_T)
      read(86,*,iostat = io_error)IWETTs_T,IMET_T
      if (io_error /= 0) then
         write(fehler,*)'Lesefehler wetter() IWETTs,IMET'
         call qerror(fehler)
      endif
      if (IMET_T /= 1) then
         write(text,'(A)')'Tagesmittelwerten'
      else
         write(text,'(A)')'Stundenwerten'
      end if ! IMET
      print*, IWETTs_T, ' Wetterstationen mit ', trim(text)
      allocate (Wetterstationskennung_T(IWETTs_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' Rueckgabewert   von   allocate Wetterstationskennung :', alloc_status
         call qerror(fehler)
      end if
      allocate (iWSta_T(IWETTs_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' Rueckgabewert   von   allocate iWSta :', alloc_status
         call qerror(fehler)
      end if
      allocate (mwetts_T(IWETTs_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' Rueckgabewert   von   allocate mwetts :', alloc_status
         call qerror(fehler)
      end if
      ! 1. Schleife über alle Wetterstationen(Datenblöcke)
      ! nur Datenblockköpfe lesen, um Dimensionierung zu ermitteln ... allokieren.
      mwettmax_T = 0
      do 7227 iWETT_T = 1,iWETTs_T ! Schleife über alle Wetterstationen
         read(86, * ,iostat = io_error )Wetterstationskennung_T(iwett_T),mWetts_T(iwett_T)
         if (io_error /= 0) then
            write(fehler,*)'Lesefehler wetter() iWSta(iwett),mWetts(iwett)'
            call qerror(fehler)
         else
            iWSta_T(iwett_T) = iwett_T
            !print*,'Wetterstation(', iWSta_T(iwett_T), ') #', Wetterstationskennung_T(iwett_T),' , ', mWetts_T(iwett_T),' Werte'
            print*, iWETT_T,'te Wetterstation soll ',mWetts_T(iwett_T),' Werte haben'
         endif
         if (mWetts_T(iwett_T) >= mwettmax_T)mwettmax_T = mWetts_T(iwett_T)
         if (mWetts_T(iwett_T) <= 0) then
            print*,'WETTER.txt: keine Werte an Wetterstation ',iwett_T, 'geht nicht'
         endif
         do 7123 mWett_T = 1,mWetts_T(iwett_T)
            read(86,*,iostat = io_error )txt !! dummy einlesen um Felddimensionen zu ermitteln
         7123    continue
      7227 continue! ende Schleife über alle Wetterstationen
      print*, 'Wetterstationen haben maximal ',mwettmax_T,' Werte.'
      allocate (itagw_T(IWETTs_T,mwettmax_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' Rueckgabewert   von   allocate itagw :', alloc_status
         call qerror(fehler)
      end if
      allocate (monatw_T(IWETTs_T,mwettmax_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' Rueckgabewert   von   allocate monatw :', alloc_status
         call qerror(fehler)
      end if
      allocate (jahrw_T(IWETTs_T,mwettmax_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' Rueckgabewert   von   allocate jahrw :', alloc_status
         call qerror(fehler)
      end if
      allocate (uhrzw_T(IWETTs_T,mwettmax_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' Rueckgabewert   von   allocate uhrzw_T:', alloc_status
         call qerror(fehler)
      end if
      allocate (zeitpunktw(IWETTs_T,mwettmax_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' Rueckgabewert   von   allocate zeitpunktw:', alloc_status
         call qerror(fehler)
      end if
      allocate (wertw_T(IWETTs_T,7,mwettmax_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' Rueckgabewert   von   allocate wertw :', alloc_status
         call qerror(fehler)
      end if
      !                                                          dlt=2.0 ! 87 delta
      !write(dateiname,'(2A)')trim(modellverzeichnis),'WETTER.delta_2.0.txt' ! 87 delta
      !write(systemaufruf,'(2A)')'rm -rf ',trim(dateiname)
      !call system(systemaufruf,sysa)
      !if(sysa.ne.0) then
      !   print*,'entfernen der alten WETTER.delta.txt schlug fehl'
      !   stop
      !end if ! sysa.ne.0
      !open(unit=87, file=dateiname, status ='new', action ='write', iostat = open_error ) ! 87 delta
      !if(open_error.ne.0) then ! 87 delta
      !   print*,'Die Datei WETTER.delta.txt läßt sich nicht öffnen' ! 87 delta
      !   stop ! 87 delta
      !end if ! open_error.ne.0 ! 87 delta
      !rewind (87) ! 87 delta
      ! 2. Lesevorgang über WETTER.txt
      rewind (86)
      read(86,*,iostat = io_error )txt !! Datei Kopf Überlesen
      !write(87,'(A40)')adjustl(VERSION_T) ! 87 delta
      read(86,*,iostat = io_error )txt !! Datei Kopf Überlesen
      !write(87,'(A40)')adjustl(MODNAME_T) ! 87 delta
      read(86,*,iostat = io_error )txt
      !write(87,'(A40)')adjustl(ERENAME_T) ! 87 delta
      read(86,*,iostat = io_error )txt
      !write(87,'(I1,2x,I1)')IWETTs_T,IMET_T  ! 87 delta
      do 227 iWETT_T = 1,iWETTs_T
         read(86,*,iostat = io_error )txt !! Block Kopf Überlesen
         !write(87,'(I8,2x,I5)')Wetterstationskennung_T(iwett_T),mWetts_T(iwett_T)  ! 87 delta
         !qsim.f90_13.10 read(86,'(I8,2x,I5)',end=161)IWSta(iwett),mWetts(iwett)
         if (IMET_T /= 1) then
            ! Tagesmittelwerte
            hcTmx2_T = -999.
            do 123 mWett_T = 1,mWetts_T(iwett_T) !! Wetterdaten an der jeweiligen Station für den jeweiligen Tag lesen ...
               !read(86,2013,iostat=io_error )itagw(iwett,mwett),monatw(iwett,mwett) &
               logi = zeile(86)
               read(ctext,*,iostat = io_error )itagw_T(iwett_T,mwett_T),monatw_T(iwett_T,mwett_T) &
                    ,jahrw_T(iwett_T,mwett_T),uhrzw_T(iwett_T,mwett_T), (wertw_T(iwett_T,ixw_T,mwett_T),ixw_T = 1,7)
               if (io_error /= 0) then
                  print*,trim(ctext)
                  write(fehler,*)'Lesefehler in obiger Zeile von WETTER.txt'
                  call qerror(fehler)
               else
                  t1 = wertw_T(iwett_T,2,mwett_T)
                  t2 = wertw_T(iwett_T,3,mwett_T)
                  if (t1 <= -99.98) then
                     t1 = -99.99
                  else
                     t1 = t1 + dlt
                  endif
                  if (t2 <= -99.98) then
                     t2 = -99.99
                  else
                     t2 = t2 + dlt
                  endif
                  !write(87,2023)itagw_T(iwett_T,mwett_T),monatw_T(iwett_T,mwett_T)  &
                  !             ,jahrw_T(iwett_T,mwett_T),uhrzw_T(iwett_T,mwett_T) &
                  !,wertw_T(iwett_T,1,mwett_T) &
                  !,t1 , t2  &  !delta
                  !,(wertw_T(iwett_T,ixw_T,mwett_T),ixw_T=4,7)
                  2023 format(i2,2x,i2,2x,I4,2x,f5.2,2x  &
                  ,f7.2,2x,  f6.2,2x,f6.2,2x,  f6.2,2x,f6.2,2x,f4.1,2x,f4.1)
               endif
               if (wertw_T(iwett_T,3,mwett_T) > hcTmx2_T)hcTmx2_T = wertw_T(iwett_T,3,mwett_T)
               ! if(wertw(iwett,3,mwett).gt.hcTmx2)hcTmx2 = wertw(iwett,3,mwett)
               uhrzw_T(iwett_T,mwett_T) = 12.0
            123 continue
            ! Fehlermeldung keine Minimumtemperaturen an einer oder mehrer Wetterstationen
            if (hcTmx2_T == (-1.)) then
               ifehl_T = 4
               ifhStr_T = IWETT_T
               ! goto 989 -->Fehlermeldung
               write(fehler,*)'Fehler wetter_qsim hcTmx2 == (-1.)'
               call qerror(fehler)
            endif
         else
            ! Zeitreihe (Stundenmittelwerte (IMET_T.eq.1) )
            do 124 mWett_T = 1,mWetts_T(iwett_T) !! Wetterdaten an der jeweiligen Station für die jeweilige Stunde lesen ...
               !read(86,2023,iostat=io_error )itagw(iwett,mwett),monatw(iwett,mwett) &
               read(86,*,iostat = io_error )itagw_T(iwett_T,mwett_T),monatw_T(iwett_T,mwett_T) &
                    ,jahrw_T(iwett_T,mwett_T),uhrzw_T(iwett_T,mwett_T) &
                    ,(wertw_T(iwett_T,ixw_T,mwett_T),ixw_T = 1,7)
               if (io_error /= 0) then
                  write(fehler,*)'Lesefehler wetter()itagw(iwett,mwett)....Stundenwerte'
                  call qerror(fehler)
               endif
            124 continue
         endif ! (IMET.ne.1)
         do mWett_T = 1,mWetts_T(iwett_T) !! Zeitpunkte in Sekunden ermitteln:
            if (IMET_T /= 1) then
               !Tagesmittelwerte Zeitpunkt high noon
               uhrzeit_stunde = 12.0
            else
               !Zeitreihe (Stundenmittelwerte (IMET_T.eq.1) ) in der Stundenmitte
               uhrzeit_stunde = uhrzw_T(iwett_T,mwett_T)
            endif ! (IMET.ne.1)
            tag = itagw_T(iwett_T,mwett_T)
            monat = monatw_T(iwett_T,mwett_T)
            jahr = jahrw_T(iwett_T,mwett_T)
            call sekundenzeit(2)
            zeitpunktw(iwett_T,mwett_T) = zeitpunkt
            ! call zeitsekunde(tag, monat, jahr, uhrzeit_stunde, zeitpunktw(iwett_T,mwett_T),tagdesjahres)
            ! print*, 'hin:',itagw_T(iwett_T,mwett_T), monatw_T(iwett_T,mwett_T), jahrw_T(iwett_T,mwett_T) &
            ! &            , 'sec:', zeitpunktw(iwett_T,mwett_T)
            ! &            , 'zurück:',tagdesjahres,tag, monat, jahr
            call zeitsekunde() !! damit auch die Uhrzeit stimmt
            ! print *,"wetter_readallo0: zeitpunktw",jahr, monat, tag, stunde, minute, sekunde, zeitpunktw(iwett_T,mwett_T) &
            !        , wertw_T(iwett_T,2,mwett_T), wertw_T(iwett_T,3,mwett_T)
            ! 229 FORMAT ("wetter_readallo0: ",I4.2,"-",I2.2,"-",I2.2," ",I2.2,":",I2.2,":",I2.2,"    ",F7.2,"    ",F7.2)
         end do !mWett_T
         do mWett_T = 2,mWetts_T(iwett_T) !! Zeitfolge prüfen:
            if (zeitpunktw(iwett_T,mwett_T) <= zeitpunktw(iwett_T,mwett_T-1)) then
               write(fehler,*)' zeitliche Abfolge der Wetterdaten fehlerhaft Station # ',iwett_T, ' Zeile # ', mWett_T &
               ,'tag,monat,jahr:', itagw_T(iwett_T,mwett_T), monatw_T(iwett_T,mwett_T) &
               , jahrw_T(iwett_T,mwett_T),'zeitpunktw(t-1),zeitpunktw(t)'  &
               , zeitpunktw(iwett_T,mwett_T-1), zeitpunktw(iwett_T,mwett_T)
               call qerror(fehler)
            end if
         end do
         print*,'Wetterstation(', iWSta_T(iwett_T), ') #', Wetterstationskennung_T(iwett_T),' , ', mWetts_T(iwett_T),' Werte' &
         ,' Sekundenzeitpunkte von bis',zeitpunktw(iwett_T,1),zeitpunktw(iwett_T,mWetts_T(iwett_T))
      227 continue !! end do alle Wetterstationen
      close (86) !! WETTER.txt schließen
      close (87) !! WETTER.delta.txt schließen
      !     allokieren der Felder für die Momentan-Werte
      allocate (glob_T(IWETTs_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' Rueckgabewert   von   allocate glob_T :', alloc_status
         call qerror(fehler)
      end if
      allocate (tlmax_T(IWETTs_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' Rueckgabewert   von   allocate tlmax_T :', alloc_status
         call qerror(fehler)
      end if
      allocate (tlmin_T(IWETTs_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' Rueckgabewert   von   allocate tlmin_T :', alloc_status
         call qerror(fehler)
      end if
      allocate (tlmed_T(IWETTs_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' Rueckgabewert   von   allocate tlmin_T :', alloc_status
         call qerror(fehler)
      end if
      allocate (ro_T(IWETTs_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' Rueckgabewert   von   allocate ro_T :', alloc_status
         call qerror(fehler)
      end if
      allocate (wge_T(IWETTs_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' Rueckgabewert   von   allocate wge_T :', alloc_status
         call qerror(fehler)
      end if
      allocate (cloud_T(IWETTs_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' Rueckgabewert   von   allocate cloud_T :', alloc_status
         call qerror(fehler)
      end if
      allocate (typw_T(IWETTs_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' Rueckgabewert   von   allocate typw_T :', alloc_status
         call qerror(fehler)
      end if
      allocate (schwi_T(IWETTs_T), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)' Rueckgabewert   von   allocate strahlung :', alloc_status
         call qerror(fehler)
      end if
      ! check zone <-> weather-station connectivity
      do i = 1,zonen_anzahl
         existing_station = .false.
         do j = 1,iWETTs_T
            if ( zone(i)%wettstat%wetterstations_nummer == Wetterstationskennung_T(j)) then
               existing_station = .true.
               zone(i)%wettstat%wetterstations_nummer = j
            end if ! Wetterstation, die von der Zone in MODELLG.3D.txt in WETTER.txt vorhanden
         end do ! alle wetterstationen
         if ( .not. existing_station) then
            write(fehler,*)'Die von Zone ',i ,' mit der Zonen-Kennnummer = ',zone(i)%zonen_nummer,  &
                 ' benötigte Wetterstation mit der Kennung '&
                 ,zone(i)%wettstat%wetterstations_nummer,'ist in WETTER.txt nicht vorhanden'
            call qerror(fehler)
         else ! existing_station
            print*, "Der",i,"-ten Zone mit der (Kenn)-Nummer = ",zone(i)%zonen_nummer," wurde die "      &
                  , zone(i)%wettstat%wetterstations_nummer,"-te Wetterstation mit der (Kenn)-Nummer = "  &
                  , Wetterstationskennung_T(zone(i)%wettstat%wetterstations_nummer), "zugeordnet."
         end if ! nicht vorhanden
      end do ! alle Zonen
      !
      
      !?! ..Ermittlung der Wetterdaten für den Zeitschritt
      !?
      !? 9191 continue
      !?      if(iwsim.eq.4)goto 344
      !?      hconuh = uhrz
      !?      uhrn = uhrz+tflie*24.
      !?      call wettles(itags,monats,jahrs,uhrz,uhrn,itagw,monatw,jahrw,uhrzw&
      !?     &,wertw,glob,tlmax,tlmin,ro,wge,cloud,typw,mwetts                  &
      !?     &,imet,iWSta,iwetts)
      !?!
      !?      uhrz = hconuh
      !?!
      !?  344 continue
   end if !! nur prozessor 0
   return
end subroutine wetter_readallo0
!----+-----+----
!> update_weather \n
!! Wetterdaten für Waermebilanz in diesem Zeitschritt\n
!! runs at all processes parallel;
!! \n\n
subroutine update_weather()
   use modell
   implicit none
   integer i, nuzo, i2
   ! update weather station values *_T values
   call wettles_wetter()  ! ersetzt wettles(), interpoliert Wetterdaten für den aktuellen Zeitpunkt
   call temperl_wetter()  ! ersetzt Temperl(), berechnet Lufttemperatur und legt sie in tlmax_T ab.
   call strahlg_wetter()  ! berechnet aus der Globalstrahlung den Strahlungsanteil, der im Gewässer ankommt.
   do i2 = 1,IWETTs_T   !! Schleife über alle Wetterstationen
      if ((kontrollknoten > 0) .and. (meinrang == 0))      &
          print*,i2,meinrang," update_weather: tlmed_T,ro_T,schwi_T,wge_T,cloud_T,typw_T = ",  &
          tlmed_T(i2),ro_T(i2),schwi_T(i2),wge_T(i2),cloud_T(i2),typw_T(i2)
   end do ! i Schleife über alle Wetterstationen
   ! transfer to nodes via transfer_quantity_p array
   do i = 1,part ! Alle Knoten auf diesem Prozessor
      iglob = (i+meinrang*part)
      if (iglob <= number_plankt_point) then ! Knotennummer existiert (letzter Prozess)
         i2 = zone(point_zone(iglob))%wettstat%wetterstations_nummer !! ist parallel !!!
         transfer_quantity_p(62+(i-1)*number_trans_quant) = tlmed_T(i2) ! air temp.
         transfer_quantity_p(63+(i-1)*number_trans_quant) = ro_T(i2)    ! humidity at node from weather station
         transfer_quantity_p(64+(i-1)*number_trans_quant) = schwi_T(i2) ! radiation received by water
         transfer_quantity_p(65+(i-1)*number_trans_quant) = wge_T(i2)   ! wind speed in m/s
         transfer_quantity_p(66+(i-1)*number_trans_quant) = cloud_T(i2) ! cloud cover in 1/8
         transfer_quantity_p(67+(i-1)*number_trans_quant) = typw_T(i2)  ! cloud type
         if (iglob == kontrollknoten)print*,'update_weather: ',tlmed_T(i2),schwi_T(i2),i2,iglob,i,' auf Prozess #',meinrang
      end if ! Knotennummer existiert(letzter Prozess)
   end do ! all i nodes at this processor
   return
end subroutine update_weather
!----+-----+----
!> Dient der Ermittlung der momentanen Wetterwerte an allen Stationen.
!! ersetzt die QSim Subroutine Wettles()\n\n
!! wird von allen Prozessen aufgerufen\n\n
!! all processes do all weather-stations
!! \n\n
subroutine wettles_wetter()
   use modell
   implicit none
   integer i, j, ipw ,z1,z2
   real b,ywert,w1,w2
   logical found1,found2,wert_gueltig
   !print*,'i : glob_T | tlmax_T | tlmin_T | ro_T | wge_T | cloud_T | typw_T'
   do i = 1,IWETTs_T   !! Schleife über alle Wetterstationen
      do ipw = 1,7   !! Schleife über alle 7 Wetterwerte
         if ( (zeitpunkt < (zeitpunktw(i,1)-43200)) .or. (zeitpunkt > (zeitpunktw(i,mwetts_T(i))+43200)) ) then
            print*,'zum Berechnungszeitpunkt liegen keine Daten an Wetterstation ',i,' vor'
            print*,'zeitpunkt = ', zeitpunkt
            write(fehler,*)'zeitpunktw(i,1) = ',zeitpunktw(i,1),' zeitpunktw(i,mwetts_T(i)) = ',zeitpunktw(i,mwetts_T(i))
            call qerror(fehler)
         end if
         ywert = 0.0
         found1 = .false.
         found2 = .false.
         w1 = 0.0
         w2 = 0.0
         z1 = 0
         z2 = 0
         do j = 1,mwetts_T(i),1 ! alle j zeitintervalle vorwärts
            if ( (zeitpunktw(i,j) <= zeitpunkt) ) then ! bis zum aktuellen Zeitpunkt
               if ( wert_gueltig(ipw,wertw_T(i,ipw,j),imet_t) ) then !gültiger wert
                  found1 = .true.
                  w1 = wertw_T(i,ipw,j)
                  z1 = zeitpunktw(i,j)
               else
                  if ( .not. found1) w1 = wertw_T(i,ipw,j) ! ungültige Werte merken bis gültiger gefunden
               end if !gültiger wert
            end if ! Wert vorher
         end do ! alle j zeitintervalle vorwärts
         do j = mwetts_T(i),1,-1 ! alle j zeitintervalle rückwärts
            if ( (zeitpunktw(i,j) >= zeitpunkt) ) then ! bis zum aktuellen Zeitpunkt
               if ( wert_gueltig(ipw,wertw_T(i,ipw,j),imet_t) ) then !gültiger wert
                  found2 = .true.
                  w2 = wertw_T(i,ipw,j)
                  z2 = zeitpunktw(i,j)
               else
                  if ( .not. found2) w2 = wertw_T(i,ipw,j) ! ungültige Werte merken bis gültiger gefunden
               end if !gültiger wert
            end if ! Wert nachher
         end do ! alle j zeitintervalle rückwärts
         if (found1 .and. found2) then
            b = real(zeitpunkt-z1)/real(z2-z1)
            ywert = (1.0-b)*w1 + b*w2
            !if(ipw.eq.2)print*,'wettles_wetter: tlmax=',ywert,zeitpunkt,w1,z1,w2,z2
         end if
         if (found1 .and. ( .not. found2)) ywert = w1
         if (( .not. found1) .and. found2) ywert = w2
         if (( .not. found1) .and. ( .not. found2)) then
            ywert = w1
            if (ipw == 7) then ! wolkentyp darf negativ = ungültig
               ywert = w1
            else
               write(fehler,*)'wettles_wetter: no valid data at weather station ',i,' for value ',ipw,"  ",w1
               call qerror(fehler)
            end if! wolkentyp
         end if ! nix gefunden
         if (ipw == 1)glob_T(i) = ywert
         if (ipw == 2)tlmax_T(i) = ywert
         if (ipw == 3)tlmin_T(i) = ywert
         if (ipw == 4)ro_T(i) = ywert
         if (ipw == 5)wge_T(i) = ywert
         if (ipw == 6)cloud_T(i) = ywert
         if (ipw == 7)typw_T(i) = ywert
      end do ! Schleife über alle 7 Wetterwerte
      if (meinrang == 0)print*,'wettles_wetter: Wetterstation,zeitpunkt = ',i,zeitpunkt,' Werte = '  &
          ,glob_T(i),tlmax_T(i),tlmin_T(i),ro_T(i),wge_T(i),cloud_T(i),typw_T(i)
   end do ! i Schleife über alle Wetterstationen
end subroutine wettles_wetter
!----+-----+----
logical function wert_gueltig(ipw,wert,imet)
   implicit none
   character (300) :: fehler
   integer ipw,imet
   real wert
   wert_gueltig = .false.
   select case (ipw)
      case(1) ! glob_T(i) = ywert
         if (wert >= 0.0)wert_gueltig = .true.
      case(2) ! tlmax_T(i) = ywert
         if ((wert >= -20.0) .and. (wert <= 50.0))wert_gueltig = .true.
      case(3) ! tlmin_T(i) = ywert
         if ((wert >= -20.0) .and. (wert <= 50.0))wert_gueltig = .true.
         if (imet == 1)wert_gueltig = .true. !! not in use with timeseries data (needed for daily averages imet=0)
      case(4) ! ro_T(i) = ywert
         if (wert >= 0.0)wert_gueltig = .true.
      case(5) ! wge_T(i) = ywert
         if (wert >= 0.0)wert_gueltig = .true.
      case(6) ! cloud_T(i) = ywert
         if ((wert >= 0.0) .and. (wert <= 8.0))wert_gueltig = .true.
      case(7) ! typw_T(i) = ywert
         if (wert >= 0.0)wert_gueltig = .true.
         case default
         write(fehler,*)'wert_gueltig: wrong number in variable ipw',ipw
         call qerror(fehler)
   end select
end function wert_gueltig
!----+-----+----
!> berechnet Lufttemperatur uhrzeitabhängig (cosinus-Verlauf) wenn die Wetterstationsdaten,
!! als Tagesmittelwerte mit Min. und Max.-Temperatur angegeben werden.
!! Bei Stundenwerten wird die Maxtemperatur genommen. Alles andere ist ein Fehler.\n
!! Hüllroutine für Temperl() \n\n
!! wird von allen Prozessen aufgerufen\n
!! all processes do all weather-stations\n
!! \n
subroutine temperl_wetter()
   use modell
   use QSimDatenfelder
   implicit none
   integer i
   real dk, sa, su, zg, zlk, geol, geob
   integer tdj, imet
   real, dimension(20)              :: TLMAX, TLMIN
   do i = 1,IWETTs_T   !! Schleife über alle Wetterstationen
      geol = modell_geol
      geob = modell_geob
      uhrz = uhrzeit_stunde
      tdj = tagdesjahres !! wird von sasu verändert (wozu ist unklar)
      call SASU(tag, monat, geob, geol, sa, su, zg, zlk, dk, tdj, ifehl)
      sonnenaufgang = sa
      sonnenuntergang = su
      IDWe(1,1) = 1
      IDWe(1,2) = 1
      anze = 1
      imet = IMET_T
      mstr = 1
      TLMAX(1) = tlmax_T(i)
      TLMIN(1) = tlmin_T(i)
      call Temperl(SA,SU,Uhrz,TEMPL,mstr,IDWe,TLMAX,TLMIN,anze,imet,azStrs)
      tlmed_T(i) = TEMPL(1)
      if ((kontrollknoten > 0) .and. (meinrang == 0))      &
          print*,'temperl_wetter: Station ',i,' Uhrz,TLMAX,TLMIN,TEMPL',Uhrz,TLMAX(1),TLMIN(1),TEMPL(1)
   end do ! alle Wetterstationen i
end subroutine temperl_wetter
!----+-----+----
!
!> Diese Hüllroutine dient dazu, die QSim-Subroutine strahlg() punktweise aufzurufen.\n\n
!! all processes do all weather-stations\n
!! \n
!!      SUBROUTINE strahlg(glob,uhrz,sa,su,schwi,tflie,geol,tdj           &\n
!!     &,geob,dk,cloud,schwia,imet,mstr,IDWe,itags,monats,VTYP            &\n
!!    &,VALTBL,EDUFBL,VALTBR,EDUFBR,breite,anze,ifehl,ifhStr)            \n
!!                                                             \n
!!     EIN PROGRAMM ZUR BERECHNUNG DER MITTL. GLOBALSTRAHLUNG \n
!!     FUER DEN BERECHNUNGSSCHRITT IN CAL/(CM2*H) AUS DER TAGES- \n
!!     SUMME                                                     \n
!!                                                                \n
!!     AUTOR : VOLKER KIRCHESCH                                    \n
!!                                                                 \n
!!     STAND: 06.11.1987                                           \n
!!                                                                 \n
!!     Parameter:                                                  \n
!!                                                                   \n
!!     SCHWI  - Globalstrahlung an der Wasseroberflaeche unter Berueck-  \n
!!              sichtigung der Reflektion an der Wasseroberflaeche     \n
!!              [cal/(cm2*h)]                                        \n
!!                                                                   \n
!!     SH     - Sonnenhoehe im Bogenmass                             \n
!!     SHGR   - Sonnenhoehe im Gradmass                               \n
!!                                                                    \n
!!     CLOUD  - Bewoelkungsgrad                                      \n
!!
!! Umstellung von Profilweise auf Wetterstationsweise (Zuordnung über Zonen)
!!
subroutine strahlg_wetter()
   use modell
   use QSimDatenfelder
   implicit none
   integer NRV(8)
   real maxi,lt
   real ar(4),br(4),Breite(1000)
   real EVALT(14),EKRBRT(14),EDUFER(14),EVDICH(14)
   real ESLEN(14),ESLENS(14),VTYP(1000,14),VALTBL(1000)
   real EDUFBL(1000),VALTBR(1000),EDUFBR(1000)
   real SHtest(50)
   ! vorher implizit definiert:
   real dk, sa, su, schwia, zg, zlk, geol, geob
   ! hier neu:
   logical printi
   integer i, tdj, azStr
   !
   !integer k1, k2, k3, k4
   !if(kontrollknoten.gt.0)then
   !   k1=knoten_zone(kontrollknoten)
   !   k2=zonen_nummer(k1)
   !   k3=wetterstations_nummer(k1)
   !   k4=Wetterstationskennung_T(k3)
   !   print*,'konstat=',kontrollknoten, k1, k2, k3, k4
   !end if
   do i = 1,IWETTs_T   !! loop all weather stations
      glob(1) = glob_T(i)     ! Eingangsdaten
      !print*,'glob=',glob(1)
      uhrz = uhrzeit_stunde
      !call zeitsekunde()    ! Rückrechnung der Uhrzeit aus dem sekunden zeitpunkt # wird schon in Zeitschleife Qsim3D gemacht
      !print*,'vor sasu tagdesjahres=',tagdesjahres,' meinrang=', meinrang
      tdj = tagdesjahres !! wird von sasu verändert (wozu ist unklar)
      !    print*,'strahlg_wetter0 station',Wetterstationskennung_T(i),' tag, monat, tagdesjahres=',tag, monat, tagdesjahres &
      !&         ,' glob(1)glob_T(i)=',glob(1),glob_T(i)
      !    print*,'SASU vorher tag, monat, modell_geob, modell_geol, sa, su, zg, zlk, dk, tdj,ifehl=' &
      !&                      ,tag, monat, modell_geob, modell_geol, sa, su, zg, zlk, dk, tdj,ifehl
      geol = modell_geol
      geob = modell_geob
      call SASU(tag, monat, geob, geol, sa, su, zg, zlk, dk, tdj,ifehl)
      !        if(kontrollknoten.gt.0)      &
      !        print*,'Wetterstation ',i,' SASU nachher: sa, su,tag, monat, modell_geob, modell_geol, zg, zlk, dk, tdj,ifehl=' &
      !     &                       ,sa,su,tag,monat,modell_geob,modell_geol,zg,zlk,dk,tdj,ifehl
      if (ifehl /= 0)call qerror("SASU fehlgeschlagen")
      if (sa > su) then
         print*,"strahlg_wetter: tag, monat, modell_geob, modell_geol, sa, su, zg, zlk, dk, tdj,ifehl" &
         ,tag, monat, modell_geob, modell_geol, sa, su, zg, zlk, dk, tdj,ifehl
         call qerror("strahlg_wetter: computing daylight hours went wrong")
      endif
      !if(meinrang.ge.(proz_anz-3)) then
      !   print*,'strahlg_wetter,sasu: meinrang,Wetterstationen, tag, monat, geob, geol, sa, su, zg, zlk, dk, tagdesjahres'
      !   print*,meinrang,i,tag, monat, modell_geob, modell_geol, sa, su, zg, zlk, dk, tagdesjahres
      !end if
      schwi(1) = 0.0     ! Rückgabewert initialisiert
      tflie = real(deltat)/3600.0 ! Zeitschrittweite von integer-Sekunden in real-Stunden umrechnen
      dk = 0.0     !
      cloud(1) = cloud_T(i)   ! Bewölkung
      schwia = 0.0     !
      mstr = 1     ! nur ein Strang
      IDWe(1,1) = 1     ! für strahlg() hat strang 1, profil 1 die 1. wetterstation
      IDWe(1,2) = 1     ! für strahlg() hat strang 1, profil 1 die 1. wetterstation
      VTYP = 0     !
      VALTBL(1) = 0     ! Keinerlei Uferbewuchs
      EDUFBL(1) = 0     !
      VALTBR(1) = 0     !
      EDUFBR(1) = 0     !
      breite(1) = 100.0     !
      anze = 1    ! nur an einem Punkt/Profil
      ifehl = 1     !
      ifhStr = 1     !
      if (kontrollknoten > 0)      &
          !print*,'strahlg_wetter station',i,Wetterstationskennung_T(i),' uhrzeit_stunde,sa,su,uhrz=',uhrzeit_stunde,sa,su,uhrz
      ij = 1 ! Zeitschritt-Nummer während eines Tages   (unbenutzt in 3D)
      azStr = 1 ! Laufindex über alle Stränge =1 in 3D
      call strahlg(glob,uhrz,sa,su,schwi,tflie,geol,tdj,geob,dk,cloud,schwia,IMET_T,mstr,IDWe,tag,monat,VTYP   &
                   ,VALTBL,EDUFBL,VALTBR,EDUFBR,breite,anze,ifehl,ifhStr   &
                   ,it_h,ij,jahrs,itage,monate,jahre,uhren,isim_end,azStr,azStrs)
      
      if (kontrollknoten > 0)      &
          !print*,meinrang,i," strahlg_wetter glob,schwi,uhrz,sa,su",glob(1),schwi(1),uhrz,sa,su
      !   SUBROUTINE strahlg(glob,uhrz,sa,su,schwi,tflie,geol,tdj,geob,dk,cloud,schwia,imet,mstr,IDWe,itags,monats,VTYP   &
      !                     ,VALTBL,EDUFBL,VALTBR,EDUFBR,breite,anze,ifehl,ifhStr
      !                     ,it_h,ij,jahrs,itage,monate,jahre,uhren,isim_end,azStr,azStrs)
      
      ! Rueckgabe
      !print*,'meinrang,uhrzeit_stunde,glob,schwi,i,tagdesjahres'
      !print*, meinrang,uhrzeit_stunde,glob(1),schwi(1),i,tagdesjahres
      schwi_T(i) = schwi(1)    ! global radiation at weather station
      !transfer to nodes in temperw_huelle: transfer_quantity_p(64+(i-1)*number_trans_quant) = schwi(1)
      if (isNaN(schwi_T(i))) then
         write(fehler,*)'strahlg_wetter station',Wetterstationskennung_T(i),' IMET_T = ',IMET_T,' isNaN(schwi_T(i))'
         call qerror(fehler)
      endif
      transfer_value_p(10) = it_h(1,1) ! Anzahl der Zeitschritte während der Hellphase (unbenutzt in 3D)
      ! isim_end = 1 Ausgabeparameter -> Algenroutinen
   end do ! i all weather stationen
   return
end subroutine strahlg_wetter
!      SUBROUTINE strahlg(glob,uhrz,sa,su,schwi,tflie,geol,tdj           &
!     &,geob,dk,cloud,schwia,imet,mstr,IDWe,itags,monats,VTYP            &
!     &,VALTBL,EDUFBL,VALTBR,EDUFBR,breite,anze,ifehl,ifhStr)
!----+-----+----
!      end module wetter
