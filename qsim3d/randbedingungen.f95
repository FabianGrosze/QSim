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
! hier enthalten:
! randbedingungen_setzen() ; randwert_planctonic() ; randbedingungen_ergaenzen() ; randbedingungen_parallel() ;
! scatter_BC() ; RB_werte_aktualisieren() ; function randwert_gueltig() ; ereigg_Randbedingungen_lesen() ;
! extnct_lesen() ;  alloc_hydraul_BC()
!
!-------------------------------------------------------------------------------Randbedingungs-Datenfelder
!> \page zuflussranddaten Randbedingungen
!! Dies sind die Konzentrationen im zufließend Wasser an den Rändern, sie
!! werden aus der Datei <a href="./exp/EREIGG.txt" target="_blank">EREIGG.txt</a> gelesen\n
!! und entsprechend den Randnummern zu den am Rand liegenden Knoten/Elementen zugeordnet.\n
!! Knoten mit der Randnummer 0 liegen im inneren des Gebiets.\n
!! Datentechnische Detail in: ereigg_Randbedingungen_lesen()
!! \n\n
!! Nur die üblicherweise aus Messungen verfügbaren Variablen sind angebbar, ( randbedingungen_setzen() )\n
!! Bei denjenigen \ref tiefengemittelte_planktische_variable , die dadurch keine Werte erhalten,
!! werden diese plausibel ergänzt; siehe: \subpage randbedingungen_ergaenzen . \n
!! \n
!! Im Programmablauf geschieht die Randbedingungs-Zuordnung vor Stoffumsatz und Stofftransport,
!! damit der Transport die am Rand gesetzten Konzentrationen (besonders an den Einströmstellen)
!! ins Gebiet einträgt.\n
!! \n
!! Aus der <a href="./exp/EREIGG.txt" target="_blank">EREIGG.txt</a>
!! Datei werden die folgenden Variablen (werts) gelesen und den in nachfolgender Tabelle
!! genannten \ref tiefengemittelte_planktische_variable zugeordnet.\n
!! \n
!! Für den aktuellen Zeitschrit wird der Wert zwischen gültigen Randwerten interpoliert.\n
!! \n
!! Gültig sind nur positive Werte. Negative Werte werden als ungültig betrachtet.\n
!! Wird an einem Rand kein gültiger Randwert gefunden, bricht die Simulation ab, ausser:\n
!! bei \ref hnf und \ref coliform.
!! Diese werden bei ausschließlich ungültigen Randwerten als an dem Rand nicht vorhanden (Null) betrachtet.\n
!! \n
!!<table >
!!<tr><th> werts(Nr.)</th><th>plankt-var. Nr.</th><th>QSim-Variablen-Name</th><th> Beschreibung </th><th> Einheit </th><th> wird gesetzt in Subrout. </th> </tr>
!!<tr><td>  1 </td><td>  - </td><td> QEINL   </td><td> Einleiter-Abfluss / in QSim3D unberücksichtigt      </td><td> m³/s    </td><td>   </td></tr>
!!<tr><td>  2 </td><td> 46 </td><td> \ref vbsb   </td><td> C-BSB5         </td><td> mg/l    </td><td>   </td></tr>
!!<tr><td>  3 </td><td> 47 </td><td> \ref vcsb   </td><td> C-CSB            </td><td> mg/l    </td><td>   </td></tr>
!!<tr><td>  4 </td><td>  3 </td><td> \ref vnh4   </td><td> NH4-N Gehalt         </td><td> mg/l    </td><td> ncyc()  </td></tr>
!!<tr><td>  5 </td><td>  4 </td><td> \ref vno2   </td><td> Nitrit-Stickstoffgehalt   </td><td> mg/l    </td><td> ncyc()  </td></tr>
!!<tr><td>  6 </td><td>  5 </td><td> \ref vno3   </td><td> Nitrat-Stickstoffgehalt   </td><td> mg/l    </td><td> ncyc()  </td></tr>
!!<tr><td>  7 </td><td> 67 </td><td> \ref gesn   </td><td> Gesamt-Stickstoff      </td><td> mg/l    </td><td> ncyc()  </td></tr>
!!<tr><td>  8 </td><td>  6 </td><td> \ref vx0   </td><td> suspendierte Nitrosomonasbiomasse      </td><td> mg/l    </td><td> ncyc()  </td></tr>
!!<tr><td>  9 </td><td>  7 </td><td> \ref vx02   </td><td> suspendierte Nitrobacterbiomasse      </td><td> mg/l    </td><td> ncyc()  </td></tr>
!!<tr><td> 10 </td><td>  9 </td><td> \ref gelp   </td><td> gelöster Phosphorgehalt   </td><td> mg/l    </td><td> po4s()   </td></tr>
!!<tr><td> 11 </td><td> 68 </td><td> \ref gesp   </td><td> Gesamt-Phosphor      </td><td> mg/l    </td><td> po4s()   </td></tr>
!!<tr><td> 12 </td><td>  8 </td><td> \ref si      </td><td> gelöster Siliziumgehalt   </td><td> mg/l    </td><td> silikat()   </td></tr>
!!<tr><td> 13 </td><td> 12 </td><td> \ref chla   </td><td> Chlorophyll-a-Gehalt      </td><td> µg/l    </td><td>   </td></tr>
!!<tr><td> 14 </td><td> 19 </td><td> \ref vkigr   </td><td> Anteil der Kieselalgen am Gesamt-Chlorophyll-a</td><td> 0-1    </td><td>   </td></tr>
!!<tr><td> 15 </td><td> 20 </td><td> \ref antbl   </td><td> Anteil der Blaualgen am Gesamt-Chlorophyll-a   </td><td> 0-1    </td><td>   </td></tr>
!!<tr><td> 16 </td><td> 50 </td><td> \ref zooind   </td><td> Rotatoriendichte      </td><td> Ind/l </td><td>   </td></tr>
!!<tr><td> 17 </td><td> 66 </td><td> \ref vph   </td><td> pH-Wert         </td><td> -    </td><td>   </td></tr>
!!<tr><td> 18 </td><td> 62 </td><td> \ref mw      </td><td> m-Wert         </td><td> mmol/l</td><td>   </td></tr>
!!<tr><td> 19 </td><td> 64 </td><td> \ref ca      </td><td> Calciumkonzentration      </td><td> mg/l    </td><td>   </td></tr>
!!<tr><td> 20 </td><td> 65 </td><td> \ref lf      </td><td> Leitfähigkeit         </td><td> µS/cm </td><td>   </td></tr>
!!<tr><td> 21 </td><td> 53 </td><td> \ref ss      </td><td> Schwebstoffgehalt      </td><td> mg/l    </td><td>   </td></tr>
!!<tr><td> 22 </td><td>  1 </td><td> \ref tempw   </td><td> Wassertemperatur      </td><td> °C    </td><td>   </td></tr>
!!<tr><td> 23 </td><td>  2 </td><td> \ref vo2   </td><td> Sauerstoffgehalt      </td><td> mg/l    </td><td>   </td></tr>
!!<tr><td> 24 </td><td> 48 </td><td> \ref chnf   </td><td> Heterotrophe Nanoflagelaten   </td><td> Ind/ml </td><td>   </td></tr>
!!<tr><td> 25 </td><td> 49 </td><td> \ref bvhnf   </td><td> Biovolumen der HNF      </td><td> µm3    </td><td>   </td></tr>
!!<tr><td> 26 </td><td> 61 </td><td> \ref coli   </td><td> Fäkalcoliforme Bakterien   </td><td> Ind/100 ml    </td><td>   </td></tr>
!!<tr><td> 27 </td><td>  - </td><td> EWAERM   </td><td> Menge der eingeleiteten Wärme (Kraftwerk) / in QSim3D unberücksichtigt   </td><td> Mcal/s</td><td>   </td></tr>
!!<tr><td> 28 </td><td> 71 </td><td> TRACER   </td><td> Tracer-Menge (nur im Tracer-Prozessbaustein berücksichtigt)    </td><td> g    </td><td> -  </td></tr>
!!</table>
!! <code>\verbatim aus funkstar.f90:
!! if(ipp==1)abfls(mstr,RBNR) = ywert
!! if(ipp==2)vbsbs(mstr,RBNR) = ywert
!! if(ipp==3)vcsbs(mstr,RBNR) = ywert
!! if(ipp==4)vnh4s(mstr,RBNR) = ywert
!! if(ipp==5)vno2s(mstr,RBNR) = ywert
!! if(ipp==6)vno3s(mstr,RBNR) = ywert
!! if(ipp==7)gesNs(mstr,RBNR) = ywert
!! if(ipp==8)vx0s(mstr,RBNR) = ywert
!! if(ipp==9)vx02s(mstr,RBNR) = ywert
!! if(ipp==10)gelps(mstr,RBNR) = ywert
!! if(ipp==11)gesPs(mstr,RBNR) = ywert
!! if(ipp==12)sis(mstr,RBNR) = ywert
!! if(ipp==13)chlas(mstr,RBNR) = ywert
!! if(ipp==14)vkigrs(mstr,RBNR) = ywert
!! if(ipp==15)antbls(mstr,RBNR) = ywert
!! if(ipp==16)zooins(mstr,RBNR) = ywert
!! if(ipp==17)vphs(mstr,RBNR) = ywert
!! if(ipp==18)mws(mstr,RBNR) = ywert
!! if(ipp==19)cas(mstr,RBNR) = ywert
!! if(ipp==20)lfs(mstr,RBNR) = ywert
!! if(ipp==21)ssalgs(mstr,RBNR) = ywert
!! if(ipp==22)tempws(mstr,RBNR) = ywert
!! if(ipp==23)vo2s(mstr,RBNR) = ywert
!! if(ipp==24)CHNFs(mstr,RBNR) = ywert
!! if(ipp==25)BVHNFs(mstr,RBNR) = ywert
!! if(ipp==26)colis(mstr,RBNR) = ywert
!! if(ipp==27)waers(mstr,RBNR) = ywert
!! 28 und 29 Tracer und kons. Substanz in tempws
!! if(ipp==30)gsPbs(mstr,RBNR) = ywert
!! if(ipp==31)glPbs(mstr,RBNR) = ywert
!! if(ipp==32)gsCads(mstr,RBNR) = ywert
!! if(ipp==33)glCads(mstr,RBNR) = ywert
!! if(ipp==34)gsCrs(mstr,RBNR) = ywert
!! if(ipp==35)glCrs(mstr,RBNR) = ywert
!! if(ipp==36)gsFes(mstr,RBNR) = ywert
!! if(ipp==37)glFes(mstr,RBNR) = ywert
!! if(ipp==38)gsCus(mstr,RBNR) = ywert
!! if(ipp==39)glCus(mstr,RBNR) = ywert
!! if(ipp==40)gsMns(mstr,RBNR) = ywert
!! if(ipp==41)glMns(mstr,RBNR) = ywert
!! if(ipp==42)gsNis(mstr,RBNR) = ywert
!! if(ipp==43)glNis(mstr,RBNR) = ywert
!! if(ipp==44)gsHgs(mstr,RBNR) = ywert
!! if(ipp==45)glHgs(mstr,RBNR) = ywert
!! if(ipp==46)gsUs(mstr,RBNR) = ywert
!! if(ipp==47)glUs(mstr,RBNR) = ywert
!! if(ipp==48)gsZns(mstr,RBNR) = ywert
!! if(ipp==49)glZns(mstr,RBNR) = ywert
!! if(ipp==50)gsAss(mstr,RBNR) = ywert
!! if(ipp==51)glAss(mstr,RBNR) = ywert
!!  \endverbatim</code>\n\n
!! type(rb) , allocatable , dimension (:) :: rabe\n
!! Zufluss-Randbedingungen aus <a href="./exp/EREIGG.txt" target="_blank">EREIGG.txt</a>\n
!! werden von der subroutine ereigg_Randbedingungen_lesen() eingelesen.\n
!! QSim3D ließt in die Struktur \ref rb ein.\n
!! \n aus randbedingungen.f95 , zurück: \ref lnk_Datentechnik und \ref zuflussranddaten siehe auch \n\n
!> Dient dem Anbringen der \ref zuflussranddaten \n
!! in Datei randbedingungen.f95
subroutine randbedingungen_setzen()
   use modell
   use QSimDatenfelder
   use aparam
   implicit none
   integer :: j, RB_zaehl
   logical einmalig
   !print*,'randbedingungen_setzen'
   !!wy call gather_planktkon() ! syncronize non-parallel fields to paralell ones
   
   !>>>> Hydraulik-Randbedingungen (Geschwindigkeit, Wassertiefe und WSP) <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
   call scatter_rb_hydraul()
   !>>>> Wetter-Randbedingungen <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
   call update_weather()
   call mpi_barrier (mpi_komm_welt, ierr)
   !Randverläufe und Wetterstationen haben alle Prozessoren alle Eingabedaten
   !und wenden sie auf ihre Knoten an.
   !
   if (meinrang == 0) then !! nur prozessor 0
      einmalig = .true. !Fehlermeldung nur einmal ausgeben
      call RB_werte_aktualisieren(rechenzeit)
      do j = 1,number_plankt_point ! nur bei casu=knotenanzahl2D
         select case (hydro_trieb)
            case(1) ! casu-transinfo
               RB_zaehl = knoten_rand(j)
            case(2) ! Untrim² netCDF
               RB_zaehl = element_rand(j)
            case(3) ! SCHISM
               RB_zaehl = knoten_rand(j)
               case default
               call qerror('randbedingungen_setzen: Hydraulischer Antrieb unbekannt')
         end select
         if ( (RB_zaehl > 0) .and. (RB_zaehl < 100000) ) then !! Alle Knoten, deren RB's bedient werden:
            if (j == kontrollknoten)print*,'Konrollstelle #',j,' ist Rand mit RB_zaehl = ',RB_zaehl
            if (inflow(j)) then !! alle Zufluss-Knoten
               if (j == kontrollknoten)print*,'Konrollstelle #',j,' ist inflow Rand mit ',inflow(j)
               call randwert_planctonic(j,RB_zaehl,einmalig)
               if (j == kontrollknoten)  &
                   print*,'RB gesetzt tracer = ',planktonic_variable(71+(j-1)*number_plankt_vari)  &
                   ,'tempw = ',planktonic_variable(1+(j-1)*number_plankt_vari)   &
                   ,'obsb = ',planktonic_variable(17+(j-1)*number_plankt_vari)
               call randbedingungen_ergaenzen(j,einmalig)
               call tiefenprofil(j)
               !if(j .eq. kontrollknoten)then ! Ausgabe
               !    print*,'kontrollknoten Randknoten: inflow =', inflow(j),' #', j,  &
               !    ' lf=',planktonic_variable(65+nk)
               !end if !kontrollknoten
            end if ! Zufluss-Randknoten
         end if ! bedienter Randknoten
      end do ! alle j Knoten
      ! meteorological Boundary Conditions
      !      templ(1)=temperatur_lu    ! Lufttemperatur aus Wetterstationsdaten
      !      ro(1)=luftfeuchte         ! relative Luftfeuchte in %
      !      wge(1)=wind               ! Windgeschwindigkeit  aus Wetterstationsdaten
      !      schwi(1)=strahlung        ! Globalstrahlung in cal/(cm2*h) von strahlg() berechnet
      !      cloud(1)=bewoelkung       ! Bewölkungsdichte  aus Wetterstationsdaten
      !      typw(1)=wolkentyp         ! Wolkentyp  aus Wetterstationsdaten
      if (kontrollknoten > 0)  &
          print*,'randbedingungen_setzen, prozessor 0, chla(kontrollknoten) = '  &
          ,planktonic_variable(11+(kontrollknoten-1)*number_plankt_vari)
   end if !! nur prozessor 0
   !
   call mpi_barrier (mpi_komm_welt, ierr)
   call scatter_BC()
   call scatter_planktkon()
   j = kontrollknoten-(meinrang*part)
   if ((j >= 1) .and. (j <= part)) then
      print*,'nach randbedingungen_setzen am Kontrollknoten:',kontrollknoten,meinrang,part,j
      print*,'Tiefe = ', rb_hydraul_p(2+(j-1)*number_rb_hydraul)
      print*,'tempw = ', planktonic_variable_p(1+(j-1)*number_plankt_vari),1+(j-1)*number_plankt_vari
      print*,'O2 = '   , planktonic_variable_p(2+(j-1)*number_plankt_vari),2+(j-1)*number_plankt_vari ! Sauerstoffgehalt tiefengemittelt
      print*,'obsb = ',planktonic_variable_p(17+(j-1)*number_plankt_vari)
      print*,'ocsb = ',planktonic_variable_p(18+(j-1)*number_plankt_vari)
      print*,'cd1 = ',planktonic_variable_p(37+(j-1)*number_plankt_vari) !    leicht abbaubare gelöste organische C-Verbindungen    mg C / l
      print*,'cd2 = ',planktonic_variable_p(38+(j-1)*number_plankt_vari) !    schwer abbaubare gelöste organische C-Verbindungen    mg C / l
      print*,'cp1 = ',planktonic_variable_p(39+(j-1)*number_plankt_vari) !     leicht abbaubare partikuläre organische C-Verbindungen    mg C / l
      print*,'cp2 = ',planktonic_variable_p(40+(j-1)*number_plankt_vari) !     schwer abbaubare partikuläre organische C-Verbindungen    mg C / l
      print*,'cm = ',planktonic_variable_p(41+(j-1)*number_plankt_vari) !     monomolekularen organischen C-Verbindungen    mg C / l
      print*,'bac = ',planktonic_variable_p(42+(j-1)*number_plankt_vari) !     Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen    mg C / l
      print*,'o2bsb = ',planktonic_variable_p(43+(j-1)*number_plankt_vari) !     Sauerstoff-Kohlenstoffverhältnis beim C-Abbau    mgO2/mgC
      print*,'bl01 = ',planktonic_variable_p(44+(j-1)*number_plankt_vari) !     schwerabbaubare Kohlenstoffverbindungen als Sauerstoffäquivalent    mg O2 / l
      print*,'bl02 = ',planktonic_variable_p(45+(j-1)*number_plankt_vari) !     leichtabbaubare Kohlenstoffverbindungen als Sauerstoffäquivalent    mg O2 / l
      print*,'vbsb = ',planktonic_variable_p(46+(j-1)*number_plankt_vari) !     BSB5 incl. lebender Organismen    mg O2 / l
      print*,'vcsb = ',planktonic_variable_p(47+(j-1)*number_plankt_vari) !     CSB incl. lebender Organismen    mg O2 / l
      print*,'chla = ',planktonic_variable_p(11+(j-1)*number_plankt_vari) !     Chlorophyll-a
      print*,'vkigr = ',planktonic_variable_p(19+(j-1)*number_plankt_vari)
      print*,'antbl = ',planktonic_variable_p(20+(j-1)*number_plankt_vari)
      print*,'chlaki = ',planktonic_variable_p(12+(j-1)*number_plankt_vari)
      print*,'chlagr = ',planktonic_variable_p(13+(j-1)*number_plankt_vari)
      print*,'chlabl = ',planktonic_variable_p(14+(j-1)*number_plankt_vari)
      print*,'aki = ',planktonic_variable_p(8+(j-1)*number_plankt_vari)
      print*,'agr = ',planktonic_variable_p(9+(j-1)*number_plankt_vari)
      print*,'abl = ',planktonic_variable_p(10+(j-1)*number_plankt_vari)
      print*,'akbcm = ',planktonic_variable_p(24+(j-1)*number_plankt_vari) !, ' Caki=',Caki
      print*,'agbcm = ',planktonic_variable_p(25+(j-1)*number_plankt_vari) !, ' Caki=',Caki
      print*,'abbcm = ',planktonic_variable_p(26+(j-1)*number_plankt_vari) !, ' Caki=',Caki
      print*,'ph = ',planktonic_variable_p(66+(j-1)*number_plankt_vari) !
   endif  ! kontrollknoten
   return
end subroutine randbedingungen_setzen
!----+-----+----
!
subroutine scatter_rb_hydraul()
   use modell
   implicit none
   integer :: j, RB_zaehl
   !>>>> Hydraulik-Randbedingungen (Geschwindigkeit, Wassertiefe und WSP) <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
   ! wurden von prozess 0 in holen_trans() gelesen und werden hier auf alle Prozesse gescattert.
   !if(meinrang.eq.0)then !! nur prozessor 0
   !      do j=1,knotenanzahl2D
   !         rb_hydraul(1+(j-1)*number_rb_hydraul)    = u(j)
   !         rb_hydraul(2+(j-1)*number_rb_hydraul)    = tief(j)
   !         rb_hydraul(3+(j-1)*number_rb_hydraul)    = p(j)
   !      end do !! alle j knoten
   !end if !! nur prozessor 0
   call MPI_Scatter(rb_hydraul, part*number_rb_hydraul, MPI_FLOAT,  &
                    rb_hydraul_p, part*number_rb_hydraul, MPI_FLOAT, 0, mpi_komm_welt, ierr)
   if (ierr /= 0) then
      write(fehler,*)' 14 MPI_Scatter(rb_hydraul failed :', ierr
      call qerror(fehler)
   end if
   call mpi_barrier (mpi_komm_welt, ierr)
   return
end subroutine scatter_rb_hydraul
!----+-----+----
!
subroutine randwert_planctonic(jjj,zaehl,logi)
   use modell
   use QSimDatenfelder
   use aparam
   implicit none
   !!  \anchor jjj wahrscheinlich ein Zähler (def. in randbedingungen; referenziert in N subroutine!?)
   integer :: jjj, zaehl, nk, i, l
   logical logi
   if (meinrang > 0) then !! nur prozessor 0
      write(fehler,*)' 789 subroutine randwert_planctonic darf nur auf prozess 0'
      call qerror(fehler)
   endif
   if (jjj > number_plankt_point)call qerror('randwert_planctonic: jjj > number_plankt_point')
   nk = (jjj-1)*number_plankt_vari
   if (nk+number_plankt_vari > number_plankt_vari*number_plankt_point) then
      print*,'randwert_planctonic: jjj,zaehl = ',jjj,zaehl, number_plankt_vari, number_plankt_point
      call qerror('planktonic_variable unterdimensioniert')
   endif
   if (zaehl > 0) then !! Randbedingung zu dieser Nummer vorhanden
      !!if(rabe(zaehl)%zufluss)then !! z.Z. Zufluss über diesen Rand?
      planktonic_variable( 1+nk) = rabe(zaehl)%wert_jetzt(22) !  Tempw temperw()
      planktonic_variable( 2+nk) = rabe(zaehl)%wert_jetzt(23) !  VO2 oxygen()
      planktonic_variable( 3+nk) = rabe(zaehl)%wert_jetzt( 4) !  VNH4 ncyc()
      planktonic_variable( 4+nk) = rabe(zaehl)%wert_jetzt( 5) !  VNO2 ncyc()
      planktonic_variable( 5+nk) = rabe(zaehl)%wert_jetzt( 6) !  VNO3 ncyc()
      planktonic_variable( 6+nk) = rabe(zaehl)%wert_jetzt(10) !  GELP po4s()
      planktonic_variable( 7+nk) = rabe(zaehl)%wert_jetzt(12) !  SI silikat()
      planktonic_variable( 8+nk) = 0.0 ! aki wird jetzt in algae_start() gesetzt
      planktonic_variable( 9+nk) = 0.0 ! agr wird jetzt in algae_start() gesetzt
      planktonic_variable(10+nk) = 0.0 ! abl wird jetzt in algae_start() gesetzt
      planktonic_variable(11+nk) = rabe(zaehl)%wert_jetzt(13) !  CHLA
      planktonic_variable(12+nk) = 0.0 ! chlaki wird jetzt in algae_start() gesetzt
      planktonic_variable(13+nk) = 0.0 ! chlagr wird jetzt in algae_start() gesetzt
      planktonic_variable(14+nk) = 0.0 ! chlabl wird jetzt in algae_start() gesetzt
      planktonic_variable(15+nk) = rabe(zaehl)%wert_jetzt( 8) !  VX0 ncyc()
      planktonic_variable(16+nk) = rabe(zaehl)%wert_jetzt( 9) !  VX02 ncyc()
      planktonic_variable(17+nk) = 0.0 !  OBSB  orgc_start() C-BSB5, kohlenstoffbürtig
      planktonic_variable(18+nk) = 0.0 !  OCSB  orgc_start() CSB, Kohlenstoffbürtig
      planktonic_variable(19+nk) = rabe(zaehl)%wert_jetzt(14) !  VKIGR
      planktonic_variable(20+nk) = rabe(zaehl)%wert_jetzt(15) !  ANTBL
      planktonic_variable(21+nk) = 0.01 ! svhemk 0.01 svhemk ### unklar ###
      planktonic_variable(22+nk) = 0.01 ! svhemg 0.01 svhemk ### unklar ###
      planktonic_variable(23+nk) = 0.01 ! svhemb 0.01 svhemk ### unklar ###
      planktonic_variable(24+nk) = 0.0 ! akbcm wird jetzt in algae_start() gesetzt
      planktonic_variable(25+nk) = 0.0 ! agbcm wird jetzt in algae_start() gesetzt
      planktonic_variable(26+nk) = 0.0 ! abbcm wird jetzt in algae_start() gesetzt
      planktonic_variable(27+nk) = 0.01 ! akiiv 0.01 svhemk ### unklar ###
      planktonic_variable(28+nk) = 0.01 ! agriv 0.01 svhemk ### unklar ###
      planktonic_variable(29+nk) = 0.01 ! abliv 0.01 svhemk ### unklar ###
      planktonic_variable(30+nk) = 0.0 ! Q_NK ncyc() wird jetzt in neahr_start() gesetzt
      planktonic_variable(31+nk) = 0.0 ! Q_PK po4s() wird jetzt in neahr_start() gesetzt
      planktonic_variable(32+nk) = 0.0 ! Q_SK silikat() wird jetzt in neahr_start() gesetzt
      planktonic_variable(33+nk) = 0.0 ! Q_NG ncyc() wird jetzt in neahr_start() gesetzt
      planktonic_variable(34+nk) = 0.0 ! Q_PG po4s() wird jetzt in neahr_start() gesetzt
      planktonic_variable(35+nk) = 0.0 ! Q_NB ncyc() wird jetzt in neahr_start() gesetzt
      planktonic_variable(36+nk) = 0.0 ! Q_PB po4s() wird jetzt in neahr_start() gesetzt
      planktonic_variable(37+nk) = 0.0 ! CD(1 orgc_start()
      planktonic_variable(38+nk) = 0.0 ! CD(2 orgc_start()
      planktonic_variable(39+nk) = 0.0 ! CP(1 orgc_start()
      planktonic_variable(40+nk) = 0.0 ! CP(2 orgc_start()
      planktonic_variable(41+nk) = 0.0 ! CM orgc_start()
      planktonic_variable(42+nk) = 0.0 ! BAC orgc_start()
      planktonic_variable(43+nk) = 0.0 ! O2BSB orgc_start()
      planktonic_variable(44+nk) = 0.0 ! BL01 orgc_start()
      planktonic_variable(45+nk) = 0.0 ! BL02 orgc_start()
      planktonic_variable(46+nk) = rabe(zaehl)%wert_jetzt( 2) ! vbsb  BSB5 incl. lebender Organismen, messbar, Randwert
      planktonic_variable(47+nk) = rabe(zaehl)%wert_jetzt( 3) ! vcsb  CSB incl. lebender Organismen , messbar, Randwert
      planktonic_variable(48+nk) = rabe(zaehl)%wert_jetzt(24) !  CHNF  orgc()
      planktonic_variable(49+nk) = rabe(zaehl)%wert_jetzt(25) !  BVHNF
      planktonic_variable(50+nk) = rabe(zaehl)%wert_jetzt(16) !  ZOOIND
      planktonic_variable(51+nk) = 0.0 ! abrzo1 ### wohl unbenutzt ###
      planktonic_variable(52+nk) = rabe(zaehl)%wert_jetzt(21) ! ssalg GESAMTSCHWEBSTOFFE incl. lebender Organismen, messbar, Randwert
      planktonic_variable(53+nk) = 0.0 ! SS suspendierte Sedimente ohne lebende Organismen
      planktonic_variable(54+nk) = 0.7 ! fssgr !! qsim.f90: fssgrs = 0.7
      planktonic_variable(55+nk) = 0.4 ! fbsgr !! qsim.f90: fbsgrs = 0.4
      planktonic_variable(56+nk) = 0.0 ! frfgr
      planktonic_variable(57+nk) = 0.04  !  nl0 Stickstoff zu Kohlenstoff in organischem Material  = 0.040 wenn nicht berechenbar
      planktonic_variable(58+nk) = 0.005 !  pl0 Phosphor zu Kohlenstoff in organischem Material    = 0.005 wenn nicht berechenbar
      planktonic_variable(59+nk) = 0.0 ! stind ph()
      planktonic_variable(60+nk) = 0.0 ! dlarvn dreissen.f90
      planktonic_variable(61+nk) = rabe(zaehl)%wert_jetzt(26) !  COLI
      planktonic_variable(62+nk) = rabe(zaehl)%wert_jetzt(18) !  MW
      planktonic_variable(63+nk) = 0.0 ! pw ph_aufteilung_einleitung()
      planktonic_variable(64+nk) = rabe(zaehl)%wert_jetzt(19) !  CA
      planktonic_variable(65+nk) = rabe(zaehl)%wert_jetzt(20) !  LF
      planktonic_variable(66+nk) = rabe(zaehl)%wert_jetzt(17) !  VPH
      planktonic_variable(67+nk) = rabe(zaehl)%wert_jetzt( 7) !  GESN ncyc()
      planktonic_variable(68+nk) = rabe(zaehl)%wert_jetzt(11) !  GESP po4s()
      planktonic_variable(69+nk) = 0.0 ! SKmor, Silizium in schwebenden, abgestorbenen Kieselalgen, algaeski<->silikat
      planktonic_variable(70+nk) = 0.0 ! DOSCF siehe COLIFORM()
      planktonic_variable(71+nk) = rabe(zaehl)%wert_jetzt(28) !  tracer (neu QSim-3D)
      !planktonic_variable(71+nk)= 1.0 !  tracer ##### test untrim
      planktonic_variable(72+nk) = 0.0 ! Salz (neu QSim-3D)
      planktonic_variable(73+nk) = 0.0 ! alter_decay
      planktonic_variable(74+nk) = 0.0 ! alter_arith
      planktonic_variable(75+nk) = 0.0 ! alter_growth
      planktonic_variable(76+nk) = GROT ! TGZoo
      planktonic_variable(77+nk) = 0.0 ! akmor_1
      planktonic_variable(78+nk) = 0.0 ! agmor_1
      planktonic_variable(79+nk) = 0.0 ! abmor_1
      planktonic_variable(80+nk) = rabe(zaehl)%wert_jetzt(48) ! Zink gesamt
      planktonic_variable(81+nk) = rabe(zaehl)%wert_jetzt(49) ! Zink gelöst
      planktonic_variable(82+nk) = rabe(zaehl)%wert_jetzt(32) ! Cadmium gesamt
      planktonic_variable(83+nk) = rabe(zaehl)%wert_jetzt(33) ! Cadmium gelöst
      planktonic_variable(84+nk) = rabe(zaehl)%wert_jetzt(38) ! Kupfer gesamt
      planktonic_variable(85+nk) = rabe(zaehl)%wert_jetzt(39) ! Kupfer gelöst
      planktonic_variable(86+nk) = rabe(zaehl)%wert_jetzt(42) ! Nickel gesamt
      planktonic_variable(87+nk) = rabe(zaehl)%wert_jetzt(43) ! Nickel gelöst
      planktonic_variable(88+nk) = rabe(zaehl)%wert_jetzt(50) ! Arsen gesamt
      planktonic_variable(89+nk) = rabe(zaehl)%wert_jetzt(51) ! Arsen gelöst
      planktonic_variable(90+nk) = rabe(zaehl)%wert_jetzt(30) ! Blei gesamt
      planktonic_variable(91+nk) = rabe(zaehl)%wert_jetzt(31) ! Blei gelöst
      planktonic_variable(92+nk) = rabe(zaehl)%wert_jetzt(34) ! Chrom gesamt
      planktonic_variable(93+nk) = rabe(zaehl)%wert_jetzt(35) ! Chrom gelöst
      planktonic_variable(94+nk) = rabe(zaehl)%wert_jetzt(36) ! Eisen gesamt
      planktonic_variable(95+nk) = rabe(zaehl)%wert_jetzt(37) ! Eisen gelöst
      planktonic_variable(96+nk) = rabe(zaehl)%wert_jetzt(44) ! Quecksilber gesamt
      planktonic_variable(97+nk) = rabe(zaehl)%wert_jetzt(45) ! Quecksilber gelöst
      planktonic_variable(98+nk) = rabe(zaehl)%wert_jetzt(40) ! Mangan gesamt
      if (isNaN(rabe(zaehl)%wert_jetzt(41))) then
         print*,'randwert_planctonic isNaN 41 jjj,zaehl = ',jjj,zaehl
         call qerror("isNaN(rabe(zaehl)%wert_jetzt(41))Mangan gelöst")
      endif
      planktonic_variable(99+nk) = rabe(zaehl)%wert_jetzt(41) ! Mangan gelöst
      planktonic_variable(100+nk) = rabe(zaehl)%wert_jetzt(46) ! Uran gesamt
      planktonic_variable(101+nk) = rabe(zaehl)%wert_jetzt(47) ! Uran gelöst
      !                  if(jjj.eq.kontrollknoten)print*,"randwert_planctonic: OBSB=",planktonic_variable(17+nk)  &
      !     &                                           ," OCSB=",planktonic_variable(18+nk)
      if (nur_alter .and. (wie_altern == 2))call alter_rand(jjj)
   end if !! RandbedingungsNummer größer 0
   return
end subroutine randwert_planctonic
!----+-----+----
!> bisher plankt_vari_vert konstant über die Tiefe d.h. 2D tiefengemittelt
!! \n\n aus randbedingungen.f95 , zurück: \ref zuflussranddaten
subroutine tiefenprofil(jjj)
   use modell
   implicit none
   integer :: jjj, nk, i, l
   if (jjj > number_plankt_point)call qerror('tiefenprofil: jjj > number_plankt_point')
   nk = (jjj-1)*number_plankt_vari
   if (nk+number_plankt_vari > number_plankt_vari*number_plankt_point) then
      print*,'tiefenprofil: jjj,zaehl = ',jjj, number_plankt_vari, number_plankt_point
      call qerror('planktonic_variable unterdimensioniert 2')
   endif
   ! konstante Verteilung in der Vertikalen
   do i = 1,14 ! tiefenprofile der ersten 14 konzentrationen
      do l = 1,num_lev
         plankt_vari_vert(l+(i-1)*num_lev+(jjj-1)*number_plankt_vari_vert*num_lev) = &
                                                                                     planktonic_variable(i+nk)
      end do ! alle l levels
   end do ! alle ersten 14 konzentrationen
   do l = 1,num_lev
      plankt_vari_vert(l+(16-1)*num_lev+(jjj-1)*number_plankt_vari_vert*num_lev) = &
                                                                                   planktonic_variable(67+nk)                                                     !  hgesNz = GESN
      plankt_vari_vert(l+(15-1)*num_lev+(jjj-1)*number_plankt_vari_vert*num_lev) = &
                                                                                   planktonic_variable(68+nk)                                                     !  hgesPz = GESP
      plankt_vari_vert(l+(17-1)*num_lev+(jjj-1)*number_plankt_vari_vert*num_lev) = &
                                                                                   planktonic_variable(30+nk)                                                     !  hQ_NKz = Q_NK
      plankt_vari_vert(l+(18-1)*num_lev+(jjj-1)*number_plankt_vari_vert*num_lev) = &
                                                                                   planktonic_variable(33+nk)                                                     !  hQ_NGz = Q_NG
      plankt_vari_vert(l+(19-1)*num_lev+(jjj-1)*number_plankt_vari_vert*num_lev) = &
                                                                                   planktonic_variable(35+nk)                                                     !  hQ_NBz = Q_NB
      plankt_vari_vert(l+(20-1)*num_lev+(jjj-1)*number_plankt_vari_vert*num_lev) = &
                                                                                   planktonic_variable(24+nk)                                                     !  hCChlkz = akbcm
      plankt_vari_vert(l+(21-1)*num_lev+(jjj-1)*number_plankt_vari_vert*num_lev) = &
                                                                                   planktonic_variable(25+nk)                                                     !  hCChlgz = agbcm
      plankt_vari_vert(l+(22-1)*num_lev+(jjj-1)*number_plankt_vari_vert*num_lev) = &
                                                                                   planktonic_variable(26+nk)                                                     !  hCChlbz = abbcm
   end do ! alle l levels
   return
end subroutine tiefenprofil
!----+-----+----
!
!> \page randbedingungen_ergaenzen Randbedingungen ergänzen
!! \n\n
!! wird nicht nur am Zuflussrand, sondern auch bei der initialisierung mit einer Randbedingung aufgerufen,
!! so dass alle Transportkonzentrationen belegt werden
!! \n\n
!! Jetzt:\n
!! algae_start() , orgc_start() , naehr_start() , pwert() \n
!! in ../metabol/zuflussrand.f90 \n
!! \n\n
!! Ehedem: \n
!! Randbedingungen werden nur für die 27 in ereigg_Randbedingungen_lesen() beschriebenen
!! Konzentrationen vorgegeben. Alle übrigen werden auf der Basis plausibler Abschätzungen erschlossen.\n
!! Diese Vorgehensweise hat ihren Ursprung in der Verfügbarkeit von Messdaten.\n
!! Ergänzungen finden für die folgenden QSim-Bausteine statt:
!! <ol>
!!    <li>\subpage algenaufteilung \n</li>
!!    <li>\subpage orgc_aufteilung  \n</li>
!!    <li>\subpage ncyc_aufteilung  \n</li>
!!    <li>  subpage ph_aufteilung \n</li>
!!    <li>\subpage po4s_aufteilung  \n</li>
!!    <li>\subpage si_aufteilung    \n</li>
!! </ol>\n\n
!! \n\n
! #mf \subpage ph_aufteilung broken link, subpage ph_aufteilung does not exist
!! in randbedingungen.f95\n
!! ereigg_Randbedingungen_lesen() \n
!! RB_werte_aktualisieren()\n
!! wert_randbedingung()\n\n
!! \n\n qsim.f90:
!!....Berechnung von nL0 und pL0 (N und P Gehalt der Abwasserbuertigen   \n
!!    org Substanz)
!! \n\n
!! läuft nur auf prozessor 0
!! noch fehlende ergänzungen für: <code>\verbatim
!!                   planktonic_variable(51+nk)= 0.0 ! abrzo1 ### wohl unbenutzt ###
!! schweb.f90:      SSALG(ior) = SSt+agr(ior)+aki(ior)+abl(ior)+(ZOOind(ior)*GROT/1000.)
!!                   planktonic_variable(52+nk)= 0.0 ! ssalg GESAMTSCHWEBSTOFFE provisorisch zusammenaddiert in randbedingungen_ergaenzen
!!                   planktonic_variable(54+nk)= 0.0 ! fssgr schweb.f90
!!                   planktonic_variable(55+nk)= 0.0 ! fbsgr orgc.f90
!!                   planktonic_variable(56+nk)= 0.0 ! frfgr orgc.f90
!!                   planktonic_variable(57+nk)= 0.0 ! nl0 Verhältnis von Stickstoff zu Kohlenstoff in organischem Material | qsim.f90: nl0s(mstr,mRB) = 0.04
!!                   planktonic_variable(58+nk)= 0.0 ! pl0 Verhältnis von Phosphor zu Kohlenstoff in organischem Material | qsim.f90: pl0s(mstr,mRB) = 0.005 wenn nicht berechenbar
!!                   planktonic_variable(59+nk)= 0.0 ! stind, Zeitsumme ph(), Funktion unklar ###
!!                   planktonic_variable(60+nk)= 0.0 ! dlarvn, dreissen.f90
!!                   planktonic_variable(69+nk)= 0.0 ! SKmor, Silizium in schwebenden, abgestorbenen Kieselalgen, algaeski<->silikat
!!                   planktonic_variable(70+nk)= 0.0 ! DOSCF, coliform()
!!                   planktonic_variable(72+nk)= 0.0 ! Salz (neu QSim-3D)
!!  \endverbatim</code>
!! \n\n zurück: \ref zuflussranddaten ; Quelle: randbedingungen.f95
!!
!! <h2> Transportkonzentrationen für welche die R.B. nicht vorgegeben sondern erschlossen werden</h2>
!!<table >
!!<tr><th> Nr.</th><th>QSim-Name</th><th> Wie            </th><th> Subroutine      </th></tr>
!!
!!<tr><td> 13 </td><td> chlaki   </td><td>         </td><td>  </td></tr>
!!<tr><td> 14 </td><td> chlagr   </td><td>         </td><td>  </td></tr>
!!<tr><td> 15 </td><td> chlabl   </td><td>         </td><td>  </td></tr>
!!<tr><td> 16 </td><td> aki   </td><td>         </td><td>  </td></tr>
!!<tr><td> 17 </td><td> agr   </td><td>         </td><td>  </td></tr>
!!<tr><td> 18 </td><td> abl   </td><td>         </td><td>  </td></tr>
!!<tr><td> 21 </td><td> svhemk   </td><td>         </td><td>  </td></tr>
!!<tr><td> 22 </td><td> svhemg   </td><td>         </td><td>  </td></tr>
!!<tr><td> 23 </td><td> svhemb   </td><td>         </td><td>  </td></tr>
!!<tr><td> 24 </td><td> akbcm   </td><td>         </td><td>  </td></tr>
!!<tr><td> 25 </td><td> agbcm   </td><td>         </td><td>  </td></tr>
!!<tr><td> 26 </td><td> abbcm   </td><td>         </td><td>  </td></tr>
!!<tr><td> 27 </td><td> akiiv   </td><td>         </td><td>  </td></tr>
!!<tr><td> 28 </td><td> agriv   </td><td>         </td><td>  </td></tr>
!!<tr><td> 29 </td><td> abliv   </td><td>         </td><td>  </td></tr>
!!<tr><td> 30 </td><td> Q_NK   </td><td> =Qmx_NK      </td><td> ncyc()     </td></tr>
!!<tr><td> 31 </td><td> Q_PK   </td><td> =Qmx_PK      </td><td> po4s()    </td></tr>
!!<tr><td> 32 </td><td> Q_SK   </td><td> =Qmx_SK      </td><td> silikat()     </td></tr>
!!<tr><td> 33 </td><td> Q_NG   </td><td> =Qmx_NG      </td><td> ncyc()     </td></tr>
!!<tr><td> 34 </td><td> Q_PG   </td><td> =Qmx_PG      </td><td> po4s()    </td></tr>
!!<tr><td> 35 </td><td> Q_NB   </td><td> =Qmx_NB      </td><td> ncyc()     </td></tr>
!!<tr><td> 36 </td><td> Q_PB   </td><td> =Qmx_PB      </td><td> po4s()    </td></tr>
!!<tr><td> 37 </td><td> CD(1   </td><td>         </td><td>  </td></tr>
!!<tr><td> 38 </td><td> CD(2   </td><td>         </td><td>  </td></tr>
!!<tr><td> 39 </td><td> CP(1   </td><td>         </td><td>  </td></tr>
!!<tr><td> 40 </td><td> CP(2   </td><td>         </td><td>  </td></tr>
!!<tr><td> 41 </td><td> CM   </td><td>         </td><td>  </td></tr>
!!<tr><td> 42 </td><td> BAC   </td><td>         </td><td>  </td></tr>
!!<tr><td> 43 </td><td> O2BSB   </td><td>         </td><td>  </td></tr>
!!<tr><td> 44 </td><td> BL01   </td><td>         </td><td>  </td></tr>
!!<tr><td> 45 </td><td> BL02   </td><td>         </td><td>  </td></tr>
!!<tr><td> 46 </td><td> vbsb   </td><td>         </td><td>  </td></tr>
!!<tr><td> 47 </td><td> vcsb   </td><td>         </td><td>  </td></tr>
!!<tr><td> 51 </td><td> abrzo1   </td><td>         </td><td>  </td></tr>
!!<tr><td> 52 </td><td> ssalg   </td><td>         </td><td>  </td></tr>
!!<tr><td> 54 </td><td> fssgr   </td><td>         </td><td>  </td></tr>
!!<tr><td> 55 </td><td> fbsgr   </td><td>         </td><td>  </td></tr>
!!<tr><td> 56 </td><td> frfgr   </td><td>         </td><td>  </td></tr>
!!<tr><td> 57 </td><td> nl0   </td><td> nl0(ior) = orgNn/(ocsbt/2.8) </td><td> orgc </td></tr>
!!<tr><td> 58 </td><td> pl0   </td><td> pl0s(mstr,mRB) = orgP/(ocsbs(mstr,mRB)/2.8) pl0(ior) = orgPn/(ocsbt/2.8)</td><td> qsim + orgc </td></tr>
!!<tr><td> 59 </td><td> stind   </td><td>         </td><td>  </td></tr>
!!<tr><td> 60 </td><td> dlarvn   </td><td>         </td><td>  </td></tr>
!!<tr><td> 63 </td><td> pw   </td><td>         </td><td>  </td></tr>
!!<tr><td> 69 </td><td> SKmor   </td><td> ?? unklar ??      </td><td> nicht in silikat(), qsim() ??? </td></tr>
!!<tr><td> 70 </td><td> DOSCF   </td><td>         </td><td>  </td></tr>
!!</table>
!! \n\n aus randbedingungen.f95 , zurück: \ref Modellerstellung oder \ref zuflussranddaten
subroutine randbedingungen_ergaenzen(j,einmalig)
   use modell
   use QSimDatenfelder
   use aparam
   implicit none
   integer :: j,nk
   real :: CPges,CDges,Cref,TOC
   logical einmalig
   !     if(j.eq.1)print*,'randbedingungen_ergaenzen'
   nk = (j-1)*number_plankt_vari
   kontroll = .false.
   if (j == kontrollknoten)kontroll = .true.
   !     ini_algae in initialisieren() initialisieren.f95
   call algae_start(                     &
                    planktonic_variable(11+nk),      & ! CHLA chlas(mstr,mRB),    &
                    planktonic_variable(19+nk),      & ! VKIGR vkigrs(mstr,mRB),    &
                    planktonic_variable(20+nk),      & ! ANTBL antbls(mstr,mRB),    &
                    planktonic_variable( 1+nk),      & ! tempws Tempw temperw()
                    planktonic_variable(24+nk),      & ! akbcm akbcms(mstr,mRB),    &
                    planktonic_variable(26+nk),      & ! abbcm abbcms(mstr,mRB),    &
                    planktonic_variable(25+nk),      & ! agbcm agbcms(mstr,mRB),                &
                    planktonic_variable( 8+nk),      & ! aki akis(mstr,mRB),    &
                    planktonic_variable(10+nk),      & ! abl abls(mstr,mRB),    &
                    planktonic_variable( 9+nk),      & ! agr agrs(mstr,mRB),                      &
                    a1Ki,a1Bl,a1Gr,                  & ! globale Parameter direkt aus QSimDatenfelder
                    Caki,Cabl,Cagr,                  & ! Kohlenstoffgehalte der Biomassen direkt aus QSimDatenfelder
                    akchl,abchl,agchl,               & ! APARAM.txt globale Parameter direkt aus QSimDatenfelder
                    planktonic_variable(12+nk),      & ! chlaki chlaks(mstr,mRB),    &
                    planktonic_variable(14+nk),      & ! chlabl chlabs(mstr,mRB),    &
                    planktonic_variable(13+nk))        ! chlagr chlags(mstr,mRB)                 &
   ! qsim.f90:!...Festlegung der Anfangs-Sedimenttemperatur Tsed = TWasser
   benthic_distribution(1+(j-1)*number_benth_distr) = planktonic_variable(1+((j-1)*number_plankt_vari))
   !!!!!! für orgC() : CSB(ocsb) und C-BSB5(obsb) in die Berechnungskonzentrationen aufteilen Bakterienmenge abschätzen ...
   if (kontroll)print*,'randbedingungen_ergaenzen: vor orgc_start'  &
       ,' OBSB = ',planktonic_variable(17+nk)  &
       ,' OCSB = ',planktonic_variable(18+nk)  &
       ,' O2BSB = ',planktonic_variable(43+nk)
   call orgc_start(                      &
                   TOC_CSB,bsbZoo,GROT,            & ! globale Parameter direkt aus QSimDatenfelder
                   planktonic_variable( 8+nk),      & ! aki | akis
                   planktonic_variable(10+nk),      & ! abl | abls
                   planktonic_variable( 9+nk),      & ! agr | agrs
                   Caki,Cabl,Cagr,CZoo,             & ! Kohlenstoffgehalte der Biomassen direkt aus QSimDatenfelder
                   bsbki,bsbbl,bsbgr,               & ! APARAM.txt globale Parameter direkt aus QSimDatenfelder
                   csbki,csbbl,csbgr,               & ! APARAM.txt globale Parameter direkt aus QSimDatenfelder
                   planktonic_variable(50+nk),      & ! ZOOIND | zooins
                   planktonic_variable(46+nk),      & ! vbsb  BSB5 incl. lebender Organismen, messbar, Randwert | vbsbs
                   planktonic_variable(47+nk),      & ! vcsb  CSB incl. lebender Organismen , messbar, Randwert | vcsbs
                   planktonic_variable(17+nk),      & ! OBSB  C-BSB5, kohlenstoffbürtig | obsbs
                   planktonic_variable(18+nk),      & ! OCSB  CSB, Kohlenstoffbürtig    | ocsbs
                   planktonic_variable(41+nk),      & ! CM   |  CMs
                   planktonic_variable(37+nk),      & ! CD(1 |  CDs1
                   planktonic_variable(38+nk),      & ! CD(2 |  CDs2
                   planktonic_variable(39+nk),      & ! CP(1 |  CPs1
                   planktonic_variable(40+nk),      & ! CP(2 |  CPs2
                   planktonic_variable(52+nk),      & ! ssalg | ssalgs
                   planktonic_variable(56+nk),      & ! frfgr | frfgrs
                   planktonic_variable(42+nk),      & ! BAC   | BACs
                   planktonic_variable(48+nk),      & ! CHNF  | CHNFs,
                   CPges,CDges,Cref,TOC )             ! Übergabewerte spez. für den jeweiligen Rand, nur hier definiert
   if (kontroll)print*,'nach orgc_start'     &
       ,' OBSB = ',planktonic_variable(17+nk)  &
       ,' OCSB = ',planktonic_variable(18+nk)  &
       ,' O2BSB = ',planktonic_variable(43+nk)
   call naehr_start(                     &
                    planktonic_variable( 8+nk),      & ! aki | akis
                    planktonic_variable(10+nk),      & ! abl | abls
                    planktonic_variable( 9+nk),      & ! agr | agrs
                    planktonic_variable( 3+nk),      & ! VNH4 | vnh4s,
                    planktonic_variable( 5+nk),      & ! VNO3 | vNO3s,
                    planktonic_variable( 4+nk),      & ! VNO2 | vno2s,
                    planktonic_variable(67+nk),      & ! GESN | gesNs,
                    planktonic_variable(50+nk),      & ! ZOOIND | zooins
                    nZoo, pZoo, GROT,               & ! globale Parameter direkt aus QSimDatenfelder
                    planktonic_variable( 6+nk),      & ! GELP | gelPs,
                    planktonic_variable(68+nk),      & ! GESP | gesPs,
                    planktonic_variable(30+nk),      & ! Q_NK  | Q_NKs
                    planktonic_variable(31+nk),      & ! Q_PK  | Q_PKs
                    planktonic_variable(32+nk),      & ! Q_SK  | Q_SKs
                    planktonic_variable(33+nk),      & ! Q_NG  | Q_NGs
                    planktonic_variable(34+nk),      & ! Q_PG  | Q_PGs
                    planktonic_variable(35+nk),      & ! Q_NB  | Q_NBs
                    planktonic_variable(36+nk),      & ! Q_PB  | Q_PBs
                    Qmx_NK,Qmn_NK,Qmx_PK,Qmn_PK,Qmx_SK,Qmn_SK, Qmx_NG,Qmn_NG,Qmx_PG,Qmn_PG, Qmx_NB,Qmn_NB,Qmx_PB,Qmn_PB,  & ! APARAM.txt globale Parameter direkt aus QSimDatenfelder
                    CPges,CDges,Cref,                & ! Übergabewerte spez. für den jeweiligen Rand, nur hier definiert
                    planktonic_variable(42+nk),      & ! BAC   | BACs
                    planktonic_variable(41+nk),      & ! CM   |  CMs
                    planktonic_variable(57+nk),      & ! nl0 Stickstoff zu Kohlenstoff in organischem Material | nl0s,
                    planktonic_variable(58+nk),      & ! pl0 Phosphor zu Kohlenstoff in organischem Material | pl0s,
                    planktonic_variable(53+nk),      & ! SS suspendierte Sedimente ohne lebende Organismen | sss,
                    planktonic_variable(52+nk),      & ! ssalg GESAMTSCHWEBSTOFFE | ssalgs,
                    0,0,0,0,                         & ! itags,monats,mstr,mRB für Kontrollausgaben in 1D benötigt
                    einmalig,kontroll,j)
   if (kontroll)print*,'randbedingungen_ergaenzen: nach algae_aufteilung'     &
       ,' chla = ',planktonic_variable(11+nk)    &
       ,' vkigr = ',planktonic_variable(19+nk)   &
       ,' antbl = ',planktonic_variable(20+nk)   &
       ,' chlaki = ',planktonic_variable(12+nk)  &
       ,' aki = ',planktonic_variable(8+nk)      &
       ,' akbcm = ',planktonic_variable(24+nk), ' Caki = ',Caki
   !     Berechnung des p-Wertes am Start (ohne Algen)
   call pwert(                           &
              planktonic_variable(62+nk),      & ! MW | mws
              planktonic_variable(66+nk),      & ! VPH | vphs
              planktonic_variable(65+nk),      & ! LF | lfs
              planktonic_variable( 1+nk),      & ! Tempw | tempws
              planktonic_variable(63+nk))! ,      & ! pw | pws
   !     &     0,0)                               !  mRB,mstr
   !      subroutine pwert(mws,vphs,lfs,tempws,pws) !!wy azStrs nicht mehr benötigt
   !
   return
end subroutine randbedingungen_ergaenzen
!----+-----+----
!> <h1>Legt die Randbedingungs-Datenfelder an </h1>
!! .... to be done PARALLEL\n
!! \n\n aus randbedingungen.f95 , zurück: \ref zuflussranddaten
!!
subroutine randbedingungen_parallel()
   use modell
   implicit none
   integer :: as, ini
   
   !      print*,meinrang,' randbedingungen_parallel() ,part, number_rb_hydraul,number_rb_wetter='  &
   !           ,part, number_rb_hydraul,number_rb_wetter
   allocate (rb_hydraul_p(part*number_rb_hydraul), stat = as )
   if (as /= 0) then
      write(fehler,*)' Rueckgabewert allocate  von rb_hydraul_p(part   :', as
      call qerror(fehler)
   end if
   do ini = 1,part*number_rb_hydraul
      rb_hydraul_p(ini) = 0.0
   end do
   call MPI_Scatter(rb_hydraul, part*number_rb_hydraul, MPI_FLOAT,  &
                    rb_hydraul_p, part*number_rb_hydraul, MPI_FLOAT, 0, mpi_komm_welt, ierr)
   if (ierr /= 0) then
      write(fehler,*)'randbedingungen_parallel MPI_Scatter(rb_hydraul failed :', ierr
      call qerror(fehler)
   end if
   !print*,'rb_hydraul_p successfully allocated and scattered',meinrang
   call mpi_barrier (mpi_komm_welt, ierr)
   !      allocate (rb_wetter_p(part*number_rb_wetter), stat = as )
   !      if(as.ne.0)then
   !         write(fehler,*)' Rueckgabewert von allocate rb_wetter_p(part   :', as
   !         call qerror(fehler)
   !      end if
   !      do ini=1,part*number_rb_wetter
   !         rb_wetter_p(ini)=0.0
   !      end do
   call MPI_Bcast(rb_extnct_ilamda,1,MPI_INT,0,mpi_komm_welt,ierr)
   if (ierr /= 0) then
      write(fehler,*)meinrang, 'MPI_Bcast(rb_extnct_ilamda,  ierr = ', ierr
      call qerror(fehler)
   end if
   !print*,meinrang, 'nachher rb_extnct_ilamda=', rb_extnct_ilamda
   call mpi_barrier (mpi_komm_welt, ierr)
   !print*,meinrang, ' anz_extnct_koeff=', anz_extnct_koeff
   !! parameter nicht broadcasten !!!! call MPI_Bcast(anz_extnct_koeff,1,MPI_INT,0,mpi_komm_welt,ierr)
   if (meinrang /= 0) then
      allocate (rb_extnct_p(rb_extnct_ilamda*anz_extnct_koeff), stat = as )
      if (as /= 0) then
         write(fehler,*)'rb_extnct_p konnte nicht allokiert werden ', as, ' prozess = ', meinrang
         call qerror(fehler)
      end if ! alloc_status .ne.0
      do ini = 1,rb_extnct_ilamda*anz_extnct_koeff
         rb_extnct_p(ini) = 0.0
      end do
   end if ! meinrang.ne.0
   !   ### call MPI_Scatter(rb_extnct, rb_extnct_ilamda*anz_extnct_koeff, MPI_FLOAT,  &
   !& rb_extnct_p, rb_extnct_ilamda*anz_extnct_koeff, MPI_FLOAT, 0, mpi_komm_welt, ierr)
   call MPI_Bcast(rb_extnct_p,rb_extnct_ilamda*anz_extnct_koeff,MPI_FLOAT,0,mpi_komm_welt,ierr)
   if (ierr /= 0) then
      write(fehler,*)'randbedingungen_parallel MPI_Bcast(rb_extnct_p failed :', ierr
      call qerror(fehler)
   end if
   !print*,'eta(ilamda)=rb_extnct_p(1 + (rb_extnct_ilamda-1)*anz_extnct_koeff),meinrang'  &
   !      ,rb_extnct_p(1 + (rb_extnct_ilamda-1)*anz_extnct_koeff),meinrang
   !call MPI_Bcast(transfer_parameter_p,number_trans_aparam,MPI_FLOAT,0,mpi_komm_welt,ierr
   return
end subroutine randbedingungen_parallel
!----+-----+----
!> <h1>Verteilen der Datenstrukturen auf die parallelen Prozesse</h1>
!! .... to be done PARALLEL\n
!! \n\n aus randbedingungen.f95 , zurück: \ref zuflussranddaten
!!
subroutine scatter_BC()
   use modell
   implicit none
   integer :: i
   call MPI_Scatter(rb_hydraul, part*number_rb_hydraul, MPI_FLOAT,  &
                    rb_hydraul_p, part*number_rb_hydraul, MPI_FLOAT, 0, mpi_komm_welt, ierr)
   if (ierr /= 0) then
      write(fehler,*)' MPI_Scatter(rb_hydraul failed :', ierr
      call qerror(fehler)
   end if
   return
end subroutine scatter_BC
!----+-----+----
!> <h1>Randwert-Interpolation</h1>
!! lineare Interpolation zwischen den gültigen Randwerten:\n
!! davor und danach konstant \n
!! analog zu QSim-1D/funkstar.f90
!! \n\n aus randbedingungen.f95 , zurück: \ref zuflussranddaten
!!
subroutine RB_werte_aktualisieren(t)
   use modell
   implicit none
   integer n,j,k
   integer t, zeit_vor, zeit_nach
   real ::a, wert_vor, wert_nach, wert
   logical :: randwert_gueltig, vor_da, nach_da
   do n = 1,ianz_rb !! alle Randbedingungen
      rabe(n)%wert_jetzt(:) = 0.0 ! initialize unused boundary concentrations
      do k = 1,n_active_concentrations !all active boundary concentrations
         vor_da = .false.
         nach_da = .false.
         do j = 1,rabe(n)%anz_rb ! alle Zeitpunkte dieser RB
            wert = rabe(n)%punkt(j)%zeil%werts(k)
            if (randwert_gueltig(wert,k)) then
               if (t >= rabe(n)%punkt(j)%zeit_sek) then ! letzter gültiger Wert vorher
                  vor_da = .true.
                  wert_vor = wert
                  zeit_vor = rabe(n)%punkt(j)%zeit_sek
               else
                  if ( .not. nach_da) then ! erster gültiger Wert danach
                     nach_da = .true.
                     wert_nach = wert
                     zeit_nach = rabe(n)%punkt(j)%zeit_sek
                  endif
               endif ! Zeitpunkt vorher
               !else ! randwert_gueltig
               !   print*,'randwert_gueltig .FALSE. Rand-n, Variable-k, Zeitpunkt-j,wert',n,k,j,wert
            endif ! randwert_gueltig
         end do ! alle Zeitintervalle in dieser Randbedingung
         if (vor_da .and. nach_da) then
            a = real(t-zeit_vor)/real(zeit_nach-zeit_vor)
            rabe(n)%wert_jetzt(k) = (1.0-a)*wert_vor + a*wert_nach
            if ((k == 22) .and. (kontrollknoten > 0)) &
                print*,'Rand',n,' t,zeit_vor,zeit_nach,wert_vor,wert_nach,a,rabe(n)%wert_jetzt(k) = '  &
                ,t,zeit_vor,zeit_nach,wert_vor,wert_nach,a,rabe(n)%wert_jetzt(k)
         endif
         if (vor_da .and. ( .not. nach_da)) then
            rabe(n)%wert_jetzt(k) = wert_vor
            if (k == 22)print*,'Rand',n,' t,zeit_vor,wert_vor,rabe(n)%wert_jetzt(k) = '  &
                ,t,zeit_vor,wert_vor,rabe(n)%wert_jetzt(k)
         endif
         if (( .not. vor_da) .and. nach_da) then
            rabe(n)%wert_jetzt(k) = wert_nach
            if ((k == 22) .and. (kontrollknoten > 0))  &
                print*,'Rand',n,' t,zeit_nach,wert_nach,rabe(n)%wert_jetzt(k) = '  &
                ,t,zeit_nach,wert_nach,rabe(n)%wert_jetzt(k)
         endif
         if (( .not. vor_da) .and. ( .not. nach_da)) then ! no valid value
            select case (k) ! which value
               case (1) ! Abfluss alle Werte gültig (in QSim-3D unbenutzt)
                  rabe(n)%wert_jetzt(k) = 0.0
                  !case (22) ! Wassertemperatur
               case (24) ! CHNF Heterotrophe Nanoflagelaten, not present
                  rabe(n)%wert_jetzt(k) = 0.0
               case (25) ! BVHNF Biovolumen der HNF, not present
                  rabe(n)%wert_jetzt(k) = 0.0
               case (26) ! COLI Fäkalcoliforme Bakterien, not present
                  rabe(n)%wert_jetzt(k) = 0.0
                  !case (27) ! EWAERM Wärmeeinleitung ### egal
                  !case (28) ! Tracer ### egal
                  case default ! alle anderen
                  print*,'rabe(n)%punkt(j)%zeil%werts(k)',(rabe(n)%punkt(j)%zeil%werts(k),j = 1,rabe(n)%anz_rb)
                  print*,'ianz_rb, anzrawe, rabe(n)%anz_rb, rabe(n)%t_guelt',ianz_rb, anzrawe, rabe(n)%anz_rb, rabe(n)%t_guelt
                  print*,'RB_werte_aktualisieren: Kein wert nirgends für Rand-variable ',k,' an Rand ',n, ' für Zeitpunkt ',t
                  write(fehler,*)'RB_werte_aktualisieren: Kein wert nirgends für Rand-variable '  &
                  ,k,' an Rand ',n, ' für Zeitpunkt ',t
                  call qerror(fehler)
            end select ! k
         endif ! no valid value
      end do ! alle k werte
   end do ! alle n Randbedingungen
   !if(hydro_trieb.eq.2)then ! untrim
   !   rabe(2)%wert_jetzt(28)= 1.0 ! untrim Tracer inflow test
   !   print*,'### untrim boundary 2 Tracer constant 1.0 ###'
   !end if
   return
end subroutine RB_werte_aktualisieren
!----+-----+----
!> <h1>Randwert gueltig?</h1>
!! \n\n aus randbedingungen.f95 , zurück: \ref zuflussranddaten
logical function randwert_gueltig(wert,n)
   use modell
   implicit none
   integer :: n
   real :: wert
   randwert_gueltig = .false.
   if ((n > n_active_concentrations) .or. (n <= 0)) then
      write(fehler,*)n,' keine gültige Randwertnummer #### Abbruch ####'
      call qerror(fehler)
   end if
   select case (n) ! Unterscheidung Randwert-variable
      case (1) ! Abfluss alle Werte gültig (in QSim-3D unbenutzt)
         randwert_gueltig = .TRUE.
         !case (22) ! Wassertemperatur
         !   randwert_gueltig = (wert.gt. -9.99)
         !case (24) ! CHNF Heterotrophe Nanoflagelaten ### egal
         !   randwert_gueltig = .TRUE.
         !case (25) ! BVHNF Biovolumen der HNF  ### egal
         !   randwert_gueltig = .TRUE.
         !case (26) ! COLI Fäkalcoliforme Bakterien  ### egal
         !   randwert_gueltig = .TRUE.
      case (27) ! EWAERM Wärmeeinleitung ### egal
         ! randwert_gueltig = (wert.gt. -99.9)
         randwert_gueltig = .TRUE.
      case (28) ! Tracer ### egal
         randwert_gueltig = .TRUE.
         case default ! alle anderen
         randwert_gueltig = (wert >= 0.0)
   end select
   return
end function randwert_gueltig
!----+-----+----
!> ereigg_Randbedingungen_lesen() wird beschrieben in: \ref zuflussranddaten
!! \n\n aus randbedingungen.f95
subroutine ereigg_Randbedingungen_lesen()
   use modell
   implicit none
   character(500) dateiname, text
   integer :: open_error, ion, read_error, alloc_status, ini
   integer :: idumm, anzi, i, j, n, m, min_nr, nr, maxrandnr, nini
   integer :: anzmax = -1
   logical :: rb_vorhanden, randwert_gueltig
   integer , allocatable , dimension (:) :: nr_vorhanden, rb_vorkommen
   type(rb_zeile) , allocatable , dimension (:) :: lesezeil
   print*,'ereigg_Randbedingungen_lesen() startet:'
   
   n_active_concentrations = anzrawe
   if (ischwer == 0 .and. ikonss == 0)n_active_concentrations = 28
   write(dateiname,'(2A)')trim(modellverzeichnis),'EREIGG.txt'
   ion = 92
   open ( unit = ion , file = dateiname, status = 'old', action = 'read ', iostat = open_error )
   if (open_error /= 0) then
      write(fehler,*)'open_error EREIGG.txt ... Datei vorhanden?'
      call qerror(fehler)
   end if ! open_error.ne.0
   rewind (ion)
   ! erster Lesedurchgang:
   do n = 1,6  ! Kopf überlesen (modell::ereigg_modell())
      if ( .not. zeile(ion)) call qerror('Kopf überlesen in ereigg_Randbedingungen_lesen() schlägt fehl')
   end do ! Kopf überlesen
   ianz_rb = 0
   min_nr = 9999
   max_rand_nr = -9999
   ! 343 read(ion,*, iostat = read_error,end=341)nr,idumm,idumm,anzi
   343 continue
   if ( .not. zeile(ion)) goto 341
   read(ctext, *, iostat = read_error) nr,idumm,idumm,anzi
   if (read_error /= 0) then
      print*,trim(ctext)
      write(fehler,*)' 343 ereigg_Randbedingungen_lesen() read_error /= 0 ianz_rb = ', ianz_rb
      call qerror(fehler)
   endif
   !print*,'RB; nr,anzi=',nr,anzi
   if (min_nr > nr) min_nr = nr
   if (max_rand_nr < nr) max_rand_nr = nr
   ianz_rb = ianz_rb+1
   if (anzi > anzmax)anzmax = anzi
   if (anzi == 0)goto 343
   do n = 1,anzi  ! Datenzeilen im Ersten Durchgang überlesen.
      if ( .not. zeile(ion)) then
         write(fehler,*)'read_error in EREIGG.txt; Randbedingung # ',ianz_rb,' Zeile ',n
         call qerror(fehler)
      end if ! open_error.ne.0
   end do
   !imstr(ianzRB) = mstr
   !iRBNR(ianzRB) = RBNR
   !ianzW(ianzRB) = NrSchr(mstr,RBNR)
   !
   !      do 226 iwe = 1,NrSchr(mstr,RBNR)
   !      read(92,9240)                                                     &
   !     &itagl(ianzRB,iwe),monatl(ianzRB,iwe),jahrl(ianzRB,iwe)            &
   !     &,uhrl(ianzRB,iwe),(werts(ianzRB,ixpp,iwe),ixpp=1,28)
   !  226 continue
   goto 343
   341  continue
   print*,'ereigg_Randbedingungen_lesen --- ',ianz_rb,' Randbedingungen mit maximal ', anzmax, ' Zeilen'
   !
   allocate (rabe(ianz_rb), stat = alloc_status )
   if (alloc_status /= 0) then
      write(fehler,*)'rabe konnte nicht allokiert werden ', alloc_status
      call qerror(fehler)
   end if ! alloc_status .ne.0
   do ini = 1,ianz_rb
      rabe(ini)%anz_rb = 0
   end do
   if ((max_rand_nr <= 0) .or. (max_rand_nr > 9999)) then
      write(fehler,*)'eine maximale Randbedingungsnummer von: ', max_rand_nr, ' ist doch wohl nicht sinnvoll?'
      call qerror(fehler)
   end if ! Randbedingungsnummer
   !
   ! zweiter Lesedurchgang:
   rewind (ion)
   do n = 1,6  ! Kopf überlesen
      if ( .not. zeile(ion)) call qerror('zweiter Lesedurchgang ereigg_Randbedingungen_lesen() Kopf überlesen schlägt fehl')
   end do ! Kopf überlesen
   do n = 1,ianz_rb !! alle Randbedingungsblöcke nochmal lesen
      if ( .not. zeile(ion)) call qerror(' ereigg_Randbedingungen_lesen() Randbedingungsblöcke nochmal lesen schlägt fehl')
      read(ctext, *, iostat = read_error) rabe(n)%nr_rb, idumm, rabe(n)%tagesmittelwert_flag, anzi
      if (read_error /= 0) then
         write(fehler,*)' 344 read_error /= 0 ereigg_Randbedingungen_lesen()'
         call qerror(fehler)
      endif
      !print*,'block-Kopf:',rabe(n)%nr_rb, idumm, rabe(n)%tagesmittelwert_flag, anzi
      allocate (lesezeil(anzi), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)'lesezeil konnte nicht allokiert werden ', alloc_status
         call qerror(fehler)
      end if ! alloc_status .ne.0
      do ini = 1,anzi
         lesezeil(ini)%itag = 0
      end do
      
      i = 0
      do m = 1,anzi !alle Zeitpunkte dieser Randlinie
         !! HIER !! wird die Randbedingungszeile in die Struktur rb_zeile gelesen    !! HIER !!
         if ( .not. zeile(ion)) call qerror('ereigg_Randbedingungen_lesen() alle Zeitpunkte dieser Randlinie lesen schlägt fehl')
         lesezeil(m)%werts(:) = -1.0
         read(ctext, *, iostat = read_error)lesezeil(m)%itag, lesezeil(m)%imonat , lesezeil(m)%ijahrl, lesezeil(m)%uhrl  &
              ,lesezeil(m)%werts(1:n_active_concentrations)
         if (read_error /= 0) then
            print*,read_error,' = read_error n_active_concentrations,anzrawe = '
            print*,n_active_concentrations,anzrawe, ischwer,' = ischwer ikonss',ikonss
            print*,m,anzi,n,' = m-th line of anzi in n-th RB , ctext:'
            print*,trim(ctext)
            print*,'lesezeil(m):'
            print*,lesezeil(m)
            call qerror("read_error EREIGG.txt ... Zeitpunkt Datenzeile lesen")
         endif
         do j = 1,anzrawe
            if (isNaN(lesezeil(m)%werts(j))) then
               print*,m,j,"isNaN(lesezeil(m)%werts(j))"
               print*,trim(ctext)
               print*,'lesezeil(m):'
               print*,lesezeil(m)
               call qerror("isNaN(lesezeil(m)%werts(j)")
            endif !isnan
         end do !anzrawe
         if (randwert_gueltig(lesezeil(m)%werts(22),22)) then
            i = i+1
         end if ! temperatur positif
      end do !! all m time-points at this boundary
      rabe(n)%t_guelt = i
      rabe(n)%anz_rb = anzi
      allocate (rabe(n)%punkt(rabe(n)%anz_rb), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)'ereigg_Randbedingungen_lesen():allocate (rabe(n)%punkt(i) fehlgeschlagen ', alloc_status
         call qerror(fehler)
      end if ! alloc_status .ne.0
      do ini = 1,rabe(n)%anz_rb
         rabe(n)%punkt(ini)%zeit_sek = 0
      end do
      do i = 1,rabe(n)%anz_rb !alle Zeitpunkte dieser Randlinie
         !! HIER !! wird die Randbedingungszeile in die Struktur rb eingefügt  !! HIER !!
         rabe(n)%punkt(i)%zeil = lesezeil(i)
         tag = rabe(n)%punkt(i)%zeil%itag
         monat = rabe(n)%punkt(i)%zeil%imonat
         jahr = rabe(n)%punkt(i)%zeil%ijahrl
         uhrzeit_stunde = rabe(n)%punkt(i)%zeil%uhrl
         call sekundenzeit(2)
         rabe(n)%punkt(i)%zeit_sek = zeitpunkt
         call zeitsekunde() !! damit auch die Uhrzeit stimmt
         !print 228, jahr, monat, tag, stunde, minute, sekunde &
         !         , rabe(n)%punkt(i)%zeil%werts(22)
         !228 FORMAT (I4.2,"-",I2.2,"-",I2.2," ",I2.2,":",I2.2,":",I2.2,"    ",F7.2)
      end do !! alle Zeitpunkte dieser Randlinie
      deallocate (lesezeil)
      print*,'Randbedingung ',n,' hat Nr. ',rabe(n)%nr_rb,' und enthält ',rabe(n)%anz_rb &
      ,' Zeitpunkte, von denen ',rabe(n)%t_guelt  &
      ,' sinnvolle Temperatur-Werts(22) enthalten; Tagesmittelwert-0 oder Zeitwert-1 ? ' &
      , rabe(n)%tagesmittelwert_flag
      if (rabe(n)%nr_rb <= 0) then
         write(fehler,*)'###Abbruch 345 ### Randbedingungsnummern müssen größer als 0 sein !!'
         call qerror(fehler)
      end if
   end do !! alle Randbedingungslinien nochmal lesen
   !! Nummerierung der Ränder eindeutig?
   do i = 1,ianz_rb
      do n = i+1,ianz_rb
         if ( rabe(i)%nr_rb == rabe(n)%nr_rb) then
            write(fehler,*)' 11 Randnummer von Rand', i, ' = ',rabe(i)%nr_rb,' ist gleich Randnummer von Rand ',n
            call qerror(fehler)
         endif
      end do ! alle n Zonen
   end do ! alle i Zonen
   !
   select case (hydro_trieb)
      case(1) ! casu-transinfo
         maxrandnr = 0
         do j = 1,knotenanzahl2D !! max Randnummer ermitteln:
            if (knoten_rand(j) > 0) then !! Randknoten
               if (knoten_rand(j) > maxrandnr)maxrandnr = knoten_rand(j)
            end if ! Randknoten
         end do ! alle j Knoten
         allocate (nr_vorhanden(maxrandnr), stat = alloc_status )
         do ini = 1,maxrandnr !! Randnummernvorkommen initialisieren
            nr_vorhanden(ini) = 0
         end do
         allocate (rb_vorkommen(maxrandnr), stat = alloc_status )
         if (alloc_status /= 0) then
            write(fehler,*)'nr_vorhanden(maxrandnr) konnte nicht allokiert werden ', alloc_status
            call qerror(fehler)
         end if ! alloc_status .ne.0
         do ini = 1,maxrandnr !! Rand-vorkommen initialisieren
            rb_vorkommen(ini) = 0
         end do
         do j = 1,knotenanzahl2D !! vorhandene Randummern durchzählen.
            if (knoten_rand(j) > 0)nr_vorhanden(knoten_rand(j)) = nr_vorhanden(knoten_rand(j)) + 1
         end do ! alle j Knoten
         do j = 1,maxrandnr !! alle Randnummern ausgeben:
            if (nr_vorhanden(j) > 0) then
               print*,'Randnummer ',j,' kommt an ',nr_vorhanden(j),' Knoten vor.'
            else
               print*,'Randnummer ',j,' kommt nie vor.'
            end if !
         end do ! alle j Randnummern
         do j = 1,maxrandnr !! Randbedingung für alle Randnummern vorhanden?
            rb_vorkommen(j) = 0
            do n = 1,ianz_rb !! alle Randbedingungen
               if (j == rabe(n)%nr_rb) then
                  print*,'Randnummer mit dem Zähler',n,' bedient die Randbedingung mit der Nummer ',rabe(n)%nr_rb
                  rb_vorkommen(j) = rb_vorkommen(j) + 1
               end if
            end do !! alle n Randbedingungen
            if (rb_vorkommen(j) < 1)print*,'### Warnung ###: für Randnummer ',j,' wurde keine Randbedingung vorgegeben.'
            if (rb_vorkommen(j) > 1) then
               write(fehler,*)'### Abbruch 284 ### für Randnummer ',j,' wurden ',rb_vorkommen(j), ' Randbedingungen angegeben.'
               call qerror(fehler)
            end if
         end do ! alle j Randnummern
         !! Randzähler in Feld knoten_rand() schreiben
         do j = 1,knotenanzahl2D
            !if(j.eq.kontrollknoten)print*,"Kontrollknoten #",j
            if (knoten_rand(j) > 0) then !! falls Randknoten
               if (rb_vorkommen(knoten_rand(j)) == 0) then !! nicht bediente Randnummern ausrangieren.
                  knoten_rand(j) = 999999
               else
                  nini = -7
                  do n = 1,ianz_rb !! alle Randbedingungen
                     if (knoten_rand(j) == rabe(n)%nr_rb) then
                        nini = n
                     end if
                  end do !! alle n Randbedingungen
                  knoten_rand(j) = nini
               end if !! nicht bediente Randnummer
               if (j == kontrollknoten) then
                  print*,"Kontrollknoten #",j," ist Randknoten mit der Rand-zähler ", knoten_rand(j)
                  if (knoten_rand(j) <= ianz_rb) then
                     print*,"Diesem Randknoten ist keine Randbedingung #",rabe(knoten_rand(j))%nr_rb," zugeordnet."
                  else
                     print*,"Diesem Randknoten ist keine Randbedingung zugeordnet"
                  endif !Randbedingung zugeordnet
               end if ! kontrollknoten
            end if ! Randknoten
         end do ! alle j Knoten
         deallocate (nr_vorhanden)
         deallocate (rb_vorkommen)
      case(2) ! Untrim² netCDF
         min_nr = 99999
         maxrandnr = 0
         do j = 1,n_elemente
            if (element_rand(j) > maxrandnr) maxrandnr = element_rand(j)
            if ( (element_rand(j) < min_nr) .and. (element_rand(j) > 0) )min_nr = element_rand(j)
            rb_vorhanden = .false.
            do n = 1,ianz_rb !! alle Randbedingungen
               if (element_rand(j) == rabe(n)%nr_rb)rb_vorhanden = .true.
            end do !! alle n Randbedingungen
            if (( .not. rb_vorhanden) .and. (element_rand(j) /= 0))  &
                print*,'ereigg_Randbedingungen_lesen Untrim: element ',j,' randnummer = ' &
                ,element_rand(j),' zugehörige Randbedingung fehlt'
         end do
         allocate (nr_vorhanden(maxrandnr), stat = alloc_status )
         allocate (rb_vorkommen(maxrandnr), stat = alloc_status )
         do j = 1,maxrandnr !! Randbedingung für alle Randnummern vorhanden?
            nr_vorhanden(j) = 0
            rb_vorkommen(j) = 0
            do n = 1,ianz_rb !! alle Randbedingungen
               if (j == rabe(n)%nr_rb) then
                  print*,'Randvorgabe mit dem Zähler',n,' bedient die Randbedingung mit der Nummer ',rabe(n)%nr_rb
                  rb_vorkommen(j) = rb_vorkommen(j) + 1
               end if
            end do !! alle n Randbedingungen
            if (rb_vorkommen(j) < 1)print*,'### Warnung ###: für Randnummer ',j,' wurde keine Randbedingung vorgegeben.'
            if (rb_vorkommen(j) > 1) then
               write(fehler,*)'### Abbruch 284 ### für Randnummer ',j,' wurden ',rb_vorkommen(j), ' Randbedingungen angegeben.'
               call qerror(fehler)
            end if
         end do ! alle j Randnummern
         do j = 1,n_elemente !! vorhandene Randummern durchzählen.
            if (element_rand(j) > 0)nr_vorhanden(element_rand(j)) = nr_vorhanden(element_rand(j)) + 1
         end do ! alle j Elemente
         do j = 1,maxrandnr !! alle Randnummern ausgeben:
            if (nr_vorhanden(j) > 0) then
               print*,'Randnummer ',j,' kommt an ',nr_vorhanden(j),' Elementen vor.'
            else
               print*,'Randnummer ',j,' kommt nie vor.'
            end if !
         end do ! alle j Randnummern
         print*,'### ereigg_Randbedingungen_lesen: Untrim netCDF Randnummern momentan nur von ',min_nr,' bis ',maxrandnr
         do n = 1,ianz_rb !! alle Randbedingungen
            if (rabe(n)%nr_rb < min_nr)  &
                print*,'ereigg_Randbedingungen_lesen Untrim: ',n,'-ter Rand mit nr = ',rabe(n)%nr_rb,' unbenutzt'
            if (rabe(n)%nr_rb > maxrandnr)  &
                print*,'ereigg_Randbedingungen_lesen Untrim: ',n,'-ter Rand mit nr = ',rabe(n)%nr_rb,' unbenutzt'
         end do !! alle n Randbedingungen
         !! Randnummer in Randzähler umwandeln.
         do j = 1,n_elemente
            if (element_rand(j) > 0) then !! falls Rand
               if (rb_vorkommen(element_rand(j)) == 0) then !! nicht bediente Randnummern ausrangieren.
                  element_rand(j) = 999999
               else
                  nini = -7
                  do n = 1,ianz_rb !! alle Randbedingungen
                     if (element_rand(j) == rabe(n)%nr_rb) then
                        nini = n
                     end if
                  end do !! alle n Randbedingungen
                  element_rand(j) = nini
               end if !! nicht bediente Randnummer
               if (j == kontrollknoten) then
                  print*,"Kontroll-Element #",j," hat Rand-zähler ", element_rand(j)
                  if (element_rand(j) <= ianz_rb) then
                     print*,"Diesem Rand-Element ist keine Randbedingung #",rabe(element_rand(j))%nr_rb," zugeordnet."
                  else
                     print*,"Diesem RandElement ist keine Randbedingung zugeordnet"
                  endif !Randbedingung zugeordnet
               end if ! kontrollknoten
            end if ! Randknoten
         end do ! alle j Elemente
         deallocate (nr_vorhanden)
         deallocate (rb_vorkommen)
      case(3) ! SCHISM
         maxrandnr = 0
         do j = 1,knotenanzahl2D !! max Randnummer ermitteln:
            if (knoten_rand(j) > 0) then !! Randknoten
               if (knoten_rand(j) > maxrandnr)maxrandnr = knoten_rand(j)
            end if ! Randknoten
         end do ! alle j Knoten
         allocate (nr_vorhanden(maxrandnr), stat = alloc_status )
         do ini = 1,maxrandnr !! Randnummernvorkommen initialisieren
            nr_vorhanden(ini) = 0
         end do
         allocate (rb_vorkommen(maxrandnr), stat = alloc_status )
         if (alloc_status /= 0) then
            write(fehler,*)'nr_vorhanden(maxrandnr) konnte nicht allokiert werden ', alloc_status
            call qerror(fehler)
         end if ! alloc_status .ne.0
         do ini = 1,maxrandnr !! Rand-vorkommen initialisieren
            rb_vorkommen(ini) = 0
         end do
         do j = 1,knotenanzahl2D !! vorhandene Randummern durchzählen.
            if (knoten_rand(j) > 0)nr_vorhanden(knoten_rand(j)) = nr_vorhanden(knoten_rand(j)) + 1
         end do ! alle j Knoten
         do j = 1,maxrandnr !! alle Randnummern ausgeben:
            if (nr_vorhanden(j) > 0) then
               print*,'Randnummer ',j,' kommt an ',nr_vorhanden(j),' Knoten vor. -SCHISM'
            else
               print*,'Randnummer ',j,' kommt nie vor. -SCHISM'
            end if !
         end do ! alle j Randnummern
         do j = 1,maxrandnr !! Randbedingung für alle Randnummern vorhanden?
            rb_vorkommen(j) = 0
            do n = 1,ianz_rb !! alle Randbedingungen
               if (j == rabe(n)%nr_rb) then
                  print*,'Randnummer mit dem Zähler',n,' bedient die Randbedingung mit der Nummer  -SCHISM',rabe(n)%nr_rb
                  rb_vorkommen(j) = rb_vorkommen(j) + 1
               end if
            end do !! alle n Randbedingungen
            if (rb_vorkommen(j) < 1)print*,'### Warnung ###: für Randnummer ',j,' wurde keine Randbedingung vorgegeben. -SCHISM'
            if (rb_vorkommen(j) > 1) then
               write(fehler,*)'### Abbruch 284 ### für Randnummer ',j,' wurden ',rb_vorkommen(j)  &
                     , ' Randbedingungen angegeben. -SCHISM'
               call qerror(fehler)
            end if
         end do ! alle j Randnummern
         !! Randnummer in Feld knoten_rand() durch Randzähler ersetzen !!
         do j = 1,knotenanzahl2D
            !if(j.eq.kontrollknoten)print*,"Kontrollknoten #",j
            if (knoten_rand(j) > 0) then !! falls Randknoten
               if (rb_vorkommen(knoten_rand(j)) == 0) then !! nicht bediente Randnummern ausrangieren.
                  knoten_rand(j) = 999999
               else
                  nini = -7
                  do n = 1,ianz_rb !! alle Randbedingungen
                     if (knoten_rand(j) == rabe(n)%nr_rb) then
                        nini = n
                     end if
                  end do !! alle n Randbedingungen
                  knoten_rand(j) = nini
               end if !! nicht bediente Randnummer
               if (j == kontrollknoten) then
                  print*,"Kontrollknoten #",j," ist Randknoten mit der Rand-zähler ", knoten_rand(j)
                  if (knoten_rand(j) <= ianz_rb) then
                     print*,"Diesem Randknoten ist keine Randbedingung #",rabe(knoten_rand(j))%nr_rb," zugeordnet."
                  else
                     print*,"Diesem Randknoten ist keine Randbedingung zugeordnet"
                  endif !Randbedingung zugeordnet
               end if ! kontrollknoten
            end if ! Randknoten
         end do ! alle j Knoten
         deallocate (nr_vorhanden)
         deallocate (rb_vorkommen)
         case default
         call qerror('ereigg_Randbedingungen_lesen: Hydraulischer Antrieb unbekannt')
   end select
   close (ion)
   print*,"ereigg_Randbedingungen_lesen Ende"
end subroutine ereigg_Randbedingungen_lesen
!----+-----+----
!
!> <h1>Datei e_extnct.dat Lesen</h1>
!! aus <a href="./exp/e_extnct.dat" target="_blank">e_extnct.dat</a>
!! siehe \ref extnct_rb aus \ref Datenmodell
!! \n\n Quelle: randbedingungen.f95 , zurück: \ref zuflussranddaten
!!
subroutine extnct_lesen()
   use modell
   use QSimDatenfelder
   use aparam
   implicit none
   character(500) dateiname, text
   integer :: io_error,i, alloc_status, ini
   if (meinrang /= 0) then ! prozess 0 only
      write(fehler,*)' 724 extnct_lesen darf nur von Prozess 0 aufgerufen werden'
      call qerror(fehler)
   endif
   write(cpfad,'(A)')trim(modellverzeichnis)
   call e_extnct_lesen(ilamda,eta,aw,ack,acg,acb,ah,as,al,cpfad)
   rb_extnct_ilamda = ilamda
   allocate (rb_extnct_p(rb_extnct_ilamda*anz_extnct_koeff), stat = alloc_status )
   if (alloc_status /= 0) then
      write(fehler,*)'103 rb_extnct_p auf prozessor 0 konnte nicht allokiert werden ', alloc_status
      call qerror(fehler)
   end if ! alloc_status .ne.0
   do ini = 1,rb_extnct_ilamda*anz_extnct_koeff
      rb_extnct_p(ini) = 0.0
   end do
   do i = 1,ilamda
      rb_extnct_p(1 + (i-1)*anz_extnct_koeff) = eta(i)
      rb_extnct_p(2 + (i-1)*anz_extnct_koeff) = aw(i)
      rb_extnct_p(3 + (i-1)*anz_extnct_koeff) = ack(i)
      rb_extnct_p(4 + (i-1)*anz_extnct_koeff) = acg(i)
      rb_extnct_p(5 + (i-1)*anz_extnct_koeff) = acb(i)
      rb_extnct_p(6 + (i-1)*anz_extnct_koeff) = ah(i)
      rb_extnct_p(7 + (i-1)*anz_extnct_koeff) = as(i)
      rb_extnct_p(8 + (i-1)*anz_extnct_koeff) = al(i)
      if (i == 1)print*,'e_extnct.dat: Wellenlänge eta(1) = ',eta(1),' Nano-Meter'
      if (i == ilamda)print*,'e_extnct.dat: Wellenlänge eta(',ilamda,') = ',eta(ilamda),' Nano-Meter'
   enddo
   return
end subroutine extnct_lesen
!----+-----+----
!
!> <h1>Felder allocieren für die hydraulischen Randbedingungen</h1>
!!
!! \n\n aus randbedingungen.f95 , zurück: \ref zuflussranddaten
subroutine alloc_hydraul_BC(nk)
   use modell
   implicit none
   integer :: nk, as, ini
   !allocate (rb_hydraul(nk*number_rb_hydraul), stat = as )
   allocate (rb_hydraul(part*proz_anz*number_rb_hydraul), stat = as )
   if (as /= 0) then
      write(fehler,*)' return value rb_hydraul(nk*   :', as
      call qerror(fehler)
   end if
   do ini = 1,nk*number_rb_hydraul
      rb_hydraul(ini) = 0.0
   end do
   return
end subroutine alloc_hydraul_BC
!----+-----+----
!
!> <h1>Volumenstrom und Tracerflüsse entlang aller ianz_rb Ränder ermitteln</h1>
!! ruft subroutine flux() aus schnitt.f95 auf
!! \n\n aus randbedingungen.f95 , zurück: \ref zuflussranddaten
!!
subroutine rand_flux(zeitzaehler)
   use modell
   implicit none
   integer :: zeitzaehler, n, i, k
   integer :: nbot, ntop, fall
   real :: deltax, d1, d2, deltad, x_kreuz, u1, u2, v1x,v1y,v2x,v2y, vox2, volst2
   real :: lang, flaeche, vol_strom, pot_ener_flux, kin_ener_flux
   real :: la,flae,vox,pox,kix
   real :: c1, c2, masx, massen_flux
   ! real , allocatable , dimension (:) :: c1, c2, masx, massen_flux
   ! allocate(c1(n_pl))
   ! allocate(c2(n_pl))
   ! allocate(masx(n_pl))
   ! allocate(massen_flux(n_pl))
   do n = 1,ianz_rb !! alle Randbedingungen
      lang = 0.0
      flaeche = 0.0
      vol_strom = 0.0
      volst2 = 0.0
      pot_ener_flux = 0.0
      kin_ener_flux = 0.0
      massen_flux = 0.0
      !print*,"rand_flux: Rand #",n ," hat ",rabe(n)%randlinie%anzkanten," Kanten."
      do i = 1, rabe(n)%randlinie%anzkanten ! alle i Kanten aus randlinie n
         nbot = rabe(n)%randlinie%kante(i)%bottom
         if (nbot <= 0) then
            write(fehler,*)nbot,' = nbot = rabe(',n,')%randlinie%kante(',i,')%bottom <= 0 '
            call qerror(fehler)
         endif
         ntop = rabe(n)%randlinie%kante(i)%top
         deltax = ( (rabe(n)%randlinie%kante(i)%normal_x**2) + (rabe(n)%randlinie%kante(i)%normal_y**2) )**0.5
         !# tief(j)=p(j)-knoten_z(j) ! Wassertiefe
         !# if(tief(j).le. min_tief )then ! min_tief parameter aus module_modell
         !#    tief(j)= min_tief
         d1 = p(nbot)-knoten_z(nbot)
         d2 = p(ntop)-knoten_z(ntop)
         v1x = u(nbot)*cos(dir(nbot))
         v1y = u(nbot)*sin(dir(nbot))
         v2x = u(ntop)*cos(dir(ntop))
         v2y = u(ntop)*sin(dir(ntop))
         u1 = rabe(n)%randlinie%kante(i)%normal_x*v1x   &
              +rabe(n)%randlinie%kante(i)%normal_y*v1y
         u2 = rabe(n)%randlinie%kante(i)%normal_x*v2x   &
              +rabe(n)%randlinie%kante(i)%normal_y*v2y
         c1 = planktonic_variable(71+(nbot-1)*number_plankt_vari) !! passiver alters-tracer
         c2 = planktonic_variable(71+(ntop-1)*number_plankt_vari)
         ! print*,'rand_flux: kante', i,' nbot,ntop ', nbot,ntop
         ! do k=1,n_pl ! ausgabe-plankt.vari. in Ganglinie speichern
         !    print*,"rand_flux:c1(",k,")=",c1(k)," c2=",c2(k)," massen_flux=",massen_flux(k)," masx=", masx(k)
         ! end do! alle k Ausgabe-Variablen
         call flux_casu(deltax,d1,d2,u1,u2,c1,c2,p(nbot),p(ntop),u(nbot),u(ntop),la,flae,vox,pox,kix,masx)
         vox2 = 0.5*(u1*d1 + u2*d2) !! Volumentromermittlung stückweise zu Testzwecken
         !       print*,"Rand #",n," Kante #",i," pox=",pox
         !&            ," deltax=",deltax," la=",la," vox=",vox," vox2=",vox2  &
         !&            ," bottom #",nbot,"Tiefe1=",d1," u1, v1x, v1y=",u1/deltax, v1x, v1y &
         !&            ," top #",ntop," Tiefe2=",d2," u2, v2x, v2y=",u2/deltax, v2x, v2y
         lang = lang + la
         flaeche = flaeche + flae
         vol_strom = vol_strom + vox
         pot_ener_flux = pot_ener_flux + pox
         kin_ener_flux = kin_ener_flux + kix
         volst2 = volst2+vox2
         massen_flux = massen_flux + masx !!
      end do ! alle i Kanten
      print*,"rand_flux: Rand #",n,' mit ',rabe(n)%randlinie%anzkanten ,' Kanten'&
      ," pot_ener_flux(MW) = ",pot_ener_flux/1000000," kin_ener_flux(MW) = ",kin_ener_flux/1000000
      !&         ," vol_strom(Integral)=",vol_strom," mittlere Fließgeschwindigkeit=",vol_strom/flaeche  &
      !&         ," vol_strom(stückweise)=",volst2," Vm daraus=",volst2/flaeche
      ! Flux-Felder randflux_gang(Randzähler,Zeitpunkt,??) :
      randflux_gang(n,zeitzaehler,1) = lang!!
      randflux_gang(n,zeitzaehler,2) = flaeche !!
      randflux_gang(n,zeitzaehler,3) = vol_strom !!
      randflux_gang(n,zeitzaehler,4) = pot_ener_flux/1000000 !! in mega-Watt
      randflux_gang(n,zeitzaehler,5) = kin_ener_flux/1000000 !! in mega-Watt
      randflux_gang(n,zeitzaehler,6) = massen_flux !!
   end do ! alle n Randbedingungen
   ! deallocate(c1)
   ! deallocate(c2)
   ! deallocate(masx)
   ! deallocate(massen_flux)
   return
end subroutine rand_flux
! Flux-Felder sollen mal: 1=Volumenrstrom, 2=potentieller und 3=kinetischer Energiefluss; n_pl=Anzahl der auszugebenden planktischen Variablen
!----+-----+----
!
!> <h1>Randknoten zu einer Randlinie zusammenstellen</h1>
!! , damit Durchfluss-Integrationen von rand_flux() ausgeführt werden können. \n
!! läuft nur auf Prozess 0
!! \n\n aus randbedingungen.f95 , zurück: \ref zuflussranddaten
subroutine randlinie_zusammenstellen()
   use modell
   implicit none
   integer :: j,k, n, anzranz, nexi, anzel, alloc_status , dreidrin, vierdrin,nzwi
   real :: kx,ky,nx,ny,einwaerts,lang,kantlang
   logical :: top
   if (meinrang /= 0) then
      write(fehler,*)'randlinie_zusammenstellen() darf nur auf Prozess 0'
      call qerror(fehler)
   else
      print*,"randlinie_zusammenstellen() auf 0"
   end if ! alloc_status .ne.0
   ! print*,"randlinie_zusammenstellen zählt mal durch"
   do n = 1,ianz_rb !! alle Randbedingungen
      anzranz = 0
      lang = 0.0
      do j = 1,knotenanzahl2D ! alle j Knoten
         if (knoten_rand(j) == n ) then
            anzranz = anzranz+1
         end if ! Randknoten
      end do ! alle j Knoten
      print*,"randlinie_zusammenstellen: Der ",n,"-te Rand mit Nummer = ",rabe(n)%nr_rb," hat ",anzranz," Knoten"
      rabe(n)%randlinie%anzkanten = anzranz-1
      allocate (rabe(n)%randlinie%kante(rabe(n)%randlinie%anzkanten+1), stat = alloc_status )
      if (alloc_status /= 0) then
         write(fehler,*)'allocate (rabe(n)%randlinie%kante fehlgeschlagen'
         call qerror(fehler)
      end if ! alloc_status .ne.0
      anzel = 0
      do j = 1,n_elemente ! alle j Elemente
         nexi = 0
         dreidrin = 0
         vierdrin = 0
         do k = 1,cornernumber(j) ! 3 oder 4
            if ( knoten_rand(elementnodes(j,k)) == n) nexi = nexi+1
         end do ! alle k knoten im Element j
         if (nexi == 2) then ! Normalfall, Element hat eine Kante auf dem Rand
            anzel = anzel+1
            rabe(n)%randlinie%kante(anzel)%element = j
            top = .true.
            do k = 1,cornernumber(j) ! alle 3 oder 4 Knoten im Element j
               if ( knoten_rand(elementnodes(j,k)) == n) then ! Randknoten
                  if ( .not. top) rabe(n)%randlinie%kante(anzel)%bottom = elementnodes(j,k)
                  if (top) then
                     rabe(n)%randlinie%kante(anzel)%top = elementnodes(j,k)
                     top = .false.
                  end if ! top
               else ! nicht Randknoten
                  if (dreidrin > 0)vierdrin = elementnodes(j,k)
                  if (vierdrin == 0)dreidrin = elementnodes(j,k)
               end if ! Randknoten
            end do ! alle k Knoten im Element j (2-Knoten-Randelement)
            kx = knoten_x(rabe(n)%randlinie%kante(anzel)%top)-knoten_x(rabe(n)%randlinie%kante(anzel)%bottom)
            ky = knoten_y(rabe(n)%randlinie%kante(anzel)%top)-knoten_y(rabe(n)%randlinie%kante(anzel)%bottom)
            nx = -1*ky
            ny = kx
            kx = knoten_x(dreidrin)-knoten_x(rabe(n)%randlinie%kante(anzel)%bottom)
            ky = knoten_y(dreidrin)-knoten_y(rabe(n)%randlinie%kante(anzel)%bottom)
            einwaerts = kx*nx + ky*ny
            if (einwaerts >= 0.0) then ! Normalenvektor auswärts drehen + top-bottom tauschen
               rabe(n)%randlinie%kante(anzel)%normal_x = -1*nx
               rabe(n)%randlinie%kante(anzel)%normal_y = -1*ny
               nzwi = rabe(n)%randlinie%kante(anzel)%bottom
               rabe(n)%randlinie%kante(anzel)%bottom = rabe(n)%randlinie%kante(anzel)%top
               rabe(n)%randlinie%kante(anzel)%top = nzwi
            else ! Normalenvektor übernehmen
               rabe(n)%randlinie%kante(anzel)%normal_x = nx
               rabe(n)%randlinie%kante(anzel)%normal_y = ny
            end if ! einwärts
            kantlang = ( (rabe(n)%randlinie%kante(anzel)%normal_x**2) + (rabe(n)%randlinie%kante(anzel)%normal_y**2) )**0.5
            lang = lang+kantlang
         end if ! Normalfall
         if (nexi >= 3) then ! unerwünschter Sonderfall
            ! anzel=anzel+2 ! unerwünschter Sonderfall
            write(fehler,*),"Am Rand mit Nummer = ",rabe(n)%nr_rb," hat das Element #",j                            &
                           ,"hat mehr als eine Randkante, dies ist unerwünscht und wird zur Zeit nicht behandelt."  &
                           ," ACHTUNG dadurch sind an diesem Rand die Integrale unvollständig."
            print*,trim(fehler)
            !call qerror(fehler)
         end if ! unerwünscht
         !if(nexi.eq.4)anzel=anzel+3 ! unerwünschter Sonderfall: 3 Kanten eines Vierecks sind Rand
         !if(nexi.gt.1)then !! Element j hat Kante an Rand n
         !   print*,"Element #",j," hat ",nexi," Knoten am Rand ",n," Randkantenlänge=",kantlang
         !   print*,"top ", rabe(n)%randlinie%kante(anzel)%top, knoten_x(rabe(n)%randlinie%kante(anzel)%top)  &
         ! &                      , knoten_y(rabe(n)%randlinie%kante(anzel)%top)
         !   print*,"bottom ", rabe(n)%randlinie%kante(anzel)%bottom, knoten_x(rabe(n)%randlinie%kante(anzel)%bottom)  &
         !&                         , knoten_y(rabe(n)%randlinie%kante(anzel)%bottom)
         !end if ! Randkante
      end do ! alle j Elemente
      print*,"randlinie_zusammenstellen: Der ",n,"-te Rand mit Nummer = ",rabe(n)%nr_rb," hat ",anzel  &
                                                                        ," Kanten und ist ",lang," m lang"
      if (anzel == rabe(n)%randlinie%anzkanten) then
         print*,"Rand mit Nummer = ",rabe(n)%nr_rb,"regulär"
      else
         if (anzel == rabe(n)%randlinie%anzkanten+1) then
            print*,"Rand mit Nummer = ",rabe(n)%nr_rb,"geschlossen oder defekt ???"
            rabe(n)%randlinie%anzkanten = anzel
         else
            write(*,*),"Am ",n,"-ten Rand mit Nummer = ",rabe(n)%nr_rb," passen Knotenanzahl = "  &
                                                       ,rabe(n)%randlinie%anzkanten+1 ," und Kantenanzahl",anzel," nicht zusammen"
            !call qerror(fehler)
         endif
      endif
   end do ! alle n Ränder
   ! Das Allocieren der Flux-Felder geschieht erst in ganglinien_parallel()
   ! weil dort erst die Anzahl der Konzentrationen für die Massenflüsse zusammengezählt wird.
   return
end subroutine randlinie_zusammenstellen
!----+-----+----
!aus qsim.f90_v13.10 orgc:
!!....Berechnung der "BSB-Komponenten" am oberen Rand
!!     (auch bei Stundenwert-Generierung)!!!!!
!      bk1 = 0.51
!      bk2 = 0.02
!      vcb = obsbs(mstr,mRB)/ocsbs(mstr,mRB)
!      antBAC = 0.0462*vcb
!      CMs(mstr,mRB) = 0.03
!!...Berechnung des BTOC
!      BTOC5s = obsbs(mstr,mRB)/4.
!      BTOCs = BTOC5s*0.782*vcb**(-0.921)
!!....Berechnung des Anteils an gelsten org. C-Verbindungen
!      alphaD = 0.21*log(vcb)+0.6294
!      if(alphaD.gt.1.)alphaD = 0.95
!      if(alphaD.lt.0.0)alphaD = 0.0
!      CDges = BTOCs*alphaD-CMs(mstr,mRB)
!      if(CDges.lt.0.00001)CDges = 0.00001
!!.......Aufteilung der gelsten Fraktion in leicht und schwer abbaubar
!      alphlD = 0.218*log(vcb)+0.717
!      if(alphlD.lt.0.0)alphlD = 0.0
!      CD1s(mstr,mRB) = CDges*alphlD
!      CD2s(mstr,mRB) = CDges*(1.-alphlD)
!!.......Aufteilung der part. Fraktion in leicht und schwer abbaubar
!      CPges = BTOCs*(1.-alphaD)
!      BACs(mstr,mRB) = CPges*antBAC
!      CPges = CPges-BACs(mstr,mRB)
!      alphlP = 0.4258*log(vcb)+1.114
!      if(alphlP.lt.0.0)alphlP = 0.0
!      CP1s(mstr,mRB) = CPges*alphlP
!      CP2s(mstr,mRB) = CPges*(1.-alphlP)
!!....Verringerung der einzelnen Fraktionen aufrund von HNF
!      CD1s(mstr,mRB) = CD1s(mstr,mRB)-0.2*CHNFs(mstr,mRB)
!      CD2s(mstr,mRB) = CD2s(mstr,mRB)-0.2*CHNFs(mstr,mRB)
!      CP1s(mstr,mRB) = CP1s(mstr,mRB)-0.2*CHNFs(mstr,mRB)
!      CP2s(mstr,mRB) = CP2s(mstr,mRB)-0.2*CHNFs(mstr,mRB)
!      if(CD1s(mstr,mRB).le.0.0)CD1s(mstr,mRB) = 0.000001
!      if(CD2s(mstr,mRB).le.0.0)CD2s(mstr,mRB) = 0.000001
!      if(CP1s(mstr,mRB).le.0.0)CP1s(mstr,mRB) = 0.000001
!      if(CP2s(mstr,mRB).le.0.0)CP2s(mstr,mRB) = 0.000001
!!....Berechnung der Komponenten fuer BSB5
!      bx1 = alphaD*alphlD+(1.-antBAC)*(1-alphaD)*alphlP                 &
!     &+antBAC*(1.-alphaD)*0.4
!      if(bx1.gt.1.)bx1 = 1.
!      bx2 = (1.-bx1)
!      hcon = 1.-(bx1*exp(-bk1*5.)+bx2*exp(-bk2*5.))
!      BL0s = obsbs(mstr,mRB)/hcon
!      O2BSBs(mstr,mRB) = BL0s/BTOCs
!      bl01s(mstr,mRB) = bl0s*bx1
!      bl02s(mstr,mRB) = bl0s*bx2
