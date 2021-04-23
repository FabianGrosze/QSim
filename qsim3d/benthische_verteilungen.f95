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
!---------------------------------------------------------------------------------------

!> \page benthische_verteilungen benthische Verteilungen
!! In den Datenfeldern modell::benthic_distribution und  modell::benthic_distribution_p werden alle Variablen gespeichert, die 
!! Eigenschaften der Sohle oder des Sediments beschreiben. 
!! Allgemeiner gesprochen handelt es sich um Verteilungen, die in jeder Vertikalen nur einmal auftreten und die nicht
!! vom fließenden Wasser transportiert werden; so dass hier auch Eigenschaften der Gewässeroberfläche gespeichert werden können.\n
!! 
!! <h2> benthische Verteilungen </h2> 
!! Die QSim-3D Nummer bezieht sich auf die Datenfelder modell::benthic_distribution und  modell::benthic_distribution_p\n 
!! Die QSim-1D Namen werden in QSim3d im module_QSimDatenfelder.f95 vereinbart.\n 
!!<table benthic_distribution>
!!<tr><th> Nr. 	</th><th> Name 				</th><th> Beschreibung 							</th><th> Dimension	</th><th> Wertebereich</th></tr>
!!<tr><td> 1 	</td><td> \anchor tsed tsed 		</td><td> Temperatur des Sediments					</td><td> °c		</td></tr>
!!<tr><td> 2 	</td><td> \anchor sised sised		</td><td> Siliziumgehalt im Sediment					</td><td> mg/m² ??	</td></tr>
!!<tr><td> 3 	</td><td> \anchor pfl pfl		</td><td> Pflanzentrockengewicht					</td><td> g/m² 		</td></tr>
!!<tr><td> 4 	</td><td> \anchor ssdr ssdr		</td><td> Schwebstoffaufnahme durch Dreissena 				</td><td> mg/l je Zeitschritt	</td></tr>
!!<tr><td> 5 	</td><td> \anchor ks ks			</td><td> ## ausser Betrieb ## jetzt in: zone(:)%reib ##  Sandrauheit nach Nikuradse, hydraulischer Reibungsbeiwert	</td><td> m		</td></tr>
!!<tr><td> 6 	</td><td> \anchor orgcsd orgcsd		</td><td> Gesamtmase Kohlenstoff, die je Zeitschritt sedimentiert 	</td><td>  mgC/l je Zeitschritt </td></tr>
!!<tr><td> 7 	</td><td> \anchor bsbbet bsbbet		</td><td> Sauerstoffverbrauch durch Organismen auf Makrophyten	  ??? (wohl unbenutzt) 	</td><td> Ausgabekonzentration		</td></tr>
!!<tr><td> 8	</td><td> \anchor hjo2 hjo2		</td><td> Sauerstofffluss ins Sediment					</td><td>  g O2/(m²*d)	</td></tr>
!!<tr><td> 9	</td><td> \anchor cmatki cmatki		</td><td> Abspülung benthischer kiesel-Algen ??				</td><td> 	</td></tr>
!!<tr><td>10	</td><td> \anchor cmatgr cmatgr		</td><td> Abspülung benthischer gruen-Algen ??				</td><td> 	</td></tr>
!!<tr><td>11	</td><td> \anchor alberg alberg		</td><td> Respiration benthischer gruen-Algen 				</td><td>  mgBio/l je Zeitschritt	</td></tr>
!!<tr><td>12	</td><td> \anchor alberk alberk		</td><td> Respiration benthischer kiesel-Algen 				</td><td>  mgBio/l je Zeitschritt	</td></tr>
!!<tr><td>13	</td><td> \anchor albewg albewg		</td><td> Wachstum benthischer gruen-Algen 				</td><td>  mgBio/l je Zeitschritt	</td></tr>
!!<tr><td>14	</td><td> \anchor albewk albewk		</td><td> Wachstum benthischer kiesel-Algen 				</td><td>  mgBio/l je Zeitschritt	</td></tr>
!!<tr><td>15	</td><td> \anchor resdr resdr		</td><td> Respirationsrate benthischer Filtrierer (Dreissena-Muscheln) 	</td><td> mgBio/l je Zeitschritt		</td></tr>
!!<tr><td>16 	</td><td> \anchor hschlr hschlr		</td><td> (Ausgabe) Sauerstoffverlust im Wasser infolge Sauerstofffluss ins Sediment </td><td> mgO/(l*h) </td></tr>
!!<tr><td>17 	</td><td> \anchor so2ein so2ein		</td><td> (Ausgabe) max. möglicher Sauerstoffgewinn im Wasser aus der Oberflächenbelüftung </td><td> mgO/l je Zeitschritt </td></tr>
!!<tr><td>18 	</td><td> \anchor do2o2d do2o2d		</td><td> Aufnahmerate Sauerstoff aus Oberflächenbelüftung		</td><td> mgO/(l*d) </td></tr>
!!<tr><td>19 	</td><td> \anchor o2ein1 o2ein1		</td><td> (Ausgabe) tatsächlicher Sauerstoffgewinn im Wasser aus der Oberflächenbelüftung </td><td>  mgO/l  je Zeitschritt </td></tr>
!!<tr><td>20	</td><td> \anchor abeowg abeowg		</td><td> Sauerstoffproduktion (Wachstum) benthischer Grünalgen 	</td><td> mgO/l je Zeitschritt 	</td></tr>
!!<tr><td>21 	</td><td> \anchor abeorg abeorg		</td><td> Sauerstoffverbrauch (Respiration) benthischer Grünalgen 	</td><td> mgO/l je Zeitschritt	</td></tr>
!!<tr><td>22	</td><td> \anchor abeowk abeowk		</td><td> Sauerstoffproduktion (Wachstum)  benthischer Kieselalgen 	</td><td> mgO/l je Zeitschritt 	</td></tr>
!!<tr><td>23 	</td><td> \anchor abeork abeork		</td><td> Sauerstoffverbrauch (Respiration) benthischer Kieselalgen 	</td><td> mgO/l je Zeitschritt	</td></tr>
!!<tr><td>24 	</td><td> \anchor ro2dr ro2dr		</td><td> Respiration Dreissena-Muscheln				</td><td> mgO/l je Zeitschritt 	</td></tr>
!!<tr><td>25 	</td><td> \anchor siruek siruek		</td><td> Rückgelöste Menge Silikat-Silizium				</td><td> 	</td></tr>
!!<tr><td>26 	</td><td> \anchor sedalk sedalk		</td><td> Sedimentierte Menge an Kiesel-Algen 				</td><td> mgBio/l je Zeitschritt	</td></tr>
!!<tr><td>27 	</td><td> \anchor sedalg sedalg		</td><td> Sedimentierte Menge an Grün-Algen 				</td><td> mgBio/l je Zeitschritt	</td></tr>
!!<tr><td>28 	</td><td> \anchor sedalb sedalb		</td><td> Sedimentierte Menge an Blau-Algen 				</td><td> mgBio/l je Zeitschritt	</td></tr>
!!<tr><td>29 	</td><td> \anchor exdrvk exdrvk		</td><td> exkretierte Biomasse der Muscheln beim Verzehr von Kiesel-Algen </td><td> mgBio/l je Zeitschritt 	</td></tr>
!!<tr><td>30 	</td><td> \anchor exdrvg exdrvg		</td><td> exkretierte Biomasse der Muscheln beim Verzehr von Grün-Algen </td><td>  mgBio/l je Zeitschritt	</td></tr>
!!<tr><td>31 	</td><td> \anchor exdrvb exdrvb		</td><td> exkretierte Biomasse der Muscheln beim Verzehr von Blau-Algen </td><td> mgBio/l je Zeitschritt	</td></tr>
!!<tr><td>32	</td><td> \anchor hjpo4 hjpo4		</td><td> Phosphat-Freisetzung aus dem Sediment 			</td><td> gP/(m²*d)	</td></tr>
!!<tr><td>33	</td><td> \anchor sedx0 sedx0		</td><td> sedimentierte Nitrosomonasbiomasse, nur Ausgabewert 		</td><td>  in µg/l	</td></tr>
!!<tr><td>34 	</td><td> \anchor bettn bettn		</td><td> OXYDIERTE STICKSTOFFMENGE AM GEWAESSERBETT 			</td><td> 	</td></tr>
!!<tr><td>35	</td><td> \anchor hjno3 hjno3		</td><td> Nitrat-Freisetzung aus dem Sediment 				</td><td>  gN/(m²*d) 	</td></tr>
!!<tr><td>36	</td><td> \anchor hjnh4 hjnh4		</td><td> Ammonium-Freisetzung aus dem Sediment 			</td><td>  gN/(m²*d)	</td></tr>
!!<tr><td>37 	</td><td> \anchor hflun3 hflun3		</td><td> Ausgabe NitratFlux Wasser/Sediment 				</td><td>  in mgN/(l*h)	</td></tr>
!!<tr><td>38 	</td><td> \anchor algdrk algdrk		</td><td> \ref Algen-Konsum-bentisch (Muscheln) Fraßrate Dreissena	</td><td>  in mg/l	 	</td></tr>
!!<tr><td>39 	</td><td> \anchor algcok algcok		</td><td> Kiesel-Algen Konsum durch Corophium ? 			</td><td> 	</td></tr>
!!<tr><td>40 	</td><td> \anchor algdrg algdrg		</td><td> grün-Algen-Konsum-bentisch (Muscheln)				</td><td>  in mg/l 	</td></tr>
!!<tr><td>41	</td><td> \anchor algdrb algdrb		</td><td> blau-Algen-Konsum-bentisch (Muscheln)				</td><td>  in mg/l 	</td></tr>
!!<tr><td>42 	</td><td> \anchor algcog algcog		</td><td> grün-Algen Konsum durch Corophium ? 				</td><td> 	</td></tr>
!!<tr><td>43 	</td><td> \anchor algcob algcob		</td><td> blau-Algen Konsum durch Corophium ? 				</td><td> 	</td></tr>
!!<tr><td>44 	</td><td> \anchor kst kst 		</td><td> ## ausser Betrieb ## Strickler-Beiwert aus Nikuradse Sandrauheit umgerechnet	</td><td> (m**(1/3))/s	</td></tr>
!!<tr><td>45 	</td><td> \anchor utau utau 		</td><td> Sohlschubspannung 						</td><td> N/m²	</td></tr>
!!<tr><td>46 	</td><td> \anchor hjsi hjsi 		</td><td> Silizium-Flux aus dem Sediment 				</td><td> gSi/(m²*d)	</td></tr>
!!<tr><td>47 	</td><td> \anchor hjn2 hjn2 		</td><td> N2 Flux vom Sediment in den Wasserkörper			</td><td> gN/(m²*d)	</td></tr>
!!<tr><td>48	</td><td> \anchor jdoc1 jdoc1 		</td><td> Flux gelöster org. Kohlenstoffe aus dem Sediment, leicht abbaubar	</td><td> g ??? (m²*d)	</td></tr>
!!<tr><td>49	</td><td> \anchor jdoc2 jdoc2 		</td><td> Flux gelöster org. Kohlenstoffe aus dem Sediment, schwer abbaubar	</td><td> g ??? (m²*d)	</td></tr>
!!<tr><td>50	</td><td> \anchor orgcsd0 orgcsd0	</td><td> teil des? sedimentierten organ. Material 			</td><td>  mgC/l je Zeitschritt ??</td></tr>
!!<tr><td>51	</td><td> \anchor orgcsd_abb orgcsd_abb	</td><td> sedimentiertes biologisch abbaubares organ. Material 		</td><td>  mgC/l je Zeitschritt ??</td></tr>
!!<tr><td>52	</td><td> \anchor sedalg_mq sedalg_mq	</td><td> ?? 		</td><td>  ?? </td></tr>
!!<tr><td>53	</td><td> \anchor sedalk0 sedalk0	</td><td> ?? 		</td><td>  ?? </td></tr>
!!<tr><td>54	</td><td> \anchor coroi coroi		</td><td> Corophium Böschung 		</td><td>  ?? </td></tr>
!!<tr><td>55	</td><td> \anchor corois corois		</td><td> Corophium Sohle		</td><td>  ?? </td></tr>
!!<tr><td>56	</td><td> \anchor zdreis zdreis	</td><td> Dreissenabiomasse pro Fläche Sohle (0. Kohorte) </td><td>  gBio/m² </td></tr>
!!<tr><td>57	</td><td>  zdreis 	</td><td> (1. Kohorte) 		</td><td>   </td></tr>
!!<tr><td>58	</td><td>  zdreis 	</td><td> (2. Kohorte) 		</td><td>   </td></tr>
!!<tr><td>59	</td><td>  zdreis 	</td><td> (3. Kohorte) 		</td><td>   </td></tr>
!!<tr><td>60	</td><td> \anchor zdrei zdrei	</td><td> Dreissenabiomasse pro Fläche Böschung (0. Kohorte) </td><td> gBio/m² </td></tr>
!!<tr><td>61	</td><td>  zdrei 	</td><td> (1. Kohorte) 		</td><td>   </td></tr>
!!<tr><td>62	</td><td>  zdrei 	</td><td> (2. Kohorte) 		</td><td>   </td></tr>
!!<tr><td>63	</td><td>  zdrei 	</td><td> (3. Kohorte) 		</td><td>   </td></tr>
!!<tr><td>64	</td><td> \anchor gewdr gewdr	</td><td> Gewicht einer Dreissena-Muschel (0. Kohorte) </td><td> mg </td></tr>
!!<tr><td>65	</td><td>  gewdr 	</td><td> (1. Kohorte) </td><td>  </td></tr>
!!<tr><td>66	</td><td>  gewdr 	</td><td> (2. Kohorte) </td><td>  </td></tr>
!!<tr><td>67	</td><td>  gewdr 	</td><td> (3. Kohorte) </td><td>  </td></tr>
!!<tr><td>68	</td><td> \anchor dlmax dlmax	</td><td> Dreissena Larven ??		</td><td>  ?? </td></tr>
!!<tr><td>69	</td><td> \anchor dlmaxs dlmaxs	</td><td> Dreissena Larven ??  		</td><td>  ?? </td></tr>
!!<tr><td>70	</td><td> \anchor gwdmax gwdmax	</td><td> Dreissena Larven ??  		</td><td>  ?? </td></tr>
!!<tr><td>71	</td><td> \anchor sgwmue sgwmue	</td><td> Dreissena Larven ??  		</td><td>  ?? </td></tr>
!!<tr><td>72	</td><td> \anchor abegm2 abegm2	</td><td> Biomasse benthischer Grünalgen  	</td><td> gBio/m³ </td></tr>
!!<tr><td>73	</td><td> \anchor abekm2 abekm2 </td><td> Biomasse benthischer Kieselalgen  	</td><td> gBio/m³ </td></tr>
!!</table>\n\n
!! Variablendefinition in module_modell.f95\n
!! aus Datei benthische_verteilungen.f95; zurück: \ref lnk_Datentechnik \n 
!! siehe dazu auch: stofftransport()

!----+-----+----
!> Verteilen der benthischen verteilungen auf die parallelen Prozesse.
!! \n\n
      subroutine benthic_parallel()
         use modell
         implicit none
         integer :: i,j,as

         !print*,meinrang,'benthic_parallel'
         call MPI_Bcast(number_benthic_points,1,MPI_INT,0,mpi_komm_welt,ierr)

         allocate (benthic_distribution_p(number_benth_distr*part), stat = as )
         if(as.ne.0)then
            write(fehler,*)' Rueckgabewert von benthic_distribution_p :', as
            call qerror(fehler)
         end if 
         do i=1,part ! i
            do j=1,number_benth_distr ! initialisierung aller konzentrationen zunächt auf 0.0 minus 1
               benthic_distribution_p(j+(i-1)*number_benth_distr)= 0.0 !!!####!-1.0
            end do
         end do

!if(meinrang.eq.0)then ! prozess 0 only
!         do i=1,number_benthic_points ! all i verticals
!            do j=1,number_benth_distr ! initialise
!               benthic_distribution(j+(i-1)*number_benth_distr)=i*100+j
!            end do
!         end do
!end if ! only prozessor 0

         call scatter_benthic()

         !call mpi_barrier (mpi_komm_welt, ierr)
         return
      END subroutine benthic_parallel

!----+-----+----
!> Verteilen der benthischen verteilungen auf die parallelen Prozesse.
!! \n\n
      subroutine scatter_benthic()
         use modell
         implicit none

         !print*,'scatter_benthic'
         
         call MPI_Scatter(benthic_distribution, part*number_benth_distr, MPI_FLOAT,  &
         benthic_distribution_p, part*number_benth_distr, MPI_FLOAT, 0, mpi_komm_welt, ierr)
         if(ierr.ne.0)then
            write(fehler,*)' 13 MPI_Scatter(benthic_distribution failed :', ierr
            call qerror(fehler)
         end if 

         !call mpi_barrier (mpi_komm_welt, ierr)
         return
      END subroutine scatter_benthic
!----+-----+----
!> wieder zusammensammeln der benthischen verteilungen von den parallelen Prozesse.
!! \n\n
      subroutine gather_benthic()
         use modell
         implicit none
         !print*,'gather_benthic'
         
         call MPI_Gather(benthic_distribution_p, part*number_benth_distr, MPI_FLOAT,  &
         benthic_distribution, part*number_benth_distr, MPI_FLOAT, 0, mpi_komm_welt, ierr)
         if(ierr.ne.0)then
            write(fehler,*)' 13b MPI_Gather(benthic_distribution_p failed :', ierr
            call qerror(fehler)
         end if 
         
         !call mpi_barrier (mpi_komm_welt, ierr)
         return
      END subroutine gather_benthic
!----+-----+----
!> Initialisierung der lokalen Konzentrationen.
!! \n\n
      subroutine ini_benthic0(nk)
         use modell
         implicit none
         integer nk,j,i, as

if(meinrang.eq.0)then ! prozess 0 only
         number_benthic_points=nk

         do j=1,number_benth_distr ! initialise
            write(benth_distr_name(j),'(18x)')
         end do
         benth_distr_name( 1)= "              tsed"
         benth_distr_name( 2)= "             sised"
         benth_distr_name( 3)= "               pfl"
         benth_distr_name( 4)= "              ssdr"
         benth_distr_name( 5)= "            Ks_rau"
         !!! ( 5)= "Nikuradse Sandrauheit in m, Sohlreibungsbeiwert (Rauheit)"
         benth_distr_name( 6)= "            orgCsd" ! Gesamtmasse Kohlenstoff, die je Zeitschritt sedimentiert
         benth_distr_name( 7)= "            bsbbet" ! Ausgabekonzentration Sauerstoffverbrauch durch Organismen auf Makrophyten
         benth_distr_name( 8)= "              hJO2" ! Sauerstoffzehrung des Sediments gO2/m² und Zeitschritt
         benth_distr_name( 9)= "            cmatki" ! Abspülung benthischer kiesel-Algen 
         benth_distr_name(10)= "            cmatgr" ! Abspülung benthischer gruen-Algen
         benth_distr_name(11)= "            alberg" ! Respiration benthischer gruen-Algen
         benth_distr_name(12)= "            alberk" ! Respiration benthischer kiesel-Algen
         benth_distr_name(13)= "            albewg" ! Wachstum benthischer gruen-Algen
         benth_distr_name(14)= "            albewk" ! Wachstum benthischer kiesel-Algen
         benth_distr_name(15)= "             resdr" ! Respirationsrate benthischer Filtrierer (Dreissena-Muscheln)
         benth_distr_name(16)= "            hschlr" ! Sauerstoffzehrung durch das Sediments, Ausgabe in mgO2/(l*h) 
         benth_distr_name(17)= "            so2ein" ! Sättigungs-Sauerstoffeintrag aus der Luft ? , Ausgabe in mgO2/(l*h) 
         benth_distr_name(18)= "            dO2o2D" ! Beiwert Oberflächenbelüftung ? (oxygen) war bbei2D
         benth_distr_name(19)= "            o2ein1" ! Defizit-Sauerstoffeintrag aus der Luft ? , Ausgabe 
         benth_distr_name(20)= "            abeowg" ! Sauerstoffproduktion benthischer Grünalgen
         benth_distr_name(21)= "            abeorg" ! Sauerstoffverbrauch benthischer Grünalgen
         benth_distr_name(22)= "            abeowk" ! Sauerstoffproduktion benthischer Kieselalgen
         benth_distr_name(23)= "            abeork" ! Sauerstoffverbrauch benthischer Kieselalgen
         benth_distr_name(23)= "            abeork" ! Sauerstoffverbrauch benthischer Kieselalgen
         benth_distr_name(24)= "             ro2dr" ! Respiration Dreissena-Muscheln pro Zeitschritt in mgO2/l je Zeitschritt 
         benth_distr_name(25)= "            SiRuek" ! Rückgelöste Menge Silikat-Silizium
         benth_distr_name(26)= "            sedalk" ! Sedimentierte Menge an Kiesel-Algen
         benth_distr_name(27)= "            sedalg" ! Sedimentierte Menge an Grün-Algen
         benth_distr_name(28)= "            sedalb" ! Sedimentierte Menge an Blau-Algen
         benth_distr_name(29)= "            exdrvk" ! exkretierte Ammoniummenge der Muscheln beim Verzehr von Kiesel-Algen   
         benth_distr_name(30)= "            exdrvg" ! exkretierte Ammoniummenge der Muscheln beim Verzehr von Grün-Algen 
         benth_distr_name(31)= "            exdrvb" ! exkretierte Ammoniummenge der Muscheln beim Verzehr von Blau-Algen 
         benth_distr_name(32)= "             hJPO4" ! Phosphat-Freisetzung aus dem Sediment
         benth_distr_name(33)= "             sedx0" ! sedimentierte Nitrosomonasbiomasse in µg/l, nur Ausgabewert
         benth_distr_name(34)= "             bettn" ! OXYDIERTE STICKSTOFFMENGE AM GEWAESSERBETT | AmmoniumFlux Wasser/Sediment in mgN/(l*h)
         benth_distr_name(35)= "             hJNO3" ! Nitrat-Freisetzung aus dem Sediment
         benth_distr_name(36)= "             hJNH4" ! Ammonium-Freisetzung aus dem Sediment
         benth_distr_name(37)= "            hFluN3" ! Ausgabe NitratFlux Wasser/Sediment in mgN/(l*h)
         benth_distr_name(38)= "            algdrk" ! \ref Algen-Konsum-bentisch (Muscheln) in mg/l	
         benth_distr_name(39)= "            algcok" ! Kiesel-Algen Konsum durch Corophium ?
         benth_distr_name(40)= "            algdrg" ! \ref grün-Algen-Konsum-bentisch (Muscheln) in mg/l	
         benth_distr_name(41)= "            algdrb" ! \ref blau-Algen-Konsum-bentisch (Muscheln) in mg/l	
         benth_distr_name(42)= "            algcog" ! grün-Algen Konsum durch Corophium ?
         benth_distr_name(43)= "            algcob" ! blau-Algen Konsum durch Corophium ?
         benth_distr_name(44)= "               Kst" ! Strickler-Beiwert aus Nikuradse Sandrauheit umgerechnet
         benth_distr_name(45)= "              utau" ! Sohlschubspannung
         benth_distr_name(46)= "              hJSi" ! Silizium-Flux aus dem Sediment
         benth_distr_name(47)= "              hJN2" ! 
         benth_distr_name(48)= "             JDOC1" ! flux dissolved organic carbon aus dem sediment
         benth_distr_name(49)= "             JDOC2" ! flux dissolved organic carbon aus dem sediment
         benth_distr_name(50)= "           orgCsd0" ! teil des? sedimentierten organ. Material
         benth_distr_name(51)= "        orgCsd_abb" ! sedimentiertes biologisch abbaubares organ. Material
         benth_distr_name(52)= "         sedAlg_MQ" ! 
         benth_distr_name(53)= "           sedAlk0" ! 
         benth_distr_name(54)= "             coroi" ! Corophium Böschung 
         benth_distr_name(55)= "            corois" ! Corophium Sohle
         benth_distr_name(56)= "           zdreis0" ! Dreissenabiomasse pro Fläche Sohle (0. Kohorte)
         benth_distr_name(57)= "           zdreis1" ! (1. Kohorte)
         benth_distr_name(58)= "           zdreis2" ! (2. Kohorte) 
         benth_distr_name(59)= "           zdreis3" ! (3. Kohorte)
         benth_distr_name(60)= "            zdrei0" ! Dreissenabiomasse pro Fläche Böschung (0. Kohorte)
         benth_distr_name(61)= "            zdrei1" ! (1. Kohorte)
         benth_distr_name(62)= "            zdrei2" ! (2. Kohorte) 
         benth_distr_name(63)= "            zdrei3" ! (3. Kohorte)
         benth_distr_name(64)= "            gewdr0" ! Gewicht einer Dreissena-Muschel (0. Kohorte)
         benth_distr_name(65)= "            gewdr1" ! (1. Kohorte)
         benth_distr_name(66)= "            gewdr2" ! (2. Kohorte)
         benth_distr_name(67)= "            gewdr3" ! (3. Kohorte)
         benth_distr_name(68)= "             dlmax" !  Dreissena Larven ??
         benth_distr_name(69)= "            dlmaxs" !  Dreissena Larven ??
         benth_distr_name(70)= "            gwdmax" !  Dreissena Larven ??
         benth_distr_name(71)= "            sgwmue" !  Dreissena Larven ??
         benth_distr_name(72)= "            abegm2" !  Ausgabewerte albenth()
         benth_distr_name(73)= "            abekm2" !  Ausgabewerte albenth()

         !benth_distr_name(44)= "            uedau0" ! Überstaudauer 0-15
         !benth_distr_name(45)= "           uedau15" ! Überstaudauer 15-25
         !benth_distr_name(46)= "           uedau25" ! Überstaudauer 25-35
         !benth_distr_name(47)= "           uedau35" ! Überstaudauer 35-unendl.
         !benth_distr_name()= "            " ! 

         !allocate (benthic_distribution(number_benth_distr*number_benthic_points), stat = as )
         allocate (benthic_distribution(number_benth_distr*part*proz_anz), stat = as )
         if(as.ne.0)then
            write(fehler,*)' Rueckgabewert   von   allocate benthic_distribution :', as
            call qerror(fehler)
         end if 
         do i=1,number_benthic_points ! all i verticals
            do j=1,number_benth_distr ! initialise
               benthic_distribution(j+(i-1)*number_benth_distr)= 0.0 !!!####!0.0
            end do
         end do

         do j=1,number_benth_distr ! default no output
            output_benth_distr(j)=.false.
         end do

          ! vorbelegen
          !do i=1,number_benthic_points ! 
            !benthic_distribution(1,k)=  4.0 !! Sediment-Temperatur Elbe-Ästuar Jahresanfang 2006
            !benthic_distribution(2,k)=  7.0 !! Siliziumgehalt im Sediment
            !benthic_distribution(5+(i-1)*number_benth_distr)= 70.0 !! Strickler Reibungsbeiwert Kst_rau (Mannings n, here: Kst=1/n)
            !benthic_distribution(11+(i-1)*number_benth_distr)= tief(i) !! water depth
         !end do
         !print*, '### ACHTUNG ### Strickler Reibungsbeiwert Kst_rau wird noch nicht eingelesen sondern hilfsweise 70 gesetzt ????'
 
end if ! only prozessor 0
      END subroutine ini_benthic0
!----+-----+----

