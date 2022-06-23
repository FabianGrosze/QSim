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
subroutine Fehlermeldungen(cpfad,j1)
   character(255)          :: cpfad
   character (len = 275)   :: pfadstring
   integer                 :: open_error
   
   
   write(pfadstring,'(2A)')trim(adjustl(cpfad)),'Fehlermeldungen.txt'
   open(unit = 599, file = pfadstring, iostat = open_error )
   if (open_error /= 0) then
      print*,'open_error Fehlermeldungen.txt'
      stop 1
   end if
   rewind(599)
   
   ! Anlegen der Datei <Fehlermeldungen.txt>
   write(599,1000)
   write(599,1010)
   write(599,1020)
   write(599,1030)
   write(599,1040)
   write(599,1050)
   write(599,1060)
   write(599,1070)
   write(599,1080)
   write(599,1090)
   write(599,1100)
   write(599,1110)
   write(599,1120)
   write(599,1130)
   write(599,1140)
   write(599,1150)
   write(599,1160)
   write(599,1170)
   write(599,1180)
   write(599,1190)
   write(599,1200)
   write(599,1210)
   write(599,1220)
   write(599,1230)
   write(599,1240)
   write(599,1250)
   write(599,1260)
   write(599,1280)
   write(599,1300)
   write(599,1320)
   write(599,1340)
   write(599,1360)
   write(599,1370)
   write(599,1380)
   1000 format('Die Tiefe am Profil ist kleiner oder gleich 0.0 m (Strang Profil):')
   1010 format('maximale Anzahl (1000) der Gitterpunkte wurde ueberschritten!')
   1020 format('die Eingabe der Modellkonstanten fehlt oder ist unvollständig. Eingabe unter QSim-Parameter')
   1030 format('für Wetterstation wurde keine Minimum-Temperaturen eingegeben! (Wetterstation):')
   1040 format('Die Anteile der Kiesel- und Blaualgen sind zusammen größer 1 (Strang):')
   1050 format('Es fehlt die Eingabe der Nitrosomonas-Konzentration an den Rändern!')
   1060 format('Es fehlt die Eingabe der Nitrobacter-Konzentration an den Rändern!')
   1070 format('Es fehlt die Eingabe des Kieselalgenanteils am Chla an den Rändern!')
   1080 format('Es fehlt die Eingabe des Blaualgenanteils am Chla an den Rändern!')
   1090 format('Es fehlt die Eingabe der Silikatwerte an den Rändern!')                   ! 10
   1100 format(' Der Bedeckungsgrad ist groesser als 8/8 oder kleiner (es liegen nur Fehlwerte vor) als 0 ')
   1110 format('Es fehlt die Zuordnung einer Wetterstation zum Strang:')
   1120 format('Die Eingabe Laubwald rechtes und Laubwald linkes Ufer ist so nicht zulässig < Laubwald beidseitig > ; Strang:')
   1130 format('Die Eingabe Nadelwald rechtes und Nadelwald linkes Ufer ist so nicht zulässig < Nadelwald beidseitig > ; Strang:')
   1140 format('Der Vegetationsanteil am linken Ufer liegt über 100% für Strang:')
   1150 format('Der Vegetationsanteil am rechten Ufer liegt über 100% für Strang:')
   1160 format('Die Zeitpunkte fuer das Makrophythenwachstum sind nicht oder unvollstaendig eingegeben für Strang:')
   1170 format('Es erfolgte keine Eingabe für die Austauschzeit zwischen Buhnfeld und Hauptstrom!')
   1180 format('Es wurde am Startprofil kein m-Wert eingegeben!')
   1190 format('Es wurde am Startprofil kein Ca-Wert eingegeben!')                   ! 20
   1200 format('Die Eingabe der geografischen Breite und geografischen Laenge ist unvollständig!')
   1210 format('Division durch 0 im Stickstoffbaustein im Strang:')
   1220 format('Division durch 0 im Sauerstoffbaustein im Strang:')
   1230 format('Division durch 0 im Temperaturbaustein im Strang:')
   1240 format('Division durch 0 im Algenbaustein im Strang:')
   1250 format('Die Eigabe der Berechnungs-Parameter für coliforme Bakterien fehlt oder ist unvollständig')
   1260 format('Die Eigabe des Faktors für BSB5/CSB-Erhöhung durch Algen fehlt oder ist falsch (s. Parameter-Hilfe)')
   1280 format('Es fehlt die Eingabe des C-BSB5 und des CSB an den Rändern (Einer von beiden muss eingegeben werden!)')
   1300 format('Es fehlt die Eingabe der Schwebstoffe an den Rändern')
   1320 format('Es fehlt die Eingabe des pH-Wertes an den Rändern')                   ! 30
   1340 format('Es fehlt die Eingabe des pH-Wertes für den Zufluss (und evt. weitere):')     ! 31
   1360 format('Es fehlt die Eingabe der Schwebstoffe für den Zufluss (und evt. weitere):')! 32
   1370 format('read_error in EREIGG.txt')
   1380 format('iphy wert (Belüftungs-Verfahren) nicht definiert')
   close (599)
end subroutine Fehlermeldungen
