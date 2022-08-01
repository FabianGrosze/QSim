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
!> \page coroph Chorophium (Schlickkrebs)
!! \n\n
!! momentan noch in Bearbeitung
!! \n\n
!! <table border="0" ><tr><td  width="70%" ><center>
!! \image html Friedrichskoog_Wattenmeer_Schlickkrebse_August-2010_SL275046_klein.jpg "Schlickkrebse (Corophium volutator)"
!! <a href="Friedrichskoog_Wattenmeer_Schlickkrebse_August-2010_SL275046.jpg">Bild groß</a> \n
!! Schlickkrebse (Corophium volutator) im Wattenmeer vor Friedrichskoog, Schleswig-Holstein \n
!! C. Löser, 1. August 2010, Creative-Commons-Lizenz\n
!! von https://de.wikipedia.org/wiki/Datei:Friedrichskoog_Wattenmeer_Schlickkrebse_August-2010_SL275046.JPG 26aug19\n
!! </center></td><td  width="30%" align="left" valign="top">
!! ein weiterer Konsument ...
!! </td></tr></table>
!!
!! <h2>Herkunft</h2>
!!    EIN PROGRAMM zu Berechnung des Einflusses von Corophium auf das  \n
!!    Phytoplankton \n
!!     AUTOR : VOLKER KIRCHESCH  \n
!!     STAND : 10.10.2011 \n
!!
!! <h2>Teilprozesse</h2>
!! Nachkommen der Jahresgruendungspopulation nco = 1  \n
!! ...
!!
!! <h2>Schnittstellenbeschreibung</h2>
!! <h2>Dokumentation und Veröffentlichungen</h2>
!! <h2>IT-Realisierung</h2>
!! \n\n
!! Quelle coroph_huelle.f95; zurück zu: \ref lnk_ueberblick
!
!> SUBROUTINE coroph_huelle() wird beschrieben in: \ref coroph \n
!! Quelle coroph_huelle.f95
subroutine coroph_huelle(i)
   use modell
   use QSimDatenfelder
   implicit none
   integer :: i,j
   iglob = (i+meinrang*part)
   
   if (kontroll) then
      print*,'coroph vorher: coro,coros = ',coro,coros
   end if 
   
   call coroph(coro,coros,tempw,flae,elen,anze,ior                             &
               ,volfco,aki,agr,algcok,algcog,tflie,bsohlm,lboem,coroI,coroIs   &
               ,abl,algcob,mstr,itags,monats,jahrs,ilang,nbuhn,ilbuhn,azStrs   &
               ,kontroll ,iglob ) !!wy
   return
end subroutine coroph_huelle
