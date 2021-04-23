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

  SUBROUTINE TAGE(ITAGs,MONATs,ANZT) 
                                                                       
!   UNTERPROGRAMM ZUR BERECHNUNG DER VERGANGENEN TAGE                 
!   SEIT DEM 21.MAERZ                                                 
                                                                       
                                                                       
!   AUTOR:VOLKER KIRCHESCH                                            
                                                                       
!   STAND:21.8.84                                                     
                                                                       
                                                                       
    INTEGER :: ANZT 


    IF(MONATs<=2)then
      ANZT = ITAGs+31*(MONATs-1)+285 
        else if(monats==3.and.itags<=21)then
          ANZT = (ITAGs+31*(MONATs-1)-INT(0.4*MONATs+2.3))+285 
            else 
              ANZT = (ITAGs+31*(MONATs-1)-INT(0.4*MONATs+2.3))-80 
    endif 

    RETURN 
  END subroutine tage                                           
