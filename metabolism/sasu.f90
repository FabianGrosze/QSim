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

  SUBROUTINE SASU(itags,monats,geob,geol,sa,su,zg,zlk,dk,tdj,ifehl)  
                                                                       
                                                                       
                                                                       
                                                                       
!   AUTOR : VOLKER KIRCHESCH                                          
                                                                       
                                                                       
!   STAND : 05.11.1987                                                
                                                                       
                                                                       
!   UNTERPROGRAMME : UP.TAGE                                          
                                                                       
                                                                       
                                                                       
    INTEGER :: ANZT, tdj, stunde 
                                                                       
!   BERECHNUNG DER VERGANGENEN TAGE SEIT DEM 21.M{RZ                  
                                                                       
    CALL TAGE(ITAGs,MONATs,ANZT) 
                                                                       
    ifehl = 0                                                                   
    if((geoB<-90.0 .or. geoB>90.0).or.(geoL< -180.0 .or. geoL> 180.0))then
      ifehl = 21
    else
         PI = 22./7. 
         INEG = 0 
         tdj = anzt+80 
         PH = 0.9876*ANZT*PI/180. 
         ZG =-7.683*SIN(PH+1.3788)+9.867*SIN(2.*PH) 
         FK = 1.921*SIN(PH+1.3788)-1.8855 
         PH = (PH*180./PI)+FK 
         DK = 0.39875*SIN(PH*PI/180.) 
         DK = ATAN(DK/SQRT(1-DK**2)) 
         GEOBn = GEOB*PI/180. 
         T0 = (-1.)*TAN(GEOBn)*TAN(DK) 
           IF(T0<0.0)THEN 
             T0=T0*(-1.) 
             INEG = 1 
           ENDIF 

         T0 = ATAN(SQRT(1.-T0**2)/T0) 
         if(ineg>0)then 
           t1 = pi-t0 
           t0 = t1 
         endif 

         D0 = 0.833/(COS(GEOBn)*COS(DK)*SIN(T0)) 
         T0 = ((T0*180./PI)+D0)*4. 
         ZLK = (15.-GEOL)*4. 
         SA = (720.-T0-ZG+ZLK)/60. 
         SU = (720.+T0-ZG+ZLK)/60. 
    endif                                                              

    RETURN 
    END subroutine sasu                                           
