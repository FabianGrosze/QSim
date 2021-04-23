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

  SUBROUTINE ANZTAG(MONAT,jahrs,JTAGE) 
                                                                       
                                                                       
                                                                       
!   EIN PROGRAMM ZUR BESTIMMUNG DER TAGESZAHL EINES MONATS            
                                                                       
                                                                       
                                                                       
                                                                       
                                                                       
!   AUTOR : VOLKER KIRCHESCH                                          
                                                                       
                                                                       
!   STAND : 22.09.2010                                                
                                                                       
                                                                       
!   UNTERPROGRAMME : KEINE                                            
                                                                       
                                                                       
                                                                       
!                                                                       
    hcon = (jahrs/4.)-int(jahrs/4.)  ! Schaltjahr 
                                                                       
    IF(MONAT==1)JTAGE = 31 
    IF(MONAT==2)JTAGE = 28 
    if(Monat==2.and.hcon<0.05)JTAGE = 29 
    IF(MONAT==3)JTAGE = 31 
    IF(MONAT==4)JTAGE = 30 
    IF(MONAT==5)JTAGE = 31 
    IF(MONAT==6)JTAGE = 30 
    IF(MONAT==7)JTAGE = 31 
    IF(MONAT==8)JTAGE = 31 
    IF(MONAT==9)JTAGE = 30 
    IF(MONAT==10)JTAGE = 31 
    IF(MONAT==11)JTAGE = 30 
    IF(MONAT==12)JTAGE = 31 
                                                                      
                                                                       
                                                                       
    RETURN 
  END subroutine ANZTAG                                           
