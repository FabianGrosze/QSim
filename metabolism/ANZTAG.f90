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
