!---------------------------------------------------------------------------------------
!
!   QSim - Programm zur Simulation der Wasserqualit�t
!
!   Copyright (C) 2020 Bundesanstalt f�r Gew�sserkunde, Koblenz, Deutschland, http://www.bafg.de
!
!   Dieses Programm ist freie Software. Sie k�nnen es unter den Bedingungen der 
!   GNU General Public License, Version 3,
!   wie von der Free Software Foundation ver�ffentlicht, weitergeben und/oder modifizieren. 
!   Die Ver�ffentlichung dieses Programms erfolgt in der Hoffnung, da� es Ihnen von Nutzen sein wird, 
!   aber OHNE IRGENDEINE GARANTIE, sogar ohne die implizite Garantie der MARKTREIFE oder der VERWENDBARKEIT F�R EINEN BESTIMMTEN ZWECK. 
!   Details finden Sie in der GNU General Public License.
!   Sie sollten ein Exemplar der GNU General Public License zusammen mit diesem Programm erhalten haben. 
!   Falls nicht, siehe http://www.gnu.org/licenses/.  
!   
!	Programmiert von:
!	1979 bis 2018 Volker Kirchesch
!	seit 2011 Jens Wyrwa, Wyrwa@bafg.de
!
!---------------------------------------------------------------------------------------

subroutine aparamles(cpfad,itags,monats,Jahrs,aggmax,akgmax,abgmax)

                                                                        
!   UNTERPROGRAMM ZUR Interpolation der Randbedingungen                   
                                                                       
                                                                        
!   AUTOR: VOLKER KIRCHESCH                                           
!                                                                       
!   STAND: 03.01.2013                                                 
                                                                       
                                                                       
      character (len = 255)                       :: cpfad 
      character (len=275)                         :: pfadstring 
      
      integer                                     :: anzDatum,  R_NR,  R_NRS, read_error
      integer, Dimension(1000)                    :: itagp, monatp, jahrp 

      real, Dimension(1000,3)                     :: wertp
                                                                       
!      open(unit=292,file='aparamt.tst') 

!.....Einlesen aus aparamt.txt
        close (192) 
        write(pfadstring,'(2A)')trim(adjustl(cpfad)),'aparamt.txt'
        open(unit=192, file=pfadstring)
        rewind (192) 

        if(monats>2)then 
          NRS = (ITAGS+31*(MONATS-1)-INT(0.4*MONATS+2.3)) 
            else 
              NRS = ITAGS+31*(MONATS-1) 
        endif
                                                                       
        NRSJ = (Jahrs-1900)*365+int((Jahrs-1900)/4)            !Tage seit 1900 (Ber�cksichtigung der Schaltjahre
                                                                       
        R_NRS = NRS + NRSJ 

        read(192,'(i4)',iostat=read_error)anzDatum 
        if(read_error<0.0)anzDatum = 0
                                                                        
        if(anzDatum==0)then
          else
            do i_Datum = 1, anzDatum     
              read(192,9245)itagp(i_Datum),monatp(i_Datum),jahrp(i_Datum),(wertp(i_Datum,ip),ip=1,3)

              if(monatp(i_Datum)>2)then 
                NRS = (itagp(i_Datum)+31*(monatp(i_Datum)-1)-INT(0.4*monatp(i_Datum)+2.3)) 
                  else 
                    NRS = itagp(i_Datum)+31*(monatp(i_Datum)-1) 
              endif

              NRSJ = (jahrp(i_Datum) - 1900)*365+int((jahrp(i_Datum)-1900)/4)
              R_NR = NRS + NRSJ 

         write(89,*)itags,monats,jahrs,R__NRS

           if(R_NR<=R_NRS)then
             if(wertp(i_Datum,1)>0.0)aggmax = wertp(i_Datum,1)                                                                                    
             if(wertp(i_Datum,2)>0.0)akgmax = wertp(i_Datum,2)                                                                                    
             if(wertp(i_Datum,3)>0.0)abgmax = wertp(i_Datum,3)                                                                                    
           endif  
           enddo
         endif

  9245 format(i2,2x,i2,2x,i4,2x,f5.2,2x,f5.2,2x,f5.2)

 End subroutine aparamles
