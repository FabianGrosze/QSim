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

      subroutine Randbedingungen(cpfad, i_Rands, iw_max)

      implicit none
      character*255               :: cpfad
      character (len=275)         :: pfadstring 
      integer                     :: read_error,open_error
      integer                     :: mstr,i_hcon,iws_RB,i_Rands,itagl,iw_max,iwe
      integer                     :: RBNR

        write(pfadstring,'(2A)')trim(adjustl(cpfad)),'EREIGG.txt' 
        open(unit=92, file=pfadstring, iostat = open_error)
        if(open_error.ne. 0) then
           print*,'open_error Randbedingungen EREIGG.txt',pfadstring
           stop 3
        end if
        rewind (92) 
                                                                       
        read(92,'(2x)') 
        read(92,'(2x)') 
        read(92,'(2x)') 
        read(92,'(2x)') 
        read(92,'(2x)') 
        read(92,'(2x)') 

        iw_max = 0
        i_rands = 0
                                                                        
        do                 ! Randbedingungsschleife Beginn     
          read(92,9230,iostat=read_error) mstr, RBNR, i_hcon, iws_RB                                             

          if(read_error<0.0)exit
                                                                       
          if(iws_RB==0)cycle

          if(iws_RB>iw_max)iw_max = iws_RB 

          i_Rands = i_Rands + 1                 ! Summenbildung der Randbedingungen  
                                                                       
          do iwe = 1,iws_RB    ! Einlesen der Randbedingungswerte für den Strang <mstr>, hier Schleifenbeginn 
            read(92,9240)itagl            
          enddo                             ! Schleifenende
        enddo                                 ! Randbedingungsschleife Ende 
                                                                       
   9230 format(I5,2x,I5,2x,I1,2x,I5) 
   9240 format(i2) 

      close(92)  
      print*,'subroutine Randbedingungen found ',i_Rands,' boundaries in EREIGG.txt'   
                                                                       
  end subroutine Randbedingungen
