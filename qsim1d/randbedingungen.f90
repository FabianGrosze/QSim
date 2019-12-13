   subroutine Randbedingungen(cpfad, i_Rands, iw_max)


    character*255               :: cpfad
    character (len=275)         :: pfadstring 

    integer                     :: read_error 



        write(pfadstring,'(2A)')trim(adjustl(cpfad)),'EREIGG.txt' 
        open(unit=92, file=pfadstring)
        rewind (92) 
                                                                       
        read(92,'(2x)') 
        read(92,'(2x)') 
        read(92,'(2x)') 
        read(92,'(2x)') 
        read(92,'(2x)') 
        read(92,'(2x)') 

        iw_max = 0
        i_rands = 0
                                                                        
        do i_Rand = 1, 500                 ! Randbedingungsschleife Beginn     
          read(92,9230,iostat=read_error)mstr,RBNR,i_hcon,iws_RB                                             

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
                                                                       
  end subroutine Randbedingungen