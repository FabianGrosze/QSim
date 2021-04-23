!
  subroutine ergeb2DFormat()

    write(255, '(A)') '*P  01  01    $Tiefe      VNH4      VNO2      VNO3      GELP        SI     TEMPW       VO2      CHLA     CCHLKI     CCHLBL     CCHLGR      gesP      gesN'
    write(255, '(A)') '*F  01  01      F5.2      F6.2      F5.3      F9.6      F5.3      F5.2      F5.2      F5.2      F6.2      F6.2      F6.2      F6.2      F7.3      F7.3'

  end subroutine ergeb2DFormat