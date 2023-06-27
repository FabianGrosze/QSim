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

!> Creates all files needed to store model results
!!
!! * ErgebM.txt
!! * ErgebT.txt
!! * Ergeb2D.txt
!! * ausgabe156.csv (optional)
!! * ausgabe157_schwermetalle.csv (optional)
!! * ausgabe158_algae.csv (optional)
!! @author Michael Schönung
!! @date 20.06.2022
subroutine init_result_files(cpfad, modell, cEreig, write_csv_files)
   implicit none
   
   ! --- dummy arguments ---
   character(len = 255), intent(in) :: cpfad           !< path to directory for output
   character(len = *),   intent(in) :: modell          !< modelname (Gerris)
   character(len = 255), intent(in) :: cEreig          !< meta data (Gerris)
   logical, intent(in)              :: write_csv_files !< switch to turn of .csv-outputs
   
   ! --- local variables ---
   character(275) :: pfadstring
   character(8)   :: versionstext
   integer        :: open_error, u_file1
   
   external :: version_string, qerror
  
   
   print *, ''
   print *, repeat('=', 78)
   print *, repeat(' ', 33), 'init output'
   print *, repeat('=', 78)
   print *, 'Creating files for results:'
   
   ! get current version number
   call version_string(versionstext)
   
   !--- ErgebM.txt ---
   print*, '   * ErgebM.txt'
   pfadstring = trim(adjustl(cpfad)) // 'ERGEBM.txt'
   open(unit = 45, file = pfadstring, iostat = open_error)
   if (open_error /= 0) call qerror("Could not open ERGEBM.txt")
      
   write(45,'(a,a)')'*V  QSim  ERGEBM  ', versionstext
   call write_header_ergebm()
   write(45,'(a50)')  modell
   write(45,'(a255)') cEreig
   
   ! --- ErgebT.txt ---
   print*, '   * ErgebT.txt'
   pfadstring =  trim(adjustl(cpfad)) // 'ERGEBT.txt'
   open(unit = 155, file = pfadstring, iostat = open_error)
   if (open_error /= 0) call qerror("Could not open ERGEBT.txt")
   
   write(155,'(a,a)') '*V  QSim  ERGEBT  ', versionstext
   call write_header_ergebt()
   write(155,'(a50)')modell
   write(155,'(a255)')cEreig
   
   ! --- Ergeb2D.txt ---
   print*, '   * Ergeb2D.txt'
   pfadstring =  trim(adjustl(cpfad)) // 'ERGEB2D.txt'
   open(unit = 255, file = pfadstring, iostat = open_error)
   close(255)
   
   if (write_csv_files) then 
      ! --- Ausgabe 156 ---
      print*, '> ausgabe156.csv'
      pfadstring =  trim(adjustl(cpfad)) // 'ausgabe156.csv'
      open(unit = 156, file = pfadstring, iostat = open_error)
      write(156,'(a)')'itags ; monats ; jahrs ; uhrhm ; mstr ; Stakm ; STRID ; vbsb ; vcsb ; vnh4 ; vno2 ; vno3 ; gsN ; gelp ;  &
                     & gsP ; Si ; chla ; zooin ; vph ; mw ; ca ; lf ; ssalg ; tempw ; vo2 ; CHNF ; coli ; Dl ; dsedH ; tracer'
      
      ! --- Ausgabe 157 Schwermetalle ---
      print*, '> ausgabe157_schwermetalle.csv'
      pfadstring =  trim(adjustl(cpfad)) // 'ausgabe157_schwermetall.csv'
      open(unit = 157, file = pfadstring, iostat = open_error)
      
      write(157,'(a)')'itags ; monats ; jahrs ; uhrhm ; mstr ; Stakm ; STRID ; gsPb ; glPb ; gsCad ; glCad ; gsCr ; glCr ; gsFe ; &
                     & glFe ; gsCu ; glCu ; gsMn ; glMn ; gsNi ; glNi ; gsHg ; glHg ; gsU ; glU ; gsZn ; glZn ; gsAs ; glAs ;     &
                     & SSeros; sedalk; sedalg; sedalb; sedss'
      
      ! --- Ausagbe 158 Algae ---
      print*, '> ausgabe158_algae.csv'
      pfadstring = trim(adjustl(cpfad)) // 'ausgabe158_algae.csv'
      open(unit = 158, file = pfadstring, iostat = open_error)
      
      write(158,'(a)')'itags ; monats ; jahrs ; uhrhm ; mstr ; Stakm ; STRID ; O2 ; chla ;&
                      &aki ; agr ; abl ; chlak ; chlag ; chlab ; ssalg ; ss'
   endif
   
   ! remove file1.err, which might still exist from previous run
   pfadstring = trim(adjustl(cpfad)) // 'file1.err'
   open(newunit = u_file1, file = trim(adjustl(cpfad)) // 'file1.err')
   close(u_file1, status = "delete")
   
contains

   subroutine write_header_ergebm
   implicit none

   write(45, '(A)') '*P  01  20      MIB5     XBSB5      MXB5      MICS      XCSB      MXCS     MINH4      XNH4     MXNH4    MIVNO2     XVNO2    MXVNO2    MIVNO3     XVNO3    MXVNO3     MIGSN      XGSN     MXGSN      MIGP     XGELP      MXGP     MIGSP      XGSP     MXGSP      MISI       XSI      MXSI    MICHLA     XCHLA    MXCHLA      MIZO   XZOOIND      MXZO     MIVPH      XVPH     MXVPH      MIMW       XMW      MXMW      MICA       XCA      MXCA      MILF       XLF      MXLF      MISS       XSS      MXSS    MITEMP     XTEMP    MXTEMP      MIO2       XO2      MXO2    MICOLI     XCOLI    MXCOLI     MIKONSS     XKONSS    MXKONS    MIGSPB     XGSPB    MXGSPB    MIGLPB     XGLPB    MXGLPB   MIGSCAD    XGSCAD   MXGSCAD   MIGLCAD    XGLCAD   MXGLCAD    MIGSCR     XGSCR    MXGSCR    MIGLCR     XGLCR    MXGLCR    MIGSFE     XGSFE    MXGSFE    MIGLFE     XGLFE    MXGLFE '
   write(45, '(A)') '*P  02  20    MIGSCU     XGSCU    MXGSCU    MIGLCU     XGLCU    MXGLCU    MIGSMN     XGSMN    MXGSMN    MIGLMN     XGLMN    MXGLMN    MIGSNI     XGSNI    MXGSNI    MIGLNI     XGLNI    MXGLNI    MIGSHG     XGSHG    MXGSHG    MIGLHG     XGLHG    MXGLHG     MIGSU      XGSU     MXGSU     MIGLU      XGLU     MXGLU    MIGSZN     XGSZN    MXGSZN    MIGLZN     XGLZN    MXGLZN    MIGSAS     XGSAS    MXGSAS    MIGLAS     XGLAS    MXGLAS '
   write(45, '(A)') '*P  03  20      XPFL '
   write(45, '(A)') '*P  04  20      XBAL '
   write(45, '(A)') '*P  05  20   XDRBIO1   XGEWDR1   XDRBIO2   XGEWDR2    XDLARN   XIDRAS1   XIDRAS2   XDRMAS1   XDRMAS2    XVOLDR   XDRAKR1   XDRAKR2   XDRBAR1   XDRBAR2   XDRMOR1   XDRMOR2    XFFOOD '
   write(45, '(A)') '*P  06  20     XCORI    XCORIS '
   write(45, '(A)') '*P  07  20    XAKIGR     XDALG    XDALGA    XALMOR    XSEDAL    XALGZO    XALGDR    XDRPFE    XNAEHR    XVKIGR    XANTBL    XALGCO      XFIK      XFIG      XFIB    XAKMUA    XAGMUA    XABMUA     XFHEK     XFHEG     XFHEB    XAKRAU    XAGREA    XABREA    XCHLAK    XCHLAG    XCHLAB    XCCHLK    XCCHLG    XCCHLB '
   write(45, '(A)') '*P  08  20       XIR     XRMUE     XRAKR     XRBAR '
   write(45, '(A)') '*P  09  20    CBSBAB     ABBAU       XCM      XBAC      XCD1      XCD2      XCP1      XCP2    XBACMU    XHNFBA '
   write(45, '(A)') '*P  10  20    XCHNFI     XCHNF    XHNFUP    XHNFRE    XHNFEX    XHNFMO    XHNFMU     XHNFZ    XHNFDR '
   write(45, '(A)') '*P  11  20     XSUSN    XBETTN      XDON     XALGN    xalNO3    xFluN3     xJNO3     xJNH4      XVX0     XSEDX     XVX02 '
   write(45, '(A)') '*P  12  20     xJPO4      xJSi '
   write(45, '(A)') '*P  13  20    XO2NIT     XALGO    XALGAO     XBSBT    XSCHLR      xJO2    XBSBBE    XO2PHY    XABEOW    XABEOR    XRO2DR    XZOORO     XPO2P     XPO2R '
   write(45, '(A)') '*P  14  20     SEDHY '
   write(45, '(A)') '*P  15  20    BMIBSB    BXBSB5    BMXBSB    BMICSB     BXCSB    BMXCSB    BMINH4     BXNH4    BMXNH4    BMINO2     BXNO2    BMXNO2    BMINO3     BXNO3    BMXNO3    BMIGSN     BXGSN    BMXGSN    BMIGLP    BXGELP    BMXGLP    BMIGSP     BXGSP    BMXGSP     BMISI      BXSI     BMXSI    BMICHL    BXCHLA    BMXCHL    BMIZOO    BXZOOI    BMXZOO     BMIPH      BXPH     BMXPH     BMIMW      BXMW     BMXMW     BMICA      BXCA     BMXCA     BMILF      BXLF     BMXLF    BMISSA    BXSSAL    BMXSSA    BMITEM    BXTEMP    BMXTEM     BMIO2     BXO2     BMXO2    BXMICL    BXCOLI    BXMXCL     BMIKONSS     BXKONSS     BMXKONSS   BMIGSPB    BXGSPB   BMXGSPB   BMIGLPB    BXGLPB   BMXGLPB  BMIGSCAD   BXGSCAD  BMXGSCAD  BMIGLCAD   BXGLCAD  BMXGLCAD   BMIGSCR    BXGSCR   BMXGSCR   BMIGLCR    BXGLCR   BMXGLCR   BMIGSFE    BXGSFE   BMXGSFE   BMIGLFE    BXGLFE   BMXGLFE '
   write(45, '(A)') '*P  16  20   BMIGSCU    BXGSCU   BMXGSCU   BMIGLCU    BXGLCU   BMXGLCU   BMIGSMN    BXGSMN   BMXGSMN   BMIGLMN    BXGLMN   BMXGLMN   BMIGSNI    BXGSNI   BMXGSNI   BMIGLNI    BXGLNI   BMXGLNI   BMIGSHG    BXGSHG   BMXGSHG   BMIGLHG    BXGLHG   BMXGLHG    BMIGSU     BXGSU    BMXGSU    BMIGLU     BXGLU    BMXGLU   BMIGSZN    BXGSZN   BMXGSZN   BMIGLZN    BXGLZN   BMXGLZN   BMIGSAS    BXGSAS   BMXGSAS   BMIGLAS    BXGLAS   BMXGLAS '
   write(45, '(A)') '*P  17  20     BXAKG    BXDALG     BXDAA    BXAMOR    BXSEDA    BXALGZ    BXALDR    BXDRPF    BTPMAX     BXVKG    BXANTB    BXALCO     BXFIK     BXFIG     BXFIB    BXKMUE    BXGMUE    BXBMUE     BXHEK     BXHEG     BXHEB     BXKRE     BXGRE     BXBRE    BXCHLK    BXCHLG    BXCHLB '
   write(45, '(A)') '*P  18  20    bxFlN3    bxJNO3    bxJNH4    bxBetN '
   write(45, '(A)') '*P  19  20    bxJPO4     bxJSi '
   write(45, '(A)') '*P  20  20     bxJO2 '
   write(45, '(A)') '*F  01  20      F6.2      F6.2      F6.2      F6.2      F6.2      F6.2      F6.2      F6.2      F6.2      F6.3      F6.3      F6.3      F9.6      F9.6      F9.6      F5.2      F5.2      F5.2      F6.3      F6.3      F6.3      F5.2      F5.2      F5.2      F5.2      F5.2      F5.2      F6.2      F6.2      F6.2      F7.1      F7.1      F7.1      F5.2      F5.2      F5.2      F5.2      F5.2      F5.2      F5.1      F5.1      F5.1      F8.1      F8.1      F8.1      F8.2      F8.2      F8.2      F5.2      F5.2      F5.2      F5.2      F5.2      F5.2      E9.2      E9.2      E9.2       F7.1       F7.1       F7.1       F6.2       F6.2       F6.2       F6.2       F6.2       F6.2       F7.3       F7.3       F7.3       F7.3       F7.3       F7.3       F6.2       F6.2       F6.2       F6.2       F6.2       F6.2       F8.1       F8.1       F8.1       F8.1       F8.1       F8.1 '
   write(45, '(A)') '*F  02  20      F6.2      F6.2      F6.2      F6.2      F6.2      F6.2      F8.1      F8.1      F8.1      F8.1      F8.1      F8.1      F6.2      F6.2      F6.2      F6.2      F6.2      F6.2      F7.3      F7.3      F7.3      F7.3      F7.3      F7.3      F7.3      F7.3      F7.3      F7.3      F7.3      F7.3      F8.1      F8.1      F8.1      F8.1      F8.1      F8.1      F5.1      F5.1      F5.1      F5.1      F5.1      F5.1 '
   write(45, '(A)') '*F  03  20      F7.2 '
   write(45, '(A)') '*F  04  20      F7.2 '
   write(45, '(A)') '*F  05  20      F7.2      F7.2      F7.2      F7.2      F7.2      F6.3      F6.3      F6.3      F6.3      F7.3      F7.5      F7.5      F7.5      F7.5      F7.5      F7.5      F5.3 '
   write(45, '(A)') '*F  06  20      F8.1      F8.1 '
   write(45, '(A)') '*F  07  20      F6.2      F8.5      F8.5      F8.5      F8.5      F9.5      F9.5      F6.2      F5.2      F5.2      F5.2     F12.8      F5.2      F5.2      F5.2      F6.3      F6.3      F6.3      F6.3      F6.3      F6.3      F6.3      F6.3      F6.3      F6.1      F6.1      F6.1      F6.2      F6.2      F6.2 '
   write(45, '(A)') '*F  08  20      F6.3      F6.3      F6.4      F6.4 '
   write(45, '(A)') '*F  09  20      F6.3      F5.3      F6.3      F6.3      F7.3      F7.3      F7.3      F7.3      F6.3      F6.3 '
   write(45, '(A)') '*F  10  20      F8.1      F7.3      F6.3      F6.3      F6.3      F6.3      F6.3      F6.3      F6.3 '
   write(45, '(A)') '*F  11  20      F7.5     F10.8      F7.5      F7.5      F7.5     F10.7     F10.8     F10.8      F8.5      F6.3      F8.5 '
   write(45, '(A)') '*F  12  20      F10.8     F10.8 '
   write(45, '(A)') '*F  13  20      F7.4      F7.4      F7.4      F7.4     F10.8     F10.8      F7.4      F7.4      F7.4      F7.4      F7.4      F7.4      F7.4      F7.4 '
   write(45, '(A)') '*F  14  20     F12.6 '
   write(45, '(A)') '*F  15  20      F6.2      F6.2      F6.2      F6.2      F6.2      F6.2      F6.2      F6.2      F6.2      F6.3      F6.3      F6.3      F9.6      F9.6      F9.6      F5.2      F5.2      F5.2      F6.3      F6.3      F6.3      F5.2      F5.2      F5.2      F5.2      F5.2      F5.2      F6.2      F6.2      F6.2      F7.1      F7.1      F7.1      F5.2      F5.2      F5.2      F5.2      F5.2      F5.2      F5.1      F5.1      F5.1      F8.1      F8.1      F8.1      F8.2      F8.2      F8.2      F5.2      F5.2      F5.2      F5.2      F5.2      F5.2      E9.2      E9.2      E9.2       F7.1       F7.1       F7.1       F6.2       F6.2       F6.2       F6.2       F6.2       F6.2       F7.3       F7.3       F7.3       F7.3       F7.3       F7.3       F6.2       F6.2       F6.2       F6.2       F6.2       F6.2       F8.1       F8.1       F8.1       F8.1       F8.1       F8.1 '
   write(45, '(A)') '*F  16  20      F6.2      F6.2      F6.2      F6.2      F6.2      F6.2      F8.1      F8.1      F8.1      F8.1      F8.1      F8.1      F6.2      F6.2      F6.2      F6.2      F6.2      F6.2      F7.3      F7.3      F7.3      F7.3      F7.3      F7.3      F7.3      F7.3      F7.3      F7.3      F7.3      F7.3      F8.1      F8.1      F8.1      F8.1      F8.1      F8.1      F5.1      F5.1      F5.1      F5.1      F5.1      F5.1 '
   write(45, '(A)') '*F  17  20      F6.2      F8.5      F8.5      F8.5      F8.5      F8.5      F8.5      F6.2      F5.2      F5.2      F5.2     F10.8      F5.2      F5.2      F5.2      F6.3      F6.3      F6.3      F6.3      F6.3      F6.3      F6.3      F6.3      F6.3      F6.1      F6.1      F6.1 '
   write(45, '(A)') '*F  18  20     F10.7     F10.8     F10.8      F8.6 '
   write(45, '(A)') '*F  19  20     F10.8     F10.8 '
   write(45, '(A)') '*F  20  20     F10.8 '
end subroutine write_header_ergebm


subroutine write_header_ergebt
   implicit none
   
   write(155, '(A)') '*P  01  02      VBSB      VCSB      VNH4      VNO2      VNO3      GSNY      GELP      GSPY        SI      CHLA    ZOOIND       VPH        MW        CA        LF     SSALG     TEMPW       VO2      CHNF     COLIY       DLY     SEDHG    TRACER     BSBtY    susNOy    O2ei1y    dalgoy    cchlky    cchlgy    cchlby    zoro2y    schlry    bettny '
   write(155, '(A)') '*P  02  02    BVBSBY    BVCSBY     BNH4Y     BNO2Y     BNO3Y     BGSNY    BGELPY     BGSPY      BSIY    BCHLAY    BZOOIY      BPHY      BMWY      BCAY      BLFY    BSSALY    BTEMPY      BO2Y     BHNFY    BCOLIY      TAU2   bTRACER '
   write(155, '(A)') '*F  01  02      F6.2      F6.2      F6.2      F6.3      F9.6      F5.2      F6.3      F5.2      F5.2      F6.2      F7.1      F5.2      F5.2      F5.1      F8.1      F6.2      F5.2      F5.2      F8.1      E9.2      F7.2     F12.6      F9.3      F8.6      F8.6      F8.6      F8.6      F6.2      F6.2      F6.2      F8.6     F10.8      F8.6 '
   write(155, '(A)') '*F  02  02      F6.2      F6.2      F6.2      F6.3      F9.6      F5.2      F5.3      F5.2      F5.2      F6.2      F7.1      F5.2      F5.2      F5.1      F6.1      F6.2      F5.2      F5.2      F8.1      E9.2      F7.3      F9.3 '
end subroutine write_header_ergebt

      
end subroutine init_result_files