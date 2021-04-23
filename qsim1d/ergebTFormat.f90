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

  subroutine ergebTFormat

    write(155, '(A)') '*P  01  02      VBSB      VCSB      VNH4      VNO2      VNO3      GSNY      GELP      GSPY        SI      CHLA    ZOOIND       VPH        MW        CA        LF     SSALG     TEMPW       VO2      CHNF     COLIY       DLY     SEDHG    TRACER     BSBtY    susNOy    O2ei1y    dalgoy    cchlky    cchlgy    cchlby    zoro2y    schlry    bettny '
    write(155, '(A)') '*P  02  02    BVBSBY    BVCSBY     BNH4Y     BNO2Y     BNO3Y     BGSNY    BGELPY     BGSPY      BSIY    BCHLAY    BZOOIY      BPHY      BMWY      BCAY      BLFY    BSSALY    BTEMPY      BO2Y     BHNFY    BCOLIY      TAU2   bTRACER '
    write(155, '(A)') '*F  01  02      F6.2      F6.2      F6.2      F6.3      F9.6      F5.2      F6.3      F5.2      F5.2      F6.2      F7.1      F5.2      F5.2      F5.1      F8.1      F6.2      F5.2      F5.2      F8.1      E9.2      F7.2     F12.6      F9.3      F8.6      F8.6      F8.6      F8.6      F6.2      F6.2      F6.2      F8.6     F10.8      F8.6 '
    write(155, '(A)') '*F  02  02      F6.2      F6.2      F6.2      F6.3      F9.6      F5.2      F5.3      F5.2      F5.2      F6.2      F7.1      F5.2      F5.2      F5.1      F6.1      F6.2      F5.2      F5.2      F8.1      E9.2      F7.3      F9.3 '


  end subroutine ergebTFormat
  