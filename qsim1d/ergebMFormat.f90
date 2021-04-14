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

 subroutine ergebMFormat 

 WRITE(45, '(A)') '*P  01  18      MIB5     XBSB5      MXB5      MICS      XCSB      MXCS     MINH4      XNH4     MXNH4    MIVNO2     XVNO2    MXVNO2    MIVNO3     XVNO3    MXVNO3     MIGSN      XGSN     MXGSN      MIGP     XGELP      MXGP     MIGSP      XGSP     MXGSP      MISI       XSI      MXSI    MICHLA     XCHLA    MXCHLA      MIZO   XZOOIND      MXZO     MIVPH      XVPH     MXVPH      MIMW       XMW      MXMW      MICA       XCA      MXCA      MILF       XLF      MXLF      MISS       XSS      MXSS    MITEMP     XTEMP    MXTEMP      MIO2       XO2      MXO2    MICOLI     XCOLI    MXCOLI     MIKONSS     XKONSS    MXKONS    M�GSZN     XGSZN    MXGSZN    M�GLZN     XGLZN    MXGLZN   M�GSCAD    XGSCAD   MXGSCAD   M�GLCAD    XGLCAD   MXGLCAD    M�GSCU     XGSCU    MXGSCU    M�GLCU     XGLCU    MXGLCU    M�GSNI     XGSNI    MXGSNI    M�GLNI     XGLNI    MXGLNI '
 WRITE(45, '(A)') '*P  02  18      XPFL '
 WRITE(45, '(A)') '*P  03  18      XBAL '
 WRITE(45, '(A)') '*P  04  18   XDRBIO1   XGEWDR1   XDRBIO2   XGEWDR2    XDLARN   XIDRAS1   XIDRAS2   XDRMAS1   XDRMAS2    XVOLDR   XDRAKR1   XDRAKR2   XDRBAR1   XDRBAR2   XDRMOR1   XDRMOR2    XFFOOD '
 WRITE(45, '(A)') '*P  05  18     XCORI    XCORIS '
 WRITE(45, '(A)') '*P  06  18    XAKIGR     XDALG    XDALGA    XALMOR    XSEDAL    XALGZO    XALGDR    XDRPFE    XNAEHR    XVKIGR    XANTBL    XALGCO      XFIK      XFIG      XFIB    XAKMUA    XAGMUA    XABMUA     XFHEK     XFHEG     XFHEB    XAKRAU    XAGREA    XABREA    XCHLAK    XCHLAG    XCHLAB    XCCHLK    XCCHLG    XCCHLB '
 WRITE(45, '(A)') '*P  07  18       XIR     XRMUE     XRAKR     XRBAR '
 WRITE(45, '(A)') '*P  08  18    CBSBAB     ABBAU       XCM      XBAC      XCD1      XCD2      XCP1      XCP2    XBACMU    XHNFBA '
 WRITE(45, '(A)') '*P  09  18    XCHNFI     XCHNF    XHNFUP    XHNFRE    XHNFEX    XHNFMO    XHNFMU     XHNFZ    XHNFDR '
 WRITE(45, '(A)') '*P  10  18     XSUSN    XBETTN      XDON     XALGN    xalNO3    xFluN3     xJNO3     xJNH4      XVX0     XSEDX     XVX02 '
 WRITE(45, '(A)') '*P  11  18     xJPO4      xJSi '
 WRITE(45, '(A)') '*P  12  18    XO2NIT     XALGO    XALGAO     XBSBT    XSCHLR      xJO2    XBSBBE    XO2PHY    XABEOW    XABEOR    XRO2DR    XZOORO     XPO2P     XPO2R '
 WRITE(45, '(A)') '*P  13  18     SEDHY '
 WRITE(45, '(A)') '*P  14  18    BMIBSB    BXBSB5    BMXBSB    BMICSB     BXCSB    BMXCSB    BMINH4     BXNH4    BMXNH4    BMINO2     BXNO2    BMXNO2    BMINO3     BXNO3    BMXNO3    BMIGSN     BXGSN    BMXGSN    BMIGLP    BXGELP    BMXGLP    BMIGSP     BXGSP    BMXGSP     BMISI      BXSI     BMXSI    BMICHL    BXCHLA    BMXCHL    BMIZOO    BXZOOI    BMXZOO     BMIPH      BXPH     BMXPH     BMIMW      BXMW     BMXMW     BMICA      BXCA     BMXCA     BMILF      BXLF     BMXLF    BMISSA    BXSSAL    BMXSSA    BMITEM    BXTEMP    BMXTEM     BMIO2      BXO2     BMXO2    BXMICL    BXCOLI    BXMXCL     BMIKONSS     BXKONSS     BMXKONS   BM�GSZN    BXGSZN   BMXGSZN   BM�GLZN    BXGLZN   BMXGLZN  BM�GSCAD   BXGSCAD  BMXGSCAD  BM�GLCAD   BXGLCAD  BMXGLCAD   BM�GSCU    BXGSCU   BMXGSCU   BM�GLCU    BXGLCU   BMXGLCU   BM�GSNI    BXGSNI   BMXGSNI   BM�GLNI    BXGLNI   BMXGLNI '
 WRITE(45, '(A)') '*P  15  18     BXAKG    BXDALG     BXDAA    BXAMOR    BXSEDA    BXALGZ    BXALDR    BXDRPF    BTPMAX     BXVKG    BXANTB    BXALCO     BXFIK     BXFIG     BXFIB    BXKMUE    BXGMUE    BXBMUE     BXHEK     BXHEG     BXHEB     BXKRE     BXGRE     BXBRE    BXCHLK    BXCHLG    BXCHLB '
 WRITE(45, '(A)') '*P  16  18    bxFlN3    bxJNO3    bxJNH4    bxBetN '
 WRITE(45, '(A)') '*P  17  18    bxJPO4     bxJSi '
 WRITE(45, '(A)') '*P  18  18     bxJO2 '     
 WRITE(45, '(A)') '*F  01  18      F6.2      F6.2      F6.2      F6.2      F6.2      F6.2      F6.2      F6.2      F6.2      F6.3      F6.3      F6.3      F9.6      F9.6      F9.6      F5.2      F5.2      F5.2      F6.3      F6.3      F6.3      F5.2      F5.2      F5.2      F5.2      F5.2      F5.2      F6.2      F6.2      F6.2      F7.1      F7.1      F7.1      F5.2      F5.2      F5.2      F5.2      F5.2      F5.2      F5.1      F5.1      F5.1      F8.1      F8.1      F8.1      F8.2      F8.2      F8.2      F5.2      F5.2      F5.2      F5.2      F5.2      F5.2      E9.2      E9.2      E9.2       F7.1       F7.1       F7.1       F9.2       F9.2       F9.2       F9.2       F9.2       F9.2       F8.4       F8.4       F8.4       F8.4       F8.4       F8.4       F7.3       F7.3       F7.3       F7.3       F7.3       F7.3       F7.3       F7.3       F7.3       F7.3       F7.3       F7.3 '
 WRITE(45, '(A)') '*F  02  18      F7.2 '
 WRITE(45, '(A)') '*F  03  18      F7.2 '
 WRITE(45, '(A)') '*F  04  18      F7.2      F7.2      F7.2      F7.2      F7.2      F6.3      F6.3      F6.3      F6.3      F7.3      F7.5      F7.5      F7.5      F7.5      F7.5      F7.5      F5.3 '
 WRITE(45, '(A)') '*F  05  18      F8.1      F8.1 '
 WRITE(45, '(A)') '*F  06  18      F6.2      F8.5      F8.5      F8.5      F8.5      F9.5      F9.5      F6.2      F5.2      F5.2      F5.2     F12.8      F5.2      F5.2      F5.2      F6.3      F6.3      F6.3      F6.3      F6.3      F6.3      F6.3      F6.3      F6.3      F6.1      F6.1      F6.1      F6.2      F6.2      F6.2 '
 WRITE(45, '(A)') '*F  07  18      F6.3      F6.3      F6.4      F6.4 '
 WRITE(45, '(A)') '*F  08  18      F6.3      F5.3      F6.3      F6.3      F7.3      F7.3      F7.3      F7.3      F6.3      F6.3 '
 WRITE(45, '(A)') '*F  09  18      F8.1      F7.3      F6.3      F6.3      F6.3      F6.3      F6.3      F6.3      F6.3 '
 WRITE(45, '(A)') '*F  10  18      F7.5     F10.8      F7.5      F7.5      F7.5     F10.7     F10.8     F10.8      F8.5      F6.3      F8.5 '
 WRITE(45, '(A)') '*F  11  18      F10.8     F10.8 '
 WRITE(45, '(A)') '*F  12  18      F7.4      F7.4      F7.4      F7.4     F10.8     F10.8      F7.4      F7.4      F7.4      F7.4      F7.4      F7.4      F7.4      F7.4 '
 WRITE(45, '(A)') '*F  13  18     F12.6 '
 WRITE(45, '(A)') '*F  14  18      F6.2      F6.2      F6.2      F6.2      F6.2      F6.2      F6.2      F6.2      F6.2      F6.3      F6.3      F6.3      F9.6      F9.6      F9.6      F5.2      F5.2      F5.2      F6.3      F6.3      F6.3      F5.2      F5.2      F5.2      F5.2      F5.2      F5.2      F6.2      F6.2      F6.2      F7.1      F7.1      F7.1      F5.2      F5.2      F5.2      F5.2      F5.2      F5.2      F5.1      F5.1      F5.1      F8.1      F8.1      F8.1      F8.2      F8.2      F8.2      F5.2      F5.2      F5.2      F5.2      F5.2      F5.2      E9.2      E9.2      E9.2       F7.1       F7.1       F7.1       F9.2       F9.2       F9.2       F9.2       F9.2       F9.2       F8.4       F8.4       F8.4       F8.4       F8.4       F8.4       F7.3       F7.3       F7.3       F7.3       F7.3       F7.3       F7.3       F7.3       F7.3       F7.3       F7.3       F7.3 '
 WRITE(45, '(A)') '*F  15  18      F6.2      F8.5      F8.5      F8.5      F8.5      F8.5      F8.5      F6.2      F5.2      F5.2      F5.2     F10.8      F5.2      F5.2      F5.2      F6.3      F6.3      F6.3      F6.3      F6.3      F6.3      F6.3      F6.3      F6.3      F6.1      F6.1      F6.1 '
 WRITE(45, '(A)') '*F  16  18     F10.7     F10.8     F10.8      F8.6 '
 WRITE(45, '(A)') '*F  17  18     F10.8     F10.8 '
 WRITE(45, '(A)') '*F  18  18     F10.8 '

 end subroutine ergebMFormat
