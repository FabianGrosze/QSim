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

         planktonic_variable_name( 1)= "             tempw"
         planktonic_variable_name( 2)= "               vo2"
         planktonic_variable_name( 3)= "              vNH4"
         planktonic_variable_name( 4)= "              vNO2"
         planktonic_variable_name( 5)= "              vNO3"
         planktonic_variable_name( 6)= "              gelp" ! geloester ortho-Phosphat-P, tiefengemittelt | war(9)
         planktonic_variable_name( 7)= "                si" ! geloestes Silikat-Silizium, tiefengemittelt | war(8)
         planktonic_variable_name( 8)= "               aki" ! war(16)
         planktonic_variable_name( 9)= "               agr" ! war(17)
         planktonic_variable_name(10)= "               abl" ! war(18)
         planktonic_variable_name(11)= "              chla" ! war(12)
         planktonic_variable_name(12)= "            chlaki" ! war(13)
         planktonic_variable_name(13)= "            chlagr" ! war(14)
         planktonic_variable_name(14)= "            chlabl" ! war(15)
!------------------------------------- obige gibt es auch tiefenaufgelöst; untenstehende nicht
         planktonic_variable_name(15)= "               vx0" ! war(6)
         planktonic_variable_name(16)= "              vx02" ! war(7)
         planktonic_variable_name(17)= "              obsb" ! war(10)
         planktonic_variable_name(18)= "              ocsb" ! war(11)
         planktonic_variable_name(19)= "             vkigr"
         planktonic_variable_name(20)= "             antbl"
         planktonic_variable_name(21)= "            svhemk"
         planktonic_variable_name(22)= "            svhemg"
         planktonic_variable_name(23)= "            svhemb"
         planktonic_variable_name(24)= "             akbcm"
         planktonic_variable_name(25)= "             agbcm"
         planktonic_variable_name(26)= "             abbcm"
         planktonic_variable_name(27)= "             akiiv"
         planktonic_variable_name(28)= "             agriv"
         planktonic_variable_name(29)= "             abliv"
         planktonic_variable_name(30)= "              Q_NK"
         planktonic_variable_name(31)= "              Q_PK"
         planktonic_variable_name(32)= "              Q_SK"
         planktonic_variable_name(33)= "              Q_NG"
         planktonic_variable_name(34)= "              Q_PG"
         planktonic_variable_name(35)= "              Q_NB"
         planktonic_variable_name(36)= "              Q_PB"
         planktonic_variable_name(37)= "              CD(1"
         planktonic_variable_name(38)= "              CD(2"
         planktonic_variable_name(39)= "              CP(1"
         planktonic_variable_name(40)= "              CP(2"
         planktonic_variable_name(41)= "                CM"
         planktonic_variable_name(42)= "               BAC"
         planktonic_variable_name(43)= "             O2BSB"
         planktonic_variable_name(44)= "              BL01"
         planktonic_variable_name(45)= "              BL02"
         planktonic_variable_name(46)= "              vbsb"
         planktonic_variable_name(47)= "              vcsb"
         planktonic_variable_name(48)= "              CHNF"
         planktonic_variable_name(49)= "             BVHNF"
         planktonic_variable_name(50)= "            zooind"
         planktonic_variable_name(51)= "            abrzo1"
         planktonic_variable_name(52)= "             ssalg"
         planktonic_variable_name(53)= "                ss"
         planktonic_variable_name(54)= "             fssgr"
         planktonic_variable_name(55)= "             fbsgr"
         planktonic_variable_name(56)= "             frfgr"
         planktonic_variable_name(57)= "               nl0"
         planktonic_variable_name(58)= "               pl0"
         planktonic_variable_name(59)= "             stind"
         planktonic_variable_name(60)= "            dlarvn"
         planktonic_variable_name(61)= "              coli"
         planktonic_variable_name(62)= "                mw"
         planktonic_variable_name(63)= "                pw"
         planktonic_variable_name(64)= "                ca"
         planktonic_variable_name(65)= "                lf"
         planktonic_variable_name(66)= "               vph"
         planktonic_variable_name(67)= "              gesN"
         planktonic_variable_name(68)= "              gesP"
         planktonic_variable_name(69)= "             SKmor"
         planktonic_variable_name(70)= "             DOSCF"
         planktonic_variable_name(71)= "            Tracer"
         planktonic_variable_name(72)= "              salz"
         planktonic_variable_name(73)= "         age_decay"
         planktonic_variable_name(74)= "         age_arith"
         planktonic_variable_name(75)= "        age_growth"
         planktonic_variable_name(76)= "          empty_76"
         planktonic_variable_name(77)= "           akmor_1"
         planktonic_variable_name(78)= "           agmor_1"
         planktonic_variable_name(79)= "           abmor_1"
         planktonic_variable_name(80)= "              gsZn"  ! Schwermetalle .. gesamt, gelöst; 
         planktonic_variable_name(81)= "              glZn"
         planktonic_variable_name(82)= "             gsCad"
         planktonic_variable_name(83)= "             glCad"
         planktonic_variable_name(84)= "              gsCu"
         planktonic_variable_name(85)= "              glCu"
         planktonic_variable_name(86)= "              gsNi"
         planktonic_variable_name(87)= "              glNi"
         planktonic_variable_name(88)= "              gsAs"
         planktonic_variable_name(89)= "              glAs"
         planktonic_variable_name(90)= "              gsPb"
         planktonic_variable_name(91)= "              glPb"
         planktonic_variable_name(92)= "              gsCr"
         planktonic_variable_name(93)= "              glCr"
         planktonic_variable_name(94)= "              gsFe"
         planktonic_variable_name(95)= "              glFe"
         planktonic_variable_name(96)= "              gsHg"
         planktonic_variable_name(97)= "              glHg"
         planktonic_variable_name(98)= "              gsMn"
         planktonic_variable_name(99)= "              glMn"
         planktonic_variable_name(100)="               gsU"
         planktonic_variable_name(101)="               glU"
