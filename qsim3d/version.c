//---------------------------------------------------------------------------------------
//
//   QSim - Programm zur Simulation der Wasserqualität
//
//   Copyright (C) 2020 Bundesanstalt für Gewässerkunde, Koblenz, Deutschland, http://www.bafg.de
//
//   Dieses Programm ist freie Software. Sie können es unter den Bedingungen der 
//   GNU General Public License, Version 3,
//   wie von der Free Software Foundation veröffentlicht, weitergeben und/oder modifizieren. 
//   Die Veröffentlichung dieses Programms erfolgt in der Hoffnung, daß es Ihnen von Nutzen sein wird, 
//   aber OHNE IRGENDEINE GARANTIE, sogar ohne die implizite Garantie der MARKTREIFE oder der VERWENDBARKEIT FÜR EINEN BESTIMMTEN ZWECK. 
//   Details finden Sie in der GNU General Public License.
//   Sie sollten ein Exemplar der GNU General Public License zusammen mit diesem Programm erhalten haben. 
//   Falls nicht, siehe http://www.gnu.org/licenses/.  
//   
//	Programmiert von:
//	1979 bis 2018 Volker Kirchesch
//	seit 2011 Jens Wyrwa, Wyrwa@bafg.de
//
//---------------------------------------------------------------------------------------

#include "version.h"

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
// needed on bfg voss-mod01
#include <string.h>

#include <iostream>
using std::cout;
using std::cin;
using std::endl;

extern "C" { void versionsdatum_();}
//void versionsdatum_();

/**
 Funktion zur Ausgabe des Compilierdatums,  \n
 Makefile schreibt version.h, das hier includet wird. \n
*/
void versionsdatum_(){

   int tag,monat,jahr;
   char versiontext[90];
   char code_source[200];

   jahr=version;
   tag=jahr/1000000; jahr=jahr-tag*1000000;
   monat=jahr/10000; jahr=jahr-monat*10000;
   sprintf(versiontext,"%i. %i. %i",tag,monat,jahr);
   //printf("heute ist der %s \n",versiontext);
   cout<<"last compiled:   "<<versiontext<<endl;
return;}
