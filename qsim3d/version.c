
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
   cout<<"Diese Version wurde zuletzt compiliert am: "<<versiontext<<endl;

return;}
