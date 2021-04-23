#include <math.h>
#include <stdio.h>
#include <stdlib.h>
// needed on bfg voss-mod01
#include <string.h>
#include <iostream>
using std::cout;
using std::cin;
using std::endl;
#include <fstream>
using std::ifstream;
using std::ofstream;

const double pi=3.1415926536;
const int maxchar=500;

extern "C" { void trans_read_(char *modver, int *wortlang, int *nonu, int *intereck, float *wicht, int *wrong, float *p, float *u, float *dir, int *kontrollknoten);}

extern "C" { bool lossy_input_(unsigned int *binin, float *p, float *u, float *d, float *w);}

/**
<h2>Einlesen von verlustbehaftet komprimierten Transportinformationen</h2>
geschrieben wurde transinfo... von casu:mass_trans2.c\n
*/
void trans_read_(char *modver, int *wortlang, int *nonu, int *intereck, float *wicht, int *wrong, float *p, float *u, float *dir, int *kontrollknoten){
   ifstream bin_in;
   int i, j, nn ;
   unsigned int bini[5];
   char file[maxchar];   char text[maxchar+20];
   char mona[maxchar];
   double sumwicht;
   double pmin, pmax;
   double umax, dt_casu;
   float p1, u1, d1, w1;

   *wrong=0;

   if(*wortlang>maxchar){*wrong=-5;return;}

   for(j=0 ; j<*wortlang ; j++){ mona[j]=modver[j]; }  mona[*wortlang]='\0';

   sprintf(file,"%s",mona);
   sprintf(text,"stat %s >/dev/null",file);
   if(system(text)!=0){
      *wrong=-1;
      cout<<" trans_read, no file "<<file<<endl;
      bin_in.close(); bin_in.clear(); return;
   }

   bin_in.open(file);
   if(!bin_in){
      cout<<"Could not open file >"<< modver<<"<  file= >"<<file<<"<"<<endl;
      *wrong=-2;
      bin_in.close(); bin_in.clear(); return;
   }/*else{
      cout<<" trans_read, opened file "<<file<<endl;
   }*/

   bin_in.read( (char *) &nn , sizeof(int)*1);
   bin_in.read( (char *) &pmax , sizeof(double)*1);
   bin_in.read( (char *) &pmin , sizeof(double)*1);
   bin_in.read( (char *) &umax , sizeof(double)*1);
   bin_in.read( (char *) &dt_casu , sizeof(double)*1);
   if(nn!=*nonu){
      cout<<"nodenumber="<<nn<<" wrong in transinfo, should be ="<<*nonu<<endl; 
      *wrong=-3; 
      bin_in.close(); bin_in.clear(); return;
   }
   //cout<<"trans_read:"<<file<<" pmax="<<pmax<<" pmin="<<pmin<<" umax="<<umax<<" dt_casu="<<dt_casu<<endl;

   for(i=0 ; i<*nonu ; i++){
      bin_in.read( (char *) &bini , sizeof(unsigned int)*5);
      if(!bin_in){cout<<"lesefehler bini in transinfo i="<<i<<"\n"; *wrong=-4;  bin_in.close(); bin_in.clear(); return; }
      //cout<<i+1;
      sumwicht=0.0;
      for(j=0 ; j<4 ; j++){
         intereck[i*4+j]=int(bini[j]/256);
         if(i==(*kontrollknoten-1) ){
            cout<<"i,j="<<i<<" "<<j<<" bini="<<bini[j]<<" intereck="<<intereck[i*4+j]<<endl;
         }
         //if(i==0)cout<<"transinfo node#="<<i<<" j="<<j<<" bini[j]="<<bini[j]<<" intereck["<<i*4+j<<"]="<<intereck[i*4+j] <<endl; 
         //if((intereck[i*4+j]>=*nonu)||(intereck[i*4+j]<1)){
         //   cout<<"intereck wrong in transinfo node#="<<i<<" j="<<j<<" bini[j]="<<bini[j]<<endl; 
         //   *wrong=-2; 
         //   return;
         //}
         wicht[i*4+j]=double(bini[j]-(intereck[i*4+j]*256))/255;
         if(isnan(wicht[i*4+j])){
            cout<<"trans_read__isnan(wicht i,j="<<i<<j<<endl; perror("trans_read_isnan(wicht[i*4+j]");}
         sumwicht=sumwicht+wicht[i*4+j];
         //sprintf(text," |  %i   %9.5f",intereck[i*4+j]+1,wicht[i*4+j]);
         //cout<<text;
      }
      if(sumwicht==0.0){
         for(j=0 ; j<4 ; j++){wicht[i*4+j]=0.0;}
      }else{
         for(j=0 ; j<4 ; j++){wicht[i*4+j]=wicht[i*4+j]/sumwicht;}
      }
      if(lossy_input_(&bini[4],&p1,&u1,&d1,&w1)){
         p[i]=( p1 )*(pmax-pmin) +pmin;
         u[i]=( u1 )*umax;
         dir[i]= 2.0*(d1-0.5)*pi;
      // aus trans_write.c (casu)
      // dir=0.0
      // if(u>0.0){dir=atan2(now.uuu[j].f[l].x,now.uuu[j].f[l].y);}
      // dir=((dir/pi)+1)/2.0; // 

      }else{cout<<"trans_read_, lossy_input gone wrong\n"; *wrong=-1;  bin_in.close(); bin_in.clear(); return; }
      //cout<<endl;
   }

   bin_in.close();bin_in.clear();
   return;
}

/**
 Funktion lossy_input zerlegt einen 4-Byte unsigned Integer wieder in 4 Reelle Zahlen im Wertebereich 0.0 ... 1.0 \n
 siehe flow_read_() \n
*/
bool lossy_input_(unsigned int *binin, float *p, float *u, float *d, float *w){
   unsigned int bin,ip,iu,id,iw;

   bin=*binin;

   ip=bin/(256*256*256);
   *p=float(ip)/255;

   bin=bin-(ip*(256*256*256));
   iu=bin/(256*256);
   *u=float(iu)/255;

   bin=bin-(iu*(256*256));
   id=bin/(256);
   *d=float(id)/255;

   bin=bin-(id*(256));
   iw=bin;
   *w=float(iw)/255;

   //cout<<" binin="<<*binin<<" ip="<<ip<<" iu="<<iu<<" id="<<id<<" iw="<<iw<<endl;
   return true;
}
