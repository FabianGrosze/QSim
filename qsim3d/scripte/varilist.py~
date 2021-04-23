#!/usr/bin/python
# -*- coding: iso-8859-1 -*-
#
## Jens Wyrwa 30nov15
#

import sys, string, time 

def intro(i):
    progname="varilist.py"
    if i == 0:
         print ("Es startet: " + progname[0:len(progname)])
    if i == 1:
         print (progname + " endet regulär")

def lesen_plankt():
    gesanz=0
    zeilen=[]
    tabelleneintrag="!!<tr><td>"

    datei = open("planktische_variablen.f95",'r')
    lines = datei.readlines()
    datei.close()

    ausgabe = open("ausgabekonzentrationen_beispiel.test",'w')
    ausgabe.write('# tiefengemittelte planktische Variablen\n')
    ausgabe2 = open("planktonic_variable_name.test",'w')
    tabellenanfang_t="<table planktonic_variable>"
    tabellenende_t="</table planktonic_variable>"
    for n in range(len(lines)):
        if lines[n].find(tabellenanfang_t) >=0 :
            #print ("tabellenanfang in Zeile " + repr(n+1))
            anfa=n
        if lines[n].find(tabellenende_t) >=0 :
            #print ("tabellenende in Zeile " + repr(n+1))
            ende=n
    for n in range(anfa,ende):
        if lines[n][0:10] == tabelleneintrag :
            zeile=lines[n].strip()
            zeile=zeile.strip("!><trd/")
            zeilen.append(zeile)
        gesanz=gesanz+1
    for n in range(len(zeilen)):
         eintraege=zeilen[n].split('</td><td>')
         if int(eintraege[0]) != (n+1) :
            print ("FEHLER planktonic_variable tabellennummer=" + repr(int(eintraege[0])) + ' zeilennummer=' + repr(n+1))
            return
         varnam=eintraege[1].replace("\t" , "")
         varnam=varnam.replace(" \\anchor " , "")
         vari=varnam.split(' ')
         beschr=eintraege[2].replace('\t' , '')
         ausgabe.write('0' + repr(n+1).rjust(5) + vari[0].rjust(18) + '  ' + beschr[0:81].ljust(80) + '\n')
         ausgabe2.write('         planktonic_variable_name(' + repr(n+1).rjust(2) + ')= "' + vari[0].rjust(18) + '"\n')
    ausgabe2.close()
    print("ausgabe auf planktonic_variable_name erfolgt")

    ausgabe.write('# tiefenaufgelöste planktische Variablen\n')
    ausgabe2 = open("plankt_vari_vert_name.test",'w')
    tabellenanfang_t="!!<table plankt_vari_vert>"
    tabellenende_t="!!</table plankt_vari_vert>"
    for n in range(len(lines)):
        if lines[n].find(tabellenanfang_t) >=0 :
            #print ("tabellenanfang_vert in Zeile " + repr(n+1))
            anfa=n
        if lines[n].find(tabellenende_t) >=0 :
            #print ("tabellenende_vert in Zeile " + repr(n+1))
            ende=n
    del zeilen[:]
    for n in range(anfa,ende):
        if lines[n][0:10] == tabelleneintrag :
            zeile=lines[n].strip()
            zeile=zeile.strip("!><trd/")
            zeilen.append(zeile)
        gesanz=gesanz+1
    for n in range(len(zeilen)):
         eintraege=zeilen[n].split('</td><td>')
         if int(eintraege[0]) != (n+1) :
            print ("FEHLER plankt_vari_vert tabellennummer=" + repr(int(eintraege[0])) + ' zeilennummer=' + repr(n+1))
            return
         varnam=eintraege[1].replace("\t" , "")
         varnam=varnam.replace(" \\anchor " , "")
         vari=varnam.split(' ')
         beschr=eintraege[2].replace('\t' , '')
         ausgabe.write('0' + repr(n+1).rjust(5) + vari[0].rjust(18) + '  ' + beschr[0:81].ljust(80) + '\n')
         ausgabe2.write('         plankt_vari_vert_name(' + repr(n+1).rjust(2) + ')= "' + vari[0].rjust(18) + '"\n')
    ausgabe2.close()
    print("ausgabe auf plankt_vari_vert_name erfolgt")

    ausgabe.close()
    print("ausgabe auf ausgabekonzentrationen_beispiel erfolgt")

    return

##-------------------------------------------------------------
if __name__ == "__main__":
    intro(0)
    lesen_plankt()
    intro(1)
#    misc()



##    s=0.125
##    zahl="0.3333"
##    z=float(zahl)
##    print("test: " + repr(s*4.0) + "  " + repr(z*2.0) )

##  del zeilen[0:2]
##  for n in range(len(zeilen)):
##       print zeilen[n]
##    print("Datei " + txt + " gelesen \n"),len(zeilen)," Tabellenzeilen von gesamt ", gesanz
##    datei.close()
##    datei = open("ausgabe",'w')
##    for n in range(len(zeilen)):
##         datei.write(zeilen[n] + '\n')
##    datei.close()
##    print("ausgabe erfolgt")
##------------------------------------ 
##  for n in datei:
##          lzeile=lines[n].lstrip("!!<tr><td>")
##          zeile=lzeile.rstrip("</td></tr>")
##          rzeile=lines[n].rstrip(">")
##          print rzeile
##  print("anz= " + str(anz) )
##  anzsiebtel=anz/7.0
##  print anzsiebtel
##  print "anz= " , anz, '{0:05d}'.format(anz) , "anz/7= ", "{0:020.12f}".format(anzsiebtel)
##  print("{0:.2f}".format(a))
##  print '{0:05d}'.format(i)
##------------------------------------ liste in liste von https://docs.python.org/2/tutorial/introduction.html#lists
## http://gnosis.cx/TPiP/chap2.txt
def misc():
    a = ['a', 'b', 'c']
    n = [1, 2, 3]
    x = [a, n]
    print (x)
    #[['a', 'b', 'c'], [1, 2, 3]]
    print (x[0])
    #['a', 'b', 'c']
    print (x[0][1])
    #'b'
    print (x[1][0]+x[1][2])
    #4
