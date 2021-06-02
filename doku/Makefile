#====================================================================
# Makefile zur Erstellung des 
# QSim Dokumentationsportals
# wyrwa seit 27aug20
#====================================================================

CONFIG=./confi
# INSTALL=/home/Frassl/public_html/dokuport
# INSTALL=/home/Wyrwa/public_html/dokuport
# INSTALL=/home/Wyrwa/public_html/dokuport_frassl
INSTALL=/home/Wyrwa/public_html/dokuport_neu

#------------------------------------------------------------- -------

usual: help

help:
	@echo "make dox     ==  compile and install Doxygen Doku to $(INSTALL)"
	@echo "make add     ==  get additional content from $(INSTALL)/taz/qsim_doku.taz"
	@echo "make offline ==  create zip-archive for offline Version"
	@echo "make clean   ==  remove all previous Versions"

dox: check date doc save insta
 
check:
	@if test -d $(INSTALL); then echo "Installing Documentation into $(INSTALL) "; else echo "can't install into $(INSTALL)"; exit 1 ; fi;
	@if test -f $(CONFIG); then echo "expecting configuration in $(CONFIG)"; else echo "$(CONFIG) missing"; exit 1 ; fi;
	@if test -d "$$MY_ARC"; then echo "Directory MY_ARC=$(MY_ARC) existing"; else echo "archive directory missing"; exit 1 ; fi;
	@if test -d ./img ; then echo "image directory existing"; else echo "image directory missing, get qsim_doku.taz ! Look at $(INSTALL)/taz"; exit 1 ; fi;

add:
	@if test -f $(INSTALL)/taz/qsim_doku.taz; then echo "fetching additional material for documentation ..."; cp $(INSTALL)/taz/qsim_doku.taz . ; else echo "additional material not available at $(INSTALL)/taz/qsim_doku.taz"; exit 1 ; fi;
	@tar xvfz qsim_doku.taz

HIER=`pwd -P`
JETZT=`date +"%-d%m%Y"`
date: 
	@echo "jetzt $(JETZT) hier $(HIER);"

clean:	
	@echo "cleaning ..."
	rm -rf ./dox
	rm -rf $(INSTALL)/*

FD=qsim_doku_`date +%d%b%y`.taz

save:
	@/bin/rm -f qsim_doku*.taz
	@tar cvfz $(FD) ./img ./exp ./html ./pdf ./taz/*.zip ./docmd ./confi ./Makefile ./*.html ./*.css
	@cp $(FD) $(MY_ARC)
	@cp $(FD) ./taz/qsim_doku.taz
	@echo "saved $(FD) to $(MY_ARC)"
	
doc:	
	doxygen $(CONFIG)
	
insta:
	cp ./dox/html/index.html ./dox/html/main.html
	cp ./img/bfg_logo.png ./dox/html/logo.png
	cp ./img/qsim_logo.png ./dox/html/qsim.png
	cp ./img/qsim_t.ico ./dox/html/qsim.ico
	cp -r ./img ./dox/html
	cp -r ./exp ./dox/html
	cp -r ./html ./dox/html
	cp -r ./pdf ./dox/html
	cp -r ./taz ./dox/html
	rsync -ua ./dox/html/* $(INSTALL)
	find $(INSTALL) -type d -exec chmod 755 {} \;
	find $(INSTALL) -type f -exec chmod 644 {} \;

offline:
	@if test -d ./dox; then echo "compressing offline Version into qsim_doku_offline.zip"; else echo "you need to execute: make create  previously "; exit 1 ; fi;
	@zip -r qsim_doku_offline.zip ./dox
	@zip -d qsim_doku_offline.zip dox/html/taz/qsim_doku.taz

