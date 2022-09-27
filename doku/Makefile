#====================================================================
# Makefile zur Erstellung des 
# QSim Dokumentationsportals
# wyrwa seit 27aug20
#====================================================================

#CONFIG=./confi
CONFIG=./confi_stripped
CONFIG_OFF=./confi_offline
# INSTALL=/home/Frassl/public_html/dokuport
# INSTALL=/home/Wyrwa/public_html/dokuport
# INSTALL=/home/Wyrwa/public_html/dokuport_frassl
# ADDITIONAL=/srv/cifs-mounts/u2/U/U2/QSim/Dokumentation_u_Handbuecher/QSim-3D
ADDITIONAL=/srv/cifs-mounts/u2/Projekte/QSim/Dokumentation_und_Handbuecher/QSim/QSim-Dokuportal/Dokuportal_ergaenzendes_Material
INSTALL=/home/Frassl/public_html/dokuportal_test


#------------------------------------------------------------- -------

usual: help

help:
	@echo "make clean   ==  remove all previous Versions"
	@echo "make doc     ==  compile local Version into ./dox/html/main.html (fast)"
	@echo "make install ==  compile and install Doku into $(INSTALL)"
	@echo "make get_add ==  get the additional content from $(ADDITIONAL)/qsim_doku_additional.zip and extract"
	@echo "make put_add ==  collect additional content into qsim_doku_additional.zip and copy to $(ADDITIONAL)"
	@echo "make offline ==  produce stand-alone Version in ./qsim_doku_offline.zip"
	@echo "make all     ==  compile, achive and install ; including upload of newly made offline Version and additional Material (slow)"

all: clean check date offline doc save insta

get_add:
	@if test -f $(ADDITIONAL)/qsim_doku_additional.zip; \
	   then echo "fetching additional material for documentation from $(ADDITIONAL)"; cp $(ADDITIONAL)/qsim_doku_additional.zip . ; \
	   else echo "additional material not available at $(ADDITIONAL) " ; exit 1 ; \
	   fi;
	@echo "downloaded qsim_doku_additional.zip ### uncompressing it here could overwrite existing data ### "

put_add:
	@zip -r qsim_doku_additional.zip ./img ./exp ./html ./pdf ./taz/*Volker.zip
	@cp qsim_doku_additional.zip $(ADDITIONAL)

clean:	
	@echo "cleaning ..."
	@rm -rf ./dox $(INSTALL)/* ./doku_offline *.taz *.zip

check:
	@if test -d $(INSTALL); then echo "Installing Documentation into $(INSTALL) "; else echo "can't install into $(INSTALL)"; exit 1 ; fi;
	@if test -f $(CONFIG); then echo "expecting configuration in $(CONFIG)"; else echo "$(CONFIG) missing"; exit 1 ; fi;
	@if test -d "$$MY_ARC"; then echo "Directory MY_ARC=$(MY_ARC) existing"; else echo "archive directory missing"; exit 1 ; fi;
	@if test -d ./img ; then echo "image directory existing"; else echo "image directory missing, get qsim_doku_additional.zip ! Look at $(INSTALL)/taz"; exit 1 ; fi;

HIER=`pwd -P`
JETZT=`date +"%-d%m%Y"`
date: 
	@echo "jetzt $(JETZT) hier $(HIER);"
		
doc:	
	@doxygen $(CONFIG)
	@cp ./dox/html/index.html ./dox/html/main.html
	@cp ./img/bfg_logo.png ./dox/html/logo.png
	@cp ./img/qsim_logo.png ./dox/html/qsim.png
	@cp ./img/qsim_t.ico ./dox/html/qsim.ico
	@cp -r ./img ./dox/html
	@cp -r ./exp ./dox/html
	@cp -r ./html ./dox/html
	@cp -r ./pdf ./dox/html
	@cp -r ./taz ./dox/html

FD=qsim_doku_`date +%d%b%y`.taz

save:
	@/bin/rm -f qsim_doku*.zip qsim_doku*.taz ./dox/html/taz/*.taz
	@zip -r qsim_doku_additional.zip ./img ./exp ./html ./pdf ./taz/*Volker.zip
	@cp qsim_doku_additional.zip $(ADDITIONAL)
	@echo "written qsim_doku_additional.zip to $(ADDITIONAL)"
	@tar cvfz $(FD) qsim_doku_additional.zip ./docmd ./conf* ./Makefile ./*.html ./*.css
	@cp $(FD) $(MY_ARC)
	@echo "saved $(FD) to $(MY_ARC)"

install: doc insta
	
insta:
	if test -f $(ADDITIONAL)/qsim_doku_additional.zip; then cp $(ADDITIONAL)/qsim_doku_additional.zip ./dox/html/taz; else echo "### $(ADDITIONAL)/qsim_doku_additional.zip missing ###"; fi;
	if test -f $(ADDITIONAL)/qsim_doku_offline.zip; then cp $(ADDITIONAL)/qsim_doku_offline.zip ./dox/html/taz; else echo "### $(ADDITIONAL)/qsim_doku_offline.zip missing ###"; echo "do: make offline - previously"; fi;
	rsync -ua ./dox/html/* $(INSTALL)
	find $(INSTALL) -type d -exec chmod 755 {} \;
	find $(INSTALL) -type f -exec chmod 644 {} \;
	
offline:
	@if test -f $(CONFIG_OFF); then echo "expecting configuration in $(CONFIG_OFF)"; else echo "$(CONFIG_OFF) missing"; exit 1 ; fi;
	rm -rf ./doku_offline qsim_doku_offline.zip
	mkdir doku_offline
	doxygen $(CONFIG_OFF)
	cp ./doku_offline/html/index.html ./doku_offline/html/main.html
	cp ./img/bfg_logo.png ./doku_offline/html/logo.png
	cp ./img/qsim_logo.png ./doku_offline/html/qsim.png
	cp ./img/qsim_t.ico ./doku_offline/html/qsim.ico
	cp -r ./img ./doku_offline/html
	cp -r ./exp ./doku_offline/html
	cp -r ./html ./doku_offline/html
	cp -r ./pdf ./doku_offline/html
	cp -r ./taz ./doku_offline/html
	cp start.html ./doku_offline/
	@zip -r qsim_doku_offline.zip ./doku_offline/* -x doku_offline/html/taz/*.taz
	@if test -d $(ADDITIONAL); then echo "going to copy qsim_doku_offline.zip into $(ADDITIONAL)"; else echo "can't save qsim_doku_offline.zip into $(ADDITIONAL)"; exit 1 ; fi;
	@cp qsim_doku_offline.zip $(ADDITIONAL)

#	@if test -d ./dox; then echo "compressing offline Version into qsim_doku_offline.zip"; else echo "you need to execute: make dox previously "; exit 1 ; fi;
#	@zip -r qsim_doku_offline.zip ./dox -x dox/html/taz/*.taz
#	@cp qsim_doku_offline.zip $(ADDITIONAL)
#	@cp -f qsim_doku_offline.zip ./dox/html/taz
#	@echo "written qsim_doku_offline.zip to $(ADDITIONAL)"

