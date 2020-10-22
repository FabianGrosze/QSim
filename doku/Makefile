#====================================================================
# Makefile zur Erstellung des 
# QSim Dokumentationsportals
# wyrwa seit 27aug20
#====================================================================

CONFIG=./confi
INSTALL=/home/Wyrwa/public_html/dokuport

#------------------------------------------------------------- -------

usual: check date doc
 
check:
	@if test -d $(INSTALL); then echo "Installing Documentation into $(INSTALL) "; else echo "can't install into $(INSTALL)"; exit 1 ; fi;
	@if test -f $(CONFIG); then echo "expecting configuration in $(CONFIG)"; else echo "$(CONFIG) missing"; exit 1 ; fi;
	@if test -d "$$MY_ARC"; then echo "Directory MY_ARC=$(MY_ARC) existing"; else echo "archive directory missing"; exit 1 ; fi;
	@if test -d ./img ; then echo "image directory existing"; else echo "image directory missing, get qsim_doku.taz !"; exit 1 ; fi;

HIER=`pwd -P`
JETZT=`date +"%-d%m%Y"`
date: 
	@echo "jetzt $(JETZT) hier $(HIER);"

clean:	
	@echo "cleaning ..."
	rm -rf ./dox ./exp ./html ./img ./pdf ./qsim_doku.taz ./taz

FD=qsim_doku_`date +%d%b%y`.taz

save:
	@/bin/rm -f qsim_doku*.taz
	@tar cvfz $(FD) ./img ./exp ./html ./pdf ./taz
	@cp $(FD) $(MY_ARC)
	@echo "saved $(FD) to $(MY_ARC)"

doc:	
	doxygen $(CONFIG)
	cp ./dox/html/index.html ./dox/html/main.html
	cp ./img/bfg_logo.png ./dox/html/logo.png
	cp ./img/qsim_logo.png ./dox/html/qsim.png
	cp ./img/qsim_t.ico ./dox/html/qsim.ico
	cp -r ./img ./dox/html
	cp -r ./exp ./dox/html
	cp -r ./html ./dox/html
	cp -r ./pdf ./dox/html
	cp -r ./taz ./dox/html
	cp qsim_doku.taz ./dox/html/taz/qsim_doku.taz
	rsync -ua ./dox/html/* $(INSTALL)
	find $(INSTALL) -type d -exec chmod 755 {} \;
	find $(INSTALL) -type f -exec chmod 644 {} \;

