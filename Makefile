# ---------------- configuration ----------------------

# if you have multiple SWI Prolog installations or an installation
# in a non-standard place, set PLLD to the appropriate plld invokation, eg
# PLLD=/usr/local/bin/plld -p /usr/local/bin/swipl

PACKNAME=switex
VER=0.0.2
PACKFILE=release/$(PACKNAME)-$(VER).tgz
# ---------------- end of configuration ---------------

main: 
	make -C c

packdir:
	mkdir -p $(PACKNAME) $(PACKNAME)/prolog 
	mkdir -p $(PACKNAME) $(PACKNAME)/bin 
	mkdir -p $(PACKNAME) $(PACKNAME)/tex 
	sed -e "s/<VER>/$(VER)/g" < pack.pl | sed -e "s/<PACKNAME>/$(PACKNAME)/g" > $(PACKNAME)/pack.pl
	rsync -ar --delete --exclude '.*' prolog $(PACKNAME)
	rsync -ar --delete --exclude '.*' bin $(PACKNAME)
	rsync -ar --delete --exclude '.*' tex $(PACKNAME)
	cp -p README.md $(PACKNAME)

pack: packdir
	tar czf $(PACKFILE) $(PACKNAME)
	rm -rf $(PACKNAME)
	git add $(PACKFILE)

install: pack
	swipl -g "pack_install('$(PACKFILE)'), halt"

install-git: install
	swipl -g "pack_property($(PACKNAME),download(D)), pack_install(D), halt"

dist-clean:
	echo 'Nothing to do'
