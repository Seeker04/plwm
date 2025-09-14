debs: deb-core deb-scryer deb-swi

deb-core: src/*
	cargo deb --variant=core --no-build

deb-scryer: src/scryer/* 
	cargo deb --variant=scryer --no-build

deb-swi: src/scryer/* 
	cargo deb --variant=swi --no-build

deps-swi:
	$(MAKE) -C ../../ bin/plwm-swi

deps-scryer:
	$(MAKE) -C ../../ bin/x11plwm.so