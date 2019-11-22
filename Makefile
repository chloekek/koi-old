all: koi manual

.PHONY: koi
koi:
	mkdir --parents build/koi
	cd build/koi && mmc --make-short-int ../../src/*.m
	cd build/koi && mmc --make-priv-int ../../src/*.m
	cd build/koi && mmc --make-int ../../src/*.m
	cd build/koi && mmc -O6 ../../src/*.m

.PHONY: manual
manual:
	sphinx-build -M html manual build/manual
