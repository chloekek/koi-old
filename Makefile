all: koi manual

.PHONY: koi
koi:
	mkdir --parents build/koi
	cd build/koi && mmc -O6 ../../src/koi.m

.PHONY: manual
manual:
	sphinx-build -M html manual build/manual
