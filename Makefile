all: manual

.PHONY: manual
manual:
	sphinx-build -M html manual build/manual
