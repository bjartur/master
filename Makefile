all:
	$(MAKE) -C NoxPes2Csv all
	$(MAKE) -C csv-to-score all
	$(MAKE) -C score-to-nox all
	$(MAKE) -C Nox2score all
	$(MAKE) -C overlap all
	$(MAKE) -C plot all
demo:
	$(MAKE) -C NoxPes2Csv demo
	$(MAKE) -C csv-to-score demo
.PHONY: demo all
