# variables ----------
SCRIPT_DIR="code/"

rscript:
	Rscript $(SCRIPT_DIR)$(SCRIPT)
rmarkdown:
	Rscript -e "rmarkdown::render('${RMD_FILE}', envir = new.env())"

# download raw data files -----
download-sipp:
	$(MAKE) SCRIPT="00a-download-sipp.R" rscript
download-uslt:
	$(MAKE) SCRIPT="00b-download-uslt.R" rscript
download:
	$(MAKE) download-sipp download-uslt
