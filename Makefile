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
download-fx:
	$(MAKE) SCRIPT="00c-download-fx-mort.R" rscript
download:
	$(MAKE) download-sipp download-uslt download-fx

estimate-sipp:
	$(MAKE) SCRIPT="02a-estimate-sipp-by-age.R" rscript
aggregate-uslt:
	$(MAKE) SCRIPT="02b-aggregate-uslt-by-age.R" rscript
project-matrix-model:
	$(MAKE) SCRIPT="02c-project-matrix-parentship-model.R" rscript
prep-data:
	$(MAKE) estimate-sipp aggregate-uslt project-matrix-model

fit-sipp:
	$(MAKE) SCRIPT="03-fit-linear-rw-sipp.R" rscript
	$(MAKE) SCRIPT="03-fit-linear-sipp.R" rscript
fit-sipp-and-projection:
	$(MAKE) SCRIPT="03-fit-linear-rw-sipp-and-projection.R" rscript
	$(MAKE) SCRIPT="03-fit-linear-sipp-and-projection.R" rscript
fit:
	$(MAKE) fit-sipp-and-projection fit-sipp 
