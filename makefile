# makefile for LH workshop presentation, 2020-05-19

SRC = ./src
OUT = ./output
DATA = ./data

.PHONY: all

all: LH_pres.html images/animation.gif images/monthly_dtc.gif images/overall.png

# entire time bubble plot of detections
images/overall.png: $(SRC)/full_series.R $(DATA)/Lec.gpkg $(DATA)/huron_lld.tif $(DATA)/GLATOS_receiverLocations_20200109_170415.csv $(DATA)/LECCI_detectionsWithLocs_20200109_170952.csv
	r $<

# create animation- all fish (animation.R)
images/animation.gif: $(SRC)/animation.R $(DATA)/Lec.gpkg $(DATA)/huron_lld.tif $(DATA)/GLATOS_receiverLocations_20200109_170415.csv $(DATA)/LECCI_detectionsWithLocs_20200109_170952.csv
	r $<

# monthly animation of detections via bubble plots
images/monthly_dtc.gif: $(SRC)/bubble_plot2.R $(DATA)/Lec.gpkg $(DATA)/huron_lld.tif $(DATA)/GLATOS_receiverLocations_20200109_170415.csv $(DATA)/LECCI_detectionsWithLocs_20200109_170952.csv
	r $<

# presentation (LH_workshop_xar_pres.Rmd)
LH_pres.html: LH_workshop_xar_pres.Rmd $(wildcard images/*) LH_wkshp.css images/monthly_dtc.gif images/animation.gif images/overall.png
	r -e "rmarkdown::render('LH_workshop_xar_pres.Rmd', output_format = 'xaringan::moon_reader', output_dir = NULL, output_file = 'LH_pres.html')"


.PHONY : clean
clean:
	rm $(OUT)/*

