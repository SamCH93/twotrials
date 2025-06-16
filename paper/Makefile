# names
FILE=est2TR
BIB=bibliography

all: $(FILE).pdf code clean
.PHONY: all clean packageinstall

## generate .tex file from .Rnw file with knitr
$(FILE).tex: $(FILE).Rnw $(BIB).bib
	Rscript -e "knitr::knit('$(FILE).Rnw')" --vanilla

## generate .pdf file from .tex file
$(FILE).pdf: $(FILE).tex
	pdflatex $(FILE)
	bibtex $(FILE)
	pdflatex $(FILE)
	pdflatex $(FILE)

## extract R code
$(FILE).R: $(FILE).Rnw
	Rscript -e "knitr::purl('$(FILE).Rnw')" --vanilla

## install twotrials package
packageinstall: ../package/out/twotrials_0.6.tar.gz
	R CMD INSTALL ../package/out/twotrials_0.6.tar.gz

## use texdiff to show difference to previous version
$(FILE)-changes.pdf: $(FILE).tex old/$(FILE)-v1.tex old/ld.cfg
	-latexdiff -c old/ld.cfg --graphics-markup=none old/$(FILE)-v1.tex $(FILE).tex > $(FILE)-changes.tex
	-pdflatex --interaction nonstopmode $(FILE)-changes
	-bibtex $(FILE)-changes
	-pdflatex --interaction nonstopmode $(FILE)-changes
	-pdflatex --interaction nonstopmode $(FILE)-changes

## clean
clean:  
	-rm $(FILE).aux  $(FILE).blg  $(FILE).log  $(FILE).tex  $(FILE).bbl \
	$(FILE).out $(FILE).brf $(FILE).tex $(FILE).synctex.gz $(FILE).pag \
	$(FILE)-changes.aux  $(FILE)-changes.blg  $(FILE)-changes.log \
	$(FILE)-changes.tex  $(FILE)-changes.bbl $(FILE)-changes.out \
	$(FILE)-changes.brf $(FILE)-changes.synctex.gz $(FILE)-changes.pag
	-rm -r ./figure/
