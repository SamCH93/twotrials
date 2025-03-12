all: pdf code clean

# names of .Rnw .bib. and .bst files
FILE=est2TR
BIB=bibliography

## generate .tex file from .Rnw file with knitr
tex: $(FILE).Rnw $(BIB).bib
	Rscript -e "knitr::knit('$(FILE).Rnw')" --vanilla

## generate .pdf file from .tex file
pdf: tex
	pdflatex $(FILE)
	bibtex $(FILE)
	pdflatex $(FILE)
	pdflatex $(FILE)

## extract R code
code: $(FILE).Rnw
	Rscript -e "knitr::purl('$(FILE).Rnw')" --vanilla

## install twotrials package
packageinstall: package/out/twotrials_0.05.tar.gz
	R CMD INSTALL package/out/twotrials_0.05.tar.gz

## clean
clean:  
	-rm $(FILE).aux  $(FILE).blg  $(FILE).log  $(FILE).tex  $(FILE).bbl \
	$(FILE).out $(FILE).brf $(FILE).tex $(FILE).synctex.gz
	-rm -r ./figure/
