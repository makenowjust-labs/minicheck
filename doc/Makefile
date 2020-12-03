pdf := minicheck-ja.pdf

all: $(pdf)

%.pdf: %.tex %.bib
	latexmk $<

clean:
	rm -rf $(pdf) _minted-*/ *.bbl
