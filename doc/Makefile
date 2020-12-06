pdf := minicheck-ja.pdf

all: $(pdf)

%.pdf: %.tex %.bib
	latexmk $<

clean:
	latexmk -C
	rm -rf $(pdf) _minted-*/ *.bbl
