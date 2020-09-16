pdfs := minicheck-ja.pdf minicheck-en.pdf

all: $(pdfs)

%.pdf: %.tex %.bib
	cluttex -exelatex --shell-escape --bibtex=bibtex --package-support=minted $<

clean:
	rm -f $(pdfs)
