PDFLATEX = pdflatex
BIBTEX = bibtex
OTT = ott
OTT_FLAGS := -tex_wrap false -tex_show_meta true -picky_multiple_parses false
SKIM = skim_revert.sh

all: pdf
  # This is for my private machine.  It forces my PDF reader to reload.
  # It should not run unless "skim_revert.sh" is in your PATH.
  ifeq ($(SKIM), skim_revert.sh)
	$(SKIM) $(CURDIR)/main.pdf
	$(SKIM) $(CURDIR)/main.pdf
  endif

ipad : all
	cp main.pdf ~/Dropbox/AdjointProgDraft.pdf

pdf : main.pdf

pdf-report : report.pdf

main-src-output.tex : main-src.tex TNT.ott ref.bib
	$(OTT) $(OTT_FLAGS) -i TNT.ott -o TNT-ott.tex -tex_name_prefix TNT \
		-tex_filter main-src.tex main-src-output.tex

main.pdf : main.tex main-src-output.tex Makefile
	$(PDFLATEX) -jobname=main main.tex
	$(BIBTEX) main
	$(PDFLATEX) -jobname=main main.tex
	$(PDFLATEX) -jobname=main main.tex
	$(PDFLATEX) -jobname=main main.tex

report.pdf : report.tex main-src-output.tex Makefile
	$(PDFLATEX) -jobname=report report.tex
	$(BIBTEX) report
	$(PDFLATEX) -jobname=report report.tex
	$(PDFLATEX) -jobname=report report.tex
	$(PDFLATEX) -jobname=report report.tex



clean :
	rm -f *.aux *.dvi *.ps main.pdf report.pdf *.log *-ott.tex *-output.tex *.bbl *.blg *.rel *.out *~
