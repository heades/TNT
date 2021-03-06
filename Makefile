PDFLATEX = pdflatex
BIBTEX = bibtex
OTT = ott
OTT_FLAGS := -tex_wrap false -tex_show_meta true -picky_multiple_parses false
SKIM = skim_revert.sh
SKIMRevinPath := $(shell command -v $(SKIM) 2> /dev/null)

all: pdf
  # This is for my private machine.  It forces my PDF reader to reload.
  # It should not run unless "skim_revert.sh" is in your PATH.
  ifdef SKIMRevinPath	
	@$(SKIM) main.pdf &>/dev/null
	@$(SKIM) main.pdf &>/dev/null
	@$(SKIM) main.pdf &>/dev/null
  endif

ipad : all
	cp main.pdf ~/Dropbox/AdjointProgDraft.pdf

quick : main.tex main-output.tex Makefile
	$(PDFLATEX) -jobname=main main-output.tex
  # This is for my private machine.  It forces my PDF reader to reload.
  # It should not run unless "skim_revert.sh" is in your PATH.
  ifdef SKIMRevinPath	
	@$(SKIM) main.pdf &>/dev/null
	@$(SKIM) main.pdf &>/dev/null
	@$(SKIM) main.pdf &>/dev/null
  endif

pdf : main.pdf

main-output.tex : main.tex TNT.ott ref.bib proofs.tex
	$(OTT) $(OTT_FLAGS) -i TNT.ott -o TNT-ott.tex -tex_name_prefix TNT \
		-tex_filter main.tex main-output.tex
	$(OTT) $(OTT_FLAGS) -i TNT.ott -o TNT-ott.tex -tex_name_prefix TNT \
		-tex_filter proofs.tex proofs-output.tex

main.pdf : main.tex main-output.tex Makefile
	$(PDFLATEX) -jobname=main main-output.tex
	$(BIBTEX) main
	$(PDFLATEX) -jobname=main main-output.tex
	$(PDFLATEX) -jobname=main main-output.tex
	$(PDFLATEX) -jobname=main main-output.tex

clean :
	rm -f *.aux *.dvi *.ps main.pdf report.pdf *.log *-ott.tex *-output.tex *.bbl *.blg *.rel *.out *~
