source := $(wildcard main.tex)
out := $(patsubst %.tex,%.pdf,$(source))

.PHNOY: all
all:
	@latexmk -pdf -synctex=1 $(source)
	@/mnt/d/Program\ Files/SumatraPDF/SumatraPDF.exe $(out) &

# %.pdf: $(source)
# 	@latexmk -pdf $<
# 	@latexmk -c
# 	@/mnt/c/Program\ Files\ \(x86\)/SumatraPDF/SumatraPDF.exe $(out) &

.PHNOY: show
show:
	@/mnt/d/Program\ Files/SumatraPDF/SumatraPDF.exe $(out) &

.PHNOY: clean
clean:
	@rm -rf *.fls *.aux *.log *.out *.synctex *.synctex.gz  *.bbl *.blg *.fdb_latexmk *.synctex\(busy\)