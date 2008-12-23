copy .\img\*.eps .\build\img /Y
lhs2tex rbonifacioAOSD.tex > rbonifacio.tex
latex rbonifacio.tex -output-directory=.\build\
bibtex .\build\rbonifacio
latex rbonifacio.tex -output-directory=.\build\
latex rbonifacio.tex -output-directory=.\build\
dvips -t a4 -Ppdf -GO -o .\build\rbonifacio.ps .\build\rbonifacio.dvi
ps2pdf .\build\rbonifacio.ps .\build\rbonifacioAOSD.pdf
