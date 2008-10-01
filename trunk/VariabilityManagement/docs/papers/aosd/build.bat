copy .\img\*.eps .\build\img /Y
lhs2tex --poly rbonifacioAOSD.tex > rbonifacio.tex
latex rbonifacio.tex -output-directory=.\build\
latex rbonifacio.tex -output-directory=.\build\

