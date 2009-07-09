all:
	happy -gca ComponentModel/Parsers/ParComponentModel.y
	alex -g ComponentModel/Parsers/LexComponentModel.x
	(cd ComponentModel/Parsers/; latex DocComponentModel.tex; dvips DocComponentModel.dvi -o DocComponentModel.ps)
	ghc --make ComponentModel/Parsers/TestComponentModel.hs -o ComponentModel/Parsers/TestComponentModel
clean:
	-rm -f ComponentModel/Parsers/*.log ComponentModel/Parsers/*.aux ComponentModel/Parsers/*.hi ComponentModel/Parsers/*.o ComponentModel/Parsers/*.dvi
	-rm -f ComponentModel/Parsers/DocComponentModel.ps
distclean: clean
	-rm -f ComponentModel/Parsers/DocComponentModel.* ComponentModel/Parsers/LexComponentModel.* ComponentModel/Parsers/ParComponentModel.* ComponentModel/Parsers/LayoutComponentModel.* ComponentModel/Parsers/SkelComponentModel.* ComponentModel/Parsers/PrintComponentModel.* ComponentModel/Parsers/TestComponentModel.* ComponentModel/Parsers/AbsComponentModel.* ComponentModel/Parsers/TestComponentModel ComponentModel/Parsers/ErrM.* ComponentModel/Parsers/SharedString.* ComponentModel/Parsers/ComponentModel.dtd ComponentModel/Parsers/XMLComponentModel.* Makefile*
	-rmdir -p ComponentModel/Parsers/
