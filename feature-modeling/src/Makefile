all:
	happy -gca FeatureModel/Parsers/SXFM/ParSXFM.y
	alex -g FeatureModel/Parsers/SXFM/LexSXFM.x
	(cd FeatureModel/Parsers/SXFM/; latex DocSXFM.tex; dvips DocSXFM.dvi -o DocSXFM.ps)
	ghc --make FeatureModel/Parsers/SXFM/TestSXFM.hs -o FeatureModel/Parsers/SXFM/TestSXFM
clean:
	-rm -f FeatureModel/Parsers/SXFM/*.log FeatureModel/Parsers/SXFM/*.aux FeatureModel/Parsers/SXFM/*.hi FeatureModel/Parsers/SXFM/*.o FeatureModel/Parsers/SXFM/*.dvi
	-rm -f FeatureModel/Parsers/SXFM/DocSXFM.ps
distclean: clean
	-rm -f FeatureModel/Parsers/SXFM/DocSXFM.* FeatureModel/Parsers/SXFM/LexSXFM.* FeatureModel/Parsers/SXFM/ParSXFM.* FeatureModel/Parsers/SXFM/LayoutSXFM.* FeatureModel/Parsers/SXFM/SkelSXFM.* FeatureModel/Parsers/SXFM/PrintSXFM.* FeatureModel/Parsers/SXFM/TestSXFM.* FeatureModel/Parsers/SXFM/AbsSXFM.* FeatureModel/Parsers/SXFM/TestSXFM FeatureModel/Parsers/SXFM/ErrM.* FeatureModel/Parsers/SXFM/SharedString.* FeatureModel/Parsers/SXFM/SXFM.dtd FeatureModel/Parsers/SXFM/XMLSXFM.* Makefile*
	-rmdir -p FeatureModel/Parsers/SXFM/
