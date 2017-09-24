all:
	#happy -gca Parcalc.y
	#alex -g Lexcalc.x
	#latex Doccalc.tex; dvips Doccalc.dvi -o Doccalc.ps
	ghc --make program.hs -o interpret
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f Doccalc.ps
distclean: clean
	-rm -f Doccalc.* Lexcalc.* Parcalc.* Layoutcalc.* Skelcalc.* Printcalc.* Testcalc.* Abscalc.* Testcalc ErrM.* SharedString.* calc.dtd XMLcalc.* Makefile*

