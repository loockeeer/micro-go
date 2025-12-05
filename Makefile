EXE=_build/default/mgoc.exe
ZIP=code.zip
all: $(EXE)

pdf:
	pandoc README.md -o README.pdf --pdf-engine=weasyprint

zip:
	zip -r "$(ZIP)" . -x "$(ZIP)" "_build/*" ".git/*"

$(EXE): *.ml*
	dune build @all

test-parser: $(EXE) tests/test.go
	-./$(EXE) --parse-only tests/test.go
	-./$(EXE) --parse-only tests/var.go
	-./$(EXE) --parse-only tests/min.go
	-./$(EXE) --parse-only tests/div.go
	-./$(EXE) --parse-only tests/arith.go
	-./$(EXE) --parse-only tests/point.go

test: $(EXE) tests/test.go
	-./$(EXE) tests/test.go
	-./$(EXE) tests/var.go
	-./$(EXE) tests/min.go
	-./$(EXE) tests/div.go
	-./$(EXE) tests/arith.go
	-./$(EXE) tests/point.go

.PHONY: clean
clean:
	dune clean
	rm -f *~ tests/*~
	rm $(ZIP)
	rm README.pdf
