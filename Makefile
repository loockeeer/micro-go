EXE=_build/default/mgoc.exe
ZIP=ROOY.zip
all: $(EXE)

zip:
	zip -r "$(ZIP)" . -x "$(ZIP)" "_build/*" ".git/*"

$(EXE): *.ml*
	dune build @all

test: $(EXE) tests/test.go
	-./$(EXE) --parse-only tests/test.go

.PHONY: clean
clean:
	dune clean
	rm -f *~ tests/*~
	rm $(ZIP)
