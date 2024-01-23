all:
	dotnet build src

clean:
	rm -rf src/bin src/obj
	rm -f src/Parser.fs src/Parser.fsi src/Parser.fsyacc.output src/Lexer.fs