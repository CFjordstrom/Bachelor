# Conversion and combination of different representations of regular languages

Running the tool requires .NET 7.0 or later, and can be built by running
$ make

Unit tests can be run with
$ ./run_unit_tests.sh

Property tests can be run with
$ ./run_property_tests.sh

A custom input can be run by navigating to the src folder and running
$ dotnet run <output option> <filename or regular language>

Possible output options are: -regex, -mindfa, -dfa, and -nfa. An alphabet can be provided after the output option using -alphabet <alphabet>

It can also be checked if a string is accepted by the regular language by running
$ dotnet run -run <input string> <filename or regular language>
