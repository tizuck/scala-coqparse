# scala-coqparse

An experimental coq parser implemented in scala using the [Parseback](https://github.com/djspiewak/parseback) Framework. 

## How to run?

For a parsing example:
- Install [Sbt](https://www.scala-sbt.org/).
- Run "sbt clean compile" in the top level directory of this project.
- Run "sbt run" in the top level directory of this project.

This will run the example provided in Main.scala. Here, two Notations are added to the core language of coq 
and an exemplary coq expression containing the symbols from the Notations is parsed.
