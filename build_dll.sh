mono FsLexYacc.9.0.2/build/fsyacc/net46/fsyacc.exe --module "JanusParser" src/JanusParser.yy
fsharpc --target:library -r FsLexYacc.Runtime.9.0.2/lib/net46/FsLexYacc.Runtime.dll src/JanusAbSyn.fs src/JanusParser.fs src/JanusLexer.fs src/JanusTypeChecker.fs src/JanusCompilerBob.fs src/Jnsc.fs --standalone
