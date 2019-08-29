mono FsLexYacc.9.0.2/build/fsyacc/net46/fsyacc.exe --module "JanusParser" JanusParser.yy
fsharpc --target:library -r FsLexYacc.Runtime.9.0.2/lib/net46/FsLexYacc.Runtime.dll JanusAbSyn.fs JanusParser.fs JanusLexer.fs JanusTypeChecker.fs JanusCompilerBob.fs Jnsc.fs --standalone
