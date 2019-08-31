mono FsLexYacc.9.0.2/build/fsyacc/net46/fsyacc.exe --module "JanusParser" JanusCompiler/JanusParser.yy
fsharpc --target:library -r FsLexYacc.Runtime.9.0.2/lib/net46/FsLexYacc.Runtime.dll JanusCompiler/JanusAbSyn.fs JanusCompiler/JanusParser.fs JanusCompiler/JanusLexer.fs JanusCompiler/JanusTypeChecker.fs JanusCompiler/JanusCompilerBob.fs JanusCompiler/Jnsc.fs --standalone
