mono FsLexYacc.9.0.2/build/fsyacc/net46/fsyacc.exe --module "JanusParser" JanusParser.yy
mono FsLexYacc.9.0.2/build/fsyacc/net46/fsyacc.exe --module "BOBParser" BOBParser.yy
fsharpc test_compiler.fsx --standalone

mono --assembly-loader=legacy test_compiler.exe fail/type1
mono --assembly-loader=legacy test_compiler.exe fail/type2
mono --assembly-loader=legacy test_compiler.exe fail/type3
mono --assembly-loader=legacy test_compiler.exe fail/type4
mono --assembly-loader=legacy test_compiler.exe fail/type5

mono --assembly-loader=legacy test_compiler.exe fail/if1
mono --assembly-loader=legacy test_compiler.exe fail/loop1
mono --assembly-loader=legacy test_compiler.exe fail/local1
mono --assembly-loader=legacy test_compiler.exe fail/local2
