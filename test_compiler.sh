mono FsLexYacc.9.0.2/build/fsyacc/net46/fsyacc.exe --module "JanusParser" JanusParser.yy
mono FsLexYacc.9.0.2/build/fsyacc/net46/fsyacc.exe --module "BOBParser" BOBParser.yy
fsharpc test_compiler.fsx --standalone
: '
mono --assembly-loader=legacy test_compiler.exe if1
mono --assembly-loader=legacy test_compiler.exe if1_un
mono --assembly-loader=legacy test_compiler.exe else1
mono --assembly-loader=legacy test_compiler.exe else1_un
mono --assembly-loader=legacy test_compiler.exe if2
mono --assembly-loader=legacy test_compiler.exe if2_un
mono --assembly-loader=legacy test_compiler.exe else2
mono --assembly-loader=legacy test_compiler.exe else2_un
mono --assembly-loader=legacy test_compiler.exe ind1
mono --assembly-loader=legacy test_compiler.exe ind1_rev
mono --assembly-loader=legacy test_compiler.exe loop1
mono --assembly-loader=legacy test_compiler.exe loop1_un
mono --assembly-loader=legacy test_compiler.exe loop2
mono --assembly-loader=legacy test_compiler.exe loop2_un
mono --assembly-loader=legacy test_compiler.exe local1
mono --assembly-loader=legacy test_compiler.exe local1_un
mono --assembly-loader=legacy test_compiler.exe local2
mono --assembly-loader=legacy test_compiler.exe local2_un
mono --assembly-loader=legacy test_compiler.exe loop1
mono --assembly-loader=legacy test_compiler.exe loop1_un
mono --assembly-loader=legacy test_compiler.exe loop3_un
mono --assembly-loader=legacy test_compiler.exe loop3
mono --assembly-loader=legacy test_compiler.exe loop4
mono --assembly-loader=legacy test_compiler.exe loop4_un
mono --assembly-loader=legacy test_compiler.exe perm2code
mono --assembly-loader=legacy test_compiler.exe perm2code_un
mono --assembly-loader=legacy test_compiler.exe factor
mono --assembly-loader=legacy test_compiler.exe factor_un
mono --assembly-loader=legacy test_compiler.exe fib1
mono --assembly-loader=legacy test_compiler.exe fib1_un
mono --assembly-loader=legacy test_compiler.exe sqrroot
mono --assembly-loader=legacy test_compiler.exe sqrroot_un
mono --assembly-loader=legacy test_compiler.exe runlength
mono --assembly-loader=legacy test_compiler.exe runlength_un
mono --assembly-loader=legacy test_compiler.exe args1
'
mono --assembly-loader=legacy test_compiler.exe perm2code
firefox sim.html
