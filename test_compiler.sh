mono FsLexYacc.9.0.2/build/fsyacc/net46/fsyacc.exe --module "JanusParser" JanusParser.yy
fsharpc test_compiler.fsx --standalone
PRGS="if1 if1_un"
PRGS="$PRGS else1 else1_un "
PRGS="$PRGS if2 if2_un"
PRGS="$PRGS else2 else2_un"
PRGS="$PRGS ind1 ind1_rev"
PRGS="$PRGS loop1 loop1_un"
PRGS="$PRGS loop2 loop2_un"
PRGS="$PRGS local1 local1_un"
PRGS="$PRGS local2 local2_un"
PRGS="$PRGS loop3 loop3_un"
PRGS="$PRGS loop4 loop4_un"
PRGS="$PRGS perm2code perm2code_un"
PRGS="$PRGS factor factor_un"
PRGS="$PRGS fib1 fib1_un"
PRGS="$PRGS sqrroot sqrroot_un"
PRGS="$PRGS runlength runlength_un"
PRGS="$PRGS args1"
RUN="mono --assembly-loader=legacy test_compiler.exe $PRGS"
$RUN
firefox sim.html
