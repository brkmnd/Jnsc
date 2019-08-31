mono FsLexYacc.9.0.2/build/fsyacc/net46/fsyacc.exe --module "JanusParser" JanusCompiler/JanusParser.yy
fsharpc test_compiler.fsx --standalone
PRGS="fail/if1"
PRGS="$PRGS fail/local1"
PRGS="$PRGS fail/local2"
PRGS="$PRGS fail/local3"
PRGS="$PRGS fail/loop1"
PRGS="$PRGS fail/type1"
PRGS="$PRGS fail/type2"
PRGS="$PRGS fail/type3"
PRGS="$PRGS fail/type4"
PRGS="$PRGS fail/type5"
PRGS="$PRGS fail/type6"
PRGS="$PRGS fail/type7"
RUN="mono --assembly-loader=legacy test_compiler.exe $PRGS"
$RUN
#will print errors in terminal
#firefox sim.html
