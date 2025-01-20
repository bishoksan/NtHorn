:- bundle('NtHorn').
version('1.0').
depends([
    core-[version>='1.18'],
    chclibs,
    'github.com/ciao-lang/ciao_ppl',
    'github.com/jfmc/ciao_yices',
    'github.com/bishoksan/RAHFT'
]).
alias_paths([
    nthorn = 'src'
]).
lib('src').
cmd('src/nthorn').
