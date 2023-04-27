#!/bin/sh

#set path to NtHorn here
tool=/Users/bishoksank/Research/progamAnalysis/softwares/NtHorn/src/nthorn
#set of files to run are taken as the first argument
Files=$1

#please provide names for result files
tmp=result_non-termination.txt
out=res_nt-Horn.txt

#for JSON output
printf "{
    "stats": {" >>$out

START_G=$(date +%s000)
for file in $Files/*.pl
do
  filename=$(basename "$file")
if [[ "$filename" == "*.c.map.t.pl" ]]; then #$file =~ \.gz$
    echo "passed here"
    filename="${filename%.c.map.t.pl}"
  else
   filename="${filename%.c.pl}"
  fi

  printf $filename
  printf "$filename, " >>$out

  START_L=$(date +%s000)
  #configuration cs
  #gtimeout 5m $tool $file

  #configuration cs.pe.clssplit
  gtimeout 5m $tool -pe -clssplit $file

  #configuration cs.pe
  #gtimeout 5m $tool -pe  $file
status=$?
  END_L=$(date +%s000)
  DIFF_L=$(( $END_L - $START_L ))
#parse result
  nt_stat=$(sed -E 's/.*precond-safe\(([a-z-]+).*$/\1/' $tmp)
  if [ $status -eq 124 ]; then
    printf  "TIMEOUT, ">>$out
    printf  "100000\n">>$out
  else
    printf  "$nt_stat, " >> $out
    printf  "$DIFF_L\n">>$out
  fi
done

END_G=$(date +%s000)
DIFF_G=$(( $END_G - $START_G ))
printf "},
    "preamble": {
        "benchmark": "my-benchmark-set",
        "prog_args": "-a hello --arg2 world -v",
        "program": "very_nice_solver",
        "prog_alias": "vns"
    }
}" >>$out
#printf  "Total time (ms): $DIFF_G\n" >>$out

#rm $tmp
