#!/bin/csh

#judge typhoon HRA based on HK2022

set MODE = 3 #1 CE1 only #CE2 only #CE1 and CE2
set THS = '100-80'
set ELID = 1 #1 linear-stationary 2 linear 3 stationary 4 else
set YST = 2000
set MST = 4
set DST = 1
set YEN = 2000
set MEN = 10
set DEN = 31
set AREA = 'nihonkai'

set ELEM = ('linear-stationary' 'linear' 'stationary' 'else')

set CMST = `printf "%02d" $MST`
set CMEN = `printf "%02d" $MEN`
set CDST = `printf "%02d" $DST`
set CDEN = `printf "%02d" $DEN`
set DATE = $YST$CMST$CDST'-'$CMEN$CDEN

set DATDIR = /mnt/jet12/makoto/extract_senjo/ext_sun_re/csv
set CSVDIR = {$DATDIR}/{$THS}/{$AREA}'_'{$THS}'_25.csv'
set RADIR = {$DATDIR}/rastat/${DATE}/$ELEM[$ELID]'_hras_stat_'{$THS}'_040.csv'
set TCDIR = /mnt/jet12/makoto/extract_senjo/environment/plot/py/data/Besttrack/bst_all.txt

echo "CSVDIR = $CSVDIR"
echo "RADIR  = $RADIR"
echo "TCDIR  = $TCDIR"

unlink fort.100
unlink fort.200
unlink fort.300

#CSV --> dat
ln -s $CSVDIR 'fort.100'
ln -s $RADIR 'fort.200'
ln -s $TCDIR 'fort.300'

#execute
./judgeTD_tool 






