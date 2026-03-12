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
set AREA = 'taiheiyou'

set ELEM = ('linear-stationary' 'linear' 'stationary' 'else')

set CMST = `printf "%02d" $MST`
set CMEN = `printf "%02d" $MEN`
set CDST = `printf "%02d" $DST`
set CDEN = `printf "%02d" $DEN`
set DATE = $YST$CMST$CDST'-'$CMEN$CDEN

set DATDIR = /mnt/jet12/makoto/extract_senjo/ext_sun_re/csv
set CSVDIR = ${DATDIR}/${THS}/${AREA}'_'${THS}'_25.csv'
#set RADIR = {$DATDIR}/rastat/${DATE}/$ELEM[$ELID]'_hras_stat_'{$THS}'_040.csv'
set TCDIR = /mnt/jet12/makoto/extract_senjo/environment/plot/py/data/Besttrack/bst_all.txt

echo "CSVDIR = $CSVDIR"
#echo "RADIR  = $RADIR"
#echo "TCDIR  = $TCDIR"

unlink fort.100
unlink fort.200
unlink fort.300
unlink fort.400

#CSV --> dat (const only)
ln -s $CSVDIR 'fort.100'
ln -s $TCDIR 'fort.300'

#fort.400 05d3 file
set DATDIR_HRA = /mnt/jet12/makoto/extract_senjo/ext_sun_re/csv/${THS}/05d3/${THS}
set DETAIL_BASENAME = ra03_5000m_recollect_${THS}_040.csv

#fort.200 hra stat file
set DATDIR2 = /mnt/jet12/makoto/extract_senjo/ext_sun_re/csv/rastat
set DETAIL_BASENAME2 = linear-stationary_hras_stat_${THS}_040.csv

#case file
set CASEFILE = fort.100

# dtst 
set DTST_COL = 3

#rm fort link(init) 
rm -f fort.400
rm -f fort.200

# year all list of hra file
set YEARS = (`awk -F, -v col=$DTST_COL 'NR>1 {print substr($col,1,4)}' $CASEFILE | sort -u`)

echo "YEARS = $YEARS"

foreach YEAR ($YEARS)

    set DATE = ${YEAR}0401-1031
    set HRAFILE = ${DATDIR_HRA}/${DATE}/${DETAIL_BASENAME}

    if (! -e "$HRAFILE") then
        echo "skip: file not found -> $HRAFILE"
        continue
    endif

    set DATE = ${YEAR}0401-1031
    set RASTATFILE = ${DATDIR2}/${DATE}/${DETAIL_BASENAME2}

    if (! -e "$RASTATFILE") then
        echo "skip: file not found -> $RASTATFILE"
        continue
    endif

    rm -f fort.400
    ln -s "$HRAFILE" fort.400

    rm -f fort.200
    ln -s "$RASTATFILE" fort.200

    echo "======================================="
    echo "YEAR    = $YEAR"
    echo "fort.400 -> $HRAFILE"
    echo "fort.200 -> $RASTATFILE"

    #execute
    ./judgeTD_tool $YEAR

end

