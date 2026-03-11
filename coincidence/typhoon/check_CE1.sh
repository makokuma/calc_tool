#!/bin/csh

set CASEFILE = fort.100
set DATDIR = /mnt/jet12/makoto/extract_senjo/ext_sun_re/csv/100-80/05d3/100-80
set DETAIL_BASENAME = ra03_5000m_recollect_100-80_040.csv

# dtst 列番号（1始まり）
set DTST_COL = 3

# fort link を掃除
rm -f fort.400

# year 一覧を作る
set YEARS = (`awk -F, -v col=$DTST_COL '{print substr($col,1,4)}' $CASEFILE | sort -u`)

echo "YEARS = $YEARS"

foreach YEAR ($YEARS)

    set DATE = ${YEAR}0401-1031
    set HRAFILE = ${DATDIR}/${DATE}/${DETAIL_BASENAME}

    if (! -e "$HRAFILE") then
        echo "skip: file not found -> $HRAFILE"
        continue
    endif

    rm -f fort.400
    ln -s "$HRAFILE" fort.400

    echo "======================================="
    echo "YEAR    = $YEAR"
    echo "fort.400 -> $HRAFILE"

    ./check_CE1_tool $YEAR

end
