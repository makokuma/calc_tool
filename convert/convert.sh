#!/bin/sh
#wgrib2ツールを使って、各変数（全レベル）をバイナリファイルとしてはき出す

var="UGRD"

#data path
#YYYY=2000
for YYYY in $(seq 2000 2022);do

    DIR="${YYYY}"
#    var="TMP"
    outdir="../data/5km/$DIR/$var"
    [ -d "$outdir" ] || mkdir -p "$outdir"
    
    tmpdir="${outdir}/_tmp"
    mkdir -p "$tmpdir"
    trap 'rm -f "${tmpdir}"/*.grb2 2>/dev/null' EXIT

    for MM in 01 02 03 04 05 06 07 08 09 10 11 12
        do
            case $MM in
            01|03|05|07|08|10|12)  DDS=31 ;;
            04|06|09|11)           DDS=30 ;;
            02)
                if [ $((YYYY % 400)) -eq 0 ] || { [ $((YYYY % 4)) -eq 0 ] && [ $((YYYY % 100)) -ne 0 ]; }; then
                DDS=29
                else 
                DDS=28
                fi
                ;;
            esac

            for DD in $(seq -w 1 $DDS) 
            do

            for HH in 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 
            do

                #data path for RRJdata
                case $MM in
                07|08|09|10|11|12)
                ifile="/mnt/hail1/regional_RA/GRIB2/prs/${YYYY}/ctrl/fcst_prs_${YYYY}${MM}${DD}${HH}00.grib2" ;;

                01|02|03|04|05|06)
                YYYY1=$((YYYY-1))
                ifile="/mnt/hail1/regional_RA/GRIB2/prs/${YYYY1}/ctrl/fcst_prs_${YYYY}${MM}${DD}${HH}00.grib2" ;;
                esac
        
                #skip
                [ -f "$ifile" ] || continue

#                echo "CHECK: $ifile"
#                if [ ! -f "$ifile" ]; then
#                    echo "MISSING: $ifile"
#                    continue
#                fi
        
                #output 
                ofile="${outdir}/fcat_prs_${var}_${YYYY}${MM}${DD}${HH}.bin"
                rm -f "$ofile"

                #inv="${outdir}/fcst_prs_${var}_${YYYY}${MM}${DD}${HH}00.inv"
#                tmpgrb="$(mktemp "${tmpdir}/${var}_${YYYY}${MM}${DD}${HH}.XXXXXX.grb2")"

                #choise var and convert
                wgrib2 "$ifile" \
                -match ":${var}:" \
                -no_header -bin "$ofile" \
                -inv /dev/null \
                > /dev/null 2>&1 || { rm -f "$ofile"; continue; }

                [ -s "$ofile" ] || rm -f "$ofile"
#                echo "converted grib --> bin ${YYYY}${MM}${DD}${HH}"
#                rm $inv
            done
            done
        done

done

