for jsonl/ in *.gz; do

gunzip jsonl/*.gz
csv=$( echo $jsonl | sed 's#\.jsonl#\.csv#' )
python jltocsv.py -f datePublished -f isPartOf -f abstract -f fullText -f id $jsonl -o $csv
done


