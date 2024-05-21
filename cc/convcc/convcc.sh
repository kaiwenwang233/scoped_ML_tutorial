#!/bin/sh
# convert original correl output to hypoDD format: cc to dt.cc nP 
#
# run: 
# ./convcc.sh dir pha.all.cc pha.all nP 

# dir: 		directory with original correl output files
#		Format: 
#		   Filename: NET.STA.cc.COMP.PHA (e.g. IV.SSFR.cc.HNE.S)
#		   Content: lines of ID1 ID2 DT CC
# pha.all.cc: 	phase file used for cc delay time measurements
# pha.all: 	phase file to be comobined with cc measurements
# nP: 		# of min P-wave delay times required to retain link 
#		between event pair 

# Uses following codes: add_nphaPS.f, OTcorr.f, reformat_dtcc_sorted.f
# compile with: 
# gfortran add_nphaPS.f -o add_nphaPS
# gfortran OTcorr.f -o OTcorr  
# gfortran reformat_dtcc_sorted.f -o reformat_dtcc_sorted 

echo "convert cc to dt.cc..."

#--- collect P delay times from station files:
ls $1/*.P > filesP
cp filesP tmp
sed -i 's/\// /g' tmp 
sed -i 's/\./ /g'  tmp
awk '{print $2 $3}' tmp > stationsP;  rm tmp
paste stationsP filesP > tmp
awk '{print "awk ",$1," P ",$2}' tmp > cmdP
sed -i 's/awk/awk '"'"'{print $0\,\"  /g' cmdP 
sed -i 's/ P /  P \"}'"'"' /g' cmdP
sed -i 's/Z\.P/Z\.P >> ccP/g' cmdP

#--- collect S delay times from station files:
ls $1/*.S > filesS
cp filesS tmp
sed -i 's/\// /g' tmp 
sed -i 's/\./ /g' tmp
awk '{print $2 $3}' tmp > stationsS; rm tmp 
paste stationsS filesS > tmp
awk '{print "awk ",$1," S ",$2}' tmp > cmdS
sed -i 's/awk/awk '"'"'{print $0\,\"  /g' cmdS 
sed -i 's/ S /  S \"}'"'"' /g' cmdS
sed -i 's/E\.S/E\.S >> ccS/g' cmdS
sed -i 's/N\.S/N\.S >> ccS/g' cmdS

#--- cat all P- and S delay times into one file:
chmod +x cmdP cmdS; rm ccP ccS; touch ccP ccS
./cmdP; ./cmdS; cat ccP ccS > tmp
rm ccP ccS cmdP cmdS files* stations*

#--- make sure ID1<ID2:
awk '{
 	if($1>$2) {print $5,$2,$1,-$3,$4,$6;}
	else {print $5,$1,$2,$3,$4,$6;}
	}'  tmp > tmp1 

sort -k 2,2 -k 3,3 tmp1 > dtcc
rm tmp tmp1

#--- reformat:
./reformat_dtcc_sorted dtcc dt.cc.0.all
rm dtcc

#--- OT correction: 
if [ $2 != $3 ]; then
	echo "apply OT correction."; 
	awk '{if($1=="#") {print $15,$2,$3,$4,$5,$6,$7}}' $2 > memOT.dat
	./OTcorr ./$3 memOT.dat dt.cc.0.all dt.cc.1.all
	rm dt.cc.0.all
else
	echo "no OT correction."
	mv dt.cc.0.all dt.cc.1.all
fi

#--- find events that are not in phase file. 
sort -n NoPhaEv.dat | uniq | wc -l

#--- add phase counter to header line:
./add_nphaPS dt.cc.1.all dt.cc.all
rm dt.cc.1.all

#--- select pairs with at least $4 P correlations, dt.cc.P5
icount=$4
awk ' BEGIN { 
		N = '$icount';
	}
	{
	if($1=="#" && $6<N) {itake=0}
	if($1=="#" && $6>=N) {itake=1}
	if(itake==1) {print $0}
}' dt.cc.all > dt.cc.P5

###awk '{if($1=="#" && $2==2016 && $3==10 && ($4==17 || $4==18)) print $15}' pha.dat > ids.2dys

