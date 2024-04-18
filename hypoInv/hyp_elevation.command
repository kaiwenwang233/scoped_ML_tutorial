DIS 4 50 1 3            /Main Distance weighting
RMS 4 0.16 1.5 3        /Residual weighting
ERR .10
*H71 22 2
*POS 1.8
MIN 4                  /min number of stations
ZTR 5 F                /trial depth, fix depth
*WET 1. .5 .2 .1       /weighting by pick quanlity
*PRE 3, 3 0 0 9        /magnitude
* OUTPUT
ERF T
TOP F

STA './stations_hypoinverse.dat'
LET 5 2 0                               /Net Sta Chn
TYP Read in crustal model(s):
CRH 1 './hypoInv/velocity_model_P.crh' /read crust model for Vp, here depth 0 is relative to the averge elevation of stations 
CRH 2 './hypoInv/velocity_model_S.crh' /read crust model for Vs
SAL 1 2
GEO T
PHS './hypoInv/hypoInput.arc'		        /input phase file

FIL				        /automatically set phase format from file
ARC './hypoInv/hypoOut.arc'		/output archive file
PRT './hypoInv/prtOut.prt'		/output print file
SUM './hypoInv/catOut.sum'        /output location summary
*RDM T
CAR 1
*LST 2
LOC				/locate the earthquake
STO
