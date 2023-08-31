DECLARE SUB Dish ()
DECLARE SUB TotalVol ()
DECLARE SUB PartVol ()

DIM i AS STRING


CLS
PRINT
PRINT
PRINT
PRINT "Press 1 for total dish volume"
PRINT
PRINT "Press 2 for partial dish volume"
PRINT
PRINT "Press 3 for dish depth"

DO UNTIL i = "1" OR i = "2" OR i = "3" OR i = "0"
 i = INKEY$
LOOP


SELECT CASE VAL(i)

CASE 1
        CLS
        CALL TotalVol
        RUN

CASE 2
        CLS
        CALL PartVol
        RUN
CASE 3
        CLS
        CALL Dish
        RUN

CASE 0
        END

END SELECT

SUB Dish '**********************************
'*  Joseph Ash Dished End Depths  *
'**********************************
DIM Di AS SINGLE
DIM Rs AS SINGLE
DIM Rn AS SINGLE
DIM Fl AS SINGLE
DIM depth AS SINGLE

CLS
INPUT "Diameter      m :- "; Di
INPUT "Spherical Rad m :- "; Rs
INPUT "Knuckle Rad  mm :- "; Rn
INPUT "Flange       mm :- "; Fl
PRINT

Rn = Rn / 1000

depth = (Rs - ((Rs - Di / 2) * (Rs + Di / 2 - 2 * Rn)) ^ .5) * 1000
PRINT USING "Dish Depth ####mm"; depth + Fl
SLEEP

END SUB

SUB PartVol

DIM PartialVolume AS SINGLE
DIM FullVolume AS SINGLE
DIM Delta AS SINGLE
DIM depth AS SINGLE
DIM Radius AS SINGLE


INPUT "Radius:- "; Radius
INPUT "Full Volume:- "; FullVolume
INPUT "Depth:- "; depth
Delta = depth / Radius

PartialVolume = .75 * FullVolume * (Delta * Delta) * (1 - Delta / 3)

PRINT USING "###.##"; PartialVolume
SLEEP




END SUB

SUB TotalVol

DIM Diameter AS SINGLE
DIM Radius AS SINGLE
DIM SphericalRadius AS SINGLE
DIM KnuckleRadius AS SINGLE
DIM KnuckleVolume AS SINGLE
DIM CapVolume AS SINGLE
DIM DishVolume AS SINGLE
DIM Dishdepth AS SINGLE
DIM StraightFlange AS SINGLE
DIM straightFlangeVolume AS SINGLE
DIM Temp AS SINGLE
DIM CapHeight AS SINGLE
CONST PI = 3.1415926536#

CLS
INPUT "Diameter:-         "; Diameter
INPUT "Spherical Radius:- "; SphericalRadius
INPUT "Knuckle Radius:-   "; KnuckleRadius
INPUT "Straight Flange:-  "; StraightFlange
INPUT "Dish Depth     :-  "; Dishdepth

KnuckleRadius = KnuckleRadius / 1000
StraightFlange = StraightFlange / 1000
Dishdepth = Dishdepth / 1000
Dishdepth = Dishdepth - KnuckleRadius - StraightFlange
Radius = Diameter * .5

'Straight Flange Volume
'***********************************************************************
 straightFlangeVolume = (PI * (Radius ^ 2)) * StraightFlange
 straightFlangeVolume = straightFlangeVolume * 1000
 PRINT USING "Flange Volume:-    ##### litres"; straightFlangeVolume
'***********************************************************************

'Knuckle Volume
'***********************************************************************
 KnuckleVolume = (PI * (Radius ^ 2)) * KnuckleRadius
 KnuckleVolume = KnuckleVolume * 1000
 PRINT USING "Knuckle Volume:-   ##### litres"; KnuckleVolume
'***********************************************************************

'Cap Volume
'***********************************************************************
 CapVolume = 1 / 6 * PI * Dishdepth * ((3 * (Radius - KnuckleRadius) ^ 2) + (Dishdepth ^ 2))
 CapVolume = CapVolume * 1000
 PRINT USING "Cap Volume:-       ##### litres"; CapVolume
'***********************************************************************

'Total Dish Depth
'***********************************************************************
 Dishdepth = Dishdepth * 1000
 DishVolume = KnuckleVolume + CapVolume + straightFlangeVolume
 PRINT USING "Dish Depth:-       ##### mm"; Dishdepth
 PRINT USING "Dish Volume:-      ##### litres"; DishVolume
 PRINT USING "Dish Volume x2     ##### litres"; DishVolume * 2
'***********************************************************************

SLEEP

END SUB

