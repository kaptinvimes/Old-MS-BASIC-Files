'**************************************************************************                                                  �
'*   title: Gauge Calibration version 5.4                                 *
'*                                                                        *
'*                                                                        *
'*   last amended:11/11/05 (c) dcw 1990, 1999, 2000, 2001,2003,2004,2005  *
'**************************************************************************
'**************************************************************************
'*   Constants                                                            *
'**************************************************************************
CONST false = 0
CONST True = NOT false
CONST pi = 3.1415927#
CONST MillibarConversion = 2.4908
'**************************************************************************
'*     External Functions & Procedures                                    *
'**************************************************************************
DECLARE FUNCTION DosVersion$ ()
DECLARE FUNCTION findfile$ (FileSpec$)
DECLARE FUNCTION GetFlag% (FLAG%)
DECLARE FUNCTION Page% (NewPage%)
DECLARE FUNCTION PrinterStat% (printer%)
DECLARE FUNCTION StatusLine% (MEssage$)
DECLARE FUNCTION Verify% (row%, MEssage$)
DECLARE FUNCTION Attribute% (Fore%, Back%)
DECLARE FUNCTION DateInput$ (D$)
DECLARE FUNCTION RevInput$ (mx%, a$)
DECLARE FUNCTION numcheck$ (BF$, numset$)
DECLARE FUNCTION LongDate$ (Day%, Month%, Year%)
DECLARE SUB aprint (row%, col%, MEssage$, Attrib%)
DECLARE SUB delay (Interval!)
DECLARE SUB EraEos ()
DECLARE SUB HelpMate (Context%, Topic$)
DECLARE SUB Panel2 (row%, col%, Rows%, Cols%, Attrib%)
DECLARE SUB PopUp (Y%, x%, H%, W%, c%, B%, S%, Z%)
DECLARE SUB printeps (printer%)
DECLARE SUB ScrollDown (TY%, TX%, BY%, BX%, Lines%, Colour%)
DECLARE SUB ScrollUp (TY%, TX%, BY%, BX%, Lines%, Colour%)
DECLARE SUB SetFlag (FLAG%, Setting%)
DECLARE SUB shutup ()
DECLARE SUB screenprint ()
DECLARE SUB mainscreen ()
DECLARE SUB pop ()
DECLARE SUB decimalfeet ()
DECLARE SUB dialtext (text$)
DECLARE SUB unitstype (feet%, unit$, dense$)
DECLARE SUB printscreen ()
DECLARE SUB MenuDriver (row%, col%, menu%)
DECLARE SUB calibrationcode (calcode%, scale$)
DECLARE SUB WorksOrder (Wo$, Cust$, Custorder$)
DECLARE SUB getangle (analog, mFormat$, offset)

DIM Chart(100)
DIM Mark$(100)
DIM Mask$(50)
DIM increment(100)
DIM hdratio(1005)
DIM areafactor(1005)
DIM SHARED menu$(0 TO 8)
DIM SHARED Wo$

DEFDBL Z
ON ERROR GOTO Trap

FOR c% = 0 TO 100
 Mark$(c%) = " "
NEXT

'**************************************************************************
'*     Main Menu                                                          *
'**************************************************************************
menubegin:
CGA% = Page%(0): LP% = 1: DOS$ = "DOS " + DosVersion$
DY$ = MID$(DATE$, 4, 2): DY% = VAL(DY$): MO$ = LEFT$(DATE$, 2)
MO% = VAL(MO$): YR$ = RIGHT$(DATE$, 2): YR% = VAL(YR$)
Now$ = DY$ + "/" + MO$ + "/" + YR$: Today$ = LongDate$(DY%, MO%, YR%)
fcount% = 1
CLS
ME$ = " Afriso Eurogauge - East Grinstead.           Calibration System Version 5.4"
CALL mainscreen
CALL WorksOrder(Wo$, Cust$, Custorder$)
CALL shutup
CALL shutup
CALL shutup
COLOR 7, 0
'**************************************************************************
d001:
LOCATE , , 0: Context% = 0: Topic$ = ""
Panel2 1, 1, 3, 80, 30
ME$ = " Afriso Eurogauge - East Grinstead.           Calibration System Version 5.4"
aprint 2, 2, ME$, 30
'**************************************************************************
D002:
aprint 25, 1, SPACE$(80), 27: OL% = LEN(Today$)
aprint 25, 3, "Press <ESC> to Abort", 27
IF OL% > 0 THEN
 OT% = 79 - OL%: aprint 25, OT%, Today$, 27
END IF
'**************************************************************************
D003:
ScrollUp 5, 1, 20, 80, 0, 0
aprint 6, 30, "����������������Ŀ", 15
aprint 7, 30, "�   TANK  MENU   �", 15
aprint 8, 30, "������������������", 15
menu% = 6: row% = 9: col% = 23: menu$(0) = "123456"
menu$(1) = "1. Rectangular Tank.           "
menu$(2) = "2. Horizontal Cylindrical Tank."
menu$(3) = "3. Vertical Cylindrical Tank.  "
menu$(4) = "4. Capacity/Height only.       "
menu$(5) = "5. Capacity/Diameter only.     "
menu$(6) = "6. Exit from Program.          "
'**************************************************************************
D004:
MenuDriver row%, col%, menu%
IF abort% THEN GOTO D005
SELECT CASE choice%
 CASE IS = 1
tank% = 1
GOTO menu2
 CASE IS = 2
tank% = 2
fcount% = 1
ME$ = " Please Wait - Initialising ..........."
aprint 23, 20, ME$, 27
COLOR 14, 1
OPEN "hdtable.dat" FOR INPUT AS 1
DO UNTIL fcount% = 1002
 LOCATE 23, 55, 0
 PRINT 1002 - fcount%
 INPUT #1, hdratio(fcount%), areafactor(fcount%)
 fcount% = fcount% + 1
LOOP
aprint 23, 1, STRING$(80, 32), 0
CLOSE 1
GOTO menu2
 CASE IS = 3
tank% = 3
GOTO menu2
 CASE IS = 4
tank% = 4
GOTO menu2
 CASE IS = 5
tank% = 5
ME$ = " Please Wait - Initialising ..........."
aprint 23, 20, ME$, 27
COLOR 14, 1
OPEN "hdtable.dat" FOR INPUT AS 1
fcount% = 1
DO UNTIL fcount% = 1002
 LOCATE 23, 55, 0
 PRINT 1002 - fcount%
 INPUT #1, hdratio(fcount%), areafactor(fcount%)
 fcount% = fcount% + 1
LOOP
aprint 23, 1, STRING$(80, 32), 0
CLOSE 1
GOTO menu2
END SELECT
'**************************************************************************
D005:
ok% = Verify%(18, "Exit program, are you sure")
IF NOT ok% THEN GOTO D004
GOTO egress
menu2:
LOCATE , , 0: Context% = 0: Topic$ = ""
D0031:
ScrollUp 5, 1, 20, 80, 0, 0
aprint 6, 31, "���������������Ŀ", 15
aprint 7, 31, "�  GAUGE  MENU  �", 15
aprint 8, 31, "�����������������", 15
menu% = 5: row% = 9: col% = 28: menu$(0) = "12345"
menu$(1) = "1. Hydrostatic Gauge."
menu$(2) = "2. Pneumatic Gauge."
menu$(3) = "3. Analog Meter or 4-20mA."
menu$(4) = "4. Dipchart in mm "
menu$(5) = "5. Exit Program.  "
'**************************************************************************
D0041:
MenuDriver row%, col%, menu%
IF abort% THEN GOTO d001
SELECT CASE choice%
 CASE IS = 1
  gauge% = 1
 CASE IS = 2
  gauge% = 2
 CASE IS = 3
  gauge% = 3
  CALL getangle(analog, mFormat$, offset)
 CASE IS = 4
  gauge% = 4
 CASE IS = 5
  GOTO d001
END SELECT
tank$ = STR$(tank%)
gauge$ = STR$(gauge%)
cal$ = tank$ + gauge$
cal% = VAL(cal$)
COLOR , 7
SELECT CASE cal%
'**************************************************************************
CASE 11
 ME$ = "TankMate: Rectangular"
 CALL mainscreen
 CALL pop
 GOSUB RectangularCalc
 IF feet% THEN CALL shutup
 CALL calibrationcode(calcode%, scale$)
 GOSUB volume
 GOSUB convertvolume
 GOSUB getchart
 GOSUB RectangularTable
 GOSUB printchart
'**************************************************************************
CASE 12
 ME$ = "EG162: Rectangular"
 CALL mainscreen
 CALL pop
 GOSUB RectangularCalc
 IF feet% THEN CALL shutup
 CALL calibrationcode(calcode%, scale$)
 GOSUB volume
 GOSUB convertvolume
 GOSUB getchart
 GOSUB RectangularTable
 GOSUB printchart
'**************************************************************************
CASE 13
 ME$ = "Analog Meter: Rectangular"
 CALL mainscreen
 CALL pop
 GOSUB RectangularCalc
 IF feet% THEN CALL shutup
 CALL calibrationcode(calcode%, scale$)
 GOSUB volume
 GOSUB convertvolume
 GOSUB getchart
 GOSUB RectangularTable
 GOSUB printchart
'**************************************************************************
CASE 14
 ME$ = "Dipchart: Rectangular"
 CALL mainscreen
 CALL pop
 GOSUB RectangularCalc
 IF feet% THEN CALL shutup
 CALL calibrationcode(calcode%, scale$)
 GOSUB volume
 GOSUB convertvolume
 GOSUB getchart
 GOSUB RectangularTable
 GOSUB printchart
'**************************************************************************
CASE 21
 ME$ = "TankMate: Horizontal Cylinder"
 CALL mainscreen
 CALL pop
 GOSUB HorizontalCalc
 IF feet% THEN CALL shutup
 CALL calibrationcode(calcode%, scale$)
 GOSUB volume
 GOSUB convertvolume
 GOSUB getchart
 GOSUB DiameterTable
 GOSUB printchart
'**************************************************************************
CASE 22
 ME$ = "EG162: Horizontal Cylinder"
 CALL mainscreen
 CALL pop
 GOSUB HorizontalCalc
 IF feet% THEN CALL shutup
 CALL calibrationcode(calcode%, scale$)
 GOSUB volume
 GOSUB convertvolume
 GOSUB getchart
 GOSUB DiameterTable
 GOSUB printchart
'**************************************************************************
CASE 23
 ME$ = "Analog Meter: Horizontal Cylinder"
 CALL mainscreen
 CALL pop
 GOSUB HorizontalCalc
 IF feet% THEN CALL shutup
 CALL calibrationcode(calcode%, scale$)
 GOSUB volume
 GOSUB convertvolume
 GOSUB getchart
 GOSUB DiameterTable
 GOSUB printchart
'**************************************************************************
CASE 24
 ME$ = "Dipchart: Horizontal Cylinder"
 CALL mainscreen
 CALL pop
 GOSUB HorizontalCalc
 IF feet% THEN CALL shutup
 CALL calibrationcode(calcode%, scale$)
 GOSUB volume
 GOSUB convertvolume
 GOSUB getchart
 GOSUB DiameterTable
 GOSUB printchart
'**************************************************************************
CASE 31
 ME$ = "TankMate: Vertical Cylinder"
 CALL mainscreen
 CALL pop
 GOSUB VerticalCalc
 IF feet% THEN CALL shutup
 CALL calibrationcode(calcode%, scale$)
 GOSUB volume
 GOSUB convertvolume
 GOSUB getchart
 GOSUB RectangularTable
 GOSUB printchart
'**************************************************************************
CASE 32
 ME$ = "EG162: Vertical Cylinder"
 CALL mainscreen
 CALL pop
 GOSUB VerticalCalc
 IF feet% THEN CALL shutup
 CALL calibrationcode(calcode%, scale$)
 GOSUB volume
 GOSUB convertvolume
 GOSUB getchart
 GOSUB RectangularTable
 GOSUB printchart
'**************************************************************************
CASE 33
 ME$ = "Analog Meter: Vertical Cylinder"
 CALL mainscreen
 CALL pop
 GOSUB VerticalCalc
 IF feet% THEN CALL shutup
 CALL calibrationcode(calcode%, scale$)
 GOSUB volume
 GOSUB convertvolume
 GOSUB getchart
 GOSUB RectangularTable
 GOSUB printchart
'**************************************************************************
CASE 34
 ME$ = "Dipchart: Vertical Cylinder"
 CALL mainscreen
 CALL pop
 GOSUB VerticalCalc
 IF feet% THEN CALL shutup
 CALL calibrationcode(calcode%, scale$)
 GOSUB volume
 GOSUB convertvolume
 GOSUB getchart
 GOSUB RectangularTable
 GOSUB printchart
'**************************************************************************
CASE 41
 ME$ = "TankMate: Capacity/Height"
 CALL mainscreen
 CALL pop
 GOSUB Capacity
 IF feet% THEN CALL shutup
 CALL calibrationcode(calcode%, scale$)
 GOSUB convertvolume
 GOSUB getchart
 GOSUB RectangularTable
 GOSUB printchart
'**************************************************************************
CASE 42
 ME$ = "EG162: Capacity/Height"
 CALL mainscreen
 CALL pop
 GOSUB Capacity
 IF feet% THEN CALL shutup
 CALL calibrationcode(calcode%, scale$)
 GOSUB convertvolume
 GOSUB getchart
 GOSUB RectangularTable
 GOSUB printchart
'**************************************************************************
CASE 43
 ME$ = "Analog Meter: Capacity/Height"
 CALL mainscreen
 CALL pop
 GOSUB Capacity
 IF feet% THEN CALL shutup
 CALL calibrationcode(calcode%, scale$)
 GOSUB convertvolume
 GOSUB getchart
 GOSUB RectangularTable
 GOSUB printchart
'**************************************************************************
CASE 44
 ME$ = "Dipchart: Capacity/Height"
 CALL mainscreen
 CALL pop
 GOSUB Capacity
 IF feet% THEN CALL shutup
 CALL calibrationcode(calcode%, scale$)
 GOSUB convertvolume
 GOSUB getchart
 GOSUB RectangularTable
 GOSUB printchart
'**************************************************************************'**************************************************************************
CASE 51
 ME$ = "TankMate: Capacity/Diameter"
 CALL mainscreen
 CALL pop
 GOSUB Capacity
 IF feet% THEN CALL shutup
 CALL calibrationcode(calcode%, scale$)
 GOSUB convertvolume
 GOSUB getchart
 GOSUB DiameterTable
 GOSUB printchart
'**************************************************************************
CASE 52
 ME$ = "EG162: Capacity/Diameter"
 CALL mainscreen
 CALL pop
 GOSUB Capacity
 IF feet% THEN CALL shutup
 CALL calibrationcode(calcode%, scale$)
 GOSUB convertvolume
 GOSUB getchart
 GOSUB DiameterTable
 GOSUB printchart
'**************************************************************************
CASE 53
 ME$ = "Analog Meter: Capacity/Diameter"
 CALL mainscreen
 CALL pop
GOSUB Capacity
 IF feet% THEN CALL shutup
 CALL calibrationcode(calcode%, scale$)
 GOSUB convertvolume
 GOSUB getchart
 GOSUB DiameterTable
 GOSUB printchart
'**************************************************************************'**************************************************************************
CASE 54
 ME$ = "Dipchart: Capacity/Diameter"
 CALL mainscreen
 CALL pop
 GOSUB Capacity
 IF feet% THEN CALL shutup
 CALL calibrationcode(calcode%, scale$)
 GOSUB convertvolume
 GOSUB getchart
 GOSUB DiameterTable
 GOSUB printchart
'**************************************************************************

END SELECT
CLS
RUN "gauge.exe"
STOP
'**************************************************************************
'*     Capacity                                                           *
'**************************************************************************
Capacity:
CALL unitstype(feet%, unit$, dense$)
SELECT CASE cal%

'**************************************************************************
'* SS160  Height *
'*****************
CASE IS = 41
aprint 25, 1, STRING$(80, 32), 31
aprint 9, 24, "Tank Capacity     :          ", 31
aprint 10, 24, "Tank Height       :          " + unit$, 31
aprint 11, 24, "Density           :          ", 31
IF cal% = 41 THEN
aprint 13, 24, "Outlet Centreline :          " + unit$, 31
aprint 14, 24, "C/L to AG1 Flange :          " + "mm", 31
END IF
DO UNTIL abort%
LOCATE 9, 44, 0
aprint 25, 10, "Enter Tank Capacity                            ", 30
aprint 25, 60, "ESC to Abort", 30
mx% = 8
a$ = Oldcapacity$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
Oldcapacity$ = a$
Capacity = VAL(a$)
LOCATE 9, 44, 0
PRINT USING "########.##"; Capacity
IF abort% THEN EXIT DO
LOCATE 10, 44, 0
aprint 25, 10, "Enter Tank Height                              ", 30
mx% = 7
a$ = Oldheight$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
Oldheight$ = a$
height = VAL(a$)
LOCATE 10, 44, 0
PRINT USING "###.###"; height
IF abort% THEN EXIT DO
LOCATE 11, 44, 0
aprint 25, 10, "Enter Density of Contents                        ", 30
mx% = 7
a$ = olddensity$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
olddensity$ = a$
Density = VAL(a$)
LOCATE 11, 44, 0
PRINT USING "###.###"; Density
IF abort% THEN EXIT DO
LOCATE 13, 44, 0
mx% = 7
aprint 25, 10, "Enter Outlet Centreline                   ", 30
a$ = oldcentreline$
a$ = RevInput$(mx%, a$)
oldcentreline$ = a$
a$ = LTRIM$(RTRIM$(a$))
Centreline = VAL(a$)
LOCATE 13, 44, 0
PRINT USING "###.###"; Centreline
IF abort% THEN EXIT DO
LOCATE 14, 44, 0
aprint 25, 10, "Enter AG1 Flange Height from C/Line (115mm)", 30
mx% = 7
a$ = oldflange$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
oldflange$ = a$
Flange = VAL(a$)
LOCATE 14, 44, 0
PRINT USING "   ####"; Flange
IF abort% THEN EXIT DO
aprint 25, 1, STRING$(80, 32), 31
ok% = Verify%(19, "Change any entered data  ")
IF NOT ok% THEN EXIT DO
LOOP
IF abort% THEN CLEAR : RUN "gauge.exe"

'**************************************************************************
'* EG162  Height *
'*****************
CASE IS = 42
aprint 25, 1, STRING$(80, 32), 31
aprint 9, 24, "Tank Capacity     :          ", 31
aprint 10, 24, "Tank Height       :          " + unit$, 31
aprint 11, 24, "Density           :          ", 31
DO UNTIL abort%
LOCATE 9, 44, 0
aprint 25, 10, "Enter Tank Capacity                            ", 30
aprint 25, 60, "ESC to Abort", 30
mx% = 8
a$ = Oldcapacity$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
Oldcapacity$ = a$
Capacity = VAL(a$)
LOCATE 9, 44, 0
PRINT USING "########.##"; Capacity
IF abort% THEN EXIT DO
LOCATE 10, 44, 0
aprint 25, 10, "Enter Tank Height                              ", 30
mx% = 7
a$ = Oldheight$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
Oldheight$ = a$
height = VAL(a$)
LOCATE 10, 44, 0
PRINT USING "###.###"; height
IF abort% THEN EXIT DO
LOCATE 11, 44, 0
aprint 25, 10, "Enter Density of Contents                        ", 30
mx% = 7
a$ = olddensity$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
olddensity$ = a$
Density = VAL(a$)
LOCATE 11, 44, 0
PRINT USING "###.###"; Density
IF abort% THEN EXIT DO
aprint 25, 1, STRING$(80, 32), 31
ok% = Verify%(19, "Change any entered data  ")
IF NOT ok% THEN EXIT DO
LOOP
IF abort% THEN CLEAR : RUN "gauge.exe"

'**************************************************************************
'* Analog Meter Height *
'***********************
CASE IS = 43
aprint 25, 1, STRING$(80, 32), 31
aprint 9, 24, "Tank Capacity     :          ", 31
aprint 10, 24, "Tank Height       :          " + unit$, 31
DO UNTIL abort%
LOCATE 9, 44, 0
aprint 25, 10, "Enter Tank Capacity                            ", 30
aprint 25, 60, "ESC to Abort", 30
mx% = 8
a$ = Oldcapacity$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
Oldcapacity$ = a$
Capacity = VAL(a$)
LOCATE 9, 44, 0
PRINT USING "########.##"; Capacity
IF abort% THEN EXIT DO
LOCATE 10, 44, 0
aprint 25, 10, "Enter Tank Height                              ", 30
mx% = 7
a$ = Oldheight$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
Oldheight$ = a$
height = VAL(a$)
LOCATE 10, 44, 0
PRINT USING "###.###"; height
IF abort% THEN EXIT DO
aprint 25, 1, STRING$(80, 32), 31
 ok% = Verify%(19, "Change any entered data  ")
 IF NOT ok% THEN EXIT DO
LOOP
IF abort% THEN CLEAR : RUN "gauge.exe"

'**************************************************************************
'*   Dipchart Height   *
'***********************
CASE IS = 44
aprint 25, 1, STRING$(80, 32), 31
aprint 9, 24, "Tank Capacity     :          ", 31
aprint 10, 24, "Tank Height       :          " + unit$, 31
DO UNTIL abort%
LOCATE 9, 44, 0
aprint 25, 10, "Enter Tank Capacity                            ", 30
aprint 25, 60, "ESC to Abort", 30
mx% = 8
a$ = Oldcapacity$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
Oldcapacity$ = a$
Capacity = VAL(a$)
LOCATE 9, 44, 0
PRINT USING "########.##"; Capacity
IF abort% THEN EXIT DO
LOCATE 10, 44, 0
aprint 25, 10, "Enter Tank Height                              ", 30
mx% = 7
a$ = Oldheight$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
Oldheight$ = a$
height = VAL(a$)
LOCATE 10, 44, 0
PRINT USING "###.###"; height
IF abort% THEN EXIT DO
aprint 25, 1, STRING$(80, 32), 31
 ok% = Verify%(19, "Change any entered data  ")
 IF NOT ok% THEN EXIT DO
LOOP
IF abort% THEN CLEAR : RUN "gauge.exe"

'**************************************************************************
'*     SS160 Diameter  *
'***********************
CASE IS = 51
aprint 25, 1, STRING$(80, 32), 31
aprint 9, 24, "Tank Capacity     :          ", 31
aprint 10, 24, "Tank Diameter     :          " + unit$, 31
aprint 11, 24, "Density           :          ", 31
aprint 13, 24, "Outlet Centreline :          " + unit$, 31
aprint 14, 24, "C/L to AG1 Flange :          " + "mm", 31
DO UNTIL abort%
LOCATE 9, 44, 0
aprint 25, 10, "Enter Tank Capacity                            ", 30
aprint 25, 60, "ESC to Abort", 30
mx% = 8
a$ = Oldcapacity$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
Oldcapacity$ = a$
Capacity = VAL(a$)
LOCATE 9, 44, 0
PRINT USING "########.##"; Capacity
IF abort% THEN EXIT DO
LOCATE 10, 44, 0
aprint 25, 10, "Enter Tank Diameter                              ", 30
mx% = 7
a$ = Oldheight$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
Oldheight$ = a$
height = VAL(a$)
LOCATE 10, 44, 0
PRINT USING "###.###"; height
IF abort% THEN EXIT DO
LOCATE 11, 44, 0
aprint 25, 10, "Enter Density of Contents                        ", 30
mx% = 7
a$ = olddensity$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
olddensity$ = a$
Density = VAL(a$)
LOCATE 11, 44, 0
PRINT USING "###.###"; Density
IF abort% THEN EXIT DO
LOCATE 13, 44, 0
mx% = 7
aprint 25, 10, "Enter Outlet Centreline                   ", 30
a$ = oldcentreline$
a$ = RevInput$(mx%, a$)
oldcentreline$ = a$
a$ = LTRIM$(RTRIM$(a$))
Centreline = VAL(a$)
LOCATE 13, 44, 0
PRINT USING "###.###"; Centreline
IF abort% THEN EXIT DO
LOCATE 14, 44, 0
aprint 25, 10, "Enter AG1 Flange Height from C/Line (115mm)", 30
mx% = 7
a$ = oldflange$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
oldflange$ = a$
Flange = VAL(a$)
LOCATE 14, 44, 0
PRINT USING "   ####"; Flange
IF abort% THEN EXIT DO
aprint 25, 1, STRING$(80, 32), 31
 ok% = Verify%(19, "Change any entered data  ")
 IF NOT ok% THEN EXIT DO
LOOP
IF abort% THEN CLEAR : RUN "gauge.exe"

'**************************************************************************
'* EG162  Diameter *
'*******************
CASE IS = 52
aprint 25, 1, STRING$(80, 32), 31
aprint 9, 24, "Tank Capacity     :          ", 31
aprint 10, 24, "Tank Diameter     :          " + unit$, 31
aprint 11, 24, "Density           :          ", 31
DO UNTIL abort%
LOCATE 9, 44, 0
aprint 25, 10, "Enter Tank Capacity                            ", 30
aprint 25, 60, "ESC to Abort", 30
mx% = 8
a$ = Oldcapacity$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
Oldcapacity$ = a$
Capacity = VAL(a$)
LOCATE 9, 44, 0
PRINT USING "########.##"; Capacity
IF abort% THEN EXIT DO
LOCATE 10, 44, 0
aprint 25, 10, "Enter Tank Diameter                              ", 30
mx% = 7
a$ = Oldheight$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
Oldheight$ = a$
height = VAL(a$)
LOCATE 10, 44, 0
PRINT USING "###.###"; height
IF abort% THEN EXIT DO
LOCATE 11, 44, 0
aprint 25, 10, "Enter Density of Contents                        ", 30
mx% = 7
a$ = olddensity$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
olddensity$ = a$
Density = VAL(a$)
LOCATE 11, 44, 0
PRINT USING "###.###"; Density
IF abort% THEN EXIT DO
aprint 25, 1, STRING$(80, 32), 31
 ok% = Verify%(19, "Change any entered data  ")
 IF NOT ok% THEN EXIT DO
LOOP
IF abort% THEN CLEAR : RUN "gauge.exe"

'**************************************************************************
'* Analog Meter  Diameter *
'**************************
CASE IS = 53
aprint 25, 1, STRING$(80, 32), 31
aprint 9, 24, "Tank Capacity     :          ", 31
aprint 10, 24, "Tank Diameter     :          " + unit$, 31
DO UNTIL abort%
LOCATE 9, 44, 0
aprint 25, 10, "Enter Tank Capacity                            ", 30
aprint 25, 60, "ESC to Abort", 30
mx% = 8
a$ = Oldcapacity$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
Oldcapacity$ = a$
Capacity = VAL(a$)
LOCATE 9, 44, 0
PRINT USING "########.##"; Capacity
IF abort% THEN EXIT DO
LOCATE 10, 44, 0
aprint 25, 10, "Enter Tank Diameter                              ", 30
mx% = 7
a$ = Oldheight$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
Oldheight$ = a$
height = VAL(a$)
LOCATE 10, 44, 0
PRINT USING "###.###"; height
IF abort% THEN EXIT DO
 aprint 25, 1, STRING$(80, 32), 31
 ok% = Verify%(19, "Change any entered data  ")
 IF NOT ok% THEN EXIT DO
LOOP
IF abort% THEN CLEAR : RUN "gauge.exe"

'**************************************************************************
'* Dipchart  Diameter     *
'**************************
CASE IS = 54
aprint 25, 1, STRING$(80, 32), 31
aprint 9, 24, "Tank Capacity     :          ", 31
aprint 10, 24, "Tank Diameter     :          " + unit$, 31
DO UNTIL abort%
LOCATE 9, 44, 0
aprint 25, 10, "Enter Tank Capacity                            ", 30
aprint 25, 60, "ESC to Abort", 30
mx% = 8
a$ = Oldcapacity$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
Oldcapacity$ = a$
Capacity = VAL(a$)
LOCATE 9, 44, 0
PRINT USING "########.##"; Capacity
IF abort% THEN EXIT DO
LOCATE 10, 44, 0
aprint 25, 10, "Enter Tank Diameter                              ", 30
mx% = 7
a$ = Oldheight$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
Oldheight$ = a$
height = VAL(a$)
LOCATE 10, 44, 0
PRINT USING "###.###"; height
IF abort% THEN EXIT DO
 aprint 25, 1, STRING$(80, 32), 31
 ok% = Verify%(19, "Change any entered data  ")
 IF NOT ok% THEN EXIT DO
LOOP
IF abort% THEN CLEAR : RUN "gauge.exe"

END SELECT
Total = Capacity
RETURN

'**************************************************************************
'*     RectangularCalc                                                    *
'**************************************************************************
RectangularCalc:
CALL unitstype(feet%, unit$, dense$)
aprint 25, 1, STRING$(80, 32), 31
aprint 8, 24, "Tank Length       :          " + unit$, 31
aprint 9, 24, "Tank Width        :          " + unit$, 31
aprint 10, 24, "Tank Height       :          " + unit$, 31
aprint 11, 24, "Density           :          ", 31
IF cal% = 11 THEN
aprint 13, 24, "Outlet Centreline :          " + unit$, 31
aprint 14, 24, "C/L to AG1 Flange :          " + "mm", 31
END IF
DO UNTIL abort%
aprint 25, 1, STRING$(80, 32), 31
LOCATE 8, 44, 0
aprint 25, 10, "Enter Tank Length                              ", 30
aprint 25, 60, "ESC to Abort", 30
mx% = 7
a$ = oldlength$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
oldlength$ = a$
length = VAL(a$)
LOCATE 8, 44, 0
PRINT USING "###.###"; length
IF abort% THEN EXIT DO
LOCATE 9, 44, 0
aprint 25, 10, "Enter Tank Width                               ", 30
mx% = 7
a$ = oldbreadth$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
oldbreadth$ = a$
breadth = VAL(a$)
LOCATE 9, 44, 0
PRINT USING "###.###"; breadth
IF abort% THEN EXIT DO
LOCATE 10, 44, 0
aprint 25, 10, "Enter Tank Height                              ", 30
mx% = 7
a$ = Oldheight$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
Oldheight$ = a$
height = VAL(a$)
LOCATE 10, 44, 0
PRINT USING "###.###"; height
IF abort% THEN EXIT DO
LOCATE 11, 44, 0
aprint 25, 10, "Enter Density of Contents                        ", 30
mx% = 7
a$ = olddensity$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
olddensity$ = a$
Density = VAL(a$)
LOCATE 11, 44, 0
PRINT USING "###.###"; Density
IF abort% THEN EXIT DO
IF cal% = 11 THEN
LOCATE 13, 44, 0
mx% = 7
aprint 25, 10, "Enter Outlet Centreline                   ", 30
a$ = oldcentreline$
a$ = RevInput$(mx%, a$)
oldcentreline$ = a$
a$ = LTRIM$(RTRIM$(a$))
Centreline = VAL(a$)
LOCATE 13, 44, 0
PRINT USING "###.###"; Centreline
IF abort% THEN EXIT DO
LOCATE 14, 44, 0
aprint 25, 10, "Enter AG1 Flange Height from C/Line (115mm)", 30
mx% = 7
a$ = oldflange$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
oldflange$ = a$
Flange = VAL(a$)
LOCATE 14, 44, 0
PRINT USING "   ####"; Flange
IF abort% THEN EXIT DO
END IF
aprint 25, 1, STRING$(80, 32), 31
 ok% = Verify%(19, "Change any entered data  ")
 IF NOT ok% THEN EXIT DO
LOOP
IF abort% THEN CLEAR : RUN "gauge.exe"
RETURN

'**************************************************************************
'*     HorizontalCalc                                                     *
'**************************************************************************
HorizontalCalc:
CALL unitstype(feet%, unit$, dense$)
aprint 25, 1, STRING$(80, 32), 31
aprint 8, 24, "Tank Length       :          " + unit$, 31
aprint 9, 24, "Tank Diameter     :          " + unit$, 31
aprint 10, 24, "Dish 1 Depth      :          " + unit$, 31
aprint 11, 24, "Dish 2 Depth      :          " + unit$, 31
aprint 12, 24, "Density           :          ", 31
IF cal% = 21 THEN
aprint 14, 24, "Outlet Centreline :          " + unit$, 31
aprint 15, 24, "C/L to AG1 Flange :          " + "mm", 31
END IF
DO UNTIL abort%
aprint 25, 1, STRING$(80, 32), 31
LOCATE 8, 44, 0
aprint 25, 10, "Enter Tank Straight Length                       ", 30
aprint 25, 60, "ESC to Abort", 30
mx% = 7
a$ = oldlength$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
oldlength$ = a$
length = VAL(a$)
LOCATE 8, 44, 0
PRINT USING "###.###"; length
IF abort% THEN EXIT DO
LOCATE 9, 44, 0
aprint 25, 10, "Enter Tank Diameter                              ", 30
mx% = 7
a$ = olddiameter$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
olddiameter$ = a$
diameter = VAL(a$)
LOCATE 9, 44, 0
PRINT USING "###.###"; diameter
IF abort% THEN EXIT DO
LOCATE 10, 44, 0
aprint 25, 10, "Enter Dish 1 Depth                                 ", 30
mx% = 7
a$ = Olddish1$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
Olddish1$ = a$
dish1 = VAL(a$)
LOCATE 10, 44, 0
PRINT USING "###.###"; dish1
IF abort% THEN EXIT DO
LOCATE 11, 44, 0
aprint 25, 10, "Enter Dish 2 Depth                                 ", 30
mx% = 7
a$ = Olddish2$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
Olddish2$ = a$
dish2 = VAL(a$)
LOCATE 11, 44, 0
PRINT USING "###.###"; dish2
LOCATE 12, 44, 0
aprint 25, 10, "Enter Density of Contents                          ", 30
mx% = 7
a$ = olddensity$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
olddensity$ = a$
Density = VAL(a$)
LOCATE 12, 44, 0
PRINT USING "###.###"; Density
IF cal% = 21 THEN
LOCATE 14, 44, 0
mx% = 7
aprint 25, 10, "Enter Outlet Centreline                     ", 30
a$ = oldcentreline$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
oldcentreline$ = a$
Centreline = VAL(a$)
LOCATE 14, 44, 0
PRINT USING "###.###"; Centreline
IF abort% THEN EXIT DO
LOCATE 15, 44, 0
aprint 25, 10, "Enter AG1 Flange Height from C/Line (115mm) ", 30
mx% = 7
a$ = oldflange$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
oldflange$ = a$
Flange = VAL(a$)
LOCATE 15, 44, 0
PRINT USING "   ####"; Flange
IF abort% THEN EXIT DO
END IF
aprint 25, 1, STRING$(80, 32), 31
 ok% = Verify%(19, "Change any entered data  ")
 IF NOT ok% THEN EXIT DO
LOOP
IF abort% THEN RUN "gauge.exe"
RETURN

'**************************************************************************
'*     VerticalCalc                                                       *
'**************************************************************************
VerticalCalc:
CALL unitstype(feet%, unit$, dense$)
aprint 25, 1, STRING$(80, 32), 31
aprint 8, 24, "Tank Height      :          " + unit$, 31
aprint 9, 24, "Tank Diameter    :          " + unit$, 31
aprint 10, 24, "Bottom Dish      :          " + unit$, 31
aprint 11, 24, "Density          :          ", 31
IF cal% = 31 THEN
aprint 13, 24, "Outlet Centreline:          " + unit$, 31
aprint 14, 24, "C/L to AG1 Flange:          " + "mm", 31
END IF
DO UNTIL abort%
aprint 25, 1, STRING$(80, 32), 31
LOCATE 8, 44, 0
aprint 25, 10, "Enter Tank Height                        ", 30
aprint 25, 60, "ESC to Abort", 30
mx% = 7
a$ = Oldheight$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
Oldheight$ = a$
height = VAL(a$)
LOCATE 8, 44, 0
PRINT USING "###.###"; height
IF abort% THEN EXIT DO
LOCATE 9, 44, 0
aprint 25, 10, "Enter Tank Diameter                             ", 30
mx% = 7
a$ = olddiameter$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
olddiameter$ = a$
diameter = VAL(a$)
LOCATE 9, 44, 0
PRINT USING "###.###"; diameter
IF abort% THEN EXIT DO
LOCATE 10, 44, 0
aprint 25, 10, "Enter Tank Bottom Dish                          ", 30
mx% = 7
a$ = Olddish$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
Olddish$ = a$
dish = VAL(a$)
LOCATE 10, 44, 0
PRINT USING "###.###"; dish
IF abort% THEN EXIT DO
LOCATE 11, 44, 0
aprint 25, 10, "Enter Density of Contents                         ", 30
mx% = 7
a$ = olddensity$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
olddensity$ = a$
Density = VAL(a$)
LOCATE 11, 44, 0
PRINT USING "###.###"; Density
IF abort% THEN EXIT DO
IF cal% = 31 THEN
LOCATE 13, 44, 0
mx% = 7
aprint 25, 10, "Enter Outlet Centreline                   ", 30
a$ = oldcentreline$
a$ = RevInput$(mx%, a$)
oldcentreline$ = a$
a$ = LTRIM$(RTRIM$(a$))
Centreline = VAL(a$)
LOCATE 13, 44, 0
PRINT USING "###.###"; Centreline
IF abort% THEN EXIT DO
LOCATE 14, 44, 0
aprint 25, 10, "Enter AG1 Flange Height to C/Line (115mm) ", 30
mx% = 7
a$ = oldflange$
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
oldflange$ = a$
Flange = VAL(a$)
LOCATE 14, 44, 0
PRINT USING "   ####"; Flange
IF abort% THEN EXIT DO
END IF
 aprint 25, 1, STRING$(80, 32), 31
 ok% = Verify%(19, "Change any entered data  ")
 IF NOT ok% THEN EXIT DO
LOOP
IF abort% THEN RUN "gauge.exe"
RETURN

'**************************************************************************
'*     Volume                                                             *
'**************************************************************************
volume:
radius = diameter * .5
pi6 = pi / 6
pir2 = pi * (radius ^ 2)
SELECT CASE cal%
CASE 11
 volume = length * breadth * height
CASE 12, 13, 14
 volume = length * breadth * height
CASE 21
 dish1volume = (pi6 * dish1) * (3 * (radius ^ 2)) + (dish1 ^ 2)
 dish2volume = (pi6 * dish2) * (3 * (radius ^ 2)) + (dish2 ^ 2)
 volume = (pir2 * length) + dish1volume + dish2volume
CASE 22, 23, 24
 dish1volume = (pi6 * dish1) * (3 * (radius ^ 2)) + (dish1 ^ 2)
 dish2volume = (pi6 * dish2) * (3 * (radius ^ 2)) + (dish2 ^ 2)
 volume = (pir2 * length) + dish1volume + dish2volume
CASE 31
 dishvolume = (pi6 * dish) * (3 * (radius ^ 2)) + (dish ^ 2)
 volume = (pir2 * height) + dishvolume
CASE 32, 33, 34
 dishvolume = (pi6 * dish) * (3 * (radius ^ 2)) + (dish ^ 2)
 volume = (pir2 * height) + dishvolume
END SELECT
RETURN

'*************************************************************************
'*     Convertvolume                                                     *
'*************************************************************************
convertvolume:
SELECT CASE cal%
 CASE 11
  VesselHeight = height
 CASE 12
  VesselHeight = height
 CASE 13
  VesselHeight = height
 CASE 14
  VesselHeight = height
 CASE 21
  VesselHeight = diameter
 CASE 22
  VesselHeight = diameter
 CASE 23
  VesselHeight = diameter
 CASE 24
  VesselHeight = diameter
 CASE 31
  VesselHeight = height + dish
 CASE 32
  VesselHeight = height + dish
 CASE 33
  VesselHeight = height + dish
 CASE 34
  VesselHeight = height + dish
 CASE 41
  VesselHeight = height
 CASE 42
  VesselHeight = height
 CASE 43
  VesselHeight = height
 CASE 44
  VesselHeight = height
 CASE 51
  VesselHeight = height
 CASE 52
  VesselHeight = height
 CASE 53
  VesselHeight = height
 CASE 54
  VesselHeight = height

END SELECT

'**************************************************************************
'* Unit Select *
'***************
IF feet% THEN
 VesselHeight = VesselHeight * 12   'INCHES
 Centreline = Centreline * 12
 Flange = Flange * .03937
ELSE
 VesselHeight = VesselHeight * 1000 * .03937
 Centreline = Centreline * 1000 * .03937
 Flange = Flange * .03937
END IF

'**************************************************************************
SELECT CASE cal%
CASE 41
	RETURN
CASE 42
	RETURN
CASE 51
	RETURN
CASE 52
	RETURN
CASE 43, 53, 44, 54
	Total = Capacity
RETURN
	CASE ELSE
SELECT CASE calcode%
CASE 1  'Litres
	IF feet% THEN
		Total = ((volume * 6.25 * 4.546))
	ELSE
		Total = ((volume * 1000))
	END IF

CASE 2  'Gallons
	IF feet% THEN
		Total = ((volume * 6.25))
	ELSE
		Total = (((volume * 1000) / 4.546))
	END IF
CASE 3  'Cu Metres
	IF feet% THEN
		Total = volume * .0283
	ELSE
		Total = volume
	END IF
CASE 4  'Cu Feet
	IF feet% THEN
		Total = volume
	ELSE
		Total = volume * 35.31
	END IF
CASE 5  'Kg
	IF feet% THEN
		Total = (volume * .0283 * 1000) * Density
	ELSE
		Total = (volume * 1000) * Density
	END IF
CASE 6  'Tonnes
	IF feet% THEN
		Total = ((volume * .0283 * 1000) * Density) * .001
	ELSE
		Total = ((volume * 1000) * Density) * .001
	END IF
CASE 7  'Tons
	IF feet% THEN
		Total = ((volume * .0283 * 1000) * Density) * .0009
	ELSE
		Total = ((volume * 1000) * Density) * .0009
	END IF
CASE 8  'Percent
	Total = 100
CASE 9  'Fractions
	scale$ = "Fractions"
	Total = 1
CASE 10 'Metres
	Total = CINT(VesselHeight * .0254)
CASE 11 'Feet
	Total = VesselHeight / 12
CASE 12 'US Gallons
	IF feet% THEN
		Total = ((volume * 6.25) / .8327)
	ELSE
		Total = (((volume * 1000) / 4.546) / .8327)
	END IF
CASE 13 'MM
	Total = (VesselHeight * .0254) * 1000

END SELECT

END SELECT
RETURN

'**************************************************************************
'*     Getchart                                                           *
'**************************************************************************
getchart:
fraction = false
percent = false
SELECT CASE calcode%
'***********
'* percent *
'***********
CASE 8
percent = True
inc = 100 * .05
max% = 100 / inc
 format$ = "###"
FOR count% = 1 TO max%
Chart(count%) = count% * 5
NEXT
counter% = count%
FOR count% = 0 TO max% STEP 5
Mark$(count%) = "M"
NEXT

'**************************************************************************
'* fractions *
'*************
CASE 9
fraction = True
inc = .0625
max% = 17
format$ = " #.#### "
Mark$(16) = "M"
FOR count% = 1 TO max%
Chart(count%) = inc
inc = inc + .0625
NEXT
FOR count% = 0 TO max% STEP 4
 Mark$(count%) = "M"
 NEXT
OPEN "fraction.msk" FOR INPUT AS 1
FOR count% = 0 TO 16
INPUT #1, Mask$(count%)
NEXT
CLOSE 1
counter% = 17

'**************************************************************************
'* All Other Calibrations *
'**************************
CASE ELSE
OPEN "chart.tbl" FOR INPUT AS 1
Z = Total * 1000
IF Z = 0 THEN RETURN
SELECT CASE Total
CASE IS <= .9
multiplyer = .0001
format$ = "##.### "
CASE IS <= 9
multiplyer = .001
format$ = "##.## "
CASE IS <= 99
multiplyer = .01
format$ = "###.# "
CASE IS <= 999
multiplyer = .1
format$ = "### "
CASE IS <= 9999
multiplyer = 1
format$ = "###### "
CASE IS <= 99999
multiplyer = 10
format$ = "####### "
CASE IS <= 999999
multiplyer = 100
format$ = "######### "
CASE IS <= 9999999
multiplyer = 1000
format$ = "######### "
END SELECT
table$ = STR$(Z)
table$ = LTRIM$(RTRIM$(table$))
table$ = LEFT$(table$, 2)
p% = INSTR(table$, ".")
DO UNTIL EOF(1)
 LINE INPUT #1, Chart$
 IF LEFT$(Chart$, 2) = table$ THEN
  EXIT DO
 END IF
LOOP
CLOSE #1
counter% = 1
FOR count% = 11 TO LEN(Chart$) STEP 5
 Chart(counter%) = VAL(MID$(Chart$, count%, 4)) * multiplyer
 counter% = counter% + 1
NEXT
FOR count1% = VAL(MID$(Chart$, 4, 2)) TO counter% STEP VAL(MID$(Chart$, 7, 2))
IF fraction = false THEN
 Mark$(count1% + 1) = "M"
END IF
NEXT
IF Chart(counter% - 1) <> Total THEN
Chart(counter%) = Total
Mark$(counter%) = "M"
Mark$(counter% - 1) = ""
counter% = counter% + 1
END IF
IF fraction = false OR percent = false THEN
LOCATE 5, 23, 0
PRINT " Calibration range is ";
PRINT USING format$; Total;
PRINT " " + scale$
END IF
END SELECT
CALL dialtext(text$)
RETURN

'**************************************************************************
'*     RectangularTable                                                   *
'**************************************************************************
RectangularTable:
inc% = counter%
Lastmark = Chart(counter% - 1)
eg162flag% = false
SELECT CASE cal%

'**************************************************************************
'* Dipchart *
'************
CASE 14, 34, 44
 startmark = 0
 FOR Mark% = 1 TO inc%
  increment(Mark%) = Chart(Mark%) / Lastmark
  increment(Mark%) = (((increment(Mark%) * VesselHeight)))
  IF NOT increment(Mark%) = ABS(increment(Mark%)) THEN
   increment(Mark%) = 0
  END IF
 NEXT

'**************************************************************************
'* 160 *
'*******
CASE 11, 31, 41
 startmark = 0
 FOR Mark% = 1 TO inc%
  increment(Mark%) = Chart(Mark%) / Lastmark
  increment(Mark%) = (((increment(Mark%) * VesselHeight) - Centreline) - Flange) * Density * MillibarConversion
  IF NOT increment(Mark%) = ABS(increment(Mark%)) THEN
   increment(Mark%) = 0
  END IF
 NEXT
		
'**************************************************************************
'* 162 *
'*******
CASE 12, 32, 42
eg162flag% = True
IF (VesselHeight * Density) > 25 THEN
increment(0) = 0                              '45
FOR Mark% = 1 TO inc%
increment(Mark%) = Chart(Mark%) / Lastmark
increment(Mark%) = (increment(Mark%) * 270)   ' + 45
NEXT
ELSE
eg162flag% = True
increment(0) = 90
FOR Mark% = 1 TO inc%
increment(Mark%) = Chart(Mark%) / Lastmark
increment(Mark%) = (increment(Mark%) * 180) + 90
NEXT
END IF

'**************************************************************************
'* analog *
'**********
CASE 13, 33, 43, 53
eg162flag% = True
increment(0) = offset
FOR Mark% = 1 TO inc%
increment(Mark%) = Chart(Mark%) / Lastmark
increment(Mark%) = ((increment(Mark%) * analog) + offset)
 NEXT
END SELECT
RETURN

'**************************************************************************
'*     DiameterTable                                                      *
'**************************************************************************
DiameterTable:
inc% = counter%
Lastmark = Chart(counter% - 1)
midmark = Lastmark * .5
eg162flag% = false
SELECT CASE cal%

'**************************************************************************
 CASE 21, 51          '* 160 *
'*****************************
 startmark% = 1
 halfdone% = True
 FOR Mark% = 1 TO 1000
increment(Mark%) = Chart(Mark%) / Lastmark
IF Chart(Mark%) > (Lastmark / 2) THEN EXIT FOR
FOR table% = startmark% TO 1001
COLOR 14, 1
LOCATE 2, 5, 0: PRINT table%
areafactor = increment(Mark%)
lessareafactor = areafactor(table% - 1)
moreareafactor = areafactor(table% + 1)
IF areafactor > lessareafactor AND areafactor <= moreareafactor THEN
hdratio = hdratio(table%)
startmark% = table%
EXIT FOR
END IF
NEXT table%
increment(Mark%) = (((hdratio * VesselHeight) - Centreline) - Flange) * Density * MillibarConversion
IF NOT increment(Mark%) = ABS(increment(Mark%)) THEN
increment(Mark%) = 0
END IF
 NEXT Mark%
 halfmark% = Mark%
 startmark% = 1000
 FOR Mark% = halfmark% TO counter% - 1
increment(Mark%) = (Lastmark - Chart(Mark%))
increment(Mark%) = increment(Mark%) / Lastmark
IF increment(Mark%) = 0 THEN
 increment(Mark%) = (VesselHeight - Centreline - Flange) * Density * MillibarConversion
 hdratio = 1
EXIT FOR
END IF
FOR table% = startmark% TO 1 STEP -1
COLOR 14, 1
LOCATE 2, 5, 0: PRINT table%
areafactor = increment(Mark%)
lessareafactor = areafactor(table% - 1)
moreareafactor = areafactor(table% + 1)
IF areafactor > lessareafactor AND areafactor < moreareafactor THEN
hdratio = hdratio(table%)
startmark% = table%
EXIT FOR
END IF
NEXT table%
 increment(Mark%) = (((VesselHeight - (hdratio * VesselHeight)) - Centreline) - Flange) * Density * MillibarConversion
 NEXT Mark%

'**************************************************************************
 CASE 24, 54     '* Dipchart *
'*****************************
 startmark% = 1
 halfdone% = True
 FOR Mark% = 1 TO 1000
increment(Mark%) = Chart(Mark%) / Lastmark
IF Chart(Mark%) > (Lastmark / 2) THEN EXIT FOR
FOR table% = startmark% TO 1001
COLOR 14, 1
LOCATE 2, 5, 0: PRINT table%
areafactor = increment(Mark%)
lessareafactor = areafactor(table% - 1)
moreareafactor = areafactor(table% + 1)
IF areafactor > lessareafactor AND areafactor <= moreareafactor THEN
hdratio = hdratio(table%)
startmark% = table%
EXIT FOR
END IF
NEXT table%
increment(Mark%) = hdratio * VesselHeight
IF NOT increment(Mark%) = ABS(increment(Mark%)) THEN
increment(Mark%) = 0
END IF
 NEXT Mark%
 halfmark% = Mark%
 startmark% = 1000
 FOR Mark% = halfmark% TO counter% - 1
increment(Mark%) = (Lastmark - Chart(Mark%))
increment(Mark%) = increment(Mark%) / Lastmark
IF increment(Mark%) = 0 THEN
 increment(Mark%) = VesselHeight
 hdratio = 1
EXIT FOR
END IF
FOR table% = startmark% TO 1 STEP -1
COLOR 14, 1
LOCATE 2, 5, 0: PRINT table%
areafactor = increment(Mark%)
lessareafactor = areafactor(table% - 1)
moreareafactor = areafactor(table% + 1)
IF areafactor > lessareafactor AND areafactor < moreareafactor THEN
hdratio = hdratio(table%)
startmark% = table%
EXIT FOR
END IF
NEXT table%
 increment(Mark%) = VesselHeight - (hdratio * VesselHeight)
 NEXT Mark%

'**************************************************************************
 CASE 22, 52          '* 162 *
'*****************************
 startmark% = 1
 increment(0) = 0 '45
FOR Mark% = 1 TO 1000
increment(Mark%) = Chart(Mark%) / Lastmark
IF Chart(Mark%) > (Lastmark / 2) THEN EXIT FOR
FOR table% = startmark% TO 1001
COLOR 14, 1
LOCATE 2, 5, 0: PRINT table%
areafactor = increment(Mark%)
lessareafactor = areafactor(table% - 1)
moreareafactor = areafactor(table% + 1)
IF areafactor > lessareafactor AND areafactor <= moreareafactor THEN
hdratio = hdratio(table%)
startmark% = table%
EXIT FOR
END IF
NEXT table%
 increment(Mark%) = (hdratio * 270)            ' + 45
 NEXT Mark%
 halfmark% = Mark%
 startmark% = 1000
 FOR Mark% = halfmark% TO (counter% - 1)
increment(Mark%) = (Lastmark - Chart(Mark%))
increment(Mark%) = increment(Mark%) / Lastmark
IF increment(Mark%) = 0 THEN
 increment(Mark%) = 270                        '315
 hdratio = 1
EXIT FOR
END IF
FOR table% = startmark% TO 1 STEP -1
COLOR 14, 1
LOCATE 2, 5, 0: PRINT table%
areafactor = increment(Mark%)
lessareafactor = areafactor(table% - 1)
moreareafactor = areafactor(table% + 1)
 IF areafactor > lessareafactor AND areafactor < moreareafactor THEN
hdratio = hdratio(table%)
startmark% = table%
EXIT FOR
END IF
NEXT table%
 increment(Mark%) = (hdratio * 270)
 increment(Mark%) = 270 - increment(Mark%)     '315
 NEXT Mark%

'**************************************************************************
 CASE 23, 53          '* Analog Meter *
'**************************************
 startmark% = 1
 increment(0) = offset
 FOR Mark% = 1 TO 1000
increment(Mark%) = Chart(Mark%) / Lastmark
IF Chart(Mark%) > (Lastmark / 2) THEN EXIT FOR
FOR table% = startmark% TO 1001
COLOR 14, 1
LOCATE 2, 5, 0: PRINT table%
areafactor = increment(Mark%)
lessareafactor = areafactor(table% - 1)
moreareafactor = areafactor(table% + 1)
IF areafactor > lessareafactor AND areafactor <= moreareafactor THEN
hdratio = hdratio(table%)
startmark% = table%
EXIT FOR
END IF
NEXT table%
increment(Mark%) = ((hdratio * analog) + offset)
 NEXT Mark%
 halfmark% = Mark%
 startmark% = 1000
 FOR Mark% = halfmark% TO (counter% - 1)
increment(Mark%) = (Lastmark - Chart(Mark%))
increment(Mark%) = increment(Mark%) / Lastmark
IF increment(Mark%) = 0 THEN
 increment(Mark%) = analog + offset
 hdratio = 1
EXIT FOR
END IF
FOR table% = startmark% TO 1 STEP -1
COLOR 14, 1
LOCATE 2, 5, 0: PRINT table%
areafactor = increment(Mark%)
lessareafactor = areafactor(table% - 1)
moreareafactor = areafactor(table% + 1)
IF areafactor > lessareafactor AND areafactor < moreareafactor THEN
hdratio = hdratio(table%)
startmark% = table%
EXIT FOR
END IF
NEXT table%
 increment(Mark%) = (hdratio * analog)
 increment(Mark%) = (analog + offset) - increment(Mark%)
 NEXT Mark%
END SELECT
RETURN

'**************************************************************************
'*     Printchart                                                         *
'**************************************************************************
printchart:
CLS 0

LOCATE 1, 1, 0
PRINT "S/O: "; Wo$; "    "; Cust$; "   Ref: "; Custorder$; "    ";
PRINT MID$(DATE$, 4, 2) + "/" + LEFT$(DATE$, 2) + "/" + RIGHT$(DATE$, 4)

LOCATE 25, 1, 0
PRINT ME$; "  Scale: "; scale$;
LOCATE 25, 49, 0
PRINT "  Dial Mark: "; text$;
index = (VesselHeight * Density) / 33.1
SELECT CASE cal%
CASE 11, 21, 31, 41, 51
full = (VesselHeight - Centreline - Flange) * Density * .9 * MillibarConversion
refill = (VesselHeight - Centreline - Flange) * Density * .3 * MillibarConversion
empty = (VesselHeight - Centreline - Flange) * Density * .1 * MillibarConversion
 LOCATE 16, 51, 0
 PRINT "Full @    :";
 PRINT USING " ###.# mbar"; full
 LOCATE 17, 51, 0
 PRINT "Refill @  :";
 PRINT USING " ###.# mbar"; refill
 LOCATE 18, 51, 0
 PRINT "Empty @   :";
 PRINT USING " ###.# mbar"; empty

CASE 14, 24, 34, 44, 54
full = (VesselHeight - Centreline - Flange) * .9 * 25.4
refill = (VesselHeight - Centreline - Flange) * .3 * 25.4
empty = (VesselHeight - Centreline - Flange) * .1 * 25.4
 LOCATE 16, 51, 0
 PRINT "Full @    :";
 PRINT USING " #### mm"; full
 LOCATE 17, 51, 0
 PRINT "Refill @  :";
 PRINT USING " #### mm"; refill
 LOCATE 18, 51, 0
 PRINT "Empty @   :";
 PRINT USING " #### mm"; empty

CASE 12, 22, 32, 42, 52
 IF VesselHeight * Density <= 25 THEN
full = (180 * .9) + 90
refill = (180 * .3) + 90
empty = (180 * .1) + 90
 ELSE
full = (270 * .9)                       ' + 45
refill = (270 * .3)                     ' + 45
empty = (270 * .1)                      ' + 45
 END IF
 LOCATE 16, 51, 0
 PRINT "Full @    :";
 PRINT USING " ###.# deg  "; full
 LOCATE 17, 51, 0
 PRINT "Refill @  :";
 PRINT USING " ###.# deg  "; refill
 LOCATE 18, 51, 0
 PRINT "Empty @   :";
 PRINT USING " ###.# deg  "; empty

CASE 13, 23, 33, 43, 53
 full = (analog * .9) + offset
 refill = (analog * .3) + offset
 empty = (analog * .1) + offset
 LOCATE 16, 51, 0
 PRINT "Full @    :";
 PRINT USING " ###.#" + mFormat$; full
 LOCATE 17, 51, 0
 PRINT "Refill @  :";
 PRINT USING " ###.#" + mFormat$; refill
 LOCATE 18, 51, 0
 PRINT "Empty @   :";
 PRINT USING " ###.#" + mFormat$; empty
END SELECT

'**************************************************************************
'*       160      *
'******************
 SELECT CASE cal%
CASE 11 AND fraction
 inc$ = " mbar "
 fmt$ = " ###.#"
 LOCATE 3, 2, 0
 PRINT Mask$(0);
 PRINT USING fmt$; increment(0);
 PRINT inc$;
 PRINT Mark$(0)
 LOCATE 19, 51, 0
 PRINT "Height/Dia:";
 PRINT USING " ###.## ins"; VesselHeight
 LOCATE 20, 51, 0
 PRINT "Range     :";
 PRINT USING " ###.## ins"; (VesselHeight - Centreline - Flange) * Density
 LOCATE 21, 51, 0
 PRINT "S.G       :";
 PRINT USING "  ##.#### kg/l"; Density
 LOCATE 22, 51, 0
 PRINT "Centreline:";
 PRINT USING " ###.## ins"; Centreline
 LOCATE 23, 51, 0
 PRINT "AG Flange :";
 PRINT USING " ###.## ins"; Flange;
 r = 4: c = 2: rr = 4

'**************************************************************************
'*       162      *
'******************
CASE 12 AND fraction
 inc$ = " deg "
 fmt$ = " ###.#"
 LOCATE 3, 2, 0
 PRINT Mask$(0);
 PRINT USING fmt$; increment(0);
 PRINT inc$;
 PRINT Mark$(0)
 LOCATE 20, 51, 0
 PRINT "Height/Dia:";
 PRINT USING " ###.## ins"; VesselHeight
 LOCATE 21, 51, 0
 PRINT "Range     :";
 PRINT USING " ###.## ins"; (VesselHeight) * Density
 LOCATE 22, 51, 0
 PRINT "S.G       :";
 PRINT USING "  ##.#### kg/l"; Density;
 LOCATE 23, 51, 0
 PRINT "Set Index :";
 PRINT USING "  ##.##       "; index;
 r = 4: c = 2: rr = 4

'**************************************************************************
'* analog meter *
'****************
CASE 13 AND fraction
 inc$ = mFormat$
 fmt$ = " ###.#"
 LOCATE 3, 2, 0
 PRINT Mask$(0);
 PRINT USING fmt$; increment(0);
 PRINT inc$;
 PRINT Mark$(0)
 LOCATE 20, 51, 0
 PRINT "Height/Dia:";
 PRINT USING " ###.## ins"; VesselHeight
 LOCATE 21, 51, 0
 PRINT "Range     :";
 PRINT USING " ###.## ins"; (VesselHeight) * Density
 LOCATE 22, 51, 0
 PRINT "S.G       :";
 PRINT USING "  ##.#### kg/l"; Density;
 r = 4: c = 2: rr = 4

'**************************************************************************
'*     160       *
'*****************
CASE 21 AND fraction
 inc$ = " mbar "
 fmt$ = " ###.#"
 LOCATE 3, 2, 0
 PRINT Mask$(0);
 PRINT USING fmt$; increment(0);
 PRINT inc$;
 PRINT Mark$(0)
 LOCATE 19, 51, 0
 PRINT "Height/Dia:";
 PRINT USING " ###.## ins"; VesselHeight
 LOCATE 20, 51, 0
 PRINT "Range     :";
 PRINT USING " ###.## ins"; (VesselHeight - Centreline - Flange) * Density
 LOCATE 21, 51, 0
 PRINT "S.G       :";
 PRINT USING "  ##.#### kg/l"; Density
 LOCATE 22, 51, 0
 PRINT "Centreline:";
 PRINT USING " ###.## ins"; Centreline
 LOCATE 23, 51, 0
 PRINT "AG Flange :";
 PRINT USING " ###.## ins"; Flange;
 r = 4: c = 2: rr = 4

'**************************************************************************
'*       162      *
'******************
CASE 22 AND fraction
 inc$ = " deg "
 fmt$ = " ###.#"
 LOCATE 3, 2, 0
 PRINT Mask$(0);
 PRINT USING fmt$; increment(0);
 PRINT inc$;
 PRINT Mark$(0)
 LOCATE 20, 51, 0
 PRINT "Height/Dia:";
 PRINT USING " ###.## ins"; VesselHeight
 LOCATE 21, 51, 0
 PRINT "Range     :";
 PRINT USING " ###.## ins"; (VesselHeight) * Density
 LOCATE 22, 51, 0
 PRINT "S.G       :";
 PRINT USING "  ##.#### kg/l"; Density;
 LOCATE 23, 51, 0
 PRINT "Set Index :";
 PRINT USING "  ##.##       "; index;
 r = 4: c = 2: rr = 4

'**************************************************************************
'*   analog meter *
'******************
CASE 23 AND fraction
 inc$ = mFormat$
 fmt$ = " ###.#"
 LOCATE 3, 2, 0
 PRINT Mask$(0);
 PRINT USING fmt$; increment(0);
 PRINT inc$;
 PRINT Mark$(0)
 LOCATE 20, 51, 0
 PRINT "Height/Dia:";
 PRINT USING " ###.## ins"; VesselHeight
 LOCATE 21, 51, 0
 PRINT "Range     :";
 PRINT USING " ###.## ins"; (VesselHeight) * Density
 LOCATE 22, 51, 0
 PRINT "S.G       :";
 PRINT USING "  ##.#### kg/l"; Density;
 r = 4: c = 2: rr = 4

'**************************************************************************
'*      160      *
'*****************                                          
CASE 31 AND fraction
 inc$ = " mbar "
 fmt$ = " ###.#"
 LOCATE 3, 2, 0
 PRINT Mask$(0);
 PRINT USING fmt$; increment(0);
 PRINT inc$;
 PRINT Mark$(0)
 LOCATE 19, 51, 0
 PRINT "Height/Dia:";
 PRINT USING " ###.## ins"; VesselHeight
 LOCATE 20, 51, 0
 PRINT "Range     :";
 PRINT USING " ###.## ins"; (VesselHeight - Centreline - Flange) * Density
 LOCATE 21, 51, 0
 PRINT "S.G       :";
 PRINT USING "  ##.#### kg/l"; Density
 LOCATE 22, 51, 0
 PRINT "Centreline:";
 PRINT USING " ###.## ins "; Centreline
 LOCATE 23, 51, 0
 PRINT "AG Flange :";
 PRINT USING " ###.## ins "; Flange;
 r = 4: c = 2: rr = 4

'**************************************************************************
'*       162       *
'*******************
CASE 32 AND fraction
 inc$ = " deg "
 fmt$ = " ###.#"
 LOCATE 3, 2, 0
 PRINT Mask$(0);
 PRINT USING fmt$; increment(0);
 PRINT inc$;
 PRINT Mark$(0)
 LOCATE 20, 51, 0
 PRINT "Height/Dia:";
 PRINT USING " ###.## ins"; VesselHeight
 LOCATE 21, 51, 0
 PRINT "Range     :";
 PRINT USING " ###.## ins"; (VesselHeight) * Density
 LOCATE 22, 51, 0
 PRINT "S.G       :";
 PRINT USING "  ##.#### kg/l"; Density;
 LOCATE 23, 51, 0
 PRINT "Set Index :";
 PRINT USING "  ##.##       "; index;
 r = 4: c = 2: rr = 4

'**************************************************************************
'* analog meter *
'****************
CASE 33 AND fraction
 inc$ = mFormat$
 fmt$ = " ###.#"
 LOCATE 3, 2, 0
 PRINT Mask$(0);
 PRINT USING fmt$; increment(0);
 PRINT inc$;
 PRINT Mark$(0)
 LOCATE 20, 51, 0
 PRINT "Height/Dia:";
 PRINT USING " ###.## ins"; VesselHeight
 LOCATE 21, 51, 0
 PRINT "Range     :";
 PRINT USING " ###.## ins"; (VesselHeight) * Density
 LOCATE 22, 51, 0
 PRINT "S.G       :";
 PRINT USING "  ##.#### kg/l"; Density;
 r = 4: c = 2: rr = 4

'**************************************************************************
'*     160      *
'****************                                             
CASE 41 AND fraction
 inc$ = " mbar "
 fmt$ = " ###.#"
 LOCATE 3, 2, 0
 PRINT Mask$(0);
 PRINT USING fmt$; increment(0);
 PRINT inc$;
 PRINT Mark$(0)
 LOCATE 19, 51, 0
 PRINT "Height/Dia:";
 PRINT USING " ###.## ins"; VesselHeight
 LOCATE 20, 51, 0
 PRINT "Range     :";
 PRINT USING " ###.## ins"; (VesselHeight - Centreline - Flange) * Density
 LOCATE 21, 51, 0
 PRINT "S.G       :";
 PRINT USING "  ##.#### kg/l"; Density
 LOCATE 22, 51, 0
 PRINT "Centreline:";
 PRINT USING " ###.## ins "; Centreline
 LOCATE 23, 51, 0
 PRINT "AG Flange :";
 PRINT USING " ###.## ins "; Flange;
 r = 4: c = 2: rr = 4

'**************************************************************************
'*      162     *
'****************
CASE 42 AND fraction
 inc$ = " deg "
 fmt$ = " ###.#"
 LOCATE 3, 2, 0
 PRINT Mask$(0);
 PRINT USING fmt$; increment(0);
 PRINT inc$;
 PRINT Mark$(0)
 LOCATE 20, 51, 0
 PRINT "Height/Dia:";
 PRINT USING " ###.## ins"; VesselHeight
 LOCATE 21, 51, 0
 PRINT "Range     :";
 PRINT USING " ###.## ins"; (VesselHeight) * Density
 LOCATE 22, 51, 0
 PRINT "S.G       :";
 PRINT USING "  ##.#### kg/l"; Density;
 LOCATE 23, 51, 0
 PRINT "Set Index :";
 PRINT USING "  ##.##       "; index;
 r = 4: c = 2: rr = 4

'**************************************************************************
'* Analog Meter *
'****************
CASE 43 AND fraction
 inc$ = mFormat$
 fmt$ = " ###.#"
 LOCATE 3, 2, 0
 PRINT Mask$(0);
 PRINT USING fmt$; increment(0);
 PRINT inc$;
 PRINT Mark$(0)
 LOCATE 20, 51, 0
 PRINT "Height/Dia:";
 PRINT USING " ###.## ins"; VesselHeight
 LOCATE 21, 51, 0
 PRINT "Range     :";
 PRINT USING " ###.## ins"; (VesselHeight) * Density
 LOCATE 22, 51, 0
 PRINT "S.G       :";
 PRINT USING "  ##.#### kg/l"; Density;
 r = 4: c = 2: rr = 4

'**************************************************************************
'*      160      *
'*****************
CASE 51 AND fraction
 inc$ = " mbar "
 fmt$ = " ###.#"
 LOCATE 3, 2, 0
 PRINT Mask$(0);
 PRINT USING fmt$; increment(0);
 PRINT inc$;
 PRINT Mark$(0)
 LOCATE 19, 51, 0
 PRINT "Height/Dia:";
 PRINT USING " ###.## ins"; VesselHeight
 LOCATE 20, 51, 0
 PRINT "Range     :";
 PRINT USING " ###.## ins"; (VesselHeight - Centreline - Flange) * Density
 LOCATE 21, 51, 0
 PRINT "S.G       :";
 PRINT USING "  ##.#### kg/l"; Density
 LOCATE 22, 51, 0
 PRINT "Centreline:";
 PRINT USING " ###.## ins "; Centreline
 LOCATE 23, 51, 0
 PRINT "AG Flange :";
 PRINT USING " ###.## ins "; Flange;
 r = 4: c = 2: rr = 4

'**************************************************************************
'*   162        *
'****************
CASE 52 AND fraction
 inc$ = " deg "
 fmt$ = " ###.#"
 LOCATE 3, 2, 0
 PRINT Mask$(0);
 PRINT USING fmt$; increment(0);
 PRINT inc$;
 PRINT Mark$(0)
 LOCATE 20, 51, 0
 PRINT "Height/Dia:";
 PRINT USING " ###.## ins"; VesselHeight
 LOCATE 21, 51, 0
 PRINT "Range     :";
 PRINT USING " ###.## ins"; (VesselHeight) * Density
 LOCATE 22, 51, 0
 PRINT "S.G       :";
 PRINT USING "  ##.#### kg/l"; Density;
 LOCATE 23, 51, 0
 PRINT "Set Index :";
 PRINT USING "  ##.##       "; index;
 r = 4: c = 2: rr = 4

'**************************************************************************
'* Analog Meter *
'****************
CASE 53 AND fraction
 inc$ = mFormat$
 fmt$ = " ###.#"
 LOCATE 3, 2, 0
 PRINT Mask$(0);
 PRINT USING fmt$; increment(0);
 PRINT inc$;
 PRINT Mark$(0)
 LOCATE 20, 51, 0
 PRINT "Height/Dia:";
 PRINT USING " ###.## ins"; VesselHeight
 LOCATE 21, 51, 0
 PRINT "Range     :";
 PRINT USING " ###.## ins"; (VesselHeight) * Density
 LOCATE 22, 51, 0
 PRINT "S.G       :";
 PRINT USING "  ##.#### kg/l"; Density;
 r = 4: c = 2: rr = 4

'**************************************************************************
'*   160        *
'****************
CASE 11, 21, 31, 41, 51
 inc$ = " mbar "
 fmt$ = " ###.#"
 LOCATE 3, 2, 0
 PRINT USING format$; Chart(0);
 PRINT USING fmt$; increment(0);
 PRINT inc$;
 PRINT Mark$(0)
 LOCATE 19, 51, 0
 PRINT "Height/Dia:";
 PRINT USING " ###.## ins"; VesselHeight
 LOCATE 20, 51, 0
 PRINT "Range     :";
 PRINT USING " ###.## ins"; (VesselHeight - Centreline - Flange) * Density
 LOCATE 21, 51, 0
 PRINT "S.G       :";
 PRINT USING "  ##.#### kg/l"; Density
 LOCATE 22, 51, 0
 PRINT "Centreline:";
 PRINT USING " ###.## ins "; Centreline
 LOCATE 23, 51, 0
 PRINT "AG Flange :";
 PRINT USING " ###.## ins "; Flange;
 r = 4: c = 2: rr = 3

'**************************************************************************
'*   Dipchart   *
'****************

CASE 14, 24, 34, 44, 54
 inc$ = " mm "
 fmt$ = " #####"
 LOCATE 3, 2, 0
 PRINT USING format$; Chart(0);
 PRINT USING fmt$; increment(0);
 PRINT inc$;
 PRINT Mark$(0)
 LOCATE 19, 51, 0
 PRINT "Height/Dia: ";
 PRINT USING "##### mm"; VesselHeight * 25.4
 LOCATE 20, 51, 0
 PRINT "Range     : ";
 PRINT USING "##### mm"; (VesselHeight - Centreline - Flange) * 25.4;
 r = 4: c = 2: rr = 3

'**************************************************************************
'*     162      *
'****************
CASE 12, 22, 32, 42, 52
 inc$ = " deg "
 fmt$ = " ###.#"
 LOCATE 3, 2, 0
 PRINT USING format$; Chart(0);
 PRINT USING fmt$; increment(0);
 PRINT inc$;
 PRINT Mark$(0)
 LOCATE 20, 51, 0
 PRINT "Height/Dia:";
 PRINT USING " ###.## ins"; VesselHeight
 LOCATE 21, 51, 0
 PRINT "Range     :";
 PRINT USING " ###.## ins"; (VesselHeight) * Density
 LOCATE 22, 51, 0
 PRINT "S.G       :";
 PRINT USING "  ##.#### kg/l"; Density;
 LOCATE 23, 51, 0
 PRINT "Set Index :";
 PRINT USING "  ##.##       "; index;
 r = 4: c = 2: rr = 3

'**************************************************************************
'*     Analog   *
'****************
CASE 13, 23, 33, 43, 53
 inc$ = mFormat$
 fmt$ = " ###.#"
 LOCATE 3, 2, 0
 PRINT USING format$; Chart(0);
 PRINT USING fmt$; increment(0);
 PRINT inc$;
 PRINT Mark$(0)
 LOCATE 20, 51, 0
 PRINT "Height/Dia:";
 PRINT USING " ###.## ins"; VesselHeight
 LOCATE 21, 51, 0
 PRINT "Range     :";
 PRINT USING " ###.## ins"; (VesselHeight) * Density
 LOCATE 22, 51, 0
 PRINT "S.G       :";
 PRINT USING "  ##.#### kg/l"; Density;
 r = 4: c = 2: rr = 3
 END SELECT
FOR count% = 1 TO counter% - 1
LOCATE r, c, 0
 SELECT CASE cal%

'**************************************************************************
'*     160      *
'****************
 CASE 11 AND fraction
 PRINT Mask$(count%);
 PRINT USING fmt$; increment(count%) * MillibarConversion;
 PRINT inc$;
 PRINT Mark$(count%)

'**************************************************************************
'*     162      *
'****************
 CASE 12 AND fraction
 PRINT Mask$(count%);
 PRINT USING fmt$; increment(count%);
 PRINT inc$;
 PRINT Mark$(count%)

'**************************************************************************
'* Analog Meter *
'****************
 CASE 13 AND fraction
 PRINT Mask$(count%);
 PRINT USING fmt$; increment(count%) + offset;
 PRINT inc$;
 PRINT Mark$(count%)

'**************************************************************************
'*     160      *
'****************
 CASE 21 AND fraction
 PRINT Mask$(count%);
 PRINT USING fmt$; increment(count%) * MillibarConversion;
 PRINT inc$;
 PRINT Mark$(count%)

'**************************************************************************
'*     162      *
'****************
 CASE 22 AND fraction
 PRINT Mask$(count%);
 PRINT USING fmt$; increment(count%);
 PRINT inc$;
 PRINT Mark$(count%)

'**************************************************************************
'* Analog Meter *
'****************
 CASE 23 AND fraction
 PRINT Mask$(count%);
 PRINT USING fmt$; increment(count%) + offset;
 PRINT inc$;
 PRINT Mark$(count%)

'**************************************************************************
'*    160       *
'****************
 CASE 31 AND fraction
 PRINT Mask$(count%);
 PRINT USING fmt$; increment(count%) * MillibarConversion;
 PRINT inc$;
 PRINT Mark$(count%)

'**************************************************************************
'*    162       *
'****************
 CASE 32 AND fraction
 PRINT Mask$(count%);
 PRINT USING fmt$; increment(count%);
 PRINT inc$;
 PRINT Mark$(count%)

'**************************************************************************
'* Analog Meter *
'****************
 CASE 33 AND fraction
 PRINT Mask$(count%);
 PRINT USING fmt$; increment(count%) + offset;
 PRINT inc$;
 PRINT Mark$(count%)

'**************************************************************************
'*    160       *
'****************
 CASE 41 AND fraction
 PRINT Mask$(count%);
 PRINT USING fmt$; increment(count%) * MillibarConversion;
 PRINT inc$;
 PRINT Mark$(count%)

'**************************************************************************
'*    162       *
'****************
 CASE 42 AND fraction
 PRINT Mask$(count%);
 PRINT USING fmt$; increment(count%);
 PRINT inc$;
 PRINT Mark$(count%)

'**************************************************************************
'* Analog Meter *
'****************
 CASE 43 AND fraction
 PRINT Mask$(count%);
 PRINT USING fmt$; increment(count%) + offset;
 PRINT inc$;
 PRINT Mark$(count%)

'**************************************************************************
'*     160      *
'****************
 CASE 51 AND fraction
 PRINT Mask$(count%);
 PRINT USING fmt$; increment(count%) * MillibarConversion;
 PRINT inc$;
 PRINT Mark$(count%)

'**************************************************************************
'*     162      *
'****************
 CASE 52 AND fraction
 PRINT Mask$(count%);
 PRINT USING fmt$; increment(count%);
 PRINT inc$;
 PRINT Mark$(count%)

'**************************************************************************
'* Analog Meter *
'****************
 CASE 53 AND fraction
 PRINT Mask$(count%);
 PRINT USING fmt$; increment(count%) + offset;
 PRINT inc$;
 PRINT Mark$(count%)

'**************************************************************************
'*  Dipchart    *
'****************
 CASE 14, 24, 34, 44, 54
 PRINT USING format$; Chart(count%);
 PRINT USING fmt$; increment(count%) * 25.4;
 PRINT inc$;
 PRINT Mark$(count%)

'**************************************************************************
'*      160     *
'****************
 CASE 11, 21, 31, 41, 51
 PRINT USING format$; Chart(count%);
 PRINT USING fmt$; increment(count%);
 PRINT inc$;
 PRINT Mark$(count%)

'**************************************************************************
'*   162   *
'***********
 CASE 12, 22, 32, 42, 52, 13, 23, 33, 43, 53
 PRINT USING format$; Chart(count%);
 PRINT USING fmt$; increment(count%);
 PRINT inc$;
 PRINT Mark$(count%)
END SELECT
r = r + 1
IF r = 24 THEN
r = rr: c = c + 25
END IF

skip:
NEXT

'*************************************************************************
'*      Write File to Disk                                               *
'*************************************************************************
row% = 15
col% = 60
OPEN Wo$ + ".cal" FOR OUTPUT AS 1

SELECT CASE cal%

CASE IS = 14, 24, 24, 44, 54
PRINT #1, "Afriso Eurogauge Ltd Calsys 5.4 ";
PRINT #1, MID$(DATE$, 4, 2) + "/" + LEFT$(DATE$, 2) + "/" + RIGHT$(DATE$, 4);
PRINT #1, " "; "      S/O: "; Wo$
PRINT #1, STRING$(74, "-")
PRINT #1, "Name: "; Cust$; "   "; "Ref: "; Custorder$
PRINT #1, ME$;
PRINT #1, "     Text on Dial: "; text$
PRINT #1, "Gauge Scale: "; scale$;
PRINT #1, USING "   Height/Diameter  ##### mm "; VesselHeight * 25.4
PRINT #1, STRING$(74, "-")
PRINT #1, "   " + scale$;
PRINT #1, TAB(21); inc$;
PRINT #1, TAB(50); "Degrees"
PRINT #1, ""
IF fraction THEN
 FOR c% = 0 TO counter% - 1
  PRINT #1, Mask$(c%) + "  ";
  PRINT #1, Mark$(c%);
  PRINT #1, TAB(20); USING "#####"; increment(c%) * 25.4;
  PRINT #1, TAB(33); "______";
  PRINT #1, TAB(44); "______";
  PRINT #1, TAB(55); "______";
  PRINT #1, TAB(66); "______"
 NEXT
ELSE
IF scale$ = "Cu Metres" THEN
 FOR c% = 0 TO counter% - 1
  PRINT #1, USING "####.##  "; Chart(c%);
  PRINT #1, Mark$(c%);
  PRINT #1, TAB(20); USING "#####"; increment(c%) * 25.4;
  PRINT #1, TAB(33); "______";
  PRINT #1, TAB(44); "______";
  PRINT #1, TAB(55); "______";
  PRINT #1, TAB(66); "______"
 NEXT
ELSE
 FOR c% = 0 TO counter% - 1
  PRINT #1, USING "#######  "; Chart(c%);
  PRINT #1, Mark$(c%);
  PRINT #1, TAB(20); USING "#####"; increment(c%) * 25.4;
  PRINT #1, TAB(33); "______";
  PRINT #1, TAB(44); "______";
  PRINT #1, TAB(55); "______";
  PRINT #1, TAB(66); "______"
 NEXT
END IF
END IF
PRINT #1, ""
PRINT #1, STRING$(74, "-")

CLOSE 1
SLEEP
CALL printscreen


CASE ELSE
PRINT #1, "Afriso Eurogauge Ltd Calsys 5.4 ";
PRINT #1, MID$(DATE$, 4, 2) + "/" + LEFT$(DATE$, 2) + "/" + RIGHT$(DATE$, 4);
PRINT #1, " "; "      S/O: "; Wo$
PRINT #1, STRING$(74, "-")
PRINT #1, "Name: "; Cust$; "   "; "Ref: "; Custorder$
PRINT #1, ME$;
PRINT #1, "     Text on Dial: "; text$
PRINT #1, "Gauge Scale: "; scale$;
PRINT #1, USING "   Set 162 index: ##.##"; index
PRINT #1, USING "Height/Diameter  ###.## ins    "; VesselHeight;
PRINT #1, USING "Gauge Working Range  ###.## ins"; (VesselHeight - Centreline - Flange) * Density
PRINT #1, USING "Density #.#### kg/l "; Density;
PRINT #1, USING " AG C/L ##.##ins/"; Centreline;
PRINT #1, USING "####mm "; (Centreline * 25.4);
PRINT #1, USING " AG Flange ##.##ins/"; Flange;
PRINT #1, USING "###mm"; (Flange * 25.4)
PRINT #1, STRING$(74, "-")
PRINT #1, "   " + scale$;
PRINT #1, TAB(21); inc$;
PRINT #1, TAB(50); "Degrees"
PRINT #1, ""

IF fraction THEN
 FOR c% = 0 TO counter% - 1
  PRINT #1, Mask$(c%) + "  ";
  PRINT #1, Mark$(c%);
  PRINT #1, TAB(20); USING "####.#"; increment(c%);
  PRINT #1, TAB(33); "______";
  PRINT #1, TAB(44); "______";
  PRINT #1, TAB(55); "______";
  PRINT #1, TAB(66); "______"
 NEXT
ELSE
IF scale$ = "Cu Metres" THEN
 FOR c% = 0 TO counter% - 1
  PRINT #1, USING "####.##  "; Chart(c%);
  PRINT #1, Mark$(c%);
  PRINT #1, TAB(20); USING "####.#"; increment(c%);
  PRINT #1, TAB(33); "______";
  PRINT #1, TAB(44); "______";
  PRINT #1, TAB(55); "______";
  PRINT #1, TAB(66); "______"
 NEXT
ELSE
 FOR c% = 0 TO counter% - 1
  PRINT #1, USING "#######  "; Chart(c%);
  PRINT #1, Mark$(c%);
  PRINT #1, TAB(20); USING "####.##"; increment(c%);
  PRINT #1, TAB(33); "______";
  PRINT #1, TAB(44); "______";
  PRINT #1, TAB(55); "______";
  PRINT #1, TAB(66); "______"
 NEXT
END IF
END IF
PRINT #1, ""
PRINT #1, STRING$(74, "-")
PRINT #1, USING "Full @####.#" + inc$; full;
PRINT #1, USING "   Refill @####.#" + inc$; refill;
PRINT #1, USING "   Empty @####.#" + inc$; empty

CLOSE 1
SLEEP
CALL printscreen

END SELECT

RETURN

D0051:
ok% = Verify%(18, "Exit program, are you sure")
IF NOT ok% THEN GOTO D0041
GOTO egress
STOP

'**************************************************************************
'*     Error trap & program exit                                          *
'**************************************************************************
'
Trap:
Recoverable% = 0
SELECT CASE ERR
 CASE 7, 14
	ME$ = "Out of memory"
 CASE 5
	ME$ = "Illegal Function Call"
 CASE 6
	ME$ = "Overflow"
 CASE 11
	ME$ = "Divide by Zero error"
 CASE 53
	ME$ = "Data File missing"
 CASE 13
	ME$ = "Type Mismatch"
 CASE 27
	ME$ = "Printer out of Paper": Recoverable% = True
 CASE 68
	ME$ = "Device unavailable": Recoverable% = True
 CASE 64
	ME$ = "Bad File Name Error": Recoverable% = True
 CASE 24, 25
	ME$ = "Device Timeout or Fault": Recoverable% = True
 CASE 61, 67
	ME$ = "Out of disk space"
 CASE 71
	ME$ = "DISK DRIVE NOT READY": Recoverable% = True
 CASE 72
	ME$ = "Disk media error"
 CASE ELSE
	a$ = STR$(ERR): ME$ = "A type" + a$ + " Error has just occurred"
END SELECT

 IF NOT Recoverable% THEN
	ME$ = ME$ + ", Had enough and giving up  ...."
	ME% = StatusLine%(ME$)
	RESUME egress
 ELSE
	ML% = LEN(ME$): MT% = 80 - (ML% \ 2)
		IF CGA% THEN ATTR% = 78 ELSE ATTR% = 112
			CALL PopUp(9, 20, 7, 42, ATTR%, 3, 2, 1)
			CALL aprint(10, MT%, ME$, ATTR% + 128): BEEP
			ME$ = "Please correct this error if possible"
			CALL aprint(12, 22, ME$, ATTR%)
			CALL aprint(13, 30, "Press a key when ready", ATTR%)
			CALL aprint(14, 32, "or <ESC> to Abort.", ATTR%)
				DO
					K$ = INKEY$
				LOOP WHILE K$ = ""
			CALL shutup
			IF K$ = CHR$(27) THEN RESUME egress
			RESUME
 END IF
'**************************************************************************
'*     Egress                                                             *
'**************************************************************************
egress:
 LOCATE 20, 1, 1: CALL EraEos
 CLS
END

FUNCTION Attribute% (Fore%, Back%) STATIC
'**************************************************************************
'*     Attribute                                                          *
'**************************************************************************
Blink% = 0
IF Fore% < 0 OR Fore% > 31 THEN Fore% = 7
IF Back% < 0 OR Back% > 7 THEN Back% = 0
IF Fore% > 15 THEN Fore% = Fore% - 16: Blink% = -1
Temp% = (Back% * 16) + Fore%
IF Blink% THEN Temp% = Temp% + 128
Attribute% = Temp%
END FUNCTION

SUB calibrationcode (calcode%, scale$)
'**************************************************************************
'*     CalibrationCode                                                    *
'**************************************************************************
CALL PopUp(7, 63, 16, 16, 79, 1, 2, 1)
aprint 8, 64, " A - litres  ", 79
aprint 9, 64, " B - Gallons ", 79
aprint 10, 64, " C - Cu metres", 79
aprint 11, 64, " D - Cu Feet", 79
aprint 12, 64, " E - kg", 79
aprint 13, 64, " F - tonnes", 79
aprint 14, 64, " G - Tons", 79
aprint 15, 64, " H - Percent", 79
aprint 16, 64, " I - Fractions", 79
aprint 17, 64, " J - metres", 79
aprint 18, 64, " K - Feet", 79
aprint 19, 64, " L - US Galls", 79
aprint 20, 64, " M - mm ", 79
aprint 21, 64, " ", 79
CALL PopUp(20, 21, 3, 40, 79, 1, 2, 1)
aprint 21, 25, "Enter Scale Code  [A-L]", 79
COLOR 14, 4
getcode:
DO WHILE a$ = ""
 LOCATE 21, 50, 1
 a$ = INKEY$
LOOP
LOCATE 21, 50, 0
a$ = UCASE$(a$)
PRINT a$
p% = INSTR("ABCDEFGHIJKLM", a$)
calcode% = p%
SELECT CASE p%
CASE 1
 scale$ = "Litres"
CASE 2
 scale$ = "Gallons"
CASE 3
 scale$ = "Cu Metres"
CASE 4
 scale$ = "Cu Feet"
CASE 5
 scale$ = "Kg"
CASE 6
 scale$ = "Tonnes"
CASE 7
 scale$ = "Tons"
CASE 8
 scale$ = "Percent"
CASE 9
 scale$ = "Fractions"
CASE 10
 scale$ = "Metres"
CASE 11
 scale$ = "Feet"
CASE 12
 scale$ = "US Gallons"
CASE 13
 scale$ = "mm"

CASE ELSE
 SOUND 999, 1
 SOUND 500, 1
 a$ = ""
 GOTO getcode
END SELECT
aprint 2, 60, "Scale = " + scale$, 27
CALL shutup
CALL shutup
END SUB

 FUNCTION DateInput$ (D$) STATIC
'**************************************************************************
'*     DateInput$                                                         *
'**************************************************************************
SHARED Now$, abort%, Again%, Context%, Topic$
CursLeft$ = CHR$(29): CursRight$ = CHR$(28)
col% = POS(0): MaxLen% = 8: COLOR 0, 7
IF MID$(D$, 3, 1) = "/" AND MID$(D$, 6, 1) = "/" AND LEN(D$) = 8 THEN
 Buffer$ = D$
ELSE
 Buffer$ = "DD/MM/YY"
END IF
DO
Pointer% = 1: keycode% = 0: abort% = 0: Again% = 0
LOCATE , col%: PRINT Buffer$; : LOCATE , col%, 1
WHILE keycode% <> 13
DO
Z$ = INKEY$
LOOP WHILE Z$ = ""
keycode% = CVI(Z$ + CHR$(0))
SELECT CASE keycode%
CASE 15104                           ' <F1> key
HelpMate Context%, Topic$
CASE 19712                           ' Right Arrow
IF Pointer% < MaxLen% THEN
 Pointer% = Pointer% + 1
 PRINT CursRight$;
 IF Pointer% = 3 OR Pointer% = 6 THEN
Pointer% = Pointer% + 1
PRINT CursRight$;
 END IF
END IF
CASE 19200                           ' Left Arrow
IF Pointer% > 1 THEN
 Pointer% = Pointer% - 1
 PRINT CursLeft$;
 IF Pointer% = 3 OR Pointer% = 6 THEN
Pointer% = Pointer% - 1
PRINT CursLeft$;
 END IF
END IF
CASE 27                              ' Escape key
keycode% = 13: abort% = -1
CASE 48 TO 57                        ' Numeric keys
MID$(Buffer$, Pointer%, 1) = Z$
PRINT Z$; : Pointer% = Pointer% + 1
IF Pointer% = 3 OR Pointer% = 6 THEN
 Pointer% = Pointer% + 1
 PRINT CursRight$;
END IF
IF Pointer% > MaxLen% THEN
 PRINT CursLeft$; : Pointer% = Pointer% - 1
END IF
CASE ELSE
END SELECT
WEND
IF Again% THEN Buffer$ = Now$: EXIT DO
IF abort% THEN Buffer$ = SPACE$(MaxLen%): EXIT DO
Y% = VAL(RIGHT$(Buffer$, 2)): D% = VAL(LEFT$(Buffer$, 2))
M% = VAL(MID$(Buffer$, 4, 2)): T$ = LongDate$(D%, M%, Y%)
IF T$ = "" THEN
 BEEP: Valid% = 0
ELSE
 Valid% = -1
END IF
LOOP UNTIL Valid%
COLOR 7, 0: LOCATE , col%, 0: PRINT Buffer$;
DateInput$ = Buffer$
END FUNCTION

SUB decimalfeet
'**************************************************************************
'*     DecimalFeet                                                        *
'**************************************************************************
CALL PopUp(7, 63, 16, 16, 27, 1, 2, 1)
aprint 8, 68, "Decimal", 27
aprint 9, 68, "Inches", 27
aprint 11, 65, " 1" + CHR$(34) + " = 0.083", 30
aprint 12, 65, " 2" + CHR$(34) + " = 0.166", 30
aprint 13, 65, " 3" + CHR$(34) + " = 0.250", 30
aprint 14, 65, " 4" + CHR$(34) + " = 0.333", 30
aprint 15, 65, " 5" + CHR$(34) + " = 0.416", 30
aprint 16, 65, " 6" + CHR$(34) + " = 0.500", 30
aprint 17, 65, " 7" + CHR$(34) + " = 0.583", 30
aprint 18, 65, " 8" + CHR$(34) + " = 0.666", 30
aprint 19, 65, " 9" + CHR$(34) + " = 0.750", 30
aprint 20, 64, " 10" + CHR$(34) + " = 0.833", 30
aprint 21, 64, " 11" + CHR$(34) + " = 0.916", 30
END SUB

SUB dialtext (text$)
'**************************************************************************
'*     DialText                                                           *
'**************************************************************************
 CALL PopUp(19, 10, 4, 60, 30, 1, 2, 1)
 aprint 20, 20, "Enter Dial Marking or <ENTER> to continue", 30
LOCATE 21, 20, 0
mx% = 16
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
text$ = a$
CALL shutup
END SUB

SUB getangle (analog, mFormat$, offset)
'**************************************************************************
'*     GetAngle                                                           *
'**************************************************************************
SHARED choice%
LOCATE , , 0: Context% = 0: Topic$ = ""
ScrollUp 7, 20, 18, 61, 0, 0
aprint 6, 31, "���������������Ŀ", 15
aprint 7, 31, "�  METER  MENU  �", 15
aprint 8, 31, "�����������������", 15
menu% = 3: row% = 9: col% = 28: menu$(0) = "123"
menu$(1) = "1. 240 Degree Scale"
menu$(2) = "2. 90 Degree Scale"
menu$(3) = "3. 4-20mA Output."
MenuDriver row%, col%, menu%
SELECT CASE choice%
 CASE IS = 1
analog = 240
mFormat$ = " deg  "
offset = 0
 CASE IS = 2
analog = 90
mFormat$ = " deg  "
offset = 0


 CASE IS = 3
 LOCATE 14, 10, 0
 LINE INPUT "Enter Start mA: "; offset$   'ma$
 mFormat$ = " mA  "
 LOCATE 15, 10, 0
 LINE INPUT "Enter End mA  : "; ma$    'offset$

 analog = VAL(ma$) - VAL(offset$)
 offset = VAL(offset$)
END SELECT



CALL shutup
END SUB

DEFSNG Z
SUB mainscreen
'**************************************************************************
'*     MainScreen                                                         *
'**************************************************************************
SHARED ME$
CLS
Panel2 1, 1, 3, 80, 30
aprint 2, (80 - LEN(ME$)) / 2, ME$, 30
aprint 25, 1, STRING$(80, 32), 0

END SUB

SUB MenuDriver (row%, col%, menu%)
'**************************************************************************
'*     MenuDriver                                                         *
'**************************************************************************
SHARED abort%, choice%, CGA%
abort% = 0: choice% = 1
IF CGA% THEN Colour% = 30 ELSE Colour% = 112
FOR I% = 1 TO menu%
IL$ = LEFT$(menu$(I%), 1): LN% = row% + I%
aprint LN%, col%, menu$(I%), 7
aprint LN%, col%, IL$, 15
NEXT I%
M1:
LN% = row% + choice%: aprint LN%, col%, menu$(choice%), Colour%
M2:
DO
 K$ = INKEY$
LOOP WHILE K$ = ""
IF LEN(K$) > 1 THEN GOTO M3
K% = ASC(K$)
IF K% = 13 THEN GOTO M5
IF K% = 27 THEN abort% = -1: GOTO M5
K$ = UCASE$(K$)
M% = INSTR(menu$(0), K$)
IF M% > 0 THEN GOTO M4
GOTO M2
M3:
LS% = choice%: Scan% = ASC(MID$(K$, 2, 1))
IF Scan% = 59 THEN
 Topic$ = "gauge" + LTRIM$(RTRIM$(STR$(choice%)))
 HelpMate 1, Topic$
 GOTO M2
END IF
IF Scan% = 80 THEN choice% = choice% + 1
IF choice% > menu% THEN choice% = 1
IF Scan% = 72 THEN choice% = choice% - 1
IF choice% < 1 THEN choice% = menu%
IF choice% = LS% THEN GOTO M2
LN% = row% + LS%
aprint LN%, col%, menu$(LS%), 7
IL$ = LEFT$(menu$(LS%), 1)
aprint LN%, col%, IL$, 15
GOTO M1
M4:
LS% = choice%: choice% = M%
IF choice% = LS% THEN GOTO M5
LN% = row% + LS%
aprint LN%, col%, menu$(LS%), 7
IL$ = LEFT$(menu$(LS%), 1)
aprint LN%, col%, IL$, 15
LN% = row% + choice%
aprint LN%, col%, menu$(choice%), Colour%
M5:
END SUB

FUNCTION numcheck$ (BF$, numset$) STATIC
'**************************************************************************
'*     NumChecks                                                          *
'**************************************************************************
I% = 1
j% = 0
lena% = LEN(BF$)
T$ = BF$
DO UNTIL I% >= lena%
 c$ = MID$(BF$, I%, 1)
 IF INSTR(numset$, c$) THEN
j% = j% + 1
MID$(T$, j%, 1) = c$
 END IF
 I% = I% + 1
LOOP
numcheck$ = LEFT$(T$, j%)
T$ = ""
END FUNCTION

SUB pop
'**************************************************************************
'*     Pop                                                                *
'**************************************************************************
CALL PopUp(7, 21, 10, 40, 27, 1, 2, 1)
END SUB

DEFDBL Z
SUB printscreen
'**************************************************************************
'*     PrintScreen                                                        *
'**************************************************************************
ok% = True%
DO UNTIL NOT ok%
 ok% = Verify%(5, "Print Calibration Table    ")
IF ok% THEN
EXIT DO
 ELSE
GOTO reject
 END IF
LOOP
ok% = True%

OPEN Wo$ + ".cal" FOR INPUT AS 1
DO UNTIL EOF(1)
 LINE INPUT #1, Line$
 LPRINT "    "; Line$
LOOP
LPRINT CHR$(27) + "E"
CLOSE 1
reject:
END SUB

DEFSNG Z
FUNCTION RevInput$ (mx%, a$) STATIC
'**************************************************************************
'*     RevInputs                                                          *
'**************************************************************************
SHARED abort%, Context%, Topic$
CL$ = CHR$(29): CR$ = CHR$(28)
IF a$ = "" THEN
 BF$ = SPACE$(mx%)
ELSE
 BF$ = LEFT$(a$ + SPACE$(mx%), mx%)
END IF
abort% = 0: PR% = 1: KC% = 0: IT% = 0: EN% = 0
COLOR 14, 0: col% = POS(0): PRINT BF$; : LOCATE , col%, 1
WHILE KC% <> 13
DO
Z$ = INKEY$                 ' Get keyboard character
 
 LOOP WHILE Z$ = ""
KC% = ASC(Z$)
IF (LEN(Z$) > 1) THEN ' If the first character was null then a
 SELECT CASE ASC(MID$(Z$, 2, 1)) ' function key was pressed.
 CASE 59                      ' <F1> key
 HelpMate Context%, Topic$
 KC% = 0
 CASE 77                      ' Right Arrow
 KC% = 4
 IF PR% < mx% THEN
PR% = PR% + 1
PRINT CR$;
 END IF
 CASE 75                      ' Left Arrow
 KC% = 19
 IF PR% > 1 THEN
PR% = PR% - 1
PRINT CL$;
 END IF
 CASE 83                      ' Delete Key
 KC% = 7
 BF$ = LEFT$(BF$, PR% - 1) + RIGHT$(BF$, mx% - PR%) + " "
 LOCATE , col%: PRINT BF$;
 LOCATE , (col% - 1) + PR%
 CASE 82                      ' Insert Key
 KC% = 22: IT% = NOT IT%
 CASE 117                     ' <CTRL> <END>
 KC% = 24
 BF$ = LEFT$(BF$, PR% - 1) + SPACE$(mx% - PR%) + " "
 LOCATE , col%: PRINT BF$;
 LOCATE , (col% - 1) + PR%
 CASE ELSE                    ' Everything else
 KC% = 0
 END SELECT
END IF
IF Z$ = CHR$(27) THEN           ' Escape key
 KC% = 13: abort% = True:
END IF
IF Z$ = CHR$(8) THEN            ' BackSpace key
 IF PR% > 1 THEN
MID$(BF$, PR%, 1) = " ": PR% = PR% - 1
PRINT CL$; " "; CL$;
 ELSE
BEEP
 END IF
END IF
IF ASC(Z$) > 31 THEN            ' Printable characters
 IF NOT IT% THEN
MID$(BF$, PR%, 1) = Z$
PRINT Z$; : PR% = PR% + 1
IF PR% > mx% THEN
 PRINT CL$; : PR% = PR% - 1
END IF
 ELSE
IF PR% < mx% THEN
 BF$ = LEFT$(BF$, PR% - 1) + Z$ + RIGHT$(BF$, mx% - (PR% - 1))
 BF$ = LEFT$(BF$, mx%): LOCATE , col%: PRINT BF$;
 LOCATE , col% + PR%: PR% = PR% + 1
ELSE
 BEEP
END IF
 END IF
END IF
WEND
IF abort% THEN BF$ = SPACE$(mx%)
COLOR 15, 1: LOCATE , col%, 0: PRINT BF$;
RevInput$ = BF$
END FUNCTION

SUB unitstype (feet%, unit$, dense$) STATIC
'**************************************************************************
'*     UnitsType                                                          *
'**************************************************************************
ok% = Verify%(19, "Enter Measurements in DECIMAL FEET ")
IF ok% THEN
 CALL decimalfeet
 feet% = -1
 unit$ = "Feet"
 dense$ = "Lb/gal"
ELSE
 feet% = 0
 unit$ = "metres"
 dense$ = "Kg/litre"
END IF
END SUB

DEFDBL Z
SUB WorksOrder (Wo$, Cust$, Custorder$)
'**************************************************************************
'*     WorksOrder                                                         *
'**************************************************************************
SHARED abort%
CALL PopUp(12, 10, 3, 60, 30, 1, 2, 1)
aprint 13, 15, "Enter Sales Order Reference:           or ESC to Quit ", 30
LOCATE 13, 45, 0
mx% = 8
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
Wo$ = a$
IF abort% THEN COLOR 7, 0: CLS : END
CALL shutup
IF a$ = "" THEN CALL WorksOrder(Wo$, Cust$, Custorder$)
'**************************************************************************
CALL PopUp(12, 10, 3, 68, 30, 1, 2, 1)
aprint 13, 15, "Enter Customer Name:                           or ESC to Quit ", 30
LOCATE 13, 36, 0
mx% = 20
a$ = ""
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
Cust$ = a$
IF abort% THEN COLOR 7, 0: CLS : END
CALL shutup
'**************************************************************************
CALL PopUp(12, 10, 3, 68, 30, 1, 2, 1)
aprint 13, 15, "Enter Customer Ref :                           or ESC to Quit ", 30
LOCATE 13, 36, 0
mx% = 16
a$ = ""
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
Custorder$ = a$
IF abort% THEN COLOR 7, 0: CLS : END
CALL shutup


END SUB

