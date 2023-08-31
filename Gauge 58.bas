'**************************************************************************
'*   title: Gauge Calibration version 5.8                                 *
'*                                                                        *
'*                                                                        *
'*   last amended:16/02/11 (c) dcw 1990-2011                              *
'**************************************************************************
'**************************************************************************
'*   Constants                                                            *
'**************************************************************************
DECLARE SUB header ()
DECLARE SUB DishedEndDepth ()

CONST false = 0
CONST True = NOT false
CONST PI = 3.1415927#
CONST MillibarConversion = 2.4908
CONST A270 = 270
CONST A180 = 180
CONST A90 = 90
CONST C254 = 25.4

'**************************************************************************
'*     External Functions & Procedures                                    *
'**************************************************************************

DECLARE FUNCTION Page% (NewPage%)
DECLARE FUNCTION StatusLine% (Message$)
DECLARE FUNCTION Verify% (row%, Message$)
DECLARE FUNCTION RevInput$ (mx%, a$)
DECLARE FUNCTION LongDate$ (Day%, Month%, Year%)
DECLARE SUB aprint (row%, col%, Message$, Attrib%)
DECLARE SUB EraEos ()
DECLARE SUB Panel2 (row%, col%, Rows%, Cols%, Attrib%)
DECLARE SUB PopUp (Y%, x%, H%, W%, c%, B%, S%, Z%)
DECLARE SUB ScrollUp (TY%, TX%, BY%, BX%, Lines%, Colour%)
DECLARE SUB shutup ()
DECLARE SUB screenprint ()
DECLARE SUB MainScreen ()
DECLARE SUB Pop ()
DECLARE SUB dialtext (text$)
DECLARE SUB printscreen ()
DECLARE SUB MenuDriver (row%, col%, menu%)
DECLARE SUB calibrationcode (calcode%, scale$)
DECLARE SUB WorksOrder (wo$, Cust$, Custorder$)
DECLARE SUB getangle (analog, mFormat$, offset)
DECLARE SUB DishedEndDepth ()
DECLARE SUB DishedEnds ()
DECLARE SUB TotalVol ()
DECLARE SUB PartVol ()


DIM Chart(100)
DIM Mark$(100)
DIM Mask$(50)
DIM increment(100)
DIM hdratio(1005)
DIM areafactor(1005)
DIM SHARED menu$(0 TO 8)
DIM SHARED wo$
DIM SHARED Cust$
DIM SHARED Custorder$
DIM SHARED line$

DEFDBL Z
ON ERROR GOTO Trap

FOR c% = 0 TO 100
 Mark$(c%) = " "
NEXT

TotalVolume = 0

feet% = 0
unit$ = "metres"
dense$ = "kg/l"
ul$ = "______"
fm1$ = "  ##### mm"
fm2$ = " ###.# deg  "
fm3$ = " #### mm"
fm4$ = " #### mm "
fm5$ = " ###### mm "
fm6$ = " ###### mm  "
fm7$ = " #.#### kg/l"
fm8$ = " ###.## mbar"
fm9$ = "##### mm"
line$ = STRING$(74, "_")

fcount% = 1

OPEN "hdtable.dat" FOR INPUT AS 1
DO UNTIL fcount% = 1002
	INPUT #1, hdratio(fcount%), areafactor(fcount%)
	fcount% = fcount% + 1
LOOP
CLOSE 1

'**************************************************************************
'*     Main Menu                                                          *
'**************************************************************************

MenuBegin:
	CGA% = Page%(0): LP% = 1: 'DOS$ = "DOS " + DosVersion$
	DY$ = MID$(DATE$, 4, 2): DY% = VAL(DY$): MO$ = LEFT$(DATE$, 2)
	MO% = VAL(MO$): YR$ = RIGHT$(DATE$, 2): YR% = VAL(YR$)
	Now$ = DY$ + "/" + MO$ + "/" + YR$: Today$ = LongDate$(DY%, MO%, YR%)
	fcount% = 1
	CLS
	ME$ = " Afriso Eurogauge Ltd                         Calibration System Version 5.8"
	CALL MainScreen
	CALL WorksOrder(wo$, Cust$, Custorder$)
	CALL shutup
	CALL shutup
	CALL shutup
	COLOR 7, 0

'**************************************************************************

d001:
	LOCATE , , 0: Context% = 0: Topic$ = ""
	Panel2 1, 1, 3, 80, 30
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
	aprint 6, 30, "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿", 15
	aprint 7, 30, "³   TANK  MENU   ³", 15
	aprint 8, 30, "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ", 15
	menu% = 7: row% = 9: col% = 23: menu$(0) = "1234567"
	menu$(1) = "1. Rectangular"
	menu$(2) = "2. Horizontal Cylindrical"
	menu$(3) = "3. Vertical Cylindrical"
	menu$(4) = "4. Capacity/Height"
	menu$(5) = "5. Capacity/Diameter"
	menu$(6) = "6. Dished End Depth & Volume"
	menu$(7) = "7. Exit from Program"

'**************************************************************************

D004:
	MenuDriver row%, col%, menu%
	IF abort% THEN GOTO D005
	SELECT CASE Choice%

		CASE IS = 1
			tank% = 1
			GOTO menu2

		CASE IS = 2
			tank% = 2
			GOTO menu2

		CASE IS = 3
			tank% = 3
			GOTO menu2

		CASE IS = 4
			tank% = 4
			GOTO menu2

		CASE IS = 5
			tank% = 5
			GOTO menu2

		CASE IS = 6
			CALL DishedEnds
			CALL shutup
			COLOR 7, 0
			GOTO D003
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
	aprint 6, 30, "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿", 15
	aprint 7, 30, "³  CALIBRATION   ³", 15
	aprint 8, 30, "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ", 15
	menu% = 5: row% = 9: col% = 23: menu$(0) = "12345"
	menu$(1) = "1. Scale in millibars (Tankmate/Unimat)"
	menu$(2) = "2. Scale in degrees   (Unitel/Unitop)  "
	menu$(3) = "3. Scale 4-20mA       (Analogue Meter) "
	menu$(4) = "4. Dipchart in mm     (Tankmate 5)     "
	menu$(5) = "5. Exit Program"

'**************************************************************************

D0041:
	MenuDriver row%, col%, menu%
	IF abort% THEN GOTO d001
	SELECT CASE Choice%

		CASE IS = 1
			gauge% = 1
			accuracy = 2   ' 2%

		CASE IS = 2
			gauge% = 2
			accuracy = 3   ' 3%

		CASE IS = 3
			gauge% = 3
			accuracy = 0
			CALL getangle(analog, mFormat$, offset)

		CASE IS = 4
			gauge% = 4
			accuracy = 0


		CASE IS = 5
			accuracy = 0

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
			ME$ = "Hydrostatic:Rectangular"
			CALL MainScreen
			CALL Pop
			GOSUB RectangularCalc
			CALL calibrationcode(calcode%, scale$)
			GOSUB volume
			GOSUB convertvolume
			GOSUB getchart
			GOSUB RectangularTable
			GOSUB printchart
'**************************************************************************
		CASE 12
			ME$ = "Pneumatic:Rectangular"
			CALL MainScreen
			CALL Pop
			GOSUB RectangularCalc
			CALL calibrationcode(calcode%, scale$)
			GOSUB volume
			GOSUB convertvolume
			GOSUB getchart
			GOSUB RectangularTable
			GOSUB printchart
'**************************************************************************
		CASE 13
			ME$ = "Analog Meter:Rectangular"
			CALL MainScreen
			CALL Pop
			GOSUB RectangularCalc
			CALL calibrationcode(calcode%, scale$)
			GOSUB volume
			GOSUB convertvolume
			GOSUB getchart
			GOSUB RectangularTable
			GOSUB printchart
'**************************************************************************
		CASE 14
			ME$ = "Dipchart:Rectangular"
			CALL MainScreen
			CALL Pop
			GOSUB RectangularCalc
			CALL calibrationcode(calcode%, scale$)
			GOSUB volume
			GOSUB convertvolume
			GOSUB getchart
			GOSUB RectangularTable
			GOSUB printchart
'**************************************************************************
		CASE 21
			ME$ = "Hydrostatic:Horizontal Cylinder"
			CALL MainScreen
			CALL Pop
			GOSUB HorizontalCalc
			CALL calibrationcode(calcode%, scale$)
			GOSUB volume
			GOSUB convertvolume
			GOSUB getchart
			GOSUB DiameterTable
			GOSUB printchart
'**************************************************************************
		CASE 22
			ME$ = "Pneumatic:Horizontal Cylinder"
			CALL MainScreen
			CALL Pop
			GOSUB HorizontalCalc
			CALL calibrationcode(calcode%, scale$)
			GOSUB volume
			GOSUB convertvolume
			GOSUB getchart
			GOSUB DiameterTable
			GOSUB printchart
'**************************************************************************
		CASE 23
			ME$ = "Analog Meter:Horizontal Cylinder"
			CALL MainScreen
			CALL Pop
			GOSUB HorizontalCalc
			CALL calibrationcode(calcode%, scale$)
			GOSUB volume
			GOSUB convertvolume
			GOSUB getchart
			GOSUB DiameterTable
			GOSUB printchart
'**************************************************************************
		CASE 24
			ME$ = "Dipchart:Horizontal Cylinder"
			CALL MainScreen
			CALL Pop
			GOSUB HorizontalCalc
			CALL calibrationcode(calcode%, scale$)
			GOSUB volume
			GOSUB convertvolume
			GOSUB getchart
			GOSUB DiameterTable
			GOSUB printchart
'**************************************************************************
		CASE 31
			ME$ = "Hydrostatic:Vertical Cylinder"
			CALL MainScreen
			CALL Pop
			GOSUB VerticalCalc
			CALL calibrationcode(calcode%, scale$)
			GOSUB volume
			GOSUB convertvolume
			GOSUB getchart
			GOSUB RectangularTable
			GOSUB printchart
'**************************************************************************
		CASE 32
			ME$ = "Pneumatic:Vertical Cylinder"
			CALL MainScreen
			CALL Pop
			GOSUB VerticalCalc
			CALL calibrationcode(calcode%, scale$)
			GOSUB volume
			GOSUB convertvolume
			GOSUB getchart
			GOSUB RectangularTable
			GOSUB printchart
'**************************************************************************
		CASE 33
			ME$ = "Analog Meter:Vertical Cylinder"
			CALL MainScreen
			CALL Pop
			GOSUB VerticalCalc
			CALL calibrationcode(calcode%, scale$)
			GOSUB volume
			GOSUB convertvolume
			GOSUB getchart
			GOSUB RectangularTable
			GOSUB printchart
'**************************************************************************
		CASE 34
			ME$ = "Dipchart:Vertical Cylinder"
			CALL MainScreen
			CALL Pop
			GOSUB VerticalCalc
			CALL calibrationcode(calcode%, scale$)
			GOSUB volume
			GOSUB convertvolume
			GOSUB getchart
			GOSUB RectangularTable
			GOSUB printchart
'**************************************************************************
		CASE 41
			ME$ = "Hydrostatic:Capacity/Height"
			CALL MainScreen
			CALL Pop
			GOSUB Capacity
			CALL calibrationcode(calcode%, scale$)
			GOSUB convertvolume
			GOSUB getchart
			GOSUB RectangularTable
			GOSUB printchart
'**************************************************************************
		CASE 42
			ME$ = "Pneumatic:Capacity/Height"
			CALL MainScreen
			CALL Pop
			GOSUB Capacity
			CALL calibrationcode(calcode%, scale$)
			GOSUB convertvolume
			GOSUB getchart
			GOSUB RectangularTable
			GOSUB printchart
'**************************************************************************
		CASE 43
			ME$ = "Analog Meter:Capacity/Height"
			CALL MainScreen
			CALL Pop
			GOSUB Capacity
			CALL calibrationcode(calcode%, scale$)
			GOSUB convertvolume
			GOSUB getchart
			GOSUB RectangularTable
			GOSUB printchart
'**************************************************************************
		CASE 44
			 ME$ = "Dipchart:Capacity/Height"
			 CALL MainScreen
			 CALL Pop
			 GOSUB Capacity
			 CALL calibrationcode(calcode%, scale$)
			 GOSUB convertvolume
			 GOSUB getchart
			 GOSUB RectangularTable
			 GOSUB printchart
'**************************************************************************
		CASE 51
			 ME$ = "Hydrostatic:Capacity/Diameter"
			 CALL MainScreen
			 CALL Pop
			 GOSUB Capacity
			 CALL calibrationcode(calcode%, scale$)
			 GOSUB convertvolume
			 GOSUB getchart
			 GOSUB DiameterTable
			 GOSUB printchart
'**************************************************************************
		CASE 52
			 ME$ = "Pneumatic:Capacity/Diameter"
			 CALL MainScreen
			 CALL Pop
			 GOSUB Capacity
			 CALL calibrationcode(calcode%, scale$)
			 GOSUB convertvolume
			 GOSUB getchart
			 GOSUB DiameterTable
			 GOSUB printchart
'**************************************************************************
		CASE 53
			 ME$ = "Analog Meter:Capacity/Diameter"
			 CALL MainScreen
			 CALL Pop
			 GOSUB Capacity
			 CALL calibrationcode(calcode%, scale$)
			 GOSUB convertvolume
			 GOSUB getchart
			 GOSUB DiameterTable
			 GOSUB printchart
'**************************************************************************
		CASE 54
			 ME$ = "Dipchart:Capacity/Diameter"
			 CALL MainScreen
			 CALL Pop
			 GOSUB Capacity
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

SELECT CASE cal%

'**************************************************************************
'* SS160  Height *
'*****************

	CASE IS = 41
		aprint 25, 1, STRING$(80, 32), 31
		aprint 9, 24, "Tank Capacity     :          ", 31
		aprint 10, 24, "Tank Height       :          " + unit$, 31
		aprint 11, 24, "Density           :          " + dense$, 31
		IF cal% = 41 THEN
			aprint 13, 24, "Outlet Centreline :          " + unit$, 31
			aprint 14, 24, "C/L to Transmitter:          " + "mm", 31
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
		aprint 25, 10, "Enter Transmitter Height from Centreline  ", 30
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
		aprint 11, 24, "Density           :          " + dense$, 31
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
		aprint 25, 10, "Enter Density of Contents                      ", 30
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
		aprint 11, 24, "Density           :          " + dense$, 31
		aprint 13, 24, "Outlet Centreline :          " + unit$, 31
		aprint 14, 24, "C/L to Transmitter:          " + "mm", 31
		DO UNTIL abort%
		LOCATE 9, 44, 0
		aprint 25, 10, "Enter Tank Capacity                              ", 30
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
		aprint 25, 10, "Enter Transmitter Height from Centreline  ", 30
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
		aprint 11, 24, "Density           :          " + dense$, 31
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
total = Capacity
RETURN

'**************************************************************************
'*     RectangularCalc                                                    *
'**************************************************************************

RectangularCalc:
	aprint 25, 1, STRING$(80, 32), 31
	aprint 8, 24, "Tank Length       :          " + unit$, 31
	aprint 9, 24, "Tank Width        :          " + unit$, 31
	aprint 10, 24, "Tank Height       :          " + unit$, 31
	aprint 11, 24, "Density           :          " + dense$, 31
	IF cal% = 11 THEN
	aprint 13, 24, "Outlet Centreline:          " + unit$, 31
	aprint 14, 24, "C/L to Transmitter:         " + "mm", 31
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
	aprint 25, 10, "Enter Transmitter Height from Centreline  ", 30
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
	aprint 25, 1, STRING$(80, 32), 31
	aprint 8, 24, "Tank Length       :          " + unit$, 31
	aprint 9, 24, "Tank Diameter     :          " + unit$, 31
	aprint 10, 24, "Dish 1 Depth      :          " + unit$, 31
	aprint 11, 24, "Dish 2 Depth      :          " + unit$, 31
	aprint 12, 24, "Density           :          " + dense$, 31
	IF cal% = 21 THEN
	aprint 14, 24, "Outlet Centreline :          " + unit$, 31
	aprint 15, 24, "C/L to Transmitter:          " + "mm", 31
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
	Diameter = VAL(a$)
	LOCATE 9, 44, 0
	PRINT USING "###.###"; Diameter
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
	aprint 25, 10, "Enter Transmitter Height from Centreline  ", 30
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
	aprint 25, 1, STRING$(80, 32), 31
	aprint 8, 24, "Tank Height      :          " + unit$, 31
	aprint 9, 24, "Tank Diameter    :          " + unit$, 31
	aprint 10, 24, "Bottom Dish      :          " + unit$, 31
	aprint 11, 24, "Density          :          " + dense$, 31
	IF cal% = 31 THEN
	aprint 13, 24, "Outlet Centreline:          " + unit$, 31
	aprint 14, 24, "C/L to Transmitter:         " + "mm", 31
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
	Diameter = VAL(a$)
	LOCATE 9, 44, 0
	PRINT USING "###.###"; Diameter
	IF abort% THEN EXIT DO
	LOCATE 10, 44, 0
	aprint 25, 10, "Enter Tank Bottom Dish                          ", 30
	mx% = 7
	a$ = Olddish$
	a$ = RevInput$(mx%, a$)
	a$ = LTRIM$(RTRIM$(a$))
	Olddish$ = a$
	Dish = VAL(a$)
	LOCATE 10, 44, 0
	PRINT USING "###.###"; Dish
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
	aprint 25, 10, "Enter Transmitter Height from Centreline  ", 30
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
	Radius = Diameter * .5
	pi6 = PI / 6
	pir2 = PI * (Radius ^ 2)
	SELECT CASE cal%
		CASE 11
			volume = length * breadth * height
		CASE 12, 13, 14
			volume = length * breadth * height
		CASE 21
			dish1volume = (pi6 * dish1) * (3 * (Radius ^ 2)) + (dish1 ^ 2)
			dish2volume = (pi6 * dish2) * (3 * (Radius ^ 2)) + (dish2 ^ 2)
			volume = (pir2 * length) + dish1volume + dish2volume
		CASE 22, 23, 24
			dish1volume = (pi6 * dish1) * (3 * (Radius ^ 2)) + (dish1 ^ 2)
			dish2volume = (pi6 * dish2) * (3 * (Radius ^ 2)) + (dish2 ^ 2)
			volume = (pir2 * length) + dish1volume + dish2volume
		CASE 31
			DishVolume = (pi6 * Dish) * (3 * (Radius ^ 2)) + (Dish ^ 2)
			volume = (pir2 * height) + DishVolume
		CASE 32, 33, 34
			DishVolume = (pi6 * Dish) * (3 * (Radius ^ 2)) + (Dish ^ 2)
			volume = (pir2 * height) + DishVolume
	END SELECT
RETURN

'*************************************************************************
'*     Convertvolume                                                     *
'*************************************************************************

convertvolume:

	SELECT CASE cal%
		 CASE 11, 12, 13, 14, 41, 42, 43, 44, 51, 52, 53, 54
			VesselHeight = height
		 CASE 21, 22, 23, 24
			VesselHeight = Diameter
		 CASE 31, 32, 33, 34
			VesselHeight = height + Dish

	END SELECT

'**************************************************************************
'* Unit Select *
'***************

 VesselHeight = VesselHeight * 1000 * .03937
 Centreline = Centreline * 1000 * .03937
 Flange = Flange * .03937

'**************************************************************************
SELECT CASE cal%
	CASE 41, 42, 51, 52
		RETURN
	CASE 43, 53, 44, 54
		total = Capacity
	RETURN

		CASE ELSE

	SELECT CASE calcode%

	CASE 1  'Litres
			total = ((volume * 1000))

	CASE 2  'Gallons
			total = (((volume * 1000) / 4.546))

	CASE 3  'Cu Metres
			total = volume

	CASE 4  'Cu Feet
			total = volume * 35.31

	CASE 5  'Kg
			total = (volume * 1000) * Density

	CASE 6  'Tonnes
			total = ((volume * 1000) * Density) * .001

	CASE 7  'Tons
			total = ((volume * 1000) * Density) * .0009

	CASE 8  'Percent
			total = 100

	CASE 9  'Fractions
			scale$ = "Fractions"
			total = 1

	CASE 10 'Metres
			total = CINT(VesselHeight * .0254)

	CASE 11 'Feet
			total = VesselHeight / 12

	CASE 12 'US Gallons
			total = (((volume * 1000) / 4.546) / .8327)

	CASE 13 'MM
			total = (VesselHeight * .0254) * 1000

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
			Z = total * 1000
			IF Z = 0 THEN RETURN

				SELECT CASE total
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

			Table$ = STR$(Z)
			Table$ = LTRIM$(RTRIM$(Table$))
			Table$ = LEFT$(Table$, 2)
			p% = INSTR(Table$, ".")
			DO UNTIL EOF(1)
				LINE INPUT #1, Chart$
				IF LEFT$(Chart$, 2) = Table$ THEN
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
			IF Chart(counter% - 1) <> total THEN
				Chart(counter%) = total
				Mark$(counter%) = "M"
				Mark$(counter% - 1) = ""
				counter% = counter% + 1
			END IF
			IF fraction = false OR percent = false THEN
				LOCATE 5, 23, 0
				PRINT " Calibration range is ";
				PRINT USING format$; total;
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
'* 160 *
'*******

		CASE 21, 51
			startmark% = 1
			halfdone% = True
			FOR Mark% = 1 TO 1000
				increment(Mark%) = Chart(Mark%) / Lastmark
				IF Chart(Mark%) > (Lastmark / 2) THEN EXIT FOR
				FOR Table% = startmark% TO 1001
					COLOR 14, 1
					LOCATE 2, 5, 0: PRINT Table%
					areafactor = increment(Mark%)
					lessareafactor = areafactor(Table% - 1)
					moreareafactor = areafactor(Table% + 1)
					IF areafactor > lessareafactor AND areafactor <= moreareafactor THEN
						hdratio = hdratio(Table%)
						startmark% = Table%
						EXIT FOR
					END IF
				NEXT Table%
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
				FOR Table% = startmark% TO 1 STEP -1
					COLOR 14, 1
					LOCATE 2, 5, 0: PRINT Table%
					areafactor = increment(Mark%)
					lessareafactor = areafactor(Table% - 1)
					moreareafactor = areafactor(Table% + 1)
					IF areafactor > lessareafactor AND areafactor < moreareafactor THEN
						hdratio = hdratio(Table%)
						startmark% = Table%
						EXIT FOR
					END IF
				NEXT Table%
					increment(Mark%) = (((VesselHeight - (hdratio * VesselHeight)) - Centreline) - Flange) * Density * MillibarConversion
			NEXT Mark%

'**************************************************************************
'* Dipchart *
'************

		CASE 24, 54
			startmark% = 1
			halfdone% = True
			FOR Mark% = 1 TO 1000
				increment(Mark%) = Chart(Mark%) / Lastmark
				IF Chart(Mark%) > (Lastmark / 2) THEN EXIT FOR
				FOR Table% = startmark% TO 1001
					COLOR 14, 1
					LOCATE 2, 5, 0: PRINT Table%
					areafactor = increment(Mark%)
					lessareafactor = areafactor(Table% - 1)
					moreareafactor = areafactor(Table% + 1)
					IF areafactor > lessareafactor AND areafactor <= moreareafactor THEN
						hdratio = hdratio(Table%)
						startmark% = Table%
						EXIT FOR
					END IF
				NEXT Table%
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
					FOR Table% = startmark% TO 1 STEP -1
						COLOR 14, 1
						LOCATE 2, 5, 0: PRINT Table%
						areafactor = increment(Mark%)
						lessareafactor = areafactor(Table% - 1)
						moreareafactor = areafactor(Table% + 1)
						IF areafactor > lessareafactor AND areafactor < moreareafactor THEN
							hdratio = hdratio(Table%)
							startmark% = Table%
							EXIT FOR
						END IF
					NEXT Table%
					increment(Mark%) = VesselHeight - (hdratio * VesselHeight)
			 NEXT Mark%

'**************************************************************************
'* 162 *
'*******

		CASE 22, 52
			startmark% = 1
			increment(0) = 0 '45
			FOR Mark% = 1 TO 1000
				increment(Mark%) = Chart(Mark%) / Lastmark
				IF Chart(Mark%) > (Lastmark / 2) THEN EXIT FOR
				FOR Table% = startmark% TO 1001
					COLOR 14, 1
					LOCATE 2, 5, 0: PRINT Table%
					areafactor = increment(Mark%)
					lessareafactor = areafactor(Table% - 1)
					moreareafactor = areafactor(Table% + 1)
					IF areafactor > lessareafactor AND areafactor <= moreareafactor THEN
						hdratio = hdratio(Table%)
						startmark% = Table%
					EXIT FOR
					END IF
				NEXT Table%
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
				FOR Table% = startmark% TO 1 STEP -1
					COLOR 14, 1
					LOCATE 2, 5, 0: PRINT Table%
					areafactor = increment(Mark%)
					lessareafactor = areafactor(Table% - 1)
					moreareafactor = areafactor(Table% + 1)
					IF areafactor > lessareafactor AND areafactor < moreareafactor THEN
						hdratio = hdratio(Table%)
						startmark% = Table%
						EXIT FOR
					END IF
				NEXT Table%
				increment(Mark%) = (hdratio * 270)
				increment(Mark%) = 270 - increment(Mark%)     '315
			NEXT Mark%

'**************************************************************************
'* Analog Meter *
'****************

		CASE 23, 53
			 startmark% = 1
			 increment(0) = offset
			 FOR Mark% = 1 TO 1000
				increment(Mark%) = Chart(Mark%) / Lastmark
				IF Chart(Mark%) > (Lastmark / 2) THEN EXIT FOR
				FOR Table% = startmark% TO 1001
					COLOR 14, 1
					LOCATE 2, 5, 0: PRINT Table%
					areafactor = increment(Mark%)
					lessareafactor = areafactor(Table% - 1)
					moreareafactor = areafactor(Table% + 1)
					IF areafactor > lessareafactor AND areafactor <= moreareafactor THEN
						hdratio = hdratio(Table%)
						startmark% = Table%
						EXIT FOR
					END IF
				NEXT Table%
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
				FOR Table% = startmark% TO 1 STEP -1
					COLOR 14, 1
					LOCATE 2, 5, 0: PRINT Table%
					areafactor = increment(Mark%)
					lessareafactor = areafactor(Table% - 1)
					moreareafactor = areafactor(Table% + 1)
					IF areafactor > lessareafactor AND areafactor < moreareafactor THEN
						hdratio = hdratio(Table%)
						startmark% = Table%
						EXIT FOR
					END IF
				NEXT Table%
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
PRINT "S/O: "; wo$; "    "; Cust$; "   Ref: "; Custorder$; "    ";
PRINT MID$(DATE$, 4, 2) + "/" + LEFT$(DATE$, 2) + "/" + RIGHT$(DATE$, 4)

LOCATE 25, 1, 0
PRINT ME$; "  Scale: "; scale$;
LOCATE 25, 49, 0
PRINT "  Dial Mark: "; text$;
index = (VesselHeight * Density) / 33.1
SELECT CASE cal%
CASE 11, 21, 31, 41, 51
temp = (VesselHeight - Centreline - Flange) * Density
full = temp * .9 * MillibarConversion
refill = temp * .3 * MillibarConversion
empty = temp * .1 * MillibarConversion
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
temp = (VesselHeight - Centreline - Flange)
full = temp * .9 * C254
refill = temp * .3 * C254
empty = temp * .1 * C254
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
full = (A180 * .9) + A90
refill = (A180 * .3) + A90
empty = (A180 * .1) + A90
 ELSE
full = (A270 * .9)                       ' + 45
refill = (A270 * .3)                     ' + 45
empty = (A270 * .1)                      ' + 45
 END IF
 LOCATE 16, 51, 0
 PRINT "Full @    :";
 PRINT USING fm2$; full
 LOCATE 17, 51, 0
 PRINT "Refill @  :";
 PRINT USING fm2$; refill
 LOCATE 18, 51, 0
 PRINT "Empty @   :";
 PRINT USING fm2$; empty

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
 PRINT USING fm5$; VesselHeight * C254
 LOCATE 20, 51, 0
 PRINT "Range     :";
 PRINT USING fm5$; (((VesselHeight - Centreline - Flange) * Density) * C254)
 LOCATE 21, 51, 0
 PRINT "S.G       :";
 PRINT USING fm7$; Density
 LOCATE 22, 51, 0
 PRINT "Centreline :";
 PRINT USING fm5$; Centreline * C254
 LOCATE 23, 51, 0
 PRINT "Transmitter:";
 PRINT USING fm5$; Flange * C254;
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
 PRINT USING fm5$; VesselHeight * C254
 LOCATE 21, 51, 0
 PRINT "Range     :";
 PRINT USING fm8$; ((VesselHeight * Density) * MillibarConversion)
 LOCATE 22, 51, 0
 PRINT "S.G       :";
 PRINT USING fm7$; Density;
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
 PRINT USING fm5$; VesselHeight * C254
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
 PRINT USING " ###### ins"; VesselHeight * C254
 LOCATE 20, 51, 0
 PRINT "Range     :";
 PRINT USING " ###### mm"; (((VesselHeight - Centreline - Flange) * Density) * C254)
 LOCATE 21, 51, 0
 PRINT "S.G       :";
 PRINT USING fm7$; Density
 LOCATE 22, 51, 0
 PRINT "Centreline :";
 PRINT USING fm5$; Centreline * C254
 LOCATE 23, 51, 0
 PRINT "Transmitter:";
 PRINT USING fm5$; Flange * C254;
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
 PRINT USING fm5$; VesselHeight * C254
 LOCATE 21, 51, 0
 PRINT "Range     :";
 PRINT USING fm8$; ((VesselHeight * Density) * MillibarConversion)
 LOCATE 22, 51, 0
 PRINT "S.G       :";
 PRINT USING fm7$; Density;
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
 PRINT USING fm5$; VesselHeight * C254
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
 PRINT USING fm5$; VesselHeight * C254
 LOCATE 20, 51, 0
 PRINT "Range     :";
 PRINT USING " ###.## mm "; (((VesselHeight - Centreline - Flange) * Density) * C254)
 LOCATE 21, 51, 0
 PRINT "S.G       :";
 PRINT USING fm7$; Density
 LOCATE 22, 51, 0
 PRINT "Centreline :";
 PRINT USING fm6$; Centreline * C254
 LOCATE 23, 51, 0
 PRINT "Transmitter:";
 PRINT USING fm6$; Flange * C254;
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
 PRINT USING fm5$; VesselHeight * C254
 LOCATE 21, 51, 0
 PRINT "Range     :";
 PRINT USING fm8$; ((VesselHeight * Density) * MillibarConversion) * C254
 LOCATE 22, 51, 0
 PRINT "S.G       :";
 PRINT USING fm7$; Density;
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
 PRINT USING fm5$; VesselHeight * C254
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
 PRINT USING fm5$; VesselHeight * C254
 LOCATE 20, 51, 0
 PRINT "Range     :";
 PRINT USING fm5$; (((VesselHeight - Centreline - Flange) * Density) * C254)
 LOCATE 21, 51, 0
 PRINT "S.G       :";
 PRINT USING fm7$; Density
 LOCATE 22, 51, 0
 PRINT "Centreline :";
 PRINT USING fm5$; Centreline * C254
 LOCATE 23, 51, 0
 PRINT "Transmitter:";
 PRINT USING fm5$; Flange * C254;
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
 PRINT USING fm5$; VesselHeight * C254
 LOCATE 21, 51, 0
 PRINT "Range     :";
 PRINT USING fm5$; (VesselHeight * Density) * C254
 LOCATE 22, 51, 0
 PRINT "S.G       :";
 PRINT USING fm7$; Density;
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
 PRINT USING fm5$; VesselHeight * C254
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
 PRINT USING fm5$; VesselHeight * C254
 LOCATE 20, 51, 0
 PRINT "Range     :";
 PRINT USING fm5$; ((VesselHeight - Centreline - Flange) * Density) * C254
 LOCATE 21, 51, 0
 PRINT "S.G       :";
 PRINT USING fm7$; Density
 LOCATE 22, 51, 0
 PRINT "Centreline :";
 PRINT USING fm6$; Centreline * C254
 LOCATE 23, 51, 0
 PRINT "Transmitter:";
 PRINT USING fm6$; Flange;
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
 PRINT USING fm5$; VesselHeight * C254
 LOCATE 21, 51, 0
 PRINT "Range     :";
 PRINT USING fm8$; ((VesselHeight * Density) * MillibarConversion)

 LOCATE 22, 51, 0
 PRINT "S.G       :";
 PRINT USING fm7$; Density;
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
 PRINT USING fm5$; VesselHeight * C254
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
 PRINT USING fm1$; VesselHeight * C254
 LOCATE 20, 51, 0
 PRINT "Range     :";
 PRINT USING fm1$; (VesselHeight - Centreline - Flange) * Density * C254
 LOCATE 21, 51, 0
 PRINT "S.G       :";
 PRINT USING fm7$; Density
 LOCATE 22, 51, 0
 PRINT "Centreline :";
 PRINT USING fm1$; Centreline * C254
 LOCATE 23, 51, 0
 PRINT "Transmitter:";
 PRINT USING fm1$; Flange * C254;
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
 PRINT USING fm9$; VesselHeight * C254
 LOCATE 20, 51, 0
 PRINT "Range     : ";
 PRINT USING fm9$; (VesselHeight - Centreline - Flange) * C254;
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
 PRINT USING fm1$; VesselHeight * C254
 LOCATE 21, 51, 0
 PRINT "Range     :";
 PRINT USING fm1$; VesselHeight * Density * C254
 LOCATE 22, 51, 0
 PRINT "S.G       :";
 PRINT USING fm7$; Density;
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
 PRINT USING fm5$; VesselHeight * C254
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
 PRINT USING fmt$; increment(count%) * C254;
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
OPEN wo$ + ".cal" FOR OUTPUT AS 1

SELECT CASE accuracy

	CASE 3
		 accuracyvolume = total * .03
	CASE 2
		 accuracyvolume = total * .02
	CASE 0


END SELECT

accuracyvolume = INT(accuracyvolume)

SELECT CASE cal%

CASE IS = 14, 24, 24, 44, 54
CALL header

PRINT #1, ME$
PRINT #1, "Scale: "; RTRIM$(LTRIM$(STR$(total))); " "; RTRIM$(LTRIM$(scale$)); " ("; RTRIM$(LTRIM$(STR$(accuracy))); "% Accuracy +/- "; RTRIM$(LTRIM$(STR$(accuracyvolume))); " "; RTRIM$(LTRIM$(scale$)); ") Max"
PRINT #1, "  Dial Text: "; text$
PRINT #1, USING "Distance ##### mm "; VesselHeight * C254
PRINT #1, line$
PRINT #1, " "
PRINT #1, "TABLE:"
PRINT #1, " "
PRINT #1, "   " + scale$;
PRINT #1, TAB(21); inc$;
PRINT #1, TAB(50); "Degrees"
PRINT #1, ""
IF fraction THEN
 FOR c% = 0 TO counter% - 1
  PRINT #1, Mask$(c%) + "  ";
  PRINT #1, Mark$(c%);
  PRINT #1, TAB(20); USING "#####"; increment(c%) * C254;
  PRINT #1, TAB(33); ul$;
  PRINT #1, TAB(44); ul$;
  PRINT #1, TAB(55); ul$;
  PRINT #1, TAB(66); ul$
 NEXT
ELSE
IF INSTR(scale$, "Cu Metres") THEN
 FOR c% = 0 TO counter% - 1
  PRINT #1, USING "####.##  "; Chart(c%);
  PRINT #1, Mark$(c%);
  PRINT #1, TAB(20); USING "#####"; increment(c%) * C254;
  PRINT #1, TAB(33); ul$;
  PRINT #1, TAB(44); ul$;
  PRINT #1, TAB(55); ul$;
  PRINT #1, TAB(66); ul$
 NEXT
ELSE
 FOR c% = 0 TO counter% - 1
  PRINT #1, USING "#######  "; Chart(c%);
  PRINT #1, Mark$(c%);
  PRINT #1, TAB(20); USING "#####"; increment(c%) * C254;
  PRINT #1, TAB(33); ul$;
  PRINT #1, TAB(44); ul$;
  PRINT #1, TAB(55); ul$;
  PRINT #1, TAB(66); ul$
 NEXT
END IF
END IF
 
PRINT #1, line$
CLOSE 1
SLEEP
CALL printscreen


CASE ELSE
CALL header


PRINT #1, ME$
PRINT #1, "Scale: "; RTRIM$(LTRIM$(STR$(total))); " "; RTRIM$(LTRIM$(scale$)); " ("; RTRIM$(LTRIM$(STR$(accuracy))); "% Accuracy +/- "; RTRIM$(LTRIM$(STR$(accuracyvolume))); " "; RTRIM$(LTRIM$(scale$)); ") Max"
PRINT #1, USING "   Index: ##.##"; index;
PRINT #1, "  Dial Text: "; text$
PRINT #1, USING "Distance ##### mm "; VesselHeight * C254;
PRINT #1, USING "  Working Distance ##### mm - "; ((VesselHeight - Centreline - Flange) * Density) * C254;
PRINT #1, USING "####.# mbar"; ((((VesselHeight - Centreline - Flange) * Density) * C254) * .98066) / 10
temp1 = ((((VesselHeight - Centreline - Flange) * Density) * C254) * .98066) / 10
PRINT #1, USING "Density #.#### kg/l "; Density;
PRINT #1, USING " C/Line  #### mm/"; Centreline * C254;
PRINT #1, USING " Transmitter #### mm/"; Flange * C254
PRINT #1, line$
PRINT #1, " "
PRINT #1, "TABLE:"
PRINT #1, " "
PRINT #1, "   " + scale$;
PRINT #1, TAB(21); inc$;

SELECT CASE cal%

	CASE 11, 21, 31, 41, 51

		PRINT #1, TAB(33); "Degrees"
		PRINT #1, ""


		IF fraction THEN

			FOR c% = 0 TO counter% - 1
				PRINT #1, Mask$(c%) + "  ";
				PRINT #1, Mark$(c%);
				PRINT #1, TAB(20); USING "####.#"; increment(c%);
				temp2 = increment(c%) / temp1
				temp3 = 270 * temp2
				PRINT #1, TAB(33); USING "###.#"; temp3;
				PRINT #1, TAB(44); ul$;
				PRINT #1, TAB(55); ul$;
				PRINT #1, TAB(66); ul$
			NEXT

		ELSE

		IF INSTR(scale$, "Cu Metres") THEN

			FOR c% = 0 TO counter% - 1
				PRINT #1, USING "####.##  "; Chart(c%);
				PRINT #1, Mark$(c%);
				PRINT #1, TAB(20); USING "####.#"; increment(c%);
				temp2 = increment(c%) / temp1
				temp3 = 270 * temp2
				PRINT #1, TAB(33); USING "###.#"; temp3;
				PRINT #1, TAB(44); ul$;
				PRINT #1, TAB(55); ul$;
				PRINT #1, TAB(66); ul$
			NEXT

		ELSE

			FOR c% = 0 TO counter% - 1
				PRINT #1, USING "#######  "; Chart(c%);
				PRINT #1, Mark$(c%);
				PRINT #1, TAB(20); USING "####.#"; increment(c%);
				temp2 = increment(c%) / temp1
				temp3 = 270 * temp2
				PRINT #1, TAB(33); USING "###.#"; temp3;
				PRINT #1, TAB(44); ul$;
				PRINT #1, TAB(55); ul$;
				PRINT #1, TAB(66); ul$
				NEXT


		END IF
		END IF


CASE ELSE

	PRINT #1, TAB(50); "Degrees"
	PRINT #1, ""


	IF fraction THEN

			FOR c% = 0 TO counter% - 1
				PRINT #1, Mask$(c%) + "  ";
				PRINT #1, Mark$(c%);
				PRINT #1, TAB(20); USING "####.#"; increment(c%);
				PRINT #1, TAB(33); ul$;
				PRINT #1, TAB(44); ul$;
				PRINT #1, TAB(55); ul$;
				PRINT #1, TAB(66); ul$
			NEXT

	ELSE

	IF scale$ = "Cu Metres" THEN

			FOR c% = 0 TO counter% - 1
				PRINT #1, USING "####.##  "; Chart(c%);
				PRINT #1, Mark$(c%);
				PRINT #1, TAB(20); USING "####.#"; increment(c%);
				PRINT #1, TAB(33); ul$;
				PRINT #1, TAB(44); ul$;
				PRINT #1, TAB(55); ul$;
				PRINT #1, TAB(66); ul$
			NEXT

	ELSE


			FOR c% = 0 TO counter% - 1
				PRINT #1, USING "#######  "; Chart(c%);
				PRINT #1, Mark$(c%);
				PRINT #1, TAB(20); USING "####.#"; increment(c%);
				PRINT #1, TAB(33); ul$;
				PRINT #1, TAB(44); ul$;
				PRINT #1, TAB(55); ul$;
				PRINT #1, TAB(66); ul$
			NEXT


	END IF
	END IF

END SELECT


PRINT #1, line$
PRINT #1, " "
PRINT #1, USING "Full @ ####.#" + inc$; full;
PRINT #1, USING "  Refill @ ####.#" + inc$; refill;
PRINT #1, USING "  Empty @ ####.#" + inc$; empty

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

 IF NOT Recoverable% THEN
	ME$ = ME$ + ", Had enough and giving up  ...."
	ME% = StatusLine%(ME$)
	RESUME egress
 END IF
'**************************************************************************
'*     Egress                                                             *
'**************************************************************************
egress:
 LOCATE 20, 1, 1: CALL EraEos
 CLS
END

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
		scale$ = "Litres    "
	CASE 2
		scale$ = "Gallons   "
	CASE 3
		scale$ = "Cu Metres "
	CASE 4
		scale$ = "Cu Feet   "
	CASE 5
		scale$ = "Kg        "
	CASE 6
		scale$ = "Tonnes    "
	CASE 7
		scale$ = "Tons      "
	CASE 8
		scale$ = "Percent   "
	CASE 9
		scale$ = "Fractions "
	CASE 10
		scale$ = "Metres    "
	CASE 11
		scale$ = "Feet      "
	CASE 12
		scale$ = "US Gallons"
	CASE 13
		scale$ = "mm        "

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

SUB DishedEndDepth
DIM Di AS SINGLE
DIM Rs AS SINGLE
DIM Rn AS SINGLE
DIM Fl AS SINGLE
DIM depth AS SINGLE
SHARED abort%
ScrollUp 5, 1, 20, 80, 0, 0
CALL Pop


aprint 25, 1, STRING$(80, 32), 31
aprint 8, 23, "Dish Depth", 31
aprint 10, 24, "Dish Diameter     :          " + "m", 31
aprint 11, 24, "Spherical Radius  :          " + "m", 31
aprint 12, 24, "Knuckle Radius    :          " + "mm", 31
aprint 13, 24, "Flange            :          " + "mm", 31

DO UNTIL abort%

	LOCATE 10, 44, 0
	aprint 25, 10, "Enter Dish Diameter in metres                  ", 30
	aprint 25, 60, "ESC to Abort", 30
	mx% = 8
	a$ = Oldcapacity$
	a$ = RevInput$(mx%, a$)
	a$ = LTRIM$(RTRIM$(a$))
	Oldcapacity$ = a$
	Di = VAL(a$)
	LOCATE 10, 44, 0
	PRINT USING " ##.###"; Di
	IF abort% THEN EXIT DO

	LOCATE 11, 44, 0
	aprint 25, 10, "Enter Spherical Radius in metres               ", 30
	mx% = 7
	a$ = Oldheight$
	a$ = RevInput$(mx%, a$)
	a$ = LTRIM$(RTRIM$(a$))
	Oldheight$ = a$
	Rs = VAL(a$)
	LOCATE 11, 44, 0
	PRINT USING "###.###"; Rs
	IF abort% THEN EXIT DO

	LOCATE 12, 44, 0
	aprint 25, 10, "Enter Knuckle Radius in millimetres             ", 30
	mx% = 7
	a$ = olddensity$
	a$ = RevInput$(mx%, a$)
	a$ = LTRIM$(RTRIM$(a$))
	olddensity$ = a$
	Rn = VAL(a$)
	LOCATE 12, 44, 0
	PRINT USING "   ####"; Rn
	IF abort% THEN EXIT DO

	LOCATE 13, 44, 0
	mx% = 7
	aprint 25, 10, "Enter Flange depth in millimetres         ", 30
	a$ = oldcentreline$
	a$ = RevInput$(mx%, a$)
	oldcentreline$ = a$
	a$ = LTRIM$(RTRIM$(a$))
	F1 = VAL(a$)
	LOCATE 13, 44, 0
	PRINT USING "   ####"; F1
	aprint 25, 1, STRING$(80, 32), 31
	ok% = Verify%(19, "Change any entered data  ")
	IF NOT ok% THEN EXIT DO
LOOP


Rn = Rn / 1000

depth = (Rs - ((Rs - Di / 2) * (Rs + Di / 2 - 2 * Rn)) ^ .5) * 1000
DD$ = STR$(INT(depth + F1))
DD$ = DD$ + "mm"
aprint 8, 34, DD$, 31
aprint 25, 3, "Press any key to continue", 27
SLEEP


END SUB

DEFSNG Z
SUB DishedEnds

SHARED Choice%
SHARED abort%

CALL MainScreen
LOCATE , , 0: Context% = 0: Topic$ = ""
ScrollUp 5, 1, 20, 80, 0, 0
aprint 6, 31, "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿", 15
aprint 7, 31, "³   DISH MENU   ³", 15
aprint 8, 31, "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ", 15

menu% = 4: row% = 9: col% = 23: menu$(0) = "1234"
menu$(1) = "1. Dish Depth"
menu$(2) = "2. "
menu$(3) = "3. "
menu$(4) = "4. Main menu"



'**************************************************************************
MenuDriver row%, col%, menu%
IF abort% THEN EXIT SUB

SELECT CASE Choice%

 CASE IS = 1
		CALL DishedEndDepth
 CASE IS = 12345
		CALL PartVol
 CASE IS = 12345
		CALL TotalVol
 CASE IS = 4
		aprint 25, 10, "                                               ", 30
		EXIT SUB
END SELECT






END SUB

DEFDBL Z
SUB getangle (analog, mFormat$, offset)
'**************************************************************************
'*     GetAngle                                                           *
'**************************************************************************
SHARED Choice%
LOCATE , , 0: Context% = 0: Topic$ = ""
ScrollUp 7, 20, 18, 61, 0, 0
aprint 6, 31, "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿", 15
aprint 7, 31, "³  METER  MENU  ³", 15
aprint 8, 31, "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ", 15
menu% = 3: row% = 9: col% = 28: menu$(0) = "123"
menu$(1) = "1. 240 Degree Scale"
menu$(2) = "2. 90 Degree Scale"
menu$(3) = "3. 0-20mA Output."
MenuDriver row%, col%, menu%
SELECT CASE Choice%
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
SUB header
PRINT #1, "CERTIFICATE of CALIBRATION ";
PRINT #1, "               Gauge Serial Number: "; wo$
PRINT #1, line$
PRINT #1, ""
PRINT #1, "Issued By:                                Approved by:"
PRINT #1, "Afriso Eurogauge Ltd                      D C Ward - Technical Manager "
PRINT #1, "Unit 4, Satellite Business Village "
PRINT #1, "Fleming Way, Crawley. RH10 9NE            Valid for 13 months from:  "
PRINT #1, "www.eurogauge.co.uk                       "; MID$(DATE$, 4, 2) + "/" + LEFT$(DATE$, 2) + "/" + RIGHT$(DATE$, 4); " or as agreed"
PRINT #1, line$
PRINT #1, ""
PRINT #1, "Originally supplied to: "
PRINT #1, Cust$
PRINT #1, "Reference: "; Custorder$
PRINT #1, line$
PRINT #1, ""
PRINT #1, "Calibrated using: Calsys 5.8 Table Compiler, DialWrite Dial Creator"
PRINT #1, "                  Druck DPI510 Pressure Calibrator Serial 1209-92"
PRINT #1, line$
PRINT #1, ""
PRINT #1, "CONFORMITY:"
PRINT #1, "This is to certify that the gauge detailed on this certificate conforms"
PRINT #1, "to the requirements of your purchase order and where applicable with the"
PRINT #1, "relevant British and European standards."
PRINT #1, line$
PRINT #1, ""
PRINT #1, "CALIBRATION PARAMETERS:"
PRINT #1, "Gauge Type:Tank Shape: ";
 
END SUB

SUB MainScreen
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
SHARED abort%, Choice%, CGA%
abort% = 0: Choice% = 1
IF CGA% THEN Colour% = 30 ELSE Colour% = 112
FOR i% = 1 TO menu%
IL$ = LEFT$(menu$(i%), 1): LN% = row% + i%
aprint LN%, col%, menu$(i%), 7
aprint LN%, col%, IL$, 15
NEXT i%
M1:
LN% = row% + Choice%: aprint LN%, col%, menu$(Choice%), Colour%
M2:
DO
 K$ = INKEY$
LOOP WHILE K$ = ""
IF LEN(K$) > 1 THEN GOTO M3
K% = ASC(K$)
IF K% = 13 THEN GOTO M5
IF K% = 27 THEN abort% = -1: GOTO M5
K$ = UCASE$(K$)
m% = INSTR(menu$(0), K$)
IF m% > 0 THEN GOTO M4
GOTO M2
M3:
LS% = Choice%: Scan% = ASC(MID$(K$, 2, 1))
IF Scan% = 59 THEN
 Topic$ = "gauge" + LTRIM$(RTRIM$(STR$(Choice%)))
 'HelpMate 1, Topic$
 GOTO M2
END IF
IF Scan% = 80 THEN Choice% = Choice% + 1
IF Choice% > menu% THEN Choice% = 1
IF Scan% = 72 THEN Choice% = Choice% - 1
IF Choice% < 1 THEN Choice% = menu%
IF Choice% = LS% THEN GOTO M2
LN% = row% + LS%
aprint LN%, col%, menu$(LS%), 7
IL$ = LEFT$(menu$(LS%), 1)
aprint LN%, col%, IL$, 15
GOTO M1
M4:
LS% = Choice%: Choice% = m%
IF Choice% = LS% THEN GOTO M5
LN% = row% + LS%
aprint LN%, col%, menu$(LS%), 7
IL$ = LEFT$(menu$(LS%), 1)
aprint LN%, col%, IL$, 15
LN% = row% + Choice%
aprint LN%, col%, menu$(Choice%), Colour%
M5:
END SUB

DEFDBL Z
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

DEFSNG Z
SUB Pop
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

DO UNTIL i$ <> ""
i$ = INKEY$
LOOP


'ok% = True%
'DO UNTIL NOT ok%
' ok% = Verify%(5, "Press a key to continue..    ")
'IF ok% THEN
'EXIT DO
' ELSE
'GOTO reject
 'END IF
'LOOP
'ok% = True%

'OPEN Wo$ + ".cal" FOR INPUT AS 1
'DO UNTIL EOF(1)
 'LINE INPUT #1, Line$
' LPRINT "    "; Line$
'LOOP
'LPRINT CHR$(27) + "E"
'CLOSE 1
'reject:
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
 'HelpMate Context%, Topic$
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

DEFDBL Z
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
DIM temp AS SINGLE
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

SUB WorksOrder (wo$, Cust$, Custorder$)
'**************************************************************************
'*     WorksOrder                                                         *
'**************************************************************************
SHARED abort%
CALL PopUp(12, 10, 3, 60, 30, 1, 2, 1)
aprint 13, 15, "Enter Sales Order Reference: ", 30
LOCATE 13, 45, 0
mx% = 8
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
wo$ = a$
IF wo$ = "" THEN
		temp$ = TIME$
		wo$ = "CT" + LEFT$(temp$, 2) + MID$(temp$, 4, 2) + RIGHT$(temp$, 2)
		wo$ = LEFT$(wo$, 8)
END IF

IF abort% THEN COLOR 7, 0: CLS : END
CALL shutup
'**************************************************************************
CALL PopUp(12, 1, 3, 76, 30, 1, 2, 1)
aprint 13, 3, "Customer: ", 30
LOCATE 13, 13, 0
mx% = 64
a$ = ""
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
Cust$ = a$
IF Cust$ = "" THEN Cust$ = "N/A"
IF abort% THEN COLOR 7, 0: CLS : END
'CALL shutup
'**************************************************************************
CALL PopUp(17, 1, 3, 50, 30, 1, 2, 1)
aprint 18, 3, "Customer Ref:   ", 30
LOCATE 18, 17, 0
mx% = 32
a$ = ""
a$ = RevInput$(mx%, a$)
a$ = LTRIM$(RTRIM$(a$))
Custorder$ = a$
IF Custorder$ = "" THEN Custorder$ = "N/A"
IF abort% THEN COLOR 7, 0: CLS : END
CALL shutup


END SUB

