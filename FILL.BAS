
   DECLARE SUB linear (XAxis%, YAxis%, X1Axis%, Y1Axis%, Height, Multiplyer%, Fill%)

   SCREEN 12
   CLS
   c = 1
x:  c = INT(RND * 100)
i$ = INKEY$: IF i$ = "" THEN GOTO x:
   SELECT CASE i$
   CASE "u"

   

   '--------------------- linear -------------------------
   CALL linear(100, 100, 200, 200, c, 1, 14)

   '------------------------------------------------------


   CASE ELSE
   END SELECT
 GOTO x:                   

SUB linear (XAxis%, YAxis%, X1Axis%, Y1Axis%, Height, Multiplyer%, Fill%)


   LINE (XAxis% - 1, YAxis% - 1)-(X1Axis% + 1, Y1Axis% + 1), 15, B' draw rect tank
   LINE (XAxis%, YAxis%)-(X1Axis%, Y1Axis%), 0, BF
   LINE (XAxis%, Y1Axis%)-(Y1Axis%, Y1Axis% - (Height * Multiplyer%)), Fill%, BF

END SUB

