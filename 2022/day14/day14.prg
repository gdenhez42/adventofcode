** Use asterisks to enter comment
&& End of line comment
NOTE Another way to enter comment

CLEAR && clear vfp desktop

** Var example
LOCAL varMsg
varMsg = "Hello world"
? "Hello world"

** If example
IF DATE() = DATE(2007,01,01) THEN
  ? "Happy new year"
ELSE
  ? "Bummer, it's only "+CDOW(DATE())
ENDIF

DO CASE
  CASE DATE() = DATE(2007,01,01)
    ? "HAPPY NEW YEAR"
  CASE CDOW(DATE()) = "Friday"
    ? "Thank god it's Friday!!!"
  CASE CDOW(DATE()) = "Sunday"
    ? "Oh damn Monday tomorrow"
  OTHERWISE
    ? "Just another day in paradise"
ENDCASE


** Read a file line by line
local ix

local array aParse[1]

for ix = 1 to ALines( aParse, FileToStr("input.txt"))

    ? AParse[m.ix]  && AParse[m.ix] contains current line in loop - do whatever with it

endfor

