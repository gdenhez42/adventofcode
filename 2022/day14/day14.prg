** Use asterisks to enter comment
&& End of line comment
NOTE Another way to enter comment

CLEAR && clear vfp desktop

** Read a file line by line
local ix, ic, il
minX = -1
minY = 0
maxX = -1
maxY = -1

local array structures[1]
structCount = ALines( structures, FileToStr("input.txt"))
for ix = 1 to structCount
	LOCAL ARRAY coords[1]
	lineCount = ALINES(coords, structures[ix], " -> ")
	LOCAL ARRAY lines[lineCount]
	
	FOR ic = 1 TO lineCount
		LOCAL ARRAY coord[1]
		ALINES(coord, coords[ic], ",")
		coord[1] = VAL(coord[1])
		coord[2] = VAL(coord[2])
		lines[ic] = coord
		DO case
			CASE minX == -1
				minX = coord[1]
			CASE coord[1] < minX
				minX = coord[1]
		ENDCASE
		DO case
			CASE maxX == -1
				maxX = coord[1]
			CASE coord[1] > maxX
				maxX = coord[1]
		ENDCASE
		DO case
			CASE maxY == -1
				maxY = coord[2]
			CASE coord[2] > maxY
				maxY = coord[2]
		ENDCASE
	ENDFOR	
ENDFOR

** Represent the cave into memory for part 1
w = INT(maxX-minX+3) && add 2 more to let sand fall into abyss
h = INT(maxY-minY+1)

LOCAL ARRAY Cave[h,w]
for ix = 1 to h
	FOR ic = 1 TO w
		Cave[ix,ic] = '.'
	ENDFOR
ENDFOR	

for ix = 1 to structCount
	LOCAL ARRAY coords[1]
	lineCount = ALINES(coords, structures[ix], " -> ")
	LOCAL ARRAY lines[lineCount]
	
	FOR ic = 1 TO lineCount-1
		LOCAL ARRAY coordStart[1]
		LOCAL ARRAY coordEnd[1]
		ALINES(coordStart, coords[ic], ",")
		ALINES(coordEnd, coords[ic+1], ",")
		coordStart[1] = INT(VAL(coordStart[1]))
		coordStart[2] = INT(VAL(coordStart[2]))
		coordEnd[1] = INT(VAL(coordEnd[1]))
		coordEnd[2] = INT(VAL(coordEnd[2]))
		IF coordStart[1] == coordEnd[1] then
			mi = MIN(coordStart[2], coordEnd[2])
			mx = MAX(coordStart[2], coordEnd[2])
			FOR il = mi TO mx
				cave[il-minY+1, coordStart[1]-minX+2] = "#"
			ENDFOR
		ENDIF
		IF coordStart[2] == coordEnd[2] then
			mi = MIN(coordStart[1], coordEnd[1])
			mx = MAX(coordStart[1], coordEnd[1])
			FOR il = mi TO mx
				cave[coordStart[2]-minY+1, il-minX+2] = "#"
			ENDFOR
		ENDIF
				
	ENDFOR
endfor

** run the simulation of sand falling for part 1
nbSand = 0
needSim = .T.
DO while needSim
	sandX = 500 - minX + 2
	sandY = 0
	stuck = .F.
	DO while NOT stuck AND sandY < maxY+1
		DO case
			CASE cave[sandY+1, sandX] == '.'
				sandY = sandY + 1
			CASE cave[sandY+1, sandX-1] == '.'
				sandY = sandY + 1
				sandX = sandX - 1
			CASE cave[sandY+1, sandX+1] == '.'
				sandY = sandY + 1
				sandX = sandX + 1
			OTHERWISE
				
				cave[sandY, sandX] = 'o'
				stuck = .T.
				nbSand = nbSand + 1
		ENDcase
	ENDdo
	IF NOT stuck then
		needSim = .F.
	endIf
ENDDO

? nbSand

** Represent the cave into memory for part 2
h = INT(maxY + 3)
w = INT(maxX - minX + h*4) 

LOCAL ARRAY Cave[h,w]
for ix = 1 to h
	DO case
		CASE ix == h
			FOR ic = 1 TO w
				Cave[ix,ic] = '#'
			ENDFOR
		OTHERWISE
			FOR ic = 1 TO w
				Cave[ix,ic] = '.'
			ENDFOR
	ENDCASE
ENDFOR	

for ix = 1 to structCount
	LOCAL ARRAY coords[1]
	lineCount = ALINES(coords, structures[ix], " -> ")
	LOCAL ARRAY lines[lineCount]
	
	FOR ic = 1 TO lineCount-1
		LOCAL ARRAY coordStart[1]
		LOCAL ARRAY coordEnd[1]
		ALINES(coordStart, coords[ic], ",")
		ALINES(coordEnd, coords[ic+1], ",")
		coordStart[1] = INT(VAL(coordStart[1]))
		coordStart[2] = INT(VAL(coordStart[2]))
		coordEnd[1] = INT(VAL(coordEnd[1]))
		coordEnd[2] = INT(VAL(coordEnd[2]))
		IF coordStart[1] == coordEnd[1] then
			mi = MIN(coordStart[2], coordEnd[2])
			mx = MAX(coordStart[2], coordEnd[2])
			FOR il = mi TO mx
				cave[il+1, coordStart[1]-minX+h*2] = "#"
			ENDFOR
		ENDIF
		IF coordStart[2] == coordEnd[2] then
			
			mi = MIN(coordStart[1], coordEnd[1])
			mx = MAX(coordStart[1], coordEnd[1])
			
			FOR il = mi TO mx
				cave[coordStart[2]+1, il-minX+h*2] = "#"
			ENDFOR
		ENDIF
				
	ENDFOR
ENDFOR

** run the simulation of sand falling for part 2
nbSand = 0
needSim = .T.
DO while needSim
	sandX = 500 - minX + h*2
	sandY = 0
	stuck = .F.
	DO while NOT stuck
		DO case
			CASE cave[sandY+1, sandX] == '.'
				sandY = sandY + 1
			CASE cave[sandY+1, sandX-1] == '.'
				sandY = sandY + 1
				sandX = sandX - 1
			CASE cave[sandY+1, sandX+1] == '.'
				sandY = sandY + 1
				sandX = sandX + 1
			OTHERWISE
				IF sandX == 500 - minX + h*2 AND sandY == 1 then
					needSim = .F.
				endif
				cave[sandY, sandX] = 'o'
				stuck = .T.
				nbSand = nbSand + 1
		ENDcase
	ENDDO
ENDDO

? nbSand
