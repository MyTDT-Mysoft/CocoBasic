
0 GOTO 9 :'SKIP SUBS
1 OX=X:OY=175-Y:PSET(OX,OY):RETURN :'PLOT
2 X=OX+X:Y=OY-Y:LINE(OX,OY)-(X,Y),PSET:OX=X:OY=Y:RETURN :'DRAW
9 :'---- START SPECRTUM APP ----
10 PMODE 4,1:PCLS:SCREEN 1,0
20 X=0:Y=175:GOSUB1
30 T=255:R=-175:B=-255:L=169
40 X=T:Y=0:GOSUB2:X=0:Y=R:GOSUB2:X=B:Y=0:GOSUB2:X=0:Y=L:GOSUB2
1000 GOTO 1000
