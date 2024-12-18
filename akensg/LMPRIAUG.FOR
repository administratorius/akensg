      DIMENSION FM(200)
      OPEN(UNIT=5,FILE='LMPRIAUG.DAT')
      OPEN(UNIT=6,FILE='LMPRIAUG.RES')
      READ(5,*) P1,X1,P2,X2,QK,XK,QD,XD
	PRINT *,OD,XD
	READ *,U
      PK=P1
      IF(XK.GT.X1) PK=PK+(P2-P1)*(XK-X1)/(X2-X1)
      PD=P2
      IF(XD.LT.X2) PD=PD-(P2-P1)*(X2-XD)/(X2-X1)
      Q=(3*PK+PD)*(XD-XK)/8
      DM=(5*QK+QD-4*Q)*(XD-XK)/6
      WRITE(6,*) 'Lenkimo momentu prieaugis ',DM
   20 STOP
      END
