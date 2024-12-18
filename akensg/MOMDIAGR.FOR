      DIMENSION WM(50),QM(50),PX(50)
      OPEN(UNIT=5,FILE='MOMDIAGR.DAT')
      OPEN(UNIT=6,FILE='MOMDIAGR.RES')
      READ(5,*) P1,X1,P2,X2,WP,QP,XP,WG,QG,XG,N1,N2
      N=1+(XG-XP)/0.2
      WM(1)=WP
      QM(1)=QP
      PX(1)=PP
      WRITE(6,2) QP,WP,N1,N2
    2 FORMAT(X,2F10.3,2I5)
      PP=P1
      IF(XP.GT.X1) PP=P1+(P2-P1)*(XP-X1)/(X2-X1)
      PG=P2
      IF(X2.GT.XG) PG=P1+(P2-P1)*(XG-X1)/(X2-X1)
      R1=(XG-XP)*(PG+2*PP)/6
      CALL PAP(N,PP,PG,WP,XP,WG,XG,QP,QG,R1,PX,WM,QM)
      STOP
      END
      SUBROUTINE PAP(N,PP,PG,WP,XP,WG,XG,QP,QG,R1,PX,WM,QM)
      DIMENSION WM(N),QM(N),PX(N)
      X=0.0
      DX=(XG-XP)/N
      YX=WP
      DY=(WG-WP)/N
      DO 5 I=2,N
      WM(I)=0.0 
      X=X+DX
      PX(I)=PP+(PG-PP)*X/(XG-XP) 
      YX=YX+DY
      QM(I)=QP-(PX(I)+PP)*X/2
      W=R1*X-PP*X*X/2-(PX(I)-PP)*X*X/6
      WM(I)=YX-W
      SQ=QM(I)-QM(I-1)
      SW=WM(I)-WM(I-1)
	WRITE(6,3) QM(I),SQ,WM(I),SW
    3 FORMAT(X,4F8.3)
    5 CONTINUE
      PRINT 6, QG,SQ,WG,SW,DX
      WRITE(6,6) QG,SQ,WG,SW,DX
    6 FORMAT(X,4F8.3,' DX=',F6.3)
      RETURN
      END
