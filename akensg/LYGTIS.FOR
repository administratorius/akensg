      DOUBLE PRECISION AM,XN,XI,SN,SK
      DIMENSION AM(11),XN(10),XI(10),SN(10),SK(3),XK(3)
      OPEN(UNIT=5, FILE='LYGTIS.DAT')
      OPEN(UNIT=6, FILE='LYGTIS.RES')
      READ (5,*) N,NE
      M=N+1
	ZN=SIN30
	PRINT *,ZN
	READ *,U
      CALL PR(N,NE,M,XN,XI,SN,AM,SK,XK)
      STOP
      END      SUBROUTINE PR(N,NE,M,XN,XI,SN,AM,SK,XK)
      DOUBLE PRECISION AM,XN,XI,SN,SK
      DIMENSION AM(M),XN(N),XI(N),SN(N),SK(3),XK(3)
      IF(NE.EQ.0) GO TO 10
      READ (5,*) XN
      DO 4 I=1,N
      XN(I)=-1.0*XN(I)
    4 CONTI
       AM(1)=1.0
      AM(2)=XN(1)+XN(2)
      AM(3)=XN(1)*XN(2)
      IK=2
      DO 7 I=4,M
      J=I-1
      AM(I)=AM(J)*XN(J)
      DO 8 K=1,IK
      L=IK+2-K
      IJ=L-1
      AM(L)=AM(L)+AM(IJ)*XN(J)
    8 CONTINUE
      IK=IK+1
    7 CONTINUE
      GO TO 18
   10 READ (5,*) AM
   18 DO 31 I=1,N
      XN(I)=0.0
   31 CONTINUE
      WRITE(6,1) N
    1 FORMAT(//1H ,15X,I2,'-tojo laipsnio lygties sprendimas'//1H ,
     *15X,'Programa "LYGTIS", A.Kensgaila'//1H ,5X,'Lygties koe',
     *'ficientai ir laisvasis narys:'/)
      WRITE(6,2) (AM(I),I=1,M)
    2 FORMAT(1H ,2X,2F35.17)
      L=0
      DO 30 I1=1,200
      X=I1*0.5-50.5
      DO 5 K=1,3
      S=AM(1)
      DO 3 J=2,M
      S=S*X+AM(J)
    3 CONTINUE
      SK(K)=ABS(S)
      XK(K)=X
      X=X+0.5
    5 CONTINUE
      IF(SK(2).EQ.0.) GO TO 29
      IF((SK(2).LT.SK(1).AND.SK(2).LE.SK(3)).OR.
     *(SK(2).LE.SK(1).AND.SK(2).LT.SK(3)).OR.(SK(2).LT.SK(1).
     *AND.SK(2).LT.SK(3))) GO TO 6
      GO TO 30
    6 X1=XK(1)
      PRINT *,'  1',X1,(SK(K),K=1,3)
      DO 17 IJ=1,200
      X=X1+0.005*IJ
      DO 15 K=1,3
      S=AM(1)
      DO 13 J=2,M
      S=S*X+AM(J)
   13 CONTINUE
      SK(K)=ABS(S)
      XK(K)=X
      X=X+0.005
   15 CONTINUE
      IF(SK(2).EQ.0.) GO TO 19
      IF((SK(2).LT.SK(1).AND.SK(2).LE.SK(3)).OR.
     *(SK(2).LE.SK(1).AND.SK(2).LT.SK(3)).OR.(SK(2).LT.SK(1).
     *AND.SK(2).LT.SK(3))) GO TO 9
      GO TO 17
    9 X2=XK(1)
      PRINT *,'  2',XK(2),(SK(K),K=1,3)
      DO 14 IK=1,1000
      X=X2+0.00001*IK
      DO 12 K=1,3
      S=AM(1)
      DO 11 J=2,M
      S=S*X+AM(J)
   11 CONTINUE
      SK(K)=ABS(S)
      XK(K)=X
      X=X+0.00001
   12 CONTINUE
      IF(SK(2).EQ.0.) GO TO 16
      IF((SK(2).LT.SK(1).AND.SK(2).LE.SK(3)).OR.
     *(SK(2).LE.SK(1).AND.SK(2).LT.SK(3)).OR.(SK(2).LT.SK(1).
     *AND.SK(2).LT.SK(3))) GO TO 16
      GO TO 14
   16 L=L+1
C     PRINT *,'   3',XK(2),(SK(K),K=1,3)
      XI(L)=0.0
      XI(L)=XK(2)
   14 CONTINUE
      GO TO 17
   19 L=L+1
      XI(L)=0.0
      XI(L)=XK(2)
   17 CONTINUE
      GO TO 30
   29 L=L+1
      XI(L)=0.0
      XI(L)=XK(2)
   30 CONTINUE
      WRITE(6,21)
   21 FORMAT(/1H ,17X,'LYGTIES SAKNYS X(1),X(2),...,X(n)'/1H ,
     *5X,'(Jeigu reiksme lygi nuliui, tai nera sprendinio)')
      WRITE(6,26) (XI(II),II=1,N)
   26 FORMAT(1H ,2X,5F14.4)
      DO 40 I=1,N
      X=XI(I)
      S=AM(1)
      DO 22 J=2,M
      S=S*X+AM(J)
   22 CONTINUE
      SN(I)=ABS(S)
   40 CONTINUE
      WRITE(6,33)
   33 FORMAT(/1H ,5X,'Nesarysis:')
      WRITE(6,34) (SN(I),I=1,N)
   34 FORMAT(1H ,2X,5F14.4)
   35 RETURN
      END
