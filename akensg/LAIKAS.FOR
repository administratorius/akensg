      INTEGER D
      DIMENSION MS(31)
      OPEN(UNIT=6,FILE='LAIKAS.RES')
      DO 1 I=1,31
      MS(I)=I
    1 CONTINUE
      PRINT *,'       BET KURIU METU ir MENESIU KALENDORIAUS PROGRAMA'
	PRINT *,'             (su rezultatu failu spausdinimui)'
    3 PRINT *,'Iveskite metus ir menesi (pvz."1796 7". Jei metai ',
     *'pries m.e., tai "-1796 7")'
      PRINT *,'Noredami iseiti, uzdarykite langa arba iveskite KITU ',
     *'metu KITA menesi'
	PRINT *,'VALIO'
	READ *,U 
      READ *,MT,MN
C      MT=1000
C	MN=12
   44 IF(MN.GT.0.AND.MN.LT.13) GO TO 6
      PRINT *,'TOKIO MENESIO NEBUNA!!!'
      GO TO 48
    6 IF(MT.NE.0) GO TO 9
      PRINT *,'NULIOJO EILES NUMERIO LAIKOTARPIU (metu,',
     *' menesiu, simtmeciu ir t.t.) NEBUNA!!!'
      GO TO 48
    9 ND=0
   	IF(MN.EQ.1) WRITE(6,*) '               ',MT,' m SAUSIS'
   	IF(MN.EQ.2) WRITE(6,*) '               ',MT,' m VASARIS'
   	IF(MN.EQ.3) WRITE(6,*) '               ',MT,' m KOVAS'
   	IF(MN.EQ.4) WRITE(6,*) '               ',MT,' m BALANDIS'
   	IF(MN.EQ.5) WRITE(6,*) '               ',MT,' m GEGUZE'
   	IF(MN.EQ.6) WRITE(6,*) '               ',MT,' m BIRZELIS'
   	IF(MN.EQ.7) WRITE(6,*) '               ',MT,' m LIEPA'
   	IF(MN.EQ.8) WRITE(6,*) '               ',MT,' m RUGPJUTIS'
   	IF(MN.EQ.9) WRITE(6,*) '               ',MT,' m RUGSEJIS'
   	IF(MN.EQ.10) WRITE(6,*) '               ',MT,' m SPALIS'
   	IF(MN.EQ.11) WRITE(6,*) '               ',MT,' m LAPKRITIS'
   	IF(MN.EQ.12) WRITE(6,*) '               ',MT,' m GRUODIS'
	IF(MT.LT.0) MT=MT+1
      N=MT/2000
      MT=MT-2000*N
      DO 10 I=1,10
      IF(MT.GT.0) GO TO 11
      MT=MT+400
   10 CONTINUE
   11 M=MT-1
      N=M/400
      M=M-400*N
      N=M/100
      ND=36524*N+ND
      M=M-100*N
      N=M/4
      ND=1461*N+ND
      M=M-4*N
      ND=365*M+ND
      J=MT/400
      J=MT-J*400
      K=J/100
      K=J-K*100
      L=K/4
      L=K-L*4
      KD=31
      IF(MN.EQ.4.OR.MN.EQ.6.OR.MN.EQ.9.OR.MN.EQ.11) KD=30
      IF(MN.NE.2) GO TO 12
      KD=29
      IF(L.NE.0.OR.(L.EQ.0.AND.K.EQ.0)) KD=28
      IF(MT.GE.400.AND.J.EQ.0) KD=29
   12 D=0
      IF(MN.EQ.1) GO TO 18
      NG=MN-1
      DO 15 I=1,NG
      ID=31
      IF(I.EQ.4.OR.I.EQ.6.OR.I.EQ.9.OR.I.EQ.11) ID=30
      IF(I.NE.2) GO TO 13
      ID=29
      IF(L.NE.0.OR.(L.EQ.0.AND.K.EQ.0)) ID=28
      IF(MT.GE.400.AND.J.EQ.0) ID=29
   13 D=D+ID
   15 CONTINUE
   18 ND=D+ND+1
      N=ND/7
      N=ND-N*7
      IF(N.EQ.0) N=7
      PRINT 21
	WRITE(6,21)
   21 FORMAT(5X,'P',8X,'A',8X,'T',8X,'K',8X,'P',8X,'S',8X,'S'/1X,
     *63('-'))
      GO TO(24,30,33,36,39,42,45),N
   24 PRINT 25
	WRITE(6,25)
   25 FORMAT(5X,'1',8X,'2',8X,'3',8X,'4',8X,'5',8X,'6',8X,'7')
      PRINT 27, (MS(I),I=8,14),(MS(J),J=15,21),(MS(K),K=22,28),
     *(MS(L),L=29,KD)
C      PRINT *,N,(MS(I),I=8,14),(MS(J),J=15,21),(MS(K),K=20,26),
C     *(MS(L),L=29,KD)
	WRITE(6,27) (MS(I),I=8,14),(MS(J),J=15,21),(MS(K),K=20,28),
     *(MS(L),L=29,KD)
   27 FORMAT(1X,I5,6I9)
      GO TO 48
   30   PRINT 31
	WRITE(6,31)
   31 FORMAT(14X,'1',8X,'2',8X,'3',8X,'4',8X,'5',8X,'6')
      PRINT 27, (MS(I),I=7,13),(MS(J),J=14,20),(MS(K),K=21,27),
     *(MS(L),L=28,KD)
C      PRINT *,N,(MS(I),I=7,13),(MS(J),J=13,19),(MS(K),K=20,26),
C     *(MS(L),L=28,KD)
	WRITE(6,27) (MS(I),I=7,13),(MS(J),J=13,19),(MS(K),K=20,26),
     *(MS(L),L=28,KD)
      GO TO 48
   33 PRINT 34
	WRITE(6,34)
   34 FORMAT(23X,'1',8X,'2',8X,'3',8X,'4',8X,'5')
      PRINT 27, (MS(I),I=6,12),(MS(J),J=13,19),(MS(K),K=20,26),
     *(MS(L),L=27,KD)
C      PRINT *,N,(MS(I),I=6,12),(MS(J),J=13,19),(MS(K),K=20,26),
C     *(MS(L),L=27,KD)
	WRITE(6,27) (MS(I),I=6,12),(MS(J),J=13,19),(MS(K),K=20,26),
     *(MS(L),L=27,KD)
      GO TO 48
   36 PRINT 37
	WRITE(6,37)
   37 FORMAT(32X,'1',8X,'2',8X'3',8X,'4')
      PRINT 27, (MS(I),I=5,11),(MS(J),J=12,18),(MS(K),K=19,25),
     *(MS(L),L=26,KD)
C      PRINT *,N,(MS(I),I=5,11),(MS(J),J=12,18),(MS(K),K=19,25),
C     *(MS(L),L=26,KD)
	WRITE(6,27) (MS(I),I=5,11),(MS(J),J=12,18),(MS(K),K=19,25),
     *(MS(L),L=26,KD)
      GO TO 48
   39 PRINT 40
	WRITE(6,40)
   40 FORMAT(41X,'1',8X,'2',8X,'3')
      PRINT 27, (MS(I),I=4,10),(MS(J),J=11,17),(MS(K),K=18,24),
     *(MS(L),L=25,KD)
C      PRINT *,N,(MS(I),I=4,10),(MS(J),J=11,17),(MS(K),K=18,24),
C     *(MS(L),L=25,KD)
	WRITE(6,27) (MS(I),I=4,10),(MS(J),J=11,17),(MS(K),K=18,24),
     *(MS(L),L=25,KD)
      GO TO 48
   42 PRINT 43
	WRITE(6,43)
   43 FORMAT(50X,'1',8X,'2')
      PRINT 27, (MS(II),II=3,9),(MS(J),J=10,16),(MS(K),K=17,23)
C      PRINT *,N,(MS(II),II=3,9),(MS(J),J=10,16),(MS(K),K=17,23)
	WRITE(6,27) (MS(II),II=3,9),(MS(J),J=10,16),(MS(K),K=17,23)
	IF(KD.LE.30) PRINT 27, (MS(L),L=24,KD)
      IF(KD.LE.30) WRITE(6,27) (MS(L),L=24,30),KD
      IF(KD.GT.30) PRINT 27, (MS(L),L=24,30),KD
      IF(KD.GT.30) WRITE(6,27) (MS(L),L=24,30),KD
      GO TO 48
   45 PRINT 46
	WRITE(6,46)
   46 FORMAT(59X,'1')
      PRINT 27, (MS(I),I=2,8),(MS(J),J=9,15),(MS(K),K=16,22)
	WRITE(6,27) (MS(I),I=2,8),(MS(J),J=9,15),(MS(K),K=16,22)
      IF(KD.LE.29) PRINT 27, (MS(L),L=23,KD)
      IF(KD.LE.29) WRITE(6,27) (MS(L),L=23,KD)
      IF(KD.EQ.30) PRINT 27, (MS(L),L=23,29),KD
      IF(KD.EQ.30) WRITE(6,27) (MS(L),L=23,29),KD
      IF(KD.EQ.31) PRINT 27, (MS(L),L=23,29),KD-1,KD
      IF(KD.EQ.31)  WRITE(6,27) (MS(L),L=23,29),KD-1,KD
   48 READ *,MT,MN
	IF(MT.EQ.0) GO TO 50
      GO TO 44
   50 STOP
      END
