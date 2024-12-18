      INTEGER D
      DIMENSION MS(31)
      OPEN(UNIT=6,FILE='KALEND
	2.RES')
      DO 1 I=1,31
      MS(I)=I
    1 CONTINUE
      PRINT *,'       BET KURIU METU ir MENESIO KALENDORIAUS PROGRAMA'
	PRINT *,'             (su rezultatu failu spausdinimui)'
    3 PRINT *,'Iveskite metu (MT), pradinio (MP)ir paskutinio (MG) ',
     *'menesiu eiles numerius, '
      PRINT *,'pvz. MT MP MG arba vieno menesio - MT MP MP. Menesiu '
     *'NP ir NG negali buti'
      PRINT *,'maziau 1 ir daugiau 12 bei MG maziau MP.'
   44 READ *,MI,NP,NG
    4 IF(NP.LT.1.OR.NG.LT.1.OR.NP.GT.12.OR.NG.GT.12.OR.NG<NP) THEN
      PRINT *,'KLAIDA, PAKARTUOKITE'
      GO TO 44
	ENDIF
      MT=MI
	DO 2 I=1,20
	IF(MT.LE.0) MT=MT+400
	IF(MT.GT.400) MT=MT-400
    2 CONTINUE
    	DO 48 MN=NP,NG
      ND=0
	PRINT *, ' '
	WRITE(6,*) ' '
  	IF(MN.EQ.1) WRITE(6,*) ' ',MI,' METAI SAUSIS'
   	IF(MN.EQ.2) WRITE(6,*) ' ',MI,' METAI VASARIS'
   	IF(MN.EQ.3) WRITE(6,*) '  ',MI,' METAI KOVAS'
   	IF(MN.EQ.4) WRITE(6,*) ' ',MI,' METAI BALANDIS'
   	IF(MN.EQ.5) WRITE(6,*) '  ',MI,' METAI GEGUZE'
   	IF(MN.EQ.6) WRITE(6,*) ' ',MI,' METAI BIRZELIS'
   	IF(MN.EQ.7) WRITE(6,*) '  ',MI,' METAI LIEPA'
   	IF(MN.EQ.8) WRITE(6,*) '',MI,' METAI RUGPJUTIS'
   	IF(MN.EQ.9) WRITE(6,*) ' ',MI,' METAI RUGSEJIS'
   	IF(MN.EQ.10) WRITE(6,*)' ',MI,' METAI SPALIS'
   	IF(MN.EQ.11) WRITE(6,*)'',MI,' METAI LAPKRITIS'
   	IF(MN.EQ.12) WRITE(6,*)' ',MI,' METAI GRUODIS'
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
   21 FORMAT(4X,'P',3X,'A',3X,'T',3X,'K',3X,'P',3X,'S',3X,'S'/3X,
     *27('-'))
	IF(N.NE.7) GO TO 44
      GO TO(24,30,33,36,39,42,45),N
   24 PRINT 25
	WRITE(6,25)
   25 FORMAT(4X,'1',3X,'2',3X,'3',3X,'4',3X,'5',3X,'6',3X,'7')
      PRINT 27, (MS(I),I=8,14),(MS(J),J=15,21),(MS(K),K=22,28),
     *(MS(L),L=29,KD)
	WRITE(6,27) (MS(I),I=8,14),(MS(J),J=15,21),(MS(K),K=22,28),
     *(MS(L),L=29,KD)
   27 FORMAT(1X,7I4)
      GO TO 48
   30 PRINT 31
	WRITE(6,31)
   31 FORMAT(8X,'1',3X,'2',3X,'3',3X,'4',3X,'5',3X,'6')
      PRINT 27, (MS(I),I=7,13),(MS(J),J=14,20),(MS(K),K=21,27),
     *(MS(L),L=28,KD)
	WRITE(6,27) (MS(I),I=7,13),(MS(J),J=14,20),(MS(K),K=21,27),
     *(MS(L),L=28,KD)
      GO TO 48
   33 PRINT 34
	WRITE(6,34)
   34 FORMAT(12X,'1',3X,'2',3X,'3',3X,'4',3X,'5')
      PRINT 27, (MS(I),I=6,12),(MS(J),J=13,19),(MS(K),K=20,26),
     *(MS(L),L=27,KD)
	WRITE(6,27) (MS(I),I=6,12),(MS(J),J=13,19),(MS(K),K=20,26),
     *(MS(L),L=27,KD)
      GO TO 48
   36 PRINT 37
	WRITE(6,37)
   37 FORMAT(16X,'1',3X,'2',3X'3',3X,'4')
      PRINT 27, (MS(I),I=5,11),(MS(J),J=12,18),(MS(K),K=19,25),
     *(MS(L),L=26,KD)
	WRITE(6,27) (MS(I),I=5,11),(MS(J),J=12,18),(MS(K),K=19,25),
     *(MS(L),L=26,KD)
      GO TO 48
   39 PRINT 40
	WRITE(6,40)
   40 FORMAT(20X,'1',3X,'2',3X,'3')
      PRINT 27, (MS(I),I=4,10),(MS(J),J=11,17),(MS(K),K=18,24),
     *(MS(L),L=25,KD)
	WRITE(6,27) (MS(I),I=4,10),(MS(J),J=11,17),(MS(K),K=18,24),
     *(MS(L),L=25,KD)
      GO TO 48
   42 PRINT 43
	WRITE(6,43)
   43 FORMAT(24X,'1',3X,'2')
      PRINT 27, (MS(I),I=3,9),(MS(J),J=10,16),(MS(K),K=17,23)
	WRITE(6,27) (MS(I),I=3,9),(MS(J),J=10,16),(MS(K),K=17,23)
	IF(KD.LE.30) PRINT 27, (MS(L),L=24,KD)
      IF(KD.LE.30) WRITE(6,27) (MS(L),L=24,KD)
	IF(KD.GT.30) PRINT 27, (MS(L),L=24,30),KD
      IF(KD.GT.30) WRITE(6,27) (MS(L),L=24,30),KD
      GO TO 48
   45 PRINT 46
	WRITE(6,46)
   46 FORMAT(28X,'1')
      PRINT 27, (MS(I),I=2,8),(MS(J),J=9,15),(MS(K),K=16,22)
	WRITE(6,27) (MS(I),I=2,8),(MS(J),J=9,15),(MS(K),K=16,22)
      IF(KD.LE.29) PRINT 27, (MS(L),L=23,KD)
      IF(KD.LE.29) WRITE(6,27) (MS(L),L=23,KD)
      IF(KD.EQ.30) PRINT 27, (MS(L),L=23,29),KD
      IF(KD.EQ.30) WRITE(6,27) (MS(L),L=23,29),KD
      IF(KD.EQ.31) PRINT 27, (MS(L),L=23,29),KD-1,KD
      IF(KD.EQ.31)  WRITE(6,27) (MS(L),L=23,29),KD-1,KD
	IF(NG.EQ.12) GO TO 50
   48 CONTINUE
      GO TO 44
   50 STOP
      END
