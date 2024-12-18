      PRINT *,'Iveskite pradinio inaso suma ir metine palukanu',
     *' norma % (pvz. 1000 3.5)'
      READ *,A1,P
	P=P/100
      PRINT *,'JEIGU norite suzinoti per kiek metu susikaups norima ',
     *'suma, iveskite sumos dydi ir nuli (pvz. 1500 0 "E"), JEIGU ',
     *'norite suzinoti kokia suma susikaups per tiek tai metu, ',
     *'iveskite nuli ir metu skaiciu, (pvz. 0 5 "E")'
      READ *,I,M
	PRINT *,I,M
	READ *,U
 	N=100
	IF(I.EQ.0) N=M
      DO 5 K=1,N
      A1=A1+A1*P
	MT=K
	IF(I.NE.0.AND.A1.GT.I) GO TO 6
    5 CONTINUE
    6 PRINT 13,MT,A1
   13 FORMAT(2X,'Per',I3,' met susikaups',F10.2)
      PRINT *,'   Perskaite, iveskite bet koki skaitmeni'
      READ *,A
   15 STOP
      END
