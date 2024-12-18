      PRINT *,'Iveskite skaiciu ir saknies laipsnio rodikli'
      PRINT *,'(Lyginio laipsnio saknis is neigiamo skaiciaus ne',
     *'traukiama!)'
    2 READ *, SK,RL
      PRINT *,SK,RL
	READ *,U
      N=RL/2
      S=RL-N*2
      S=SK
      IF(SK.LT.0.) S=0-SK
      AT=S**(1/RL)
      IF(SK.LT.0.) AT=0-AT
      PRINT *,'Atsakymas =',AT
      GO TO 2
	STOP
      END
