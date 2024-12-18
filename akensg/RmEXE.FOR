      DIMENSION FV(80,3),AFV(3,80),AFH(3,80),FH(80,3),AWI(3,80),
     *WI(80,3),APV(5,80),PV(80,5),APH(5,80),PH(80,5),MS(200,4),
     *VH(20,3),HH(20,3),WH(20,3),MTV(50,3),MTH(50,3),CPM(50,8),
     *XM(150),YM(150),MG(150,4),AST(3,200),IMT(200,4),
     *STM(150,4),MAT(4,150),ITM(150,4),UM(150,4),BM(150,4),SK(150)
      DIMENSION QM(150,4),WM(150,4),MI(50),WP(150,4),VTM(50),HTM(50),
     *VPM(50),HPM(50),MVT(50),MHT(50),MVH(10),MVP(50,3),MHP(50,3),
     *XX(50),IM(100),MM(13,12),NK(20)
      COMMON /C1/ NFV,NFH,NWI,NPV,NPH,NVH
      OPEN(UNIT=5,FILE='RmDUO.DAT')
      OPEN(UNIT=6,FILE='RmREZ.RES')
      PRINT 3
    3 FORMAT(4X,' Daugiaangio-daugiaaukscio remo tik is horizontaliu ',
     *'ir vertikaliu strypu'/21X,'irazoms ir pamatu apkrovoms skaiciuo',
     *'ti'/34X,'P R O G R A M A'/1X,'(Galima ir sijos - ivedant duomen',
     *'is nurodoma, kad mazguose statmenu strypu'/22X,'nera - ju itvi',
     *'rtinimo mazge kodas = 0)'/1X,' Kaip ivedinesite duomenis? Jei ',
     *'ekrane, iveskite 0. Jei duomenu faile, ives-'/1X,'kite bet kok',
     *'i skaitmeni. Ivedinejant ekrane ir padarius klaida teks vel pr',
     *'a-'/1X,'det is naujo. Todel ivedineti ekrane rekomenduojama, k',
     *'ai remas ne daugiau 5-6'/1X,'strypu ir nereikia varijuoti apkr',
     *'ovu kombinacijas.'/1X,'PASIRINNKITE - iveskite 0 arba, pvz. 3')
      READ *,LJ
	IF(LJ.NE.0) GO TO 12
      PRINT 6
    6 FORMAT(1X,' Skaiciuojamojoje schemoje mazgai (atremtas ar ne st',
     *'rypo galas, atremta '/1X,'ar ne ju sandura, tarpine strypo ilg',
     *'yje atrama) numeruojami knygos pusla-'/1X,'pio skaitymo tvarka',
     *': horizontaleje - is kaires i desine, hori-'/1X,'zontales - is',
     *' virsaus zemyn. Koordiniaciu pradzia pasirenkama taip,'/1X,'ka',
     *'d x=0 butu pats kairiausias ir y=0 butu pats zemiausias skaici',
     *'uojamo-'/1X,'je schemoje mazgas.'/1X,'  Duomenys ivedami eilut',
     *'emis pagal duodamus pavyzdzius. Skaiciai eilu-'/1X,'teje atski',
     *'riami padarant tarpeli, trupmenine dalis atskiriama tasku. Kai',
     */1X,'trupmenine dalis lygi nuliui, ivedama tik sveikoji dalis ',
     *'(net be tasko).'/1X,'  Pries nuspaudziant "Enter", patikrinkit',
     *' ivedamu eiluteje duomenu kieki'/1X,'ir ar yra tarpai'/3X,'IV',
     *'ESKITE mazgu ir laisvai pasirenkamu pjuviu kiekius (pjuviu, su',
     *'siju-'/1X,'siu su apkrovu dislokacija ir l.mometu ekstremumais',
     *', nurodineti nereikia)')
      READ *,NM,NC
	NCM=NM+NC
	PRINT 9
    9 FORMAT(1X,' IVESKITE vertikaliai atremtu, horizontaliai atremtu',
     *' mazgu ir mazgu-atra-'/1X,'mu, kuriose standziai itvirtinta da',
     *'ugiau kaip 1 strypas, kiekius (pastaruju'/1X,'gali nebuti - iv',
     *'edama 0), pvz. 3 2 1 "E".3 2 0 "E"') 
      READ *,NVT,NHT,NVH
	GO TO 27
   12 READ(5,*) NM,NC,NVT,NHT,NVH,NFV,NFH,NWI,NPV,NPH
      NCM=NM+NC
   15 IF(MZG.EQ.0) GO TO 27
      PRINT 18,MZG,NVT,(MVT(L),L=1,NVT)
   18 FORMAT(1X,' Neleistinas mazgo Nr',I3,' ar jam gretimu mazgu atr',
     *'emimas arba strypu juose'/1X,'itvirtinimas. PATIKSLINKITE. Jei',
     *' duomenu faile, iveskite bet kokia raide.'/1X,'Jei ekrane, pat',
     *'ikrinama vertikaliai atremtu mazgu kiekis ir ju Nr.'/1X,' Ives',
     *'ta:',I3,' mzg., kuriu Nr',10I4)
      PRINT *,'Jeigu viskas gerai, iveskite nuli, jei ne - tikslu atr',
     *'emtu mazgu kieki:'
      READ *,KK
      IF(KK.EQ.0) GO TO 21
      NVT=KK
      PRINT *,'ir didejancia tvarka ju Nr:'
      READ *,(MVT(K),K=1,NVT)
      GO TO 30
   21 PRINT 24,NHT,(MHT(L),L=1,NHT)
   24 FORMAT(1X,'Patikrinama horizontaliai atremti mazgai. Ivesta:',I3,
     *' mzg., ku-'/1X,'riu Nr',10I4)
      PRINT *,'Jeigu viskas gerai, iveskite nuli, jei ne - tikslu atr',
     *'emtu mazgu kieki:'
      READ *,KK
      IF(KK.EQ.0) GO TO 27
      NHT=KK
      PRINT *,'ir didejancia tvarka ju Nr:'
      READ *,(MHT(K),K=1,NHT)
   27 CALL DUOM(LJ,KK,MZG,NM,NCM,XM,YM,MG,MAT,ITM,IMT,BM,MS,STM,NVT,
     *MVT,NHT,MHT,NVH,MVH,MM,NK,NX)
   30	IF(MZG.NE.0) GO TO 39
   	IF(LJ.NE.0) GO TO 36
   	PRINT 33
   33 FORMAT(1X,'Iveskite apkrovu kiekius (jei kokios nera, rasykite ',
     *'nuli) tokia eiles tvarka:'/1X,'1. Koncentruotos vertikali',
     *'os;'/1X,'2. Koncentruotos horizintalios;'/1X,'3. jegu moment',
     *'ai;'/1X,'4. Isskirstytos vertikalios;'/1X,'5. Isskirstytos hor',
     *'izontalios.'/1X,'Pvz., 3 0 1 2 1 "E"')
	READ *,NFV,NFH,NWI,NPV,NPH
   36 CALL APKR(LJ,NX,NM,XM,YM,MG,AFV,FV,AFH,FH,AWI,WI,APV,PV,APH,PH)
      IF(ID.EQ.1) GO TO 42
 	PRINT *,'Ar strypu santykiniai standumai aktualu? Jei NE, ivesk',
     *'ite nuli, jei TAIP, tai'
      PRINT *,'santykiniai standumai nurodomi arba duomenu faile (ive',
     *'skite "1"), arba "ant'
      PRINT *,'ekrano" pagal nurodomus strypo galu mazgus (tuomet ive',
     *'skite "2")'
      READ *,ID
   39 CALL MAZG(ID,NM,AST,STM,BM,MS,MG)
C   39 CALL MAZG(LJ,ID,MZG,NM,AST,STM,BM,MS,MG)
   42 NC=NCM-NM
   45 CALL PAGR(NU,MZG,NM,NCM,NC,NVT,NHT,XM,YM,FV,FH,WI,
     *PV,PH,MG,SK,ITM,MS,STM,BM,UM,QM,WM,MI,WP,XX,VTM,
     *HTM,VPM,HPM,MTV,MTH,VH,HH,WH,MVT,MHT,MVH,MVP,MHP,CPM)
      IF(MZG.GT.0) GO TO 15
   48 STOP
      END
      SUBROUTINE DUOM(LJ,KK,MZG,NM,NCM,XM,YM,MG,MAT,ITM,IMT,BM,MS,STM,
     *NVT,MVT,NHT,MHT,NVH,MVH,MM,NK,NX)
      DIMENSION XM(NCM),YM(NCM),MG(NM,4),MAT(4,NM),ITM(NM,4),STM(NM,4),
     *IMT(NM,4),BM(NM,4),MS(NM,4),MVT(NVT),MHT(NHT),MVH(NVH),MM(13,12),
     *NK(20)
      IF(KK.NE.0) GO TO 6
	IF(LJ.EQ.0) GO TO 53
      IF(MZG.NE.0) GO TO 57
      READ (5,3) AIM,ABM
      READ (5,*) XM
      READ (5,3) AIM,ABM
      READ (5,*) YM
      READ (5,3) AIM,ABM
      READ (5,*) MAT
      READ (5,3) AIM,ABM
      READ (5,*) MVT
      READ (5,3) AIM,ABM
      READ (5,*) MHT
      READ (5,3) AIM,ABM
      READ (5,*) MVH
    3 FORMAT(2A4)
      GO TO 60
   53 PRINT 2
    2 FORMAT(1X,'Iveskite nurodomu mazgu KOORDINATES X ir Y bei STRYP',
     *'U ITVIRTINIMO juose '/1X,'kodus: 0 - kai pritvirtinto nera; 1 ',
     *'- kai pritvirtintas dvigubu sarnyru '/1X,'(perduodama tik asin',
     *'e jega); 2 - kai pritvirtinta sarnyru (perduodama '/1X,'skersin',
     *'e arba ir asine jega); 3 - kai itvirtintas standziai TOKIA EIL',
     *'ES '/1X,'TVARKA: strypas kairen-aukstyn-desinen-zemyn (pvz. 5.',
     *'3 3.5 3 0 3 2) bei'/1X,'tarpiniu pjuviu tik KOORDINATES X ir Y')    
      DO 8 I=1,NCM
	IF(I.GT.NM) GO TO 1
	PRINT *,'  Mazgo',I
	READ *,XM(I),YM(I),(ITM(I,L),L=1,4)
	PRINT *,I,(ITM(I,L),L=1,4)
	GO TO 8
    1	NP=NP+1
      PRINT *,'  Pjuvio',NP
   	READ *,XM(I),YM(I)
    8	CONTINUE
      PRINT *,'Iveskite vertikaliai atremtu mazgu Nr, pvz. 2 3 5 ...'
      READ *,(MVT(L),L=1,NVT)
      PRINT *,'Iveskite horizontaliai atremtu mazgu Nr, pvz. 1 3 8 ...'
      READ *,(MHT(L),L=1,NHT)
	IF(NVH.EQ.0) GO TO 60
	PRINT *,'Iveskite mazgu-atramu, kuriose standziai itvirtinta da',
     *'ugiau kaip 1 strypas Nr'
	READ *,(MVH(L),L=1,NVH)
      GO TO 60
   57 PRINT *,'Patikrinama strypo tarp nurodyto ir jam gretimo mazgu ',
     *'itvirtinimo kodai.'
	PRINT *,'Ivesta:'
      DO 59 L=1,4
      LG=L+2
      IF(LG.GT.4) LG=LG-4
      M1=MG(MZG,L)
      IF(M1.EQ.0) GO TO 59
      PRINT 58,MZG,M1,ITM(MZG,L),ITM(M1,LG)
   58 FORMAT(1X,'Tarp',I3,' ir',I3,' itvirt.kodai',I2,' ir',I2,'.',
     *'Jei viskas gerai, iveskite nuli,'/X,'jei klaidingi itvirtinimo ',
     *'kodai - 1,o jei tokio strypo nera - 2')
      READ *,NVR
	IF(NVR.EQ.0) GO TO 59
	IF(NVR.EQ.1) THEN
CCCCCCCCCCCCCCCCCC
	PRINT *,'Iveskite strypo nurodytuose mazguose itvirtinimo kodus'
      READ *,I1,I2
      ITM(MZG,L)=I1
      ITM(M1,LG)=I2
	GO TO 6
	ENDIF
	IF(NVR.EQ.2) THEN
	PRINT *,'patikslinkite abieju mazgu abscises X1 ir X2, ivesd',
     *'ami pvz. 3.8 6 "E"'
      READ *,XM(MZG),XM(M1)
	GO TO 60
	ENDIF
   59 CONTINUE
   60 IF(LJ.EQ.0) GO TO 6
      DO 5 J=1,4
      DO 5 I=1,NM
      ITM(I,J)=MAT(J,I)
    5 CONTINUE
    6 KK=0
      DO 13 I=1,NM
CCCCCCCCCCCCCCCCCCCCCC
      M=I+1
      IF(M.GT.NM) GO TO 7
      IF(YM(M).NE.YM(I).OR.XM(M).NE.XM(I)) GO TO 7
      IF(ITM(I,1).EQ.ITM(I,3).AND.ITM(I,2).EQ.ITM(I,4).AND.ITM(M,3).EQ.
     *ITM(M,1).AND.ITM(M,4).EQ.ITM(M,2)) GO TO 7
      IF(ITM(I,4).EQ.ITM(I,1).AND.ITM(I,3).EQ.ITM(I,2).AND.ITM(M,4).EQ.
     *ITM(M,1).AND.ITM(M,3).EQ.ITM(M,2)) GO TO 7
      IF(ITM(I,2).EQ.ITM(I,1).AND.ITM(I,4).EQ.ITM(I,3).AND.ITM(M,2).EQ.
     *ITM(M,1).AND.ITM(M,4).EQ.ITM(M,3)) GO TO 7
      PRINT *,'Dvigubame mazge ivesta strypu juose itvirtinimo kodai
     * - Nr ir kodai:'
      PRINT *,I,(ITM(I,L),L=1,4)
      PRINT *,M,(ITM(M,L),L=1,4)
      PRINT 102
  102 FORMAT(1X,'Jei toks dvigubas mazgas yra, pirma iveskite klaidin',
     *'gojo mazgo Nr. Jeigu tokio'/1X,'vertikalaus strypo nera, iveski',
     *'te skaiciu 777')
      READ *,IJ
      IF(IJ.EQ.777) GO TO 4
      PRINT *,'ir jo strypu kairen-aukstyn-desinen-zemyn itvirtinimo ',
     *'kodus, (2 2 3 3  "E")'
      READ *,(ITM(IJ,L),L=1,4)
      GO TO 6
    4 PRINT *,'ir nurodytu mazgu abscises X1 X2 (4.5 9 "E")'
      READ *,XM(I),XM(M)
      GO TO 6
    7 DO 9 J=1,4
      IMT(I,J)=ITM(I,J)
      MG(I,J)=0
    9 CONTINUE
   13 CONTINUE
      DO 15 I=1,NM-1
      IF(I-1.GT.0.AND.XM(I-1).EQ.XM(I).AND.YM(I-1).EQ.YM(I).AND.
     *ITM(I,1).EQ.ITM(I,3)) THEN
      MG(I,1)=I-1
      MG(I,3)=I-1
      MG(I-1,2)=I
      MG(I-1,4)=I
      GO TO 15
      ENDIF
      M3=I+1
      IF(IMT(I,1).EQ.0.AND.IMT(I,2).EQ.0.AND.IMT(I,3).EQ.0.AND.
     *IMT(I,4).EQ.0.AND.ITM(M3,1).GT.0) IMT(I,3)=4
      IF(XM(I+1).EQ.XM(I).AND.YM(I+1).EQ.YM(I).AND.ITM(I,3).EQ.3) M3=I+2
      IF(IMT(M3,1).EQ.0.AND.IMT(M3,2).EQ.0.AND.IMT(M3,3).EQ.0.AND.
     *IMT(M3,4).EQ.0.AND.IMT(I,3).GT.0) IMT(M3,1)=4
      Y=YM(I)
      Y3=YM(M3)
      IT=IMT(I,3)
      I3=IMT(M3,1)
      IF(IT.GT.0.AND.I3.GT.0.AND.Y3.EQ.Y) THEN
      MG(I,3)=M3
      MG(M3,1)=I
   11 GO TO 15
      ENDIF
      IF(IT.EQ.0.AND.I3.EQ.0) GO TO 15
      IF(Y3.EQ.Y) GO TO 12
      PRINT 10,I,M3
   10 FORMAT(1X,'Nevienodos mazgu',I3,' ir',I3,' ordinates Y. Patiksl',
     *'inkite, ivedant ju bendra or-'/1X,'dinate Y, pvz. 7.13 "E"')
      READ *,YM(I)
      YM(M3)=YM(I)
      GO TO 6
   12 IF(IMT(M3,1).EQ.IMT(I,3).OR.IMT(M3,1).GT.0.AND.IMT(I,3).GT.0)
     *GO TO 15
      PRINT *,'Ivesta mazgu Nr              ',I,M3
      PRINT *,'ir strypo tarp ju kodai I1 I2',IMT(I,3),IMT(M3,1)
      PRINT *,'Patikslinkite, ivedant kodus (abu arba 0,arba abu 1...',
     *'3) pvz. 0 0 ar 2 3 "E"'
      READ *,ITM(I,3),ITM(M3,1)
      GO TO 6
   15 CONTINUE
      NX=1
      Y=YM(1)
      K=0
      DO 24 I=1,NM
      IF(YM(I).NE.Y) GO TO 21
      IF(I.LT.NM.AND.XM(I).EQ.XM(I+1).AND.IMT(I,3).EQ.IMT(I,1)) GO TO 24
      K=K+1
      MM(NX,K)=I
      NK(NX)=K
      GO TO 24
   21 Y=YM(I)
      NX=NX+1
      K=1
      NK(NX)=K
      MM(NX,K)=I
   24 CONTINUE
	IF(NX.EQ.1) GO TO 63
      DO 54 I=1,NX-1
      K=NK(I)
      DO 51 J1=1,K
	IF(MAZ.GT.0) GO TO 43
      M=MM(I,J1)
      IF(IMT(M,1).EQ.0.AND.IMT(M,2).EQ.0.AND.IMT(M,3).EQ.0.AND.
     *IMT(M,4).EQ.0) IMT(M,4)=4
      IF(J1.GT.1.AND.XM(M-1).EQ.XM(M).AND.ITM(M,1).NE.ITM(M,3)) GO TO 51
      IF(J1.LT.K.AND.XM(M+1).EQ.XM(M).AND.ITM(M,4).EQ.2) M=M+1
      IT=IMT(M,4)
	IF(IT.EQ.0) GO TO 51
      X=XM(M)
      N=I+1
   36 K2=NK(N)
      DO 39 J2=1,K2
      MA=MM(N,J2)
      IF(J2.LT.K2.AND.XM(MA+1).EQ.XM(MA).AND.IMT(MA,2).EQ.2) MA=MA+1
      X2=XM(MA)
      IF(X2.EQ.X.AND.IMT(MA,1).EQ.0.AND.IMT(MA,2).EQ.0.AND.
     *IMT(MA,3).EQ.0.AND.IMT(MA,4).EQ.0) IMT(MA,2)=4
      I2=IMT(MA,2)
      IF(X2.EQ.X.AND.(IT.EQ.0.OR.I2.EQ.0)) GO TO 43
      IF(X2.EQ.X.AND.IT.GT.0.AND.I2.GT.0) THEN
      MG(M,4)=MA
      MG(MA,2)=M
      IF(J1.LT.K.AND.XM(M+1).EQ.XM(M).AND.ITM(M,2).EQ.2) THEN
      MG(M,2)=M+1
      MG(M+1,4)=M
      ENDIF
      IF(J1.GT.1.AND.XM(M-1).EQ.XM(M).AND.ITM(M,2).EQ.2) THEN
      MG(M,2)=M-1
      MG(M-1,4)=M
      ENDIF
      GO TO 51
      ENDIF
   39 CONTINUE
   42 N=N+1
      IF(N.LE.NX) GO TO 36
   43 IF(X2.EQ.X.AND.(IT.EQ.0.AND.I2.EQ.0)) GO TO 51
      IF(MAZ.NE.0) M=MAZ
   	PRINT 47,M
   47 FORMAT(1X,'Patikslinkite mazgo',I3,' ir mazgo kitame strypo gale',
     *' abscises X bei strypo'/1X,'itvirtinimo juose kodus I1 ir I2,'
     *' ivesdami mazgu Nr (butinai M1<M2) X I1 '/1X,'I2 "E",(pvz. 5 13 ',
     *'6.3  3 2 "E"'/1X,'PASTABA: Jei kitame gale mazgas DVIGUBAS, pak', 
     *'artokite, ivedant kito mazgo Nr')
      READ *,MV,MZ,XM(MV),ITM(MV,4),ITM(MZ,2)
      XM(MZ)=XM(MV)
      MAZ=0
      GO TO 6
   51 CONTINUE
   54 CONTINUE
   63 DO 75 I=1,NM
      IF(IMT(I,2).GT.0.AND.MG(I,2).EQ.0.OR.IMT(I,4).GT.0.AND.MG(I,4).
     *EQ.0) THEN
	MAZ=I
	GO TO 6
	ENDIF 
      DO 72 J=1,4
      BM(I,J)=0.0
      MS(I,J)=0
      M=MG(I,J)
      IF(M.EQ.0) GO TO 72
      IF(J.EQ.1.OR.J.EQ.3) BM(I,J)=ABS(XM(I)-XM(M))
      IF(J.EQ.2.OR.J.EQ.4) BM(I,J)=ABS(YM(I)-YM(M))
      I1=ITM(I,J)
      MS(I,J)=I1
      IF(I1.LT.2) GO TO 72
      L=J+2
      IF(L.GT.4) L=L-4
      IF(ITM(I,J).EQ.3) GO TO 66
      IF(ITM(I,J).EQ.2.AND.ITM(M,L).GT.1) GO TO 66
      IF(ITM(I,J).LT.2.AND.ITM(M,L).EQ.3) GO TO 66
      PRINT 64,I,M
   64 FORMAT(2X,'Neleistini strypo tarp',I3,' ir',I3,' mazgu itvirt',
     *'inimo kodai. PATIKSLINKITE!'/1H ,2X,'Iveskite pvz. 3 1 "E"')
      READ *,ITM(I,J),ITM(M,L)
      GO TO 6
   66 I2=ITM(M,L)
      IF(I1.NE.3) GO TO 69
      IF(I2.EQ.0) MS(I,J)=4
      IF(I2.EQ.1) MS(I,J)=5
      IF(I2.EQ.2) MS(I,J)=6
      IF(I2.EQ.3) MS(I,J)=7
      GO TO 72
   69 IF(I1.NE.2) GO TO 72
      IF(I2.EQ.2) MS(I,J)=2
      IF(I2.EQ.3) MS(I,J)=3
   72 CONTINUE
   75 CONTINUE
      IF(MZG.NE.0) GO TO 90
      DO 92 I=1,NM
      DO 92 J=1,4
      STM(I,J)=0.0
   92 CONTINUE
      DO 96 MA=1,NM
      DO 93 J=1,4
      IF(MS(MA,J).LT.6) GO TO 93
      STM(MA,J)=1.0
   93 CONTINUE
   96 CONTINUE
   90 RETURN
      END
      SUBROUTINE APKR(LJ,NX,NM,XM,YM,MG,AFV,FV,AFH,FH,AWI,WI,APV,PV,
     *APH,PH)
      COMMON /C1/ NFV,NFH,NWI,NPV,NPH,NVH
      DIMENSION XM(NM),YM(NM),MG(NM,4),AFV(3,NFV),FV(NFV,3),AFH(3,NFH),
     *FH(NFH,3),AWI(3,NWI),WI(NWI,3),APV(5,NPV),PV(NPV,5),APH(5,NPH),
     *PH(NPH,5)
      IF(LJ.EQ.O) GO TO 24
	READ (5,3) AIM,ABM
      READ (5,*) AFV
      READ (5,3) AIM,ABM
      READ (5,*) AFH
      READ (5,3) AIM,ABM
      READ (5,*) AWI
      READ (5,3) AIM,ABM
      READ (5,*) APV
      READ (5,3) AIM,ABM
      READ (5,*) APH
      READ (5,3) AIM,ABM
    3 FORMAT(2A4)
      DO 7 I=1,NFV
      DO 6 J=1,3
      FV(I,J)=AFV(J,I)
    6 CONTINUE
    7 CONTINUE
      DO 15 I=1,NPV
      DO 15 J=1,5
      PV(I,J)=APV(J,I)
   15 CONTINUE
      DO 9 I=1,NFH
      DO 9 J=1,3
      FH(I,J)=AFH(J,I)
    9 CONTINUE
	DO 16 I=1,NPH
      DO 16 J=1,5
      PH(I,J)=APH(J,I)
   16 CONTINUE
      DO 12 I=1,NWI
      DO 12 J=1,3
      WI(I,J)=AWI(J,I)
   12 CONTINUE
      GO TO 10
   24 PRINT 4	 
    4 FORMAT(1X,'Pries apkrovu, nukreiptu aukstyn, kairen, o momentu ',
     *'- pries laikr. rodykliu'/1X,'sukimasi, dydzius dedamas "minus"',
     *' zenklas') 
C	PRINT *,NFV,NFH,NWI,NPV,NPH 
C	READ *,U
      IF(NFV.EQ.0) GO TO 41
	PRINT *,'VERTIKALIOS KONCENTRUOTOS pagal schema: DYDIS X Y "E" ',
     *'Pvz. -2.5 3.1 6 "E"'
	DO 26 I=1,NFV
      READ *,(FV(I,J),J=1,3)
   26 CONTINUE
   41 IF(NFH.EQ.0) GO TO 42
      PRINT *,'HORIZONTAL. KONCENTRUOTOS pagal schema: DYDIS X Y "E".',
     *' Pvz. 2.5 3 3.5 "E"'
	DO 28 I=1,NFH
      READ *,(FH(I,J),J=1,3)
   28 CONTINUE
   42 IF(NWI.EQ.0) GO TO 43
      PRINT *,'ISORES MOMENTAI pagal schema: DYDIS X Y "E". ',
     *'Pvz. 12.5 3.1 6.3 "En"'
      DO 32 I=1,NWI	 
      READ *,(WI(I,J),J=1,3)
   32 CONTINUE
   43 IF(NPV.EQ.0) GO TO 44
      PRINT *,'ISSKIRSTYTA VERTIKALI pagal schema: Y P1 X1 P2 X2 "E" ',
     *'t.y. ordinate - dydis'
      PRINT *,'ir abscise kairiame gale - dydis ir abscise desiniame ',
     *'gale - "E" Pvz. 9 3.5'
      PRINT *,'2.5 8 8.5 "E"'
      DO 25 I=1,NPV
      READ *,(PV(I,J),J=1,5)
   25 CONTINUE
   44 IF(NPH.EQ.0) GO TO 10
      PRINT *,'ISSKIRSTYTA HORIZONTALI pagal schema: X P1 Y1 P2 Y2 "E',
     *'" t.y. abscise - dydis'
      PRINT *,'ir ordinate apacioje - dydis ir ordinate virsuje - "E"',
     *' Pvz. 6 7.5  2  0  4.5 "E"'
      DO 38 I=1,NPH
      READ *,(PH(I,J),J=1,5)
   38 CONTINUE
   10 DO 2 I=1,NFV
	XV=FV(I,2)
	YV=FV(I,3)
      DO 1 J=1,NM
      M2=MG(J,2)
      XI=XM(J)
      YI=YM(J)
      M1=MG(J,1)
	IF(XI.EQ.XV.AND.YI.EQ.YV) GO TO 2
	IF(YI.GT.YV) GO TO 1
	IF(XI.LT.XV) GO TO 1
	IF(M2.GT.0.AND.YM(M2).GT.YV.AND.XI.EQ.XV) GO TO 2
	IF(YI.EQ.YV.AND.M1.GT.0.AND.XM(M1).LE.XV) GO TO 2
    1 CONTINUE  
	PRINT 45,I,(FV(I,L),L=1,3)
   45 FORMAT(1X,'Patikslinkite',I3,' vertikalios apkrovos (',
     *3F7.2,')')
      PRINT *,' koordinates, ivesdami X Y'
	READ *,FV(I,2),FV(I,3)
	GO TO 10
    2 CONTINUE  
      DO 13 I=1,NPV
	X1=PV(I,3)
	X2=PV(I,5)
	YF=PV(I,1)
	DO 14 J=1,NM
	IF(X1.GE.X2) GO TO 14
	Y=YM(J)
	X=XM(J)
	M1=MG(J,1)
	IF(Y.NE.YF.OR.X.LT.X1.OR.M1.EQ.0) GO TO 14
	IF(XM(M1).LE.X1) XP=XM(M1)
	IF(X.LT.X2) GO TO 14
	IF(M1.GT.0) XG=XM(J)
	IF(XP.LE.X1.AND.XG.GE.X2) GO TO 13
   14 CONTINUE
	PRINT 46,I,(PV(I,L),L=1,5)
   46 FORMAT(1X,'Patikslinkite',I3,' vertikalios apkrovos (',
     *5F7.2,')')
      PRINT *,' koordinates, ivesdami Y X1 X2'
      READ *,PV(I,1),PV(I,3),PV(I,5)
	GO TO 10
   13 CONTINUE
      DO 8 I=1,NFH
	XH=FH(I,2)
	YH=FH(I,3)
      DO 5 J=1,NM
      M2=MG(J,2)
      XI=XM(J)
      YI=YM(J)
      M1=MG(J,1)
	IF(XI.EQ.XH.AND.YI.EQ.YH) GO TO 8
	IF(YI.GT.YH) GO TO 5
	IF(XI.LT.XH) GO TO 5
	IF(M2.GT.0.AND.YM(M2).GT.YH.AND.XI.EQ.XH) GO TO 8
	IF(YI.EQ.YH.AND.M1.GT.0.AND.XM(M1).LE.XH) GO TO 8
    5 CONTINUE  
	PRINT 19,I,(FH(I,L),L=1,3)
   19 FORMAT(1X,'Patikslinkite',I3,' horizontalios apkrovos (',
     *3F7.2,')')
      PRINT *,' koordinates, ivesdami X Y'C
	READ *,FH(I,2),FH(I,3)
	GO TO 10
    8 CONTINUE 
      DO 18 I=1,NPH
	Y1=PH(I,3)
	Y2=PH(I,5)
	XF=PH(I,1)
	DO 17 J=1,NM
	Y=YM(J)
	X=XM(J)
	IF(Y1.GE.Y2.OR.Y2.LT.Y) GO TO 17
	M2=MG(J,2)
	IF(X.NE.XF.OR.M2.EQ.0) GO TO 17
	IF( Y2.LE.YM(M2).AND.Y2.GE.Y) YG=Y2
	IF( Y1.GE.Y.AND.Y1.LT.YM(M2)) YP=Y1
	IF(X.NE.XF.OR.Y.GT.Y2.OR.M2.EQ.0) GO TO 17
	IF(X.NE.XF) GO TO 17
	IF(MG(M2,2).EQ.0) YG=YM(M2)
	IF(YP.GT.Y2) GO TO 17
	IF(YM(M2).GE.Y2.AND.YM(M2).LT.YG) YG=YM(M2)
	IF(Y.GT.Y1) GO TO 17
	YP=YM(J)
	IF(YP.LE.Y1.AND.YG.GE.Y2) GO TO 18
   17 CONTINUE
	PRINT 36,I,(PH(I,L),L=1,5)
   36 FORMAT(1X,'Patikslinkite',I3,' horizontalios apkrovos (',
     *5F7.2,')')
      PRINT *,' koordinates, ivesdami X Y1 Y2'
      READ *,PH(I,1),PH(I,3),PH(I,5)
	GO TO 10
   18 CONTINUE
	DO 23 I=1,NWI
	XW=WI(I,2)
	YW=WI(I,3)
      DO 22 J=1,NM
      X=XM(J)
      Y=YM(J)
      M1=MG(J,1)
      M2=MG(J,2)
	IF(X.EQ.XW.AND.Y.EQ.YW) GO TO 23
	IF(Y.GT.YW) GO TO 22
	IF(X.LT.XW) GO TO 22
	IF(M2.GT.0.AND.YM(M2).GT.YW.AND.X.EQ.XW) GO TO 23
	IF(Y.EQ.YW.AND.M1.GT.0.AND.XM(M1).LE.XW) GO TO 23
   22 CONTINUE  
	PRINT 29,I,(WI(I,L),L=1,3)
   29 FORMAT(1X,'Patikslinkite',I3,' apkrovu momento (',3F7.2,')')
      PRINT *,' koordinates, ivesdami X Y'
	READ *,WI(I,2),WI(I,3)
	GO TO 10
   23 CONTINUE 
   27 RETURN
      END
      SUBROUTINE MAZG(ID,NM,AST,STM,BM,MS,MG)
      DIMENSION STM(NM,4),BM(NM,4),MS(NM,4),MG(NM,4),AST(3,200)
      DO 2 I=1,NM
      DO 2 J=1,4
      IF(MS(I,J).GT.5.AND.BM(I,J).NE.0.AND.STM(I,J).EQ.0.) STM(I,J)=1.0
    2 CONTINUE
      IF(ID.EQ.0) GO TO 50
      IF(ID.NE.2) GO TO 15
      PRINT *,'Iveskite santykinius standumus (ST>1,0) tik tu strypu,',
     *' kuriu vienas galas '
      PRINT *,'(mazge M1) itvirtintas standziai, kitas galas (mazge',
     *' M2) STATMENAI ATREMTAS,'
      PRINT *,'t.y. M1 M2 ST pvz. 5 11 1.8'
      PRINT *,'PASTABA: Standziai abiem galais itvirtinto strypo pakanka',
     * nurodyti vieno '
	PRINT *,'galo koda.    SUKLYDUS - PAKARTOKITE! (mazgu Nr pastebi '
     *'kompiuteris)' 
      DO 12 I=1,400
    5 READ *,M1,M2,S
      IF(M1.EQ.0) GO TO 15
	CALL STAN(NM,M1,M2,S,MS,MG,STM,MA,MB,KL,ID)
   12 CONTINUE
   15 IF(ID.NE.1) GO TO 50
      READ (5,*,ERR=16) AST 
   16 DO 21 L=1,200
      M1=AST(1,L)
      M2=AST(2,L)
	S=AST(3,L)
      IF(M1.EQ.0) GO TO 50
    7	CALL STAN(NM,M1,M2,S,MS,MG,STM,MA,MB,KL,ID)
   21 CONTINUE
   50 RETURN
      END
	SUBROUTINE STAN(NM,M1,M2,S,MS,MG,STM,MA,MB,KL,ID)
	DIMENSION MS(NM,4),MG(NM,4),STM(NM,4)
	KL=0
      DO 6 MA=1,NM
      DO 3 J=1,4
      I1=MS(MA,J)
      IF(I1.LT.6) GO TO 3
      MB=MG(MA,J)
      J3=J+2
      IF(J3.GT.4) J3=J3-4
      IF(MA.EQ.M1.AND.M2.EQ.MB) GO TO 9
    3 CONTINUE
    6 CONTINUE
	KL=1
	GO TO 10
    9 STM(MA,J)=S
      IF(ID.NE.1) PRINT *,'Ivesta:',MA,MB,STM(MA,J)
      IF(I1.EQ.7) STM(MB,J3)=STM(MA,J)
      IF(I1.EQ.7.AND.ID.NE.1) PRINT *,'Ivesta:',MB,MA,STM(MB,J3)
   10 RETURN
      END
      SUBROUTINE PAGR(NU,MZG,NM,NCM,NC,NVT,NHT,XM,YM,FV,
     *FH,WI,PV,PH,MG,SK,ITM,MS,STM,BM,UM,QM,WM,MI,WP,XX,VTM,
     *HTM,VPM,HPM,MTV,MTH,VH,HH,WH,MVT,MHT,MVH,MVP,MHP,CPM)
      COMMON /C1/ NFV,NFH,NWI,NPV,NPH,NVH
      COMMON /C2/WK,WD,QK,AK,YI,XK,XD,CX,WCK,WCD,ACK,ACD,QCK,QCD
      DIMENSION XM(150),YM(150),MG(NM,4),STM(NM,4),MS(NM,4),ITM(NM,4),
     *BM(NM,4),UM(NM,4),SK(NM)
      DIMENSION FV(NFV,3),FH(NFH,3),WI(NWI,3),PV(NPV,5),PH(NPH,5)
      DIMENSION QM(NM,4),WM(NM,4),WP(NM,4),MI(50),VTM(NVT),HTM(NHT),
     *VPM(50),HPM(50),MVT(NVT),MHT(NHT),MVP(50,3),MHP(50,3),CPM(50,8),
     *MTV(50,3),MTH(50,3),XX(50),VH(NFV,3),HH(NFH,3),WH(NWI,3),MVH(NVH)
   41 DO 15 I=1,NM
      DO 15 J=1,4
      QM(I,J)=0.0
      WM(I,J)=0.0
   15 CONTINUE
      DO 39 L=1,NFV
      VH(L,1)=0-FV(L,1)
      VH(L,2)=FV(L,3)
      VH(L,3)=FV(L,2)
   39 CONTINUE
   50 DO 37 L=1,NFH
      HH(L,1)=FH(L,1)
      HH(L,2)=FH(L,3)
      HH(L,3)=FH(L,2)
   37 CONTINUE
      DO 38 L=1,NWI
      WH(L,1)=WI(L,1)
      WH(L,2)=WI(L,3)
      WH(L,3)=WI(L,2)
   38 CONTINUE
      VK=0.0
      HK=0.0
      DO 42 I=1,NM
      SM=0
      XI=XM(I)
      YI=YM(I)
      M5=MG(I,1)
      M6=MG(I,2)
      DO 36 J=1,4
      UM(I,J)=0.0
      M=MG(I,J)
      IF(M.NE.0) XJ=XM(M)
      IF(M.NE.0) YJ=YM(M)
      B=BM(I,J)
      K1=MS(I,J)
      L=J+2
      IF(L.GT.4) L=L-4
      IF(M.NE.0.AND.K1.EQ.3.AND.B.NE.0.) STM(I,J)=STM(M,L)
      IF(K1.LT.2.OR.K1.EQ.3.OR.B.EQ.0.) GO TO 36
      IF(K1.EQ.6) UM(I,J)=STM(I,J)*0.75/B
      IF(K1.EQ.7) UM(I,J)=STM(I,J)/B
      DO 27 K=1,NVT
      IL=MVT(K)
      DO 24 N=1,4
      IF(ITM(IL,N).NE.3) GO TO 24
      J1=N+1
      IF(J1.GT.4) J1=J1-4
      J2=N+2
      IF(J2.GT.4) J2=J2-4
      J3=N+3
      IF(J3.GT.4) J3=J3-4
      IF(ITM(IL,J1).EQ.0.AND.ITM(IL,J2).EQ.0.AND.ITM(IL,J3).EQ.0)
     *UM(IL,N)=0.0
   24 CONTINUE
   27 CONTINUE
      SM=UM(I,J)+SM
      IF(J.EQ.2.OR.J.EQ.4) GO TO 30
      CALL PRAD(J,K1,B,XI,XJ,YI,NFV,FV,NWI,WI,NPV,PV,R1,R2,W)
      GO TO 33
   30 CALL PRAD(J,K1,B,YI,YJ,XI,NFH,HH,NWI,WH,NPH,PH,R1,R2,W)
   33 QM(I,J)=QM(I,J)+R1
      IF(M.NE.0) QM(M,L)=QM(M,L)+R2
      WM(I,J)=WM(I,J)+W
   36 CONTINUE
      IF(SM.EQ.0.) GO TO 42
      DO 29 J=1,4
      UM(I,J)=UM(I,J)/SM
   29 CONTINUE
   42 CONTINUE
   60 DO 69 J=1,NWI
      DO 66 I=1,NM
      IF(WI(J,2).NE.XM(I).OR.WI(J,3).NE.YM(I)) GO TO 66
      DO 65 K=1,4
      M=MG(I,K)
      K2=K+2
      IF(K2.GT.4) K2=K2-4
      WM(I,K)=WM(I,K)-WI(J,1)*UM(I,K)
      IF(MS(I,K).NE.0.OR.M.EQ.0) GO TO 65
      WM(I,K)=WI(J,1)
      WM(M,K2)=WM(M,K2)-WI(J,1)
   65 CONTINUE
   66 CONTINUE
   69 CONTINUE
      DO 72 I=1,NM
      DO 71 J=1,4
      WP(I,J)=WM(I,J)
   71 CONTINUE
   72 CONTINUE
      CALL CIK(NM,MG,MS,UM,WM,NVH,MVH)
      DO 75 I=1,NM
      DO 75 J=1,4
      WP(I,J)=WM(I,J)-WP(I,J)
   75 CONTINUE
      CO=1.0
      NN=0
      CALL SKER(NN,NM,CO,MG,MS,BM,WP,WM,QM)
      DO 78 I=1,NM
      SK(I)=XM(I)
   78 CONTINUE
      DO 84 I=1,NM
      SN=SK(I)
      K=I+1
      DO 81 J=K,NM
      S=SK(J)
      IF(S.GT.SN) GO TO 81
      SK(J)=SK(I)
      SK(I)=S
      SN=S
   81 CONTINUE
   84 CONTINUE
      NX=1
      XX(1)=0.0
      DO 87 I=1,NM
      IF(SK(I).LE.XX(NX)) GO TO 87
      NX=NX+1
      XX(NX)=SK(I)
   87 CONTINUE
   90 NVP=0
      DO 108 K=1,NX
      AT=0.0
      IP=0
      IG=0
      JV=0
      DO 105 I=1,NM
      IF(XM(I).NE.XX(K)) GO TO 105
      IF(IG.NE.0.AND.I.LE.IG) GO TO 105
      IF((ITM(I,3).EQ.0.AND.BM(I,3).NE.0.).OR.(ITM(I,1).EQ.0.AND.
     *BM(I,1).NE.0.)) GO TO 105
      IF(MG(I,2).EQ.0) IP=I
      AT=QM(I,3)+QM(I,1)+AT
      IF(JV.EQ.0.AND.(MS(I,1).EQ.3.OR.MS(I,1).GT.5)) JV=I
      IF(JV.EQ.0.AND.(MS(I,3).EQ.3.OR.MS(I,3).GT.5)) JV=I
      IF(MG(I,4).NE.0) GO TO 105
      IG=I
      DO 93 L=1,NFV
      F=FV(L,1)
      XF=FV(L,2)
      YF=FV(L,3)
      IF(XF.EQ.XX(K).AND.YF.LE.YM(IP).AND.YF.GE.YM(I)) AT=AT+F
   93 CONTINUE
      IF(NVT.EQ.0) GO TO 99
      MA=0
      DO 96 J=1,NVT
      M=MVT(J)
      IF(M.LT.IP.OR.M.GT.IG) GO TO 96
      IF(IP.EQ.0) GO TO 97
      IF(XM(M).NE.XM(I).OR.YM(M).GT.YM(IP).OR.YM(M).LT.YM(I)) GO TO 96
      MA=1
      VTM(J)=AT
      MTV(J,1)=IP
      MTV(J,2)=I
      MTV(J,3)=MVT(J)
   96 CONTINUE
   97 IF(MA.NE.0) GO TO 102
   99 NVP=NVP+1
      MVP(NVP,1)=IP
      MVP(NVP,2)=I
      MVP(NVP,3)=JV
      VPM(NVP)=AT
  102 AT=0.0
      IP=0
      JV=0
  105 CONTINUE
  108 CONTINUE
      NHP=0
      IG=0
      JH=0
      DO 129 I=1,NM
      IF(MG(I,1).NE.0) GO TO 111
      IF(IG.NE.0.AND.I.LE.IG) GO TO 129
      M4=MG(I,4)
      IF(M4.NE.0.AND.MS(M4,2).EQ.4) GO TO 129
      M2=MG(I,2)
      IF(M2.NE.0.AND.MS(M2,4).EQ.4) GO TO 129
      IP=I
      AT=QM(I,2)+QM(I,4)
      GO TO 114
  111 IF(YM(I).NE.YM(IP).OR.I.LE.IG) GO TO 129
      AT=QM(I,2)+QM(I,4)+AT
  114 IF(JH.EQ.0.AND.(MS(I,4).EQ.3.OR.MS(I,4).GT.5)) JH=I
      IF(JH.EQ.0.AND.(MS(I,2).EQ.3.OR.MS(I,2).GT.5)) JH=I
      IF(MG(I,3).NE.0) GO TO 129
      IG=I
      DO 117 L=1,NFH
      F=FH(L,1)
      XF=FH(L,2)
      YF=FH(L,3)
      IF(YF.EQ.YM(IP).AND.XF.GE.XM(IP).AND.XF.LE.XM(I)) AT=AT+F
  117 CONTINUE
      IF(NHT.EQ.0) GO TO 123
      MA=0
      DO 120 J=1,NHT
      M=MHT(J)
      IF(M.LT.IP.OR.M.GT.IG) GO TO 120
      IF(YM(M).NE.YM(I).OR.XM(M).LT.XM(IP).OR.XM(M).GT.XM(I)) GO TO 120
      MA=1
      HTM(J)=AT
      IF(YM(IP).EQ.YM(M)) MTH(J,1)=IP
      IF(YM(IG).EQ.YM(M)) MTH(J,2)=IG
      MTH(J,3)=MHT(J)
  120 CONTINUE
      IF(MA.NE.0) GO TO 126
  123 NHP=NHP+1
      MHP(NHP,1)=IP
      MHP(NHP,2)=IG
      MHP(NHP,3)=JH
      HPM(NHP)=AT
  126 AT=0.0
      IP=0
      IG=0
      JH=0
  129 CONTINUE
      IF(NHP.EQ.0) GO TO 135
      KH=1
      DO 132 K=1,NHP
      IF(ABS(HPM(K)).GT.ABS(HPM(KH))) KH=K
  132 CONTINUE
      IH=MHP(KH,1)
      LH=MHP(KH,2)
      JH=MHP(KH,3)
      HPP=HPM(KH)
  135 IF(NVP.EQ.0) GO TO 141
      KV=1
      DO 138 K=1,NVP
      IF(ABS(VPM(K)).GT.ABS(VPM(KV))) KV=K
  138 CONTINUE
      IV=MVP(KV,1)
      LV=MVP(KV,2)
CKKKKKKKKKKKKK
C   	PRINT *,KK,MZG,LV,'  KK MZG'  
      JV=MVP(KV,3)
      VPP=VPM(KV)
      IF(ABS(VPP).GT.ABS(HPP)) GO TO 147
  141 ISK=ISK+1
      IF(ABS(HPP).LT.0.0001.AND.MZG.EQ.0) GO TO 203
  200 DO 144 J=2,4,2
      IF(JH.EQ.0.OR.STM(JH,J).EQ.0.) GO TO 144
      CST=STM(JH,J)
      BST=BM(JH,J)
  144 CONTINUE
      K5=2
      CALL ASV(K5,MZG,IH,LH,NM,XM,YM,HPP,BST,CST,MS,MG,STM,UM,NVH,MVH,
     *BM,WP,WM,QM)
      IF(MZG.NE.0) GO TO 240
      GO TO 90
  147 ISK=ISK+1
      IF(ABS(VPP).LT.0.0001.AND.MZG.EQ.0) GO TO 203
  202 DO 150 J=1,3,2
      IF(JV.EQ.0.OR.STM(JV,J).EQ.0.) GO TO 150
      CST=STM(JV,J)
      BST=BM(JV,J)
  150 CONTINUE
      K5=1
      CALL ASV(K5,MZG,IV,LV,NM,XM,YM,VPP,BST,CST,MS,MG,STM,UM,NVH,MVH,
     *BM,WP,WM,QM)
      IF(ISK.GT.2000) THEN
      MZG=LV
CKKKKKKKKKKKKK
C   	PRINT *,KK,MZG,' KK MZG'  
      ISK=0
      GO TO 240
      ENDIF
      GO TO 90
  203 DO 159 J=1,NWI
      WW=WI(J,1)
      DO 156 I=1,NM
      X=WI(J,2)
      IF(X.NE.XM(I)) GO TO 156
      Y=WI(J,3)
      IF(Y.NE.YM(I)) GO TO 156
      WM(I,1)=WM(I,1)+WW*UM(I,1)
      WM(I,2)=WM(I,2)+WW*UM(I,2)
      WM(I,3)=WM(I,3)+WW*UM(I,3)
      WM(I,4)=WM(I,4)+WW*UM(I,4)
  156 CONTINUE
  159 CONTINUE
      DO 162 I=1,NM
      DO 162 J=1,4
      UM(I,J)=0.0
  162 CONTINUE
      DO 165 K=1,NVT
      M=MTV(K,3)
      IP=MTV(K,1)
      AT=VTM(K)
      IG=MTV(K,2)
      CALL ASHN(NM,M,IP,IG,AT,XM,YM,NFV,FV,MG,QM,UM)
  165 CONTINUE
      AT=0.0
      DO 168 K=1,NVP
      M=MVP(K,3)
      IP=MVP(K,1)
      IG=MVP(K,2)
      CALL ASHN(NM,M,IP,IG,AT,XM,YM,NFV,FV,MG,QM,UM)
  168 CONTINUE
      DO 171 K=1,NHT
      M=MTH(K,3)
      IP=MTH(K,1)
      IG=MTH(K,2)
      AT=HTM(K)
      IF(M.EQ.0) GO TO 171
      CALL ASI(NM,M,IP,IG,AT,XM,YM,NFH,FH,MG,QM,UM)
  171 CONTINUE
      AT=0.0
      DO 174 K=1,NHP
      M=MHP(K,3)
      IP=MHP(K,1)
      IG=MHP(K,2)
      IF(M.EQ.0) GO TO 174
      CALL ASI(NM,M,IP,IG,AT,XM,YM,NFH,FH,MG,QM,UM)
  174 CONTINUE
	S=0.005
      W=0.0
      DO 163 I=1,NM
      SW=WM(I,1)+WM(I,2)+WM(I,3)+WM(I,4)
      DO 161 L=1,NVH
      IF(MVH(L).EQ.I) GO TO 163
  161 CONTINUE
      DO 166 L=1,NHT
      IF(MHT(L).EQ.I) GO TO 163
  166 CONTINUE
      DO 160 L=1,NWI
      W=0
      XW=WI(L,2)
      YW=WI(L,3)
      IF(XW.EQ.XM(I).AND.YW.EQ.YM(I)) THEN
      W=WI(L,1)
      GO TO 167
      ENDIF
  160 CONTINUE
      GO TO 163
  167 SW=WM(I,1)+WM(I,2)+WM(I,3)+WM(I,4)-W
      IF(ABS(SW).LE.S) GO TO 163
      MZG=I
      GO TO 240
  163 CONTINUE
      WRITE(6,1)
    1 FORMAT(1H ,15X,'Daugiaangio, daugiaaukscio remo tik is vertika',
     *'liu'/1H ,16X,'ir horizontaliu strypu arba daugiaatrames sij',
     *'os'/1H ,30X,'irazu skaiciavimas'/1H ,30X,'Programa "REMA',
     *'SIJA"'/1H ,52X,'2'/1H ,52X,'|'/1H ,21X,'Mazgo strypu ',
     *'numeracija:  1 -- * -- 3'/1H ,52X,'|'/1H ,52X,'4'/1H ,8X,'!!!',
     *' PATIKRINK IVESTUS DUOMENIS'/1H ,10X,'Mazgo',9X,'Gret.mazgu Nr',
     *4X,'Itvirt. kodai',5X,'Sant.standumai'/1H ,5X,'NR',4X,'X',6X,
     *'Y',5X,'1',3X,'2',3X,'3',3X,'4',4X,'1',3X,'2',3X,'3',3X,'4',4X,
     *'1',4X,'2',4X,'3',4X,'4'/1H ,4X,17('-'),2(2X,15('-')),2X,19('-'))
      DO 2 I=1,NM
      DO 2 J=1,4
      IF(MS(I,J).LT.6) STM(I,J)=0
    2 CONTINUE
      DO 348 I=1,NM
      WRITE(6,342) I,XM(I),YM(I),(MG(I,J),J=1,4),(ITM(I,J),J=1,4),
     *(STM(I,J),J=1,4)
  342 FORMAT(1H ,4X,I2,2F7.2,1X,4I4,1X,4I4,1X,4F5.1)
  348 CONTINUE
      WRITE(6,331)
  331 FORMAT(/1H ,7X,'Pastaba. Strypu standumuose nulis reiskia, kad',
     *' strypo arba nera, arba'/1H ,16X,'jo standumas neitakingas.')
      CALL SPAPK(FV,FH,WI,PV,PH,NVT,MVT,NHT,MHT,MVH)
      WRITE(6,177)
  177 FORMAT(/1H ,10X,'R E Z U L T A T A I:'/1H ,9X,'Atramines reak',
     *'cijos:'/1H ,10X,'(TEIGIAMOS - aukstyn, kairen ir strypas atra',
     *'mos atzvilgiu'/1H ,10X,'lenkiamas priesinga laikrodzio rodykle',
     *'s judejimui kryptimi).'/1H ,20X,35('-')/1H ,20X,':ATRAMA:   RV',
     *'   :   RH   :    M   :'/1H ,20X,':------:',3('--------:'))
      NA=NVT+NHT
      DO 183 I=1,NA
      IF(I.GT.NVT) GO TO 180
      K=MVT(I)
      MI(I)=K
      GO TO 183
  180 J=I-NVT
      K=MHT(J)
      MI(I)=K
  183 CONTINUE
      DO 189 I=1,NA
      MM=MI(I)
      K=I+1
      DO 186 J=K,NA
      M=MI(J)
      IF(M.GE.MM) GO TO 186
      MI(J)=MI(I)
      MI(I)=M
      MM=M
  186 CONTINUE
  189 CONTINUE
      DO 204 K=1,NA
      I=MI(K)
      IF(K.NE.1.AND.I.EQ.MI(K-1)) GO TO 204
      V=0.0
      H=0.0
      W=0.0
      DO 192 J=1,NVT
      IF(MVT(J).EQ.I) V=VTM(J)
  192 CONTINUE
      DO 195 J=1,NHT
      IF(MHT(J).EQ.I) H=HTM(J)
  195 CONTINUE
      DO 198 J=1,4
      IF(W.NE.0.) GO TO 198
      M=MG(I,J)
      M1=J+1
      IF(M1.GT.4) M1=M1-4
      M1=MG(I,M1)
      M2=J+2
      IF(M2.GT.4) M2=M2-4
      M2=MG(I,M2)
      M3=J+3
      IF(M3.GT.4) M3=M3-4
      M3=MG(I,M3)
      IF(M.NE.0.AND.M1.EQ.0.AND.M2.EQ.0.AND.M3.EQ.0) W=WM(I,J)
  198 CONTINUE
      DO 199 J=1,NVH
      IF(I.EQ.MVH(J)) W=WM(I,1)+WM(I,2)+WM(I,3)+WM(I,4)
  199 CONTINUE
      WRITE(6,201) I,V,H,W
  201 FORMAT(1H ,23X,I2,3X,F8.3,2F9.3)
  204 CONTINUE
      Y0=0.0
      IF(YM(NM).NE.YM(1)) Y0=2
      CALL REZ(Y0,NM,MS,WM,UM,QM)
      WRITE(6,*) '.'
      WRITE(6,*) '                             Tas pats tarpiniu pjuviu'
  210 KL=0
  239 CALL TARP(Y0,NM,NC,NCM,XM,YM,MG,MS,NFV,FV,HH,NFH,FH,VH,NWI,WI,
     *WH,NPV,PV,NPH,PH,WM,QM,UM,CPM,XX)
  240 RETURN
      END
      SUBROUTINE SPAPK(FV,FH,WI,PV,PH,NVT,MVT,NHT,MHT,MVH)
      COMMON /C1/ NFV,NFH,NWI,NPV,NPH,NVH
      DIMENSION FV(NFV,3),FH(NFH,3),WI(NWI,3),PV(NPV,5),PH(NPH,5),
     *MVT(NVT),MHT(NHT),MVH(NVH)
      WRITE(6,48) (MVT(I),I=1,NVT)
   48 FORMAT(/1H ,7X,'Vertikaliu atramu numeriai:'/1H ,7X,20I3)
      WRITE(6,51) (MHT(I),I=1,NHT)
   51 FORMAT(/1H ,7X,'Horizontaliu atramu numeriai:'/1H 7X,20I3)
      WRITE(6,52) (MVH(I),I=1,NVH)
   52 FORMAT(/1H ,7X,'Atramu, kuriose standziai itvirtinta >1 stry',
     *'pas, numeriai:'/1H ,7X,20I3)
      WRITE(6,3)
    3 FORMAT(/1H ,8X,'Vert.konc.apkrovu',7X,'Horiz.konc.apkrovu',9X,'I',
     *'sores momentu'/1H ,9X,2('F',6X,'X',6X,'Y',10X),'M',6X,'X',6X,'Y')
      IFG=NFV
      IF(NFH.GT.NFV) IFG=NFH
      IF(NWI.GT.IFG) IFG=NWI
      DO 27 I=1,IFG
      IF(I.LE.NFV.AND.I.LE.NFH.AND.I.LE.NWI)
     *WRITE(6,6) (FV(I,J),J=1,3),(FH(I,J),J=1,3),(WI(I,J),J=1,3)
    6 FORMAT(6X,3F7.2,4X,3F7.2,4X,3F7.2)
      IF(I.LE.NFV.AND.I.LE.NFH.AND.I.GT.NWI)
     *WRITE(6,9) (FV(I,J),J=1,3),(FH(I,J),J=1,3)
    9 FORMAT(6X,3F7.2,4X,3F7.2)
      IF(I.LE.NFV.AND.I.GT.NFH.AND.I.GT.NWI)
     *WRITE(6,12) (FV(I,J),J=1,3)
   12 FORMAT(6X,3F7.2)
      IF(I.LE.NFV.AND.I.GT.NFH.AND.I.LE.NWI)
     *WRITE(6,15) (FV(I,J),J=1,3),(WI(I,J),J=1,3)
   15 FORMAT(6X,3F7.2,29X,3F7.2)
      IF(I.GT.NFV.AND.I.LE.NFH.AND.I.LE.NWI)
     *WRITE(6,18) (FH(I,J),J=1,3),(WI(I,J),J=1,3)
   18 FORMAT(31X,3F7.2,4X,3F7.2)
      IF(I.GT.NFV.AND.I.GT.NFH.AND.I.LE.NWI)
     *WRITE(6,21) (WI(I,J),J=1,3)
   21 FORMAT(56X,3F7.2)
      IF(I.GT.NFV.AND.I.LE.NFH.AND.I.GT.NWI)
     *WRITE(6,24) (FH(I,J),J=1,3)
   24 FORMAT(31X,3F7.2)
   27 CONTINUE
      WRITE(6,30)
   30 FORMAT(/1H ,11X,'Vert.isskirst.apkrovu',19X,'Horiz.iss',
     *'kirst.apkrovu'/1H ,7X,'Y',6X,'Pk',4X,'Xk',5X,'Pd',4X,
     *'Xd',12X,'X',6X,'Pa',4X,'Ya',5X,'Pv',4X,'Yv')
      IFG=NPV
      IF(NPH.GT.NPV) IFG=NPH
      DO 45 I=1,IFG
      IF(I.LE.NPV.AND.I.LE.NPH) WRITE(6,36) (PV(I,J),J=1,5),
     *(PH(I,J),J=1,5)
   36 FORMAT(5X,F6.2,2(F7.2,F6.2),8X,F6.2,2(F7.2,F6.2))
      IF(I.LE.NPV.AND.I.GT.NPH) WRITE(6,39) (PV(I,J),J=1,5)
   39 FORMAT(5X,F6.2,2(F7.2,F6.2))
      IF(I.GT.NPV.AND.I.LE.NPH) WRITE(6,42) (PH(I,J),J=1,5)
   42 FORMAT(45X,F6.2,2(F7.2,F6.2))
   45 CONTINUE
      RETURN
      END
      SUBROUTINE PRAD(L,K1,B,XI,XJ,YI,NFV,FV,NWI,WI,NPV,PV,R1,R2,W)
      DIMENSION FV(NFV,3),WI(NWI,3),PV(NPV,5)
      R1=0.0
      R2=0.0
      W=0.0
      J=L
      IF(L.EQ.2) J=3
      IF(L.EQ.4) J=1
      DO 12 K=1,NFV
      F=FV(K,1)
      XF=FV(K,2)
      YF=FV(K,3)
      U=ABS(XI-XF)
      V=B-U
      IF(YF.NE.YI) GO TO 12
      IF(K1.NE.4.OR.XF.NE.XJ) GO TO 4
      R1=R1+F
      IF(J.EQ.1) R2=0-F
      IF(J.EQ.3) R2=F
      IF(J.EQ.1) W=W+F*U
      IF(J.EQ.3) W=W-F*U
      GO TO 12
    4 IF((J.EQ.1.AND.XF.LE.XJ).OR.(J.EQ.3.AND.XF.LE.XI)) GO TO 12
      IF((J.EQ.1.AND.XF.GE.XI).OR.(J.EQ.3.AND.XF.GE.XJ)) GO TO 12
      IF(K1.NE.4.AND.K1.NE.5) GO TO 6
      R1=R1+F
      IF(J.EQ.1) W=W+F*U
      IF(J.EQ.3) W=W-F*U
      GO TO 12
    6 IF(K1.EQ.2) R1=R1+F*V/B
      IF(K1.NE.6) GO TO 9
      R=(3*B*B-V*V)*F*V/(2*B**3)
      R1=R1+R
      R2=F-R+R2
      WW=(B+V)*U*V*F/(2*B*B)
      W=W+WW
      IF(J.EQ.3) W=W-2*WW
      GO TO 12
    9 IF(K1.NE.7) GO TO 12
      R1=R1+(3*U+V)*F*V*V/(B*B*B)
      IF(J.EQ.1) W=W+F*V*V*U/(B*B)
      IF(J.EQ.3) W=W-F*V*V*U/(B*B)
   12 CONTINUE
      DO 24 K=1,NPV
      YF=PV(K,1)
      IF(YF.NE.YI) GO TO 24
      X1=PV(K,3)
      X2=PV(K,5)
      IF((J.EQ.1.AND.X2.LE.XJ).OR.(J.EQ.3.AND.X2.LE.XI)) GO TO 24
      IF((J.EQ.1.AND.X1.GE.XI).OR.(J.EQ.3.AND.X1.GE.XJ)) GO TO 24
      P1=PV(K,2)
      P2=PV(K,4)
      PK=P1
      PD=P2
      U=XI-X2
      IF(J.EQ.1.AND.U.NE.0.) PK=P2
      IF(J.EQ.3) U=X1-XI
      V=X1-XJ
      IF(J.EQ.1.AND.V.NE.0.) PD=P1
      IF(J.EQ.3) V=XJ-X2
      IF(U.LT.0.) U=0.0
      IF(V.LT.0.) V=0.0
      IF(U.EQ.0.) PK=P1+(P2-P1)*(XI-X1)/(X2-X1)
      IF(V.EQ.0.) PD=P2-(P2-P1)*(X2-XJ)/(X2-X1)
      C=B-U-V
      Y=U+C/2
      Z=V+C/2
      IF(K1.EQ.2) R1=R1+((V+C*2/3)*PK+(V+C/3)*PD)*C*0.5/B
      IF(K1.NE.6) GO TO 18
      IF(P2.EQ.P1) GO TO 17
      CV=C+V
      PF=PK*CV/C
      W1=(3*CV*CV-5*B*B)*PF*CV*CV/(30*B*B)
      R11=PF*CV*CV/(3*B)-W1/B
      W2=(5*B*B-3*V*V)*(PF-PK)*V*V/(30*B*B)
      R12=(PF-PK)*V*V/(3*B)+W2/B
      W3=(B+Z-C*C/(4*Y))*(PF-PK)*C*Y*Z/(2*B*B)
      R13=((PF-PK)*C*Z+W3)/B
      PF=PD*CV/C
      W4=(3*CV*CV/(B*B)-10)*PF*CV*CV/120
      R14=(PF*CV*CV/6-W4)/B
      W5=(10-3*V*V/(B*B))*(PF-PD)*V*V/120
      R15=((PF-PD)*V*V/6+W5)/B
      W6=(2-V*V/(B*B))*PD*V*V/8
      R16=(PD*V*V/2+W6)/B
      RK=R11-R12-R13+R14-R15-R16
      R2=(PK+PD)*C*0.5-RK+R2
      R1=R1+RK
      WK=W1+W2+W3+W4+W5+W6
      IF(J.EQ.1) W=W-WK
      IF(J.EQ.3) W=W+WK
      GO TO 24
   17 WK=(B+Z-0.25*C*C/Y)*P2*C*Y*Z/(2*B*B)
      R1=R1+(P2*C*Z+WK)/B
      R2=R2+(P2*C*Y-WK)/B
      IF(J.EQ.1) W=W+WK
      IF(J.EQ.3) W=W-WK
      GO TO 24
   18 IF(K1.NE.7) GO TO 21
      IF(P2.EQ.P1) GO TO 19
      CV=C+V
      PF=PK*CV/C
      WK1=(4*CV-5*B)*PF*CV*CV*CV/(20*B*B)
      WD1=(10-15*CV/B+6*CV*CV/(B*B))*PF*CV*CV/30
      R11=(PF*CV*CV/3-WK1-WD1)/B
      WK2=(5*B-4*V)*(PF-PK)*V*V*V/(20*B*B)
      WD2=(10-15*V/B+6*V*V/(B*B))*(PF-PK)*V*V/30
      R12=((PF-PK)*V*V/3+WK2-WD2)/B
      WK3=(Y*Z*Z-(2*Z-Y)*C*C/12)*(PF-PK)*C/(B*B)
      WD3=(Y*Y*Z-(2*Y-Z)*C*C/12)*(PF-PK)*C/(B*B)
      R13=((PF-PK)*C*Z+WK3-WD3)/B
      CU=C+U
      PF=PD*CU/C
      WK4=(15*CU/B-10-6*CU*CU/(B*B))*PF*CU*CU/30
      WD4=(5*B-4*CU)*PF*CU*CU*CU/(20*B*B)
      R14=((3*B-2*CU)*PF*CU/6-WK4-WD4)/B
      WK5=(10-15*U/B+6*U*U/(B*B))*(PF-PD)*U*U/30
      WD5=(5*B-4*U)*(PF-PD)*U*U*U/(20*B*B)
      R15=((3*B-2*U)*(PF-PD)*U/6+WK5-WD5)/B
      WK6=(Y*Z*Z-(2*Z-Y)*C*C/12)*(PF-PD)*C/(B*B)
      WD6=(Y*Y*Z-(2*Y-Z)*C*C/12)*(PF-PD)*C/(B*B)
      R16=((PF-PD)*C*Z+WK6-WD6)/B
      R1=R11-R12-R13+R14-R15-R16+R1
      WK=WK1+WK2+WK3+WK4+WK5+WK6
      IF(J.EQ.1) W=W-WK
      IF(J.EQ.3) W=W+WK
      GO TO 24
   19 WK=(Y*Z*Z-(2*Z-Y)*C*C/12)*P2*C/(B*B)
      WD=(Y*Y*Z-(2*Y-Z)*C*C/12)*P2*C/(B*B)
      R1=R1+(P2*C*Z+WK-WD)/B
      IF(J.EQ.1) W=W+WK
      IF(J.EQ.3) W=W-WK
      GO TO 24
   21 IF(K1.NE.4.AND.K1.NE.5) GO TO 24
      R1=R1+(PK+PD)*C/2
      WK=(U+C/3)*PK*0.5*C+(U+2*C/3)*PD*0.5*C
      IF(J.EQ.1) W=W+WK
      IF(J.EQ.3) W=W-WK
   24 CONTINUE
      DO 37 K=1,NWI
      WW=WI(K,1)
      YF=WI(K,3)
      XF=WI(K,2)
      IF(YF.NE.YI) GO TO 37
      IF((J.EQ.1.AND.XF.LE.XJ).OR.(J.EQ.3.AND.XF.LE.XI)) GO TO 37
      IF((J.EQ.1.AND.XF.GE.XI).OR.(J.EQ.3.AND.XF.GE.XJ)) GO TO 37
      IF(K1.EQ.4.OR.K1.EQ.5) W=W-WW
      U=ABS(XI-XF)
      V=B-U
      IF(K1.NE.6) GO TO 30
      RR=(B*B-V*V)*WW*3/(2*B*B*B)
      W=W+(B*B-3*V*V)*WW/(2*B*B)
      R1=R1+RR
      IF(J.EQ.3) R1=R1-2*RR
      R2=R2-RR
      IF(J.EQ.3) R2=R2+2*RR
      GO TO 37
   30 IF(K1.NE.7) GO TO 33
      IF(J.EQ.1) R1=R1+6*WW*U*V/(B*B*B)
      IF(J.EQ.3) R1=R1-6*WW*U*V/(B*B*B)
      W=W+(2*U-V)*WW*V/(B*B)
      GO TO 37
   33 IF(K1.NE.2) GO TO 75
      IF(J.EQ.1) R1=R1+WW/B
      IF(J.EQ.3) R1=R1-WW/B
   37 CONTINUE
   75 RETURN
      END
      SUBROUTINE ASV(K5,MZG,IV,LV,NM,XM,YM,VPP,BST,CST,MS,MG,STM,UM,
     *NVH,MVH,BM,WP,WM,QM)
      DIMENSION MS(NM,4),MG(NM,4),STM(NM,4),UM(NM,4),XM(NM),YM(NM),
     *BM(NM,4),WP(NM,4),WM(NM,4),QM(NM,4),MVH(NVH)
      DO 3 L=1,NM
      DO 3 J=1,4
      WP(L,J)=0.0
    3 CONTINUE
      IF(IV.EQ.0) IV=LV
      K1=1
      K3=3
      IF(K5.EQ.1) GO TO 4
      K1=4
      K3=2
    4 DO 9 I=IV,LV
      IF(K5.EQ.1.AND.XM(I).NE.XM(LV)) GO TO 9
      IF(K5.EQ.2.AND.YM(I).NE.YM(LV)) GO TO 9
      I1=MS(I,K1)
      M1=0
      IF(I1.EQ.3.OR.I1.GT.5) M1=MG(I,K1)
      I3=MS(I,K3)
      M3=0
      IF(I3.EQ.3.OR.I3.GT.5) M3=MG(I,K3)
      IF(M3.EQ.0) GO TO 6
      BS=BM(I,K3)
      IF(BS.EQ.0.) GO TO 6
      WP(I,K3)=0.0
      IF(I3.EQ.6) WP(I,K3)=5*STM(I,K3)*BST*BST/(BS*BS*CST)
      IF(I3.EQ.7) WP(I,K3)=10*STM(I,K3)*BST*BST/(BS*BS*CST)
      WP(M3,K1)= 0.0
      IF(I3.EQ.7) WP(M3,K1)=WP(I,K3)
      IF(I3.EQ.3) WP(M3,K1)=5*STM(M3,K1)*BST*BST/(BS*BS*CST)
    6 IF(M1.EQ.0) GO TO 9
      BS=BM(I,K1)
      IF(BS.EQ.0.) GO TO 9
      WP(I,K1)=0.0
      IF(I1.EQ.6) WP(I,K1)=-5*STM(I,K1)*BST*BST/(BS*BS*CST)
      IF(I1.EQ.7) WP(I,K1)=-10*STM(I,K1)*BST*BST/(BS*BS*CST)
      WP(M1,K3)=0.0
      IF(I1.EQ.7) WP(M1,K3)=WP(I,K1)
      IF(I1.EQ.3) WP(M1,K3)=-5*STM(M1,K3)*BST*BST/(BS*BS*CST)
    9 CONTINUE
      CALL CIK(NM,MG,MS,UM,WP,NVH,MVH)
      Q=0.0
      DO 15 I=IV,LV
      IF(K5.EQ.2.AND.YM(I).NE.YM(LV)) GO TO 15
      IF(K5.EQ.1.AND.XM(I).NE.XM(LV)) GO TO 15
      M3=MG(I,K3)
      M1=MG(I,K1)
      Q1=0.0
      IF(M3.NE.0) Q1=WP(M3,K1)
      Q2=0.0
      IF(M1.NE.0) Q2=WP(M1,K3)
      IF(BM(I,K3).NE.0.) Q=(WP(I,K3)+Q1)/BM(I,K3)+Q
      IF(BM(I,K1).NE.0.) Q=Q-(WP(I,K1)+Q2)/BM(I,K1)
   15 CONTINUE
      MZG=0
      IF(Q.NE.0) GO TO 16
      MZG=IV
      GO TO 17
   16 CO=VPP/Q
      NN=1
      CALL SKER(NN,NM,CO,MG,MS,BM,WP,WM,QM)
   17 RETURN
      END
      SUBROUTINE CIK(NM,MG,MS,UM,WM,NVH,MVH)
      DIMENSION MG(NM,4),MS(NM,4),UM(NM,4),WM(NM,4),MVH(NVH)
      DO 9 II=1,30
      DO 6 I=1,NM
      DO 2 L=1,NVH
      IF(MVH(L).EQ.I) GO TO 6
    2 CONTINUE
      SW=WM(I,1)+WM(I,2)+WM(I,3)+WM(I,4)
      IF(ABS(SW).LT.0.00005) GO TO 6
      DO 3 J=1,4
      W1=0.0
      I1=MS(I,J)
      IF(I1.LT.6) GO TO 3
      M=MG(I,J)
      JM=J+2
      IF(JM.GT.4) JM=JM-4
      W1=UM(I,J)*SW
      WM(I,J)=WM(I,J)-W1
      IF(I1.EQ.7) WM(M,JM)=WM(M,JM)-W1/2
    3 CONTINUE
    6 CONTINUE
    9 CONTINUE
   10 RETURN
      END
      SUBROUTINE SKER(NN,NM,CO,MG,MS,BM,WP,WM,QM)
      DIMENSION MG(NM,4),MS(NM,4),BM(NM,4),WP(NM,4),WM(NM,4),QM(NM,4)
      DO 12 I=1,NM
      DO 9 J=1,4
      W1=WP(I,J)*CO
      IF(NN.NE.0) WM(I,J)=WM(I,J)+W1
      IT=MS(I,J)
      IF(IT.LT.6) GO TO 9
      M=MG(I,J)
      J2=J+2
      IF(J2.GT.4) J2=J2-4
      A1=0.0
      IF(BM(I,J).NE.0.) A1=W1/BM(I,J)
      GO TO(3,6,6,3),J
    3 QM(I,J)=QM(I,J)+A1
      IF(M.NE.0) QM(M,J2)=QM(M,J2)-A1
      GO TO 9
    6 QM(I,J)=QM(I,J)-A1
      IF(M.NE.0) QM(M,J2)=QM(M,J2)+A1
    9 CONTINUE
   12 CONTINUE
      RETURN
      END
      SUBROUTINE ASI(NM,M,IP,IG,AT,XM,YM,NFH,FH,MG,QM,UM)
      DIMENSION XM(NM),YM(NM),FH(NFH,3),MG(NM,4),QM(NM,4),UM(NM,4)
      DO 12 I=1,NM
      IF(YM(I).NE.YM(M)) GO TO 12
      IF(I.LT.IP.OR.I.GT.IG) GO TO 12
      M1=MG(I,1)
      M3=MG(I,3)
      M2=MG(I,2)
      M4=MG(I,4)
      IF(M3.EQ.0.AND.M1.EQ.0) GO TO 12
      UM(I,3)=UM(I,1)-QM(I,2)-QM(I,4)
      IF(M1.NE.0.AND.M3.EQ.M1) UM(I,3)=0-UM(I,3)
      IF(I.EQ.M) UM(I,3)=UM(I,3)+AT
      F=0.0
      DO 3 J=1,NFH
      IF(FH(J,3).NE.YM(I)) GO TO 3
      IF(FH(J,2).EQ.XM(I)) UM(I,3)=UM(I,3)-FH(J,1)
      IF(M1.EQ.0) GO TO 6
      IF(M3.NE.0.AND.FH(J,2).GT.XM(I).AND.FH(J,2).LT.XM(M3))
     *F=F+FH(J,1)
    3 CONTINUE
    6 IF(M2.NE.0.AND.M4.EQ.M2) UM(I,3)=UM(I,3)-QM(I+1,2)-QM(I+1,4)
      IF(M1.EQ.M3.OR.M3.LT.I) GO TO 12
      IF(I.EQ.IG) UM(I,3)=0.0
      IF(M1.LT.I) GO TO 9
      UM(M1,3)=UM(M1,1)-QM(M1,4)
      UM(I,1)=UM(M1,3)
      UM(I,3)=UM(I,1)-QM(I,2)
    9 UM(M3,1)=UM(I,3)-F
   12 CONTINUE
      RETURN
      END
      SUBROUTINE ASHN(NM,M,IP,IG,AT,XM,YM,NFV,FV,MG,QM,AM)
      DIMENSION XM(NM),YM(NM),FV(NFV,3),MG(NM,4),QM(NM,4),AM(NM,4)
      DO 15 I=1,NM
      X=0.0
      IF(M.NE.0) X=XM(M)
      IF(XM(I).NE.X) GO TO 15
      M1=MG(I,1)
      M2=MG(I,2)
      M3=MG(I,3)
      M4=MG(I,4)
      IF(M2.EQ.0.AND.M4.EQ.0) GO TO 15
      IF(I.LT.IP.OR.I.GT.IG) GO TO 15
      IF(M2.NE.0.AND.M4.EQ.M2) GO TO 15
      AM(I,4)=AM(I,2)-QM(I,1)-QM(I,3)
      IF(M1.NE.0.AND.M3.EQ.M1) GO TO 12
      IF(M1.NE.0.AND.M1.EQ.M4.AND.M2.GT.I) GO TO 15
      IF(M3.NE.0.AND.M2.EQ.M3.AND.M2.GT.I)
     *AM(I,4)=AM(I+1,2)-QM(I+1,3)-QM(I,1)
      IF(M.EQ.I) AM(I,4)=AM(I,4)+AT
      GO TO 13
   12 AM(I,4)=AM(I,2)-QM(M1,1)-QM(M1,3)
      AM(M1,4)=0-QM(M1,1)-QM(M1,3)
   13 F=0.0
      DO 6 L=1,NFV
      IF(FV(L,2).NE.XM(I)) GO TO 6
      IF(FV(L,3).EQ.YM(I)) AM(I,4)=AM(I,4)-FV(L,1)
      IF(M4.NE.0.AND.FV(L,3).LT.YM(I).AND.FV(L,3).GT.YM(M4)) F=F+FV(L,1)
    6 CONTINUE
      IF(M4.NE.0) AM(M4,2)=AM(I,4)-F
      IF(I.EQ.IG) AM(I,4)=0.0
   15 CONTINUE
      RETURN
      END
      SUBROUTINE REZ(Y0,NM,MS,WM,UM,QM)
      DIMENSION MS(NM,4),WM(NM,4),UM(NM,4),QM(NM,4)
      WRITE(6,3)
    3 FORMAT(/1H ,10X,'Strypu irazos:'/1H ,5X,'(Teigiamos reiksmes: ',
     *'L.momentai - kai strypas pjuvio atzvilgiu lenkiamas'/1H ,5X,
     *'priesinga l.r.judejimui kryptimi; Sk. jegos - kai pjuvio atzv',
     *'ilgiu'/1H ,5X,'suka l.r.judejimo kryptimi; Asines jegos - kai',
     *' strypa tempia).')
      WRITE(6,6)
    6 FORMAT(1H ,8X,68('-')/1H ,8X,'| Mazgo|',8X,'Kairiau mazgo',8X,
     *'|',7X,'Desiniau mazgo',8X,'|'/1H ,8X,'|  Nr  |',2(29('-'),'|')
     */1H ,8X,'|',6X,'|',2(4X,'M',4X,':',4X,'N',4X,':',4X,'Q',4X,'|')
     */1H ,8X,'|------|',2(2(9('-'),':'),9('-'),'|'))
      DO 12 I=1,NM
      IF(MS(I,1).NE.0) QM(I,1)=0-QM(I,1)
      WRITE(6,9) I,WM(I,1),UM(I,1),QM(I,1),WM(I,3),UM(I,3),QM(I,3)
    9 FORMAT(1H ,8X,'|',I4,'  |',2(2(F8.3,' :'),F8.3,' |'))
   12 CONTINUE
      IF(Y0.EQ.0.) GO TO 36
      WRITE(6,15)
   15 FORMAT(/1H ,8X,68('-')/1H ,8X,'| Mazgo|',7X,'Auksciau mazgo',8X,
     *'|',8X,'Zemiau mazgo',9X,'|'/1H ,8X,'|  Nr  |',2(29('-'),
     *'|')/1H ,8X,'|',6X,'|',2(4X,'M',4X,':',4X,'N',4X,':',4X,'Q',4X,
     *'|')/1H ,8X,'|------|',2(2(9('-'),':'),9('-'),'|'))
      DO 21 I=1,NM
      IF(MS(I,4).NE.0) QM(I,4)=0-QM(I,4)
      WRITE(6,18) I,WM(I,2),UM(I,2),QM(I,2),WM(I,4),UM(I,4),QM(I,4)
   18 FORMAT(1H ,8X,'|',I4,'  |',2(2(F8.3,' :'),F8.3,' |'))
   21 CONTINUE
   36 RETURN
      END
      SUBROUTINE TARP(Y0,NM,NC,NCM,XM,YM,MG,MS,NFV,FV,HH,NFH,FH,VH,
     *NWI,WI,WH,NPV,PV,NPH,PH,WM,QM,UM,CPM,XX)
      COMMON /C2/WK,WD,QK,AK,YI,XK,XD,CX,WCK,WCD,ACK,ACD,QCK,QCD
      DIMENSION XM(150),YM(150),FV(NFV,3),FH(NFH,3),WI(NWI,3),
     *PV(NPV,5),PH(NPH,5),WM(NM,4),MG(NM,4),MS(NM,4),QM(NM,4),UM(NM,4),
     *CPM(50,8),HH(NFV,3),VH(NFH,3),WH(NWI,3),XX(50)
      NCM=NM+NC
      DO 22 I=1,NFV
      X=FV(I,2)
      Y=FV(I,3)
      CALL PJUV(NCM,X,Y,XM,YM)
   22 CONTINUE
      DO 25 I=1,NFH
      X=FH(I,2)
      Y=FH(I,3)
      CALL PJUV(NCM,X,Y,XM,YM)
   25 CONTINUE
      DO 29 I=1,NWI
      X=WI(I,2)
      Y=WI(I,3)
      CALL PJUV(NCM,X,Y,XM,YM)
   29 CONTINUE
      DO 32 I=1,NPV
      X=PV(I,3)
      Y=PV(I,1)
      CALL PJUV(NCM,X,Y,XM,YM)
      X=PV(I,5)
      Y=PV(I,1)
      CALL PJUV(NCM,X,Y,XM,YM)
   32 CONTINUE
      DO 35 I=1,NPH
      X=PH(I,1)
      Y=PH(I,3)
      CALL PJUV(NCM,X,Y,XM,YM)
      X=PH(I,1)
      Y=PH(I,5)
      CALL PJUV(NCM,X,Y,XM,YM)
   35 CONTINUE
      IF(NCM.NE.NM) WRITE(6,213)
  213 FORMAT(1H ,1X,75('-')/1H ,1X,
     *'|   Pjuvio    |   Kairiau ar zemiau pjuvio  | Desiniau ar auk',
     *'sciau pjuvio |'/1H ,1X,'|',13('-'),'|',2(29('-'),'|')/1H ,1X,
     *'|  X   :  Y   |',2(4X,'M',4X,':',4X,'N',4X,':',4X,'Q',4X,
     *'|')/1H ,1X,'|------:------|',2(2(9('-'),':'),9('-'),'|'))
  210 KL=0
      J1=NM+1
      IF(KT.EQ.1) J1=NM+NC+1
      DO 225 J=J1,NCM
      XC=XM(J)
      YC=YM(J)
      KL=KL+1
      DO 222 I=1,NM
      M=MG(I,1)
      IF(YM(I).NE.YC.OR.M.EQ.0) GO TO 216
      XK=XM(M)
      XD=XM(I)
      IF(XC.GE.XD.OR.XC.LE.XK) GO TO 222
      YI=YM(I)
      WK=WM(M,3)
      WD=WM(I,1)
      QK=QM(M,3)
      AK=UM(M,3)
      CX=XC
      CALL CHPJ(XC,YC,Y0,FV,FH,WI,PV,NFV,NFH,NWI,NPV)
      GO TO 219
  216 M=MG(I,4)
      IF(XM(I).NE.XC.OR.M.EQ.0.OR.Y0.EQ.0.) GO TO 222
      XK=YM(M)
      XD=YM(I)
      IF(YC.LE.XK.OR.YC.GE.XD) GO TO 222
      YI=XM(I)
      WK=WM(M,2)
      WD=WM(I,4)
      QK=QM(M,2)
      AK=UM(M,2)
      CX=YC
      CALL CHPJ(XC,YC,Y0,HH,VH,WH,PH,NFH,NFV,NWI,NPH)
  219 CPM(KL,1)=XC
      CPM(KL,2)=YC
      CPM(KL,3)=WCK
      CPM(KL,4)=ACK
      CPM(KL,5)=QCK
      CPM(KL,6)=WCD
      CPM(KL,7)=ACD
      CPM(KL,8)=QCD
      GO TO 225
  222 CONTINUE
  225 CONTINUE
      NC=NCM-NM
      KT=KT+1
      IF(KT.EQ.1) THEN
      WRITE(6,*) ' '
      WRITE(6,212)
  212 FORMAT(30X,'Lenkimo momentu ekstremumai')
      GO TO 2
      ENDIF
      GO TO 230
    2 DO 3 I=1,50
      XX(I)=0.0
    3 CONTINUE
      DO 24 K=1,NPV
      NX=0
      YC=PV(K,1)
      X1=PV(K,3)
      X2=PV(K,5)
      P1=PV(K,2)
      P2=PV(K,4)
      P=P1
      IF(P1.EQ.0.) P=P2
      DO 6 I=1,NM
      IF(YM(I).NE.YC) GO TO 6
      IF(XM(I).LT.X1.OR.XM(I).GT.X2) GO TO 6
      NX=NX+1
      XX(NX)=XM(I)
    6 CONTINUE
      DO 9 I=1,NC
      IF(CPM(I,2).NE.YC) GO TO 9
      IF(CPM(I,1).LT.X1.OR.CPM(I,1).GT.X2) GO TO 9
      NX=NX+1
      XX(NX)=CPM(I,1)
    9 CONTINUE
      DO 12 I=1,NX
      XN=XX(I)
      JP=I+1
      DO 12 J=JP,NX
      X=XX(J)
      IF(X.GE.XN) GO TO 12
      XX(I)=X
      XX(J)=XN
      XN=X
   12 CONTINUE
      DO 21 J=2,NX
      XK=XX(J-1)
      XD=XX(J)
      IF(XD.EQ.XK) GO TO 21
      I1=0
      I2=0
      N1=0
      N2=0
      QK=0.0
      QD=0.0
      DO 18 I=1,NM
      IF(YM(I).NE.YC.OR.XM(I).LT.XK.OR.XM(I).GT.XD) GO TO 18
      IF(I.GT.1.AND.XM(I).NE.0.AND.XM(I).EQ.XM(I-1)) GO TO 18
      IF(I.LT.NM.AND.XM(I).EQ.XM(I+1)) GO TO 16
      IF(XM(I).EQ.XK) I1=I
      IF(XM(I).EQ.XD) I2=I
      GO TO 18
   16 IF(XM(I).EQ.XK) I1=I
      IF(XM(I).EQ.XK.AND.MS(I,3).LT.4) I1=I+1
      IF(XM(I).EQ.XD) I2=I
      IF(XM(I).EQ.XD.AND.MS(I,1).LT.4) I2=I+1
   18 CONTINUE
      DO 19 N=1,NC
      IF(CPM(N,2).NE.YC) GO TO 19
      IF(CPM(N,1).EQ.XK) N1=N
      IF(CPM(N,1).EQ.XD) N2=N
   19 CONTINUE
      IF(I1.NE.0) QK=QM(I1,3)
      IF(I2.NE.0) QD=QM(I2,1)
      IF(N1.NE.0.AND.QK.EQ.0.) QK=CPM(N1,8)
      IF(N2.NE.0.AND.QD.EQ.0.) QD=CPM(N2,5)
      SD=QK*QD
      IF(SD.EQ.-0.) SD=0
      IF(SD.GE.0.) GO TO 21
      IF(I1.NE.0) XC=XK+ABS(QM(I1,3)/P)
      IF(N1.NE.0) XC=XK+ABS(CPM(N1,8)/P)
      IF(P2.EQ.P1) GO TO 20
      PK=P1
      IF(X1.LT.XK) PK=P1+(P2-P1)*(XK-X1)/(X2-X1)
      PD=P2
      IF(X2.GT.XD) PD=P2-(P2-P1)*(X2-XD)/(X2-X1)
      D=(XD-XK)/(PD-PK)
      E=0-PK*D
      X=SQRT(E*E+QK*D*2)
      XC=XK+X+E
      IF(E.GT.X) XC=E-X+XK
   20 NCM=NCM+1
      XM(NCM)=XC
      YM(NCM)=YC
   21 CONTINUE
   24 CONTINUE
      DO 48 K=1,NPH
      NX=0
      XC=PH(K,1)
      Y1=PH(K,3)
      Y2=PH(K,5)
      P1=PH(K,2)
      P2=PH(K,4)
      P=P1
      IF(P1.EQ.0.) P=P2
      DO 27 I=1,NM
      IF(XM(I).NE.XC) GO TO 27
      IF(YM(I).LT.Y1.OR.YM(I).GT.Y2) GO TO 27
      NX=NX+1
      XX(NX)=YM(I)
   27 CONTINUE
      DO 30 I=1,NC
      IF(CPM(I,1).NE.XC) GO TO 30
      IF(CPM(I,2).LT.Y1.OR.CPM(I,2).GT.Y2) GO TO 30
      NX=NX+1
      XX(NX)=CPM(I,2)
   30 CONTINUE
      DO 33 I=1,NX
      XN=XX(I)
      JP=I+1
      DO 33 J=JP,NX
      X=XX(J)
      IF(X.GE.XN) GO TO 33
      XX(I)=X
      XX(J)=XN
      XN=X
   33 CONTINUE
      DO 45 M=1,NX-1
      J=NX+1-M
      YK=XX(J-1)
      YD=XX(J)
      IF(YD.EQ.YK) GO TO 45
      I1=0
      I2=0
      N1=0
      N2=0
      QK=0.0
      QD=0.0
      DO 42 I=1,NM
      IF(XM(I).NE.XC.OR.YM(I).LT.YK.OR.YM(I).GT.YD) GO TO 42
      X=0.0
      IF(I.GT.1) X=XM(I-1)
      Y=0.0
      IF(I.GT.1) Y=YM(I-1)
      IF(XM(I).EQ.X.AND.YM(I).EQ.Y) GO TO 42
      X=0.0
      IF(I.LT.NM) X=XM(I+1)
      Y=0.0
      IF(I.LT.NM) Y=YM(I+1)
      IF(XM(I).EQ.X.AND.YM(I).EQ.Y) GO TO 40
      IF(YM(I).EQ.YK) I1=I
      IF(YM(I).EQ.YD) I2=I
      GO TO 42
   40 IF(YM(I).EQ.YK) I1=I
      IF(YM(I).EQ.YK.AND.MS(I,1).GT.3.AND.MS(I,3).GT.3) I1=I+1
      IF(YM(I).EQ.YD) I2=I+1
      IF(YM(I).EQ.YD.AND.MS(I,4).GT.3) I2=I
   42 CONTINUE
      DO 43 N=1,NC
      IF(CPM(N,1).NE.XC) GO TO 43
      IF(CPM(N,2).EQ.YK) N1=N
      IF(CPM(N,2).EQ.YD) N2=N
   43 CONTINUE
      IF(I1.NE.0) QK=QM(I1,2)
      IF(I2.NE.0) QD=QM(I2,4)
      IF(N1.NE.0.AND.QK.EQ.0.) QK=CPM(N1,8)
      IF(N2.NE.0.AND.QD.EQ.0.) QD=CPM(N2,5)
      SD=QK*QD
      IF(SD.EQ.-0.) SD=0
      IF(SD.GE.0.) GO TO 45
      IF(I1.NE.0) YC=YK+ABS(QM(I1,2)/P)
      IF(N1.NE.0) YC=YK+ABS(CPM(N1,8)/P)
      IF(P2.EQ.P1) GO TO 55
      PK=P1
      IF(Y1.LT.YK) PK=P1+(P2-P1)*(YK-Y1)/(Y2-Y1)
      PD=P2
      IF(Y2.GT.YD) PD=P2-(P2-P1)*(Y2-YD)/(Y2-Y1)
      D=(YD-YK)/(PD-PK)
      E=0-PK*D
      Y=SQRT(E*E+QK*D*2)
      YC=YK+Y+E
      IF(E.GT.Y) YC=E-Y+YK
   55 NCM=NCM+1
      XM(NCM)=XC
      YM(NCM)=YC
   45 CONTINUE
   48 CONTINUE
      GO TO 210
  230 RETURN
      END
      SUBROUTINE PJUV(NCM,X,Y,XM,YM)
      DIMENSION XM(150),YM(150)
      DO 9 L=1,NCM
      IF(XM(L).EQ.X.AND.YM(L).EQ.Y) GO TO 12
    9 CONTINUE
      NCM=NCM+1
      XM(NCM)=X
      YM(NCM)=Y
   12 RETURN
      END
      SUBROUTINE CHPJ(XC,YC,Y0,FV,FH,WI,PV,NFV,NFH,NWI,NPV)
      COMMON /C2/WK,WD,QK,AK,YI,XK,XD,CX,WCK,WCD,ACK,ACD,QCK,QCD
      DIMENSION FV(NFV,3),FH(NFH,3),WI(NWI,3),PV(NPV,5)
	B1=XD-XK
      UC=CX-XK
      VC=B1-UC
      QCK=QK
      ACK=AK
      WCK=(UC*WD-VC*WK)/B1
      R=0.0
      W=0.0
      Q2=0.0
      W2=0.0
      P=0.0
      DO 3 J=1,NPV
      YP=PV(J,1)
      IF(PV(J,1).NE.YI) GO TO 3
      X1=PV(J,3)
      X2=PV(J,5)
      IF(X1.GE.XD.OR.X2.LE.XK) GO TO 3
      V=XD-X2
      IF(V.LT.0.) V=0.0
      U=X1-XK
      IF(U.LT.0.) U=0.0
      P1=PV(J,2)
      P2=PV(J,4)
      PK=P1
      PD=P2
      IF(U.EQ.0.) PK=P1+(P2-P1)*(XK-X1)/(X2-X1)
      IF(V.EQ.0.) PD=P2-(P2-P1)*(X2-XD)/(X2-X1)
      SP=B1-U-V
      R=R+((V+SP*2/3)*PK+(V+SP/3)*PD)*SP*0.5
      IF(U.GE.UC) GO TO 3
      WW=((V-VC+SP*2/3)*PK+(V-VC+SP/3)*PD)*SP*0.5
      PP=(PK+PD)*SP/2
      IF(VC.LT.V) GO TO 2
      PC=PK+(PD-PK)*(UC-U)/SP
      WW=(UC-U)*(UC-U)*PK/3+(UC-U)*(UC-U)*PC/6
      PP=(PC+PK)*(UC-U)/2
    2 W=W+WW
      P=P+PP
    3 CONTINUE
      QCK=QCK-P
      DO 9 J=1,NFV
      IF(FV(J,3).NE.YI) GO TO 9
      XF=FV(J,2)
      IF(XF.LE.XK.OR.XF.GE.XD) GO TO 9
      R=R+FV(J,1)*(XD-XF)
      IF(XF.EQ.CX) Q2=FV(J,1)
      IF(XF.GE.CX) GO TO 9
      QCK=QCK-FV(J,1)
      W=W+(CX-XF)*FV(J,1)
    9 CONTINUE
      DO 15 J=1,NWI
      IF(WI(J,3).NE.YI) GO TO 15
      XW=WI(J,2)
      IF(XW.LE.XK.OR.XW.GE.XD) GO TO 15
      R=R-WI(J,1)
      IF(XW.EQ.CX) W2=WI(J,1)
      IF(XW.GE.CX) GO TO 15
      W=W-WI(J,1)
   15 CONTINUE
      WCK=WCK-R*UC/B1+W
      WCD=W2-WCK
      QCD=QCK-Q2
      IF(Y0.EQ.0.) GO TO 25
      A=0.0
      A2=0.0
      DO 21 J=1,NFH
      IF(FH(J,3).NE.YI) GO TO 21
      XF=FH(J,2)
      IF(XF.LE.XK.OR.XF.GE.XD) GO TO 21
      IF(XF.EQ.CX) A2=FH(J,1)
      IF(XF.GE.CX) GO TO 21
      A=A-FH(J,1)
   21 CONTINUE
      ACK=ACK+A
      ACD=ACK-A2
   25 WRITE(6,27) XC,YC,WCK,ACK,QCK,WCD,ACD,QCD
   27 FORMAT(1H ,1X,'|',F6.3,':',F6.3,'|',2(2(F8.3,' :'),F8.3,' |'))
   50 RETURN
      END
