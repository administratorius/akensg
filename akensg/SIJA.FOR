      IMPLICIT INTEGER (D,S)
      COMMON /C1/ NEA,MINA
      COMMON /C2/ SIG2,XO,A,D1,SA,AM,NV1,NV2,
     *LP,LH,SP,SH,AT,KSI,ATL,Z,QV
      COMMON /P1/ DV2,SK1,SK2,EV,EA,X,HO,TD,RBS,AM1,AM2,
     *QTN,ANG,BTM,RBG,ME,KIET
      COMMON /C3/ DV1,NEV,A1,QT,J,RB
      DIMENSION D(17),ATM(17,10),ASK(10,4),AKL(10),DVR(14),
     *MODA(4),RBM(9),RBTM(9),BMOD(9),BKL(9),VS(17),VAM(14,4),
     *RBSM(9),RBGM(9),MDA(10)
      OPEN(UNIT=5,FILE='SIJA.DAT')
      OPEN(UNIT=6,FILE='SIJA.RES')
      DATA ATM /2.65,4.59,7.07,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
     *0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,6.36,11.31,17.67,0.0,
     *0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
     *21.99,31.67,43.10,56.3,71.25,87.96,106.44,137.44,172.41,
     *225.19,285.01,351.86,0.0,0.0,0.0,10.04,17.84,28.67,41.28,
     *56.19,73.39,92.88,114.67,138.75,179.17,224.75,293.55,371.52,
     *458.67,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,141.37,171.06,
     *220.89,277.09,361.91,458.04,565.49,0.0,0.0,0.0,0.0,0.0,0.0,
     *0.0,0.0,0.0,0.0,153.94,186.27,240.53,301.72,394.08,498.76,
     *615.75,8.59,14.39,20.52,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
     *0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,40.06,57.68,
     *78.51,102.54,129.78,160.22,193.87,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
     *0.0,0.0,0.0,53.41,76.91,104.68,136.72,173.04,213.63,258.49,
     *333.79,418.71,546.89,0.0,0.0,0.0,0.0,0.0,0.0,0.0,64.01,92.17,
     *125.46,163.87,207.39,256.04,309.81,0.0,0.0,0.0,0.0,0.0/
      DATA ASK /1.909,3.33,5.105,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
     *0.0,0.0,4.948,8.796,13.744,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
     *0.0,17.671,25.447,34.636,45.239,57.256,0.0,0.0,0.0,7.21,
     *12.818,20.028,28.84,39.254,51.271,64.89/
      DATA MDA /170,210,210,200,180,180,200,190,190,190/
      DATA MODA /170,210,200,200/
      DATA VS /0.055,0.099,0.154,0.222,0.395,0.617,0.888,1.208,
     *1.578,1.998,2.466,2.984,3.84,4.83,6.31,7.99,9.865/
      DATA RBTM /0.48,0.57,0.66,0.75,0.9,1.05,1.2,1.3,1.4/
      DATA RBM /4.5,6.0,7.5,8.50,11.50,14.50,17.0,19.5,22.0/
      DATA RBSM /0.7,0.85,1.0,1.15,1.4,1.6,1.8,1.95,2.1/
      DATA RBGM /5.5,7.5,9.5,11.0,15.0,18.5,22.0,25.5,29.0/
      DATA BMOD /16.0,18.0,21.0,23.0,27.0,30.0,32.5,34.5,36.0/
      DATA D /3,4,5,6,8,10,12,14,16,18,20,22,25,28,32,36,40/
      READ(5,*) ANG,SH,SP,LH,LP,LA,DH,DS
      READ(5,*) Q,QILG
      READ(5,*) BK1,BK2,KIET,GAM
      READ(5,*) SA1,SA2,DA1,DA2,DVG,DSG
      READ(5,*) KSI,LIL
      READ(5,5) BKL
    5 FORMAT(14(A4,1X))
      READ(5,6) AKL
    6 FORMAT(/14(A4,1X))
      IP=BK1/2.5-2
      IF(BK1.GT.15.) IP=BK1/5+1
      IG=BK2/2.5-2
      IF(BK2.GT.15.) IG=BK2/5+1
      IF(LH.EQ.0) WRITE(6,17)
   17 FORMAT(/1H ,12X,'GELZBETONINES STACIAKAMPIO SKERSPJUVIO ',
     *'SIJOS SKAICIAVIMAS'/1H ,25X,'(Programa "SIJA")')
      IF(LH.NE.0) WRITE(6,18)
   18 FORMAT(/1H ,12X,'GELZBETONINES TEJINIO SKERSPJUVIO SIJOS ',
     *'SKAICIAVIMAS'/1H ,25X,'(Programa "SIJA")')
      IF(LH.EQ.0) LP=SP
      WRITE(6,20) ANG,LA
   20 FORMAT(/1H ,35X,'UZDUODAMA:'/1H ,5X,'Sijos nominali',
     *'nis ilgis ',F5.2,' m, atremimo ilgis Latr=',I3,' cm')
      WRITE(6,11) Q,QILG
   11 FORMAT(1H ,5X,'Naudinga skaiciuojamoji apkrova ',F6.2,
     *' kN/m'/1H ,10X,'Ilgalaike apkrova =',F4.2,' naudingos ',
     *'skaiciuojamosios')
      IF(LH.NE.0) WRITE(6,13) SH,SP,LH,LP
   13 FORMAT(1H ,5X,'Pradinis skerspjuvis (cm): H=',I3,'  B=',
     *I3,'  HL=',I2,'  BL=',I3)
      IF(LH.EQ.0) WRITE(6,19) SH,SP
   19 FORMAT(1H ,5X,'Pradinis skerspjuvis (cm): H=',I3,'  B=',I3)
      IF(DH.EQ.0) GO TO 24
      IF(DS.EQ.0) GO TO 22
      WRITE(6,21)
   21 FORMAT(1H ,5X,'Ar leidziama didinti skerspjuvi?',
     *' Ir "H" ir "B" -taip')
      GO TO 28
   22 WRITE(6,23)
   23 FORMAT(1H ,5X,'Ar leidziama didinti skerspjuvi? "H" -taip',
     *'  "B" -ne')
      GO TO 28
   24 IF(DS.EQ.0) GO TO 26
      WRITE(6,25)
   25 FORMAT(1H ,5X,'Ar leidziama didinti skerspjuvi? "H" -ne',
     *'  "B" -taip')
      GO TO 28
   26 WRITE(6,27)
   27 FORMAT(1H ,5X,'Ar leidziama didinti skerspjuvi? Ir "H"',
     *' ir "B" -ne')
   28 IF(KSI.EQ.0) WRITE(6,29)
   29 FORMAT(1H ,5X,'Virsutine armatura tik konstruktyviai')
      IF(KSI.EQ.1) WRITE(6,30)
   30 FORMAT(1H ,5X,'Virsutine armatura - pagal skaiciavima (dvigu',
     *'bas armavimas).')
      IF(KIET.EQ.0) WRITE(6,31) BKL(IP),BKL(IG)
   31 FORMAT(1H ,5X,'Betonas nuo B',A4,'iki B',A4,'klases ',
     *'imtinai, kietinamas naturaliomis salygomis')
      IF(KIET.EQ.1) WRITE(6,32) BKL(IP),BKL(IG)
   32 FORMAT(1H ,5X,'Betonas nuo B',A4,'iki B',A4,'klases ',
     *'imtinai, kietinamas sutinant')
      WRITE(6,33) GAM,AKL(SA1),AKL(SA2),DA1,DA2
   33 FORMAT(1H ,10X,'Darbo salygu koeficientas g=',F4.2/1H ,5X,
     *'Apatine armatura - nuo ',A4,' iki ',A4,' klases nuo D=',I2,
     *' iki D=',I2,' mm,')
      IF(DVG.EQ.0.AND.DSG.EQ.0) WRITE(6,36)
   36 FORMAT(1H ,5X,'Virsutines ir skersines armaturos diametras',
     *' neribojamas')
      IF(DVG.NE.0.AND.DSG.EQ.0) WRITE(6,37) DVG
   37 FORMAT(1H ,5X,'Virsutines armaturos Dmax=',I2,' mm, skersi',
     *'nes - neribojamas')
      IF(DVG.EQ.0.AND.DSG.NE.0) WRITE(6,38) DSG
   38 FORMAT(1H ,5X,'Virsutines armaturos diametras neribojamas,',
     *' skersines Dmax=',I2,' mm')
      IF(DVG.NE.0.AND.DSG.NE.0) WRITE(6,40) DVG,DSG
   40 FORMAT(1H ,5X,'Virsutines armaturos Dmax=',I2,' ir sker',
     *'sines Dmax=',I2,' mm')
      WRITE(6,41)
   41 FORMAT(/1H ,28X,'SKAICIAVIMO REZULTATAI:')
      ANG=ANG-0.04*LA/3
      NA1=DA1-2
      IF(DA1.GT.6) NA1=(DA1+2)/2
      IF(DA1.GT.25) NA1=DA1/5+9
      NA2=DA2-2
      IF(DA2.GT.6) NA2=(DA2+2)/2
      IF(DA2.GT.25) NA2=DA2/5+9
      SIG2=400
      IF(GAM.LT.1.) SIG2=500
      QIN=QILG*Q
      QTN=(0.9-QILG)*Q
   39 MINJ=1
      IF(SP.GT.12) MINJ=2
      IF(SP.GT.35) MINJ=3
      IPR=IP
      MINA=20
      IF(SH.LT.11) MINA=10
      IF(SH.GT.10.AND.SH.LT.25) MINA=15
      IF(LH.EQ.0) LP=SP
      QT=((LP-SP)*LH+SP*SH)*0.0027+Q
      AM=QT*ANG*ANG/8
      QV=QT*ANG/2
      QN2=QIN+((LP-SP)*LH+SP*SH)*0.00245
      AM1=(QTN+QN2)*ANG*ANG/8
      AM2=ANG*ANG*QN2/8
      MAXEA=(SH-20)/20+1
      IF(MAXEA.GT.5) MAXEA=5
   45 NEA=1
      NRR=0
      DO 51 I=IPR,IG
      CALL BX(GAM,KIET,I,RBM,RB,RBSM,RBS,RBGM,RBG,BMOD,BTM,XO)
      J=MINJ
   43 DO 49 NAV=NA1,NA2
      DO 48 SA=SA1,SA2
      AT=ATM(NAV,SA)
      IF(AT.EQ.0.) GO TO 48
      D1=D(NAV)
      X=D1*D1
      Y=0.5*D1
      CALL STIP(NAV,D1,SP,SH,A,DVG,MAXJ,NV1,NV2)
      RBT=RBTM(I)*GAM
      HO=SH-A-Y*0.1
      Z=0.06*RBT*SP*HO
      ATL=AT*J
      CALL REIK(J,MINA,RB,AA,AV,NRR,HO,SK1,SV,N1,N2,TD,A1,NEV,ATM,D,
     *DV1,DV2,SK2)
      IF(AA.GE.AM) GO TO 81
   48 CONTINUE
   49 CONTINUE
      J=J+1
      IF(J.LE.MAXJ) GO TO 43
   51 CONTINUE
      NEA=2
      IF(NEA.GT.MAXEA) GO TO 89
      DO 58 I=IPR,IG
      CALL BX(GAM,KIET,I,RBM,RB,RBSM,RBS,RBGM,RBG,BMOD,BTM,XO)
      J=MINJ
   52 DO 56 NAV=NA1,NA2
      DO 55 SA=SA1,SA2
      AT=ATM(NAV,SA)
      IF(AT.EQ.0.) GO TO 55
      D1=D(NAV)
      CALL STIP(NAV,D1,SP,SH,A,DVG,MAXJ,NV1,NV2)
      DO 54 NAD=NA1,NAV
      IF(ATM(NAD,SA).EQ.0.) GO TO 54
      D2=D(NAD)
      X=D1*D1+D2*D2
      Y=(D1*D2*D2+(D1**3+D2**3)*0.5)/X
      RBT=RBTM(I)*GAM
      HO=SH-A-Y*0.1
      Z=0.06*RBT*SP*HO
      ATL=(AT+ATM(NAD,SA))*J
      CALL REIK(J,MINA,RB,AA,AV,NRR,HO,SK1,SV,N1,N2,TD,A1,NEV,ATM,D,
     *DV1,DV2,SK2)
      IF(AA.GE.AM) GO TO 81
   54 CONTINUE
   55 CONTINUE
   56 CONTINUE
      J=J+1
      IF(J.LE.MAXJ) GO TO 52
   58 CONTINUE
      NEA=3
      IF(NEA.GT.MAXEA) GO TO 89
      DO 68 I=IPR,IG
      CALL BX(GAM,KIET,I,RBM,RB,RBSM,RBS,RBGM,RBG,BMOD,BTM,XO)
      J=MINJ
   59 DO 66 NAV=NA1,NA2
      DO 65 SA=SA1,SA2
      AT=ATM(NAV,SA)
      IF(AT.EQ.0.) GO TO 65
      D1=D(NAV)
      CALL STIP(NAV,D1,SP,SH,A,DVG,MAXJ,NV1,NV2)
      DO 64 NAD=NA1,NAV
      IF(ATM(NAD,SA).EQ.0.) GO TO 64
      D2=D(NAD)
      DO 63 NAT=NA1,NAD
      IF(ATM(NAT,SA).EQ.0.) GO TO 63
      D3=D(NAT)
      X=D1*D1+D2*D2+D3*D3
      Y=(25+D1+D2)*D3*D3+(D1**3+D2**3+D3**3)*0.5
      Y=(D1*D2*D2+Y)/X
      RBT=RBTM(I)*GAM
      HO=SH-A-Y*0.1
      Z=0.06*RBT*SP*HO
      ATL=(AT+ATM(NAD,SA)+ATM(NAT,SA))*J
      CALL REIK(J,MINA,RB,AA,AV,NRR,HO,SK1,SV,N1,N2,TD,A1,NEV,ATM,D,
     *DV1,DV2,SK2)
      IF(AA.GE.AM) GO TO 81
   63 CONTINUE
   64 CONTINUE
   65 CONTINUE
   66 CONTINUE
      J=J+1
      IF(J.LE.MAXJ) GO TO 59
   68 CONTINUE
      NEA=4
      IF(NEA.GT.MAXEA) GO TO 89
      DO 78 I=IPR,IG
      CALL BX(GAM,KIET,I,RBM,RB,RBSM,RBS,RBGM,RBG,BMOD,BTM,XO)
      J=MINJ
   69 DO 77 NAV=NA1,NA2
      DO 76 SA=SA1,SA2
      AT=ATM(NAV,SA)
      IF(AT.EQ.0.) GO TO 76
      D1=D(NAV)
      CALL STIP(NAV,D1,SP,SH,A,DVG,MAXJ,NV1,NV2)
      DO 75 NAD=NA1,NAV
      IF(ATM(NAD,SA).EQ.0.) GO TO 75
      D2=D(NAD)
      DO 74 NAT=NA1,NAD
      IF(ATM(NAT,SA).EQ.0.) GO TO 74
      D3=D(NAT)
      DO 73 NAK=NA1,NAT
      IF(ATM(NAK,SA).EQ.0.) GO TO 73
      D4=D(NAK)
      X=D1*D1+D2*D2+D3*D3+D4*D4
      Y=(D1**3+D2**3+D3**3+D4**3)*0.5+D1*D2*D2
      Y=((25+D1+D2)*D3*D3+(50+D1+D2+D3)*D4*D4+Y)/X
      RBT=RBTM(I)*GAM
      HO=SH-A-Y*0.1
      Z=0.06*RBT*SP*HO
      ATL=(AT+ATM(NAD,SA)+ATM(NAT,SA)+ATM(NAK,SA))*J
      CALL REIK(J,MINA,RB,AA,AV,NRR,HO,SK1,SV,N1,N2,TD,A1,NEV,ATM,D,
     *DV1,DV2,SK2)
      IF(AA.GE.AM) GO TO 81
   73 CONTINUE
   74 CONTINUE
   75 CONTINUE
   76 CONTINUE
   77 CONTINUE
      J=J+1
      IF(J.LE.MAXJ) GO TO 69
   78 CONTINUE
      NEA=5
      IF(NEA.GT.MAXEA) GO TO 89
      DO 88 I=IPR,IG
      CALL BX(GAM,KIET,I,RBM,RB,RBSM,RBS,RBGM,RBG,BMOD,BTM,XO)
      J=MINJ
   79 DO 87 NAV=NA1,NA2
      DO 86 SA=SA1,SA2
      AT=ATM(NAV,SA)
      IF(AT.EQ.0.) GO TO 86
      D1=D(NAV)
      CALL STIP(NAV,D1,SP,SH,A,DVG,MAXJ,NV1,NV2)
      DO 85 NAD=NA1,NAV
      IF(ATM(NAD,SA).EQ.0.) GO TO 85
      D2=D(NAD)
      DO 84 NAT=NA1,NAD
      IF(ATM(NAT,SA).EQ.0.) GO TO 84
      D3=D(NAT)
      DO 83 NAK=NA1,NAT
      IF(ATM(NAK,SA).EQ.0.) GO TO 83
      D4=D(NAK)
      DO 82 NAP=NA1,NAK
      IF(ATM(NAP,SA).EQ.0.) GO TO 82
      D5=D(NAP)
      X=D1*D1+D2*D2+D3*D3+D4*D4+D5*D5
      Y=(D1**3+D2**3+D3**3+D4**3+D5**3)*0.5+D1*D2*D2
      Y=(25+D1+D2)*D3*D3+(50+D1+D2+D3)*D4*D4+Y
      Y=((75+D1+D2+D3+D4)*D5*D5+Y)/X
      RBT=RBTM(I)*GAM
      HO=SH-A-Y*0.1
      Z=0.06*RBT*SP*HO
      ATL=(AT+ATM(NAD,SA)+ATM(NAT,SA)+ATM(NAK,SA)+ATM(NAP,SA))*J
      CALL REIK(J,MINA,RB,AA,AV,NRR,HO,SK1,SV,N1,N2,TD,A1,NEV,ATM,D,
     *DV1,DV2,SK2)
      IF(AA.GE.AM) GO TO 81
   82 CONTINUE
   83 CONTINUE
   84 CONTINUE
   85 CONTINUE
   86 CONTINUE
   87 CONTINUE
      J=J+1
      IF(J.LE.MAXJ) GO TO 79
   88 CONTINUE
      NRR=1
   89 GO TO 110
   81 NRR=0
      IF(AV.EQ.0.0.AND.SH.LT.16.AND.Z.GE.QV) GO TO 214
      P=LP
      IF(LP.GT.(SP+LH*3)) P=SP+LH*3
      FIF=(P-SP)*0.75*LH/(SP*HO)+1
      IF(FIF.GT.1.5) FIF=1.5
      QRS=QV*QV/(0.8*FIF*RBT*SP*HO*HO)
      QRT=0.03*RBT*SP
      QR=QRS
      IF(QRT.GT.QRS) QR=QRT
      IF(SH.GT.45) GO TO 165
      SX=0.2*SH
      UX=SX*2.5
      IF(UX.GT.15.) UX=15.0
      GO TO 170
  165 SX=SH/15.0
      UX=SX*5.0
      IF(UX.GT.50.) UX=50.0
  170 UM=5.0
      IF(SH.GT.50) UM=10.0
      IF(SH.GT.90) UM=15.0
      IF(UX.LT.UM) UX=UM
      SX=SH*3/20.
      UX2=SX*5.0
      IF(UX2.LT.UM) UX2=UM
      IF(UX2.GT.50.) UX2=50.0
      DS1=D1*0.35
      IF(DS1.LT.3) DS1=3
      IF(D1.LT.20) GO TO 172
      DS1=((D1+3)*0.3+0.8)/2
      DS1=DS1*2
  172 IF(DS1.GT.12) DS1=12
      NS1=DS1-2
      IF(DS1.GT.6) NS1=(DS1+2)/2
      DS2=0.8*D1
      IF(DS2.LT.3) DS2=3
      IF(D1.EQ.12.OR.D1.EQ.14) DS2=D1-2
      IF(DSG.NE.0) DS2=DSG
      IF(DS2.GT.18) DS2=18
      NS2=DS2-2
      IF(DS2.GT.6) NS2=(DS2+2)/2
      IF(Z.GE.QV) GO TO 190
      U1=UX
      DO 176 K=1,15
      IF(U1.LE.15.) U1=U1-(K-1)*2.5
      IF(U1.GT.15.) U1=U1-(K-1)*5.0
      IF(U1.LT.UM) GO TO 177
      DO 175 NS=NS1,NS2
      DS=NS+2
      IF(NS.GT.4) DS=NS*2-2
      DO 174 S=1,4
      IF(ASK(NS,S).EQ.0.) GO TO 174
      AS=ASK(NS,S)*J/U1
      IF(S.EQ.4.AND.DS.GT.10.AND.D1*1./DS.LE.3.) AS=AS*1.137
      IF(S.EQ.4.AND.DS.LE.10.AND.D1*1./DS.LE.3.) AS=AS*1.118
      FI=DS*DS*J*0.03927*MODA(S)/(BTM*SP*U1)+1
      IF(FI.GT.1.3) FI=1.3
      FI=(1-0.01*RB)*FI
      AB=0.03*SP*HO*RB*FI
      U2=U1
      IF(AS.GE.QR.AND.AB.GE.QV) GO TO 180
  174 CONTINUE
  175 CONTINUE
  176 CONTINUE
  177 IPR=I+1
      IF(IPR.LE.IG) GO TO 45
      NRR=3
      GO TO 110
  180 IF(QR.EQ.QRT) GO TO 213
      QR=0.3125*QV*QV/(FIF*RBT*SP*HO*HO)
      U2=UX2
      DO 182 L=1,15
      IF(U2.LE.15.) U2=U2-(L-1)*2.5
      IF(U2.GT.15.) U2=U2-(L-1)*5.0
      IF(U2.LT.UM) GO TO 177
      AS=ASK(NS,S)*J/U2
      IF(S.EQ.4.AND.D1*1./DS.LE.3.) AS=AS*1.118
      IF(AS.GE.QR) GO TO 213
  182 CONTINUE
  190 DO 195 NS=NS1,NS2
      DO 195 S=1,4
      IF(ASK(NS,S).NE.0.) GO TO 200
  195 CONTINUE
  200 DS=NS+2
      IF(NS.GT.4) DS=NS*2-2
      U1=UX
      U2=UX2
  213 LS1=(ANG*25+LA/2)/U1+1
      LS1=LS1*U1
      SK3=((LS1+1)/U1+1)*2+(ANG*100+LA-2*(LS1+1))/U2-1
  214 ME=MDA(SA)
      EA=ME/BTM
      EV=MDA(SV)/BTM
      NRR=0
      CALL ANTR(LIL,A,D1,DV1,J,LP,SP,LH,SH,NRR,RO,AP1,AP3)
      IF(NRR.EQ.0) GO TO 130
  110 IF(NRR.EQ.1) WRITE(6,116) Q
  116 FORMAT(5X,'Kai Q.GE ',F6.2,' kN/kv.m, neimanoma ',
     *'parinkti apatines armaturos')
      IF(NRR.EQ.2) WRITE(6,117) Q
  117 FORMAT(5X,'Kai Q.GE ',F6.2,' kN/kv.m, neimanoma ',
     *'parinkti virsutines armaturos')
      IF(NRR.EQ.3) WRITE(6,119) Q
  119 FORMAT(5X,'Kai Q.GE.',F6.2,' kN/kv.m,  neimanomas sk',
     *'ersinis armavimas')
      IF(NRR.EQ.4) WRITE(6,120) Q
  120 FORMAT(5X,'Duotomis salygomis, kai Q=',F6.2,' neleistinas ',
     *'plysiu plotis')
      IF(NRR.EQ.5) WRITE(6,121) Q
  121 FORMAT(5X,'Duotomis salygomis, kai Q=',F6.2,' neleistina',
     *'s ilinkis')
      IF(DH.EQ.0.AND.DS.EQ.0) GO TO 280
      WRITE(6,122)
  122 FORMAT(15X,'Didiname skerspjuvi'/)
      IF(DH.EQ.0) GO TO 123
      DH=5
      IF(SH.GT.55) DH=10
      IF(SH.LT.25) DH=1
      SH=SH+DH
  123 IF(DS.EQ.0) GO TO 125
      IF(DH.NE.0) DS=(DH+1)/3
      SP=(SP+DS)/DS
      SP=SP*DS
  125 IF(SP.LT.LP) GO TO 39
      LP=SP
      LH=0
      GO TO 39
  130 SL=A*10
      ST=1273.26*AT/(D1*D1)
      VA=0.0
  134 GO TO (155,150,145,140,135),NEA
  135 X=D1*D1+D2*D2+D3*D3+D4*D4
      Y=(D1**3+D2**3+D3**3+D4**3)*0.5+D1*D2*D2
      Y=((25+D1+D2)*D3*D3+(50+D1+D2+D3)*D4*D4+Y)/X
      CALL NUTR(SH,LP,LH,ANG,LA,X,Y,ST,A,SP,D5,L5)
      VA=L5*J*VS(NAP)/1000
  140 X=D1*D1+D2*D2+D3*D3
      Y=(D1**3+D2**3+D3**3)*0.5+D1*D2*D2
      Y=((25+D1+D2)*D3*D3+Y)/X
      CALL NUTR(SH,LP,LH,ANG,LA,X,Y,ST,A,SP,D4,L4)
      VA=L4*J*VS(NAK)/1000+VA
  145 X=D1*D1+D2*D2
      Y=((D1**3+D2**3)*0.5+D1*D2*D2)/X
      CALL NUTR(SH,LP,LH,ANG,LA,X,Y,ST,A,SP,D3,L3)
      VA=L3*J*VS(NAT)/1000+VA
  150 X=D1*D1
      Y=0.5*D1
      CALL NUTR(SH,LP,LH,ANG,LA,X,Y,ST,A,SP,D2,L2)
      VA=L2*J*VS(NAD)/1000+VA
  155 VA=((LA-2)/100.+ANG)*J*VS(NAV)+VA
      L1=ANG*100+LA*4.0/3-2.0
      L1=L1*10
      VV=((LA-2)/100.+ANG)*(SK1*VS(N1)+SK2*VS(N2))
      IF(LH.NE.0) WRITE(6,216) QT,BKL(I),SH,SP,LH,LP,SL,J
  216 FORMAT(1H ,5X,'Skaic',
     *'iuojamoji apkrova P=',F6.2,' kN/m,    betonas B',A4,
     *'klases'/1H ,5X,'Skerspjuvio matmenys(cm): H=',I3,
     *'   B=',I3,'   HL=',I2,'   BL=',I3/1H ,5X,'Apsauginis ',
     *'sluoksnis ',I2,' mm'/1H ,5X,'Sija armuojama',I3,' vie',
     *'nodo tipo karkasais. Kiekviename karkase:')
      IF(LH.EQ.0) WRITE(6,217) QT,BKL(I),SH,SP,SL,J
  217 FORMAT(1H ,5X,'Skaic',
     *'iuojamoji apkrova P=',F6.2,' kN/m,    betonas B',A4,
     *'klases'/1H ,5X,'Skerspjuvio matmenys(cm): H=',I3,
     *'   B=',I3,'. Apsauginis ',
     *'sluoksnis ',I2,' mm'/1H ,5X,'Sija armuojama',I3,' vie',
     *'nodo tipo karkasais. Kiekviename karkase:')
      IF(L2*1.0/L1.GT.0.98) L2=L1
      GO TO (220,225,230,235,240),NEA
  220 WRITE(6,221) AKL(SA),D1,L1
  221 FORMAT(1H ,12X,'Apatine armatura ',A4,' klases, isdestyta',
     *' 1 eile.'/1H ,15X,'Strypu diametras D=',I2,' mm, ilgis',
     *' L=',I5,' mm')
      GO TO 245
  225 WRITE(6,226) AKL(SA),D1,L1,D2,L2
  226 FORMAT(1H ,12X,'Apatine armatura ',A4,' klases, isdestyta ',
     *'2 eilemis.'/1H ,15X,'Strypu diametrai ir ilgiai: D1=',I2,
     *' mm,  L1=',I5,' mm'/1H ,43X,'D2=',I2,' "    L2=',I5,' "')
      GO TO 245
  230 WRITE(6,231) AKL(SA),D1,L1,D2,L2,D3,L3
  231 FORMAT(1H ,12X,'Apatine armatura ',A4,' klases, isdestyta ',
     *'3 eilemis.'/1H ,15X,'Strypu diametrai ir ilgiai: D1=',I2,
     *' mm,  L1=',I5,' mm'/1H ,43X,'D2=',I2,' "    L2=',I5,' "'/
     *1H ,43X,'D3=',I2,' "    L3=',I5,' "')
      GO TO 245
  235 WRITE(6,236) AKL(SA),D1,L1,D2,L2,D3,L3,D4,L4
  236 FORMAT(1H ,12X,'Apatine armatura ',A4,' klases, isdestyta ',
     *'4 eilemis.'/1H ,15X,'Strypu diametrai ir ilgiai: D1=',I2,
     *' mm,  L1=',I5,' mm'/1H ,43X,'D2=',I2,' "    L2=',I5,' "'/
     *1H ,43X,'D3=',I2,' "    L3=',I5,' "'/1H ,43X,'D4=',I2,
     *' "    L4=',I5,' "')
      GO TO 245
  240 WRITE(6,241) AKL(SA),D1,L1,D2,L2,D3,L3,D4,L4,D5,L5
  241 FORMAT(1H ,12X,'Apatine armatura ',A4,' klases, isdestyta ',
     *'5 eilemis.'/1H ,15X,'Strypu diametrai ir ilgiai: D1=',I2,
     *' mm,  L1=',I5,' mm'/1H ,43X,'D2=',I2,' "    L2=',I5,' "'/
     *1H ,43X,'D3=',I2,' "    L3=',I5,' "'/1H ,43X,'D4=',I2,' "    ',
     *'L4=',I5,' "'/1H ,43X,'D5=',I2,' "    L5=',I5,' "')
  245 IF(NEA.GT.1) WRITE(6,242)
  242 FORMAT(1H ,7X,'Pastaba. Antras strypas priglaudzi',
     *'amas prie pirmos eiles strypo.')
      IF(NEA.GT.2) WRITE(6,243)
  243 FORMAT(1H ,16X,'Kitu eiliu strypai dedami paliekant 25 mm',
     *' tarpus')
      IF(AV.NE.0.0.OR.SH.GT.15.OR.Z.LT.QV) GO TO 247
      WRITE(6,246)
  246 FORMAT(1H ,5X,'Gniuzdoma zona nearmuojama ',
     *'ir skersine armatura nededama')
      VV=0.0
      V=0.0
      GO TO 266
  247 IF(NEV.NE.1) GO TO 251
      IF(SK1.GT.J) GO TO 249
      WRITE(6,248) AKL(SV),DV1
  248 FORMAT(1H ,12X,'Virsutine armatura ',A4,' klases D=',
     *I2,' mm po 1 strypa')
      GO TO 255
  249 STT=SK1-J
      WRITE(6,250) AKL(SV),DV1,STT
  250 FORMAT(1H ,12X,'Virsutine armatura ',A4,' klases D=',
     *I2,' mm po 1 strypa kiekvienam'/1H ,15X,'karkasui ',
     *'plius',I3,' str. sijos lentynoje')
      GO TO 255
  251 IF(SK1.GT.J) GO TO 253
      WRITE(6,252) AKL(SV),DV1,DV2
  252 FORMAT(1H ,12X,'Virsutine armatura ',A4,' klases po',
     *' 2 strypus (D1=',I2,' ir D2=',I2,' mm)')
      GO TO 255
  253 STT=SK1-J
      WRITE(6,254) AKL(SV),DV1,DV2,STT,DV1
  254 FORMAT(1H ,12X,'Virsutine armatura ',A4,' klases po',
     *' 2 strypus (D1=',I2,' ir D2=',I2,' mm)'/1H ,20X,
     *'plius',I3,' str.(D=',I2,') sijos lentynoje')
  255 WRITE(6,256) AKL(S),DS,SK3
  256 FORMAT(1H ,12X,'Skersine armatura ',A4,' klases, D=',I2,
     *' mm. Strypai(',I3,' vnt) isde-')
      IF(U2.EQ.U1) GO TO 260
      WRITE(6,258) LS1,U1,U2
  258 FORMAT(1H ,15X,'stomi: galuose (L=',I3,' cm) -',
     *' kas',F5.1,' cm, viduryje - kas',F5.1,' cm')
      GO TO 265
  260 SK3=100*ANG/U1+1
      WRITE(6,261) U1
  261 FORMAT(1H ,15X,'stomi kas',F5.1,' cm')
  265 V=J*VS(NS)*SK3*(SH-2)/100
  266 WRITE(6,267) AP1,AP3,RO
  267 FORMAT(1H ,5X,'Normaliniu plysiu plotis (mm): A1=',F5.3,
     *', A2=',F5.3,'. Ilinkis ',F5.2,' cm')
      VB=VA+VV+V
      IF(VV.NE.0.) WRITE(6,270) VA,VV,V,VB
  270 FORMAT(1H ,5X,'Armaturos kiekis(kg): apat.=',F7.2,
     *', virs.=',F7.2,', skers.=',F7.2/1H ,27X,
     *'bendras =',F7.2/1H ,5X,70('='))
      IF(VV.EQ.0.) WRITE(6,271) VA
  271 FORMAT(1H ,5X,'Armaturos kiekis -',F7.2,' kg'/1H ,5X,70('='))
  280 STOP
      END
      SUBROUTINE BX(GAM,KIET,I,RBM,RB,RBSM,RBS,RBGM,RBG,BMOD,BTM,XO)
      DIMENSION RBM(9),RBSM(9),RBGM(9),BMOD(9)
      RB=RBM(I)*GAM
      XO=0.85-0.008*RB
      RBG=RBGM(I)
      RBS=RBSM(I)
      BTM=BMOD(I)
      IF(KIET.NE.0) BTM=BTM-3.
      IF(KIET.NE.0.AND.I.EQ.1) BTM=BTM+0.5
      IF(KIET.NE.0.AND.I.GT.3) BTM=BTM-0.5
      RETURN
      END
      SUBROUTINE STIP(NAV,D1,SP,SH,A,DVG,J,NV1,NV2)
      IMPLICIT INTEGER (D,S)
      COMMON /C1/ NEA,MINA
      L=(D1+2)/6+1
      IF(D1.GT.30) L=(D1+7)/6+1
      A=0.5*L
      IF(MINA.GE.D1) A=MINA/10.
      C=3.0
      IF(D1.GT.25.OR.NEA.GT.2) C=5.0
      IF(SH.GT.90) C=7.0
      J=(SP-2*A+C)/(D1*0.1+C)
      NV1=NAV*0.5-1.2
      IF(NV1.LT.1) NV1=1
      IF(DVG.EQ.0) GO TO 5
      NV2=DVG-2
      IF(DVG.GT.6) NV2=(DVG+2)/2
      GO TO 10
    5 NV2=NAV-1
      IF(NV2.LT.1) NV2=1
   10 IF(NV2.GT.12) NV2=12
      RETURN
      END
      SUBROUTINE REIK(J,MINA,RB,AA,AV,NRR,HO,SK1,SV,
     *N1,N2,TD,A1,NEV,ATM,D,DV1,DV2,SK2)
      IMPLICIT INTEGER (D,S)
      COMMON /C2/ SIG2,XO,A,D1,SA,AM,NV1,NV2,
     *LP,LH,SP,SH,AT,KSI,ATL,Z,QV
      DIMENSION ATM(17,10),D(17)
      NRR=0
      ST=1273.26*AT/(D1*D1)
      IF(SA.LT.2.OR.SA.GT.6) ST=ST+400
      XR=XO*SIG2/((1-XO/1.1)*ST+SIG2)
      AR=(1-0.5*XR)*XR
      XR=XR*HO
      SK1=0
      SK2=0
      DV1=0
      DV2=0
      NEV=0
      N1=1
      N2=1
      WK=(HO-0.5*LH)*LP*RB*LH*0.001
      AL=0.0
      IF(WK.LT.AM) AL=(LP-SP)*(HO-0.5*LH)*RB*LH/1000
      L=LP
      IF(WK.LT.AM) L=SP
      AV=0.0
      IF(SH.LT.16.AND.Z.GE.QV) GO TO 20
    3 IF(LH.NE.0) SK1=(LP-SP)/20
      SK1=SK1*2+J
      IF(LH.EQ.0) SK1=J
      NEV=1
      DO 5 N1=NV1,NV2
      DO 5 SV=1,4
      A1=ATM(N1,SV)*SK1
      IF(A1.EQ.0.) GO TO 5
      DV1=D(N1)
      APS=(MINA+0.5*DV1)*0.1
      AV=((AM-AL)*100-0.1*AR*RB*L*HO*HO)/(HO-APS)
      IF(AV.LT.0.) AV=0.0
      IF(KSI.EQ.0) GO TO 30
      IF(A1.GE.AV) GO TO 30
    5 CONTINUE
      NRR=2
      IF(SH.LT.40) GO TO 35
      NEV=2
      SK2=J
      DO 15 N1=NV1,NV2
      DO 15 SV=1,4
      IF(ATM(N1,SV).EQ.0.) GO TO 15
      DV1=D(N1)
      DO 10 N2=NV1,N1
      IF(ATM(N2,SV).EQ.0.) GO TO 10
      DV2=D(N2)
      APS=(DV1**3+DV2**3)*0.5+DV1*DV2*DV2
      APS=(APS/(DV1*DV1+DV2*DV2)+MINA)*0.1
      A1=ATM(N1,SV)*SK1+ATM(N2,SV)*SK2
      AV=((AM-AL)*100-0.1*AR*RB*L*HO*HO)/(HO-APS)
      IF(AV.LT.0.) AV=0.0
      IF(A1.GE.AV) GO TO 30
   10 CONTINUE
   15 CONTINUE
   16 NRR=2
      GO TO 35
   20 DO 23 N1=NV1,NV2
      DO 23 SV=1,4
      IF(ATM(N1,SV).NE.0.) GO TO 25
   23 CONTINUE
   25 DV1=D(N1)
      APS=0.05*DV1+1
      AV=((AM-AL)*100-0.1*AR*RB*L*HO*HO)/(HO-APS)
      IF(AV.LT.0.) AV=0.0
      IF(AV.NE.0.) GO TO 3
      A1=0.0
   30 X=(ATL-A1)/(RB*SP*0.1)
      IF(LH.NE.0) X=X*SP/LP
      IF(LH.NE.0.AND.X.GT.LH*1.)
     *X=(ATL-A1-(LP-SP)*LH*RB*0.1)/(RB*SP*0.1)
      TD=HO-APS
      AK=(HO-0.5*X)*X*SP*RB
      IF(LH.EQ.0) AA=AK
      IF(LH.NE.0.AND.X.LE.LH*1.) AA=AK*LP/SP
      IF(LH.NE.0.AND.X.GT.LH*1.) AA=AK+(HO-0.5*LH)*(LP-SP)*LH*RB
      AA=(AA+A1*TD*10)/1000
   35 RETURN
      END
      SUBROUTINE ANTR(LIL,A,D1,DV1,J,LP,SP,LH,SH,NRR,RO,AP1,AP3)
      IMPLICIT INTEGER (D,S)
      COMMON /P1/ DV2,SK1,SK2,EV,EA,X,HO,TD,RBS,AM1,AM2,
     *QTN,ANG,BTM,RBG,ME,KIET
      VD=(DV1*DV1*SK1+DV2*DV2*SK2)*(EV-1)*0.007854
      AD=(EA-1)*X*J*0.007854
      REDA=AD+VD+(LP-SP)*LH+SP*SH
      REDS=(SH-HO)*AD+(SH+TD-HO)*VD
      REDS=(SH-0.5*LH)*(LP-SP)*LH+0.5*SP*SH*SH+REDS
      YB=REDS/REDA
      YBV=SH-YB
      YA=YB+HO-SH
      YAV=YBV+TD-HO
      REDI=YA*YA*AD+YAV*YAV*VD+SP*SH*SH*SH/12
      REDI=(YB-YBV)*(YB-YBV)*SP*SH/4+(LP-SP)*LH*LH*LH/12+REDI
      REDI=(YBV-0.5*LH)*(YBV-0.5*LH)*(LP-SP)*LH+REDI
      WT=(LH*LH*LH/3.0+YBV*YBV*LH-LH*LH*YBV)*(LP-SP)+YBV*YBV*YBV*SP/3
      IF(YBV.LE.LH*1.) WT=YBV*YBV*YBV*LP/3
      WT=(YA*YA*AD+YAV*YAV*VD+WT)*2/YB+YB*YB*SP*0.5
      AAT=AD/(EA-1)
      AVT=VD/(EV-1)
      S8=35
      IF(KIET.EQ.0) S8=40
      RMR=REDI/(YB*REDA)
      RP=(AAT*YA-AVT*YAV)*S8+(AAT+AVT)*S8*RMR
      PLM=RBS*WT-RP
      AP1=0.0
      AP3=0.0
      IF(AM1.GT.PLM/1000) GO TO 131
      AM3=QTN*ANG*ANG/8
      RO=(AM3+AM2*2)*5000/(4.08*BTM*REDI)
      GO TO 133
  131 RD1=AM1*1000/(RBG*SP*HO*HO)
      RD2=AM2*1000/(RBG*SP*HO*HO)
      FF2=(LP-SP)*LH/(SP*HO)
      FF1=EV*AVT/(0.9*SP*HO)+FF2
      FF2=EV*AVT/(0.3*SP*HO)+FF2
      TT=LH*0.5/HO
      IF(LH.EQ.0) TT=(HO-TD)/HO
      BD1=(1-TT)*FF1
      BD2=(1-TT)*FF2
      GN2=EA*AAT*10/(SP*HO)
      GN1=GN2/(GN2*1.8+(RD1+BD1)*5+1)
      GN3=GN2/(GN2*1.8+(RD2+BD2)*5+1)
      GN2=GN2/(GN2*1.8+(RD2+BD1)*5+1)
      IF(GN1.GT.1.) GN1=0.99
      IF(GN2.GT.1.) GN2=0.99
      IF(GN3.GT.1.) GN3=0.99
      T1=(SH-0.05*D1-GN1*HO-A)/((1-GN1)*HO)
      T2=(SH-0.05*D1-GN2*HO-A)/((1-GN2)*HO)
      T3=(SH-0.05*D1-HO*GN3-A)/((1-GN3)*HO)
      IF(T1.LT.1.) T1=1.0
      IF(T2.LT.1.) T2=1.0
      IF(T3.LT.1.) T3=1.0
      ZP1=(1-(TT*FF1*2+GN1*GN1)/((FF1+GN1)*2))*HO
      ZP2=(1-(TT*FF1*2+GN2*GN2)/((FF1+GN2)*2))*HO
      ZP3=(1-(TT*FF2*2+GN3*GN3)/((FF2+GN3)*2))*HO
      GM1=AM1*T1/(AAT*ZP1*ME)
      GM2=AM2*T2/(AAT*ZP2*ME)
      GM3=AM2*T3/(AAT*ZP3*ME)
      FL=1.6-15*AAT/(SP*HO)
      IF(FL.LT.1.3) FL=1.3
      AP2=2000*AAT/(SP*HO)
      IF(AP2.GT.40.) AP2=40.0
      AP1=(70-AP2)*GM1*D1**(1/3.)
      AP2=AP1*GM2/GM1
      AP3=AP2*GM3*FL/GM2
      AP1=AP1-AP2+AP3
      NRR=0
      IF(AP1.LE.0.4.AND.AP3.LE.0.3) GO TO 132
      NRR=4
      GO TO 134
  132 FM1=RBS*WT/(AM1*1000+RP)
      FM2=RBS*WT/(AM2*1000+RP)
      PS1=1.25-1.1*FM1
      PS2=1.25-1.1*FM2
      PS3=1.25-0.8*FM2
      IF(PS1.GT.1.) PS1=1.0
      IF(PS2.GT.1.) PS2=1.0
      IF(PS3.GT.1.) PS3=1.0
      RR1=5000*ANG*ANG/(ME*AAT*4.8)
      RR2=4500*ANG*ANG/(SP*HO*BTM*4.8)
      RO1=(RR1*PS1+RR2/((FF1+GN1)*0.45))*AM1/(HO*ZP1)
      RO2=(RR1*PS2+RR2/((FF1+GN2)*0.45))*AM2/(HO*ZP2)
      RO3=(RR1*PS3+RR2/((FF2+GN3)*0.15))*AM2/(HO*ZP3)
      RO=RO1-RO2+RO3
  133 FLE=ANG/1.5
      IF(ANG.GT.6.AND.ANG.LE.10) FLE=4.0
      IF(ANG.GT.10) FLE=ANG/2
      IF(LIL.NE.0) FLE=LIL
      NRR=0
      IF(RO.LE.FLE) GO TO 134
      NRR=5
  134 RETURN
      END
      SUBROUTINE NUTR(SH,LP,LH,ANG,LA,X,Y,ST,A,SP,D,L)
      IMPLICIT INTEGER (D,S)
      COMMON /C3/ DV1,NEV,A1,QT,J,RB
      H=SH-A-Y/10
      AT=(H-A-0.05*DV1*NEV)*A1/100
      AA=7.854E-4*ST*J*X
      Z=(AA-A1)/(0.1*RB*LP)
      IF(Z.GT.LH) GO TO 1
      AT=AT+(H-0.5*Z)*Z*RB*LP/1000
      GO TO 2
    1 Z=(AA-A1)/(0.1*RB*SP)-(LP-SP)*LH/SP
      AT=AT+(H-0.5*Z)*Z*RB*SP/1000
      AT=AT+(LP-SP)*(H-0.5*LH)*RB*LH/1000
    2 Z=ANG*ANG-8*AT/QT
      IF(Z.LT.0.) Z=0.0
      Z=(ANG-SQRT(Z))*50
      W=(0.7*ST/RB+11)*0.1*D
      IF(W.LT.2*D) W=2*D
      L=(ANG*100-(Z-W)*2)/5
      L=L*50
      IF(L-950*ANG) 11,11,10
   10 L=ANG*100+LA-3.5
      L=L*10
   11 RETURN
      END
