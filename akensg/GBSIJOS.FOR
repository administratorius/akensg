      IMPLICIT INTEGER (D,S)
      DIMENSION DM(17),VS(17),ATA(9,17),ATS(10,4),AKL(9),MDA(9),
     *SER(14),BKL(9),BRS(9),BTS(9),BRN(9),BTN(9),BMD(9)
      COMMON /C1/ SB,SH,LB,LH,MNA,BL,RZ
      COMMON /C2/ RB,RT,NL,AT,WS,AR,DG,KGN
      COMMON /C3/ D1,D2,D3,D4,D5,Y1,Y2,Y3,Y4,Y5,DV,YV,SZ
      COMMON /UT/ EA,FV,EV,RV,RU,WT,U1,U2,SS1,SS2,FA
      OPEN(UNIT=5,FILE='GBSIJOS.DAT')
      OPEN(UNIT=6,FILE='GBSIJOS.RES')
      DATA DM /3,4,5,6,8,10,12,14,16,18,20,22,25,28,32,36,40/
      DATA VS /0.055,0.099,0.154,0.222,0.395,0.617,0.888,1.208,
     *1.578,1.998,2.466,2.984,3.84,4.83,6.31,7.99,9.865/
      DATA ATA /2.6507,8.5883,0.0,0.0,0.0,0.0,0.0,0.0,0.0,4.5867,14.388,
     *0.0,0.0,0.0,0.0,0.0,0.0,0.0,7.0686,20.519,0.0,0.0,0.0,0.0,0.0,0.0,
     *0.0,0.0,27.709,6.3617,0.0,10.037,0.0,0.0,0.0,0.0,0.0,0.0,11.31,
     *0.0,17.844,0.0,0.0,0.0,0.0,0.0,0.0,17.671,21.991,28.667,0.0,
     *40.055,53.407,64.01,0.0,0.0,0.0,31.667,41.281,0.0,57.68,76.906,
     *92.174,0.0,0.0,0.0,43.103,56.187,0.0,78.509,104.68,125.46,0.0,
     *0.0,0.0,56.298,73.388,0.0,102.54,136.72,163.87,0.0,0.0,0.0,
     *71.251,92.881,0.0,129.78,173.04,207.39,0.0,0.0,0.0,87.965,
     *114.67,141.37,160.22,213.63,256.04,0.0,0.0,0.0,106.44,138.75,
     *171.06,193.87,258.49,309.81,0.0,0.0,0.0,137.44,179.17,220.89,
     *0.0,333.79,0.0,0.0,0.0,0.0,172.41,224.75,277.09,0.0,418.71,
     *0.0,0.0,0.0,0.0,225.19,293.55,361.91,0.0,546.89,0.0,0.0,0.0,0.0,
     *285.01,371.52,458.04,0.0,0.0,0.0,0.0,0.0,0.0,351.86,458.67,
     *565.49,0.0,0.0,0.0/
      DATA MDA /170,200,210,210,200,180,190,190,190/
      DATA SER /410,405,395,1490,1410,1335,1255,235,295,390,540,
     *590,785,980/
      DATA ATS /1.9085,3.3301,5.1051,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
     *0.0,0.0,4.948,8.7965,13.745,0.0,0.0,0.0,0.0, 0.0,0.0,0.0,0.0,0.0,
     *17.672,25.447,34.636,45.239,57.256,0.0,0.0,0.0,8.0582,14.326,
     *22.777,32.798,44.642,58.308,73.796/
      DATA BRS /4.5,6.0,7.5,8.5,11.5,14.5,17.0,19.5,22.0/
      DATA BTS /0.48,0.57,0.66,0.75,0.9,1.05,1.2,1.3,1.4/
      DATA BRN /5.5,7.5,9.5,11.0,15.0,18.5,22.0,25.5,29.0/
      DATA BTN /0.7,0.85,1.0,1.15,1.4,1.6,1.8,1.95,2.1/
      DATA BMD /16.0,18.0,21.0,23.0,27.0,30.0,32.5,34.5,36.0/
      READ(5,*) ILG,LA,SH,SB,LH,LB
      READ(5,*) P,PT,PI
      READ(5,*) BK1,BK2,KT,GM
      READ(5,*) MXE,KPR,KGL,DA1,DA2
      READ(5,*) KGN,LIL,RZ
      READ(5,2) BKL
    2 FORMAT(9(A4,1X))
      READ(5,4) AKL
    4 FORMAT(/9(A4,1X))
      ANG=ILG*0.01-0.04*LA/3
      IPR=BK1/2.5-2
      IF(BK1.GT.15.) IPR=BK1/5+1
      IG=BK2/2.5-2
      IF(BK2.GT.15.) IG=BK2/5+1
      SG=400
      IF(GM.LT.1.) SG=500
      CALL SPAU(ILG,SH,SB,LH,LB,LA,P,PT,PI,BKL,IPR,IG,KT,GM,KPR,
     *KGL,DA1,DA2,LIL,AKL,KGN,RZ)
      IF(LB.LT.SB.OR.LH.EQ.0) LB=SB
      KP=KPR
      KG=KGL
      DP=DA1-2
      IF(DA1.GT.6) DP=(DA1+2)/2
      IF(DA1.GT.25) DP=DA1/5+9
      DG=DA2-2
      IF(DA2.GT.6) DG=(DA2+2)/2
      IF(DA2.GT.25) DG=DA2/5+9
      IP=IPR
      BL=LB
      IF(LH.EQ.0) GO TO 8
      B=SB+ANG*100/3
      IF(B.LT.BL) BL=B
      B=12*LH+SB
      L1=SH/10
      L2=SH/20
      IF(LH.LT.L1.AND.LH.GE.L2) B=6*LH+SB
      IF(B.LT.BL) BL=B
      IF(LH.LT.L2) BL=SB
    8 MNA=20
      IF(SH.LT.11) MNA=10
      IF(SH.GT.10.AND.SH.LT.25) MNA=15
      PS=((LB-SB)*LH+SB*SH)*0.0027+P
      WS=PS*ANG*ANG/8
      QS=ANG*PS/2
      PI=PI+((LB-SB)*LH+SB*SH)*0.00245
      WN1=(PT+PI)*ANG*ANG/8
      WN2=ANG*ANG*PI/8
      NL=(LB-SB)/20
      NL=NL*2
      YV=0.0
      JV=DP/2
      IF(JV.LT.1) JV=1
      DZ=DP
      NZ=0
      SZ=1
      IF((SB.GE.SH.AND.SH.LT.31).OR.SH.LT.16) SZ=0
      IBP=IP
      KAP=KP
    9 DO 60 IB=IP,IG
      IF(KG.LT.KP) KG=KP
      IF(DG.LT.DP) DG=DP
      RB=BRS(IB)*GM
      RT=BTS(IB)*GM
      RBN=BRN(IB)
      RTN=BTN(IB)
      BM=BMD(IB)
      IF(KT.EQ.0) GO TO 10
      IF(IB.EQ.1.OR.IB.EQ.4) BM=BM-1.5
      IF(IB.EQ.2.OR.IB.EQ.3) BM=BM-2
      IF(IB.EQ.5.OR.IB.EQ.6) BM=BM-3
      IF(IB.GT.6) BM=BM-3.5
   10 NR=0
      DO 57 KA=KP,KG
      IE=1
      S=0
      DO 12 N1=DP,DG
      IF(KA.EQ.1.AND.N1.GT.3.OR.KA.EQ.2.AND.N1.GT.4) GO TO 12
      T1=ATA(KA,N1)
      IF(T1.EQ.0.) GO TO 12
      AT=T1
      S=S+1
      D1=DM(N1)
      IF(S.EQ.1) CALL RIBI(KA,T1,D1,GM,RB,XR,AR)
      CALL STIP(IE,ATA,DM,HO,NA,KV,NV,JV,IV,MXN,ATR,AA,VA,AO)
      IF(AO.GT.AR.OR.ATR.EQ.0.) GO TO 12
      IF(AA.NE.0.AND.AA/ATR.GE.RZ) GO TO 63
   12 CONTINUE
      IF(IE.EQ.MXE) GO TO 57
   13 IE=2
      S=0
      DO 18 N1=DP,DG
      T1=ATA(KA,N1)
      IF(T1.EQ.0.) GO TO 18
      S=S+1
      D1=DM(N1)
      IF(S.EQ.1) CALL RIBI(KA,T1,D1,GM,RB,XR,AR)
      DO 15 N2=DP,N1
      T2=ATA(KA,N2)
      IF(T2.EQ.0.) GO TO 15
      D2=DM(N2)
      AT=T1+T2
      CALL STIP(IE,ATA,DM,HO,NA,KV,NV,JV,IV,MXN,ATR,AA,VA,AO)
      IF(AO.GT.AR.OR.ATR.EQ.0.) GO TO 15
      IF(AA/ATR.GE.RZ) GO TO 63
   15 CONTINUE
   18 CONTINUE
      IF(IE.EQ.MXE) GO TO 57
      IE=3
      S=0
      DO 27 N1=DP,DG
      T1=ATA(KA,N1)
      IF(T1.EQ.0.) GO TO 27
      S=S+1
      D1=DM(N1)
      IF(S.EQ.1) CALL RIBI(KA,T1,D1,GM,RB,XR,AR)
      DO 24 N2=DP,N1
      T2=ATA(KA,N2)
      IF(T2.EQ.0.) GO TO 24
      D2=DM(N2)
      DO 21 N3=DP,N2
      T3=ATA(KA,N3)
      IF(T3.EQ.0.) GO TO 21
      D3=DM(N3)
      AT=T1+T2+T3
      CALL STIP(IE,ATA,DM,HO,NA,KV,NV,JV,IV,MXN,ATR,AA,VA,AO)
      IF(AO.GT.AR.OR.ATR.EQ.0.) GO TO 21
      IF(AA/ATR.GE.RZ) GO TO 63
   21 CONTINUE
   24 CONTINUE
   27 CONTINUE
      IF(IE.EQ.MXE) GO TO 57
      IE=4
      S=0
      DO 39 N1=DP,DG
      T1=ATA(KA,N1)
      IF(T1.EQ.0.) GO TO 39
      S=S+1
      D1=DM(N1)
      IF(S.EQ.1) CALL RIBI(KA,T1,D1,GM,RB,XR,AR)
      DO 36 N2=DP,N1
      T2=ATA(KA,N2)
      IF(T2.EQ.0.) GO TO 36
      D2=DM(N2)
      DO 33 N3=DP,N2
      T3=ATA(KA,N3)
      IF(T3.EQ.0.) GO TO 33
      D3=DM(N3)
      DO 30 N4=DP,N3
      T4=ATA(KA,N4)
      IF(T4.EQ.0.) GO TO 30
      D4=DM(N4)
      AT=T1+T2+T3+T4
      CALL STIP(IE,ATA,DM,HO,NA,KV,NV,JV,IV,MXN,ATR,AA,VA,AO)
      IF(AO.GT.AR.OR.ATR.EQ.0.) GO TO 30
      IF(AA/ATR.GE.RZ) GO TO 63
   30 CONTINUE
   33 CONTINUE
   36 CONTINUE
   39 CONTINUE
      IF(IE.EQ.MXE) GO TO 57
      IE=5
      S=0
      DO 54 N1=DP,DG
      T1=ATA(KA,N1)
      IF(T1.EQ.0.) GO TO 54
      S=S+1
      D1=DM(N1)
      IF(S.EQ.1) CALL RIBI(KA,T1,D1,GM,RB,XR,AR)
      DO 51 N2=DP,N1
      T2=ATA(KA,N2)
      IF(T2.EQ.0.) GO TO 51
      D2=DM(N2)
      DO 48 N3=DP,N2
      T3=ATA(KA,N3)
      IF(T3.EQ.0.) GO TO 48
      D3=DM(N3)
      DO 45 N4=DP,N3
      T4=ATA(KA,N4)
      IF(T4.EQ.0.) GO TO 45
      D4=DM(N4)
      DO 42 N5=DP,N4
      T5=ATA(KA,N5)
      IF(T5.EQ.0.) GO TO 42
      D5=DM(N5)
      AT=T1+T2+T3+T4+T5
      CALL STIP(IE,ATA,DM,HO,NA,KV,NV,JV,IV,MXN,ATR,AA,VA,AO)
      IF(AO.GT.AR.OR.ATR.EQ.0.) GO TO 42
      IF(AA/ATR.GE.RZ) GO TO 63
   42 CONTINUE
   45 CONTINUE
   48 CONTINUE
   51 CONTINUE
   54 CONTINUE
   57 CONTINUE
   60 CONTINUE
      IF(SZ.NE.0) GO TO 61
      IP=IP+1
      IF(IP.LE.IG) GO TO 9
      IP=IBP
      SZ=SZ+1
      IF(SZ.EQ.1) GO TO 9
   61 JV=JV+1
      IF(JV.LE.DG.AND.JV.LE.N1) GO TO 9
      IF(AO.GT.AR.OR.AA/ATR.LT.RZ) NR=1
      GO TO 150
   63 NZ=0
      GZ=1.0
      HS=SH-Y1
      SV=YV*10
      F1=1-0.01*RB
      Q1=0.03*SB*HS*RB*F1
      S=LB
      IF((SB+3*LH).LT.S) S=SB+3*LH
   64 F2=1+(S-SB)*LH*0.75/(SB*HS)
      IF(F2.GT.1.5) F2=1.5
      Q2=0.06*HS*SB*RT*F2
      IF(Q1.LT.QS.OR.Q2.LT.QS) NZ=1
      IF((Q2.LT.QS.AND.LA.GE.D1).OR.(Q2.GE.QS.AND.LA.GE.D1/2)) GO TO 65
      PRINT *,'Nepakankamas atremimo ilgis. Iveskite 0 ir pataisykite'
      READ *,N
      V=3/N
   65 IF(SZ.NE.0) GO TO 102
      IP=IB+1
      IF(NZ.NE.0.AND.IP.LE.IG) GO TO 9
      IP=IBP
	IF(KGN.EQ.0) AV=0.0
  102 IF(SZ.EQ.0.AND.NZ.EQ.0.AND.AV.EQ.0.) GZ=0.0
      IF(GZ.EQ.0.) GO TO 99
      IF(NZ.NE.0) SZ=SZ+1
      IF(SZ.EQ.1) GO TO 9
      DSP=D1/3
      IF(DSP.LT.3) DSP=3
      IF(D1.GT.20) DSP=8
      IF(D1.GT.32) DSP=10
      SP=DSP-2
      IF(DSP.GT.6) SP=(DSP+2)/2
      SG=D1-3
      IF(D1.GT.6) SG=(D1+2)/2-1
      IF(D1.GT.25) SG=D1/5+8
      IF(SG.LT.1) SG=1
      QRS=QS*QS/(0.8*F2*RT*SB*HS*HS)
      QRT=0.0
      IF(NZ.NE.0) QRT=0.03*F2*RT*SB
      QR=QRS
      IF(QRT.GT.QRS) QR=QRT
      SX=0.2*SH
      UX=SX*2.5
      IF(UX.GT.15.) UX=15.0
      IF(SH.LT.46) GO TO 105
      SX=SH/15.0
      UX=SX*5.0
      IF(UX.GT.50.) UX=50.0
  105 UN=5.0
      IF(SH.GT.50) UN=10.0
      IF(SH.GT.90) UN=15.0
      IF(UX.LT.UN) UX=UN
      SX=SH*3/20.
      UX2=SX*5.0
      IF(UX2.LT.UN) UX2=UN
      IF(UX2.GT.50.) UX2=50.0
      IF(NZ.EQ.0) GO TO 87
      DD=0
  106 S=1
      DO 78 IS=SP,SG
      K1=1
      IF(IS.GT.3) K1=3
      DO 75 KS=K1,5
      IF(KS.EQ.2) GO TO 75
      IF(KS.GT.2) S=KS-1
      AT=ATS(IS,S)
      IF(AT.EQ.0.) GO TO 75
      DS=IS+2
      IF(IS.GT.4) DS=IS*2-2
      U1=UX+2.5
      IF(UX.GT.17.5) U1=UX+5.0
      DO 72 KU=1,15
      IF(U1.LE.17.5) U1=U1-2.5
      IF(U1.GT.17.5) U1=U1-5.0
      IF(U1.LT.UN) GO TO 75
      AS=AT*NA/U1
      IF(KS.EQ.5.AND.DS.GT.10.AND.D1*1./DS.LE.3.) AS=AS*1.137
      IF(KS.EQ.5.AND.DS.LE.10.AND.D1*1./DS.LE.3.) AS=AS*1.118
      FI=DS*DS*NA*0.03927*MDA(KS)/(BM*SB*U1)+1
      IF(FI.GT.1.3) FI=1.3
      FI=(1-0.01*RB)*FI
      AB=0.03*SB*HS*RB*FI
      IF(AS.GE.QR.AND.AB.GE.QS) GO TO 81
   72 CONTINUE
   75 CONTINUE
   78 CONTINUE
      IP=IB+1
      IF(IP.LE.IG) GO TO 9
      IP=IBP
      NR=2
      GO TO 150
   81 QR=0.3125*QS*QS/(F2*RT*SB*HS*HS)
      IF(QRT.GT.QR) QR=QRT
      U2=UX2+2.5
      IF(UX2.GT.15.) U2=UX2+5.0
      DO 84 L=1,15
      IF(U2.LE.17.5) U2=U2-2.5
      IF(U2.GT.17.5) U2=U2-5.0
      AU=AT*NA/U2
      IF(AU.GE.QR) GO TO 99
   84 CONTINUE
   87 DO 93 IS=SP,SG
      S=0
      DO 90 KS=1,5
      IF(KS.EQ.2) GO TO 90
      S=S+1
      IF(ATS(IS,S).NE.0.) GO TO 96
   90 CONTINUE
   93 CONTINUE
   96 DS=IS+2
      IF(IS.GT.4) DS=IS*2-2
      U1=UX
      U2=UX2
   99 IF(GZ.EQ.0.) DV=0
      ISP=SP
      IF(U2.NE.0.) LU=ANG*50/U2
      SS2=U2*LU
      SS1=(ILG-SS2-6)/2
      T=T1/(0.07854*D1*D1)+0.006
      IF(LO.NE.0) GO TO 113
      ST=T*100
      EA=MDA(KA)/BM
      IF(KV.NE.0) EV=MDA(KV)/BM
      FV=(NA+NL)*DV*DV*0.007854
      RV=(EV-1)*FV
   97 X=D1*D1+D2*D2+D3*D3+D4*D4+D5*D5
      Y=SH-HO
      CALL NUTR(ILG,ANG,RB,SH,SB,LH,BL,PS,ST,NA,X,Y,YV,VA,LB,
     *KT,IB,RTN,PLM,D5,L5,Z5,XL,RD)
      RDI=RD
      AP=FA
      IF(RP.EQ.0.) RP=RU
      IF(WP.EQ.0.) WP=WT
      L1=ILG*10-20
      L2=0
      VT=L1*NA*VS(N1)/1000
	IF(IE.EQ.1) GO TO 113
      GO TO (113,111,108,104,101),IE
  101 X=D1*D1+D2*D2+D3*D3+D4*D4
      Y=(D1*D1*Y1+D2*D2*Y2+D3*D3*Y3+D4*D4*Y4)/X
      CALL NUTR(ILG,ANG,RB,SH,SB,LH,BL,PS,ST,NA,X,Y,YV,VA,LB,
     *KT,IB,RT,PL,D5,L5,Z5,XP,RD)
      VT=VT+L5*NA*VS(N5)/1000
  104 X=D1*D1+D2*D2+D3*D3
      Y=(D1*D1*Y1+D2*D2*Y2+D3*D3*Y3)/X
      CALL NUTR(ILG,ANG,RB,SH,SB,LH,BL,PS,ST,NA,X,Y,YV,VA,LB,
     *KT,IB,RT,PL,D4,L4,Z4,XP,RD)
      VT=VT+L4*NA*VS(N4)/1000
  108 X=D1*D1+D2*D2
      Y=(D1*D1*Y1+D2*D2*Y2)/X
      CALL NUTR(ILG,ANG,RB,SH,SB,LH,BL,PS,ST,NA,X,Y,YV,VA,LB,
     *KT,IB,RT,PL,D3,L3,Z3,XP,RD)
      VT=VT+L3*NA*VS(N3)/1000
  111 X=D1*D1
      Y=Y1
	CALL NUTR(ILG,ANG,RB,SH,SB,LH,BL,PS,ST,NA,X,Y,YV,VA,LB,
     *KT,IB,RT,PL,D2,L2,Z2,XP,RD)
  113 IF(N2.NE.0) VT=VT+L2*VS(N2)*NA/1000
      IF(GZ.EQ.0.) GO TO 114
      VG=L1*NV*VS(IV)/1000
  114 UP=U1
      ANK=(0.3+0.05*ST/RB)*D1
      IF(ANK.LT.D1*1.2) ANK=1.2*D1
      IF(ANK.LT.20) ANK=20
      IF(XP.LT.LA/3.) XP=LA/3.0
      XL=XP-1+LA*2/3.0
      IF(XL.GE.ANK) GO TO 118
  116 IF(GZ.NE.0.) AS=AT*NA/U1
      IF(KS.EQ.5.AND.DS.GT.10.AND.D1*1./DS.LE.3.) AS=AS*1.137
      IF(KS.EQ.5.AND.DS.LE.10.AND.D1*1./DS.LE.3.) AS=AS*1.118
      CP=(QS-PS*XP/100)/(AS+PS/100)
      WR=(CP+XP)*QS-(CP+XP)*(CP+XP)*PS/200
      X=(FA*ST*XL*0.1/ANK-VA)/(RB*SB*0.1)
      Z=HS-X/2
      IF(X.LT.2*YV) Z=HS-YV
      W=FA*ST*Z*XL*0.1/ANK+AS*CP*CP/2
      IF(W/WR.GE.RZ) GO TO 118
      IF(GZ.EQ.0.) GO TO 119
      IF(U1.GT.15) U1=U1-5.0
      IF(U1.LE.15) U1=U1-2.5
      IF(U1.GE.UN) GO TO 116
      DD=DS
      SP=SP+1
      IF(SP.LE.SG.AND.SP.LE.7) GO TO 106
      NR=5
      IF(L2.EQ.L1) GO TO 150
      NR=0
      L2=L1
      X=D1*D1+D2*D2
      Y=(D1*D1*Y1+D2*D2*Y2)/X
      FA=X*0.00784*NA
      HS=SH-Y
      LO=2
      GO TO 64
  119 NA=NA+1
      IF(NA.LE.MXN) GO TO 97
      DP=DP+1
      IF(DP.LE.DG) GO TO 9
      DP=DZ
      SZ=SZ+1
      IF(SZ.EQ.1) GO TO 9
      GO TO 100
  118 LZ=XL+CP+1
      IF(GZ.EQ.0.) GO TO 117
      SKZ=0
      IF(UP.NE.U1) SKZ=2+2*LZ/U1
      IF(U2.EQ.U1.AND.UP.EQ.U1) SK3=1+ILG/U1
      IF(U2.EQ.UP.AND.UP.NE.U1) SK3=(ILG-2*LZ)/UP
      IF(U2.NE.U1.AND.UP.EQ.U1) SK3=1+SS2/U2+2*SS1/U1
      IF(U2.NE.UP.AND.UP.NE.U1) SK3=SS2/U2+(SS1-LZ)*2/UP
      IF(DD.EQ.DS) SK3=SK3+SKZ
  117 S1=(Y1+0.02)*10
      S2=(Y2+0.02)*10
      S3=(Y3+0.02)*10
      S4=(Y4+0.02)*10
      S5=Y5*10
      AP1=0.0
      AP3=0.0
      IF(WN1.GT.PLM/1000) GO TO 120
      RO=(WN1+WN2*2)*1225.5/(BM*RDI)
      GO TO 135
  120 RD1=WN1*1000/(RBN*SB*HO*HO)
      RD2=WN2*1000/(RBN*SB*HO*HO)
      FF1=RV/0.9
      FF2=(FF1*3+(BL-SB)*LH)/(SB*HO)
      FF1=(FF1+(BL-SB)*LH)/(SB*HO)
      H=LH
      IF((LH.EQ.0.OR.LB.EQ.SB).AND.FV.EQ.0.) H=0.0
      IF((LH.EQ.0.OR.LB.EQ.SB).AND.FV.NE.0.) H=YV*2
      BD1=(1-0.5*H/HO)*FF1
      BD2=BD1
      IF(FF1.NE.0.) BD2=BD1*FF2/FF1
      XI2=EA*10*AP/(SB*HO)
      XI1=XI2/(XI2*1.8+(RD1+BD1)*5+1)
      XI3=XI2/(XI2*1.8+(RD2+BD2)*5+1)
      XI2=XI2/(XI2*1.8+(RD2+BD1)*5+1)
      IF(XI1.GT.1.) XI1=0.99
      IF(XI2.GT.1.) XI2=0.99
      IF(XI3.GT.1.) XI3=0.99
      T=SH-HO
      T1=(SH-XI1*HO-Y1)/((1-XI1)*HO)
      T2=(SH-XI2*HO-Y1)/((1-XI2)*HO)
      T3=(SH-HO*XI3-Y1)/((1-XI3)*HO)
      Z1=(H*FF1/HO+XI1*XI1)*0.5/(FF1+XI1)
      Z2=(H*FF1/HO+XI2*XI2)*0.5/(FF1+XI2)
      Z3=(H*FF2/HO+XI3*XI3)*0.5/(FF2+XI3)
      Z1=(1-Z1)*HO
      Z2=(1-Z2)*HO
      Z3=(1-Z3)*HO
      IF(T1.LT.1.) T1=1.0
      IF(T2.LT.1.) T2=1.0
      IF(T3.LT.1.) T3=1.0
      GM1=WN1*T1/(AP*Z1)
      GM2=WN2*T2/(AP*Z2)
      GM3=WN2*T3/(AP*Z3)
      L=N1
      IF(KA.EQ.1) GO TO 123
      L=N1+3
      IF(KA.EQ.2) GO TO 123
      L=KA+5
  123 T=SER(L)*1.0/1000
      IF(GM1.GT.T) GM1=T
      IF(GM2.GT.T) GM2=T
      IF(GM3.GT.T) GM3=T
  129 ET=1.0
      IF(KA.LT.3) ET=1.2
      IF(KA.EQ.3) ET=1.3
      UM=AP/(SB*HO)
      IF(UM.GT.0.02) UM=0.02
      FL=1.6-15*UM
      UD=D1**(1/3.)
      AP3=(70-2000*UM)*FL*UD
      AP3=(70-2000*UM)*UD*ET/MDA(KA)
      AP1=GM1*AP3
      AP2=GM2*AP3
      AP3=GM3*FL*AP3
      AP1=AP1-AP2+AP3
      IF(0.4/AP1.GE.RZ.AND.0.3/AP3.GE.RZ) GO TO 132
      KP=KP+1
      IF(KP.LE.KG) GO TO 9
      KP=KAP
      NR=3
      GO TO 150
  132 FM1=RTN*WP/(WN1*1000+RP)
      FM2=RTN*WP/(WN2*1000+RP)
      PS1=1.25-1.1*FM1
      PS2=1.25-1.1*FM2
      PS3=1.25-0.8*FM2
      IF(PS1.GT.1.) PS1=1.0
      IF(PS2.GT.1.) PS2=1.0
      IF(PS3.GT.1.) PS3=1.0
      RR1=5000*ANG*ANG/(MDA(KA)*AP*4.8)
      C=0.9
      IF(IB.EQ.1) C=0.7
      RR2=C*5000*ANG*ANG/(SB*HO*BM*4.8)
      RO1=(RR1*PS1+RR2/((FF1+XI1)*0.45))*WN1/(HO*Z1)
      RO2=(RR1*PS2+RR2/((FF1+XI2)*0.45))*WN2/(HO*Z2)
      RO3=(RR1*PS3+RR2/((FF2+XI3)*0.15))*WN2/(HO*Z3)
      RO=RO1-RO2+RO3
  135 FLE=ANG/1.5
      IF(ANG.GT.6.AND.ANG.LE.10) FLE=4.0
      IF(ANG.GT.10) FLE=ANG/2
      IF(LIL.NE.0) FLE=LIL
      IF(RO.LE.FLE) GO TO 138
      IP=IP+1
      IF(IP.LE.IG) GO TO 9
      IP=IBP
      NR=4
      GO TO 150
  138 IF(LH.NE.0) WRITE(6,141) PS,BKL(IB),SH,SB,LH,LB,NA
  141 FORMAT(1H ,5X,'Skaiciuojamoji apkrova P=',F6.2,' kN/m,    beton',
     *'as B',A4,'klases'/1H ,5X,'Skerspjuvio matmenys(cm): H=',I3,
     *'   B=',I3,'   HL=',I2,'   BL=',I3/1H ,5X,'Sija armuoj',
     *'ama',I3,' vienodo tipo karkasais. Kiekviename karkase:')
      IF(LH.EQ.0) WRITE(6,144) PS,BKL(IB),SH,SB,NA
  144 FORMAT(1H ,5X,'Skaiciuojamoji apkrova P=',F6.2,' kN/m,    beton',
     *'as B',A4,'klases'/1H ,5X,'Skerspjuvio matmenys(cm): H=',I3,
     *'   B=',I3,'.'/1H ,5X,'Sija armuojama',I3,' vienodo tipo karkas',
     *'ais. Kiekviename karkase:')
      IF(IE.NE.1) GO TO 148
      WRITE(6,146) AKL(KA),D1,L1,S1
  146 FORMAT(1H ,6X,'- Apatine armatura ',A4,' klases, isdestyta 1 ei',
     *'le.'/1H ,10X,'Strypu diametrai, ilgiai ir atstumai nuo ce',
     *'ntro iki elemento'/1H ,10X,'apacios (mm):'/1H ,12X,' - d=',I2,
     *',  l=',I5,',  ya=',I3)
      GO TO 162
  148 IF(D1.GT.8) WRITE(6,152) AKL(KA),IE,D1,L1,S1,D2,L2
  152 FORMAT(1H ,6X,'- Apatine armatura ',A4,' klases, isdestyta',I2,
     *' eilemis.'/1H ,10X,'Strypu diametrai, ilgiai ir atstumai nuo c',
     *'entro iki elemento'/1H ,10X,'apacios (mm):'/1H ,12X,'I eil.  -',
     *' d=',I2,',  l=',I5,',  ya=',I3/1H ,12X,'II eil. - d=',
     *I2,',  l=',I5,',  - priglaudziama prie I eiles.')
      IF(D1.LT.10) WRITE(6,154) AKL(KA),IE,D1,L1,S1,D2,L2,S2
  154 FORMAT(1H ,6X,'- Apatine armatura ',A4,' klases, isdestyta',I2,
     *' eilemis.'/1H ,10X,'Strypu diametrai, ilgiai ir atstumai nuo c',
     *'entro iki elemento'/1H ,10X,'apacios (mm):'/1H ,12X,'I eil.  -',
     *' d=',I2,',  l=',I5,',  ya=',I3/1H ,12X,'II eil. - d=',
     *I2,',  l=',I5,',  ya=',I3)
      IF(IE.EQ.2) GO TO 162
      WRITE(6,156) D3,L3,S3
  156 FORMAT(1H ,12X,'III eil.- d=',I2,',  l=',I5,',  ya=',I3)
      IF(IE.EQ.3) GO TO 162
      WRITE(6,158) D4,L4,S4
  158 FORMAT(1H ,12X,'IV eil. - d=',I2,',  l=',I5,',  ya=',I3)
      IF(IE.EQ.4) GO TO 162
      WRITE(6,160) D5,L5,S5
  160 FORMAT(1H ,12X,'V eil.  - d=',I2,',  l=',I5,',  ya=',I3)
  162 IF(GZ.NE.0.) GO TO 164
      WRITE(6,163)
  163 FORMAT(1H ,5X,'Gniuzdoma zona nearmuojama ',
     *'ir skersine armatura nededama')
      GO TO 180
  164 IF(NV.EQ.NA) WRITE(6,165) AKL(KV),DV,SV
  165 FORMAT(1H ,6X,'- Virsutine armatura ',A4,' klases D=',
     *I2,' mm po 1 strypa.'/1H ,10X,'Centro atstumas nuo elemento ',
     *'virsaus yv=',I2,' mm')
      IF(NV.GT.NA) WRITE(6,166) AKL(KV),DV,NL,SV
  166 FORMAT(1H ,6X,'- Virsutine armatura ',A4,' klases D=',
     *I2,' mm po 1 strypa kiekvienam'/1H ,10X,'karkasui ',
     *'plius',I3,' str. sijos lentynoje (atstumas yv=',I2,' mm)')
      IF(DD.NE.0.AND.DS.GT.DD) GO TO 172
      WRITE(6,167) AKL(KS),DS,SK3
  167 FORMAT(1H ,6X,'- Skersine armatura ',A4,' klases, D=',I2,
     *' mm. Strypai(',I3,' vnt) isde-')
      IF(UP.GT.U1) GO TO 157
      IF(U2.EQ.U1) WRITE(6,170) U1
  170 FORMAT(1H ,10X,'stomi kas',F5.1,' cm')
      IF(U2.NE.U1) WRITE(6,168) SS2,U2,U1
  168 FORMAT(1H ,10X,'stomi viduryje (l=',I3,' cm) -',
     *' kas',F5.1,' cm, galuose - kas',F5.1,' cm')
      GO TO 180
  157 IF(U2.NE.UP) WRITE(6,169) SS2,U2,LZ,U1,UP
  169 FORMAT(1H ,10X,'stomi viduryje (l=',I3,' cm) -',
     *' kas',F5.1,' cm, atraminese'/1H ,10X,'(l=',I3,' cm) zonose - ',
     *'kas',F5.1,' ir likusioje dalyje - kas',F5.1,' cm)')
      IF(U2.EQ.UP) WRITE(6,171) LZ,U1,UP
  171 FORMAT(1H ,10X,'stomi atraminese (l=',I3,' cm) zonose - kas',
     *F5.1,' cm ir likusio-'/1H ,10X,'je dalyje - kas',F5.1,' cm)')
      GO TO 180
  172 WRITE(6,173) AKL(KS),LZ,DS,SKZ,DD,SK3
  173 FORMAT(1H ,6X,'- Skersine armatura ',A4,' klases: atraminese (l=',
     *I3,' cm) zonose'/1H ,8X,'d=',I2,' mm (',I2,' vnt), o likusioje ',
     *'dalyje d=',I2,' mm (',I2,' vnt).'/1H ,8X,'Strypai isdestomi:')
      IF(U2.NE.U1.AND.UP.EQ.U1) WRITE(6,174) SS2,U2,U1
  174 FORMAT(1H ,10X,'- viduryje (l=',I3,' cm) - kas',F5.1,' cm, ga',
     *'luose - kas',F5.1,' cm')
      IF(U2.NE.UP.AND.UP.NE.U1) WRITE(6,175) SS2,U2,U1,UP
  175 FORMAT(1H ,9X,'- viduryje (l=',I3,' cm) - kas',F5.1,' cm, atram',
     *'inese zonose - kas'/1H ,9X,F5.1,' cm ir likusioje dalyje ',
     *'- kas',F5.1,' cm')
      IF(U2.EQ.U1.AND.UP.EQ.U1) WRITE(6,176) U1
  176 FORMAT(1H ,10X,'- visame ilgyje - kas',F5.1,' cm')
      IF(U2.EQ.UP.AND.UP.NE.U1) WRITE(6,177) U1,UP
  177 FORMAT(1H ,10X,'- atraminese zonose - kas',F5.1,' cm, o likus',
     *'ioje dalyje - kas',F5.1,' cm')
  180 IF(IS.NE.0) VK=(SH-2)*(SK3+SKZ)*NA*VS(IS)/100
      VV=VT+VG+VK
      L1=AP1*1000
      L3=AP3*1000
      WRITE(6,182) L1,L3,RO
  182 FORMAT(1H ,5X,'Normaliniu plysiu plotis (mm): A1=0.',I3,
     *', A2=0.',I3,'. Ilinkis ',F5.2,' cm')
      IF(GZ.NE.0.) WRITE(6,184) VT,VG,VK,VV
  184 FORMAT(1H ,5X,'Armaturos kiekis(kg): apat.=',F7.2,
     *', virs.=',F7.2,', skers.=',F7.2/1H ,27X,
     *'bendras =',F7.2/1H ,5X,70('='))
      IF(GZ.EQ.0.) WRITE(6,186) VT
  186 FORMAT(1H ,6X,'Armaturos kiekis -',F7.2,' kg'/1H ,5X,70('='))
  150 IF(NR.EQ.0) GO TO 100
      IF(NR.EQ.1) WRITE(6,188)
  188 FORMAT(5X,'Duotomis salygomis neimanomas isilginis armavimas')
      IF(NR.EQ.2) WRITE(6,190)
  190 FORMAT(5X,'Duotomis salygomis neimanomas skersinis armavimas')
      IF(NR.EQ.3) WRITE(6,192)
  192 FORMAT(5X,'Duotomis salygomis neleistinas plysiu plotis')
      IF(NR.EQ.4) WRITE(6,194)
  194 FORMAT(5X,'Duotomis salygomis neleistinas ilinkis')
      IF(NR.EQ.5) WRITE(6,195)
  195 FORMAT(5X,'Atramine zona neatlaiko lenkimo momento')
      WRITE(6,196)
  196 FORMAT(5X,'Padidinkite medziagu klase ir(arba) skerspjuvius')
  100 STOP
      END
      SUBROUTINE SPAU(ILG,SH,SB,LH,LB,LA,P,PT,PI,BKL,IP,IG,
     *KT,GM,KP,KG,DA1,DA2,LIL,AKL,KGN,RZ)
      IMPLICIT INTEGER (D,S)
      DIMENSION BKL(9),AKL(9)
      IF(LH.EQ.0) WRITE(6,3)
    3 FORMAT(/1H ,12X,'GELZBETONINES STACIAKAMPIO SKERSPJUVIO ',
     *'SIJOS SKAICIAVIMAS'/1H ,25X,'(Programa "GBSIJOS")')
      IF(LH.NE.0) WRITE(6,6)
    6 FORMAT(/1H ,12X,'GELZBETONINES TEJINIO SKERSPJUVIO SIJOS ',
     *'SKAICIAVIMAS'/1H ,25X,'(Programa "SIJA")')
      WRITE(6,9) ILG,LA
    9 FORMAT(/1H ,35X,'UZDUODAMA:'/1H ,5X,'Sijos konstrukci',
     *'nis ilgis ',I4,' cm, atremimo ilgis Latr=',I3,' cm')
      WRITE(6,12) P,PT,PI
   12 FORMAT(1H ,5X,'Naudinga skaiciuojamoji apkrova ',F6.2,
     *' kN/m, naudingos normatyvi-'/1H ,10X,'nes trumpalaike ir ',
     *'ilgalaike apkrovos -',F7.2,' ir',F7.2,' kN/m')
      IF(LH.NE.0) WRITE(6,15) SH,SB,LH,LB
   15 FORMAT(1H ,5X,'Pradinis skerspjuvis (cm): H=',I3,'  B=',
     *I3,'  HL=',I2,'  BL=',I3)
      IF(LH.EQ.0) WRITE(6,18) SH,SB
   18 FORMAT(1H ,5X,'Pradinis skerspjuvis (cm): H=',I3,'  B=',I3)
      IF(KT.EQ.0) WRITE(6,51) BKL(IP),BKL(IG),GM
   51 FORMAT(1H ,5X,'Betonas nuo B',A4,'iki B',A4,'klases ',
     *'imtinai, kietinamas naturaliomis'/1H ,10X,'salygomis. ',
     *'Darbo salygu koeficientas g=',F4.2)
      IF(KT.EQ.1) WRITE(6,54) BKL(IP),BKL(IG),GM
   54 FORMAT(1H ,5X,'Betonas nuo B',A4,'iki B',A4,'klases ',
     *'imtinai, kietinamas sutinant'/1H ,10X,'Darbo salygu koef',
     *'icientas g=',F4.2)
      WRITE(6,57) AKL(KP),AKL(KG),DA1,DA2
   57 FORMAT(1H ,5X,'Apatine armatura - nuo ',A4,' iki ',A4,
     *' klases nuo D=',I2,' iki D=',I2,' mm')
      IF(KGN.EQ.0) WRITE(6,61)
   61 FORMAT(1H ,5X,'Gniuzdomos ir skersines armaturos klase - bet',
     *' kuri is BpI, AI,'/1H ,7X,'AII ir AIII')
      IF(KGN.NE.0) WRITE(6,62) AKL(KGN)
   62 FORMAT(1H ,5X,'Gniuzdomos zonos armatura ',A4,' klases, skers',
     *'ine - bet kuri is BpI,'/1H ,7X,'AI, AII ir AIII')
      IF(LIL.EQ.0) WRITE(6,72)
   72 FORMAT(1H ,5X,'Leistinas ilinkis - pagal normas')
      IF(LIL.NE.0) WRITE(6,75) LIL
   75 FORMAT(1H ,5X,'Leistinas ilinkis -',I3,' cm')
      IF(RZ.GE.1.) GO TO 59
      IZ=RZ*100
      WRITE(6,58) IZ
   58 FORMAT(1H ,5X,'Rizikos laipsnis (minimalus parinkto ir rei',
     *'kiamo armaturos'/1H ,7X,'kiekio santykis, minimalus leidziamo ',
     *'ir paskaiciuoto ilinkio'/1H ,7X,'santykis ir pan.) - 0.',I2)
   59 WRITE(6,78)
   78 FORMAT(/1H ,28X,'SKAICIAVIMO REZULTATAI:')
      RETURN
      END
      SUBROUTINE RIBI(KA,AT,D,GM,RB,XR,AR)
      INTEGER D,ST
      XR=AT/(0.07854*D*D)+0.006
      ST=XR*100
      IF(KA.EQ.2.OR.KA.GT.6) ST=ST+400
      XR=ST/400.0
      IF(GM.LT.1.) XR=XR*0.8
      OM=0.85-0.008*RB
      XR=(1-OM/1.1)*XR+1
      XR=OM/XR
      AR=(1-0.5*XR)*XR
      RETURN
      END
      SUBROUTINE STIP(IE,ATA,DM,HO,NA,KV,NV,JV,IV,MXN,ATR,AA,VA,AO)
      IMPLICIT INTEGER (D,S)
      DIMENSION DM(17),ATA(9,17)
      COMMON /C1/ SB,SH,LB,LH,MNA,BL,RZ
      COMMON /C2/ RB,RT,NL,AT,WS,AR,DG,KGN
      COMMON /C3/ D1,D2,D3,D4,D5,Y1,Y2,Y3,Y4,Y5,DV,YV,SZ
      DVP=D1/3
      IF(DVP.LT.3) DVP=3
      IF(D1.GT.20) DVP=8
      IF(D1.GT.32) DVP=10
      IP=DVP-2
      IF(DVP.GT.6) IP=(DVP+2)/2
      A1=D1*D1*0.01
      S=MNA
      IF(D1.GT.MNA) S=D1
      L=MNA+25
      IF(D1.LE.10) L=MNA+20
      IF(D1.GT.MNA) L=D1*2
      IF(DV.NE.0) LV=MNA+30
      IF(DV.NE.0.AND.DV.GT.MNA) LV=DV*2
      IF(LV.GT.L) L=LV
      IF(IE.GT.2.AND.L.LT.50) L=50
      MXN=(SB*10-2*S-D1)/L
      Y=S+0.5*D1
      S=Y/5
      IF(Y.GT.5*S) S=S+1
      A=0.5*S
      Y1=A
      AS=A1
      IF(IE.EQ.1) GO TO 3
      A2=D2*D2*0.01
      AS=A1+A2
      E=MNA
      IF(D2.GT.MNA) E=D2
      Y2=(D1+D2)*0.05+Y1
      IF(D1.GT.6) GO TO 1
      Y=(D1+D2)*0.5+E
      S=Y/5
      IF(Y.GT.5*S) S=S+1
      Y2=0.5*S+Y1
    1 A=(A1*Y1+A2*Y2)/AS
      IF(IE.EQ.2) GO TO 3
      E=MNA
      IF(D3.GT.MNA) E=D3
      A3=D3*D3*0.01
      AS=AS+A3
      Y=(D1+D3)*0.5+E+D2
      IF(D1.LT.8) Y=(D2+D3)*0.5+E
      S=Y/5
      IF(Y.GT.5*S) S=S+1
      Y3=0.5*S+Y1
      IF(D1.LT.8) Y3=0.5*S+Y2
      A=((A1+A2)*A+A3*Y3)/AS
      IF(IE.EQ.3) GO TO 3
      E=MNA
      IF(D4.GT.MNA) E=D4
      A4=D4*D4*0.01
      AS=AS+A4
      Y=(D3+D4)*0.5+E
      S=Y/5
      IF(Y.GT.5*S) S=S+1
      Y4=0.5*S+Y3
      A=((A1+A2+A3)*A+A4*Y4)/AS
      IF(IE.EQ.4) GO TO 3
      E=MNA
      IF(D5.GT.MNA) E=D5
      A5=D5*D5*0.01
      AS=AS+A5
      Y=(D4+D5)*0.5+E
      S=Y/5
      IF(Y.GT.5*S) S=S+1
      Y5=0.5*S+Y4
      A=((A1+A2+A3+A4)*A+A5*Y5)/AS
    3 HO=SH-A
      W=WS*100
      WO=(HO-0.5*LH)*RB*BL*LH*0.1
      WL=(HO-0.5*LH)*(BL-SB)*LH*0.1*RB
      AO=(W-WL)/(0.1*RB*SB*HO*HO)
      IF(WO.GE.W) AO=W/(0.1*RB*BL*HO*HO)
      I1=IP
      I2=JV
      IF(I2.LT.I1) I2=I1
      AA=0.0
      AV=0.0
      DV=0
      IF(KGN.NE.2) GO TO 4
      PRINT *,'Armatura BpII gn.zonoj nenumatyta. Ivesk',
     *'te 0 ir duomenu faile pataisykite'
      READ *,N
      U=I1/N
    4 DO 12 IV=I1,I2
      IF(SZ.EQ.0.AND.AO.LE.AR) GO TO 7
      IF(KGN.EQ.0) GO TO 5
      KV=KGN
      AV=ATA(KV,IV)
      IF(AV.NE.0.) GO TO 6
      GO TO 12
    5 K1=1
      IF(IV.GT.2) K1=3
      DO 9 K=K1,5
      KV=K
      IF(K.EQ.2) GO TO 9
      AV=ATA(KV,IV)
      IF(AV.NE.0.) GO TO 6
    9 CONTINUE
    6 DV=DM(IV)
      Y=MNA+0.5*DV
      IF(DV.GT.MNA) Y=1.5*DV
      S=Y/5
      IF(Y.GT.S*5) S=S+1
      YV=0.5*S
    7 DO 8 NA=1,MXN
      NV=NA+NL
      VA=NV*AV
      V=(HO-YV)*VA
      WK=WO+V
      AO=(W-WL-V)/(0.1*RB*SB*HO*HO)
      IF(WK.GE.W) AO=(W-V)/(0.1*RB*BL*HO*HO)
      IF(AO.GT.AR) GO TO 8
      XO=1-(1-2*AO)**0.5
      ATR=(XO*SB*HO+(BL-SB)*LH)*RB*0.1+VA
      IF(WK.GE.W) ATR=XO*BL*HO*RB*0.1+VA
      AA=AT*NA
      IF(AA/ATR.GE.RZ) GO TO 15
    8 CONTINUE
      IF(SZ.EQ.0.AND.AO.LE.AR) GO TO 15
   12 CONTINUE
   15 RETURN
      END
      SUBROUTINE NUTR(ILG,ANG,RB,SH,SB,LH,BL,PS,ST,NA,X,Y,YV,VA,LB,
     *KT,IB,RTN,PLM,D,L,Z,XL,RDI)
      IMPLICIT INTEGER (D,S)
      COMMON /UT/ EA,FV,EV,RV,RU,WT,U1,U2,SS1,SS2,FA
      FA=X*0.007854*NA
      RA=(EA-1)*FA
      RDA=RA+RV+(LB-SB)*LH+SB*SH
      RDS=Y*RA+(SH-YV)*RV
      RDS=(SH-0.5*LH)*(LB-SB)*LH+0.5*SB*SH*SH+RDS
      YB=RDS/RDA
      YBV=SH-YB
      YA=YB-Y
      YAV=YBV-YV
      RDI=YA*YA*RA+YAV*YAV*RV+SB*SH*SH*SH/12
      RDI=(YB-YBV)*(YB-YBV)*SB*SH/4+(LB-SB)*LH*LH*LH/12+RDI
      RDI=(YBV-0.5*LH)*(YBV-0.5*LH)*(LB-SB)*LH+RDI
      WT=(LH**3/3.0+(YBV-LH)*YBV*LH)*(LB-SB)+YBV**3*SB/3
      IF(YBV.LE.LH*1.) WT=YBV*YBV*YBV*LB/3
      WT=(YA*YA*RA+YAV*YAV*RV+WT)*2/YB+YB*YB*SB*0.5
      S8=35
      IF(KT.EQ.0) S8=40
      IF(IB.LT.9) GO TO 4
      S8=40
      IF(KT.EQ.0) S8=50
    4 BR=RDI/(YB*RDA)
      RU=(FA*YA-FV*YAV+(FA+FV)*BR)*S8
      PLM=RTN*WT-RU
      H=SH-Y
      AT=(H-YV)*VA/100
      AA=ST*FA/10
      G=(AA-VA)/(0.1*RB*BL)
      IF(G.GT.LH) GO TO 1
      AT=AT+(H-0.5*G)*G*RB*BL/1000
      GO TO 2
    1 G=(AA-VA)/(0.1*RB*SB)-(BL-SB)*LH/SB
      AT=AT+(H-0.5*G)*G*RB*SB/1000
      AT=AT+(BL-SB)*(H-0.5*LH)*RB*LH/1000
    2 XL=ANG*ANG-0.008*PLM/PS
      IF(XL.LT.0.) XL=0.0
      XL=(ANG-SQRT(XL))*50
      IF(D.EQ.0) GO TO 10
      Z=ANG*ANG-8*AT/PS
      IF(Z.LT.0.) Z=0.0
      Z=(ANG-Z**0.5)*50-2*D
      LL=ANG*100-2*Z+4
      IF(U2.EQ.0.) GO TO 3
      IF(LL.GT.SS2) GO TO 5
      L2=1+LL/U2
      L=U2*L2
      GO TO 3
    5 L2=1+(LL-SS2)/(2*U1)
      L=U1*L2*2+SS2
      IF(LL.GT.L) L=L+U1
    3 IF(L.EQ.0) L=LL-4
      L=(L+4)*10
      IF(L.GT.950*ANG) L=ILG*10-20
      Z=(ILG-0.1*L)/2
   10 RETURN
      END
