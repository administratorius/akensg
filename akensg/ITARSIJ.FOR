      IMPLICIT INTEGER (D,S)
      DIMENSION RBM(6),RTM(6),RBN(6),RTN(6),EBM(6),EBS(6),
     *RAM(4),RIM(4),EIM(4),ASKM(9,4),MES(4),AKL(6),APKL(4)
      COMMON BL,SB,HF,A3,HO,LH,T2,E2,RBS,UM,U1,U2,AL,EI,YI,RTS,WPA,
     *SH,DI,RJ,AS,RAI,RA,AI,AA,EA,EV,EB
      DATA RBM /0.85,1.15,1.45,1.7,1.95,2.2/
      DATA RTM /0.075,0.09,0.105,0.12,0.13,0.14/
      DATA RBN /1.1,1.5,1.85,2.2,2.55,2.9/
      DATA RTN /0.115,0.14,0.16,0.18,0.195,0.21/
      DATA EBM /2.3,2.7,3.0,3.25,3.45,3.6/
      DATA EBS /2.05,2.4,2.7,2.9,3.1,3.25/
      DATA RAM /45.0,51.0,68.0,81.5/
      DATA RIM /54.0,59.0,78.5,98.0/
      DATA EIM /18.0,19.0,19.0,19.0/
      DATA ASKM /1.909,3.33,5.105,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
     *0.0,0.0,4.948,8.796,13.744,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
     *0.0,17.671,25.447,34.636,45.239,0.0,0.0,0.0,8.058,14.326,
     *22.777,32.798,44.642,58.308/
      DATA MES /17,21,21,20/
      OPEN(UNIT=5,FILE='ITARSIJ.DAT')
      OPEN(UNIT=6,FILE='ITARSIJ.RES')
C
      PRINT *,'PROGRAMA JAU PASENUSI! IVESKITE BET KOKIA RAIDE'
	READ *,A
C
	READ(5,*) ANG,LAT,SB,SH,LB,LH,ITIP,FLE
      READ(5,*) Q,QIN,QTN
      READ(5,*) KB1,KB2,GB,KIET,RBPR,SAL,DREG
      READ(5,*) I1,I2,D1,D2,S5,S2,NE,NS,SMN,ISA,ITA
      READ(5,*) KAP,KVI,DA,DV,NN,SA,SV
      READ(5,*) KS1,KS2,DS1,DS2
      READ(5,1) AKL,APKL
    1 FORMAT(10A4)
      IF(ITIP.EQ.1.AND.LH.EQ.0) WRITE(6,2)
    2 FORMAT(1H ,5X,'STACIAKAMPIO SKERSPJUVIO ITEMPTAI ARMUOTOS GELZ',
     *'BETONINES'/1H ,5X,'SIJOS SKAICIAVIMAS (Programa ',
     *'"ITARSIJA"-Kensgaila)')
      IF(ITIP.EQ.1.AND.LH.NE.0) WRITE(6,3)
    3 FORMAT(1H ,5X,'TEJINIO SKERSPJUVIO ITEMPTAI ARMUOTOS GELZ',
     *'BETONINES'/1H ,10X,'SIJOS SKAICIAVIMAS'/1H ,8X,'Programa ',
     *'"ITARSIJA"-Kensgaila')
      IF(ITIP.EQ.2) WRITE(6,4)
    4 FORMAT(1H ,5X,'ITEMPTAI ARMUOTOS GELZBETONINES PLOKSTES SU BR',
     *'IAUNOMIS'/1H ,5X,'KRASTUOSE SKAICIAVIMAS (Programa ',
     *'"ITARSIJA"-Kensgaila)')
      IF(ITIP.EQ.3) WRITE(6,5)
    5 FORMAT(1H ,5X,'"TT" FORMOS SKERSPJUVIO ITEMPTAI ARMUOTOS GELZ',
     *'BETONINES PLOKSTES'/1H ,10X,'SKAICIAVIMAS (Programa ',
     *'"ITARSIJA"-Kensgaila)')
      IF(LH.EQ.0) WRITE(6,6) ANG,LAT,SB,SH
    6 FORMAT(1H ,13X,'PRADINIAI DUOMENYS:'/1H ,3X,'Sijos l=',F5.2,
     *' m, atremimo ilgis =',I2,' cm'/1H ,5X,'Skerspjuvio b=',I3,
     *' ir h=',I4,' cm'/1H ,3X,'Apkrovos (kN/m, neiskaitant sijos',
     *' svorio):')
      IF(LH.NE.0.AND.ITIP.EQ.1) WRITE(6,7) ANG,LAT,SB,SH,LB,LH
    7 FORMAT(1H ,13X,'PRADINIAI DUOMENYS:'/1H ,3X,'Sijos l=',F5.2,
     *' m, atremimo ilgis =',I2,' cm'/1H ,5X,'Skerspjuvio b=',I3,
     *', h=',I4,', bl=',I4,' ir hl=',I3,' cm'/1H ,3X,'Apkrovos ',
     *'(kN/m, neiskaitant sijos svorio):')
      IF(ITIP.GT.1) WRITE(6,8) ANG,LAT,SB,SH,LB,LH
    8 FORMAT(1H ,13X,'PRADINIAI DUOMENYS:'/1H ,3X,'Sijos l=',F5.2,
     *' m, atremimo ilgis =',I2,' cm'/1H ,5X,'Skerspjuvio (vienos',
     *' briaunos b=',I3,' cm), h=',I4,', bl=',I4,' ir hl=',I3,
     *' cm'/1H ,3X,'Apkrovos (kN/m, neiskaitant plokstes svorio):')
      WRITE(6,9) Q,QIN,QTN,KB1,KB2
    9 FORMAT(1H ,5X,'- skaiciuojamoji P=',F6.2/1H ,5X,'- normine ',
     *'ilgalaike Pilg=',F6.2/1H ,5X,'- normine trumpalaike Ptr=',
     *F6.2/1H ,3X,'Betonas nuo B',I2,' iki B',I2,' klases, kieti',
     *'namas')
      IF(KIET.EQ.0) WRITE(6,10) RBPR
   10 FORMAT(1H ,6X,'naturaliai. Santykinis stiprumas apspaudimo',
     *' metu',F4.2,' galutinio')
      IF(KIET.EQ.1) WRITE(6,11) RBPR
   11 FORMAT(1H ,6X,'sutinant. Santykinis stiprumas apspaudimo',
     *' metu',F4.2,' galutinio')
      WRITE(6,12) GB
   12 FORMAT(1H ,6X,'Betono darbo salygu koeficientas g=',F3.1)
      IF(SAL.EQ.0.AND.DREG.LT.40) WRITE(6,13)
   13 FORMAT(1H ,6X,'Konstrukcija numatyta eksploatuoti viduje,',
     *' kai dregnis < 40%')
      IF(SAL.EQ.0.AND.DREG.GE.40) WRITE(6,14)
   14 FORMAT(1H ,6X,'Konstrukcija numatyta eksploatuoti viduje,',
     *' kai dregnis > 40%')
      IF(SAL.EQ.1) WRITE(6,15)
   15 FORMAT(1H ,6X,'Konstrukcija numatyta eksploatuoti lauke')
      WRITE(6,16) AKL(I1),AKL(I2),D1,D2,NE,SMN,NS
   16 FORMAT(1H ,3X,'Itempiama armatura nuo ',A4,' iki ',A4,' kla',
     *'ses ir nuo d=',I3,' iki d=',I3,' mm'/1H ,6X,'isdestant ',
     *'strypus ne daugiau kaip',I2,' eil. bei ne maziau'/1H ,6X,
     *'kaip po',I2,' ir ne daugiau kaip po',I2,' strypus eileje')
      IF(KIET.EQ.1.AND.ITA.EQ.1) WRITE(6,17)
   17 FORMAT(1H ,3X,'Skaiciuojant butina ivertinti tai, kad arma',
     *'turos itempimo atramos'/1H ,6X,'yra uz sutinimo kameros ribu')
      IF(ISA.EQ.0) WRITE(6,18)
   18 FORMAT(1H ,3X,'Armatura betone inkaruojama be papildomu ziedu')
      IF(ISA.EQ.1) WRITE(6,19)
   19 FORMAT(1H ,3X,'Armatura betone inkaruojama su papildomais',
     *' ziedais galuose')
      IF(KAP.NE.0.AND.KVI.NE.0) WRITE(6,20) DA,APKL(KAP),DV,APKL(KVI),
     *APKL(KS1),APKL(KS2),DS1,DS2
   20 FORMAT(1H ,3X,'Neitempiama apatine armatura d',I2,A4,', virsu',
     *'tine d',I2,A4,' ir sker-'/1H ,6X,'sine nuo ',A4,' iki ',A4,
     *' klases bei nuo d=',I2,' iki d=',I2,' mm')
      ANG=ANG-0.04*LAT/3
      IF(FLE.NE.0.) GO TO 27
      FLE=ANG/2
      IF(ANG.GE.5.0.AND.ANG.LE.10.) FLE=3.0
      IF(ANG.GT.10.) FLE=ANG/4
   27 IF(LH.EQ.0) LB=SB
      CALL PAPR(KAP,DA,RA,RAS,EA)
   28 CALL PAPR(KVI,DV,RV,RVS,EV)
      NB=1
      IF(ITIP.EQ.2.OR.ITIP.EQ.3) NB=2
      SMN=SMN*NB
      AA=0.007854*DA*DA*NN*NB
      AV=0.007854*DV*DV*NN*NB
      YAP=0.1*SA
      YVI=0.1*SV
      HF=LH*1.0
      IF(LH.EQ.0) HF=0.0
      IF(LH.EQ.0.AND.DV.NE.0) HF=2*YV
   30 IF(SH.GT.45) GO TO 31
      DUG=25
      SGN=50
      SGX=SH/25
      SGX=SGX*5
      IF(SGX.GT.150) SGX=150
      GO TO 32
   31 SGN=100
      DUG=50
      SGX=SH/10
      SGX=SGX*50
      IF(SGX.GT.500) SGX=500
   32 SVN=SGN
      DUV=50
      SVX=SH*3/20
      SVX=SVX*50
      IF(SVX.GT.500) SVX=500
      IF(SH.LT.20) GO TO 33
      SVX=(SH-2)/3
      SVX=SVX*25
      IF(SVX.GT.500) SVX=500
   33 GO TO(36,39,42),ITIP
   36 BL=LB*1.0
      B=ANG*100/3+SB
      IF(B.LT.BL) BL=B
      B=LH*12.0+SB
      IF(LH*1.0.GE.SH*0.1.AND.B.LT.BL) BL=B
      B=LH*6.0+SB
      IF(LH*1.0.GE.SH*0.05.AND.LH*1.0.LT.SH*0.1.AND.B.LT.BL) BL=B
      IF(LH.EQ.0.OR.LH*1.0.LT.SH*0.05) BL=SB*1.0
      GO TO 45
   39 SB=2*SB
      BL=LB*1.0
      B=ANG*100/3+SB
      IF(B.LT.BL) BL=B
      B=LH*12.0+SB
      IF(LH*1.0.LT.SH*0.1.AND.B.LT.BL) BL=B
      GO TO 45
   42 SB=2*SB
      BL=LB*1.0
      B=ANG*100/3+SB
      IF(B.LT.BL) BL=B
      B=LH*12.0+2*SB+0.5*LB
      IF(LH*1.0.GE.SH*0.1.AND.B.LT.BL) BL=B
      B=LH*18.0+2*SB
      IF(LH*1.0.GE.SH*0.05.AND.LH*1.0.LT.SH*0.1.AND.B.LT.BL) BL=B
      B=LH*12.0+SB
      IF(LH*1.0.LT.SH*0.05.AND.B.LT.BL) BL=B
   45 QSN=(SB*SH+(LB-SB)*LH)*0.00025
      QN=QIN+QTN+QSN
      QNI=QIN+QSN
      QS=(SB*SH+(LB-SB)*LH)*0.000275+Q
      HSN=QSN*ANG*ANG/8
      HN1=QN*ANG*ANG/8
      HN2=QNI*ANG*ANG/8
      HMS=QS*ANG*ANG/8
      Q1=QS*ANG*0.5
      Q2=Q1/2
      QN1=QN*ANG/2
      IF(I2.LT.5) GO=3.0+36/(ANG+0.04*LAT/3)
   46 M3=DS1-2
      IF(DS1.GT.6) M3=(DS1+2)/2
      M4=DS2-2
      IF(DS2.GT.6) M4=(DS2+2)/2
      DO 129 I=I1,I2
      ET=1.0
      IF(I.GT.4) ET=1.2
      ETA=1.0
      IF(I.EQ.2) ETA=1.2
      IF(I.EQ.3.OR.I.GT.4) ETA=1.15
      IF(I.EQ.4) ETA=1.1
      IF(I.GT.4) GO TO 49
      RAI=RAM(I)
      RI=RIM(I)
      EI=EIM(I)
      G1=0.3*RI+GO
      G2=RI-GO
      G=(G2-G1)/10
      PL1=0.4
      PL2=0.3
      IF(I.LT.3) GO TO 48
      PL1=0.3
      PL2=0.2
      IF(SAL.EQ.0) GO TO 48
      PL1=0.2
      PL2=0.1
   48 IF(I.EQ.1.AND.D1.LT.20) D1=20
      IF(I.EQ.2.AND.D2.GT.22) D2=22
      IF(I.GT.2.AND.D2.GT.28) D2=28
      IF(D1.GT.D2) D1=D2
      J1=(D1-8)/2
      IF(D1.GT.22) J1=(D1-1)/3
      IF(D1.EQ.40) J1=12
      J2=(D2-8)/2
      IF(D2.GT.22) J2=(D2-1)/3
      IF(D2.EQ.40) J2=12
      GO TO 53
   49 J1=1
      J2=3
      ET=1.2
   53 DO 126 J=J1,J2
      IF(I.LT.5) GO TO 54
      IF(I.EQ.6) GO TO 305
      EI=200.0
      GO TO(301,302,303),J
  301 RAI=121.5
      RI=146.0
      PL1=0.2
      PL2=0.1
      IF(SAL.EQ.0) GO TO 304
      PL1=0.2
      PL2=0.2
      GO TO 304
  302 RAI=114.5
      RI=137.0
      PL1=0.3
      PL2=0.2
      IF(SAL.EQ.0) GO TO 304
      PL1=0.2
      PL2=0.1
      GO TO 304
  303 RAI=104.5
      RI=125.5
      PL1=0.3
      PL2=0.2
      IF(SAL.EQ.0) GO TO 304
      PL1=0.2
      PL2=0.1
  304 G1=0.315*RI
      GO=0.05*RI
      G2=RI*0.95
      G=(G2-G1)/10
      DI=J+2
      GO TO 51
  305 EI=180.0
      GO TO(306,307,308),J
  306 RAI=114.5
      RI=137.0
      PL1=0.2
      PL2=0.1
      IF(SAL.EQ.0) GO TO 309
      PL1=0.2
      PL2=0.2
      GO TO 309
  307 RAI=111.0
      RI=135.0
      PL1=0.3
      PL2=0.2
      IF(SAL.EQ.0) GO TO 309
      PL1=0.2
      PL2=0.1
      GO TO 309
  308 RAI=104.5
      RI=108.0
      PL1=0.3
      PL2=0.2
      IF(SAL.EQ.0) GO TO 309
      PL1=0.2
      PL2=0.1
  309 G1=0.315*RI
      GO=0.05*RI
      G2=RI*0.95
      G=(G2-G1)/10
      DI=(J+2)*3
      GO TO 51
   54 DI=(J+4)*2
      IF(J.LT.8) GO TO 51
      DI=J*3+1
      IF(J.LT.10) GO TO 51
      DI=J*4-8
   51 AI=0.007854*DI*DI
      IF(ISA.EQ.1) GO TO 56
      IF(I.EQ.2.AND.DI.GT.18.AND.KB1.LT.20) KB1=20
      IF((I.EQ.3.OR.I.EQ.5).AND.KB1.LT.20) KB1=20
      IF(I.EQ.3.AND.DI.GT.18.AND.KB1.LT.25) KB1=25
      IF((I.EQ.4.OR.I.EQ.6).AND.KB1.LT.30) KB1=30
   56 IF(KB2.LT.KB1) KB2=KB1
      Y1=1.5
      IF(SH.GT.10.AND.SH.LE.25) Y1=2.0
      IF(SH.GT.25) Y1=2.5
      IF(DI.LT.12) GO TO 52
      D=(0.5*DI+4)/5+(Y1-0.5)*2
      IF(DI*0.1.GT.(Y1-0.5)) D=(1.5*DI+4)/5
      Y1=D*0.5
   52 IF(S5*0.1.GT.Y1) Y1=S5*0.1
      Y2=S2*0.1
      IF(S2.EQ.0) Y2=Y1+1.0
      S1=Y1*10
      S2=Y2*10
      Y3=0.0
      S=0
      DO 123 SE=1,NE
      Y=(SE-1)*Y2+Y1
      IF(SE.GT.1) Y3=(SE-2)*Y2+Y3
      DO 120 SS=1,NS
      S=S+NB
      IF(S.LT.SMN) GO TO 120
      YIT=(((SE-1)*Y1+Y3)*NS+SS*Y)*NB/S
      AS=AI*S+AA
      YS=(AI*S*YIT+AA*YAP)/AS
      RS=(AI*S*RAI+AA*RA)/AS
      HO=SH-YS
      IF(I.EQ.6) AI=AI*1.03
   55 DO 117 KB=KB1,KB2,5
      K=(KB-10)/5
      RB=RBM(K)*GB
      RT=RTM(K)*GB
      RBS=RBN(K)
      RTS=RTN(K)
      EB=EBM(K)
      IF(KIET.EQ.1) EB=EBS(K)
      RPN=0.05*KB
      RPX=RBS*RBPR
      IF(RPN.LT.1.1) RPN=1.1
      IF(I.GT.3.AND.RPN.LT.1.55) RPN=1.55
      IF(RPN.GT.RPX) RPN=RPX
      ALI=EI/EB
      ALA=EA/EB
      ALV=EV/EB
      A1=(ALI-1)*AI*S
      A2=(ALA-1)*AA
      A3=(ALV-1)*AV
      YBA=0.5*SB*SH*SH+(SH-0.5*LH)*(LB-SB)*LH
      YBA=A1*YI+A2*YA+(SH-YV)*A3+YBA
      REA=SB*SH+(LB-SB)*LH+A1+A2+A3
      YBA=YBA/REA
      YBV=SH-YBA
      YI=YBA-YIT
      YA=YBA-YAP
      YV=YBV-YVI
      U1=YBA-YS
      U2=YBA-Y1
      V1=0.5*SH-YBA
      V2=YBV-0.5*LH
      REI=(SB*SH**3+(LB-SB)*LH**3)/12.0
      REI=SB*SH*V1*V1+(LB-SB)*LH*V2*V2+REI
      REI=A1*YI*YI+A2*YA*YA+A3*YV*YV+REI
      WA=REI/YBA
      WV=REI/YBV
      WPA=WA*1.75
      WPV=WV*1.75
      L=1
   78 GIO=(L-1)*G+G1
      IF(GIO.GT.G2) GO TO 120
      P1=0.03*GIO
      IF(I.GT.4) P1=(0.22*GIO/RI-0.1)*0.1*GIO
      P2=0.0
      IF(KIET.EQ.1.AND.ITA.EQ.1) P2=8.5
      P3=0.0
      IF(I.LT.5) GO TO 59
      V=1.25+0.15*DI
      P3=V*EI/(ANG+0.04*LAT/3)
   59 P5=0.0
      IF(I.GT.4) P5=3.0
      GI1=GIO-P1-P2-P3-P5
      T1=GI1*AI*S
      GIB=T1/REA+(T1*YI-100*HSN)*YI/REI
      IF(GIB.GT.RPX) GO TO 58
      DO 57 SM=1,50
      RPN=RPN+(SM-1)*0.01
      IF(RPN.GT.RPX) GO TO 58
      RBP=RPN/RBS
      V=GIB/RPN
      IF(V.GT.1.) GO TO 57
      V1=0.25*RPN+0.25
      IF(V1.GT.0.8) V1=0.8
      V2=5.25-1.85*RPN
      IF(V2.LT.1.1) V2=1.1
      IF(V2.GT.2.5) V2=2.5
      P6=V*4
      IF(V.GT.V1) P6=(V-V1)*V2*8.5+4*V1
      IF(KIET.EQ.1) P6=0.85*P6
      GVB=T1/REA-(T1*YI-100*HSN)*YV/REI
      GI1=GI1-P6
      GA1=P6*YA/YI
      GV1=P6
      IF(GVB.LT.0.) GV1=0.0
      T1=GI1*AI*S-GA1*AA-GV1*AV
      E1=(GI1*AI*S*YI-GA1*AA*YA+GV1*AV*YV)/T1
      GBA=T1/REA+(T1*E1-100*HSN)*YBA/REI
      IF(GBA.GT.RPN) GO TO 57
      BD=0.0
      FI=1.6-GBA/RPN
      IF(FI.LT.0.7) FI=0.7
      IF(FI.GT.1.) FI=1.0
      R=WV*FI/REA
      HRV=(E1-R)*T1-100*HSN
      HCV=RTS*RBP*WPV
      PLV=0.0
      IF(HCV.GE.HRV) GO TO 60
      HZ=(E1+YV)*T1+100*HSN
      VH=YBA+YV
      TD=HZ/(SB*VH*VH*RBS*RBP)
      FI=A3/(0.9*SB*VH)
      V1=YBA-YS
      BDA=(1-V1/VH)*FI
      E=HZ/T1
      UM=AV*ALV/(SB*VH)
      XI=10*UM/(18*UM+(TD+BDA)*5+1)
      XI=(1.5+FI)/(11.5*E/VH-5)+XI
      IF(XI.GT.1) XI=1.0
      Z=(V1*2*FI/VH+XI*XI)/((FI+XI)*2)
      Z=(1-Z)*VH
      GM=((E1+YV-Z)*T1-100*HSN)/(AV*Z)
      IF(GM.GT.RVS) GO TO 57
      PLV=(0.07-2*UM)*GM*DV**(1.0/3)/EV
      IF(PLV.GT.PL1) GO TO 57
      FM=RTS*RPN*WPV/((E1-R)*T1+100*HSN)
      IF(FM.GT.1.) FM=1.0
      HPL=SH-(1.2+FM)*XI*VH
      IF(HPL.LT.0.) HPL=0.0
      IF(HPL.GT.SH/2) GO TO 57
      FM=RTS*RPN*WPV/((E1-R)*T1-100*HSN)
      IF(FM.LT.0.45) FM=0.45
      IF(FM.GT.1.) FM=1.0
      TD=AS*YBA/((AS+AV)*YBV)
      IF(TD.GT.1.4) TD=1.4
      IF(I.EQ.4) TD=TD*0.85
      BD=(1.5-0.9/TD)*(1-FM)
      IF(BD.LT.0.) BD=0.0
      NNEP=0
      GO TO 60
   57 CONTINUE
   58 IF(KB.LT.KB2) GO TO 117
      NNEP=1
      PRINT *,'Netenkinami reikalavimai apspaudimo metu.',KB,KB2
      IF((KV5.EQ.0.AND.D5.EQ.0).OR.(KVI.EQ.4.AND.DV.EQ.16)) GO TO 61
      PRINT *,'Jei galite, padidinkite virs.ar-ros klase ir(arba) sker',
     *'smeni: pav. 3 12 (max 4 ir 16). Jei negalite, rasykite "0 0"'
      READ *, KV5,D5
      IF(KV5.NE.0) KVI=KV5
      IF(D5.NE.0) DV=D5
      IF(KV5.NE.0.OR.D5.NE.0) GO TO 28
      NNEP=1
   61 IF(KB.EQ.40) GO TO 132
      PRINT *,'Ar leidziate imti aukstesne betono ',
     *'klase? Jei taip, rasykite pav. 35 (max 40), jei ne - 0'
      READ *,KB3
      NNEP=1
      IF(KB3.EQ.0) GO TO 132
      KB2=KB3
      NNEP=0
      GO TO 55
   60 P8=4.0
      IF(KIET.EQ.1) P8=3.5
      IF(KB.LT.40) GO TO 63
      P8=5.0
      IF(KIET.EQ.1) P8=4.0
   63 GIB=T1/REA+(T1*E1-100*HSN)*YI/REI
      GVB=T1/REA-(T1*E1-100*HSN)*YV/REI
      GV2=P8
      IF(GVB.LT.0.) GV2=0.0
      V=GIB/RPN
      V1=1.0
      IF(KIET.EQ.1) V1=0.85
      P9=15*V1*V
      IF(V.GT.0.75) P9=(V-0.375)*30*V1
      GV3=P9
      IF(GVB.LT.0.) GV3=0.0
      V=P1+P2+P3+P5+P6+P8+P9
      IF(V.LT.10.) V=10.0
      GI2=GIO-V
      GSP=(1.0/(S**0.5)+1)*GO*0.5/GI2
      IF(GSP.LT.0.1) GSP=0.1
      GI2=(1-GSP)*GI2
      GA2=(P6+P8+P9)*YA/YI
      GV2=GV1+GV2+GV3
      T2=(GI2*S*AI-GA2*AA-GV2*AV)*(1-BD)
      E2=(GI2*AI*S*YI-GA2*AA*YA+GV2*AV*YV)/T2
      V3=0.2+0.01*KB
      IF(V3.LT.0.5) V3=0.5
      ZIN=((GI1+P6)*0.25/RPN+10)*DI*0.1
      WST=0.5*SB*YBV*YBV+(YBV-0.5*LH)*(LB-SB)*LH+A3*YV
      TAU=QN1*WST/(SB*REI)
      BE=YBA/SH
      Z1=LAT/3.0
      GX=T2*LAT/(REA*ZIN)
      CALL PIS(Z1,V3,SH,BE,QN1,SB,GX,TAU,RBS,RTS,ISP1)
      IF(GX.GE.0.05) ISP1=0
      Z1=ZIN-2.0*LAT/3
      GX=T2/REA
      CALL PIS(Z1,V3,SH,BE,QN1,SB,GX,TAU,RBS,RTS,ISP2)
      IF(GX.GE.0.05) ISP2=0
      ISP3=0
      IF(LH.EQ.0) GO TO 66
      Z1=Z1+2
      YL=YBV-LH
      WIS=(ZIN+2)*QN1
      GX=T2/REA+(WIS-T2*E2)*YL/REI
      V1=0.5*LB*LH*LH+(YV-YL)*A3
      TAU=QN1*V1/(SB*REI)
      CALL PIS(Z1,V3,SH,BE,QN1,SB,GX,TAU,RBS,RTS,ISP3)
      IF(GX.GE.0.05) ISP3=0
   66 INE=1
      IF(ISP1.EQ.0.AND.ISP2.EQ.0.AND.ISP3.EQ.0) INE=0
      GBV=T2/REA-(T2*E2-100*HN1)*YBV/REI
      FI=1.6-GBV/RBS
      IF(FI.LT.0.7) FI=0.7
      IF(FI.GT.1.) FI=1.0
      RJ=WA*FI/REA
      WPL=(E2+RJ)*T2+RTS*WPA
      NPL=1
      IF(0.01*WPL.LT.HN1) GO TO 72
      NU=0
   69 C2=1.0
      NPL=0
      IF(NU.EQ.1) C2=1.2
      C1=1.0
      IF(PLV.NE.0.) C1=1.15
      R1=QTN*ANG*ANG*C1*C2/(8.5*EB*REI)
      R2=HN2*C1*C2/(4.25*EB*REI)
      IF(DREG.LT.40) R2=R2*1.5
      R2=HN2*C1*C2/(8.5*EB*REI)+R2
      R3=T2*E2*C1*C2/(850*EB*REI)
      R4=(P6+P8+P9-GV1-GV2-GV3)*0.001/(HO*EI)
      IF(PLV.NE.0.) R4=R4*1.25
      V1=R3+R4
      V2=T2*E2*0.002/(0.85*EB*REI)
      IF(DREG.LT.40) V2=V2*1.5
      RIL=R1+R2-R3-R4
      IF(V2.GT.V1) RIL=R1+R2-V2
      GO TO 75
   72 UM=(AI*S+AA)/(SB*HO)
      IF(UM.GT.0.02) UM=0.02
      AL=(ALI*AI*S+ALA*AA)/(AI*S+AA)
      UN=0.45
      FL=1.0
      FLS=1.1
      IF(I.GT.4) FLS=1.0
      CALL PLYS(ET,UN,HN1,FL,FLS,GM,PL3,R1)
      V1=1.0
      IF(PLV.NE.0.) V1=(1-BD)*1.1
      IF(V1.GT.1.) V1=1.0
      GBA=T2*V1/REA+(T2*V1*E2-100*HN2)*YBA/REI
      MU=0
      IF(GBA.LE.0.05) MU=1
      GS=GI2+GM
      MG=0
      IF(GS.LE.RI*0.8) MG=1
      NU=0
      IF(MU.EQ.1.AND.MG.EQ.1) NU=1
      IF(NU.EQ.1) GO TO 69
      IF(GS.GT.RI) GO TO 117
      CALL PLYS(ET,UN,HN2,FL,FLS,GM,PL4,R2)
      UN=0.15
      IF(DREG.LT.40) UN=0.1
      FL=1.6-15*UM
      FLS=0.8
      CALL PLYS(ET,UN,HN2,FL,FLS,GM,PL5,R3)
      PLS=PL3-PL4+PL5
      IF(PLS.LE.PL1.AND.PL5.LE.PL2) GO TO 76
      L=L+1
      GO TO 78
   76 IF(R2.LT.0.) R2=0.0
      IF(R3.LT.0.) R3=0.0
      R4=(P6+P8+P9-GV1-GV2-GV3)*0.001/(HO*EI)
      IF(PLV.NE.0.) R4=R4*1.25
      RIL=R1-R2+R3-R4
   75 F=RIL*50000*ANG*ANG/48
      IF(F.LE.FLE) GO TO 81
      L=L+1
      GO TO 78
   81 GSC=40.0
      IF(GB.LT.1.) GSC=50.0
      GSR=RAI+40-GI2
      OM=0.8-0.08*RB
      XIR=OM/((1-OM/1.1)*GSR/GSC+1)
      RBL=(BL-SB)*LH*RB
      IF(I.GT.1) GO TO 87
      X=(RS*AS-RV*AV)/(RB*BL)
      XI=X/HO
C      IF(XI.GT.XIR) GO TO 96
      IF(LH.NE.0.AND.X.GT.LH*1.0) GO TO 84
      AM=(HO-0.5*X)*RB*BL*X+(YS+YV)*AV*RV
      GO TO 95
   84 X=(AS*RS-RBL-RV*AV)/(RB*SB)
      XI=X/HO
C      IF(XI.GT.XIR) GO TO 96
      AM=(HO-0.5*X)*RB*SB*X+(HO-0.5*LH)*RBL+(YS+YV)*AV*RV
      GO TO 95
   87 CN=10.0
      DO 93 K=1,26
      X1=(K-1)*0.02+0.5
      GG=ETA-(ETA-1)*(2*X1/XIR-1)
      X2=(AS*RS*GG-AV*RV)/(RB*BL)
      IF(LH.EQ.0) GO TO 90
      IF(X2.LE.LH*1.) GO TO 90
      X2=(AS*RS*GG-AV*RV-RBL)/(RB*SB)
   90 X2=X2/HO
C      IF(X2.GT.XIR) GO TO 96
      C=ABS(X1-X2)
      IF(C.GT.CN) GO TO 93
      G6=GG
      CN=C
   93 CONTINUE
      X=(AI*S*RAI*G6+AA*RA-RV*AV)/(RB*BL)
      XI=X/HO
C      IF(XI.GT.XIR) GO TO 96
      AM=(HO-0.5*X)*RB*BL*X+(YS+YV)*AV*RV
      IF(LH.EQ.0) GO TO 95
      IF(X.LE.LH*1.) GO TO 95
      X=(AI*S*RAI*G6+AA*RA-AV*RV-RBL)/(RB*SB)
      XI=X/HO
C      IF(XI.GT.XIR) GO TO 96
      AM=(HO-0.5*X)*RB*SB*X+(HO-0.5*LH)*RBL+(YS+YV)*AV*RV
   95 IF(AM.LT.HMS*100) GO TO 117
      GO TO 97
   96 IF(KB.LT.KB2) GO TO 117
      NNEP=2
      PRINT *,'XI virsija XIrib. Jei galite, stiprinkite virsutine ',
     *'armatura, pav."3 14"(max 4 16), arba "3 0", arba "0 14". Jei',
     *' ne - rasykite "0 0"'
      READ *,KVR,DVR
      IF(KVR.NE.0) KVI=KVR
      IF(DVR.NE.0) DV=DVR
      IF(DVR.NE.0.OR.KVR.NE.0) GO TO 28
      NNEP=2
      GO TO 132
   97 FB=1-0.1*RB
      QR=0.3*FB*RB*SB*HO
      IF(Q1.LE.QR) GO TO 99
      NNEP=3
      IF(KB.LT.KB2) GO TO 117
      PRINT *,'Nepakankamas betoninis skerspjuvis skers.jegos atzvilgiu'
      PRINT *,'Jei galite, didinkite betono klase, pav. 35 (max 40).',
     *' Jei ne - rasykite 0'
      READ *,KB3
      IF(KB3.EQ.0) GO TO 132
      KB2=KB3
      NNEP=0
      GO TO 55
   99 IF(SH.GT.14) GO TO 102
      P1=0.5*Q
      V=2*RT*SB*HO
      V1=1.2*RT*SB*HO*HO*P1
      V1=V1**0.5*2
      FN=T2*0.1/(RT*SB*HO)
      IF(FN.GT.0.5) FN=0.5
      V2=(1+FN)*0.6*RT*SB*HO+Q*HO
      IF(V2.GT.V1) V1=V2
      IF(V.LT.Q1.OR.V1.LT.Q1) GO TO 102
      DS=0
      GO TO 138
  102 BF=LB*1.0
      IF(BF.GT.(SB+3.0*LH)) BF=SB+3.0*LH
      FF=(BF-SB)*0.75*LH/(SB*HO)
      IF(FF.GT.0.5) FF=0.5
      FF=FF+FN+1
      IF(FF.GT.1.5) FF=1.5
      QB=0.6*FF*RT*SB*HO
      IF(QB.LT.Q1) GO TO 105
      UG=0.1*SGX
      UV=0.1*SVX
      DS=DS1
      KS=KS1
      IF(INE.EQ.0) GO TO 138
  105 M1=(SGX-SGN)/DUG+1
      V=0.3*FF*RT*SB
      DO 108 M=1,M1
      DU=(SGX-(M-1)*DUG)/DUG
      UG=DU*DUG*0.1
      DO 108 M2=M3,M4
      DS=M2+2
      IF(M2.GT.4) DS=(M2-1)*2
      ASK=0.007854*DS*DS*NN*NB
      DO 107 KS=KS1,KS2
      QW=ASKM(M2,KS)
      IF(QW.EQ.0.) GO TO 107
      IF(INE.EQ.0) GO TO 106
      ES=MES(KS)*1.0
      V1=2*ES*ASK/(EB*SB*UG)
      V1=(1+V1)*150*EB+100*DS*ES/HO
      V1=0.6*DS*UG*ET/(ASK*HO*V1)
      PLI1=QN1*V1
      PLI2=(QIN+QSN)*ANG*0.5*V1
      PLI3=PLI2*1.5
      PLI=PLI1-PLI2+PLI3
      IF(PLI.GT.PL1.OR.PLI3.GT.PL2) GO TO 107
  106 QW=QW*NN*NB/UG
      IF(QW.LT.V) GO TO 107
      QR=2*FF*RT*SB*HO*HO*QW
      QR=QR**0.5*2
      IF(QR.GE.Q1) GO TO 111
  107 CONTINUE
  108 CONTINUE
      IF(KB.LT.KB2) GO TO 117
      NNEP=4
      PRINT *,'Neimanomas skersinis armavimas. Jei galite,'
      PRINT *,'didinkite arm-ros klase ir(arba) diametra'
      PRINT *,'pvz.3 10. Jei ne - rasykite 0 0'
      READ *,KS3,DS3
      IF(KS3.EQ.0.AND.DS3.EQ.0) GO TO 132
      KS2=KS3
      DS2=DS3
      NNEP=0
      GO TO 46
  111 M1=(SVX-SVN)/DUV+1
      DO 114 M=1,M1
      DU=(SVX-(M-1)*DUV)/DUV
      UV=DU*DUV*0.1
      QW=ASKM(M2,KS)*NN*NB/UV
      IF(QW.LT.V) GO TO 114
      QR=2*FF*RT*SB*HO*HO*QW
      QR=2*QR**0.5
      IF(QR.GE.Q2) GO TO 138
  114 CONTINUE
  117 CONTINUE
  120 CONTINUE
  123 CONTINUE
  126 CONTINUE
  129 CONTINUE
  132 PRINT *, NNEP
      PRINT *,'Ar leidziate didinti sijos(briaunos) skerspjuvi, t.y. ',
     *'ploti ir(arba) auksti? Jei ne, rasykite "0 0", jei taip ',
     *'rasykite pav. "0 60", arba "20 0", arba "20 60"'
      READ *,DEB,DEH
      IF(DEB.NE.0) SB=DEB
      IF(DEH.NE.0) SH=DEH
      IF(DEB.NE.0.OR.DEH.NE.0) GO TO 30
      WRITE(6,135)
  135 FORMAT(5X,'Duotomis salygomis uzduotis neissprendziama')
      GO TO 200
  138 WRITE(6,139)
  139 FORMAT(/1H ,15X,'R E Z U L T A T A I')
      IF(KIET.EQ.0) WRITE(6,140) KB,RBP
  140 FORMAT(/1H ,3X,'Betonas B',I2,' klases, kietinamas naturaliai'/1H ,
     *8X,'Santykinis stiprumas apspaudimo metu -',F4.2,' galutinio')
      IF(KIET.EQ.1) WRITE(6,142) KB,RBP
  142 FORMAT(/1H ,3X,'Betonas B',I2,' klases, kietinamas sutinant'/1H ,
     *8X,'Santykinis stiprumas apspaudimo metu -',F4.2,' galutinio')
      IF(LH.EQ.0) WRITE(6,144) SB,SH
  144 FORMAT(1H ,3X,'Skaiciuojamasis skerspjuvis b=',I2,' ir h=',
     *I3,' cm')
      MB=BL
      IF(LH.NE.0) WRITE(6,146) SB,SH,MB,LH
  146 FORMAT(1H ,3X,'Skaiciuojamasis skerspjuvis b=',I2,', h=',I3,
     *', bl=',I3,' ir hl=',I2,' cm')
      IF(S.LT.10.AND.ITIP.EQ.1) WRITE(6,148) AKL(I),S,DI,SE,NS
  148 FORMAT(1H ,3X,'Itempiama armatura ',A4,' klases,',I2,
     *' strypai d=',I2,' mm, isdestomi'/1H ,8X,I2,' eil. po',
     *I2,' strypus')
      IF(S.LT.10.AND.ITIP.GT.1) WRITE(6,150) AKL(I),S,DI,SE,NS
  150 FORMAT(1H ,3X,'Itempiama armatura ',A4,' klases,',I2,
     *' strypai d=',I2,' mm, isdestomi'/1H ,8X,
     *I2,' eil. po',I2,' strypus kiekvienos briaunos eileje')
      IF(S.GT.9.AND.ITIP.EQ.1) WRITE(6,152) AKL(I),S,DI,SE,NS
  152 FORMAT(1H ,3X,'Itempiama armatura ',A4,' klases,',I3,
     *' strypu d=',I2,' mm, isdestomu'/1H ,8X,I2,' eil. po',
     *I2,' strypus')
      IF(S.GT.9.AND.ITIP.GT.1) WRITE(6,154) AKL(I),S,DI
  154 FORMAT(1H ,3X,'Itempiama armatura ',A4,' klases,',I3,
     *' strypu d=',I2,' mm, isdestomu'/1H ,8X,I2,' eil. po',
     *I2,' strypus kiekvienos briaunos eileje')
      IF(SE.EQ.1) WRITE(6,155) S1
  155 FORMAT(1H ,6X,'Strypu eiles centro atstumas nuo sijos(bria',
     *'unos) apacios -',I3,' mm')
      IF(SE.GT.1) WRITE(6,156) S1,S2
  156 FORMAT(1H ,6X,'Pirmos strypu eiles centro atstumas nuo sijos',
     *'(briaunos) apacios -'/1H ,8X,I3,' mm ir tarp kitu eiliu ',
     *'centru -',I3,' mm')
      IF(ITA.EQ.0.AND.I.LT.5) WRITE(6,157)
  157 FORMAT(1H ,6X,'Armatura itempiama elektroterminiu budu,',
     *' atramos - ant formos')
      IF(ITA.EQ.1.AND.I.LT.5) WRITE(6,158)
  158 FORMAT(1H ,6X,'Armatura itempiama elektroterminiu budu,',
     *' atramos - ne ant formos, stende')
      IF(ITA.EQ.0.AND.I.GT.4) WRITE(6,159)
  159 FORMAT(1H ,6X,'Armatura itempiama mechaniniu budu,',
     *' atramos - ant formos')
      IF(ITA.EQ.1.AND.I.GT.4) WRITE(6,160)
  160 FORMAT(1H ,6X,'Armatura itempiama mechaniniu budu,',
     *' atramos - ne ant formos, stende')
      IF(ISA.EQ.0) WRITE(6,161)
  161 FORMAT(1H ,6X,'Betone ar-ra inkaruojama be papildomu ziedu')
      IF(ISA.EQ.1) WRITE(6,162)
  162 FORMAT(1H ,6X,'Betone ar-ra inkaruojama su papildomais ziedais')
      PRI=GIO*10
      PSA=0.1*PRI/EI
      WRITE(6,163) PRI,PSA
  163 FORMAT(1H ,6X,'Kontrol. pradiniai itemp.',F6.1,' MPa, sant.',
     *' pailgejimas',F4.1,' mm/m')
      IF(NPL.EQ.0.AND.INE.EQ.0) GO TO 167
      IF(NPL.NE.0.AND.INE.EQ.0) WRITE(6,164) PLS,PL5,F
  164 FORMAT(1H ,3X,'Normaliniu plysiu plotis a1=',F4.2,' ir a2=',
     *F4.2,' mm. Istrizi neatsiveria'/1H ,3X,'Maksimalus ilinkis f=',
     *F3.1,' cm')
      IF(NPL.NE.0.AND.INE.NE.0) WRITE(6,165) PLS,PL5,PLI,PLI3,F
  165 FORMAT(1H ,3X,'Normaliniu plysiu plotis a1=',F4.2,' ir a2=',
     *F4.2,' mm'/1H ,6X,'bei istrizuju a1=',F4.2,' ir a2=',F4.2,
     *' mm'/1H ,3X,'Maksimalus ilinkis f=',F3.1,' cm')
      IF(NPL.EQ.0.AND.INE.NE.0) WRITE(6,166) PLI,PLI3,F
  166 FORMAT(1H ,3X,'Normaliniai plysiai neatsiveria. Istrizuju ',
     *'plotis a1=',F4.2,' ir a2=',F4.2,' mm'/1H ,3X,'Maksimalus ',
     *'ilinkis f=',F3.1,' cm')
      GO TO 169
  167 WRITE(6,168) F
  168 FORMAT(1H ,3X,'Normaliniai ir istrizieji plysiai neatsive',
     *'ria'/1H ,3X,'Maksimalus ilinkis f=',F3.1,' cm')
  169 SUG=(100*ANG/4+2*LAT/3+5)/5
      SUG=SUG*5
      IF(NN.EQ.1) WRITE(6,170) DA,APKL(KAP),DV,APKL(KVI),DS,
     *APKL(KS),SUG,UG,UV
  170 FORMAT(/1H ,10X,'P A S T A B O S:'/1H ,3X,'1. Sija (arba kiek',
     *'viena plokstes briauna) papildomai armuojama kar-'/1H ,
     *6X,'karkasu, kurio apatinis strypas d',I2,A4,', virsutinis d',I2,
     *A4/1H ,6X,'bei skersiniai d=',I2,A4,', isdestomi atraminese',I4,
     *' cm ilgio zonose'/1H ,6X,'kas ',F4.1,' cm ir viduryje kas ',
     *F4.1,' cm')
      IF(NN.GT.1) WRITE(6,172) NN,DA,APKL(KAP),DV,APKL(KVI),DS,
     *APKL(KS),SUG,UG,UV
  172 FORMAT(/1H ,10X,'P A S T A B O S:'/1H ,3X,'1. Sija (arba kiek',
     *'viena plokstes briauna) papildomai armuojama',I2,' kar-'/1H ,
     *6X,'kasais, kuriu kiekvieno apatinis strypas d',I2,A4,
     *' virsutinis d',I2,A4/1H ,6X,'bei skersiniai d=',I2,A4,
     *', isdestomi atraminese',I4,' cm ilgio zonose'/1H ,6X,
     *'kas 'F4.1,' cm ir viduryje kas ',F4.1,' cm')
      WRITE(6,174) SA,SV
  174 FORMAT(1H ,8X,'Apatines ar-ros centro atstumas nuo sijos',
     *'(briaunos) apacios',I3,' mm'/1H ,6X,'ir virsutines nuo virsaus',
     *I3,' mm')
      IF(SAL.EQ.0.AND.DREG.LT.40) WRITE(6,176)
  176 FORMAT(1H ,3X,'2. Sija(plokste) numatyta eksploatuoti patal',
     *'poje, kai dregnis < 40%')
      IF(SAL.EQ.0.AND.DREG.GE.40) WRITE(6,178)
  178 FORMAT(1H ,3X,'2. Sija(plokste) numatyta eksploatuoti patal',
     *'poje, kai dregnis > 40%')
      IF(SAL.EQ.1) WRITE(6,180)
  180 FORMAT(1H ,3X,'2. Sija(plokste) numatyta eksploatuoti lauke')
  200 STOP
      END
      SUBROUTINE PAPR(KAP,DA,RA,RAS,EA)
      INTEGER DA
      I=DA-2
      IF(DA.GT.6) I=(DA+2)/2
      IF(I.GT.4) GO TO 4
      GO TO(1,2,3,4),I
    1 RA=37.5
      RAS=41.0
      GO TO 4
    2 RA=36.6
      RAS=40.5
      GO TO 4
    3 RA=36.0
      RAS=39.5
    4 GO TO(5,6,7,8),KAP
    5 EA=17.0
      GO TO 9
    6 RA=22.5
      RAS=23.5
      EA=21.00
      GO TO 9
    7 RA=28.0
      RAS=29.5
      EA=21.0
      GO TO 9
    8 RA=35.5
      IF(DA.GT.8) RA=36.5
      RAS=39.0
      EA=20.0
    9 RETURN
      END
      SUBROUTINE PIS(Z,V3,SH,BE,Q,SB,GX,TAU,RBS,RTS,ISP)
      IMPLICIT INTEGER (D,S)
      AL=Z/SH
      V1=AL*AL+1
      V2=AL*AL+BE*BE
      GY=(3-BE*2)/(V1*V1)-BE/(V2*V2)
      GY=Q*BE*BE*GY/(1.57*SB*SH)
      IF(AL.GT.0.7) GY=0.0
      V1=(GY-GX)*0.5
      V2=(-1*GX-GY)*0.5
      V2=(V2*V2+TAU*TAU)**0.5
      GT=V1+V2
      GC=V1-V2
      GB=(1-ABS(GC)/RBS)/V3
      IF(GB.GT.1.) GB=1.0
      GB=GB*RTS
      ISP=0
      IF(GB.LT.GT) ISP=1
      RETURN
      END
      SUBROUTINE PLYS(ET,UN,W,FL,FLS,GM,PL,R)
      IMPLICIT INTEGER (D,S)
      COMMON BL,SB,HF,A3,HO,LH,T2,E2,RBS,UM,U1,U2,AL,EI,YI,RTS,WPA,
     *SH,DI,RJ,AS,RAI,RA,AI,AA,EA,EV,EB
      FF=((BL-SB)*LH+0.5*A3/UN)/(SB*HO)
      BD=(1-HF/(2*HO))*FF
      WZ=(YI-E2)*T2+100*W
      TP=WZ/(SB*HO*HO*RBS)
      EP=WZ/T2
      XP=10*UM*AL/(18*UM*AL+(TP+BD)*5+1)
      XP=(1.5+FF)/(11.5*EP/HO-5)+XP
      IF(XP.GT.1.) XP=1.0
      ZP=(HF*FF/HO+XP*XP)/((FF+XP)*2)
      ZP=(1-ZP)*HO
      TN=(SH-U2-XP*HO)/(SH-U1-XP*HO)
      GM=((E2-ZP)*T2*TN+WZ)/(AS*ZP)
      PL=(0.07-2*UM)*ET*FL*GM*DI**(1.0/3)/EV
    5 FORMAT(2X,I5,7F9.4)
      HZ=(YI-E2)*T2+W*100
      TI=HZ/(SB*HO*HO*RBS)
      XI=10*UM*AL/(18*UM*AL+(TI+BD)*5+1)
      E1=HZ/T2
      XI=(1.5+FF)/(11.5*E1/HO-5)+XI
      IF(XI.GT.1.) XI=1.0
      B=SB*1.0
      IF(XI*HO.LT.LH*1.0) B=BL
      ZI=(HF*FF/HO+XI*XI)/((FF+XI)*2)
      ZI=(1-ZI)*HO
      FM=RTS*WPA/(W*100-(E2+RJ)*T2)
      IF(FM.GT.1.) FM=1.0
      V=E1/HO
      V1=1.2/FLS
      IF(V.LT.V1) V=V1
      PSI=1.25-FLS*FM-(1-FM*FM)/((3.5-1.8*FM)*V)
      IF(PSI.GT.1.) PSI=1.0
      V=(AI*EI+AA*EA)*1000
      R=HZ*PSI/V+0.9*HZ/((FF+XI)*B*HO*EB*UN*1000)
      R=R/(HO*ZI)-T2*PSI/(V*HO)
      RETURN
      END
