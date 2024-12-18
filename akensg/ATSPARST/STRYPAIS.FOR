      DIMENSION AM(14),BM(13),CM(6),MF(16)
      DIMENSION SX(77,3),FKM(77),KBM(77),KSM(77)
      COMMON /C1/ YBC,YAC,YBD,YAB,B,H,SAB,DAB,MM,MD,SBC,DBC,MAD,
     *SAC,DAC,KB,KS,SBD,DBD
      DATA BM/2.0,1.9,1.8,1.7,1.6,1.5,1.4,1.3,1.2,1.1,1.0,0.9,0.8/
      DATA CM/1.1,1.0,0.9,0.8,0.7,0.6/
      DATA AM/2.9,2.8,2.7,2.6,2.5,2.4,2.3,2.2,2.1,2.0,1.9,1.8,1.7,1.6/
      DATA MF/115,110,105,100,95,90,85,80,75,70,65,60,55,50,45,40/
      DATA SX/1.13,1.43,1.46,1.62,1.86,2.1,2.35,2.43,2.65,2.75,2.96,
     *3.08,3.48,3.79,3.89,4.2,4.38,4.8,4.96,5.41,6.13,6.86,7.28,7.39,
     *8.15,8.78,9.38,9.42,10.1,10.6,10.7,10.8,11.5,12.3,12.8,13.8,13.9,
     *15.2,15.6,17.2,19.2,19.7,22.0,22.8,24.3,24.7,26.3,27.3,28.9,
     *29.7,31.4,32.5,33.4,34.4,37.4,37.8,38.8,42.2,43.3,47.1,49.1,50.9,
     *54.6,54.8,60.4,62.0,68.6,76.5,78.4,87.7,94.3,97.0,106.1,111.5,
     *119.7,133.1,142.0,2.0,2.5,2.0,2.8,2.5,3.6,4.0,3.2,4.5,3.6,5.0,
     *4.0,4.5,4.0,5.0,4.5,5.6,5.0,6.3,5.6,6.3,7.0,6.3,7.5,7.0,7.5,8.0,
     *7.0,7.5,9.0,7.0,8.0,7.5,8.0,7.5,10.0,9.0,11.0,9.0,11.0,10.0,12.5,
     *12.5,10.0,12.5,14.0,10.0,14.0,12.5,10.0,16.0,14.0,12.5,16.0,16.0,
     *12.5,18.0,18.0,16.0,20.0,16.0,20.0,20.0,16.0,16.0,20.0,22.0,20.0,
     *25.0,25.0,20.0,25.0, 25.0, 20.0, 25.0, 25.0, 25.0,0.3,0.3,0.4,
     *0.3,0.4,0.3,0.3,0.4,0.3,0.4,0.3,0.4,0.4,0.5,0.4,0.5,0.4,0.5,0.4,
     *0.5,0.5,0.5,0.6,0.5,0.6,0.6,0.6,0.7,0.7,0.6,0.8,0.7,0.8,0.8,0.9,
     *0.7,0.8,0.7,0.9,0.8,1.0,0.8,0.9,1.2,1.0,0.9,1.4,1.0,1.2,1.6,1.0,
     *1.2,1.4,1.1,1.2,1.6,1.1,1.2,1.4,1.2,1.6,1.3,1.4,1.8,2.0,1.6,1.6,
     *2.0,1.6,1.8,2.5,2.0,2.2,3.0,2.5,2.8,3.0/
      DO 71 I=1,77
      FKM(I)=SX(I,1)
      KBM(I)=(SX(I,2)+0.03)*10
      KSM(I)=(SX(I,3)+0.03)*10
   71 CONTINUE
    3 PRINT *,'Iveskite uzduoties varianto Nr'
      READ *,IV
      PRINT 4
    4 FORMAT(1H ,5X,'|Var.|Duome-|Stry|  Skerspju-  |Pailge-|Mazgo B',
     *'|'/1H ,5X,'|Sch.| nys  |pas |     vis     | jimas |S, kamp',
     *'|'/1H ,5X,'|----|------|----|-------------|-------|--------|')
    5 PI=3.14159
      IA=0
      IB=0
      IC=0
      IJ=0
      N=1
      NS=1
      DO 130 J=1,1000
      NS=NS+5
      IF(NS.GT.32) NS=NS-32
      NCD=0
      IF(NS.EQ.2.OR.NS.EQ.5.OR.NS.EQ.6.OR.NS.EQ.11.OR.NS.EQ.12.
     *OR.NS.EQ.13.OR.NS.EQ.17.OR.NS.EQ.19.OR.NS.EQ.21.OR.NS.EQ.22.
     *OR.NS.EQ.23.OR.NS.EQ.27.OR.NS.EQ.32) NCD=1
      N1=0
      IF(J.EQ.1) GO TO 7
      IF(NS.NE.2.AND.NS.NE.30) GO TO 7
      DO 10 J1=1,20
      IA=IA+3
      IF(J1.EQ.1) IA=IA+1
      IF(IA.GT.14) IA=IA-14
      A=AM(IA)
      IJ=IJ+3
      IF(IJ.GT.16) IJ=IJ-16
      NF=MF(IJ)
      DO 9 J2=1,20
      IA=IA+3
      IF(J1.EQ.1) IA=IA+1
      IF(IA.GT.14) IA=IA-14
      A=AM(IA)
      IB=IB+1
      IF(IB.GT.13) IB=IB-13
      BI=BM(IB)
      IF(NS.EQ.30.AND.(BI.EQ.A.OR.ABS(A-2*BI).LT.0.1)) GO TO 9
      IF(NS.EQ.2.AND.ABS(A-BI).LT.0.1) GO TO 9
      DO 8 J3=1,20
      IC=IC+1
      IF(IC.GT.6) IC=IC-6
      C=CM(IC)
      IF(NS.EQ.30) GO TO 6
      AB=(C*C*2)**0.5
      AC=BI
      BC=A
      BD=(BI*BI+A*A)**0.5
      YBC=-1.0*NF*COS(PI/6)
      YAC=-1.0*NF*SIN(PI/6)
      YBD=YBC*BD/(BI-A)
      YAB=YBD*BI*AB/(BD*C)
      CALL PAP(FKM,KBM,KSM,AB,AC,BC,BD)
      DX=(DBD*BD+DAB*A)/(A-BI)
      DY=DX+DAB*AB/C
      DL=(DX*DX+DY*DY)**0.5
      IF(DL.GT.10.) GO TO 8
      CALL SPAU(IV,N,NS,NCD,A,BI,C,NF,DL,PK)
      GO TO 130
    6 IF(2*C.LT.A.OR.C.EQ.BI) GO TO 8
      AB=(4*BI*BI+A*A)**0.5
      BC=((2*BI-A)**2+A*A)**0.5
      BD=((2*C-A)**2+BI*BI)**0.5
      AC=A
      SA=A/AB
      CA=(2*BI)/AB
      SB=A/BC
      CB=(2*BI-A)/BC
      SG=(2*C-A)/BD
      CG=BI/BD
      YBC=NF/SB
      SK=SA*CG+SG*CA
      YAC=YBC*CB
      YAB=-1.0*YBC*(CG*SB+CB*SG)/(SA*CG+SG*CA)
      YBD=-1.0*(YAB*CA+YBC*CB)/CG
      CALL PAP(FKM,KBM,KSM,AB,AC,BC,BD)
      DY=(DBD*CA-DAB*CG)/(SA*CG+CA*SG)
      DX=(DAB+DY*SA)/CA
      DL=(DX*DX+DY*DY)**0.5
      IF(DL.LT.9.) GO TO 85
    8 CONTINUE
    9 CONTINUE
   10 CONTINUE
      GO TO 130
   85 IF(NS.EQ.30) PK=270+ATAN(DX/DY)*180/PI
      IF(NS.EQ.2) PK=270-ATAN(DX/DY)*180/PI
      CALL SPAU(IV,N,NS,NCD,A,BI,C,NF,DL,PK)
      GO TO 130
    7 DO 50 I=1,2000
      IJ=IJ+3
      IF(IJ.GT.16) IJ=IJ-16
      N1=N1+1
      IF(N1.GT.20) N1=N1-20
      IA=IA+3
      IF(N1.EQ.1) IA=IA+1
      IF(IA.GT.14) IA=IA-14
      IB=IB+1
      IF(IB.GT.13) IB=IB-13
      IC=IC+1
      IF(IC.GT.6) IC=IC-6
      C=CM(IC)
      IF(N.GT.IV) GO TO 55
      A=AM(IA)
      BI=BM(IB)
      C=CM(IC)
      NF=MF(IJ)
      IF(NS.EQ.23.AND.ABS(C-BI).LT.0.1) GO TO 50
      IF(NS.EQ.29.AND.ABS(A-C).LT.0.1) GO TO 50
   15 GO TO(16,130,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,
     *34,35,36,37,38,39,40,41,42,43,44,130,46,47),NS
   16 AB=BI
      AC=A
      BC=(A*A+BI*BI)**0.5
      BD=(BI*BI+C*C)**0.5
      YBC=-1.0*NF*BC/AB
      YAC=NF*AC/AB
      YBD=-1.0*YBC*AC*BD/(BC*BI)
      YAB=YBD*C/BD-YBC*AB/BC
      CALL PAP(FKM,KBM,KSM,AB,AC,BC,BD)
      DX=DAB
      DY=DBD*BD/BI+DAB*C/BI
      DL=(DX*DX+DY*DY)**0.5
      IF(DL.GT.5.) GO TO 50
      CALL SPAU(IV,N,NS,NCD,A,BI,C,NF,DL,PK)
      GO TO 130
   18 AB=BI
      AC=A
      BC=(AB*AB+AC*AC)**0.5
      BD=(2*C*C)**0.5
      YBC=-1.0*NF*BC/AB
      YAC=-1.0*YBC*AC/BC
      YBD=YBC*AC/(0.7071*BC)
      YAB=-1.0*(YBC*AB/BC+0.7071*YBD)
      CALL PAP(FKM,KBM,KSM,AB,AC,BC,BD)
      DY=DAB
      DX=(DY*0.7071-DBD)/0.7071
      DL=(DX*DX+DY*DY)**0.5
      IF(DL.GT.5.) GO TO 50
      PK=360-ATAN(DY/DX)*180/PI
      CALL SPAU(IV,N,NS,NCD,A,BI,C,NF,DL,PK)
      GO TO 130
   19 AC=A
      BC=BI
      AB=(AC*AC+BC*BC)**0.5
      BD=(BC*BC+C*C)**0.5
      SA=AC/AB
      CA=BC/AB
      SB=C/BD
      CB=BC/BD
      YBC=-0.866*NF
      YAC=-0.5*NF
      YAB=-1.0*YBC*SB/(CA*SB+CB*SA)
      YBD=-1.0*YAB*SA/SB
      CALL PAP(FKM,KBM,KSM,AB,AC,BC,BD)
      DY=-1.0*(DBD*CA+DAB*CB)/(SB*CA+CB*SA)
      DX=(DAB+DY*SA)/CA
      DL=(DX*DX+DY*DY)**0.5
      IF(DL.GT.5.) GO TO 50
      PK=ATAN(DY/DX)*180/PI
      CALL SPAU(IV,N,NS,NCD,A,BI,C,NF,DL,PK)
      GO TO 130
   20 IF(A.EQ.BI.AND.BI.EQ.C) GO TO 50
      AB=(BI*BI+C*C)**0.5
      BC=(A*A+(A-C)*(A-C))**0.5
      BD=(A*A+C*C)**0.5
      AC=A
      SA=BI/AB
      CA=C/AB
      SB=(A-C)/BC
      CB=A/BC
      SG=C/BD
      CG=A/BD
      YBC=-0.866*NF/CB
      YAC=0.5*NF-YBC*SB
      YBD=YBC*(SB*CA+SA*CB)/(SG*CA-SA*CG)
      YAB=(YBC*CB+YBD*CG)/CA
      CALL PAP(FKM,KBM,KSM,AB,AC,BC,BD)
      DY=(DAB*CG+DBD*CA)/(SA*CG-CA*SG)
      DX=(DBD+DY*SG)/CG
      DL=(DX*DX+DY*DY)**0.5
      IF(DL.GT.5.) GO TO 50
      PK=180-ATAN(DY/DX)*180/PI
      CALL SPAU(IV,N,NS,NCD,A,BI,C,NF,DL,PK)
      GO TO 130
   21 AB=(BI*BI+C*C)**0.5
      BC=BI
      BD=(A*A+BI*BI)**0.5
      AC=A
      SA=BI/AB
      CA=C/AB
      SB=A/BD
      CB=BI/BD
      RK=PI*75/180
      YBC=-1.0*NF*COS(RK)
      YAC=-1.0*NF*SIN(RK)
      YBD=-1.0*YBC/(CB+CA*SB/SA)
      YAB=-1.0*YBD*SB/SA
      CALL PAP(FKM,KBM,KSM,AB,AC,BC,BD)
      DY=(DBD+DAB*CB/CA)/(SB+SA*CB/CA)
      DX=(DY*SA-DAB)/CA
      DL=(DX*DX+DY*DY)**0.5
      IF(DL.GT.5.) GO TO 50
      PK=180+ATAN(DY/DX)*180/PI
      CALL SPAU(IV,N,NS,NCD,A,BI,C,NF,DL,PK)
      GO TO 130
   22 AB=A
      BC=(2*A*A)**0.5
      BD=(C*C+BI*BI)**0.5
      AC=A
      SA=A/BC
      CA=A/BC
      SB=BI/BD
      CB=C/BD
      RK=PI/3
      YBC=-1.0*NF*SIN(RK)/CA
      YAC=NF*COS(RK)-YBC*SA
      YAB=-1.0*YBC*(CA+CB*SA/SB)
      YBD=-YBC*SA/SB
      CALL PAP(FKM,KBM,KSM,AB,AC,BC,BD)
      DY=(DAB*CB+DBD)/SB
      DX=DAB
      DL=(DX*DX+DY*DY)**0.5
      IF(DL.GT.5.) GO TO 50
      PK=ATAN(DY/DX)*180/PI
      CALL SPAU(IV,N,NS,NCD,A,BI,C,NF,DL,PK)
      GO TO 130
   23 AB=A
      BC=(A*A+C*C)**0.5
      BD=(C*C+BI*BI)**0.5
      AC=C
      SA=C/BC
      CA=A/BC
      SB=C/BD
      CB=BI/BD
      YBC=-1.0*NF/CA
      YAC=-1.0*YBC*SA
      YBD=YBC*SA/SB
      YAB=-1.0*(YBD*CB+YBC*CA)
      CALL PAP(FKM,KBM,KSM,AB,AC,BC,BD)
      DY=(DAB*CB-DBD)/SB
      DX=DAB
      DL=(DX*DX+DY*DY)**0.5
      IF(DL.GT.5.) GO TO 50
      PK=270-ATAN(DX/DY)*180/PI
      CALL SPAU(IV,N,NS,NCD,A,BI,C,NF,DL,PK)
      GO TO 130
   24 IF(A.EQ.C) GO TO 130
      AB=(A*A+C*C)**0.5
      BC=((A-C)*(A-C)+A*A)**0.5
      BD=(4*C*C+BI*BI)**0.5
      AC=A
      SA=A/BC
      CA=(A-C)/BC
      SB=A/AB
      CB=C/AB
      SG=BI/BD
      CG=2*C/BD
      YBC=NF/SA
      S1=SG*CB
      S2=SB*CG
      IF(S1.EQ.S2) GO TO 130
      YAC=-1.0*YBC*CA
      YAB=YBC*(SA*CG+SG*CA)/(SG*CB-SB*CG)
      YBD=(YBC*CA-YAB*CB)/CG
      CALL PAP(FKM,KBM,KSM,AB,AC,BC,BD)
      DY=(DBD*CB-DAB*CG)/(CG*SB-SG*CB)
      DX=(DY*SB+DAB)/CB
      DL=(DX*DX+DY*DY)**0.5
      IF(DL.GT.8.) GO TO 50
      PK=180-ATAN(DY/DX)*180/PI
      CALL SPAU(IV,N,NS,NCD,A,BI,C,NF,DL,PK)
      GO TO 130
   25 AB=(A*A+C*C)**0.5
      BC=A
      BD=(C*C+BI*BI)**0.5
      AC=C
      SA=C/AB
      CA=A/AB
      SB=BI/BD
      CB=C/BD
      RK=PI*5/18
      YBC=-1.0*NF*SIN(RK)
      YAC=-1.0*NF*COS(RK)
      YAB=-1.0*YBC*SB/(CA*SB+SA*CB)
      YBD=-1.0*YAB*SA/SB
      CALL PAP(FKM,KBM,KSM,AB,AC,BC,BD)
      DY=-1.0*(DAB*CB+DBD*CA)/(CA*SB+SA*CB)
      DX=-1.0*(DBD+DY*SB)/CB
      DL=(DX*DX+DY*DY)**0.5
      IF(DL.GT.5.) GO TO 50
      PK=270+ATAN(DX/DY)*180/PI
      CALL SPAU(IV,N,NS,NCD,A,BI,C,NF,DL,PK)
      GO TO 130
   26 AB=((BI+C)*(BI+C)+C*C)**0.5
      BC=(A*A+C*C)**0.5
      BD=(A*A+(BI+C)*(BI+C))**0.5
      AC=BI
      SA=(BI+C)/AB
      CA=C/AB
      SB=(BI+C)/BD
      CB=A/BD
      SG=C/BC
      CG=A/BC
      YBC=-1.0*NF/CG
      YAC=YBC*SG
      YAB=YBC*(SB*CG-SG*CB)/(SA*CB+SB*CA)
      YBD=(YAB*CA-YBC*CG)/CB
      CALL PAP(FKM,KBM,KSM,AB,AC,BC,BD)
      DY=(DBD*CA+DAB*CB)/(SA*CB+CA*SB)
      DX=(DBD-DY*SB)/CB
      DL=(DX*DX+DY*DY)**0.5
      IF(DL.GT.5.) GO TO 50
      PK=180-ATAN(DY/DX)*180/PI
      CALL SPAU(IV,N,NS,NCD,A,BI,C,NF,DL,PK)
      GO TO 130
   27 AB=(2*C*C)**0.5
      BC=BI
      BD=A
      AC=(BI*BI+A*A)**0.5
      SA=C/AB
      CA=C/AB
      SB=BI/AC
      CB=A/AC
      RK=PI/4
      YAC=NF*SIN(RK)/CB
      YBC=-1.0*YAC*SB-NF*COS(RK)
      YAB=YBC/SA
      YBD=YAB*CA
      CALL PAP(FKM,KBM,KSM,AB,AC,BC,BD)
      DX=-1.0*DBD
      DY=(DX*CA-DAB)/SA
      DL=(DX*DX+DY*DY)**0.5
      IF(DL.GT.5.) GO TO 50
      PK=270+ATAN(DX/DY)*180/PI
      CALL SPAU(IV,N,NS,NCD,A,BI,C,NF,DL,PK)
      GO TO 130
   28 AB=(BI*BI+C*C)**0.5
      BC=(A*A+C*C)**0.5
      BD=C
      AC=A
      SA=C/BC
      CA=A/BC
      SB=C/AB
      CB=BI/AB
      RK=PI*7/18
      YBC=NF*SIN(RK)/SA
      YAC=-1.0*NF*COS(RK)-YBC*CA
      YAB=-1.0*YBC*CA/CB
      YBD=YAB*SB-YBC*SA
      CALL PAP(FKM,KBM,KSM,AB,AC,BC,BD)
      DY=-1.0*DBD
      DX=(DY*SB-DAB)/CB
      DL=(DX*DX+DY*DY)**0.5
      IF(DL.GT.5.) GO TO 50
      PK=180+ATAN(DY/DX)*180/PI
      CALL SPAU(IV,N,NS,NCD,A,BI,C,NF,DL,PK)
      GO TO 130
   29 AB=BI
      BC=(A*A+BI*BI)**0.5
      BD=(2*C*C)**0.5
      AC=A
      SA=A/BC
      CA=BI/BC
      SB=0.7071
      YBC=-1.0*NF/CA
      YAC=-1.0*YBC*SA
      YBD=YAC/SB
      YAB=YBD*SB-YBC*CA
      CALL PAP(FKM,KBM,KSM,AB,AC,BC,BD)
      DX=DAB
      DY=(DX*SB+DBD)/SB
      DL=(DX*DX+DY*DY)**0.5
      IF(DL.GT.5.) GO TO 50
      PK=ATAN(DY/DX)*180/PI
      CALL SPAU(IV,N,NS,NCD,A,BI,C,NF,DL,PK)
      GO TO 130
   30 AB=A
      BC=(C*C+A*A)**0.5
      BD=(C*C+BI*BI)**0.5
      AC=C
      SA=A/BC
      CA=C/BC
      SB=C/BD
      CB=BI/BD
      RK=PI/3
      YBC=-1.0*NF*COS(RK)/SA
      YAC=-1.0*NF*SIN(RK)-YBC*CA
      YBD=YBC*CA/CB
      YAB=-1.0*YBC*SA-YBD*SB
      CALL PAP(FKM,KBM,KSM,AB,AC,BC,BD)
      DY=DAB
      DX=(DY*SB-DBD)/CB
      DL=(DX*DX+DY*DY)**0.5
      IF(DL.GT.5.) GO TO 50
      PK=ATAN(DY/DX)*180/PI
      CALL SPAU(IV,N,NS,NCD,A,BI,C,NF,DL,PK)
      GO TO 130
   31 AB=((BI-A)*(BI-A)+A*A)**0.5
      BC=(BI*BI+A*A)**0.5
      BD=(C*C+BI*BI)**0.5
      AC=A
      SA=A/BC
      CA=BI/BC
      SB=A/AB
      CB=(A-BI)/AB
      SG=C/BD
      CG=BI/BD
      RK=PI*35/180
      YBC=-1.0*NF*SIN(RK)/SA
      YAC=NF*COS(RK)-YBC*CA
      YBD=-1.0*YBC*(CA+SA*CB/SB)/(SG*CB/SB-CG)
      YAB=-1.0*YBC*SA/SB-YBD*SG/SB
      CALL PAP(FKM,KBM,KSM,AB,AC,BC,BD)
      DY=(DAB*CG-DBD*CB)/(SB*CG-SG*CB)
      DX=(DY*SG-DBD)/CG
      DL=(DX*DX+DY*DY)**0.5
      IF(DL.GT.6.) GO TO 50
      PK=270+ATAN(DX/DY)*180/PI
      CALL SPAU(IV,N,NS,NCD,A,BI,C,NF,DL,PK)
      GO TO 130
   32 IF(A.EQ.C) GO TO 50
      AB=BI
      BC=C
      BD=(4*C*C+A*A)**0.5
      AC=(A*A+C*C)**0.5
      SA=C/AC
      CA=A/AC
      SB=2*C/BD
      CB=A/BD
      RK=PI/4
      YAC=-1.0*NF*SIN(RK)/CA
      YBC=-1.0*NF*COS(RK)+YAC*SA
      YBD=-1.0*YBC/SB
      YAB=-1.0*YBD*CB
      CALL PAP(FKM,KBM,KSM,AB,AC,BC,BD)
      DX=-1.0*DAB
      DY=(DX*CB+DBD)/SB
      DL=(DX*DX+DY*DY)**0.5
      IF(DL.GT.5.) GO TO 50
      PK=ATAN(DY/DX)*180/PI
      CALL SPAU(IV,N,NS,NCD,A,BI,C,NF,DL,PK)
      GO TO 130
   33 AB=(BI*BI+C*C)**0.5
      BC=(BI*BI+A*A)**0.5
      BD=C
      AC=A+C
      SA=BI/AB
      CA=C/AB
      SB=BI/BC
      CB=A/BC
      YBC=-1.0*NF/SB
      YAC=-1.0*YBC*CB
      YAB=YBC*CB/CA
      YBD=YBC*SB+YAB*SA
      CALL PAP(FKM,KBM,KSM,AB,AC,BC,BD)
      DY=-1.0*DBD
      DX=(DY*SA-DAB)/CA
      DL=(DX*DX+DY*DY)**0.5
      IF(DL.GT.5.) GO TO 50
      PK=180-ATAN(DY/DX)*180/PI
      CALL SPAU(IV,N,NS,NCD,A,BI,C,NF,DL,PK)
      GO TO 130
   34 AB=(2*C*C)**0.5
      BC=BI
      BD=(BI*BI+A*A)**0.5
      AC=A
      SA=0.7071
      CA=0.7071
      SB=A/BD
      CB=BI/BD
      RK=PI/6
      YBC=-1.0*NF*COS(RK)
      YAC=-1.0*NF*0.5
      YAB=YBC/(SA*CB/SB+CA)
      YBD=-1.0*YAB*SA/SB
      CALL PAP(FKM,KBM,KSM,AB,AC,BC,BD)
      DY=-1.0*(DBD*CA+DAB*CB)/(SB*CA+CB*SA)
      DX=-1.0*(DY*SA+DAB)/CA
      DL=(DX*DX+DY*DY)**0.5
      IF(DL.GT.5.) GO TO 50
      PK=180+ATAN(DY/DX)*180/PI
      CALL SPAU(IV,N,NS,NCD,A,BI,C,NF,DL,PK)
      GO TO 130
   35 AB=(A*A+C*C)**0.5
      BC=((A+C)*(A+C)+A*A)**0.5
      BD=BI
      AC=A
      SA=A/AB
      CA=C/AB
      SB=A/BC
      CB=(A+C)/BC
      YBC=NF/SB
      YAC=-1.0*YBC*CB
      YAB=-1.0*YBC*SB/SA
      YBD=-1.0*YBC*CB-YAB*CA
      CALL PAP(FKM,KBM,KSM,AB,AC,BC,BD)
      DY=(DBD*CA-DAB)/SA
      DX=-1.0*DBD
      DL=(DX*DX+DY*DY)**0.5
      IF(DL.GT.5.) GO TO 50
      PK=360-ATAN(DY/DX)*180/PI
      CALL SPAU(IV,N,NS,NCD,A,BI,C,NF,DL,PK)
      GO TO 130
   36 IF(A.EQ.C) GO TO 130
      AB=(A*A+C*C)**0.5
      BC=(BI*BI+C*C)**0.5
      BD=((A-C)*(A-C)+BI*BI)**0.5
      AC=A
      SA=A/AB
      CA=C/AB
      SB=C/BC
      CB=BI/BC
      SG=(A-C)/BD
      CG=BI/BD
      YBC=-1.0*NF/CB
      YAC=-1.0*YBC*SB
      S1=CA*SG
      S2=CG*SA
      YAB=YBC*(CB*SG+CG*SB)/(CA*SG-CG*SA)
      YBD=(YAB*SA+YBC*SB)/SG
      CALL PAP(FKM,KBM,KSM,AB,AC,BC,BD)
      DX=(DBD*SA+DAB*SG)/(CG*SA-CA*SG)
      DY=(DAB+DX*CA)/SA
      DL=(DX*DX+DY*DY)**0.5
      IF(DL.GT.5.) GO TO 50
      PK=180+ATAN(DY/DX)*180/PI
      CALL SPAU(IV,N,NS,NCD,A,BI,C,NF,DL,PK)
      GO TO 130
   37 AB=(BI*BI+C*C)**0.5
      BC=(4*C*C+A*A)**0.5
      BD=(A*A+C*C)**0.5
      AC=C
      SA=BI/AB
      CA=C/AB
      SB=2*C/BC
      CB=A/BC
      SG=C/BD
      CG=A/BD
      YBC=-1.0*NF/CB
      YAC=-1.0*YBC*SB
      YAB=YBC*(SG*CB-SB*CG)/(SA*CG+SG*CA)
      YBD=(YAB*CA-YBC*CB)/CG
      CALL PAP(FKM,KBM,KSM,AB,AC,BC,BD)
      DX=(DBD*SA-DAB*SG)/(CG*SA+CA*SG)
      DY=(DBD-DX*CG)/SG
      DL=(DX*DX+DY*DY)**0.5
      IF(DL.GT.5.) GO TO 50
      PK=270-ATAN(DX/DY)*180/PI
      CALL SPAU(IV,N,NS,NCD,A,BI,C,NF,DL,PK)
      GO TO 130
   38 AB=(BI*BI+C*C)**0.5
      BC=A
      BD=(2*A*A)**0.5
      AC=A
      SA=BI/AB
      CA=C/AB
      SB=0.7071
      CB=0.7071
      RK=PI*65/180
      YBC=-1.0*NF*SIN(RK)
      YAC=NF*COS(RK)
      YAB=YBC*SB/(CA*SB+CB*SA)
      YBD=-1.0*YAB*SA/SB
      CALL PAP(FKM,KBM,KSM,AB,AC,BC,BD)
      DY=-1.0*(DAB*CB+DBD*CA)/(CB*SA+CA*SB)
      DX=-1.0*(DAB+DY*SA)/CA
      DL=(DX*DX+DY*DY)**0.5
      IF(DL.GT.5.) GO TO 50
      PK=180+ATAN(DY/DX)*180/PI
      CALL SPAU(IV,N,NS,NCD,A,BI,C,NF,DL,PK)
      GO TO 130
   39 AB=(A*A+4*C*C)**0.5
      BC=(A*A+C*C)**0.5
      BD=(BI*BI+C*C)**0.5
      AC=C
      SA=C/BC
      CA=A/BC
      SB=2*C/AB
      CB=A/AB
      SG=BI/BD
      CG=C/BD
      YBC=-1.0*NF/CA
      YAC=YBC*SA
      YAB=-1.0*YBC*(SA*CG+SG*CA)/(SB*CG+SG*CB)
      YBD=(YBC*CA+YAB*CB)/CG
      CALL PAP(FKM,KBM,KSM,AB,AC,BC,BD)
      DY=-1.0*(DAB*CG+DBD*CB)/(CG*SB+SG*CB)
      DX=(DAB+DY*SB)/CB
      DL=(DX*DX+DY*DY)**0.5
      IF(DL.GT.5.) GO TO 50
      PK=ATAN(DY/DX)*180/PI
      CALL SPAU(IV,N,NS,NCD,A,BI,C,NF,DL,PK)
      GO TO 130
   40 AB=(A*A+BI*BI)**0.5
      BC=BI
      BD=(2*C*C)**0.5
      AC=A
      SA=BI/AB
      CA=A/AB
      SB=0.7071
      CB=0.7071
      RK=PI/4
      YAC=-1.0*NF*COS(RK)
      YBC=-1.0*NF*SIN(RK)
      YAB=-1.0*YBC*CB/(SA*CB+CA*SB)
      YBD=-1.0*YAB*CA/CB
      CALL PAP(FKM,KBM,KSM,AB,AC,BC,BD)
      DY=(DAB*CB-DBD*CA)/(SB*CA+CB*SA)
      DX=(DY*SA-DAB)/CA
      DL=(DX*DX+DY*DY)**0.5
      IF(DL.GT.5.) GO TO 50
      PK=90+ATAN(DX/DY)*180/PI
      CALL SPAU(IV,N,NS,NCD,A,BI,C,NF,DL,PK)
      GO TO 130
   41 AB=(A*A+BI*BI)**0.5
      BC=BI
      BD=(BI*BI+C*C)**0.5
      AC=A
      SA=A/AB
      CA=BI/AB
      SB=C/BD
      CB=BI/BD
      RK=PI*5/18
      YBC=-1.0*NF*COS(RK)
      YAC=NF*SIN(RK)
      YAB=-1.0*YBC*SB/(CA*SB+SA*CB)
      YBD=-1.0*YAB*SA/SB
      CALL PAP(FKM,KBM,KSM,AB,AC,BC,BD)
      DY=(DAB*CB+DBD*CA)/(SA*CB+CA*SB)
      DX=(DY*SB-DBD)/CB
      DL=(DX*DX+DY*DY)**0.5
      IF(DL.GT.5.) GO TO 50
      PK=ATAN(DY/DX)*180/PI
      CALL SPAU(IV,N,NS,NCD,A,BI,C,NF,DL,PK)
      GO TO 130
   42 AB=(C*C+BI*BI)**0.5
      BC=A
      BD=C
      AC=(A*A+C*C)**0.5
      SA=BI/AB
      CA=C/AB
      SB=C/AC
      CB=A/AC
      YAC=-1.0*NF/SB
      YBC=-1.0*YAC*CB
      YAB=YBC/CA
      YBD=YAB*SA
      CALL PAP(FKM,KBM,KSM,AB,AC,BC,BD)
      DY=DBD
      DX=(DAB+DY*SA)/CA
      DL=(DX*DX+DY*DY)**0.5
      IF(DL.GT.5.) GO TO 50
      PK=360-ATAN(DY/DX)*180/PI
      CALL SPAU(IV,N,NS,NCD,A,BI,C,NF,DL,PK)
      GO TO 130
   43 AB=BI
      BC=(BI*BI+A*A)**0.5
      BD=(BI*BI+C*C)**0.5
      AC=A
      SA=A/BC
      CA=BI/BC
      SB=BI/BD
      CB=C/BD
      RK=PI*65/180
      YBC=-1.0*NF*SIN(RK)/CA
      YAC=-1.0*NF*COS(RK)-YBC*SA
      YAB=-1.0*YBC*(CA*SB+CB*SA)/SB
      YBD=-1.0*YBC*SA/SB
      CALL PAP(FKM,KBM,KSM,AB,AC,BC,BD)
      DX=DAB
      DY=(DBD+DX*CB)/SB
      DL=(DX*DX+DY*DY)**0.5
      IF(DL.GT.5.) GO TO 50
      PK=360-ATAN(DY/DX)*180/PI
      CALL SPAU(IV,N,NS,NCD,A,BI,C,NF,DL,PK)
      GO TO 130
   44 AB=(2*C*C)**0.5
      BC=(BI*BI+C*C)**0.5
      BD=(A*A+C*C)**0.5
      AC=BI+C
      SA=C/AB
      CA=C/AB
      SB=C/BC
      CB=BI/BC
      SG=C/BD
      CG=A/BD
      YBC=-1.0*NF/SB
      YAC=-1.0*YBC*CB
      YBD=(SB+CB)*YBC/(SG-CG)
      YAB=(YBD*SG-YBC*SB)/SA
      CALL PAP(FKM,KBM,KSM,AB,AC,BC,BD)
      DY=(DAB*CG+DBD*CA)/(SA*CG-CA*SG)
      DX=(DY*SA-DAB)/CA
      DL=(DX*DX+DY*DY)**0.5
      IF(DL.GT.10.) GO TO 50
      PK=180+ATAN(DY/DX)*180/PI
      CALL SPAU(IV,N,NS,NCD,A,BI,C,NF,DL,PK)
      GO TO 130
   46 AB=(4*C*C+A*A)**0.5
      BC=(A*A+C*C)**0.5
      BD=(C*C+BI*BI)**0.5
      AC=C
      SA=2*C/AB
      CA=A/AB
      SB=C/BC
      CB=A/BC
      SG=BI/BD
      CG=C/BD
      YBC=NF/CB
      YAC=YBC*SB
      YAB=-1.0*YBC*(CG*SB+CB*SG)/(SA*CG+SG*CA)
      YBD=(YAB*CA+YBC*CB)/CG
      CALL PAP(FKM,KBM,KSM,AB,AC,BC,BD)
      DY=(DBD*CA+DAB*CG)/(SA*CG+CA*SG)
      DX=-1.0*(DAB-DY*SA)/CA
      DL=(DX*DX+DY*DY)**0.5
      IF(DL.GT.5.) GO TO 50
      PK=180-ATAN(DY/DX)*180/PI
      CALL SPAU(IV,N,NS,NCD,A,BI,C,NF,DL,PK)
      GO TO 130
   47 AB=(BI*BI+C*C)**0.5
      BC=BI
      BD=(A*A+BI*BI)**0.5
      AC=A
      SA=BI/AB
      CA=C/AB
      SB=A/BD
      CB=BI/BD
      RK=PI/6
      YBC=NF*COS(RK)
      YAC=NF*0.5
      YAB=YBC*SB/(SA*CB+SB*CA)
      YBD=-1.0*YAB*SA/SB
      CALL PAP(FKM,KBM,KSM,AB,AC,BC,BD)
      DY=(DAB*CB+DBD*CA)/(CA*SB+CB*SA)
      DX=-1.0*(DBD-DY*SB)/CB
      DL=(DX*DX+DY*DY)**0.5
      IF(DL.GT.5.) GO TO 50
      PK=ATAN(DY/DX)*180/PI
      CALL SPAU(IV,N,NS,NCD,A,BI,C,NF,DL,PK)
      GO TO 130
   50 CONTINUE
  130 CONTINUE
   55 PRINT 56
   56 FORMAT(2X,'Paaiskinimas: "Mazgo B S,kamp" reiskia mazgo B pos',
     *'linki mm ir jo krypti'/16X,'laipsniais teigiamu x asies atzv',
     *'ilgiu pries l.rodykle')
      PRINT *,'Jei baigete, iveskite nuli; jei tikrinsite kita var',
     *'ianta - bet koki skaitmeni'
      READ *,NON
      IF(NON.NE.0) GO TO 3
      STOP
      END
      SUBROUTINE PAP(FKM,KBM,KSM,AB,AC,BC,BD)
      DIMENSION FKM(77),KBM(77),KSM(77)
      COMMON /C1/ YBC,YAC,YBD,YAB,B,H,SAB,DAB,MM,MD,SBC,DBC,MAD,
     *SAC,DAC,KB,KS,SBD,DBD
      B=(ABS(YAB)/2)**0.5
      IB=2*B
      B=IB*0.5
      H=B*2
      SM=ABS(YAB*10/(2*B*B))
      IF(SM.LE.10.) GO TO 2
      BB=B-1
      DO 4 I=1,3
      BB=BB+0.5
      HH=BB*2-1.5
      DO 3 J=1,4
      HH=HH+0.5
      IF(HH.LE.0.) GO TO 3
      SK=ABS(YAB*10/(BB*HH))
      IF(SK.GT.10.) GO TO 3
      B=BB
      H=HH
      GO TO 2
    3 CONTINUE
    4 CONTINUE
    2 SAB=YAB*10/(H*B)
      IF(ABS(SAB).LE.10.) GO TO 5
      B=B+0.5
      SAB=YAB*10/(H*B)
    5 DAB=SAB*AB/10
      DM=(ABS(YBC)*8.1618)**0.5
      MD=1.6*DM
      MM=DM
      A=(MD*MD-MM*MM)*0.007854
      SBC=10*YBC/A
      IF(ABS(SBC).LE.100.) GO TO 6
      MD=MD+1
      A=(MD*MD-MM*MM)*0.007854
      SBC=10*YBC/A
    6 DBC=SBC*BC/70
      D=(ABS(YAC)*9.095)**0.5
      MAD=D+0.5
      SAC=1273.24*YAC/(MAD*MAD)
      IF(ABS(SAC).LE.140.) GO TO 7
      MAD=MAD+1
      SAC=1273.24*YAC/(MAD*MAD)
    7 DAC=SAC*AC/200
      AK=ABS(YBD)/16
      DO 10 I=1,77
      FK=FKM(I)
      IF(FK.GE.AK) GO TO 15
   10 CONTINUE
   15 KB=KBM(I)
      KS=KSM(I)
      SBD=YBD*10/FK
      DBD=SBD*BD/200
   16 RETURN
      END
      SUBROUTINE SPAU(IV,N,NS,NCD,A,BI,C,NF,DL,PK)
      COMMON /C1/ YBC,YAC,YBD,YAB,B,H,SAB,DAB,MM,MD,SBC,DBC,MAD,
     *SAC,DAC,KB,KS,SBD,DBD
      IF(N.NE.IV) GO TO 15
      IF(PK.GT.360.) PK=PK-360
      IF(PK.LT.0.) PK=PK+360
      PRINT 20,A,B,H,DAB
   20 FORMAT(1H ,11X,'a=',F3.1,2X,'A-B',
     *F6.1,'x',F4.1,' cm',F8.2)
      PRINT 21, N,BI,MD,MM,DBC,DL
   21 FORMAT(1H ,5X,I4,2X,'b=',F3.1,2X,'B-C',3X,
     *I3,':',I2,' mm',2X,F8.2,F9.4)
      IF(NCD.EQ.0) PRINT 22, NS,C,MAD,DAC,PK
   22 FORMAT(1H ,5X,I4,2X,'c=',F3.1,2X,'A-C',4X,
     *'d=',I3,' mm',2X,F8.2,F9.2)
      IF(NCD.EQ.1) PRINT 23, NS,C,MAD,DAC,PK
   23 FORMAT(1H ,5X,I4,2X,'c=',F3.1,2X,'C-D',4X,
     *'d=',I3,' mm',2X,F8.2,F9.2)
      PRINT 24, NF,KB,KB,KS,DBD
   24 FORMAT(1H ,11X,'F=',I3,2X,'B-D',3X,
     *I3,'x',I3,'x',I2,1X,F8.2/1H ,5X,49('-'))
   15 N=N+1
      RETURN
      END
