      DIMENSION DL(20,3),MA(20,3)
      DIMENSION WA(9),WB(9),WC(12),NT(23),WT(23),NL(22),WL(22)
      COMMON F11,F12,F13,F14,F21,F22,F31,F32,F33,F34,F35,
     *P11,P12,P13,P21,P22,P23,P31,P32,P33,P34,V11,V12,V13,V14,V21,
     *V22,V23,V24,V31,V32,V33,V34,V35
      DATA DL/0.8,0.7,0.5,1.0,0.9,0.8,0.6,1.0,0.5,0.7,1.0,0.7,0.5,
     *1.0,0.6,0.9,0.6,0.5,0.8,0.6,1.0,1.2,1.2,0.8,0.9,1.2,1.4,1.2,
     *1.6,1.0,1.0,1.0,1.4,1.6,1.8,1.2,1.6,1.8,1.6,1.2,1.6,1.8,1.8,1.2,
     *1.4,1.4,1.6,1.0,1.2,1.6,1.6,1.4,1.6,1.0,1.0,1.2,1.2,1.6,1.4,1.8/
      DATA MA/10,15,20,25,20,15,15,30,25,20,30,25,20,30,10,15,25,15,
     *20,25,15,30,15,15,20,20,30,20,30,25,30,20,25,15,20,25,25,15,30,
     *25,30,20,25,20,20,25,15,20,20,15,30,15,10,20,25,25,25,20,20,25/
      DATA NT /10,12,14,16,18,18,20,20,22,22,24,24,27,27,30,30,
     *33,36,40,45,50,55,60/
      DATA WT /39.7,58.4,81.7,109.,143.,159.,184.,203.,232.,254.,
     *289.,317.,371.,407.,472.,518.,597.,743.,953.,1231.,1589.,
     *2035.,2560.0/
      DATA NL /5,6,8,10,12,14,14,16,16,18,18,20,20,22,22,24,24,
     *27,30,33,36,40/
      DATA WL /9.1,15.0,22.4,34.8,50.6,70.2,77.7,93.4,103.,121.,
     *132.,152.,167.,192.,212.,242.,265.,308.,387.,484.,601.,761.0/
   99 PRINT *,'Iveskite uzduoties eil.Nr'
      READ *, NG
      NS=0
      I1=0
      I2=0
      ID=0
      DO 50 I=1,NG
      F11=0.0
      F12=0.0
      F13=0.0
      F14=0.0
      F21=0.0
      F22=0.0
      F31=0.0
      F32=0.0
      F33=0.0
      F34=0.0
      F35=0.0
      P11=0.0
      P12=0.0
      P13=0.0
      P21=0.0
      P22=0.0
      P23=0.0
      P31=0.0
      P32=0.0
      P33=0.0
      P34=0.0
      V11=0.0
      V12=0.0
      V13=0.0
      V14=0.0
      V21=0.0
      V22=0.0
      V23=0.0
      V24=0.0
      V31=0.0
      V32=0.0
      V33=0.0
      V34=0.0
      V35=0.0
      NC=0
      ID=ID+1
      IF(ID.GT.20) ID=ID-20
      I1=I1+3
      IF(ID.EQ.11) I1=I1+1
      IF(I1.GT.20) I1=I1-20
      NS=NS+7
      IF(NS.GT.30) NS=NS-30
      I2=I2+1
      IF(ID.EQ.17) I2=I2+1
      IF(I2.GT.20) I2=I2-20
      S1=0.5*NS-NS/2
      IF(S1.EQ.0.) NC=1
      IF(I.LT.NG) GO TO 50
      A=DL(I1,1)
      B=DL(I2,2)
      C=DL(I1,3)
      KF=MA(I2,1)
      M=MA(I1,2)
      IP=MA(I2,3)
      S=A+B+C
      S3=S
      IF(S1.EQ.0.) S3=A+A+C
      GO TO(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,
     *22,23,24,25,26,27,28,29,30),NS
    1 F14=KF
      P11=IP
      P12=IP
      V13=0-M
      F21=KF
      P22=IP
      P23=0-IP
      V23=M
      F31=0-KF
      F34=KF
      P31=IP
      P33=IP
      P34=IP
      V33=0-M
      GO TO 45
    2 F13=KF
      P12=IP
      P13=IP
      V11=M
      F22=0-KF
      P21=0-IP
      P22=IP
      P23=IP
      V22=0-M
      F33=0-KF
      F35=KF
      P31=IP
      P33=IP
      P34=IP
      V32=M
      GO TO 45
    3 F12=KF
      P12=IP
      V14=M
      F21=KF
      P22=0-IP
      P23=IP
      V23=0-M
      F31=KF
      F33=0-KF
      P32=IP
      P33=IP
      V34=M
      GO TO 45
    4 F13=KF
      P13=IP
      V11=0-M
      F22=KF
      P22=IP
      P23=IP
      V22=0-M
      F32=KF
      F33=0-KF
      P31=IP
      P32=0-IP
      P33=IP
      V35=M
      GO TO 45
    5 F13=KF
      P12=IP
      P13=IP
      V12=0-M
      F21=KF
      P22=0-IP
      V24=M
      F34=KF
      F33=0-KF
      P31=IP
      P32=IP
      P33=IP
      V31=0-M
      GO TO 45
    6 F11=KF
      P12=IP
      V13=M
      F22=KF
      P21=IP
      P23=IP
      V22=M
      F33=0-KF
      F32=KF
      P34=0-IP
      P31=IP
      P33=IP
      V35=0-M
      GO TO 45
    7 F14=KF
      P13=IP
      V12=M
      F21=KF
      P22=IP
      V23=M
      F31=0-KF
      F34=KF
      P31=0-IP
      P32=IP
      P33=IP
      V33=0-M
      GO TO 45
    8 F12=KF
      P12=IP
      P11=IP
      V13=0-M
      F21=KF
      P22=0-IP
      V23=0-M
      F35=KF
      F32=0-KF
      P31=IP
      P34=IP
      P33=IP
      V33=M
      GO TO 45
    9 F14=KF
      P11=IP
      V13=M
      F22=KF
      P21=IP
      P22=0-IP
      V22=M
      F31=KF
      F33=0-KF
      P32=IP
      P34=IP
      P33=IP
      V34=0-M
      GO TO 45
   10 F11=0-KF
      P12=IP
      V12=M
      F21=KF
      P21=IP
      V23=0-M
      F32=KF
      F35=0-KF
      P31=0-IP
      P32=IP
      P33=IP
      V33=M
      GO TO 45
   11 F12=KF
      P12=IP
      P13=IP
      V14=M
      F22=KF
      P21=IP
      P22=IP
      V24=0-M
      F33=0-KF
      F34=KF
      P31=0-IP
      P33=IP
      P34=IP
      V32=M
      GO TO 45
   12 F12=0-KF
      P11=IP
      V13=0-M
      F21=KF
      P21=IP
      P22=IP
      V21=M
      F32=KF
      F33=0-KF
      P32=IP
      P33=IP
      P34=0-IP
      V35=0-M
      GO TO 45
   13 F12=KF
      P11=IP
      P12=IP
      V14=0-M
      F21=KF
      P23=IP
      V23=M
      F31=0-KF
      F34=KF
      P31=IP
      P32=0-IP
      P33=0-IP
      V33=0-M
      GO TO 45
   14 F11=KF
      P12=IP
      P13=IP
      V12=M
      F22=KF
      P21=IP
      P22=IP
      V22=0-M
      F35=0-KF
      F33=KF
      P34=IP
      P32=0-IP
      P33=0-IP
      V32=M
      GO TO 45
   15 F14=KF
      P12=IP
      V12=M
      F21=KF
      P23=IP
      P22=0-IP
      V21=0-M
      F31=0-KF
      F33=KF
      P31=IP
      P33=0-IP
      P34=0-IP
      V34=M
      GO TO 45
   16 F12=KF
      P13=IP
      V11=M
      F21=KF
      P22=IP
      V24=0-M
      F35=0-KF
      F33=KF
      P31=IP
      P34=IP
      P32=0-IP
      P33=IP
      V32=M
      GO TO 45
   17 F14=KF
      P11=IP
      P12=IP
      V12=M
      F22=KF
      P22=IP
      P23=0-IP
      V22=0-M
      F31=0-KF
      F33=KF
      P31=IP
      P33=IP
      P32=IP
      V31=M
      GO TO 45
   18 F13=KF
      P11=IP
      P12=IP
      V11=0-M
      F21=KF
      P21=IP
      P22=0-IP
      V24=0-M
      F33=0-KF
      F35=KF
      P32=IP
      P34=IP
      P33=IP
      V32=M
      GO TO 45
   19 F13=KF
      P12=IP
      V14=0-M
      F21=0-KF
      P21=IP
      P22=IP
      P23=IP
      V23=0-M
      F34=KF
      P31=IP
      P34=IP
      P33=0-IP
      P32=IP
      V31=M
      GO TO 45
   20 F11=KF
      P11=IP
      V12=M
      F22=KF
      P22=IP
      P21=0-IP
      V22=M
      F32=0-KF
      F35=KF
      P32=IP
      P34=IP
      P33=IP
      V33=0-M
      GO TO 45
   21 F12=KF
      P12=IP
      P13=IP
      V13=M
      F22=KF
      P22=0-IP
      P23=0-IP
      V21=0-M
      F31=KF
      F33=0-KF
      P34=IP
      P33=IP
      P32=IP
      V31=M
      GO TO 45
   22 F12=KF
      P12=IP
      V11=0-M
      F21=KF
      P22=IP
      P23=IP
      V23=M
      F32=KF
      F35=0-KF
      P32=0-IP
      P33=0-IP
      V35=0-M
      GO TO 45
   23 F13=KF
      P11=IP
      P12=0-IP
      V14=M
      F22=KF
      P23=IP
      V22=0-M
      F33=KF
      F34=0-KF
      P33=IP
      P32=IP
      V31=M
      GO TO 45
   24 F13=KF
      P11=IP
      P12=IP
      V12=M
      F21=KF
      P21=0-IP
      P23=0-IP
      V23=0-M
      F32=KF
      F33=0-KF
      P31=IP
      P32=IP
      P34=IP
      V35=M
      GO TO 45
   25 F12=KF
      P13=IP
      V13=0-M
      F22=KF
      P21=0-IP
      P23=IP
      V22=M
      F31=KF
      F34=0-KF
      P33=IP
      P32=IP
      P31=IP
      V32=0-M
      GO TO 45
   26 F13=KF
      P11=IP
      V11=M
      F21=KF
      P21=IP
      P22=IP
      V23=M
      F32=KF
      F35=0-KF
      P31=0-IP
      P32=0-IP
      P34=IP
      V33=0-M
      GO TO 45
   27 F14=KF
      P12=IP
      P13=IP
      V12=M
      F22=KF
      P21=0-IP
      P22=0-IP
      V22=0-M
      F31=0-KF
      F34=KF
      P33=IP
      P32=IP
      P34=IP
      V33=M
      GO TO 45
   28 F13=KF
      P11=IP
      P12=IP
      P13=IP
      V12=M
      F21=KF
      P23=IP
      P22=IP
      V21=M
      F35=KF
      F33=0-KF
      P32=IP
      P31=0-IP
      P33=IP
      V32=0-M
      GO TO 45
   29 F13=KF
      P11=IP
      V14=0-M
      F22=KF
      P21=IP
      P23=0-IP
      V22=0-M
      F34=0-KF
      F33=KF
      P33=IP
      P31=IP
      P34=IP
      V31=M
      GO TO 45
   30 F11=KF
      P13=IP
      P12=IP
      V13=M
      F22=KF
      P21=IP
      P23=IP
      P22=0-IP
      V22=M
      F33=KF
      F35=0-KF
      P32=IP
      P34=IP
      P33=IP
      V32=0-M
   45 CALL PAP(NC,NS,NG,A,B,C,S,S3,WA,WB,WC,NT,WT,NL,WL)
   50 CONTINUE
      PRINT *,'Jei baigete, iveskite nuli; jei norite testi - ',
     *'bet koki skaitmeni'
      READ *,IJ
      IF(IJ.NE.0) GO TO 99
   55 STOP
      END
      SUBROUTINE PAP(NC,NS,NG,A,B,C,S,S3,WA,WB,WC,NT,WT,NL,WL)
      DIMENSION WA(9),WB(9),WC(12),NT(23),WT(23),NL(22),WL(22)
      COMMON F11,F12,F13,F14,F21,F22,F31,F32,F33,F34,F35,
     *P11,P12,P13,P21,P22,P23,P31,P32,P33,P34,V11,V12,V13,V14,V21,
     *V22,V23,V24,V31,V32,V33,V34,V35
      IF(NC.NE.0) GO TO 5
      Q14=F14
      Q13D=F14+P13*C
      Q13K=Q13D+F13
      Q12D=Q13K+P12*B
      Q12K=Q12D+F12
      Q11=Q12K+P11*A+F11
      W14=0-V14
      W13D=-1*(V14+F14*C+0.5*P13*C*C)
      W13K=W13D-V13
      W12D=-1*(V14+V13+F14*(B+C)+F13*B+P13*C*(B+C/2)+P12*B*B/2)
      W12K=W12D-V12
      W11=-1*V11-V12-V13-V14-F14*S-F13*(A+B)-F12*A
      W11=W11-P13*C*(S-C/2)-P12*B*(A+B/2)-P11*A*A/2
      R31=(S3+A)*F31+F32*S3+(A+B)*F33+F34*B-V31-V32-V33-V34-V35
      R31=(S3+A/2)*P31*A+(S3-C/2)*P32*C+(B+A/2)*P33*A+R31
      R31=(P34*B*B/2+R31)/S3
      R32=F31+F32+F33+F34+F35+P31*A+P32*C+P33*A+P34*B-R31
      Q31=0-F31
      Q32K=Q31-P31*A
      Q32D=Q32K+R31
      Q33K=Q32D-P32*C
      Q33D=Q33K-F33
      Q34K=Q33D-P33*A
      Q34D=Q34K-F34
      Q35=0-R32
      W31=V31
      W32K=V31-F31*A-P31*A*A/2
      W32D=W32K+V32
      W33K=V31+V32-(A+C)*F31+R31*C-(C+A/2)*P31*A-P32*C*C/2
      W33D=W33K+V33
      W35=0-V35
      W34D=R32*B-V35-P34*B*B/2
      W34K=W34D-V34
      GO TO 10
    5 Q11=0-F11
      Q12K=Q11-P11*A
      Q12D=Q12K-F12
      Q13K=Q12D-P12*B
      Q13D=Q13K-F13
      Q14=Q13D-P13*C+F14
      W11=V11
      W12K=V11-F11*A-P11*A*A/2
      W12D=W12K+V12
      W13K=V11+V12-(A+B)*F11-F12*B-P12*B*B/2-(B+A/2)*P11*A
      W13D=W13K+V13
      W14=V11+V12+V13+V14-F11*S-(S-A)*F12-F13*C-(S-A/2)*P11*A
      W14=W14-(C+B/2)*P12*B-P13*C*C/2
      R32=(S3+B)*F35+S3*F34+(S3-A)*F33+F32*A+F31+V31+V32+V33+V34+V35
      R32=(S3+B/2)*P34*B+(S3-A/2)*P33*A+(A+C/2)*P32*C+P31*A*A/2+R32
      R32=R32/S3
      R31=F32+F33+F35+P31*A+P32*C+P33*A+P34*B-R32
      Q31=R31
      Q32K=Q31-P31*A
      Q32D=Q32K-F32
      Q33K=Q32D-P32*C
      Q33D=Q33K-F33
      Q34K=Q33D-P33*A
      Q34D=F35+P34*B
      Q35=F35
      W31=V31
      W32K=V31+R31*A-P31*A*A/2
      W32D=W32K+V32
      W33K=V31+V32+(A+C)*R31-(C+A/2)*P31*A-P32*C*C/2-F32*C
      W33D=W33K+V33
      W35=0-V35
      W34D=-1*(F35*B+V35+P34*B*B/2)
      W34K=W34D-V34
   10 R22=(A+B)*F22+A*F21+V21+V22+V23+V24+P21*A*A/2+(A+B/2)*P22*B
      R22=((S-C/2)*P23*C+R22)/S
      R21=F21+F22+P21*A+P22*B+P23*C-R22
      Q21=R21
      Q22K=Q21-P21*A
      Q22D=Q22K-F21
      Q24=0-R22
      Q23D=Q24+P23*C
      Q23K=Q23D+F22
      W21=V21
      W22K=V21+R21*A-P21*A*A/2
      W22D=W22K+V22
      W23K=V21+V22+(A+B)*R21-F21*B-(B+A/2)*P21*A-P22*B*B/2
      W23D=W23K+V23
      W24=0-V24
      WA(8)=0.0
      IF(Q14*Q13D.GE.0.) GO TO 6
      X=ABS(Q14/P13)
      W=0-V14-F14*X-P13*X*X/2
      WA(8)=ABS(W)
      IF(NC.EQ.0) GO TO 6
      X=C-X
      W=V11+V12+V13-(A+B+X)*F11-(B+X)*F12-X*F13-(B+X+A/2)*P11*A
      W=W-(X+B/2)*P12*B-P13*X*X/2
      WA(8)=ABS(W)
    6 WA(5)=0.0
      IF(Q12D*Q13K.GE.0.) GO TO 7
      X=ABS(Q13K/P12)
      W=0-V14-V13-(C+X)*F14-X*F13-(X+C/2)*P13*C-P12*X*X/2
      WA(5)=ABS(W)
      IF(NC.EQ.0) GO TO 7
      X=B-X
      W=V11+V12-(A+X)*F11-X*F12-(X+A/2)*P11*A-P12*X*X/2
      WA(5)=ABS(W)
    7 WA(2)=0.0
      IF(Q11*Q12K.GE.0.) GO TO 8
      X=ABS(Q12K/P11)
      W=0-V14-V13-V12-(B+C+X)*F14-(X+B/2)*P12*B-P11*X*X/2
      W=W-(B+X)*F13-X*F12-(B+X+C/2)*P13*C
      WA(2)=ABS(W)
      IF(NC.EQ.0) GO TO 8
      X=A-X
      W=V11-X*F11-P11*X*X/2
      WA(2)=ABS(W)
    8 WA(9)=ABS(W14)
      WA(7)=ABS(W13D)
      WA(6)=ABS(W13K)
      WA(4)=ABS(W12D)
      WA(3)=ABS(W12K)
      WA(1)=ABS(W11)
      WAX=WA(1)
      DO 13 J=2,9
      IF(WA(J).GT.WAX) WAX=WA(J)
   13 CONTINUE
      WB(1)=ABS(W21)
      WB(2)=0.0
      IF(Q21*Q22K.GE.0.) GO TO 16
      X=ABS(Q21/P21)
      W=R21*X+V21-P21*X*X/2
      WB(2)=ABS(W)
   16 WB(3)=ABS(W22K)
      WB(4)=ABS(W22D)
      WB(5)=0.0
      IF(Q22D*Q23K.GE.0.) GO TO 17
      X=ABS(Q22D/P22)
      W=(A+X)*R21+V21+V22-F21*X-(X+A/2)*P21*A-P22*X*X/2
      WB(5)=ABS(W)
   17 WB(6)=ABS(W23K)
      WB(7)=ABS(W23D)
      WB(8)=0.0
      IF(Q23D*Q24.GE.0.) GO TO 18
      X=ABS(Q24/P23)
      W=R22*X-V21-P23*X*X/2
      WB(8)=ABS(W)
   18 WB(9)=ABS(W24)
      WBX=WB(1)
      DO 23 J=2,9
      IF(WB(J).GT.WBX) WBX=WB(J)
   23 CONTINUE
      WC(1)=ABS(W31)
      WC(2)=0.0
      IF(Q31*Q32K.GE.0.) GO TO 26
      X=ABS(Q31/P31)
      F1=0-F31
      IF(NC.NE.0) F1=R31
      W=F1*X+V31-P31*X*X/2
      WC(2)=ABS(W)
   26 WC(3)=ABS(W32K)
      WC(4)=ABS(W32D)
      WC(5)=0.0
      IF(Q32D*Q33K.GE.0.) GO TO 27
      X=ABS(Q32D/P32)
      F1=0-F31
      IF(NC.NE.0) F1=R31
      F2=R31
      IF(NC.NE.0) F2=0-F32
      W=(A+X)*F1+F2*X+V31+V32-(X+A/2)*P31*A-P32*X*X/2
      WC(5)=ABS(W)
   27 WC(12)=ABS(W35)
      WC(11)=0.0
      IF(Q35*Q34D.GE.0.) GO TO 28
      X=ABS(Q35/P34)
      F1=R32
      IF(NC.NE.0) F1=0-F35
      W=F1*X-V31-P34*X*X/2
      WC(11)=ABS(W)
   28 WC(10)=ABS(W34D)
      WC(9)=ABS(W34K)
      WC(8)=0.0
      IF(Q33D*Q34K.GE.0.) GO TO 29
      X=ABS(Q34K/P33)
      F1=R32
      IF(NC.NE.0) F1=0-F35
      F2=0-F34
      IF(NC.NE.0) F2=R32
      W=F2*X+(B+X)*F1-V35-V34-(X+B/2)*P34*B-P33*X*X/2
      WC(8)=ABS(W)
   29 WC(7)=ABS(W33D)
      WC(6)=ABS(W33K)
      WCX=WC(1)
      DO 33 J=2,12
      IF(WC(J).GT.WCX) WCX=WC(J)
   33 CONTINUE
   61 BA=(WAX*100)**(1.0/3)
      IH=BA*2
      IB=BA*2.0
      BA=IB*0.5-0.5
      HA=IH-1.0
      DO 35 J=1,3
      BA=BA+0.5
      DO 34 K=1,3
      HA=HA+0.5
      WAR=BA*HA*HA*0.0025
      IF(WAR.GE.WAX) GO TO 51
   34 CONTINUE
   35 CONTINUE
   51 WR=WBX/0.16
      DO 37 JB=1,23
      IF(WT(JB).GE.WR) GO TO 38
   37 CONTINUE
   38 N2=NT(JB)
      WBR=WT(JB)
      WR=WCX/0.32
      DO 39 JC=1,22
      IF(WL(JC).GE.WR) GO TO 40
   39 CONTINUE
   40 N3=NL(JC)
      PRINT 45,NG,WAX,WBX,WCX
   45 FORMAT(1H ,5X,66('-')/1H ,5X,'|Uzd.Nr',I4,' |Max l.moment.|',
     *3(F10.2,'  |'))
      K2=0
      IF(JB.EQ.6.OR.JB.EQ.8.OR.JB.EQ.10.OR.JB.EQ.12.OR.JB.EQ.14.
     *OR.JB.EQ.16) K2=1
      K3=0
      IF(JC.EQ.7.OR.JC.EQ.9.OR.JC.EQ.9.OR.JC.EQ.11.OR.JC.EQ.13.
     *OR.JC.EQ.15.OR.JC.EQ.17) K3=1
   12 IF(K2.EQ.0.AND.K3.EQ.0) PRINT 41,NS,BA,HA,N2,N3
   41 FORMAT(1H ,5X,'|Sch.Nr',I4,' | Skerspjuvis |',F6.1,'x',F4.1,
     *' | Dvit.Nr',I3,' |  2[Nr',I3,'   |'/1H ,5X,66('-'))
      IF(K2.EQ.0.AND.K3.EQ.1) PRINT 42,NS,BA,HA,N2,N3
   42 FORMAT(1H ,5X,'|Sch.Nr',I4,' | Skerspjuvis |',F6.1,'x',F4.1,
     *' | Dvit.Nr',I3,' |  2[Nr',I3,'a  |'/1H ,5X,66('-'))
      IF(K2.EQ.1.AND.K3.EQ.0) PRINT 43,NS,BA,HA,N2,N3
   43 FORMAT(1H ,5X,'|Sch.Nr',I4,' | Skerspjuvis |',F6.1,'x',F4.1,
     *' | Dvit.Nr',I3,'a|   2[Nr',I3,'  |'/1H ,5X,66('-'))
      IF(K2.EQ.1.AND.K3.EQ.1) PRINT 44,NS,BA,HA,N2,N3
   44 FORMAT(1H ,5X,'|Sch.Nr',I4,' | Skerspjuvis |',F6.1,'x',F4.1,
     *' | Dvit.Nr',I3,'a|  2[Nr',I3,'a  |'/1H ,5X,66('-'))
      PRINT 46
   46 FORMAT(1H ,5X,'Paaiskinimai: skerspjuvis bxh - staciakampio ',
     *'plotis ir aukstis,'/1H ,19X,'Dvit.Nr - dvitejinio profilio ',
     *'Nr,'/1H ,19X,'2[Nr - dvieju loviniu profiliu Nr.')
   20 RETURN
      END
