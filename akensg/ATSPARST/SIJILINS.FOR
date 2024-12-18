      DIMENSION DL(7,3),MA(7,3),WA(9)
      COMMON IR,CK,F11,F12,F13,F14,P11,P12,P13,V11,V12,V13,V14
      DATA DL/0.6,0.7,0.8,0.9,1.0,1.1,1.2,0.8,0.9,1.0,1.1,1.2,1.3,1.4,
     *1.0,1.1,1.2,1.3,1.4,1.5,1.6/
      DATA MA/10,12,14,16,18,20,25,8,9,10,11,12,13,14,15,18,21,24,27,
     *30,33/
   99 PRINT *,'Iveskite uzduoties eil.Nr'
      READ *, NG
      NS=0
      I1=0
      I2=0
      I3=0
      CK=0.4
      DO 50 I=1,NG
      F11=0.0
      F12=0.0
      F13=0.0
      F14=0.0
      P11=0.0
      P12=0.0
      P13=0.0
      V11=0.0
      V12=0.0
      V13=0.0
      V14=0.0
      NC=0
      I1=I1+1
      IF(I1.GT.7) I1=1
      NS=NS+7
      IF(NS.GT.30) NS=NS-30
      I2=I2+2
      IF(I1.EQ.1) I2=I2+1
      IF(I2.GT.7) I2=I2-7
      I3=I3+3
      IF(I2.EQ.1) I3=I3+1
      IF(I3.GT.7) I3=I3-7
      CK=CK+0.1
      IF(CK.GT.1.3) CK=0.4
      IF(I.LT.NG) GO TO 50
      S1=0.5*NS-NS/2
      IF(S1.EQ.0.) NC=1
      A=DL(I1,1)
      B=DL(I2,2)
      C=DL(I3,3)
      KF=MA(I3,1)
      M=MA(I1,3)
      IP=MA(I2,2)
      S=A+B+C
      S3=S
      IF(S1.EQ.0.) S3=A+A+C
      GO TO(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,
     *22,23,24,25,26,27,28,29,30),NS
    1 F14=KF
      P11=IP
      P12=IP
      V13=0-M
      GO TO 45
    2 F13=KF
      P12=IP
      P13=IP
      V11=M
      GO TO 45
    3 F12=KF
      P12=IP
      V14=M
      GO TO 45
    4 F13=KF
      P13=IP
      V11=0-M
      GO TO 45
    5 F13=KF
      P12=IP
      P13=IP
      V12=0-M
      GO TO 45
    6 F11=KF
      P12=IP
      V13=M
      GO TO 45
    7 F14=KF
      P13=IP
      V12=M
      GO TO 45
    8 F12=KF
      P12=IP
      P11=IP
      V13=0-M
      GO TO 45
    9 F14=KF
      P11=IP
      V13=M
      GO TO 45
   10 F11=0-KF
      P12=IP
      V12=M
      GO TO 45
   11 F12=KF
      P12=IP
      P13=IP
      V14=M
      GO TO 45
   12 F12=0-KF
      P11=IP
      V13=0-M
      GO TO 45
   13 F12=KF
      P11=IP
      P12=IP
      V14=0-M
      GO TO 45
   14 F11=KF
      P12=IP
      P13=IP
      V12=M
      GO TO 45
   15 F14=KF
      P12=IP
      V12=M
      GO TO 45
   16 F12=KF
      P13=IP
      V11=M
      GO TO 45
   17 F14=KF
      P11=IP
      P12=IP
      V12=M
      GO TO 45
   18 F13=KF
      P11=IP
      P12=IP
      V11=0-M
      GO TO 45
   19 F13=KF
      P12=IP
      V14=0-M
      GO TO 45
   20 F11=KF
      P11=IP
      V12=M
      GO TO 45
   21 F12=KF
      P12=IP
      P13=IP
      V13=M
      GO TO 45
   22 F12=KF
      P12=IP
      V11=0-M
      GO TO 45
   23 F13=KF
      P11=IP
      P12=0-IP
      V14=M
      GO TO 45
   24 F13=KF
      P11=IP
      P12=IP
      V12=M
      GO TO 45
   25 F12=KF
      P13=IP
      V13=0-M
      GO TO 45
   26 F13=KF
      P11=IP
      V11=M
      GO TO 45
   27 F14=KF
      P12=IP
      P13=IP
      V12=M
      GO TO 45
   28 F13=KF
      P11=IP
      P12=IP
      P13=IP
      V12=M
      GO TO 45
   29 F13=KF
      P11=IP
      V14=0-M
      GO TO 45
   30 F11=KF
      P13=IP
      P12=IP
      V13=M
   45 IF(NC.NE.0) GO TO 55
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
      Y3=0
      IF(CK.LT.C) Y3=C-CK
      Y2=B+C-CK
      Y1=Y2+A
      V1=0-V14-V13-F14*CK-(CK-C)*F13-(CK-C/2)*P13*C-(CK-C)*(CK-C)*P12/2
      IF(CK.LE.C) V1=W13K
      V=0-V14-F14*CK-0.5*P13*CK*CK
      VI=(2*W11*Y1+W11*Y2+W12K*Y1+2*W12K*Y2)*A/6
      VI=VI+(2*W12D*Y2+W12D*Y3+V1*Y2+2*V1*Y3)*(Y2-Y3)/6
      VI=VI+(2*W13D+V)*Y3*Y3/6
      VI=VI+((Y1+Y2)*P11*A**3+(Y2+Y3)*P12*(Y2-Y3)**3)/24
      VI=VI+P13*Y3**4/24
      VP=((W11+W12K)*A+(W12D+V1)*(Y2-Y3)+(W13D+V)*Y3)/2
      VP=VP+(P11*A*A*A+P12*(Y2-Y3)**3+P13*Y3**3)/12
   90 WK=V
      IF(CK.GE.C) WK=V1
      GO TO 52
   55 Q11=0-F11
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
      Y2=0
      IF(CK.LT.A) Y2=A-CK
      Y3=B+A-CK
      Y4=Y3+C
      V1=V11+V12-F11*CK-(CK-A)*F12-(CK-A/2)*P11*A-(CK-A)*(CK-A)*P12/2
      IF(CK.LE.A) V1=W12D
      V=V11-F11*CK-0.5*P11*CK*CK
      VI=(2*W14*Y4+W14*Y3+W13D*Y4+2*W13D*Y3)*C/6
      VI=VI+(2*W13K*Y3+W13K*Y2+V1*Y3+2*V1*Y2)*(Y3-Y2)/6
      VI=VI+(2*W12K+V)*Y2*Y2/6
      VI=VI+((Y3+Y4)*P13*C**3+(Y2+Y3)*P12*(Y3-Y2)**3)/24
      VI=VI+P11*Y2**4/24
      VP=((W14+W13D)*C+(W13K+V1)*(Y3-Y2)+(W12K+V)*Y2)/2
      VP=VP+(P13*C*C*C+P12*(Y3-Y2)**3+P11*Y2**3)/12
      WK=V
      IF(CK.GE.A) WK=V1
   52 WA(8)=0.0
      IF(Q14*Q13D.GE.0.) GO TO 56
      X=ABS(Q14/P13)
      W=0-V14-F14*X-P13*X*X/2
      WA(8)=ABS(W)
      IF(NC.EQ.0) GO TO 6
      X=C-X
      W=V11+V12+V13-(A+B+X)*F11-(B+X)*F12-X*F13-(B+X+A/2)*P11*A
      W=W-(X+B/2)*P12*B-P13*X*X/2
      WA(8)=ABS(W)
   56 WA(5)=0.0
      IF(Q12D*Q13K.GE.0.) GO TO 57
      X=ABS(Q13K/P12)
      W=0-V14-V13-(C+X)*F14-X*F13-(X+C/2)*P13*C-P12*X*X/2
      WA(5)=ABS(W)
      IF(NC.EQ.0) GO TO 7
      X=B-X
      W=V11+V12-(A+X)*F11-X*F12-(X+A/2)*P11*A-P12*X*X/2
      WA(5)=ABS(W)
   57 WA(2)=0.0
      IF(Q11*Q12K.GE.0.) GO TO 58
      X=ABS(Q12K/P11)
      W=0-V14-V13-V12-(B+C+X)*F14-(X+B/2)*P12*B-P11*X*X/2
      W=W-(B+X)*F13-X*F12-(B+X+C/2)*P13*C
      WA(2)=ABS(W)
      IF(NC.EQ.0) GO TO 58
      X=A-X
      W=V11-X*F11-P11*X*X/2
      WA(2)=ABS(W)
   58 WA(9)=ABS(W14)
      WA(7)=ABS(W13D)
      WA(6)=ABS(W13K)
      WA(4)=ABS(W12D)
      WA(3)=ABS(W12K)
      WA(1)=ABS(W11)
      WAX=WA(1)
      DO 59 J=2,9
      IF(WA(J).GT.WAX) WAX=WA(J)
   59 CONTINUE
      BA=(WAX*100)**(1.0/3)
      IH=BA*2
      IB=BA*2.0
      BA=IB*0.5-0.5
      HA=IH-1.0
      DO 35 J=1,3
      BA=BA+0.5
      DO 34 K=1,3
      HA=HA+0.5
      WAR=BA*HA*HA*0.0025
      IF(WAR.GE.WAX) GO TO 36
   34 CONTINUE
   35 CONTINUE
   36 VI=0-VI
      VP=0-VP
      FIL=(120*VI/BA)*(1000/HA**3)
      PAS=VP*120/(BA*HA**3)
      PRINT 53, I,FIL,NS,BA,HA,PAS
   53 FORMAT(1H ,5X,56('-')/1H ,5X,'| Uzd.Nr',I4,' |',6X,'bxh=',6X,
     *'|   Ilinkis',F7.2,' mm    |'/1H ,5X,'| Sch.Nr',I4,' |',F6.1,
     *'X',F5.1,' cm | Deviacija',F9.6,' rad |'/1H ,5X,56('-'))
   50 CONTINUE
      PRINT *,'Jei baigete, iveskite nuli,o jei norite testi - ',
     *'bet koki skaitmeni'
      READ *, IJ
      IF(IJ.NE.0) GO TO 99
   60 STOP
      END
