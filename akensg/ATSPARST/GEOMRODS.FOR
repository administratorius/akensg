      DIMENSION DL(20,6),DK(20,9),BM(6)
      DIMENSION XM(6),YM(6)
      COMMON X1,X2,X3,X4,Y1,Y2,Y3,Y4,DX1,DY1,U2,V2,UV2,U3,V3,UV3,
     *U4,V4,UV4
      DATA DL/8.,10.,12.,14.,14.,16.,16.,18.,18.,20.,20.,20.,18.,
     *18.,16.,16.,14.,14.,12.,10.,8.98,10.9,13.3,15.6,17.0,18.1,
     *19.5,20.7,22.2,23.4,25.2,23.4,22.2,20.7,19.5,18.1,17.0,
     *15.6,13.3,10.9,4.0,4.6,5.2,5.8,6.2,6.4,6.8,7.0,7.4,7.6,
     *8.0,7.6,7.4,7.0,6.8,6.4,6.2,5.8,5.2,4.6,1.31,1.44,1.54,1.67,
     *1.87,1.8,2.0,1.94,2.13,2.07,2.28,2.07,2.13,1.94,2.0,1.8,
     *1.87,1.67,1.54,1.44,89.4,174.0,304.0,491.0,545.0,747.0,
     *823.0,1090.0,1190.0,1520.0,1670.0,1520.0,1190.0,1090.0,823.0,
     *747.0,545.0,491.0,304.0,174.0,12.8,20.4,31.2,45.4,57.5,63.3,
     *78.8,86.0,105.0,113.0,139.0,113.0,105.0,86.0,78.8,63.3,57.5,
     *45.4,31.2,20.4/
      DATA DK /7.0,7.5,8.0,8.0,9.0,10.0,10.0,11.0,11.0,12.5,12.5,
     *12.5,10.0,9.0,7.5,9.0,12.5,7.5,6.3,6.3,4.5,5.0,5.0,5.0,
     *5.6,6.3,6.3,7.0,7.0,8.0,8.0,8.0,6.3,5.6,5.0,5.6,8.0,5.0,
     *4.0,4.0,5.59,7.25,6.36,7.55,8.54,12.6,15.5,11.4,13.9,14.1,
     *23.4,16.0,9.59,11.18,9.47,7.86,19.7,6.11,4.04,7.68,27.8,40.9,
     *41.6,49.0,70.6,127.0,154.0,142.0,172.0,227.0,365.0,256.0,
     *98.3,90.9,52.4,65.3,312.0,34.8,16.3,29.6,9.05,14.6,12.7,
     *14.8,21.2,39.2,47.1,45.6,54.6,73.7,117.0,83.0,30.6,27.1,
     *18.5,19.7,100.0,12.5,5.16,9.15,1.05,1.21,1.13,1.17,1.28,
     *1.5,1.58,1.58,1.64,1.8,2.0,1.84,1.42,1.36,1.29,1.26,1.92,
     *1.17,0.91,1.07,2.28,2.44,2.6,2.65,2.95,3.32,3.4,3.55,3.61,
     *4.01,4.22,4.05,3.23,3.04,2.52,2.92,4.14,2.39,2.03,2.2,0.406,
     *0.435,0.387,0.386,0.384,0.391,0.387,0.402,0.4,0.407,0.4,0.406,
     *0.393,0.38,0.43,0.384,0.404,0.436,0.397,0.386,5.,6.,5.,
     *6.,6.,8.,10.,6.5,8.,7.,12.,8.,6.,8.,8.,5.5,10.,5.,4.,8./
      DATA BM /10.,11.,12.,13.,14.,15./
    7 PRINT *,'Iveskite uzduoties eil.Nr'
      READ(5,*) NG
      NS=0
      I1=0
      IB=0
      ND=0
      PI=3.141593
      N=0
      DO 50 I=1,1200
      NC=0
      XO=0.0
      YO=0.0
      DS4=0.0
      ND=ND+1
      IF(ND.GT.30) ND=ND-30
      IF(ND.EQ.1) NS=NS+1
      I1=I1+3
      IF(I1.GT.20) I1=I1-20
      NS=NS+7
      IF(NS.GT.30) NS=NS-30
      IB=IB+1
      IF(IB.GT.6) IB=IB-6
      B1=BM(IB)
      H1=2*B1
      A1=(2-PI/16)*B1*B1
      D2=0.5*B1
      H2=B1
      B2=0.5*H2
      F=2.0/3-PI/1024
      DX1=F*B1**4
      DY1=(F-0.5)*B1**4
      A2=0.25*B1*B1
      DX2=B1**4/72
      DY2=DX2/4
      DS2=B1**4/288
      IF(NS.EQ.1.OR.NS.EQ.3.OR.NS.EQ.4.OR.NS.EQ.6.OR.NS.EQ.7.
     *OR.NS.EQ.9.OR.NS.EQ.10.OR.NS.EQ.11.OR.NS.EQ.13.OR.NS.EQ.18.
     *OR.NS.EQ.20.OR.NS.EQ.21.OR.NS.EQ.28.OR.NS.EQ.30) NC=1
      IF(NC.EQ.1) GO TO 9
      H4=DK(I1,1)
      B4=DK(I1,2)
      KD=DK(I1,9)
      KH=H4*10
      KB=B4*10
      A4=DK(I1,3)
      DX4=DK(I1,4)
      DY4=DK(I1,5)
      XO=DK(I1,6)
      YO=DK(I1,7)
      TG=DK(I1,8)
      TG=2*TG/(1-TG*TG)
      DS4=(DX4-DY4)*TG*0.5
      GO TO 10
    9 H4=DL(I1,1)
      LN=H4
      B4=DL(I1,3)
      A4=DL(I1,2)
      DX4=DL(I1,5)
      DY4=DL(I1,6)
      Z=DL(I1,4)
   10 GO TO(11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,
     *29,30,31,32,33,34,35,36,37,38,39,40),NS
   11 X1=B4+B1/2
      X2=B4+2*B1/3
      X3=X2
      X4=B4-Z
      Y1=B1
      Y2=H1+B1/6
      Y3=B1/6
      Y4=H4/2
      U2=DY2
      V2=DX2
      U3=U2
      V3=V2
      UV2=DS2
      UV3=UV2
      U4=DX4
      V4=DY4
      UV4=DS4
      CALL PAP(A1,A2,A4,A,XC,YC,AL,UX,UN,S1,DXC,DYC,UVC,RU,RV)
      IF(ABS(AL).LT.10.) GO TO 50
      N=N+1
      IF(N.GT.NG) GO TO 101
      XM(1)=B4-XC
      XM(2)=B1+B4-XC
      XM(3)=XM(2)
      XM(4)=B4-XC
      XM(5)=0-XC
      XM(6)=0-XC
      YM(1)=2*B1-YC
      YM(2)=2.5*B1-YC
      YM(3)=B1/2-YC
      YM(4)=0-YC
      YM(5)=0-YC
      YM(6)=H4-YC
      GO TO 44
   12 X1=B1/2
      X2=7*B1/6
      X3=B1/3
      X4=YO
      Y1=B1
      Y2=B1/3
      Y3=B1/6
      Y4=H1+XO
      U2=DX2
      V2=DY2
      U3=V2
      V3=U2
      UV2=0-DS2
      UV3=UV2
      U4=DY4
      V4=DX4
      UV4=0-DS4
      CALL PAP(A1,A2,A4,A,XC,YC,AL,UX,UN,S1,DXC,DYC,UVC,RU,RV)
      IF(ABS(AL).LT.10.) GO TO 50
      N=N+1
      IF(N.GT.NG) GO TO 101
      XM(1)=0-XC
      XM(2)=B1-XC
      IF(H4.GT.B1) XM(2)=H4-YC
      XM(3)=1.5*B1-XC
      XM(4)=0-XC
      XM(5)=B1-XC
      XM(6)=0.0
      YM(1)=2*B1+B4-YC
      YM(2)=2*B1-YC
      YM(3)=0-YC
      YM(4)=B1/2-YC
      YM(5)=0-YC
      YM(6)=0.0
      GO TO 44
   13 X1=B1
      X2=B1/3
      X3=7*B1/6
      X4=(B1+H4)/2
      Y1=B1+B4
      Y2=H1+B4-B1/3
      Y3=H1+B4-B1/6
      Y4=B4-Z
      U2=DX2
      V2=DY2
      U3=V2
      V3=U2
      UV2=0-DS2
      UV3=UV2
      U4=DY4
      V4=DX4
      UV4=DS4
      CALL PAP(A1,A2,A4,A,XC,YC,AL,UX,UN,S1,DXC,DYC,UVC,RU,RV)
      IF(ABS(AL).LT.10.) GO TO 50
      N=N+1
      IF(N.GT.NG) GO TO 101
      XM(1)=0-XC
      XM(2)=0.5*B1-XC
      XM(3)=1.5*B1-XC
      XM(4)=1.5*B1-XC
      IF(H4.GT.B1) XM(4)=0.5*B1+H4-XC
      XM(5)=0.5*B1+H4-XC
      XM(6)=0.5*B1-XC
      YM(1)=2*B1+B4-YC
      YM(2)=YM(1)
      YM(3)=1.5*B1+B4-YC
      YM(4)=B4-YC
      YM(5)=0-YC
      YM(6)=0-YC
      GO TO 44
   14 X1=B1/2
      X2=B1/3
      X3=X2*2
      X4=B1+Z
      Y1=B1
      Y2=H1+B1/6
      Y3=B1/6
      Y4=H4/2
      U2=DY2
      V2=DX2
      U3=U2
      V3=V2
      UV2=0-DS2
      UV3=DS2
      U4=DX4
      V4=DY4
      UV4=DS4
      CALL PAP(A1,A2,A4,A,XC,YC,AL,UX,UN,S1,DXC,DYC,UVC,RU,RV)
      IF(ABS(AL).LT.4.) GO TO 50
      N=N+1
      IF(N.GT.NG) GO TO 101
      XM(1)=0-XC
      XM(2)=B1-XC
      XM(3)=B1+B4-XC
      XM(4)=B1+B4-XC
      XM(5)=0-XC
      XM(6)=0.0
      YM(1)=2.5*B1-YC
      YM(2)=2*B1-YC
      YM(3)=H4-YC
      YM(4)=0-YC
      YM(5)=0-YC
      YM(6)=0.0
      GO TO 44
   15 X1=B4+B1/2
      X2=B4+2*B1/3
      X3=B4+B1/6
      X4=B4-XO
      Y1=B1
      Y2=H1+B1/6
      Y3=B1/3
      Y4=H1-YO
      U2=DY2
      V2=DX2
      U3=V2
      V3=U2
      UV2=DS2
      UV3=0-UV2
      U4=DX4
      V4=DY4
      UV4=0-DS4
      CALL PAP(A1,A2,A4,A,XC,YC,AL,UX,UN,S1,DXC,DYC,UVC,RU,RV)
      IF(ABS(AL).LT.5.) GO TO 50
      N=N+1
      IF(N.GT.NG) GO TO 101
      XM(1)=0-XC
      XM(2)=B4+B1-XC
      XM(3)=B4+B1-XC
      XM(4)=0.5*B1+B4-XC
      XM(5)=B4-XC
      XM(6)=0.0
      YM(1)=2*B1-YC
      YM(2)=2.5*B1-YC
      YM(3)=0-YC
      YM(4)=0-YC
      YM(5)=B1-YC
      IF(H4.GT.B1) YM(5)=2*B1-H4-YC
      YM(6)=0.0
      GO TO 44
   16 X1=B1/2
      X2=7*B1/6
      X3=B1/3
      X4=H4/2
      Y1=B1+B4
      Y2=H1+B4-2*B1/3
      Y3=H1+B4-B1/6
      Y4=B4-Z
      U2=DX2
      V2=DY2
      U3=V2
      V3=U2
      UV2=0-DS2
      UV3=DS2
      U4=DY4
      V4=DX4
      UV4=DS4
      CALL PAP(A1,A2,A4,A,XC,YC,AL,UX,UN,S1,DXC,DYC,UVC,RU,RV)
      IF(ABS(AL).LT.10.) GO TO 50
      N=N+1
      IF(N.GT.NG) GO TO 101
      XM(1)=0-XC
      XM(2)=B1-XC
      XM(3)=1.5*B1-XC
      XM(4)=H4-XC
      IF(B1.GT.B4) XM(4)=B1-XC
      XM(5)=H4-XC
      XM(6)=0-XC
      YM(1)=1.5*B1+B4-YC
      YM(2)=2*B1+B4-YC
      YM(3)=B1+B4-YC
      YM(4)=B4-YC
      YM(5)=0-YC
      YM(6)=0-YC
      GO TO 44
   17 X1=B1/2
      X2=7*B1/6
      X3=B1/3
      X4=H4/2
      Y1=B1
      Y2=H1-B1/3
      Y3=B1/6
      Y4=H1+Z
      U2=DX2
      V2=DY2
      U3=V2
      V3=U2
      UV2=DS2
      UV3=0-UV2
      U4=DY4
      V4=DX4
      UV4=DS4
      CALL PAP(A1,A2,A4,A,XC,YC,AL,UX,UN,S1,DXC,DYC,UVC,RU,RV)
      IF(ABS(AL).LT.6.) GO TO 50
      N=N+1
      IF(N.GT.NG) GO TO 101
      XM(1)=0-XC
      XM(2)=H4-XC
      XM(3)=1.5*B1-XC
      XM(4)=B1-XC
      XM(5)=0-XC
      XM(6)=0.0
      YM(1)=2*B1+B4-YC
      YM(2)=2*B1+B4-YC
      YM(3)=2*B1-YC
      YM(4)=0-YC
      YM(5)=0.5*B1-YC
      YM(6)=0.0
      GO TO 44
   18 X1=B1
      X2=B1/3
      X3=7*B1/6
      X4=1.5*B1-YO
      Y1=B1
      Y2=B1/3
      Y3=B1/6
      Y4=H1+XO
      U2=DX2
      V2=DY2
      U3=V2
      V3=U2
      UV2=DS2
      UV3=UV2
      U4=DY4
      V4=DX4
      UV4=DS4
      CALL PAP(A1,A2,A4,A,XC,YC,AL,UX,UN,S1,DXC,DYC,UVC,RU,RV)
      IF(ABS(AL).LT.10.) GO TO 50
      N=N+1
      IF(N.GT.NG) GO TO 101
      XM(1)=0.5*B1-XC
      IF(H4.GT.B1) XM(1)=1.5*B1-H4-YC
      XM(2)=1.5*B1-XC
      XM(3)=1.5*B1-XC
      XM(4)=0.5*B1-XC
      XM(5)=0-XC
      XM(6)=0.0
      YM(1)=2*B1-YC
      YM(2)=2*B1+B4-YC
      YM(3)=0.5*B1-YC
      YM(4)=0-YC
      YM(5)=0-YC
      YM(6)=0.0
      GO TO 44
   19 X1=B1/2
      X2=7*B1/6
      X3=5*B1/6
      X4=H4/2
      Y1=B1+B4
      Y2=B4+B1/3
      Y3=B4+H1-B1/3
      Y4=B4-Z
      U2=DX2
      V2=DY2
      U3=U2
      V3=V2
      UV2=0-DS2
      UV3=UV2
      U4=DY4
      V4=DX4
      UV4=DS4
      CALL PAP(A1,A2,A4,A,XC,YC,AL,UX,UN,S1,DXC,DYC,UVC,RU,RV)
      IF(ABS(AL).LT.10.) GO TO 50
      N=N+1
      IF(N.GT.NG) GO TO 101
      XM(1)=0-XC
      XM(2)=0.5*B1-XC
      XM(3)=1.5*B1-XC
      XM(4)=H4-XC
      XM(5)=0-XC
      XM(6)=0.0
      YM(1)=2*B1+B4-YC
      YM(2)=2*B1+B4-YC
      YM(3)=B4-YC
      YM(4)=0-YC
      YM(5)=0-YC
      YM(6)=0.0
      GO TO 44
   20 X1=B1
      X2=B1/3
      X3=5*B1/6
      X4=(B1+H4)/2
      Y1=B1
      Y2=4*B1/3
      Y3=B1/6
      Y4=H1+Z
      U2=DX2
      V2=DY2
      U3=V2
      V3=U2
      UV2=DS2
      UV3=0-UV2
      U4=DY4
      V4=DX4
      UV4=DS4
      CALL PAP(A1,A2,A4,A,XC,YC,AL,UX,UN,S1,DXC,DYC,UVC,RU,RV)
      IF(ABS(AL).LT.8.) GO TO 50
      N=N+1
      IF(N.GT.NG) GO TO 101
      XM(1)=B1/2-XC
      XM(2)=0.5*B1+H4-XC
      XM(3)=1.5*B1-XC
      IF(H4.GT.B1) XM(3)=XM(2)
      XM(4)=1.5*B1-XC
      XM(5)=B1/2-XC
      XM(6)=0-XC
      YM(1)=H1+B4-YC
      YM(2)=H1+B4-YC
      YM(3)=H1-YC
      YM(4)=0-YC
      YM(5)=B1/2-YC
      YM(6)=B1-YC
      GO TO 44
   21 X1=B1/2
      X2=2*B1/3
      X3=B1/6
      X4=B1+Z
      Y1=B1
      Y2=H1+B1/6
      Y3=B1/3
      Y4=H4/2
      U2=DY2
      V2=DX2
      U3=V2
      V3=U2
      UV2=DS2
      UV3=0-UV2
      U4=DX4
      V4=DY4
      UV4=DS4
      CALL PAP(A1,A2,A4,A,XC,YC,AL,UX,UN,S1,DXC,DYC,UVC,RU,RV)
      IF(ABS(AL).LT.5.) GO TO 50
      N=N+1
      IF(N.GT.NG) GO TO 101
      XM(1)=0-XC
      XM(2)=B1-XC
      XM(3)=B4+B1-XC
      XM(4)=B4+B1-XC
      XM(5)=B1/2-XC
      XM(6)=0-XC
      YM(1)=2*B1-YC
      YM(2)=2.5*B1-YC
      YM(3)=H4-YC
      YM(4)=0-YC
      YM(5)=0-YC
      YM(6)=B1-YC
      GO TO 44
   22 X1=B4+B1/2
      X2=B4+2*B1/3
      X3=B4+B1/3
      X4=B4-XO
      Y1=B1
      Y2=H1+B1/6
      Y3=B1/6
      Y4=H1-YO
      U2=DY2
      V2=DX2
      U3=U2
      V3=V2
      UV2=DS2
      UV3=0-UV2
      U4=DX4
      V4=DY4
      UV4=0-DS4
      CALL PAP(A1,A2,A4,A,XC,YC,AL,UX,UN,S1,DXC,DYC,UVC,RU,RV)
      IF(ABS(AL).LT.3.) GO TO 50
      N=N+1
      IF(N.GT.NG) GO TO 101
      XM(1)=0-XC
      XM(2)=B1+B4-XC
      XM(3)=B1+B4-XC
      XM(4)=B4-XC
      XM(5)=0.0
      XM(6)=0.0
      YM(1)=2*B1-YC
      YM(2)=2.5*B1-YC
      YM(3)=0-YC
      YM(4)=0.5*B1-YC
      YM(5)=0.0
      YM(6)=0.0
      GO TO 44
   23 X1=B4+B1/2
      X2=B4+2*B1/3
      X3=B4+5*B1/6
      X4=B4-Z
      Y1=B1
      Y2=H1+B1/6
      Y3=B1/3
      Y4=H4/2
      U2=DY2
      V2=DX2
      U3=V2
      V3=U2
      UV2=DS2
      UV3=UV2
      U4=DX4
      V4=DY4
      UV4=DS4
      CALL PAP(A1,A2,A4,A,XC,YC,AL,UX,UN,S1,DXC,DYC,UVC,RU,RV)
      IF(ABS(AL).LT.10.) GO TO 50
      N=N+1
      IF(N.GT.NG) GO TO 101
      XM(1)=B4-XC
      XM(2)=B1+B4-XC
      XM(3)=B4+B1-XC
      XM(4)=0.5*B1+B4-XC
      XM(5)=0-XC
      XM(6)=0-XC
      YM(1)=2*B1-YC
      YM(2)=2.5*B1-YC
      YM(3)=B1-YC
      YM(4)=0-YC
      YM(5)=0-YC
      YM(6)=H4-YC
      GO TO 44
   24 X1=B1/2
      X2=2*B1/3
      X3=5*B1/6
      X4=B1+XO
      Y1=B1*1.5
      Y2=B1/3
      Y3=2.5*B1-B1/3
      Y4=B1/2+YO
      U2=DY2
      V2=DX2
      U3=V2
      V3=U2
      UV2=0-DS2
      UV3=UV2
      U4=DX4
      V4=DY4
      UV4=0-DS4
      CALL PAP(A1,A2,A4,A,XC,YC,AL,UX,UN,S1,DXC,DYC,UVC,RU,RV)
      IF(ABS(AL).LT.10.) GO TO 50
      N=N+1
      IF(N.GT.NG) GO TO 101
      XM(1)=0-XC
      XM(2)=0.5*B1-XC
      XM(3)=B1-XC
      XM(4)=B4+B1-XC
      XM(5)=B1-XC
      XM(6)=0-XC
      YM(1)=2.5*B1-YC
      YM(2)=2.5*B1-YC
      YM(3)=1.5*B1-YC
      IF(H4.GT.B1) YM(3)=0.5*B1+H4-YC
      YM(4)=0.5*B1-YC
      YM(5)=0-YC
      YM(6)=0.5*B1-YC
      GO TO 44
   25 X1=B1
      X2=B1/3
      X3=7*B1/6
      X4=1.5*B1-YO
      Y1=B1+B4
      Y2=B4+4*B1/3
      Y3=B4+H1-B1/6
      Y4=B4-XO
      U2=DX2
      V2=DY2
      U3=V2
      V3=U2
      UV2=DS2
      UV3=0-UV2
      U4=DY4
      V4=DX4
      UV4=0-DS4
      CALL PAP(A1,A2,A4,A,XC,YC,AL,UX,UN,S1,DXC,DYC,UVC,RU,RV)
      IF(ABS(AL).LT.10.) GO TO 50
      N=N+1
      IF(N.GT.NG) GO TO 101
      XM(1)=0.5*B1-XC
      XM(2)=1.5*B1-XC
      XM(3)=1.5*B1-XC
      XM(4)=0.5*B1-XC
      IF(H4.GT.B1) XM(4)=1.5*B1-H4-XC
      XM(5)=0-XC
      XM(6)=0.0
      YM(1)=2*B1+B4-YC
      YM(2)=1.5*B1+B4-YC
      YM(3)=0-YC
      YM(4)=B4-YC
      YM(5)=B1+B4-YC
      YM(6)=0.0
      GO TO 44
   26 X1=B1/2
      X2=7*B1/6
      X3=2*B1/3
      X4=B1-YO
      IF(B1.GE.H4) GO TO 54
      PR=H4-B1
      X1=X1+PR
      X2=X2+PR
      X3=X3+PR
      X4=H4-YO
   54 Y1=B1
      Y2=4*B1/3
      Y3=B1/6
      Y4=H1+XO
      U2=DX2
      V2=DY2
      U3=V2
      V3=U2
      UV2=0-DS2
      UV3=DS2
      U4=DY4
      V4=DX4
      UV4=DS4
      CALL PAP(A1,A2,A4,A,XC,YC,AL,UX,UN,S1,DXC,DYC,UVC,RU,RV)
      IF(ABS(AL).LT.10.) GO TO 50
      N=N+1
      IF(N.GT.NG) GO TO 101
      XM(1)=0-XC
      XM(2)=B1-XC
      XM(3)=1.5*B1-XC
      XM(4)=B1-XC
      XM(5)=0-XC
      XM(6)=0.0
      IF(B1.GE.H4) GO TO 55
      PR=H4-B1
      XM(1)=0-XC
      XM(2)=XM(2)+PR
      XM(3)=XM(3)+PR
      XM(4)=XM(4)+PR
      XM(5)=XM(5)+PR
      XM(6)=0.0
   55 YM(1)=2*B1-YC
      YM(2)=2*B1+B4-YC
      YM(3)=B1-YC
      YM(4)=0.5*B1-YC
      YM(5)=0-YC
      YM(6)=0.0
      GO TO 44
   27 X1=B1
      X2=B1/3
      X3=5*B1/6
      X4=1.5*B1-H4+YO
      Y1=B1
      Y2=5*B1/3
      Y3=B1/6
      Y4=H1+XO
      U2=DX2
      V2=DY2
      U3=V2
      V3=U2
      UV2=0-DS2
      UV3=UV2
      U4=DY4
      V4=DX4
      UV4=0-DS4
      CALL PAP(A1,A2,A4,A,XC,YC,AL,UX,UN,S1,DXC,DYC,UVC,RU,RV)
      IF(ABS(AL).LT.10.) GO TO 50
      N=N+1
      IF(N.GT.NG) GO TO 101
      XM(1)=0-XC
      XM(2)=1.5*B1-H4-XC
      XM(3)=1.5*B1-XC
      XM(4)=1.5*B1-XC
      XM(5)=0.5*B1-XC
      XM(6)=0.0
      YM(1)=2*B1-YC
      YM(2)=2*B1+B4-YC
      YM(3)=2*B1-YC
      YM(4)=0-YC
      YM(5)=0.5*B1-YC
      YM(6)=0.0
      GO TO 44
   28 X1=B1/2
      X2=B1/3
      X3=B1/6
      X4=B1-H4/2
      IF(B1.GE.H4) GO TO 56
      PR=H4-B1
      X1=X1+PR
      X2=X2+PR
      X3=X3+PR
      X4=H4/2
   56 Y1=B1*1.5
      Y2=B1/3
      Y3=6.5*B1/3
      Y4=2.5*B1+Z
      U2=DY2
      V2=DX2
      U3=V2
      V3=U2
      UV2=DS2
      UV3=DS2
      U4=DY4
      V4=DX4
      UV4=DS4
      CALL PAP(A1,A2,A4,A,XC,YC,AL,UX,UN,S1,DXC,DYC,UVC,RU,RV)
      IF(ABS(AL).LT.5.) GO TO 50
      N=N+1
      IF(N.GT.NG) GO TO 101
      XM(1)=B1-H4-XC
      XM(2)=B1-XC
      XM(3)=B1-XC
      XM(4)=0-XC
      XM(5)=0-XC
      XM(6)=0.0
      IF(B1.GE.H4) GO TO 51
      PR=H4-B1
      XM(1)=0-XC
      XM(2)=H4-XC
      XM(3)=XM(2)
      XM(4)=XM(4)+PR
      XM(5)=XM(5)+PR
   51 YM(1)=2.5*B1+B4-YC
      YM(2)=2.5*B1+B4-YC
      YM(3)=0.5*B1-YC
      YM(4)=0-YC
      YM(5)=1.5*B1-YC
      YM(6)=0.0
      GO TO 44
   29 X1=B1/2
      X2=B1/3
      X3=X2
      X4=B1+XO
      Y1=B1
      Y2=H1+B1/6
      Y3=B1/6
      Y4=YO
      U2=DY2
      V2=DX2
      U3=U2
      V3=V2
      UV2=0-DS2
      UV3=UV2
      U4=DX4
      V4=DY4
      UV4=0-DS4
      CALL PAP(A1,A2,A4,A,XC,YC,AL,UX,UN,S1,DXC,DYC,UVC,RU,RV)
      IF(ABS(AL).LT.10.) GO TO 50
      N=N+1
      IF(N.GT.NG) GO TO 101
      XM(1)=0-XC
      XM(2)=B1-XC
      XM(3)=B1+B4-XC
      XM(4)=B1-XC
      XM(5)=0-XC
      XM(6)=0.0
      YM(1)=2.5*B1-YC
      YM(2)=2*B1-YC
      YM(3)=0-YC
      YM(4)=0-YC
      YM(5)=0.5*B1-YC
      YM(6)=0.0
      GO TO 44
   30 X1=B4+B1/2
      X2=B4+B1/3
      X3=B4+B1/6
      X4=B4-Z
      Y1=B1
      Y2=H1+B1/6
      Y3=B1/3
      Y4=2.5*B1-H4/2
      U2=DY2
      V2=DX2
      U3=V2
      V3=U2
      UV2=0-DS2
      UV3=UV2
      U4=DX4
      V4=DY4
      UV4=DS4
      CALL PAP(A1,A2,A4,A,XC,YC,AL,UX,UN,S1,DXC,DYC,UVC,RU,RV)
      IF(ABS(AL).LT.10.) GO TO 50
      N=N+1
      IF(N.GT.NG) GO TO 101
      XM(1)=0-XC
      XM(2)=B4-XC
      XM(3)=B1+B4-XC
      XM(4)=B1+B4-XC
      XM(5)=0.5*B1+B4-XC
      XM(6)=0-XC
      YM(1)=2.5*B1-YC
      YM(2)=2.5*B1-YC
      YM(3)=2*B1-YC
      YM(4)=0-YC
      YM(5)=0-YC
      YM(6)=2.5*B1-H4-YC
      GO TO 44
   31 X1=B1/2
      X2=7*B1/6
      X3=5*B1/6
      X4=H4/2
      Y1=B1+B4
      Y2=H1+B4-B1/3
      Y3=B1/3+B4
      Y4=B4-Z
      U2=DX2
      V2=DY2
      U3=U2
      V3=V2
      UV2=DS2
      UV3=UV2
      U4=DY4
      V4=DX4
      UV4=DS4
      CALL PAP(A1,A2,A4,A,XC,YC,AL,UX,UN,S1,DXC,DYC,UVC,RU,RV)
      IF(ABS(AL).LT.10.) GO TO 50
      N=N+1
      IF(N.GT.NG) GO TO 101
      XM(1)=0-XC
      XM(2)=1.5*B1-XC
      XM(3)=H4-XC
      XM(4)=H4-XC
      XM(5)=0-XC
      XM(6)=0.0
      YM(1)=2*B1+B4-YC
      YM(2)=2*B1+B4-YC
      YM(3)=B4-YC
      YM(4)=0-YC
      YM(5)=0-YC
      YM(6)=0.0
      GO TO 44
   32 X1=B1
      X2=B1/3
      X3=7*B1/6
      X4=1.5*B1-YO
      Y1=B1
      Y2=2*B1/3
      Y3=B1/6
      Y4=H1+XO
      U2=DX2
      V2=DY2
      U3=V2
      V3=U2
      UV2=0-DS2
      UV3=DS2
      U4=DY4
      V4=DX4
      UV4=DS4
      CALL PAP(A1,A2,A4,A,XC,YC,AL,UX,UN,S1,DXC,DYC,UVC,RU,RV)
      IF(ABS(AL).LT.10.) GO TO 50
      N=N+1
      IF(N.GT.NG) GO TO 101
      XM(1)=0.5*B1-XC
      IF(H4.GT.B1) XM(1)=1.5*B1-H4-XC
      XM(2)=1.5*B1-XC
      XM(3)=1.5*B1-XC
      XM(4)=0.5*B1-XC
      XM(5)=0-XC
      XM(6)=0.0
      YM(1)=2*B1-YC
      YM(2)=2*B1+B4-YC
      YM(3)=0.5*B1-YC
      YM(4)=0-YC
      YM(5)=B1-YC
      YM(6)=0.0
      GO TO 44
   33 X1=B1/2
      X2=7*B1/6
      X3=B1/3
      X4=YO
      Y1=B1+B4
      Y2=B4+H1-B1/3
      Y3=B4+H1-B1/6
      Y4=B4-XO
      U2=DX2
      V2=DY2
      U3=V2
      V3=U2
      UV2=DS2
      UV3=UV2
      U4=DY4
      V4=DX4
      UV4=DS4
      CALL PAP(A1,A2,A4,A,XC,YC,AL,UX,UN,S1,DXC,DYC,UVC,RU,RV)
      IF(ABS(AL).LT.10.) GO TO 50
      N=N+1
      IF(N.GT.NG) GO TO 101
      XM(1)=0-XC
      XM(2)=B1-XC
      XM(3)=1.5*B1-XC
      XM(4)=B1-XC
      IF(H4.GT.B1) XM(4)=H4-XC
      XM(5)=0-XC
      XM(6)=0.0
      YM(1)=1.5*B1+B4-YC
      YM(2)=2*B1+B4-YC
      YM(3)=2*B1+B4-YC
      YM(4)=B4-YC
      YM(5)=0-YC
      YM(6)=0.0
      GO TO 44
   34 X1=B1
      X2=B1/3
      X3=5*B1/6
      X4=B1/2+YO
      Y1=B1
      Y2=4*B1/3
      Y3=B1/6
      Y4=H1+XO
      U2=DX2
      V2=DY2
      U3=V2
      V3=U2
      UV2=DS2
      UV3=0-UV2
      U4=DY4
      V4=DX4
      UV4=0-DS4
      CALL PAP(A1,A2,A4,A,XC,YC,AL,UX,UN,S1,DXC,DYC,UVC,RU,RV)
      IF(ABS(AL).LT.10.) GO TO 50
      N=N+1
      IF(N.GT.NG) GO TO 101
      XM(1)=0.5*B1-XC
      XM(2)=1.5*B1-XC
      IF(H4.GT.B1) XM(2)=0.5*B1+H4-XC
      XM(3)=1.5*B1-XC
      XM(4)=0.5*B1-XC
      XM(5)=0-XC
      XM(6)=0.0
      YM(1)=2*B1+B4-YC
      YM(2)=2*B1-YC
      YM(3)=0-YC
      YM(4)=0.5*B1-YC
      YM(5)=B1-YC
      YM(6)=0.0
      GO TO 44
   35 X1=B1/2
      X2=B1/3
      X3=X2/2
      X4=B1-YO
      IF(B1.GE.H4) GO TO 57
      PR=H4-B1
      X1=X1+PR
      X2=X2+PR
      X3=X3+PR
      X4=H4-YO
   57 Y1=B1+B4
      Y2=H1+B4+B1/6
      Y3=B4+B1/3
      Y4=B4-XO
      U2=DY2
      V2=DX2
      U3=V2
      V3=U2
      UV2=0-DS2
      UV3=UV2
      U4=DY4
      V4=DX4
      UV4=0-DS4
      CALL PAP(A1,A2,A4,A,XC,YC,AL,UX,UN,S1,DXC,DYC,UVC,RU,RV)
      IF(ABS(AL).LT.10.) GO TO 50
      N=N+1
      IF(N.GT.NG) GO TO 101
      XM(1)=0-XC
      XM(2)=B1-XC
      XM(3)=B1-XC
      XM(4)=B1-H4-XC
      XM(5)=0-XC
      XM(6)=0.0
      IF(B1.GE.H4) GO TO 52
      PR=H4-B1
      XM(1)=XM(1)+PR
      XM(2)=XM(2)+PR
      XM(3)=H4-XC
      XM(4)=0-XC
      XM(5)=XM(5)+PR
   52 YM(1)=2.5*B1+B4-YC
      YM(2)=2*B1+B4-YC
      YM(3)=0-YC
      YM(4)=B4-YC
      YM(5)=B1+B4-YC
      YM(6)=0.0
      GO TO 44
   36 X1=B1/2
      X2=7*B1/6
      X3=B1/6
      X4=B1-YO
      IF(B1.GE.H4) GO TO 58
      PR=H4-B1
      X1=X1+PR
      X2=X2+PR
      X3=X3+PR
      X4=H4-YO
   58 Y1=B1
      Y2=H1-B1/3
      Y3=H1-B1/3
      Y4=H1+XO
      U2=DX2
      V2=DY2
      U3=U2
      V3=V2
      UV2=DS2
      UV3=DS2
      U4=DY4
      V4=DX4
      UV4=DS4
      CALL PAP(A1,A2,A4,A,XC,YC,AL,UX,UN,S1,DXC,DYC,UVC,RU,RV)
      IF(ABS(AL).LT.10.) GO TO 50
      N=N+1
      IF(N.GT.NG) GO TO 101
      XM(1)=B1-H4-XC
      XM(2)=B1-XC
      XM(3)=1.5*B1-XC
      XM(4)=B1-XC
      XM(5)=0-XC
      XM(6)=0-XC
      IF(B1.GE.H4) GO TO 53
      PR=H4-B1
      XM(1)=0-XC
      XM(2)=H4-XC
      XM(3)=XM(3)+PR
      XM(4)=XM(4)+PR
      XM(5)=XM(5)+PR
      XM(6)=XM(6)+PR
   53 YM(1)=2*B1-YC
      YM(2)=2*B1+B4-YC
      YM(3)=2*B1-YC
      YM(4)=0-YC
      YM(5)=0-YC
      YM(6)=B1-YC
      GO TO 44
   37 X1=B4+B1/2
      X2=B4+2*B1/3
      X3=B4+2*B1/3
      X4=B4-XO
      Y1=B1
      Y2=H1+B1/6
      Y3=B1/6
      Y4=YO
      U2=DY2
      V2=DX2
      U3=U2
      V3=V2
      UV2=DS2
      UV3=DS2
      U4=DX4
      V4=DY4
      UV4=DS4
      CALL PAP(A1,A2,A4,A,XC,YC,AL,UX,UN,S1,DXC,DYC,UVC,RU,RV)
      IF(ABS(AL).LT.10.) GO TO 50
      N=N+1
      IF(N.GT.NG) GO TO 101
      XM(1)=B4-XC
      XM(2)=B4+B1-XC
      XM(3)=B4+B1-XC
      XM(4)=B4-XC
      XM(5)=0-XC
      XM(6)=0.0
      YM(1)=2*B1-YC
      YM(2)=2.5*B1-YC
      YM(3)=0.5*B1-YC
      YM(4)=0-YC
      YM(5)=0-YC
      YM(6)=0.0
      GO TO 44
   38 X1=B1/2
      X2=7*B1/6
      X3=B1/6
      X4=H4/2
      Y1=B1
      Y2=2*B1/3
      Y3=B1/3
      Y4=H1+Z
      U2=DX2
      V2=DY2
      U3=U2
      V3=V2
      UV2=DS2
      UV3=0-DS2
      U4=DY4
      V4=DX4
      UV4=DS4
      CALL PAP(A1,A2,A4,A,XC,YC,AL,UX,UN,S1,DXC,DYC,UVC,RU,RV)
      IF(ABS(AL).LT.10.) GO TO 50
      N=N+1
      IF(N.GT.NG) GO TO 101
      XM(1)=0-XC
      XM(2)=H4-XC
      XM(3)=1.5*B1-XC
      XM(4)=B1-XC
      XM(5)=0.5*B1-XC
      XM(6)=0-XC
      YM(1)=2*B1+B4-YC
      YM(2)=2*B1+B4-YC
      YM(3)=B1-YC
      YM(4)=0-YC
      YM(5)=0-YC
      YM(6)=B1-YC
      GO TO 44
   39 X1=B4+B1/2
      X2=B4+2*B1/3
      X3=B4+5*B1/6
      X4=B4-XO
      Y1=B1
      Y2=H1+B1/6
      Y3=B1/3
      Y4=H1-YO
      U2=DY2
      V2=DX2
      U3=V2
      V3=U2
      UV2=DS2
      UV3=DS2
      U4=DX4
      V4=DY4
      UV4=0-DS4
      CALL PAP(A1,A2,A4,A,XC,YC,AL,UX,UN,S1,DXC,DYC,UVC,RU,RV)
      IF(ABS(AL).LT.5.) GO TO 50
      N=N+1
      IF(N.GT.NG) GO TO 101
      XM(1)=0-XC
      XM(2)=B1+B4-XC
      XM(3)=B1+B4-XC
      XM(4)=0.5*B1+B4-XC
      XM(5)=B4-XC
      XM(6)=0.0
      YM(1)=2*B1-YC
      YM(2)=2.5*B1-YC
      YM(3)=B1-YC
      YM(4)=0-YC
      YM(5)=0-YC
      YM(6)=0.0
      GO TO 44
   40 X1=B1/2
      X2=7*B1/6
      X3=B1/3
      X4=H4/2
      Y1=B1
      Y2=2*B1/3
      Y3=B1/6
      Y4=H1+Z
      U2=DX2
      V2=DY2
      U3=V2
      V3=U2
      UV2=DS2
      UV3=0-DS2
      U4=DY4
      V4=DX4
      UV4=DS4
      CALL PAP(A1,A2,A4,A,XC,YC,AL,UX,UN,S1,DXC,DYC,UVC,RU,RV)
      IF(ABS(AL).LT.10.) GO TO 50
      N=N+1
      IF(N.GT.NG) GO TO 101
      XM(1)=0-XC
      XM(2)=H4-XC
      XM(3)=1.5*B1-XC
      XM(4)=B1-XC
      XM(5)=0-XC
      XM(6)=0.0
      YM(1)=2*B1+B4-YC
      YM(2)=2*B1+B4-YC
      YM(3)=B1-YC
      YM(4)=0-YC
      YM(5)=0.5*B1-YC
      YM(6)=0.0
   44 G1=AL*PI/180
      SN=SIN(G1)
      CS=COS(G1)
      UMX=YM(1)*SN+XM(1)*CS
      UMN=UMX
      VMX=YM(1)*CS-XM(1)*SN
      VMN=VMX
      DO 43 K=2,6
      U=YM(K)*SN+XM(K)*CS
      IF(U.GT.UMX) UMX=U
      IF(U.LT.UMN) UMN=U
      V=YM(K)*CS-XM(K)*SN
      IF(V.GT.VMX) VMX=V
      IF(V.LT.VMN) VMN=V
   43 CONTINUE
      W1=UX/VMX
      W2=0-UX/VMN
      WUX=W1
      IF(W2.GT.W1) WUX=W2
      WUN=W1
      IF(W2.LT.W1) WUN=W2
      W1=UN/UMX
      W2=0-UN/UMN
      WVX=W1
      IF(W2.GT.W1) WVX=W2
      WVN=W1
      IF(W2.LT.W1) WVN=W2
      IF(N.LT.NG) GO TO 50
      IF(N.GT.NG) GO TO 101
      PRINT 46,B1,H1,N,B2,H2,UMX,VMX,WUX,WVX,D2,AL
   46 FORMAT(1 H,2X,'| Var.|Staciakm.bxh=',F4.1,'x',F4.1,' cm|',
     *'      |   Umax   Vmax  | Wumax    Wvmax |'/1H ,2X,'|',I4,
     *' |Trikamp. bxh=',F4.1,'x',F4.1,' cm| Alfa |',2F8.2,'|',
     *2F8.2,'|'/1H ,2X,'|Schem|Skritulio d=',F4.1,' cm',4X,'  |',
     *F5.1,' |  Umin    Vmin  | Wumin    Wvmin |')
      IF(NC.EQ.0) GO TO 47
      IF(I1.EQ.5.OR.I1.EQ.7.OR.I1.EQ.9.OR.I1.EQ.11.OR.I1.EQ.13.
     *OR.I1.EQ.15.OR.I1.EQ.17) GO TO 5
      PRINT 4, NS,LN,UMN,VMN,WUN,WVN
    4 FORMAT(1H ,2X,'|',I4,' |Lovinis profilis Nr',I3,3X,'|laipsn|',
     *2F8.2,'|',2F8.2,'|'/)
      GO TO 50
    5 PRINT 6, NS,LN,UMN,VMN,WUN,WVN
    6 FORMAT(1H ,2X,'|',I4,' |Lovinis profilis Nr',I3,'a',2X,'|',
     *'laipsn|',2F8.2,'|',2F8.2,'|'/)
      GO TO 50
   47 PRINT 8, NS,KH,KB,KD,UMN,VMN,WUN,WVN
    8 FORMAT(1H ,2X,'|',I4,' |Nelygias.kamp.',I4,'x',I3,'x',I2,
     *'|laipsnI',2F8.2,'|',2F8.2,'|'/)
   50 CONTINUE
  101 PRINT 102
  102 FORMAT(2X,'Paaiskinimai: - Alfa - skerspjuvio svarbiausiosios a',
     *'sies U pasukimo kampas'/25X,'Xc asies teigiamos krypties atzvi',
     *'lgiu (plius - pries',/25X,'l.rodykle, minus - pagal l.rodyk',
     *'le)'/16X,'- Umax,Umin - skerspjuvio tolimiausiu tasku atstumai',
     *' svar-'/30X,'biausiosios asies V atzvilgiu (tolimiausi vien',
     *'oj'/30X,'ir kitoj asies pusej)'/16X,'- Vmax,Vmin - skerspjuvio',
     *' tolimiausiu tasku atstumai svar-'/30X,'biausiosios asies U at',
     *'zvilgiu (tolimiausi vienoj'/30X,'ir kitoj asies pusej)')
      PRINT 103
  103 FORMAT(16X,'- Wumax,Wumin - skerspjuvio atsparumo momentai',
     *' svarbiausio-'/32X,'sios asies U atzvilgiu'/16X,'- Wvmax,Wvmin',
     *' - tas pats svarbiausiosios asies V atzvilgiu')
      PRINT *,'Jei baigete, iveskite nuli; jei tikrinsite kita ',
     *'varianta - bet koki skaitmeni'
      READ *,NON
      IF(NON.NE.0) GO TO 7
      STOP
      END
      SUBROUTINE PAP(A1,A2,A4,A,XC,YC,AL,UX,UN,S1,DXC,DYC,UVC,RU,RV)
      COMMON X1,X2,X3,X4,Y1,Y2,Y3,Y4,DX1,DY1,U2,V2,UV2,U3,V3,UV3,
     *U4,V4,UV4
      A=A1+A4
      XC=(A1*X1+A2*(X2-X3)+A4*X4)/A
      YC=(A1*Y1+A2*(Y2-Y3)+A4*Y4)/A
      P1=Y1-YC
      P2=Y2-YC
      P3=Y3-YC
      P4=Y4-YC
      R1=X1-XC
      R2=X2-XC
      R3=X3-XC
      R4=X4-XC
      DXC=DX1+U2-U3+U4+A1*P1*P1+A2*(P2*P2-P3*P3)+A4*P4*P4
      DYC=DY1+V2-V3+V4+A1*R1*R1+A2*(R2*R2-R3*R3)+A4*R4*R4
      UVC=A1*P1*R1+A2*(P2*R2-P3*R3)+A4*P4*R4
      UVC=UVC+UV2-UV3+UV4
      G1=ATAN(2*UVC/(DYC-DXC))
      AL=G1*28.6479
      UX=((DXC-DYC)**2+UVC*UVC*4)**0.5
      UN=(DXC+DYC-UX)*0.5
      UX=(DXC+DYC+UX)*0.5
      SN=SIN(G1)
      CS=COS(G1)
      S1=(DXC-DYC)*SN*0.5+UVC*CS
      RU=(UX/A)**0.5
      RV=(UN/A)**0.5
      RETURN
      END
