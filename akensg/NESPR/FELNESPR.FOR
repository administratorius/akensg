      DIMENSION DDV(23,2),DM1(20,3),DM2(20,2),MAP(20,3)
      DIMENSION WMA(14),QMA(14),AM(3),UM(3),FM(3),PM(3),WM(3)
      COMMON /C1/ A1,A2,A3,A,B,IF,IP,M
      COMMON /C2/ U1,U2,U3,F1,F2,F3,E1,E2,E3,P1,P2,P3
      OPEN(UNIT=5,FILE='FELNESPR.DAT')
      OPEN(UNIT=6,FILE='FELNESPR.RES')
      DATA DDV /10,12,14,16,18,18,20,20,22,22,24,24,27,27,30,30,33,
     *36,40,45,50,55,60,39.7,58.4,81.7,109.0,143.0,159.0,184.0,
     *203.0,232.0,254.0,289.0,317.0,371.0,407.0,472.0,518.0,597.0,
     *743.0,953.0,1231.0,1589.0,2035.0,2560.0/
      DATA DM1 /3.3,4.6,3.9,3.0,5.0,3.4,4.7,4.0,3.2,5.1,3.5,4.8,4.1,
     *3.1,4.6,3.6,4.9,4.2,3.0,4.7,0.5,1.4,0.4,1.2,0.5,0.6,1.1,0.5,
     *1.0,1.5,0.7,0.6,0.8,0.7,1.0,1.2,0.7,1.1,2.0,1.8,2.8,3.0,2.5,
     *2.1,2.7,2.6,2.4,2.5,2.3,3.0,2.7,2.1,2.6,2.1,2.9,3.0,2.5,3.0,
     *2.6,3.5/
      DATA DM2 /4.5,3.8,4.2,5.6,3.9,4.3,3.6,3.2,5.2,3.8,4.2,4.0,3.4,
     *5.0,4.0,4.1,3.7,3.5,4.8,4.0,1.1,0.7,1.5,1.4,1.8,1.2,0.8,0.8,
     *0.7,0.9,1.1,1.0,1.4,1.3,1.5,0.6,0.4,1.2,0.4,0.7/
      DATA MAP /30,65,55,40,45,35,50,35,45,60,55,65,70,35,45,30,55,
     *35,40,65,15,18,25,16,23,10,14,25,17,18,15,22,27,12,14,18,11,
     *16,25,19,30,15,25,10,35,45,20,25,35,30,45,55,40,35,25,50,35,
     *55,60,15/
      READ(5,*) NP,NV,IR
      IF(IR.NE.0) WRITE(6,1)
    1 FORMAT(2X,77('-')/1H ,1X,'|   |   |   |   |   |  M1k |  M2k',
     *' |  M3k |  M4k |  M k |  M k |  M k |      |'/1H ,1X,'|Var|',
     *' l1| l2| l3| a |  M1d |  M2d |  M3d |  M4d |  M d |  M d |',
     *'  M d |  Max |'/1H ,1X,'|Sch| F | M | p | b |  Q1k |',
     *'  Q2k |  Q3k |  Q4k |  Q k |  Q k |  Q k |  Nr  |'/1H ,1X,
     *'|   |   |   |   |   |  Q1d |  Q2d |  Q3d |  Q4d |  Q d',
     *' |  Q d |  Q d |      |'/1H ,1X,'|',5('---|'),8(6('-'),'|'))
      NX=0
      I1=0
      I2=0
      I3=0
      I4=0
      I5=0
      I6=0
      I7=0
      ND=0
      NPS=NP+NV-1
      DO 150 J=1,2000
      NV=J
      IF(J.GT.NPS) GO TO 151
      ND=ND+1
      IF(ND.GT.33) ND=ND-33
      IF(ND.EQ.1) NX=NX+1
      NX=NX+5
      IF(NX.GT.33) NX=NX-33
      I1=I1+3
      IF(I1.GT.20) I1=I1-19
      I2=I2+1
      IF(I2.GT.20) I2=I2-20
      I3=I3+7
      IF(I3.GT.20) I3=I3-18
      I4=I4+11
      IF(I4.GT.20) I4=I4-20
      I5=I5+13
      IF(I5.GT.20) I5=I5-17
      I6=I6+17
      IF(I6.GT.20) I6=I6-20
      I7=I7+19
      IF(I7.GT.20) I7=I7-20
      IF(J.LT.NP) GO TO 150
      A1=DM1(I1,1)
      A=DM1(I2,2)
      A3=DM1(I3,3)
      A2=DM2(I4,1)
      B=DM2(I5,2)
      IF=MAP(I6,1)
      IP=MAP(I7,2)
      M=MAP(I5,3)
      IF(IR.NE.0) GO TO 10
      WRITE(6,3) J,NX
    3 FORMAT(1H ,20X,'***',I5,' variantas *',I4,' schema  ***'/1H ,
     *5X,'Rasti konstrukcineje schemoje nurodytos sijos nezinomus at',
     *'raminius'/1H ,5X,'momentus, nubraizyti irazu diagramas bei pa',
     *'rinkti sijos skerspjuvi'/1H ,5X,'(plieninis dvitejis, [S]=160',
     *' MPa), kai duota:')
      IF(NX.EQ.2.OR.NX.EQ.5.OR.NX.EQ.9.OR.NX.EQ.12.OR.NX.EQ.13.
     *OR.NX.EQ.17.OR.NX.EQ.20.OR.NX.EQ.21.OR.NX.EQ.22.OR.NX.EQ.24.
     *OR.NX.EQ.25.OR.NX.EQ.30.OR.NX.EQ.32) GO TO 6
      WRITE(6,5) A1,A2,A3,A,B,IF,M,IP
    5 FORMAT(1H ,5X,'l1=',F3.1,' m, l2=',F3.1,' m, l3=',F3.1,' m, a=',
     *F3.1,' m ir b=',F3.1,' m bei apkrovos'/1H ,5X,'F=',I2,' kN, M=',
     *I2,' kN.m ir p=',I2,' kN/m.')
      GO TO 150
    6 WRITE(6,7) A1,A2,A,B,IF,M,IP
    7 FORMAT(1H ,5X,'l1=',F3.1,' m, l2=',F3.1,' m, a=',
     *F3.1,' m ir b=',F3.1,' m bei apkrovos'/1H ,5X,'F=',I2,' kN, M=',
     *I2,' kN.m ir p=',I2,' kN/m.')
      GO TO 150
   10 AM(1)=A1
      AM(2)=A2
      AM(3)=A3
      DO 8 I=1,3
      UM(I)=0.0
      FM(I)=0.0
      WM(I)=0.0
      PM(I)=0.0
    8 CONTINUE
      DO 9 I=1,14
      WMA(I)=0.0
      QMA(I)=0.0
    9 CONTINUE
      GO TO(11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,
     *27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43),NX
   11 WMA(11)=0-0.5*IP*A*A
      WMA(4)=WMA(11)
      UM(1)=B
      UM(3)=A
      PM(2)=IP
      WM(1)=0-M
      FM(3)=IF
      CALL RFS(FM,WM,PM,AM,UM,RF2,RF3)
      CALL SKAI(NX,RF2,RF3,WMA)
      QMA(1)=0.0
      QMA(11)=IP*A
      CALL SKER(NV,NX,AM,UM,FM,WM,PM,WMA,QMA,DDV)
      GO TO 150
   12 WMA(4)=0-0.5*IP*(A+B)**2
      WM(2)=M
      FM(3)=IF
      UM(2)=A
      UM(3)=B
      AM(3)=A2
      AM(2)=A1
      CALL RFS(FM,WM,PM,AM,UM,RF2,RF3)
      CALL SKAI(NX,RF2,RF3,WMA)
      QMA(1)=0.0
      QMA(10)=(A+B)*IP
      CALL SKER(NV,NX,AM,UM,FM,WM,PM,WMA,QMA,DDV)
      GO TO 150
   13 WMA(1)=IF*B
      UM(2)=A
      UM(3)=A
      FM(3)=IF
      PM(1)=IP
      WM(2)=M
      CALL RFS(FM,WM,PM,AM,UM,RF2,RF3)
      CALL SKAI(NX,RF2,RF3,WMA)
      QMA(1)=IF
      QMA(11)=0.0
      CALL SKER(NV,NX,AM,UM,FM,WM,PM,WMA,QMA,DDV)
      GO TO 150
   14 WMA(1)=0-IF*A
      UM(1)=A1-B
      UM(2)=A2-A
      WM(1)=0-M
      FM(2)=0-IF
      PM(3)=IP
      CALL RFS(FM,WM,PM,AM,UM,RF2,RF3)
      CALL SKAI(NX,RF2,RF3,WMA)
      QMA(1)=0-IF
      QMA(11)=0.0
      CALL SKER(NV,NX,AM,UM,FM,WM,PM,WMA,QMA,DDV)
      GO TO 150
   15 WMA(1)=IP*B*B/2
      WM(1)=M
      FM(2)=IF
      UM(1)=B
      UM(2)=A2-A
      CALL RFS(FM,WM,PM,AM,UM,RF2,RF3)
      CALL SKAI(NX,RF2,RF3,WMA)
      QMA(1)=IP*B
      QMA(10)=0.0
      CALL SKER(NV,NX,AM,UM,FM,WM,PM,WMA,QMA,DDV)
      GO TO 150
   16 FM(1)=IF
      FM(3)=0-IF
      WM(2)=M
      PM(1)=IP
      UM(3)=A3-A
      UM(2)=A2
      UM(1)=A1/2
      CALL RFS(FM,WM,PM,AM,UM,RF2,RF3)
      CALL SKAI(NX,RF2,RF3,WMA)
      QMA(1)=0.0
      QMA(11)=0.0
      CALL SKER(NV,NX,AM,UM,FM,WM,PM,WMA,QMA,DDV)
      GO TO 150
   17 WMA(4)=0-IP*B*B/2
      UM(2)=A
      FM(2)=IF
      PM(3)=0-IP
      WM(1)=M
      CALL RFS(FM,WM,PM,AM,UM,RF2,RF3)
      CALL SKAI(NX,RF2,RF3,WMA)
      QMA(1)=0.0
      QMA(11)=IP*B
      CALL SKER(NV,NX,AM,UM,FM,WM,PM,WMA,QMA,DDV)
      GO TO 150
   18 UM(2)=B
      FM(2)=0-IF
      PM(2)=IP
      WM(1)=M
      UM(3)=A
      FM(3)=IF
      CALL RFS(FM,WM,PM,AM,UM,RF2,RF3)
      CALL SKAI(NX,RF2,RF3,WMA)
      QMA(1)=0.0
      QMA(11)=0.0
      CALL SKER(NV,NX,AM,UM,FM,WM,PM,WMA,QMA,DDV)
      GO TO 150
   19 WMA(4)=IF*B
      UM(3)=B
      WM(3)=0-M
      PM(2)=IP
      AM(3)=A2
      AM(2)=A1
      CALL RFS(FM,WM,PM,AM,UM,RF2,RF3)
      CALL SKAI(NX,RF2,RF3,WMA)
      QMA(1)=0.0
      QMA(10)=0-IF
      CALL SKER(NV,NX,AM,UM,FM,WM,PM,WMA,QMA,DDV)
      GO TO 150
   20 UM(1)=A1
      UM(2)=A
      FM(2)=IF
      PM(3)=IP
      WM(1)=M
      UM(3)=B
      WM(3)=0-M
      CALL RFS(FM,WM,PM,AM,UM,RF2,RF3)
      CALL SKAI(NX,RF2,RF3,WMA)
      QMA(1)=0.0
      QMA(11)=0.0
      CALL SKER(NV,NX,AM,UM,FM,WM,PM,WMA,QMA,DDV)
      GO TO 150
   21 WMA(4)=0-IP*B*B/2
      UM(3)=A3-A
      UM(2)=B
      FM(2)=IF
      WM(3)=M
      PM(1)=IP
      CALL RFS(FM,WM,PM,AM,UM,RF2,RF3)
      CALL SKAI(NX,RF2,RF3,WMA)
      QMA(1)=0.0
      QMA(11)=IP*B
      CALL SKER(NV,NX,AM,UM,FM,WM,PM,WMA,QMA,DDV)
      GO TO 150
   22 WMA(1)=0-IF*B
      UM(2)=A
      PM(1)=0-IP
      WM(2)=0-M
      CALL RFS(FM,WM,PM,AM,UM,RF2,RF3)
      CALL SKAI(NX,RF2,RF3,WMA)
      QMA(1)=0-IF
      QMA(10)=0.0
      CALL SKER(NV,NX,AM,UM,FM,WM,PM,WMA,QMA,DDV)
      GO TO 150
   23 WMA(4)=0.5*IP*(A+B)**2
      UM(3)=A2-A
      UM(2)=A
      FM(2)=IF
      WM(3)=M
      AM(3)=A2
      AM(2)=A1
      CALL RFS(FM,WM,PM,AM,UM,RF2,RF3)
      CALL SKAI(NX,RF2,RF3,WMA)
      QMA(1)=0.0
      QMA(10)=0-(A+B)*IP
      CALL SKER(NV,NX,AM,UM,FM,WM,PM,WMA,QMA,DDV)
      GO TO 150
   24 WMA(1)=0-IP*B*B/2
      UM(1)=A1-A
      UM(2)=A
      FM(2)=IF
      PM(3)=IP
      WM(1)=M
      CALL RFS(FM,WM,PM,AM,UM,RF2,RF3)
      CALL SKAI(NX,RF2,RF3,WMA)
      QMA(1)=0-IP*B
      QMA(11)=0.0
      CALL SKER(NV,NX,AM,UM,FM,WM,PM,WMA,QMA,DDV)
      GO TO 150
   25 UM(2)=A2/2
      FM(2)=IF
      PM(2)=IP
      WM(1)=M
      PM(3)=IP
      CALL RFS(FM,WM,PM,AM,UM,RF2,RF3)
      CALL SKAI(NX,RF2,RF3,WMA)
      QMA(1)=0.0
      QMA(11)=0.0
      CALL SKER(NV,NX,AM,UM,FM,WM,PM,WMA,QMA,DDV)
      GO TO 150
   26 WMA(4)=0-0.5*IP*A*A
      UM(2)=A2/2
      PM(1)=0-IP
      FM(2)=IF
      WM(3)=M
      CALL RFS(FM,WM,PM,AM,UM,RF2,RF3)
      CALL SKAI(NX,RF2,RF3,WMA)
      QMA(1)=0.0
      QMA(11)=IP*A
      CALL SKER(NV,NX,AM,UM,FM,WM,PM,WMA,QMA,DDV)
      GO TO 150
   27 WMA(1)=M
      UM(2)=A
      PM(1)=IP
      FM(2)=IF
      CALL RFS(FM,WM,PM,AM,UM,RF2,RF3)
      CALL SKAI(NX,RF2,RF3,WMA)
      QMA(1)=0.0
      QMA(10)=0.0
      CALL SKER(NV,NX,AM,UM,FM,WM,PM,WMA,QMA,DDV)
      GO TO 150
   28 WMA(4)=0-IF*B
      UM(2)=B
      UM(1)=A1-A
      FM(1)=0-IF
      WM(2)=0-M
      PM(3)=IP
      CALL RFS(FM,WM,PM,AM,UM,RF2,RF3)
      CALL SKAI(NX,RF2,RF3,WMA)
      QMA(1)=0.0
      QMA(11)=IF
      CALL SKER(NV,NX,AM,UM,FM,WM,PM,WMA,QMA,DDV)
      GO TO 150
   29 WMA(1)=0-IP*A*A/2
      UM(3)=A3
      UM(1)=A
      WM(3)=0-M
      FM(1)=0-IF
      PM(2)=IP
      CALL RFS(FM,WM,PM,AM,UM,RF2,RF3)
      CALL SKAI(NX,RF2,RF3,WMA)
      QMA(1)=0-IP*A
      QMA(11)=0.0
      CALL SKER(NV,NX,AM,UM,FM,WM,PM,WMA,QMA,DDV)
      GO TO 150
   30 WMA(4)=M
      UM(2)=A1-A
      FM(2)=0-IF
      PM(3)=IP
      AM(3)=A2
      AM(2)=A1
      CALL RFS(FM,WM,PM,AM,UM,RF2,RF3)
      CALL SKAI(NX,RF2,RF3,WMA)
      QMA(1)=0.0
      QMA(10)=0.0
      CALL SKER(NV,NX,AM,UM,FM,WM,PM,WMA,QMA,DDV)
      GO TO 150
   31 WMA(1)=0-M
      UM(1)=A
      PM(2)=0-IP
      FM(1)=IF
      CALL RFS(FM,WM,PM,AM,UM,RF2,RF3)
      CALL SKAI(NX,RF2,RF3,WMA)
      QMA(1)=0.0
      QMA(10)=0.0
      CALL SKER(NV,NX,AM,UM,FM,WM,PM,WMA,QMA,DDV)
      GO TO 150
   32 WMA(1)=0-IP*0.5*(A+B)**2
      UM(1)=A1-B
      UM(2)=A
      FM(1)=0-IF
      WM(2)=0-M
      CALL RFS(FM,WM,PM,AM,UM,RF2,RF3)
      CALL SKAI(NX,RF2,RF3,WMA)
      QMA(1)=0-IP*(A+B)
      QMA(10)=0.0
      CALL SKER(NV,NX,AM,UM,FM,WM,PM,WMA,QMA,DDV)
      GO TO 150
   33 WMA(4)=0-0.5*IP*B*B
      UM(1)=A1-A
      WM(3)=0-M
      FM(1)=IF
      PM(3)=IP
      CALL RFS(FM,WM,PM,AM,UM,RF2,RF3)
      CALL SKAI(NX,RF2,RF3,WMA)
      QMA(1)=0.0
      QMA(11)=IP*B
      CALL SKER(NV,NX,AM,UM,FM,WM,PM,WMA,QMA,DDV)
      GO TO 150
   34 WMA(4)=0-M
      UM(2)=B
      PM(3)=0-IP
      FM(2)=IF
      AM(3)=A2
      AM(2)=A1
      CALL RFS(FM,WM,PM,AM,UM,RF2,RF3)
      CALL SKAI(NX,RF2,RF3,WMA)
      QMA(1)=0.0
      QMA(10)=0.0
      CALL SKER(NV,NX,AM,UM,FM,WM,PM,WMA,QMA,DDV)
      GO TO 150
   35 WMA(4)=0-(A+B)*IF
      UM(2)=A1-A
      WM(2)=0-M
      PM(3)=IP
      AM(3)=A2
      AM(2)=A1
      CALL RFS(FM,WM,PM,AM,UM,RF2,RF3)
      CALL SKAI(NX,RF2,RF3,WMA)
      QMA(1)=0.0
      QMA(10)=IF
      CALL SKER(NV,NX,AM,UM,FM,WM,PM,WMA,QMA,DDV)
      GO TO 150
   36 UM(2)=A2
      UM(1)=A1/2
      FM(1)=0-IF
      PM(1)=IP
      WM(2)=M
      PM(3)=IP
      CALL RFS(FM,WM,PM,AM,UM,RF2,RF3)
      CALL SKAI(NX,RF2,RF3,WMA)
      QMA(1)=0.0
      QMA(11)=0.0
      CALL SKER(NV,NX,AM,UM,FM,WM,PM,WMA,QMA,DDV)
      GO TO 150
   37 WMA(1)=0-M
      UM(2)=B
      PM(1)=IP
      FM(2)=IF
      PM(3)=0-IP
      CALL RFS(FM,WM,PM,AM,UM,RF2,RF3)
      CALL SKAI(NX,RF2,RF3,WMA)
      QMA(1)=0.0
      QMA(11)=0.0
      CALL SKER(NV,NX,AM,UM,FM,WM,PM,WMA,QMA,DDV)
      GO TO 150
   38 UM(2)=A2/2
      FM(2)=IF
      PM(1)=IP
      WM(1)=M
      PM(3)=0-IP
      CALL RFS(FM,WM,PM,AM,UM,RF2,RF3)
      CALL SKAI(NX,RF2,RF3,WMA)
      QMA(1)=0.0
      QMA(11)=0.0
      CALL SKER(NV,NX,AM,UM,FM,WM,PM,WMA,QMA,DDV)
      GO TO 150
   39 WMA(1)=0-IP*A*A/2
      UM(3)=A3-B
      WM(2)=0-M
      UM(2)=A2
      FM(3)=0-IF
      PM(1)=IP
      CALL RFS(FM,WM,PM,AM,UM,RF2,RF3)
      CALL SKAI(NX,RF2,RF3,WMA)
      QMA(1)=0-IP*A
      QMA(11)=0.0
      CALL SKER(NV,NX,AM,UM,FM,WM,PM,WMA,QMA,DDV)
      GO TO 150
   40 WMA(4)=M
      UM(3)=A2-B
      FM(3)=0-IF
      PM(2)=IP
      AM(3)=A2
      AM(2)=A1
      CALL RFS(FM,WM,PM,AM,UM,RF2,RF3)
      CALL SKAI(NX,RF2,RF3,WMA)
      QMA(1)=0.0
      QMA(10)=0.0
      CALL SKER(NV,NX,AM,UM,FM,WM,PM,WMA,QMA,DDV)
      GO TO 150
   41 WMA(4)=M
      UM(1)=A
      PM(2)=0-IP
      FM(1)=IF
      UM(3)=A3-B
      FM(3)=0-IF
      CALL RFS(FM,WM,PM,AM,UM,RF2,RF3)
      CALL SKAI(NX,RF2,RF3,WMA)
      QMA(1)=0.0
      QMA(11)=0.0
      CALL SKER(NV,NX,AM,UM,FM,WM,PM,WMA,QMA,DDV)
      GO TO 150
   42 WMA(1)=IF*B
      UM(1)=A1-B
      WM(1)=M
      PM(2)=IP
      CALL RFS(FM,WM,PM,AM,UM,RF2,RF3)
      CALL SKAI(NX,RF2,RF3,WMA)
      QMA(1)=IF
      QMA(10)=0.0
      CALL SKER(NV,NX,AM,UM,FM,WM,PM,WMA,QMA,DDV)
      GO TO 150
   43 WMA(1)=M-IP*A*A/2
      WMA(4)=0-IF*B
      UM(2)=A2/2
      WM(2)=0-M
      PM(1)=IP
      CALL RFS(FM,WM,PM,AM,UM,RF2,RF3)
      CALL SKAI(NX,RF2,RF3,WMA)
      QMA(1)=0-IP*A
      QMA(11)=IF
      CALL SKER(NV,NX,AM,UM,FM,WM,PM,WMA,QMA,DDV)
  150 CONTINUE
  151 STOP
      END
      SUBROUTINE RFS(FM,WM,PM,AM,UM,RF2,RF3)
      DIMENSION AM(3),UM(3),FM(3),PM(3),WM(3),RK(3),RD(3)
      DO 3 I=1,3
      U=UM(I)
      A=AM(I)
      F=FM(I)
      P=PM(I)
      W=WM(I)
      V=A-U
      U=U/A
      V=V/A
      RKF=(1+V)*F*A*A*U*V
      RKM=(3*V*V-1)*W*A
      RK(I)=(1+V)*F*A*A*U*V+P*A*A*A/4+(3*V*V-1)*W*A
      RD(I)=(1+U)*F*A*A*U*V+P*A*A*A/4+(1-3*U*U)*W*A
    3 CONTINUE
      RF2=RD(1)+RK(2)
      RF3=RD(2)+RK(3)
      RETURN
      END
      SUBROUTINE SKAI(NX,RF2,RF3,WMA)
      DIMENSION WMA(14),QMA(14)
      COMMON /C1/ A1,A2,A3,A,B,IF,IP,M
      W1=WMA(1)
      W4=WMA(4)
      S3=A3
      S2=A2
      S1=A1
      IF(NX.NE.2.AND.NX.NE.9.AND.NX.NE.13.AND.NX.NE.20.AND.NX.NE.24.
     *AND.NX.NE.25.AND.NX.NE.30) GO TO 1
      S3=A2
      S2=A1
      S1=0.0
    1 IF(NX.EQ.5.OR.NX.EQ.12.OR.NX.EQ.17.OR.NX.EQ.21.OR.NX.EQ.22.
     *OR.NX.EQ.32) S3=0.0
      C1=0.5*S2/(S2+S3)
      C2=(W4*S3+RF3)*0.5/(S2+S3)
      C3=W1*S1+RF2
      W2=(C2*S2-C3)/((S1+S2)*2-S2*C1)
      W3=0-(C1*W2+C2)
      WMA(1)=W1
      WMA(2)=W2
      IF(NX.EQ.10) WMA(2)=W2-M
      WMA(3)=W3
      IF(NX.EQ.29) WMA(3)=W3+M
      IF(NX.EQ.6.OR.NX.EQ.26) WMA(3)=W3-M
      WMA(4)=W4
      IF(NX.EQ.19) WMA(4)=M
      IF(NX.NE.2.AND.NX.NE.9.AND.NX.NE.13.AND.NX.NE.20.AND.NX.NE.24.
     *AND.NX.NE.25.AND.NX.NE.30) GO TO 2
      WMA(1)=WMA(2)
      WMA(2)=WMA(3)
      WMA(3)=WMA(4)
      WMA(4)=0.0
    2 WMA(8)=W1
      IF(NX.EQ.7.OR.NX.EQ.8.OR.NX.EQ.15.OR.NX.EQ.17.OR.NX.EQ.28)
     *WMA(8)=M
      WMA(9)=W2
      WMA(10)=W3
      IF(NX.EQ.16) WMA(10)=W3+M
      IF(NX.EQ.23) WMA(10)=W3-M
      IF(NX.EQ.5.OR.NX.EQ.12.OR.NX.EQ.17.OR.NX.EQ.21.OR.NX.EQ.22.
     *OR.NX.EQ.32) WMA(10)=0.0
      WMA(11)=W4
      IF(NX.NE.2.AND.NX.NE.9.AND.NX.NE.13.AND.NX.NE.20.AND.NX.NE.24.
     *AND.NX.NE.25.AND.NX.NE.30) GO TO 20
      WMA(8)=WMA(9)
      WMA(9)=WMA(10)
      WMA(10)=WMA(11)
      WMA(11)=0.0
   20 RETURN
      END
      SUBROUTINE SKER(NV,NX,AM,UM,FM,WM,PM,WMA,QMA,DDV)
      DIMENSION AM(3),UM(3),FM(3),WM(3),PM(3),WMA(14),QMA(14),DDV(23,2)
      COMMON /C1/ A1,A2,A3,A,B,IF,IP,M
      IG=3
      IF(NX.EQ.5.OR.NX.EQ.12.OR.NX.EQ.17.OR.NX.EQ.21.OR.NX.EQ.22.
     *OR.NX.EQ.32) IG=2
      IF(NX.NE.2.AND.NX.NE.9.AND.NX.NE.13.AND.NX.NE.20.AND.NX.NE.24.
     *AND.NX.NE.25.AND.NX.NE.30) GO TO 2
      DO 1 I=1,2
      J=I+1
      AM(I)=AM(J)
      UM(I)=UM(J)
      FM(I)=FM(J)
      WM(I)=WM(J)
      PM(I)=PM(J)
    1 CONTINUE
      WMA(1)=0.0
      IG=2
    2 DO 20 I=1,IG
      IK=I+7
      ID=I+1
      JK=I+4
      JD=I+11
      WK=WMA(IK)
      WD=WMA(ID)
      S=AM(I)
      U=UM(I)
      V=S-U
      F=FM(I)
      W=WM(I)
      IF(U.EQ.0.0.OR.V.EQ.0.) W=0.0
      P=PM(I)
      QMA(IK)=0.5*P*S+((S-U)*F-W-WK+WD)/S
      QMA(ID)=QMA(IK)-F-P*S
      QU=9.0
      QV=9.0
      IF(U.EQ.0.0.OR.V.EQ.0.) GO TO 3
      WMA(JK)=0.5*P*U*V+(F*U*V-W*U+WD*U+WK*V)/S
      WUD=WMA(JK)+W
      QMA(JK)=QMA(IK)-P*U
      QUD=QMA(JK)-F
      WMA(JD)=WMA(JK)+W
      QMA(JD)=QMA(JK)-F
      IF(P.EQ.0.) GO TO 20
      X=ABS(QMA(IK)/P)
      IF(X.GE.U) GO TO 5
      Y=U-X
      WU=0.5*P*X*Y+(WK*Y+WMA(JK)*X)/U
      QU=QMA(IK)-P*X
      GO TO 7
    5 X=ABS(QUD/P)
      IF(X.GE.V) GO TO 7
      Y=V-X
      WV=0.5*P*X*Y+(QMA(ID)*X+WUD*Y)/V
      QV=QUD-P*X
    7 IF(QU.NE.0.) GO TO 8
      WMA(JK)=WU
      QMA(JK)=0.0
      WMA(JD)=WU
      QMA(JD)=0.0
      GO TO 20
    8 IF(QV.NE.0.) GO TO 20
      WMA(JK)=WV
      QMA(JK)=0.0
      WMA(JD)=WV
      QMA(JD)=0.0
      GO TO 20
    3 IF(P.EQ.0.) GO TO 6
      U=ABS(QMA(IK)/P)
      IF(U.GE.S) U=S/2
      V=S-U
      WMA(JK)=0.5*P*U*V+(WK*V+WD*U)/S
      QMA(JK)=QMA(IK)-P*U
      WMA(JD)=WMA(JK)
      QMA(JD)=QMA(JK)
      GO TO 20
    6 WMA(JK)=(WK+WD)/2
      QMA(JK)=QMA(IK)
      WMA(JD)=WMA(JK)
      QMA(JD)=QMA(JK)
   20 CONTINUE
      WX=ABS(WMA(1))
      DO 12 I=2,14
      WW=ABS(WMA(I))
      IF(WW.GT.WX) WX=WW
   12 CONTINUE
      K=1
      WR=WX*100/16
   13 DO 15 J=1,23
      WA=K*DDV(J,2)
      IF(WA.GE.WR) GO TO 18
   15 CONTINUE
      K=K+1
      GO TO 13
   18 NP=DDV(J,1)
      WRITE(6,44) (WMA(JS),JS=1,7),NV,A1,A2,A3,A,(WMA(KS),KS=8,14),WX
   44 FORMAT(1H ,21X,7F7.2/1H ,I5,4F4.1,8F7.2)
      IF(J.EQ.6.OR.J.EQ.8.OR.J.EQ.10.OR.J.EQ.12.OR.J.EQ.14.
     *OR.J.EQ.16) GO TO 21
      WRITE(6,45) NX,IF,M,IP,B,(QMA(LS),LS=1,7),NP,(QMA(IS),IS=8,14)
   45 FORMAT(1H ,I5,3I4,F4.1,7F7.2,I5/1H ,21X,7F7.2)
      GO TO 24
   21 WRITE(6,46) NX,IF,M,IP,B,(QMA(LS),LS=1,7),NP,(QMA(IS),IS=8,14)
   46 FORMAT(1H ,I5,3I4,F4.1,7F7.2,I5,'a'/1H ,21X,7F7.2)
   24 WRITE(6,47)
   47 FORMAT(1H ,1X,'|',5('---|'),8(6('-'),'|'))
      RETURN
      END
