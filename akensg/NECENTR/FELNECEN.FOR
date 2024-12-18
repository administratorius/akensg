      INTEGER F
      COMMON NN,F,B,XF,YF
      OPEN(UNIT=5,FILE='FELNECEN.DAT')
      OPEN(UNIT=6,FILE='FELNECEN.RES')
      READ(5,*) NP,NV,IR
      IF(IR.NE.0) WRITE(6,1)
    1 FORMAT(2X,75('-')/1H ,1X,'|Var| F(kN)|  X2 |  X3 |   A1  |',
     *'  A2  |  A3  |   A   |  S1  |  S2  |  aX  |'/1H ,1X,'|Sch|',
     *'  XF  |  Y2 |  Y3 |  Ix1  | Ix2  | Ix3  |  Ix   |  S3  |',
     *'  S4  |  aY  |'/1H ,1X,'|   |  YF  |    B cm   |  Iy1  |',
     *' Iy2  | Iy3  |  Iy   |  Xb  |  Yb  |      |'/1H ,1X,
     *'|---|------|',11('-'),'|-------|',2(6('-'),'|'),'-------|',
     *3(6('-'),'|'))
      F=300
      B=16.0
      NX=0
      ND=0
      NPS=NP+NV-1
      DO 150 J=1,2000
      NN=0
      X1=0.0
      X2=0.0
      Y1=0.0
      Y2=0.0
      F=F+15
      B=B+3
      IF(F.GT.465) F=F-160
      IF(B.GT.40.) B=B-25
      IF(J.GT.NPS) GO TO 151
      ND=ND+1
      IF(ND.GT.32) ND=ND-32
      IF(ND.EQ.1) NX=NX+1
      NX=NX+5
      IF(NX.GT.32) NX=NX-32
      IF(J.LT.NP) GO TO 150
      GO TO(11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,
     *27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42),NX
   11 XF=0.75*B
      YF=-0.25*B
      I1=5
      I2=4
      N1=2
      N2=2
      X1=B*5/12
      X2=0.144*B
      CALL PIRM(NX,I1,N1,X1,Y1,A1,WX1,WY1)
      NN=1
      CALL PIRM(NX,I2,N2,X2,Y2,A2,WX2,WY2)
      GO TO 149
   12 XF=0.5*B
      YF=0.25*B
      I1=2
      I2=4
      N1=2
      N2=2
      Y1=0.375*B
      X2=0.894*B
      CALL PIRM(NX,I1,N1,X1,Y1,A1,WX1,WY1)
      NN=1
      CALL PIRM(NX,I2,N2,X2,Y2,A2,WX2,WY2)
      GO TO 149
   13 XF=-0.625*B
      YF=-0.25*B
      I1=5
      I2=4
      N1=2
      N2=2
      X1=7*B/24
      X2=0.731*B
      CALL PIRM(NX,I1,N1,X1,Y1,A1,WX1,WY1)
      NN=1
      CALL PIRM(NX,I2,N2,X2,Y2,A2,WX2,WY2)
      GO TO 149
   14 XF=-0.75*B
      YF=0.25*B
      I1=2
      I2=4
      N1=4
      N2=2
      X1=7*B/8
      Y1=3*B/8
      Y2=0.144*B
      CALL PIRM(NX,I1,N1,X1,Y1,A1,WX1,WY1)
      NN=1
      CALL PIRM(NX,I2,N2,X2,Y2,A2,WX2,WY2)
      GO TO 149
   15 XF=-1*B
      YF=0.5*B
      I1=5
      I2=3
      N1=2
      N2=1
      X1=5*B/12
      CALL PIRM(NX,I1,N1,X1,Y1,A1,WX1,WY1)
      NN=1
      CALL PIRM(NX,I2,N2,X2,Y2,A2,WX2,WY2)
      GO TO 149
   16 XF=-0.75*B
      YF=-0.25*B
      I1=1
      I2=4
      N1=2
      N2=2
      X2=0.856*B
      Y1=3*B/8
      CALL PIRM(NX,I1,N1,X1,Y1,A1,WX1,WY1)
      NN=1
      CALL PIRM(NX,I2,N2,X2,Y2,A2,WX2,WY2)
      GO TO 149
   17 XF=-1*B
      YF=-0.5*B
      I1=2
      I2=4
      N1=2
      N2=2
      X1=7*B/8
      Y2=0.394*B
      CALL PIRM(NX,I1,N1,X1,Y1,A1,WX1,WY1)
      NN=1
      CALL PIRM(NX,I2,N2,X2,Y2,A2,WX2,WY2)
      GO TO 149
   18 XF=-0.75*B
      YF=0.25*B
      I1=5
      I2=4
      N1=2
      N2=2
      X1=7*B/12
      Y2=0.356*B
      CALL PIRM(NX,I1,N1,X1,Y1,A1,WX1,WY1)
      NN=1
      CALL PIRM(NX,I2,N2,X2,Y2,A2,WX2,WY2)
      GO TO 149
   19 XF=0.75*B
      YF=0.25*B
      I1=2
      I2=4
      N1=4
      N2=2
      X1=7*B/8
      X2=0.356*B
      Y1=3*B/8
      CALL PIRM(NX,I1,N1,X1,Y1,A1,WX1,WY1)
      NN=1
      CALL PIRM(NX,I2,N2,X2,Y2,A2,WX2,WY2)
      GO TO 149
   20 XF=-0.5*B
      YF=-0.25*B
      I1=1
      I2=5
      N1=2
      N2=2
      X1=7*B/8
      X2=B/3
      CALL PIRM(NX,I1,N1,X1,Y1,A1,WX1,WY1)
      NN=1
      CALL PIRM(NX,I2,N2,X2,Y2,A2,WX2,WY2)
      GO TO 149
   21 XF=B
      YF=0.25*B
      I1=1
      I2=4
      N1=2
      N2=2
      X2=0.894*B
      Y1=3*B/8
      CALL PIRM(NX,I1,N1,X1,Y1,A1,WX1,WY1)
      NN=1
      CALL PIRM(NX,I2,N2,X2,Y2,A2,WX2,WY2)
      GO TO 149
   22 XF=0.875*B
      YF=-0.25*B
      I1=5
      I2=4
      N1=2
      N2=2
      X1=17*B/24
      X2=0.231*B
      CALL PIRM(NX,I1,N1,X1,Y1,A1,WX1,WY1)
      NN=1
      CALL PIRM(NX,I2,N2,X2,Y2,A2,WX2,WY2)
      GO TO 149
   23 XF=-0.5*B
      YF=-0.25*B
      I1=5
      I2=4
      N1=2
      N2=2
      X1=2*B/3
      Y2=0.356*B
      CALL PIRM(NX,I1,N1,X1,Y1,A1,WX1,WY1)
      NN=1
      CALL PIRM(NX,I2,N2,X2,Y2,A2,WX2,WY2)
      GO TO 149
   24 XF=0.5*B
      YF=-0.25*B
      I1=2
      I2=3
      N1=4
      N2=1
      X1=7*B/8
      Y1=3*B/8
      CALL PIRM(NX,I1,N1,X1,Y1,A1,WX1,WY1)
      NN=1
      CALL PIRM(NX,I2,N2,X2,Y2,A2,WX2,WY2)
      GO TO 149
   25 XF=0.625*B
      YF=-0.375*B
      I1=5
      I2=4
      N1=2
      N2=2
      X1=5*B/6
      Y2=0.394*B
      CALL PIRM(NX,I1,N1,X1,Y1,A1,WX1,WY1)
      NN=1
      CALL PIRM(NX,I2,N2,X2,Y2,A2,WX2,WY2)
      GO TO 149
   26 XF=-0.75*B
      YF=-0.375*B
      I1=1
      I2=4
      N1=2
      N2=2
      X1=7*B/8
      Y2=0.356*B
      CALL PIRM(NX,I1,N1,X1,Y1,A1,WX1,WY1)
      NN=1
      CALL PIRM(NX,I2,N2,X2,Y2,A2,WX2,WY2)
      GO TO 149
   27 XF=0.5*B
      YF=0.25*B
      I1=1
      I2=4
      N1=2
      N2=2
      X1=5*B/8
      Y2=0.394*B
      CALL PIRM(NX,I1,N1,X1,Y1,A1,WX1,WY1)
      NN=1
      CALL PIRM(NX,I2,N2,X2,Y2,A2,WX2,WY2)
      GO TO 149
   28 XF=0.5*B
      YF=0.375*B
      I1=1
      I2=5
      N1=1
      N2=2
      X2=7*B/24
      CALL PIRM(NX,I1,N1,X1,Y1,A1,WX1,WY1)
      NN=1
      CALL PIRM(NX,I2,N2,X2,Y2,A2,WX2,WY2)
      GO TO 149
   29 XF=-0.75*B
      YF=-0.375*B
      I1=1
      I2=4
      N1=1
      N2=2
      X2=0.856*B
      CALL PIRM(NX,I1,N1,X1,Y1,A1,WX1,WY1)
      NN=1
      CALL PIRM(NX,I2,N2,X2,Y2,A2,WX2,WY2)
      GO TO 149
   30 XF=-0.625*B
      YF=0.25*B
      I1=5
      I2=4
      N1=2
      N2=2
      X1=13*B/24
      X2=0.269*B
      CALL PIRM(NX,I1,N1,X1,Y1,A1,WX1,WY1)
      NN=1
      CALL PIRM(NX,I2,N2,X2,Y2,A2,WX2,WY2)
      GO TO 149
   31 XF=-1*B
      YF=0.5*B
      I1=5
      I2=3
      N1=2
      N2=1
      X1=5*B/6
      CALL PIRM(NX,I1,N1,X1,Y1,A1,WX1,WY1)
      NN=1
      CALL PIRM(NX,I2,N2,X2,Y2,A2,WX2,WY2)
      GO TO 149
   32 XF=-0.625*B
      YF=-0.25*B
      I1=2
      I2=4
      N1=4
      N2=2
      X1=7*B/8
      Y1=3*B/8
      X2=0.144*B
      CALL PIRM(NX,I1,N1,X1,Y1,A1,WX1,WY1)
      NN=1
      CALL PIRM(NX,I2,N2,X2,Y2,A2,WX2,WY2)
      GO TO 149
   33 XF=-0.5*B
      YF=0.25*B
      I1=5
      I2=4
      N1=2
      N2=2
      X1=B/3
      X2=0.606*B
      CALL PIRM(NX,I1,N1,X1,Y1,A1,WX1,WY1)
      NN=1
      CALL PIRM(NX,I2,N2,X2,Y2,A2,WX2,WY2)
      GO TO 149
   34 XF=-B
      YF=-0.5*B
      I1=1
      I2=4
      N1=1
      N2=2
      X2=0.519*B
      CALL PIRM(NX,I1,N1,X1,Y1,A1,WX1,WY1)
      NN=1
      CALL PIRM(NX,I2,N2,X2,Y2,A2,WX2,WY2)
      GO TO 149
   35 XF=B
      YF=0.5*B
      I1=1
      I2=4
      N1=2
      N2=2
      X1=7*B/8
      X2=0.144*B
      CALL PIRM(NX,I1,N1,X1,Y1,A1,WX1,WY1)
      NN=1
      CALL PIRM(NX,I2,N2,X2,Y2,A2,WX2,WY2)
      GO TO 149
   36 XF=0.75*B
      YF=0.125*B
      I1=1
      I2=5
      N1=1
      N2=2
      X2=11*B/24
      CALL PIRM(NX,I1,N1,X1,Y1,A1,WX1,WY1)
      NN=1
      CALL PIRM(NX,I2,N2,X2,Y2,A2,WX2,WY2)
      GO TO 149
   37 XF=-0.5*B
      YF=0.25*B
      I1=2
      I2=3
      N1=2
      N2=1
      X1=7*B/8
      CALL PIRM(NX,I1,N1,X1,Y1,A1,WX1,WY1)
      NN=1
      CALL PIRM(NX,I2,N2,X2,Y2,A2,WX2,WY2)
      GO TO 149
   38 XF=-B
      YF=-0.25*B
      I1=5
      I2=4
      N1=2
      N2=2
      X1=B/6
      X2=0.894*B
      CALL PIRM(NX,I1,N1,X1,Y1,A1,WX1,WY1)
      NN=1
      CALL PIRM(NX,I2,N2,X2,Y2,A2,WX2,WY2)
      GO TO 149
   39 XF=0.625*B
      YF=0.25*B
      I1=2
      I2=5
      N1=4
      N2=2
      X1=7*B/8
      X2=7*B/24
      Y1=3*B/8
      CALL PIRM(NX,I1,N1,X1,Y1,A1,WX1,WY1)
      NN=1
      CALL PIRM(NX,I2,N2,X2,Y2,A2,WX2,WY2)
      GO TO 149
   40 XF=0.75*B
      YF=-0.25*B
      I1=2
      I2=4
      N1=2
      N2=2
      Y1=0.375*B
      X2=0.606*B
      CALL PIRM(NX,I1,N1,X1,Y1,A1,WX1,WY1)
      NN=1
      CALL PIRM(NX,I2,N2,X2,Y2,A2,WX2,WY2)
      GO TO 149
   41 XF=-0.875*B
      YF=-0.375*B
      I1=5
      I2=4
      N1=2
      N2=2
      X1=5*B/12
      Y2=0.394*B
      CALL PIRM(NX,I1,N1,X1,Y1,A1,WX1,WY1)
      NN=1
      CALL PIRM(NX,I2,N2,X2,Y2,A2,WX2,WY2)
      GO TO 149
   42 XF=0.875*B
      YF=-0.25*B
      I1=2
      I2=4
      N1=2
      N2=2
      X1=5*B/8
      X2=0.394*B
      CALL PIRM(NX,I1,N1,X1,Y1,A1,WX1,WY1)
      NN=1
      CALL PIRM(NX,I2,N2,X2,Y2,A2,WX2,WY2)
  149 IF(IR.NE.0) GO TO 47
      IH=B
      IB=2*B
      BT=B/2
      BK=B/4
      WRITE(6,49) J,NX,XF,YF,F,IB,IH
   49 FORMAT(/1H ,20X,'***',I5,' variantas *',I4,' schema  ***'/1H ,
     *5X,'Schemoje nurodyto skerspjuvio trumpa kolona necentriskai ',
     *'(x=',F6.2,','/1H ,5X,'y=',F6.2,' cm) gniuzdoma F=',I4,' kN. Re',
     *'ikia nustatyti ir parodyti brezinyje'/1H ,5X,'neutralios linij',
     *'os padeti, paskaiciuoti normalinius itempius skerspju-'/1H ,5X,
     *'vio kampuose ir nubraizyti itempiu diagramas bei paskaiciuoti',
     *' ir nubrai-'/1H ,5X,'zyti skerspjuvio branduolio ribas, kai ko',
     *'lonos skerspjuvio BxH=',I2,'x',I2,' cm,')
      GO TO(51,52,51,52,53,54,52,51,52,55,54,51,51,57,51,54,54,55,
     *54,51,53,52,51,54,54,55,57,51,56,52,51,52),NX
   51 WRITE(6,61) BT,BT
   61 FORMAT(1H ,5X,'o ji silpninanciu lygiasoniu trikampiu b=h=',F4.1,
     *' cm bei pusapskritimiu'/1H ,5X,'d=',F4.1,' cm.')
      GO TO 150
   52 WRITE(6,62) BK,BT
   62 FORMAT(1H ,5X,'o ji silpninanciu kvadratu b=',F5.2,
     *' cm bei pusapskritimiu d=',F4.1,' cm.')
      GO TO 150
   53 WRITE(6,63) BT,BT
   63 FORMAT(1H ,5X,'o ji silpninanciu lygiasoniu trikampiu b=h=',F4.1,
     *' cm bei apskritimo'/1H ,5X,'d=',F4.1,' cm.')
      GO TO 150
   54 WRITE(6,64) BK,BT,BT
   64 FORMAT(1H ,5X,'o ji silpninanciu staciakampiu bxh=',F5.2,'x',
     *F4.1,' cm bei pusapskritimiu'/1H ,5X,'d=',F4.1,' cm.')
      GO TO 150
   55 WRITE(6,65) BT,BK,BT
   65 FORMAT(1H ,5X,'o ji silpninanciu lygiasoniu trikampiu b=h=',F4.1,
     *' cm bei staciakampiu'/1H ,5X,F5.2,'x',F4.1,' cm.')
      GO TO 150
   56 WRITE(6,66) BT,BK
   66 FORMAT(1H ,5X,'o ji silpninanciu lygiasoniu trikampiu b=h=',F4.1,
     *' cm bei kvadratu'/1H ,5X,'a=',F5.2,' cm.')
      GO TO 150
   57 WRITE(6,67) BK,BT
   67 FORMAT(1H ,5X,'o ji silpninanciu kvadratu a=',F5.2,
     *' cm bei apskritimo d=',F4.1,' cm.')
      GO TO 150
   47 A=B*B*2-A1-A2
      WX=B*B*B*B/6-WX1-WX2
      WY=2*B*B*B*B/3-WY1-WY2
      IF(NX.NE.29) GO TO 44
      A=A-B*B/16
      W=B*B*B*B/3072
      WX=WX-W
      WY=WY-W
   44 CX=WX/A
      CY=WY/A
      AX=0-CY/XF
      AY=0-CX/YF
      FX=CY/B
      FY=CX*2/B
      S=10*F/A
      SX=10*F*XF/WY
      SY=10*F*YF/WX
      S1=0-S+SX*B-SY*B/2
      S2=0-S-SX*B-SY*B/2
      S4=0-S+SX*B+SY*B/2
      S3=0-S-SX*B+SY*B/2
      IP=B*B*2
      IPX=B*B*B*B/6
      IPY=2*B*B*B*B/3
      IY=WY
      IX1=WX1
      IX2=WX2
      IX=WX
      IY1=WY1
      IY2=WY2
      WRITE(6,45) J,F,X1,X2,IP,A1,A2,A,S1,S2,AX,NX,XF,Y1,Y2,IPX,
     *IX1,IX2,IX,S3,S4,AY,YF,B,IPY,IY1,IY2,IY,FX,FY
   45 FORMAT(1H ,1X,I4,I6,F7.2,F6.2,I7,F8.2,F7.2,F8.2,3F7.2/1H ,1X,
     *I4,F7.2,2F6.2,I8,2I7,I8,3F7.2/1H ,5X,F7.2,F8.1,I12,
     *2I7,I8,2F7.2/1H ,1X,'|---|------|',11('-'),
     *'|-------|',2(6('-'),'|'),'-------|',3(6('-'),'|'))
  150 CONTINUE
  151 STOP
      END
      SUBROUTINE PIRM(NX,I,N,X,Y,A,WX,WY)
      COMMON NN,F,B,XF,YF
      IF(NN.EQ.1) GO TO 2
      NF=XF*100
      XF=0.01*NF
      NF=YF*100
      YF=0.01*NF
    2 GO TO(3,6,9,12,15),I
    3 A=B*B/8
      WX=B*B*B*B/384
      WY=WX/4
      IF(NX.NE.6.AND.NX.NE.11) GO TO 18
      WX=WY
      WY=WX*4
      GO TO 18
    6 A=B*B/16
      WX=B*B*B*B/3072
      WY=WX
      GO TO 18
    9 PI=3.141593
      A=B*B*PI/16
      WX=B*B*B*B*PI/1024
      WY=WX
      GO TO 18
   12 PI=3.141593
      A=B*B*PI/32
      WX=B*B*B*B*PI/2048
      WY=B*B*B*B*0.00686/16
      IF(NX.NE.4.AND.NX.NE.7.AND.NX.NE.8.AND.NX.NE.13.AND.NX.NE.15.
     *AND.NX.NE.16.AND.NX.NE.17.AND.NX.NE.31) GO TO 18
      W=WY
      WY=WX
      WX=W
      GO TO 18
   15 A=B*B/8
      WX=B*B*B*B*3/2304
      WY=B*B*B*B/576
   18 WX=(WX+A*Y*Y)*N
      WY=(WY+A*X*X)*N
      A=A*N
   21 RETURN
      END
