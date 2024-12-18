      DIMENSION FIM(220),DLK(84,5),DNK(50,8),DDV(23,6),DLV(22,7),
     *DOM(20,2),DMM(83,9)
      DATA FIM /0.999,0.998,0.996,0.995,0.994,0.993,0.992,0.99,0.989,
     *0.988,0.986,0.984,0.982,0.980,0.978,0.975,0.973,0.971,0.969,
     *0.967,0.964,0.961,0.959,0.956,0.953,0.950,0.947,0.945,0.942,
     *0.939,0.936,0.932,0.929,0.926,0.922,0.919,0.916,0.913,0.909,
     *0.906,0.902,0.899,0.895,0.891,0.888,0.884,0.880,0.876,0.873,
     *0.869,0.865,0.861,0.856,0.852,0.848,0.844,0.840,0.835,0.831,
     *0.827,0.822,0.818,0.814,0.809,0.804,0.800,0.796,0.791,0.786,
     *0.782,0.777,0.772,0.768,0.763,0.758,0.753,0.748,0.744,0.739,
     *0.734,0.727,0.720,0.713,0.706,0.700,0.693,0.686,0.679,0.672,
     *0.665,0.658,0.652,0.645,0.639,0.632,0.625,0.619,0.612,0.606,
     *0.599,0.593,0.587,0.580,0.574,0.568,0.562,0.556,0.549,0.543,
     *0.537,0.531,0.525,0.520,0.514,0.508,0.502,0.496,0.491,0.485,
     *0.479,0.474,0.468,0.463,0.457,0.452,0.447,0.441,0.436,0.430,
     *0.425,0.420,0.415,0.410,0.405,0.400,0.396,0.391,0.386,0.381,
     *0.376,0.371,0.366,0.362,0.357,0.352,0.347,0.342,0.338,0.333,
     *0.328,0.324,0.320,0.317,0.313,0.309,0.305,0.301,0.298,0.294,
     *0.290,0.287,0.284,0.281,0.278,0.274,0.271,0.268,0.265,0.262,
     *0.259,0.256,0.254,0.251,0.249,0.246,0.243,0.241,0.238,0.236,
     *0.233,0.231,0.228,0.226,0.224,0.222,0.219,0.217,0.215,0.212,
     *0.210,0.208,0.206,0.204,0.202,0.200,0.199,0.197,0.195,0.193,
     *0.191,0.189,0.188,0.186,0.184,0.182,0.181,0.179,0.177,0.176,
     *0.174,0.173,0.171,0.170,0.168,0.167,0.166,0.164,0.163,0.161,0.16/
      DATA DLK /20,25,20,28,25,32,36,40,32,45,36,50,40,45,40,50,45,
     *56,50,63,56,63,70,70,63,75,70,80,75,80,70,75,90,70,80,75,80,
     *90,75,100,100,90,110,90,100,110,100,125,125,100,125,140,100,
     *140,125,100,160,140,125,160,160,125,180,180,160,200,160,200,
     *200,160,160,220,200,220,200,250,250,200,250,250,200,250,250,250,
     *3,3,4,3,4,3,3,3,4,3,4,3,4,4,5,4,5,4,5,4,5,5,4,5,6,5,6,5,6,6,
     *7,7,6,8,7,8,8,7,9,6,7,8,7,9,8,8,10,8,9,12,10,9,14,10,12,16,
     *10,12,14,11,12,16,11,12,14,12,16,13,14,18,20,14,16,16,20,16,18,
     *25,20,22,30,25,28,30,1.13,1.43,1.46,1.62,1.86,1.86,2.1,2.35,
     *2.43,2.65,2.75,2.96,3.08,3.48,3.79,3.89,4.29,4.38,4.8,4.96,
     *5.41,6.13,6.2,6.86,7.28,7.39,8.15,8.63,8.78,9.38,9.42,10.1,
     *10.6,10.7,10.8,11.5,12.3,12.3,12.8,12.8,13.8,13.9,15.2,15.6,
     *15.6,17.2,19.2,19.7,22.0,22.8,24.3,24.7,26.3,27.3,28.9,29.7,
     *31.4,32.5,33.4,34.4,37.4,37.8,38.8,42.2,43.3,47.1,49.1,50.9,
     *54.6,54.8,60.4,60.4,62.,68.6,76.5,78.4,87.7,94.3,97.0,106.1,
     *111.5,119.7,133.1,142.0,0.4,0.81,0.5,1.16,1.03,1.77,2.56,3.55,
     *2.26,5.13,3.29,7.11,4.58,6.63,5.53,9.21,8.03,13.1,11.2,18.9,16.0,
     *23.1,29.0,31.9,27.1,39.5,37.6,52.7,46.6,57.0,43.0,53.3,82.1,
     *48.2,65.3,59.8,73.4,94.3,66.1,122.0,131.0,106.0,176.0,118.0,
     *147.0,198.0,179.0,294.0,327.0,209.0,360.0,466.0,237.0,512.0,
     *422.0,264.0,774.0,602.0,482.0,844.0,913.0,539.0,1216.0,1317.0,
     *1046.0,1823.0,1175.0,1961.0,2097.0,1299.0,1419.0,2814.0,2363.0,
     *3175.0,2871.0,4717.0,5247.0,3466.0,5765.0,6270.0,4020.0,7006.0,
     *7717.0,8177.0,0.6,0.73,0.64,0.8,0.76,0.89,0.99,1.09,0.94,
     *1.21,1.04,1.33,1.13,1.26,1.17,1.38,1.3,1.52,1.42,1.69,1.57,
     *1.74,1.88,1.9,1.78,2.02,1.94,2.17,2.06,2.19,1.99,2.1,2.43,2.02,
     *2.23,2.15,2.27,2.47,2.18,2.68,2.71,2.51,2.96,2.55,2.75,3.0,
     *2.83,3.36,3.4,2.91,3.45,3.78,2.99,3.82,3.53,3.06,4.3,3.9,3.61,
     *4.35,4.39,3.68,4.85,4.89,4.47,5.37,4.55,5.42,5.46,4.63,4.7,
     *5.93,5.54,6.02,5.7,6.75,6.83,5.89,6.91,7.0,6.07,7.11,7.23,7.31/
      DATA DNK /25,32,40,32,45,50,40,45,50,56,63,56,63,70,63,75,
     *80,75,80,63,90,90,75,100,100,90,110,100,110,125,100,125,140,
     *125,140,160,125,160,180,160,180,160,200,200,200,250,200,250,
     *250,250,16,20,25,20,28,32,25,28,32,36,40,36,40,45,40,50,
     *50,50,50,40,56,56,50,63,63,56,70,63,70,80,63,80,90,80,90,100,
     *80,100,110,100,110,100,125,125,125,160,125,160,160,160,3,3,3,
     *4,3,3,4,4,4,4,4,5,5,5,6,5,5,6,6,8,5,6,8,6,7,8,6,8,8,7,10,8,8,
     *10,10,9,12,10,10,12,12,14,11,12,14,12,16,16,18,20,1.16,1.49,
     *1.89,1.94,2.14,2.42,2.47,2.8,3.17,3.58,4.04,4.41,4.98,5.59,
     *5.9,6.11,6.36,7.25,7.55,7.68,7.86,8.54,9.47,9.59,11.1,11.18,
     *11.4,12.6,13.9,14.1,15.5,16.0,18.0,19.7,22.2,22.9,23.4,25.3,
     *28.3,30.0,33.7,34.7,34.9,37.9,43.9,48.3,49.8,63.6,71.1,78.5,
     *0.7,1.52,3.06,1.93,4.41,6.17,3.93,5.68,7.98,11.4,16.3,13.8,
     *19.9,27.8,23.3,34.8,41.6,40.9,49.0,29.6,65.3,70.6,52.4,98.3,
     *113.0,90.9,142.0,127.0,172.0,227.0,154.0,256.0,364.0,312.0,
     *444.0,606.0,365.0,667.0,952.0,784.0,1123.0,897.0,1449.0,1568.0,
     *1801.0,3147.0,2026.0,4091.0,4545.0,4987.0,0.22,0.46,0.93,0.57,
     *1.32,1.99,1.18,1.69,2.56,3.7,5.16,4.48,6.26,9.05,7.28,12.5,
     *12.7,14.6,14.8,9.15,19.7,21.2,18.5,30.6,35.5,27.1,45.6,39.2,
     *54.6,73.7,47.1,83.0,120.0,100.0,146.0,186.0,117.0,204.0,276.0,
     *239.0,324.0,272.0,446.0,482.0,551.0,1032.0,617.0,1333.0,1474.0,
     *1613.0,0.86,1.08,1.32,1.12,1.47,1.6,1.37,1.51,1.85,1.82,2.03,
     *1.86,2.08,2.28,2.12,2.39,2.6,2.44,2.65,2.2,2.92,2.95,2.52,3.23,
     *3.28,3.04,3.55,3.32,3.61,4.01,3.4,4.05,4.49,4.14,4.58,5.19,
     *4.22,5.23,5.88,5.32,5.97,5.4,6.5,6.54,6.62,7.97,6.71,8.14,8.23,
     *8.31,0.42,0.49,0.59,0.53,0.64,0.72,0.63,0.68,0.76,0.84,0.91,
     *0.88,0.95,1.05,0.99,1.17,1.13,1.21,1.17,1.07,1.26,1.28,1.29,
     *1.42,1.46,1.36,1.58,1.5,1.64,1.8,1.58,1.84,2.03,1.92,2.12,2.23,
     *2.0,2.28,2.44,2.36,2.52,2.43,2.79,2.83,2.91,3.53,2.99,3.69,
     *3.77,3.85/
      DATA DDV /10,12,14,16,18,18,20,20,22,22,24,24,27,27,30,30,33,
     *36,40,45,50,55,60,55,64,73,81,90,100,100,110,110,120,115,125,
     *125,135,135,145,140,145,155,160,170,180,190,4.5,4.8,4.9,5.0,
     *5.1,5.1,5.2,5.2,5.4,5.4,5.6,5.6,6.,6.,6.5,6.5,7.,7.5,8.3,9.,
     *10,11,12,12.0,14.7,17.4,20.2,23.4,25.4,26.8,28.9,30.6,32.8,
     *34.8,37.5,40.2,43.2,46.5,49.9,53.8,61.9,72.6,84.7,100,118,138,
     *198,350,572,873,1290,1430,1840,2030,2550,2790,3460,3800,5010,
     *5500,7080,7780,9840,13380,19062,27696,39727,55962,76806,17.9,
     *27.9,41.9,58.6,82.6,114,115,155,157,206,198,260,260,337,337,436,
     *419,516,667,808,1043,1356,1725/
      DATA DLV /50,65,80,100,120,140,140,160,160,180,180,200,200,220,
     *220,240,240,270,300,330,360,400,32,36,40,46,52,58,62,64,68,
     *70,74,76,80,82,87,90,95,95,100,105,110,115,4.4,4.4,4.5,4.5,
     *4.8,4.9,4.9,5.0,5.0,5.1,5.1,5.2,5.2,5.4,5.4,5.6,5.6,6.0,6.5,
     *7.0,7.5,8.0,6.16,7.51,8.98,10.9,13.3,15.6,17.,18.1,19.5,20.7,
     *22.2,23.4,25.2,26.7,28.8,30.6,32.9,35.2,40.5,46.5,53.4,61.5,
     *22.8,48.6,89.4,174,304,491,545,747,823,1090,1190,1520,1670,
     *2110,2330,2900,3180,4160,5810,7980,10820,15220,5.61,8.7,12.8,
     *20.4,31.2,45.4,57.5,63.3,78.8,86,105,113,139,151,187,208,254,
     *262,327,410,513,642,1.16,1.24,1.31,1.44,1.54,1.67,1.87,1.8,2.,
     *1.94,2.13,2.07,2.28,2.21,2.46,2.42,2.67,2.47,2.52,2.59,2.68,
     *2.75/
      DATA DOM /300,320,340,360,380,400,420,440,460,480,500,520,
     *540,560,580,600,620,640,660,680,2.5,2.7,2.9,3.1,3.3,3.5,3.7,3.9,
     *4.1,4.3,4.5,4.7,4.8,4.9,5.0,5.1,5.2,5.3,5.4,5.5/
    7 PRINT *,'Iveskite uzduoties varianto Nr'
      READ(5,*) NV
      PRINT 1
    1 FORMAT(1H ,5X,47('-')/1H ,5X,'|Variantas|Schema|Apkrova| H m |',
     *' Skerspjuvis  |'/1H ,5X,'|---------|------|-------|-----',
     *'|--------------|')
      NX=1
      ND=0
      I1=0
      I2=0
      DO 150 J=1,2000
      ND=ND+1
      IF(ND.GT.32) ND=ND-32
      I1=I1+3
      IF(ND.EQ.11) I1=I1+1
      IF(I1.GT.20) I1=I1-20
      NX=NX+5
      IF(NX.GT.32) NX=NX-32
      LK=NX/4
      LK=NX-LK*4
      UM=2.0
      IF(LK.EQ.1) UM=0.5
      IF(LK.EQ.2) UM=0.7
      IF(LK.EQ.3) UM=1.0
      I2=I2+7
      IF(ND.EQ.17) I2=I2+1
      IF(I2.GT.20) I2=I2-20
      IF(J.LT.NV) GO TO 150
      IF(J.GT.NV) GO TO 151
      DO 6 K=1,83
      DO 4 L=1,9
      DMM(K,L)=0.0
    4 CONTINUE
    6 CONTINUE
      H=DOM(I1,2)
      F=DOM(I2,1)
    8 N=0
      GO TO(11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,
     *27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42),NX
   11 NE=4
      UM=0.5
      NG=50
      DO 51 I=1,NG
      IH=DNK(I,1)
      IB=DNK(I,2)
      D=DNK(I,3)
      A1=DNK(I,4)
      X1=DNK(I,5)
      Y1=DNK(I,6)
      XO=DNK(I,8)
      YO=DNK(I,7)
        A=A1*4
      WX=(Y1+A1*(0.05*IB-XO)**2)*4
      WY=X1*4+(YO*YO+(0.1*IH-YO)**2)*A1*2
      CALL SKAI(N,I,NG,F,H,IB,IH,D,WX,WY,A,UM,FIM,DMM,NN)
   51 CONTINUE
      IF(J.LT.NV) GO TO 150
      CALL NLK(J,NX,F,H,NE,NN,DMM)
      GO TO 150
   12 NE=2
      UM=0.7
      D=0.0
      NG=22
      DO 52 I=1,NG
      IH=DLV(I,1)
      IB=DLV(I,2)
      A1=DLV(I,4)
      X1=DLV(I,5)
      Y1=DLV(I,6)
      Z=DLV(I,7)
      A=A1*2
      WX=X1*2
      WY=(Y1+A1*(0.1*IB-Z)**2)*2
      CALL SKAI(N,I,NG,F,H,IB,IH,D,WX,WY,A,UM,FIM,DMM,NN)
   52 CONTINUE
      IF(J.LT.NV) GO TO 150
      CALL DVL(J,NX,NG,F,H,NE,NN,DMM)
      GO TO 150
   13 NE=3
      UM=1.0
      NG=23
      DO 53 I=1,NG
      IH=DDV(I,1)*10
      IB=DDV(I,2)
      D=DDV(I,3)
      A1=DDV(I,4)
      X1=DDV(I,5)
      Y1=DDV(I,6)
      A=A1*NE
      WX=X1*2+Y1
      WY=(Y1+A1*(0.05*(IH+D))**2)*2+X1
      CALL SKAI(N,I,NG,F,H,IB,IH,D,WX,WY,A,UM,FIM,DMM,NN)
   53 CONTINUE
      IF(J.LT.NV) GO TO 150
      CALL DVL(J,NX,NG,F,H,NE,NN,DMM)
      GO TO 150
   14 NE=8
      UM=2.0
      NG=83
      DO 54 I=1,NG
      IH=DLK(I,1)
      IB=IH
      D=DLK(I,2)
      A1=DLK(I,3)
      X1=DLK(I,4)
      Z=DLK(I,5)
      A=A1*NE
      WX=8*X1+4*A1*(Z**2+(0.1*IB-Z)**2)
      WY=WX
      CALL SKAI(N,I,NG,F,H,IB,IH,D,WX,WY,A,UM,FIM,DMM,NN)
   54 CONTINUE
      IF(J.LT.NV) GO TO 150
      CALL NLK(J,NX,F,H,NE,NN,DMM)
      GO TO 150
   15 NE=4
      UM=0.5
      NG=50
      DO 55 I=1,NG
      IH=DNK(I,1)
      IB=DNK(I,2)
      D=DNK(I,3)
      A1=DNK(I,4)
      X1=DNK(I,6)
      Y1=DNK(I,5)
      XO=DNK(I,7)
      YO=DNK(I,8)
      A=A1*NE
      WX=(X1+A1*YO**2)*4
      WY=(Y1+A1*XO**2)*4
      CALL SKAI(N,I,NG,F,H,IB,IH,D,WX,WY,A,UM,FIM,DMM,NN)
   55 CONTINUE
      IF(J.LT.NV) GO TO 150
      CALL NLK(J,NX,F,H,NE,NN,DMM)
      GO TO 150
   16 NE=8
      UM=0.7
      NG=83
      DO 56 I=1,NG
      IH=DLK(I,1)
      IB=IH
      D=DLK(I,2)
      A1=DLK(I,3)
      X1=DLK(I,4)
      Z=DLK(I,5)
      A=A1*NE
      WX=(X1+A1*(0.1*IB-Z)**2)*8
      WY=X1*8+((0.2*IB-Z)**2+Z*Z)*A1*4
      CALL SKAI(N,I,NG,F,H,IB,IH,D,WX,WY,A,UM,FIM,DMM,NN)
   56 CONTINUE
      IF(J.LT.NV) GO TO 150
      CALL NLK(J,NX,F,H,NE,NN,DMM)
      GO TO 150
   17 NE=4
      UM=1.0
      D=0.0
      NG=22
      DO 57 I=1,NG
      IH=DLV(I,1)
      IB=DLV(I,2)
      A1=DLV(I,4)
      X1=DLV(I,5)
      Y1=DLV(I,6)
      Z=DLV(I,7)
      A=A1*4
      WX=X1*4
      WY=4*Y1+((0.1*IB+Z)**2+Z*Z)*A1*2
      CALL SKAI(N,I,NG,F,H,IB,IH,D,WX,WY,A,UM,FIM,DMM,NN)
   57 CONTINUE
      IF(J.LT.NV) GO TO 150
      CALL DVL(J,NX,NG,F,H,NE,NN,DMM)
      GO TO 150
   18 NE=4
      UM=2.0
      NG=50
      DO 58 I=2,NG
      IH=DNK(I,1)
      IB=DNK(I,2)
      D=DNK(I,3)
      A1=DNK(I,4)
      X1=DNK(I,5)
      Y1=DNK(I,6)
      YO=DNK(I,7)
      XO=DNK(I,8)
      A=A1*NE
      WY=(X1+A1*YO**2)*4
      WX=(Y1+A1*(0.1*IB-XO)**2)*4
      CALL SKAI(N,I,NG,F,H,IB,IH,D,WX,WY,A,UM,FIM,DMM,NN)
   58 CONTINUE
      IF(J.LT.NV) GO TO 150
      CALL NLK(J,NX,F,H,NE,NN,DMM)
      GO TO 150
   19 NE=2
      UM=0.5
      NG=50
      DO 59 I=1,NG
      IH=DNK(I,1)
      IB=DNK(I,2)
      D=DNK(I,3)
      A1=DNK(I,4)
      X1=DNK(I,5)
      Y1=DNK(I,6)
      XO=DNK(I,8)
      YO=DNK(I,7)
      A=A1*NE
      WX=(X1+A1*(0.05*IH-YO)**2)*2
      WY=(Y1+A1*(0.05*IB-XO)**2)*2
      CALL SKAI(N,I,NG,F,H,IB,IH,D,WX,WY,A,UM,FIM,DMM,NN)
   59 CONTINUE
      IF(J.LT.NV) GO TO 150
      CALL NLK(J,NX,F,H,NE,NN,DMM)
      GO TO 150
   20 NE=2
      UM=0.7
      D=0.0
      NG=22
      DO 60 I=1,NG
      IH=DLV(I,1)
      IB=DLV(I,2)
      A1=DLV(I,4)
      X1=DLV(I,5)
      Y1=DLV(I,6)
      Z=DLV(I,7)
      A=A1*NE
      WX=X1*2
      WY=(Y1+Z*Z*A1)*2
      CALL SKAI(N,I,NG,F,H,IB,IH,D,WX,WY,A,UM,FIM,DMM,NN)
   60 CONTINUE
      IF(J.LT.NV) GO TO 150
      CALL DVL(J,NX,NG,F,H,NE,NN,DMM)
      GO TO 150
   21 NE=4
      UM=1.0
      NG=83
      DO 61 I=1,NG
      IH=DLK(I,1)
      IB=IH
      D=DLK(I,2)
      A1=DLK(I,3)
      X1=DLK(I,4)
      Z=DLK(I,5)
      A=A1*NE
      WX=(X1+A1*(0.1*IB-Z)**2)*4
      WY=WX
      CALL SKAI(N,I,NG,F,H,IB,IH,D,WX,WY,A,UM,FIM,DMM,NN)
   61 CONTINUE
      IF(J.LT.NV) GO TO 150
      CALL NLK(J,NX,F,H,NE,NN,DMM)
      GO TO 150
   22 NE=2
      UM=2.0
      NG=23
      DO 62 I=1,NG
      IH=DDV(I,1)*10
      IB=DDV(I,2)
      D=DDV(I,3)
      A1=DDV(I,4)
      X1=DDV(I,5)
      Y1=DDV(I,6)
      A=A1*NE
      WX=X1*2
      WY=(Y1+A1*(0.05*IB)**2)*2
      CALL SKAI(N,I,NG,F,H,IB,IH,D,WX,WY,A,UM,FIM,DMM,NN)
   62 CONTINUE
      IF(J.LT.NV) GO TO 150
      CALL DVL(J,NX,NG,F,H,NE,NN,DMM)
      GO TO 150
   23 NE=4
      UM=0.5
      D=0.0
      NG=22
      DO 63 I=1,NG
      IH=DLV(I,1)
      IB=DLV(I,2)
      A1=DLV(I,4)
      X1=DLV(I,5)
      Y1=DLV(I,6)
      Z=DLV(I,7)
      A=A1*NE
      WX=X1*4
      WY=Y1*4+(Z*Z+(0.2*IB-Z)**2)*A1*2
      CALL SKAI(N,I,NG,F,H,IB,IH,D,WX,WY,A,UM,FIM,DMM,NN)
   63 CONTINUE
      IF(J.LT.NV) GO TO 150
      CALL DVL(J,NX,NG,F,H,NE,NN,DMM)
      GO TO 150
   24 NE=4
      UM=0.7
      D=0
      NG=22
      DO 64 I=1,NG
      IH=DLV(I,1)
      IB=DLV(I,2)
      A1=DLV(I,4)
      X1=DLV(I,5)
      Y1=DLV(I,6)
      Z=DLV(I,7)
      A=A1*NE
      WX=X1*4
      WY=Y1*4+((0.1*IB-Z)**2+(0.2*IB-Z)**2)*A1*2
      CALL SKAI(N,I,NG,F,H,IB,IH,D,WX,WY,A,UM,FIM,DMM,NN)
   64 CONTINUE
      IF(J.LT.NV) GO TO 150
      CALL DVL(J,NX,NG,F,H,NE,NN,DMM)
      GO TO 150
   25 NE=2
      UM=1.0
      NG=50
      DO 65 I=1,NG
      IH=DNK(I,1)
      IB=DNK(I,2)
      D=DNK(I,3)
      A1=DNK(I,4)
      X1=DNK(I,5)
      Y1=DNK(I,6)
      XO=DNK(I,8)
      YO=DNK(I,7)
      A=A1*NE
      WX=X1*2
      WY=(Y1+A1*XO**2)*2
      CALL SKAI(N,I,NG,F,H,IB,IH,D,WX,WY,A,UM,FIM,DMM,NN)
   65 CONTINUE
      IF(J.LT.NV) GO TO 150
      CALL NLK(J,NX,F,H,NE,NN,DMM)
      GO TO 150
   26 NE=4
      UM=2.0
      NG=50
      DO 66 I=1,NG
      IH=DNK(I,1)
      IB=DNK(I,2)
      D=DNK(I,3)
      A1=DNK(I,4)
      X1=DNK(I,5)
      Y1=DNK(I,6)
      XO=DNK(I,8)
      YO=DNK(I,7)
      A=A1*NE
      WX=(X1+A1*(0.05*IH-YO)**2)*4
      WY=Y1*4+((0.1*IB+XO)**2+XO*XO)*A1*2
      CALL SKAI(N,I,NG,F,H,IB,IH,D,WX,WY,A,UM,FIM,DMM,NN)
   66 CONTINUE
      IF(J.LT.NV) GO TO 150
      CALL NLK(J,NX,F,H,NE,NN,DMM)
      GO TO 150
   27 NE=4
      UM=0.5
      NG=83
      DO 67 I=1,NG
      IH=DLK(I,1)
      IB=IH
      D=DLK(I,2)
      A1=DLK(I,3)
      X1=DLK(I,4)
      Z=DLK(I,5)
      A=A1*NE
      WX=(X1+A1*(0.05*IB-Z)**2)*4
      WY=X1*4+(Z*Z+(0.1*IB+Z)**2)*A1*2
      CALL SKAI(N,I,NG,F,H,IB,IH,D,WX,WY,A,UM,FIM,DMM,NN)
   67 CONTINUE
      IF(J.LT.NV) GO TO 150
      CALL NLK(J,NX,F,H,NE,NN,DMM)
      GO TO 150
   28 NE=4
      UM=0.7
      NG=50
      DO 68 I=1,NG
      IH=DNK(I,1)
      IB=DNK(I,2)
      D=DNK(I,3)
      A1=DNK(I,4)
      X1=DNK(I,5)
      Y1=DNK(I,6)
      XO=DNK(I,8)
      YO=DNK(I,7)
      A=A1*NE
      WX=(Y1+A1*(0.1*IB-XO)**2)*4
      WY=(X1+A1*(0.1*IH-YO)**2)*4
      CALL SKAI(N,I,NG,F,H,IB,IH,D,WX,WY,A,UM,FIM,DMM,NN)
   68 CONTINUE
      IF(J.LT.NV) GO TO 150
      CALL NLK(J,NX,F,H,NE,NN,DMM)
      GO TO 150
   29 NE=4
      UM=1.0
      NG=83
      DO 69 I=1,NG
      IH=DLK(I,1)
      IB=IH
      D=DLK(I,2)
      A1=DLK(I,3)
      X1=DLK(I,4)
      Z=DLK(I,5)
      A=A1*NE
      WX=(X1+A1*(0.1*IB-Z)**2)*4
      WY=(X1+A1*Z*Z)*4
      CALL SKAI(N,I,NG,F,H,IB,IH,D,WX,WY,A,UM,FIM,DMM,NN)
   69 CONTINUE
      IF(J.LT.NV) GO TO 150
      CALL NLK(J,NX,F,H,NE,NN,DMM)
      GO TO 150
   30 NE=4
      UM=2.0
      D=0.0
      NG=22
      DO 70 I=1,NG
      IH=DLV(I,1)
      IB=DLV(I,2)
      A1=DLV(I,4)
      X1=DLV(I,5)
      Y1=DLV(I,6)
      Z=DLV(I,7)
      A=A1*NE
      WX=(X1+Y1+Z*Z*A1)*2
      WY=(X1+Y1+A1*(0.05*IH+Z)**2)*2
      CALL SKAI(N,I,NG,F,H,IB,IH,D,WX,WY,A,UM,FIM,DMM,NN)
   70 CONTINUE
      IF(J.LT.NV) GO TO 150
      CALL DVL(J,NX,NG,F,H,NE,NN,DMM)
      GO TO 150
   31 NE=4
      UM=0.5
      NG=83
      DO 71 I=1,NG
      IH=DLK(I,1)
      IB=IH
      D=DLK(I,2)
      A1=DLK(I,3)
      X1=DLK(I,4)
      Z=DLK(I,5)
      A=A1*NE
      WX=(X1+A1*Z*Z)*4
      WY=WX
      CALL SKAI(N,I,NG,F,H,IB,IH,D,WX,WY,A,UM,FIM,DMM,NN)
   71 CONTINUE
      IF(J.LT.NV) GO TO 150
      CALL NLK(J,NX,F,H,NE,NN,DMM)
      GO TO 150
   32 NE=4
      UM=0.7
      D=0
      NG=22
      DO 72 I=1,NG
      IH=DLV(I,1)
      IB=DLV(I,2)
      A1=DLV(I,4)
      X1=DLV(I,5)
      Y1=DLV(I,6)
      Z=DLV(I,7)
      A=A1*NE
      WX=X1*4
      WY=Y1*4+((0.1*IB-Z)**2+(0.1*IB+Z)**2)*A1*2
      CALL SKAI(N,I,NG,F,H,IB,IH,D,WX,WY,A,UM,FIM,DMM,NN)
   72 CONTINUE
      IF(J.LT.NV) GO TO 150
      CALL DVL(J,NX,NG,F,H,NE,NN,DMM)
      GO TO 150
   33 NE=4
      UM=1.0
      NG=83
      DO 73 I=1,NG
      IH=DLK(I,1)
      IB=IH
      D=DLK(I,2)
      A1=DLK(I,3)
      X1=DLK(I,4)
      Z=DLK(I,5)
      A=A1*NE
      WX=(X1+A1*(0.05*IB-Z)**2)*4
      WY=X1*4+(Z*Z+(0.1*IB-Z)**2)*A1*2
      CALL SKAI(N,I,NG,F,H,IB,IH,D,WX,WY,A,UM,FIM,DMM,NN)
   73 CONTINUE
      IF(J.LT.NV) GO TO 150
      CALL NLK(J,NX,F,H,NE,NN,DMM)
      GO TO 150
   34 NE=3
      UM=2.0
      NG=23
      DO 74 I=1,NG
      IH=DDV(I,1)*10
      IB=DDV(I,2)
      D=DDV(I,3)
      A1=DDV(I,4)
      X1=DDV(I,5)
      Y1=DDV(I,6)
      A=A1*NE
      WX=Y1*2+X1
      WY=X1*2+Y1+(0.05*D+0.05*IH)**2*A1*2
      CALL SKAI(N,I,NG,F,H,IB,IH,D,WX,WY,A,UM,FIM,DMM,NN)
   74 CONTINUE
      IF(J.LT.NV) GO TO 150
      CALL DVL(J,NX,NG,F,H,NE,NN,DMM)
      GO TO 150
   35 NE=4
      UM=0.5
      NG=50
      DO 75 I=1,NG
      IH=DNK(I,1)
      IB=DNK(I,2)
      D=DNK(I,3)
      A1=DNK(I,4)
      X1=DNK(I,5)
      Y1=DNK(I,6)
      XO=DNK(I,8)
      YO=DNK(I,7)
      A=A1*NE
      WX=(X1+A1*(0.05*IH-YO)**2)*4
      WY=Y1*4+(XO*XO+(0.1*IB-XO)**2)*A1*2
      CALL SKAI(N,I,NG,F,H,IB,IH,D,WX,WY,A,UM,FIM,DMM,NN)
   75 CONTINUE
      IF(J.LT.NV) GO TO 150
      CALL NLK(J,NX,F,H,NE,NN,DMM)
      GO TO 150
   36 NE=4
      UM=0.7
      NG=50
      DO 76 I=1,NG
      IH=DNK(I,1)
      IB=DNK(I,2)
      D=DNK(I,3)
      A1=DNK(I,4)
      X1=DNK(I,5)
      Y1=DNK(I,6)
      XO=DNK(I,8)
      YO=DNK(I,7)
      A=A1*NE
      WX=(Y1+A1*XO*XO)*4
      WY=(X1+A1*(0.1*IH-YO)**2)*4
      CALL SKAI(N,I,NG,F,H,IB,IH,D,WX,WY,A,UM,FIM,DMM,NN)
   76 CONTINUE
      IF(J.LT.NV) GO TO 150
      CALL NLK(J,NX,F,H,NE,NN,DMM)
      GO TO 150
   37 NE=2
      UM=1.0
      NG=83
      DO 77 I=1,NG
      IH=DLK(I,1)
      IB=IH
      D=DLK(I,2)
      A1=DLK(I,3)
      X1=DLK(I,4)
      Z=DLK(I,5)
        A=A1*2
      WX=(X1+A1*(0.05*IB-Z)**2)*2
      WY=WX
      CALL SKAI(N,I,NG,F,H,IB,IH,D,WX,WY,A,UM,FIM,DMM,NN)
   77 CONTINUE
      IF(J.LT.NV) GO TO 150
      CALL NLK(J,NX,F,H,NE,NN,DMM)
      GO TO 150
   38 NE=3
      UM=2.0
      NG=23
      DO 78 I=1,NG
      IH=DDV(I,1)*10
      IB=DDV(I,2)
      D=DDV(I,3)
      A1=DDV(I,4)
      X1=DDV(I,5)
      Y1=DDV(I,6)
      A=A1*NE
      WX=X1*3
      WY=Y1*3+0.02*IB*IB*A1
      CALL SKAI(N,I,NG,F,H,IB,IH,D,WX,WY,A,UM,FIM,DMM,NN)
   78 CONTINUE
      IF(J.LT.NV) GO TO 150
      CALL DVL(J,NX,NG,F,H,NE,NN,DMM)
      GO TO 150
   39 NE=4
      UM=0.5
      NG=50
      DO 79 I=1,NG
      IH=DNK(I,1)
      IB=DNK(I,2)
      D=DNK(I,3)
      A1=DNK(I,4)
      X1=DNK(I,5)
      Y1=DNK(I,6)
      XO=DNK(I,8)
      YO=DNK(I,7)
      A=A1*NE
      WX=(Y1+X1)*2+(((IB+IH)*0.05-YO)**2+((IB+IH)*0.05-XO)**2)*A1*2
      WY=WX
      CALL SKAI(N,I,NG,F,H,IB,IH,D,WX,WY,A,UM,FIM,DMM,NN)
   79 CONTINUE
      IF(J.LT.NV) GO TO 150
      CALL NLK(J,NX,F,H,NE,NN,DMM)
      GO TO 150
   40 NE=2
      UM=0.7
      NG=83
      DO 80 I=1,NG
      IH=DLK(I,1)
      IB=IH
      D=DLK(I,2)
      A1=DLK(I,3)
      X1=DLK(I,4)
      Z=DLK(I,5)
      A=A1*NE
      WX=X1*2
      WY=(X1+A1*Z*Z)*2
      CALL SKAI(N,I,NG,F,H,IB,IH,D,WX,WY,A,UM,FIM,DMM,NN)
   80 CONTINUE
      IF(J.LT.NV) GO TO 150
      CALL NLK(J,NX,F,H,NE,NN,DMM)
      GO TO 150
   41 NE=4
      UM=1.0
      D=0.0
      NG=22
      DO 81 I=1,NG
      IH=DLV(I,1)
      IB=DLV(I,2)
      A1=DLV(I,4)
      X1=DLV(I,5)
      Y1=DLV(I,6)
      Z=DLV(I,7)
      A=A1*NE
      WX=(X1+Y1+A1*(0.05*IH+Z)**2)*2
      WY=(X1+Y1+A1*(0.05*IH-Z)**2)*2
      CALL SKAI(N,I,NG,F,H,IB,IH,D,WX,WY,A,UM,FIM,DMM,NN)
   81 CONTINUE
      IF(J.LT.NV) GO TO 150
      CALL DVL(J,NX,NG,F,H,NE,NN,DMM)
      GO TO 150
   42 NE=8
      UM=2.0
      NG=83
      DO 82 I=1,NG
      IH=DLK(I,1)
      IB=IH
      D=DLK(I,2)
      A1=DLK(I,3)
      X1=DLK(I,4)
      Z=DLK(I,5)
      A=A1*NE
      WX=(X1+A1*(0.1*IB-Z)**2)*8
      WY=X1*8+(Z*Z+(0.1*IB+Z)**2)*A1*4
      CALL SKAI(N,I,NG,F,H,IB,IH,D,WX,WY,A,UM,FIM,DMM,NN)
   82 CONTINUE
      IF(J.LT.NV) GO TO 150
      CALL NLK(J,NX,F,H,NE,NN,DMM)
  150 CONTINUE
  151 PRINT 152
  152 FORMAT(2X,'Paaiskinimai: skerspjuvis ...K... ... ... reiskia: K',
     *' - kampuotis,'/28X,'ir lentynu plociai bei storis mm.'/16X,
     *'skerspjuvis ...LV Nr... reiskia lovinis profilis'/28X,'ir jo ',
     *'Nr sortimento lenteleje.'/16X,'skerspjuvis ...DT Nr... reiskia',
     *' dvitejinis profilis'/28X,'ir jo Nr sortimento lenteleje.')
      PRINT *,'Jei baigete, iveskite nuli, jei norite testi - ',
     *'bet koki skaitmeni'
      READ *,NON
      IF(NON.NE.0) GO TO 7
      STOP
      END
      SUBROUTINE SKAI(N,I,NG,F,H,IB,IH,D,WX,WY,A,UM,FIM,DMM,NN)
      DIMENSION FIM(220),DMM(83,9)
      IF(NG.GT.23) GO TO 3
      NB=0
      IF(NG.EQ.22) GO TO 1
      IF(I.EQ.6.OR.I.EQ.8.OR.I.EQ.10.OR.I.EQ.12.OR.I.EQ.14.
     *OR.I.EQ.16) NB=1
      GO TO 3
    1 IF(I.EQ.7.OR.I.EQ.9.OR.I.EQ.11.OR.I.EQ.13.OR.I.EQ.15.
     *OR.I.EQ.17) NB=1
    3 WN=WX
      IF(WY.LT.WX) WN=WY
      R=(WN/A)**0.5
      BD=H*UM*100/R
      IF(BD.GT.210.) GO TO 12
      DO 5 K=1,220
      SK=K-BD
      IF(SK.GT.0.) GO TO 6
    5 CONTINUE
    6 LD=BD
      FI=FIM(K-1)-(FIM(K-1)-FIM(K))*(BD-LD)
      SG=F/(A*FI)
      IF(SG.GT.16.) GO TO 12
      N=N+1
      DO 7 M=1,9
      DMM(N,M)=0.0
    7 CONTINUE
      DMM(N,1)=IH
      DMM(N,2)=IB
      DMM(N,3)=D
      DMM(N,4)=A
      DMM(N,5)=WX
      DMM(N,6)=WY
      DMM(N,7)=BD
      DMM(N,8)=FI
      DMM(N,9)=SG
    8 IF(I.LT.NG) GO TO 12
      SX=0.0
      DO 11 K=1,N
      IF(DMM(K,9).LT.SX) GO TO 11
      NN=K
      SX=DMM(K,9)
   11 CONTINUE
   12 RETURN
      END
      SUBROUTINE NLK(J,NX,F,H,NE,NN,DMM)
      DIMENSION DMM(83,9)
      KF=F
      IH=DMM(NN,1)
      IB=DMM(NN,2)
      ID=DMM(NN,3)
      PRINT 5, J,NX,KF,H,NE,IH,IB,ID
    5 FORMAT(1H,7X,I5,2I8,F7.1,I4,'K',I3,'x',I3,'x',I2)
      RETURN
      END
      SUBROUTINE DVL(J,NX,NG,F,H,NE,NN,DMM)
      DIMENSION DMM(83,9)
      KF=F
      IH=DMM(NN,1)/10
      IB=DMM(NN,2)
      IF(NG.EQ.22) GO TO 15
      IF(IB.GT.0) GO TO 10
      PRINT 5, J,NX,KF,H,NE,IH
    5 FORMAT(1H,7X,I5,2I8,F7.1,I6,'DT Nr',I2)
      GO TO 20
   10 PRINT 6, J,NX,KF,H,NE,IH
    6 FORMAT(1H,7X,I5,2I8,F7.1,I5,'DT Nr',I2,'a')
      GO TO 20
   15 IF(IB.GT.0) GO TO 17
      PRINT 7, J,NX,KF,H,NE,IH
    7 FORMAT(1H,7X,I5,2I8,F7.1,I6,'LV Nr',I2)
      GO TO 20
   17 PRINT 8, J,NX,KF,H,NE,IH
    8 FORMAT(1H,7X,I5,2I8,F7.1,I5,'LV Nr',I2,'a')
   20 RETURN
      END
