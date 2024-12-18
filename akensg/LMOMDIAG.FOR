      DIMENSION WM(50),SM(50),ZM(50),SZ(50)
      OPEN(UNIT=5,FILE='LMOMDIAG.DAT')
      OPEN(UNIT=6,FILE='LMOMDIAG.RES')
      READ(5,*) P,W2,B,Z,YP
      W2=0-W2
      BF=(W2*W2+B*B)**0.5
      SIX=W2/BF
      COX=B/BF
      IG=B/Z+1
      WP=W2/B
      CALL PAP(P,BF,Z,SIX,COX,IG,WP,WM,SM,ZM,SZ)
      WRITE(6,*) (ZM(J),J=1,IG)
      WRITE(6,*) (WM(J),J=1,IG)
      WRITE(6,*) (SM(J),J=1,IG)
      WRITE(6,*) (SZ(J),J=1,IG)
      STOP
      END
      SUBROUTINE PAP(P,BF,Z,SIX,COX,IG,WP,WM,SM,ZM,SZ)
      DIMENSION WM(IG),SM(IG),ZM(IG),SZ(IG)
      WA=0.0
      ZA=0.0
      SM(1)=0.0
      X=0.0
      DX=Z/COX
      DO 5 I=2,18
      X=X+DX
      WL=(BF-X)*P*X*0.5
      Z=COX*X-SIX*WL
      WX=WP*Z+W1
      ZM(I)=Z
      SZ(I)=Z-ZA
      WM(I)=WX+WL/COX
      SM(I)=WM(I)-WA
      WA=WM(I)
      ZA=Z
    5 CONTINUE
      RETURN
      END
