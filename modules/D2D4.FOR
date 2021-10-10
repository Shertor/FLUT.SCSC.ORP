
      SUBROUTINE D2D4 (D0,D2,D4,M)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER P
      DIMENSION D0(M,M),D2(M,M),D4(M,M),TP(50),
     1TP1(50),TP2(50),TP3(50),TP4(50)
C
      IF (M.GT.50) THEN
      WRITE (*,*) 'M больше 50. Stop.'
      STOP
      ENDIF
C
      PI=3.141592653589D0
      DO 2 MU=1,M
      Z=DCOS((2.*FLOAT(MU)-1.)*PI/2./FLOAT(M))
      Z2=Z*Z
      Z3=Z2*Z
      Z4=Z3*Z
      CALL DIFT (Z,M,TP,TP1,TP2,TP3,TP4)
      DO 2 I=1,M
      TI=(2.*I-1.)*PI/2./M
      SUM=0.D0
      SUM1=0.D0
      SUM0=0.D0
      DO 3 P=0,M-1
      TPI=DCOS(P*TI)
      IF (P.EQ.0) TPI=0.5*TPI
      D0DZ0=(Z*Z-1.)**2*TP(P+1)
      D2DZ2=(12.*Z2-4.)*TP(P+1)+(8.*Z3-8.*Z)*TP1(P+1)+
     +(Z4-2.*Z2+1.)*TP2(P+1)
      D4DZ4=24.*TP(P+1)+96.*Z*TP1(P+1)+(72.*Z2-
     -24.)*TP2(P+1)+(16.*Z3-16.*Z)*TP3(P+1)+
     +(Z4-2.*Z2+1.)*TP4(P+1)
      SUM0=SUM0+TPI*D0DZ0/DSIN(TI)**4
      SUM=SUM+TPI*D2DZ2/DSIN(TI)**4
    3 SUM1=SUM1+TPI*D4DZ4/DSIN(TI)**4
      D0(MU,I)=2.*SUM0/M
      D2(MU,I)=2.*SUM/M
    2 D4(MU,I)=2.*SUM1/M
      RETURN
      END
