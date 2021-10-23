      PROGRAM FLUTBP14
C Флаттер прямоугольной пластины
      
      IMPLICIT REAL*8 (A-H,O-Z)
      character*26 PATH 
      REAL*8 K
      CHARACTER*3 KEY
      INTEGER :: i
      CHARACTER(len=32) :: arg
      COMMON /D/ Dx,Dxy,Dy
      COMMON /DATA/ K,H,BL,RO,DB,TJ1
      COMMON //PATH
      DATA PATH /'C:/reports/'/
C
      IF(COMMAND_ARGUMENT_COUNT().NE.8)THEN
            WRITE(*,*)'ERROR, 8 COMMAND-LINE ARGUMENTS REQUIRED'
            STOP
      ENDIF
      
C ОПРЕДЕЛЕНИЕ ХАРАКТЕРНЫХ ВЕЛИЧИН

C Характерное давление P0=1 атм (физическая)=1.0133E5 н/м**2=1.0333 кГ/см**2
      P0=1.0333

C Коэффициент Пуассона PNU=0.33
      PNU=0.33
      CALL get_command_argument(2, arg)
      READ(arg,*)PNU
      PNUx = PNU
      CALL get_command_argument(3, arg)
      READ(arg,*)PNUy
      
C Показатель политропы K=1.4 - это соответствует скорсти звука 331.26 м/сек
      K=1.4
C
      
C Безразмерная толщина пластинки H
      H = 0.007
      CALL get_command_argument(1, arg)
      READ(arg,*)H
C
      
C Модуль Юнга для аллюминия E=0.7E6 кГ/см**2
      E=0.7E6
      CALL get_command_argument(4, arg)
      READ(arg,*)E
      Ex = E
      CALL get_command_argument(5, arg)
      READ(arg,*)Ey
C

C     Плотность воздуха 1.2928 (кг/м**3)
      R=1.2928
      
C     Плотность материала пластины 2.7E3 (кг/м**3)
      RO=2.7E3
      CALL get_command_argument(6, arg)
      READ(arg,*)RO
C


C     Половина ширины пластины
      BL=1.D0
C

C     Скорости звука
      C0=SQRT(K*9.81523E4*P0/R)

   15 FORMAT('Sonic speed  C0 = ',E12.5)
      C1=1.D2*SQRT(9.81523E4*E/RO)
      C1=SQRT((1.D6/10.19762)*E/RO)
   16 FORMAT('Sonic speed in the Plate C1 = ',E12.5)
      C2=C1/C0
   17 FORMAT('Dimensiolens sonic speed in the Plate C2 = ',E12.5)  

C     Безразмерное значение модуля Юнга ( E/P0 ) для аллюминия
      E=E/P0
      Ex=Ex/P0
      Ey=Ey/P0

C     Модуль Сдвига
      Gxy=1.5D4
      CALL get_command_argument(7, arg)
      READ(arg,*)Gxy
C
      Gxy=Gxy/P0
      
C     Цилиндрическая жесткость DB   
      Dx=Ex*H**3/12.0/(1.0-PNUx*PNUy)
      Dy=Ey*H**3/12.0/(1.0-PNUx*PNUy)
      Dxy=2.0*Gxy*H**3/12.0+Dx*PNUy
      D=Dx
      Dx=Dx/D
      Dy=Dy/D
      Dxy=Dxy/D
      !DB=E*H**3/12.D0/(1.D0-PNU**2)
      DB=D
      
C Безразмерная плотность
      RO=K*RO/R
C
      
C     УГОЛ ПОТОКА
      CALL get_command_argument(8, arg)
      READ(arg,*)TJ1
      
      CALL FLUT34C
C
      CALL FLUT33D
C
      CALL FLUT16C
C
      CALL FLUT16D
C
      STOP
      END

C     FLUT34C SUBPROGRAMM
      SUBROUTINE FLUT34C
C
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*1 KEY,KEY1
      REAL*8 K
      EXTERNAL F
      COMMON /DATA/ K,H,BL,RO,DB,TJ1
      COMMON /COM1/ IPR,NUMB
      COMMON /COM2/ NLAMDA,TJ,M,N,NT
      COMMON /COM3/ V,IN
      COMMON /FL/ FLG(20)
      COMMON /ABH0H1/ AA,BB,H0,H1
      DIMENSION F1(50),F2(50),V19(2),IN19(2)
      DIMENSION AA(2500),BB(2500),H0(6250000),H1(6250000)

      NUMB = 1
      
C     ANGLE OF FLOW
      TJ2 = 180.
      TJ=TJ1/TJ2
 700  NOPAUSE=1
      
      KEY1 = 'A'
   10 FORMAT(A)
      IF(KEY1.EQ.'Q') RETURN
      IF(KEY1.EQ.'A') GO TO 25
      IF(KEY1.EQ.'Y') GO TO 100

      
      READ (*,*) M,N
      IF(M.GT.40.OR.N.GT.40) THEN

      STOP
      ENDIF
C
      NT=M*N
C
C
  150 NOPAUSE=1

      READ (*,*) NLAMDA
C
      READ (*,*) A,B
C
      CALL MATRH2 (M,N,NT,H0,AA,BB,H1,BL,NUMB)
C
      IF(F(A)*F(B).GT.0.) THEN

      GO TO 150
      ENDIF
      GO TO 200
  100 CONTINUE
C
      IPR=1
      M=9
      N=9
      NT=M*N
      NLAMDA=1

      READ (*,10) KEY
      IF(KEY.EQ.'Y') THEN

      READ (*,*) NLAMDA
      ENDIF
      V0=0.
      
      READ (*,10) KEY
      IF(KEY.EQ.'Y') THEN
      
      
      READ (*,*) V0
      ENDIF
      STEP=0.01
     
      READ (*,10) KEY
      IF(KEY.EQ.'Y') THEN

          
      READ (*,*) STEP
      ENDIF
      V=2.6
      V=15.D0

      READ (*,10) KEY
      IF(KEY.EQ.'Y') THEN
      READ (*,*) V
      ENDIF
      NSTEP=10
      
      READ (*,10) KEY
      IF(KEY.EQ.'Y') THEN
      
      
      READ (*,*) NSTEP
      ENDIF
      GO TO 310
   25 CONTINUE
      IPR=1
      M=9
      N=9
      NT=M*N
      NLAMDA=1
      V0=0.
      STEP=0.01
      V=2.6
      V=15.D0
      NSTEP=10
      
   26 FORMAT(8HNLAMDA= ,I3)
      
   27 FORMAT(4HV0= ,E9.2)
      
   28 FORMAT(6HSTEP= ,E9.2)
      
   29 FORMAT(4HV = ,E9.2)
      
   30 FORMAT(8HNSTEP = ,I3)
  310 NOPAUSE=1
      XX=V0
C
      CALL MATRH2 (M,N,NT,H0,AA,BB,H1,BL,NUMB)
C
      FF=F(XX)
      DO I=1,NLAMDA
      F1(I)=FLG(I)
      ENDDO
  400 XX=XX+STEP
      IF(XX.LE.V) THEN
      FF=F(XX)
      DO I=1,NLAMDA
      F2(I)=FLG(I)
      ENDDO
      ELSE
      GO TO 500
      ENDIF
      DO I=1,NLAMDA
      IF (F1(I)*F2(I).LT.0.) THEN
      IN=I
      GO TO 300
      ENDIF
      ENDDO
c     pause
      DO I=1,NLAMDA
      F1(I)=F2(I)
      ENDDO
      GO TO 400
  500 NOPAUSE=1!WRITE(*,*) 'End of search of the root'
      !WRITE (*,*) 'In the interval'
      !WRITE (*,526) V0,V
  526 FORMAT(E10.3,1H ,E10.3,1H )
      !WRITE (*,*)'Changes of signs at funct of stability are not rev'
      GO TO 700
  300 NOPAUSE=1!WRITE (*,15) IN
   15 FORMAT(5H IN =,I3)
      B=XX
      A=XX-STEP
      !WRITE (*,14) A,B
   14 FORMAT (4H A =,E10.3,4H B =,E10.3)
C     STOP
      NLAMD=NLAMDA
      NLAMDA=IN
C 200 EPS1=1.E-4
  200 EPS1=1.E-8
      ETA=0.0
      IFAIL=0
      IPR=2
      CALL C05ADF (A,B,EPS1,ETA,F,X,IFAIL)
      !WRITE (*,*) 'IFAIL = ',IFAIL
      IF (IFAIL.NE.0) THEN
      !WRITE(*,*) 'Mistake in the subprogramme of calc of the root'
      STOP
      ENDIF
      !WRITE (*,13) M,N
   13 NOPAUSE=1!FORMAT(18Hto the grid of M =, I3,4H N =, I3,' Is found value 
      !* critical speed')
      !WRITE (*,12) X
   12 FORMAT (1X,E15.8)
      IF(KEY1.NE.'Y'.AND.KEY1.NE.'A') THEN
      V=X
      IN=NLAMDA
      !WRITE (*,*) 'Result'
      !WRITE (*,70) V,IN
      RETURN
      ENDIF
      V9=X
      IN9=IN
      !WRITE(*,*) 'Grid 19x19; Beginning of search of the root.'
      M=19
      N=19
      NT=M*N
C
      CALL MATRH2 (M,N,NT,H0,AA,BB,H1,BL,NUMB)
C
      FF1=F(A)
      FF2=F(B)
      IF (FF1*FF2.GT.0.) THEN
      NLAMDA=NLAMD
      XX=A
      FF=F(XX)
      DO I=1,NLAMD
      F1(I)=FLG(I)
      ENDDO
  401 XX=XX+STEP
      IF(XX.LE.V) THEN
      FF=F(XX)
      DO I=1,NLAMD
      F2(I)=FLG(I)
      ENDDO
      ELSE
      GO TO 501
      ENDIF
      DO I=1,NLAMD
      IF (F1(I)*F2(I).LT.0.) THEN
      IN=I
      GO TO 301
      ENDIF
      ENDDO
      DO I=1,NLAMD
      F1(I)=F2(I)
      ENDDO
      GO TO 401
  501 CONTINUE
      NLAMDA=NLAMD
      XX=A
      FF=F(XX)
      DO I=1,NLAMD
      F1(I)=FLG(I)
      ENDDO
  402 XX=XX-STEP
C*1.07.97
      IF (XX.LE.0.D0) GO TO 502
C*1.07.97
      IF(XX.GT.VL) THEN
      FF=F(XX)
      DO I=1,NLAMD
      F2(I)=FLG(I)
      ENDDO
      ELSE
      GO TO 502
      ENDIF
      DO I=1,NLAMD
      IF (F1(I)*F2(I).LT.0.) THEN
      IN=I
      GO TO 302
      ENDIF
      ENDDO
c     pause
      DO I=1,NLAMD
      F1(I)=F2(I)
      ENDDO
      GO TO 402
C     STOP
  302 NOPAUSE=1!WRITE (*,15) IN
      A1=XX
      B1=XX+STEP
      IP=1
      GO TO 303
  301 NOPAUSE=1!WRITE (*,15) IN
      B1=XX
      A1=XX-STEP
      IP=2
      !WRITE (*,14) A1,B1
  303 NLAMDA=IN
      CALL C05ADF (A1,B1,EPS1,ETA,F,X,IFAIL)
      !WRITE (*,*) 'IFAIL = ',IFAIL
      IF (IFAIL.NE.0) THEN
      !WRITE(*,*) 'The mistake in the subprogramme of calc of the root'
      STOP
      ENDIF
      !WRITE (*,33) M,N
   33 FORMAT(18Hto the grid of M =, I3,4H N =, I3,' Is found value 
     * critical speed')

      !WRITE (*,12) X
      !WRITE (*,15) NLAMDA
      V19(IP)=X
      IN19(IP)=NLAMDA
      IF(IP.EQ.2) THEN
      VL=A-NSTEP*STEP
      !WRITE (*,34) A
   34 FORMAT(39Hpoisk the root to the left of the point,E10.3)
      GO TO 501
      ENDIF
      ELSE
      CALL C05ADF (A,B,EPS1,ETA,F,X,IFAIL)
      !WRITE (*,*) 'IFAIL = ',IFAIL
      IF (IFAIL.NE.0) THEN
      !WRITE(*,*) 'Mistake in the subprogramme of calc of the root'
      STOP
      ENDIF
      !WRITE (*,12) X
      !WRITE (*,15) NLAMDA
      V19(1)=X
      IN19(1)=NLAMDA
      V=X
      IN=NLAMDA
      NLAMDA=NLAMD
      !WRITE (*,*) 'Результат'
      WRITE (*,70) V,IN
      ENDIF
      !WRITE (*,*) ' '
      !WRITE (*,50) V9,IN9
      !WRITE (*,60) V19(1),IN19(1)
      GO TO 800
C     RETURN
  502 CONTINUE
      !WRITE (*,35) A
   35 FORMAT('Sleva from the point, critical speed is not found',E10.3,
     *'Critical speed is not found')
      !WRITE (*,50) V9,IN9
      !WRITE (*,60) V19(1),IN19(1)
      !WRITE (*,60) V19(2),IN19(2)
   50 FORMAT(5H V9= ,E13.6,6H IN9 =,I3)
   60 FORMAT(5H V19=,E13.6,6H IN19=,I3)
      V=V19(2)
      IN=IN19(2)
      IF(V19(1).GT.0.0.AND.V19(1).LT.V19(2)) THEN
      V=V19(1)
      IN=IN19(1)
      ENDIF
      !WRITE (*,*) ' '
      WRITE (*,*) 'Rezult'
      WRITE (*,70) V
      NLAMDA=NLAMD
   70 FORMAT(E13.6,';',I3,';') 
      GO TO 700
  800 RETURN
      END
      REAL*8 FUNCTION F(XX)
C 19.09.96
      IMPLICIT REAL*8 (A-H,O-Z)
      character*26 PATH 
      COMMON /COM1/ IPR,NUMB
      COMMON /COM2/ NLAMDA,TJ,M,N,NT
      COMMON /DATA/ K,H,BL,RO,DB
      COMMON /FL/ FLG(20)
      COMMON /ABH0H1/ A,B,H0,H1
      COMMON //PATH
C     PARAMETER (M=9,N=9,NT=M*N,BL=0.25)
C     DIMENSION A(N,N),B(M,M),DX(N,N),DY(M,M),H0(NT,NT),H1(NT,NT),
C    1X(NT),Y(NT),IANA(NT),IJ(NT)

      DIMENSION A(2500),B(2500),DX(2500),DY(2500),H0(6250000),
     1H1(6250000),X(2500),Y(2500),IANA(2500),IJ(2500),SCALE(2500)
C     COMPLEX*16 CZ,LAMDA(NLAMDA)
      REAL*8 K
      COMPLEX*16 CZ,LAMDA(40)
C
C     CALL MATRH1 (M,N,NT,H0,A,B,DX,DY,H1,TJ,BL,K,DB,XX,NUMB)
      CALL MATRH3 (M,N,NT,H0,DX,DY,H1,TJ,BL,K,DB,XX)
C Вычисление собственных значений матрицы H0
      NOUT = 4
      OPEN(UNIT=NOUT,FILE=TRIM(ADJUSTL(PATH))//'NOUT')
C
      CALL BALANC(NT,NT,H0,LOW,IGH,SCALE)
      CALL ELMHES (NT,NT,LOW,IGH,H0,IANA)
      CALL HQRM2(NT,NT,LOW,IGH,H0,X,Y,IERR,200)
      IF (IERR.NE.0) THEN
      WRITE(*,*) ' HQRM2 IERR = ', IERR
      STOP
      ENDIF
C 13  FORMAT (13I5)
  12  FORMAT (4E18.11)
      IL=0
C     NLAMDA=3
      DO 90 I=1,NT
   90 IJ(I)=1
C Вычисление минимального по модулю собственного значения
  100 IL=IL+1
      P=1.D25
      DO 33 I=1,NT
      IF(IJ(I).EQ.0) GO TO 33
      Q=SQRT(X(I)**2+Y(I)**2)
      IF(Y(I).LT.0.D0) GO TO 33
      IF(Q.LT.P) THEN
      P=Q
      II=I
      ENDIF
   33 CONTINUE
      CZ=CMPLX(X(II),Y(II))
      LAMDA(IL)=CZ
C Проверка условия устойчивости
      FL=REAL(CZ)*K**2-RO*H*DIMAG(CZ)**2
      FLG(IL)=FL
      IF(IL.LT.NLAMDA) THEN
      IJ(II)=0
      IF(Y(II).NE.0.D0) IJ(II+1)=0
      GO TO 100
      ENDIF
   52 FORMAT (9E8.1)
      F=FL
      !IF(IPR.EQ.2) WRITE (*,120) F,XX
  120 FORMAT (10H        F=,E12.4,10H        V=,E12.4)
      RETURN
      END
      SUBROUTINE FLUT33D
C 22.03.97 Тетрадь Флаттер N 4, стр. 69
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     PARAMETER (M=19,N=19,NT=M*N,BL=0.25)
C     PARAMETER (M=19,N=19,NT=M*N,MMI=NT,MI=2,BL=0.5)
C     DIMENSION A(N,N),B(M,M),DX(N,N),DY(M,M),H0(NT,NT),H1(NT,NT),
C    1X(NT),Y(NT),INT(NT),IJ(NT),URR(NT),UI(NT),ER(M),YY(N),Z(N),
C    2Y2(21),SCALE(NT)
C     DIMENSION  ZZ(NT,NT),RM1(NT,NT),
C    *RV1(NT),RV2(NT)
      DIMENSION A(6250000),B(2500),DX(2500),DY(2500),H0(6250000),
     1H1(6250000),X(2500),Y(2500),INT(2500),IJ(2500),URR(2500),UI(2500),
     2ER(50),YY(50),Z(50),Y2(51),SCALE(2500),ZZ(6250000),RM1(6250000),
     3RV1(2500),RV2(2500),XGRID(50),YGRID(50)
      DATA PI/3.141592653589D0/
      LOGICAL SELECT(1600)
      REAL*8 K
      COMPLEX*16 CZ,LAMDA(40)
      character*26 PATH 
      CHARACTER*1 KEY,KEY1
      EQUIVALENCE (H1(1),ZZ(1)),(A(1),RM1(1))
      COMMON /COM1/ IPR,NUMB
      COMMON /DATA/ K,H,BL,RO,DB
      COMMON /COM2/ NLAMDA,TJ,M,N,NT
      COMMON /COM3/ V,IN
      COMMON //PATH      
C
      !PAUSE 
      !STOP 
      !WRITE (*,*) 'Automatic or manual finding of eigen 
      !*vector (Y/N/Q)' 
      !READ (*,10) KEY1 
      !10 FORMAT(A) 
      !IF(KEY1.EQ.'Q') RETURN 
      GO TO 110
      !IF(KEY1.EQ.'Y')  
      WRITE (*,*) ' '
      WRITE (*,*) 'M = ?, N = ?'
      READ (*,*) M,N
      IF(M.GT.50.OR.N.GT.50) THEN
      WRITE (*,*) 'M или N больше 50. Stop'
      STOP
      ENDIF
C
      NT=M*N
      MMI=NT
      MI=2
C
      WRITE (*,*) ' '
      WRITE (*,*) 'TJ =? (numerator denominator)'
      READ (*,*) TJ1,TJ2
      TJ=TJ1/TJ2
C
      WRITE (*,*) ' '
      WRITE (*,*) 'NLAMDA = ? '
C
      READ (*,*) NLAMDA
      IF(NLAMDA.GT.20) THEN
      WRITE (*,*) 'M or N more than 20. Stop'
      STOP
      ENDIF
      WRITE(*,*) 'Enter the velocity vector'
      READ(*,*) V
  110 CONTINUE
      CALL MATRH1 (M,N,NT,H0,A,B,DX,DY,H1,TJ,BL,K,DB,V,NUMB)
C     CALL MATRH3 (M,N,NT,H0,DX,DY,H1,TJ,BL,K,DB,XX)
C     CALL MATRH3 (M,N,NT,H0,DX,DY,H1,TJ,BL,K,DB,V)
C Вычисление собственных значений матрицы H0
      MMI=NT
      MI=2
      NOUT = 4
      OPEN(UNIT=NOUT,FILE=TRIM(ADJUSTL(PATH))//'NOUT')
      !WRITE (*,75) V,IN
      WRITE (NOUT,75) V,IN
   75 FORMAT(5H V = ,E13.6,5H IN =,I3)
      OPEN (8,FILE=TRIM(ADJUSTL(PATH))//'MATRA')
      DO 170 I=1,NT
  170 SELECT(I)=.FALSE.
      CALL BALANC(NT,NT,H0,LOW,IGH,SCALE)
      CALL ELMHES(NT,NT,LOW,IGH,H0,INT)
      WRITE(8,12) (H0(I),I=1,NT*NT)
      ENDFILE 8
C     CALL HQRM(NT,NT,LOW,IGH,H0,X,Y,IERR)
      CALL HQRM2(NT,NT,1,NT,H0,X,Y,IERR,200)
C     CALL HQR (NT,NT,LOW,IGH,H0,X,Y,IERR)
      !WRITE(*,*) 'IERR = ',IERR
  12  FORMAT (4E18.11)
      WRITE(NOUT,*) 'Real part of eigenvalues'
      WRITE(NOUT,12) (X(I),I=1,NT)
C     WRITE(*,*) 'Real part of eigenvalues'
C     WRITE(*,12) (X(I),I=1,NT)
      WRITE(NOUT,*) 'Imaginary part of eigenvalues'
      WRITE(NOUT,12) (Y(I),I=1,NT)
C     WRITE(*,*) 'Imaginary part of eigenvalues'
C     WRITE(*,12) (Y(I),I=1,NT)
      IS=0
      DO I=1,NT
      IF (Y(I).GT.0.) IS=IS+1
      ENDDO
      !WRITE(*,120) IS
  120 FORMAT(I3,'Complex couples')
      WRITE(NOUT,120) IS
      IL=0
C     NLAMDA=3
      DO 90 I=1,NT
   90 IJ(I)=1
C Вычисление минимального по модулю собственного значения
  100 IL=IL+1
      WRITE(NOUT,*) 'IL = ',IL
      !WRITE(*,*) 'IL = ',IL
C     WRITE(*,*) 'NLAMDA = ',NLAMDA
      P=1.D25
      DO 33 I=1,NT
      IF(IJ(I).EQ.0) GO TO 33
      Q=SQRT(X(I)**2+Y(I)**2)
      IF(Y(I).LT.0.D0) GO TO 33
      IF(Q.LT.P) THEN
      P=Q
      II=I
      ENDIF
   33 CONTINUE
      !WRITE(*,*) 'II = ',II
      CZ=CMPLX(X(II),Y(II))
C     WRITE (*,*) CZ
      LAMDA(IL)=CZ
      WRITE (NOUT,115) IL
  115 FORMAT(I3,' - ое eigenvalue')
      WRITE(NOUT,*) ' '
      WRITE(NOUT,12) CZ
      !WRITE (*,115) IL
      !WRITE(*,*) ' '
      !WRITE(*,12) CZ
C Проверка условия устойчивости
      FL=REAL(CZ)*K**2-RO*H*DIMAG(CZ)**2
      !WRITE (*,*) 'Check of stability condition'
      !WRITE(*,12) FL
      WRITE (NOUT,*) 'Check of stability condition'
      WRITE(NOUT,12) FL
C     PAUSE
      !WRITE(*,*) 'To calculate the eigenvector (Y/N)'
      !READ(*,15) KEY
      !15  FORMAT(A)
      SELECT(II)=.TRUE.
      !IF(KEY.EQ.'N') GO TO 400
      REWIND 8
      READ(8,12) (H0(I),I=1,NT*NT)
      CALL INVIT(NT,NT,H0,X,Y,SELECT,MMI,MI,ZZ,IERR,RM1,RV1,RV2)
      !WRITE(*,*) 'IERR = ',IERR
      IF (IERR.NE.0) THEN
      WRITE(*,*) 'Mistake in the subprogramme of
     * calculation of the eigenvector'
      !WRITE(*,*) ' IERR = ', IERR
      STOP
      ENDIF
      REWIND 8
      READ(8,12) (H0(I),I=1,NT*NT)
      CALL ELMBAK(NT,LOW,IGH,H0,INT,MI,ZZ)
      CALL BALBAK(NT,NT,LOW,IGH,SCALE,MI,H0)
      M11=2*M
      DO 22 I=1,NT                                                      ZAP03500
      URR(I)=ZZ(I)
  22  UI(I)= ZZ(NT+I)
      OPEN (9,FILE=TRIM(ADJUSTL(PATH))//'FDATA1')
      NREZ1=18
      OPEN(UNIT=NREZ1,FILE=TRIM(ADJUSTL(PATH))//'REZULT.txt')
         DO  I=1,M
         T=(2.D0*I-1.D0)*PI/2.D0/M
         XGRID(I)=COS(T)
         ENDDO
         DO  I=1,N
         T=(2.D0*I-1.D0)*PI/2.D0/N
         YGRID(I)=BL*COS(T) 
         ENDDO     
      IJ=0
      DO I=1,M
      DO J=1,N
      IJ=IJ+1
      WRITE (NREZ1,150) XGRID(I),YGRID(J),URR(IJ)
      ENDDO
      ENDDO
      !WRITE (*,*) 'Write Maple'
150   FORMAT(1X,F9.5,F9.5,F9.5)                 
      WRITE (NOUT,*) 'Eigenvector RE'
      WRITE (NOUT,12) (URR(I),I=1,NT)
      WRITE (9,12) (URR(I),I=1,NT)
      WRITE (NOUT,*) 'Eigenvector IM'
      WRITE (NOUT,12) (UI(I),I=1,NT)
C Обязательно M нечетное
      DO 40 I=1,N
40    YY(I)=URR(N*(((M-1)/2))+I)
      DO 20 LL=1,21
      X2=-1.+0.1*(LL-1)
20    Y2(LL)=EIGEN(X2,YY,Z,N,-1.D0,+1.D0)
      CALL NORM (Y2,21)
      WRITE(NOUT,*) 'Eigenvector Re on the axis x in 21 points'
      WRITE(NOUT,12) Y2
      PRINT 12,Y2
      IF(DIMAG(CZ).EQ.0.D0) GO TO 200
      DO 44 I=1,N
44    YY(I)=UI(N*(((M-1)/2))+I)
      DO 21 LL=1,21
      X2=-1.+0.1*(LL-1)
21    Y2(LL)=EIGEN(X2,YY,Z,N,-1.D0,+1.D0)
      CALL NORM (Y2,21)
      WRITE(NOUT,*) 'Eigenvector Im on the axis x in 21 points'
      WRITE(NOUT,12) Y2
      PRINT 12,Y2
  200 CONTINUE
      !WRITE (*,*) 'Check of symmetry of the eigenvector'
      WRITE (NOUT,*) 'Check of symmetry of the eigenvector'
      NM=(M-1)/2
      DO 50 I=1,NM
      I1=(I-1)*N
      I2=(M-I)*N
      P=0.D0
      DO 51 J=1,N
      Q=URR(I1+J)-URR(I2+J)
      IF(ABS(Q).GT.P) P=ABS(Q)
   51 CONTINUE
   50 ER(I)=P
      !WRITE(*,52) (ER(I),I=1,NM)
      WRITE(NOUT,52) (ER(I),I=1,NM)
      IF(DIMAG(CZ).EQ.0.D0) GO TO 300
      DO 60 I=1,NM
      I1=(I-1)*N
      I2=(M-I)*N
      P=0.D0
      DO 61 J=1,N
      Q=UI(I1+J)-UI(I2+J)
      IF(ABS(Q).GT.P) P=ABS(Q)
   61 CONTINUE
   60 ER(I)=P
      !WRITE(*,52) (ER(I),I=1,NM)
      WRITE(NOUT,52) (ER(I),I=1,NM)
  300 CONTINUE
  400 CONTINUE
      IF(IL.LT.NLAMDA) THEN
      IJ(II)=0
      IF(Y(II).NE.0.D0) IJ(II+1)=0
      GO TO 100
      ENDIF
      WRITE (NOUT,*) 'LAMDA'
      !WRITE (*,*) 'LAMDA'
      WRITE (NOUT,70) (LAMDA(I),I=1,NLAMDA)
      !WRITE (*,70) (LAMDA(I),I=1,NLAMDA)
   70 FORMAT(1X,2E18.11)
      RETURN
   52 FORMAT (9E8.1)
      END
      SUBROUTINE FLUT16C
C 23.09.96 Флаттер тетрадь N2,стр. 129
      IMPLICIT REAL*8 (A-H,O-Z)
C     PARAMETER (M=19,N=19,NT=M*N,NG=101)
      DIMENSION U(2500),Y(2500),Z(2500),Y2(101),X2(101)
      REAL*8 K
      character*26 PATH
      COMMON /DATA/ K,H,BL,RO,DB
      COMMON /COM2/ NLAMDA,TJ,M,N,NT
      COMMON //PATH
      PI=3.14159265359D0
      OPEN (9,FILE=TRIM(ADJUSTL(PATH))//'FDATA1')
      REWIND 9
      NG=101
      READ(9,12) (U(I),I=1,NT)
C     READ(4,12) U
C 12  FORMAT (1X,4E18.11)
  12  FORMAT (4E18.11)
C     WRITE(*,12) U(1),U(NT)
C
      DO 40 I=1,N
40    Y(I)=U(N*(((M-1)/2))+I)
      DO 20 L=1,NG
      X=-1.+0.02*(L-1)
      X2(L)=X
  20  Y2(L)=EIGEN (X,Y,Z,N,-1.D0,+1.D0)
      CALL NORM1(Y2,NG)
      OPEN(5,FILE=TRIM(ADJUSTL(PATH))//'RDATA1')
      WRITE (5,13) X2
      WRITE (5,13) Y2
  13  FORMAT(1X,4E19.11)
C     WRITE(*,12) U(1),U(NT)
      I1=(N+1)/2
      DO 41 I=1,M
      Y(I)=U(I1)
  41  I1=I1+N
      STEP=2.*BL/(NG-1)
      DO 21 L=1,NG
      X=-BL+STEP*(L-1)
      X2(L)=X
  21  Y2(L)=EIGEN (X,Y,Z,M,-BL,BL)
      CALL NORM1(Y2,NG)
      WRITE (5,13) X2
      WRITE (5,13) Y2
      RETURN
      END                                                               ZAP01380
      SUBROUTINE NORM1(Y,N)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION Y(1)
      P=0.D0
      DO 1 I=1,N
      IF (ABS(Y(I)).GT.P) IP=I
      IF (ABS(Y(I)).GT.P) P=ABS(Y(I))
  1   CONTINUE
      P=Y(IP)
      DO 2 I=1,N
  2   Y(I)=Y(I)/P
      RETURN
      END
      SUBROUTINE FLUT16D
C 28.02.97 Флаттер тетрадь N4,стр. 47
      IMPLICIT REAL*8 (A-H,O-Z)
C     PARAMETER (M=19,N=19,NT=M*N,NG=101)
C     DIMENSION U(NT),Y(NT),Z(NT),Y2(NG),X2(NG),W2(NG,NG),Z2(M,NG)
      DIMENSION U(2500),Y(2500),Z(2500),X2(101),W2(101,101),
     *Z2(4040)
      REAL*8 K
      character*26 PATH
      COMMON /DATA/ K,H,BL,RO,DB
      COMMON /COM2/ NLAMDA,TJ,M,N,NT
      COMMON //PATH
      NG=101
      PI=3.14159265359D0
      OPEN (9,FILE=TRIM(ADJUSTL(PATH))//'FDATA1')
      REWIND 9
      READ(9,12) (U(I),I=1,NT)
C     READ(4,12) U
C 12  FORMAT (1X,4E18.11)
  12  FORMAT (4E18.11)
C     WRITE(*,12) U(1),U(NT)
C
      DO 50 J=1,M
      DO 40 I=1,N
40    Y(I)=U(N*(J-1)+I)
      DO 20 L=1,NG
      X=-1.+0.02*(L-1)
      X2(L)=X
C 20  Z2(J,L)=EIGEN (X,Y,Z,N,-1.D0,+1.D0)
  20  Z2((L-1)*M+J)=EIGEN (X,Y,Z,N,-1.D0,+1.D0)
  50  CONTINUE
C
      OPEN(7,FILE=TRIM(ADJUSTL(PATH))//'RDATA2')
      WRITE (7,13) X2
  13  FORMAT(1X,4E19.11)
C     WRITE(*,12) U(1),U(NT)
C
      DO 60 J=1,NG
      DO 41 I=1,M
C 41  Y(I)=Z2(I,J)
  41  Y(I)=Z2((J-1)*M+I)
C
      STEP=2.*BL/(NG-1)
      DO 21 L=1,NG
      X=-BL+STEP*(L-1)
      X2(L)=X
C 21  W2(J,L)=EIGEN (X,Y,Z,M,-BL,BL)
  21  W2(L,J)=EIGEN (X,Y,Z,M,-BL,BL)
  60  CONTINUE
C
      CALL NORM1(W2,NG*NG)
C
      WRITE (7,13) X2
      WRITE (7,13) W2
C
      RETURN
      END                                                               
      SUBROUTINE MATRH2 (M,N,NT,H0,A,B,H1,BL,NUMB)
C  Вычисление матрицы для свободно опертой и защемленной прямоугольной пластины
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(N,N),B(M,M),H0(NT,NT),H1(NT,NT)
      IF (NUMB.NE.1.AND.NUMB.NE.2) THEN
      WRITE (*,*) 'Wrong number of the boundary-value problem. STOP.'
      STOP
      ENDIF
      IF (NUMB.EQ.2) THEN
      !CALL BOXBIG (H1,N,M,NT,BL)
      CALL SCSC_D0D4(H1,N,M,NT,BL)
      GO TO 110
      ENDIF
C     Построение матрицы H0
      CALL ZSL1 (A,N,-1.D0,+1.D0)
      CALL ZSL1 (B,M,-BL,BL)
      DO 1 I=1,NT
      DO 1 J=1,NT
    1 H0(I,J)=0.D0
      DO 2 I=1,M
      DO 2 J=1,M
      DO 2 NU=1,N
      I1=(I-1)*N+NU
      DO 2 MU=1,N
      J1=(J-1)*N+MU
      IF(I.EQ.J) THEN
      H0(I1,J1)=A(NU,MU)
      IF(NU.EQ.MU) H0(I1,J1)=H0(I1,J1)+B(I,I)
      ENDIF
      IF(I.NE.J) THEN
      IF(NU.EQ.MU) H0(I1,J1)=B(I,J)
      ENDIF
    2 CONTINUE
C Вычисление квадрата матрицы H0
      DO 3 I=1,NT
      DO 3 J=1,NT
      P=0.D0
      DO 4 L=1,NT
    4 P=P+H0(I,L)*H0(L,J)
    3 H1(I,J)=P
C
  110 CONTINUE
      RETURN
      END
      SUBROUTINE MATRH3 (M,N,NT,H0,DX,DY,H1,TJ,BL,K,DB,V)
C Вычисление матрицы для свободно опертой и защемленной прямоугольной пластины
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION DX(N,N),DY(M,M),H0(NT,NT),H1(NT,NT)
      REAL*8 K
C
      PI=3.141592653589D0
C
      PSI=PI*TJ
      VX= V*COS(PSI)
      VY= V*SIN(PSI)
C
      CALL DIFX1(DX,N,-1.D0,1.D0)
      CALL DIFX1(DY,M,-BL,BL)
C
      DO 5 I=1,NT
      DO 5 J=1,NT
    5 H0(I,J)=0.D0
      DO 6 I=1,M
      DO 6 J=1,M
      DO 6 NU=1,N
      I1=(I-1)*N+NU
      DO 6 MU=1,N
      J1=(J-1)*N+MU
      IF(I.EQ.J) THEN
      H0(I1,J1)=VX*DX(NU,MU)
      IF(NU.EQ.MU) H0(I1,J1)=H0(I1,J1)+VY*DY(I,I)
      ENDIF
      IF(I.NE.J) THEN
      IF(NU.EQ.MU) H0(I1,J1)=VY*DY(I,J)
      ENDIF
    6 CONTINUE
      DO 7 I=1,NT
      DO 7 J=1,NT
    7 H0(I,J)=DB*H1(I,J)-K*H0(I,J)
      RETURN
      END
      SUBROUTINE MATRH1(M,N,NT,H0,A,B,DX,DY,H1,TJ,BL,K,DB,V,NUMB)
C Вычисление матрицы для свободно опертой и защемленной прямоугольной пластины
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(N,N),B(M,M),DX(N,N),DY(M,M),H0(NT,NT),H1(NT,NT)
      REAL*8 K
      IF (NUMB.NE.1.AND.NUMB.NE.2) THEN
      WRITE (*,*) 'ЌҐЇа ўЁ«м­л© ­®¬Ґа Єа Ґў®© § ¤ зЁ. STOP.'
      STOP
      ENDIF
      IF (NUMB.EQ.2) THEN
      !CALL BOXBIG (H1,N,M,NT,BL)
      CALL SCSC_D0D4(H1,N,M,NT,BL)
      GO TO 110
      ENDIF
C     Построение матрицы H0
      CALL ZSL1 (A,N,-1.D0,+1.D0)
      CALL ZSL1 (B,M,-BL,BL)
      DO 1 I=1,NT
      DO 1 J=1,NT
    1 H0(I,J)=0.D0
      DO 2 I=1,M
      DO 2 J=1,M
      DO 2 NU=1,N
      I1=(I-1)*N+NU
      DO 2 MU=1,N
      J1=(J-1)*N+MU
      IF(I.EQ.J) THEN
      H0(I1,J1)=A(NU,MU)
      IF(NU.EQ.MU) H0(I1,J1)=H0(I1,J1)+B(I,I)
      ENDIF
      IF(I.NE.J) THEN
      IF(NU.EQ.MU) H0(I1,J1)=B(I,J)
      ENDIF
    2 CONTINUE
C Вычисление квадрата матрицы H0
      DO 3 I=1,NT
      DO 3 J=1,NT
      P=0.D0
      DO 4 L=1,NT
    4 P=P+H0(I,L)*H0(L,J)
    3 H1(I,J)=P
C
  110 CONTINUE
C
      PI=3.141592653589D0
C
      PSI=PI*TJ
      VX= V*COS(PSI)
      VY= V*SIN(PSI)
C
      CALL DIFX1(DX,N,-1.D0,1.D0)
      CALL DIFX1(DY,M,-BL,BL)
C
      DO 5 I=1,NT
      DO 5 J=1,NT
    5 H0(I,J)=0.D0
      DO 6 I=1,M
      DO 6 J=1,M
      DO 6 NU=1,N
      I1=(I-1)*N+NU
      DO 6 MU=1,N
      J1=(J-1)*N+MU
      IF(I.EQ.J) THEN
      H0(I1,J1)=VX*DX(NU,MU)
      IF(NU.EQ.MU) H0(I1,J1)=H0(I1,J1)+VY*DY(I,I)
      ENDIF
      IF(I.NE.J) THEN
      IF(NU.EQ.MU) H0(I1,J1)=VY*DY(I,J)
      ENDIF
    6 CONTINUE
      DO 7 I=1,NT
      DO 7 J=1,NT
    7 H0(I,J)=DB*H1(I,J)-K*H0(I,J)
      RETURN
      END
      SUBROUTINE BOXBIG (H,N,M,NT,BL)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER P
      DIMENSION H(NT,NT),TP(50),TP1(50),TP2(50),TP3(50),TP4(50),
     *TK(50),TK1(50),TK2(50),TK3(50),TK4(50)
      IF (N.GT.50.OR.M.GT.50) THEN
      WRITE (*,*) 'M or N more than 50. Stop.'
      STOP
      ENDIF
C     Построение матрицы H
      PI=3.141592653589D0
      I1=0
!      WRITE (*,*) 'HELLO'
      DO 2 MU=1,M
      Z=COS((2.*MU-1.)*PI/2./M)
      Z2=Z*Z
      Z3=Z2*Z
      Z4=Z3*Z
      CALL DIFT (Z,M,TP,TP1,TP2,TP3,TP4)
      DO 2 NU=1,N
      X=COS((2.*NU-1.)*PI/2./N)
      X2=X*X
      X3=X2*X
      X4=X3*X
      I1=I1+1
      CALL DIFT (X,N,TK,TK1,TK2,TK3,TK4)
      I2=0
      DO 2 I=1,M
      TI=(2.*I-1.)*PI/2./M
      STI=SIN(TI)**4
      DO 2 J=1,N
      TJ=(2.*J-1.)*PI/2./N
      STJ=SIN(TJ)**4
      I2=I2+1
      SUM=0.D0
      DO 3 K=0,N-1
      TKJ=COS(K*TJ)
      IF (K.EQ.0) TKJ=0.5*TKJ
      D2DX2=(12.*X2-4.)*TK(K+1)+(8.*X3-8.*X)*TK1(K+1)+
     +(X4-2.*X2+1.)*TK2(K+1)
      D4DX4=24.*TK(K+1)+96.*X*TK1(K+1)+(72.*X2-
     -24.)*TK2(K+1)+(16.*X3-16.*X)*TK3(K+1)+
     +(X4-2.*X2+1.)*TK4(K+1)
      DO 3 P=0,M-1
      TPI=COS(P*TI)
      IF (P.EQ.0) TPI=0.5*TPI
      D2DZ2=(12.*Z2-4.)*TP(P+1)+(8.*Z3-8.*Z)*TP1(P+1)+
     +(Z4-2.*Z2+1.)*TP2(P+1)
      D4DZ4=24.*TP(P+1)+96.*Z*TP1(P+1)+(72.*Z2-
     -24.)*TP2(P+1)+(16.*Z3-16.*Z)*TP3(P+1)+
     +(Z4-2.*Z2+1.)*TP4(P+1)
      DELTA=D4DX4*(Z*Z-1.)**2*TP(P+1)+2.*D2DX2*D2DZ2/BL/BL+
     +(X*X-1.)**2*TK(K+1)*D4DZ4/BL**4
    3 SUM=SUM+TKJ*TPI*DELTA/STI/STJ
    2 H(I1,I2)=4.*SUM/M/N
      DO 5 I=1,NT
      DO 6 J=I+1,NT
C     IF(J.GE.NT) GO TO 5
      H(J,I)=(H(J,I)+H(I,J))/2.
    6 H(I,J)=H(J,I)
    5 CONTINUE
      RETURN
      END
      SUBROUTINE BOXBIG_ORT (H,N,M,NT,BL)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER P
      DIMENSION H(NT,NT),TP(50),TP1(50),TP2(50),TP3(50),TP4(50),
     *TK(50),TK1(50),TK2(50),TK3(50),TK4(50)
      COMMON /D/ Dx,Dxy,Dy
      IF (N.GT.50.OR.M.GT.50) THEN
      WRITE (*,*) 'M or N it is more 50. Stop.'
      STOP
      ENDIF
C     Построение марицы H
      WRITE(*,*) 'BL=', BL
      PAUSE
      PI=3.141592653589D0
      I1=0
      DO 2 MU=1,M
      Z=COS((2.*MU-1.)*PI/2./M)
      Z2=Z*Z
      Z3=Z2*Z
      Z4=Z3*Z
      CALL DIFT (Z,M,TP,TP1,TP2,TP3,TP4)
      DO 2 NU=1,N
      X=COS((2.*NU-1.)*PI/2./N)
      X2=X*X
      X3=X2*X
      X4=X3*X
      I1=I1+1
      CALL DIFT (X,N,TK,TK1,TK2,TK3,TK4)
      I2=0
      DO 2 I=1,M
      TI=(2.*I-1.)*PI/2./M
      STI=SIN(TI)**4
      DO 2 J=1,N
      TJ=(2.*J-1.)*PI/2./N
      STJ=SIN(TJ)**4
      I2=I2+1
      SUM=0.D0
      DO 3 K=0,N-1
      TKJ=COS(K*TJ)
      IF (K.EQ.0) TKJ=0.5*TKJ
      D2DX2=(12.*X2-4.)*TK(K+1)+(8.*X3-8.*X)*TK1(K+1)+
     +(X4-2.*X2+1.)*TK2(K+1)
      D4DX4=24.*TK(K+1)+96.*X*TK1(K+1)+(72.*X2-
     -24.)*TK2(K+1)+(16.*X3-16.*X)*TK3(K+1)+
     +(X4-2.*X2+1.)*TK4(K+1)
      DO 3 P=0,M-1
      TPI=COS(P*TI)
      IF (P.EQ.0) TPI=0.5*TPI
      D2DZ2=(12.*Z2-4.)*TP(P+1)+(8.*Z3-8.*Z)*TP1(P+1)+
     +(Z4-2.*Z2+1.)*TP2(P+1)
      D4DZ4=24.*TP(P+1)+96.*Z*TP1(P+1)+(72.*Z2-
     -24.)*TP2(P+1)+(16.*Z3-16.*Z)*TP3(P+1)+
     +(Z4-2.*Z2+1.)*TP4(P+1)
      DELTA=Dx*D4DX4*(Z*Z-1.)**2*TP(P+1)+2.0*Dxy*D2DX2*D2DZ2/BL/BL+
     +(X*X-1.)**2*TK(K+1)*Dy*D4DZ4/BL**4
    3 SUM=SUM+TKJ*TPI*DELTA/STI/STJ
    2 H(I1,I2)=4.*SUM/M/N
      DO 5 I=1,NT
      DO 6 J=I+1,NT
C     IF(J.GE.NT) GO TO 5
      H(J,I)=(H(J,I)+H(I,J))/2.
    6 H(I,J)=H(J,I)
    5 CONTINUE
      RETURN
      END
     
      SUBROUTINE SCSC_D0D4 (H,N,M,NT,B)
      IMPLICIT REAL*8 (A-H,O-Z)
      
      ! МАССИВЫ ПОЛИНОМОВ ЧЕБЫШЕВА И ИХ ПРОИЗВОДНЫХ
      REAL*8, DIMENSION(N):: T, T1, T2, T3, T4

      !ЗНАЧЕНИЯ M'',M', M || ДЛЯ ЧАСТИ РАСЧЕТА КОНСТАНТ (1) ДЛЯ Y=B, (2) ДЛЯ Y=-B
      REAL*8, DIMENSION(2):: M_D2, M_D1, M_D0

      !ЗНАЧЕНИЯ М''', M''''
      REAL*8 M_D3, M_D4

      !ЗНАЧЕНИЕ СУММЫ SM'' || ДЛЯ ЧАСТИ РАСЧЕТА КОНСТАНТ (1) ДЛЯ Y=B, (2) ДЛЯ Y=-B
      REAL*8, DIMENSION(2):: SM_VAL_D2

      !МАТРИЦЫ ПРОИЗВОДНЫХ ПО ИКС
      REAL*8, DIMENSION(N, N):: DX0, DX2, DX4
      
      !МАТРИЦЫ ПРОИЗВОДНЫХ ПО ИКРЕК
      REAL*8, DIMENSION(N, N):: DY0, DY1, DY2, DY3, DY4
      
      !ИТОГОВАЯ МАТРИЦА
      REAL*8, DIMENSION(N*N, N*N):: H

      !МАТРИЦА, ХРАНЯЩАЯ ЗНАЧЕНИЯ ФОРМУЛЫ ПЕРЕД СУММИРОВАНИЕМ ДЛЯ НАХОЖДЕНИЯ ЗНАЧЕНИЯ В ОДНОЙ ТОЧКЕ
      REAL*8 DELTA

      !ЗНАЧЕНИЕ МIO
      REAL*8 SM_VAL_D0

      !ПРОИЗВОДНЫЕ MIO
      REAL*8 SM_VAL_D1, SM_VAL_D3, SM_VAL_D4

      !НУМЕРАТОРЫ
      INTEGER I, J, K

      !СЧЕТЧИК ХОДА ПО УЗЛАМ
      INTEGER MXI


      ! значение х_i в узле сетки
      REAL*8 ZI

      !ЗНАЧЕНИЕ ИНТЕРОПЛИРУЕМОЙ ФУНКЦИИ
      REAL*8 F_VAL_D0, F_VAL_D1, F_VAL_D2, F_VAL_D3, F_VAL_D4

      !ЗНАЧЕНИЯ КОНСТАНТ c И d
      REAL*8 C_VAL, D_VAL

      !МАТРИЦА A_CD
      REAL*8, DIMENSION(2,2):: A_CD

      !ЗНАЧЕНИЕ ОПРЕДЕЛИТЕЛЯ МАТРИЦЫ A_CD
      REAL*8 DET_A_CD

      COMMON /D/ Dx,Dxy,Dy

      DATA PI/3.141592653589793238462643D0/

      !ИНТЕРПОЛИРУЕМАЯ ФУНКЦИЯ
      F_D0(Z) = SIN(PI*Z)
      F_D1(Z) = PI*COS(PI*Z)
      F_D2(Z) = -PI**2*SIN(PI*Z)
      F_D3(Z) = -PI**3*COS(PI*Z)
      F_D4(Z) = PI**4*SIN(PI*Z)

      !ВЫЧИСЛЕНИЕ ЗНАЧЕНИЙ M'', M', M В ТОЧКАХ B И -B
      CALL DIFT(1.D0,N,T,T1,T2,T3,T4)
      M_D2(1) = 2.D0*T(N) + 4.D0*B*T1(N)
      M_D1(1) = 2.D0*B*T(N)
      M_D0(1) = 0.D0
      CALL DIFT(-1.D0,N,T,T1,T2,T3,T4)
      M_D2(2) = 2.D0*T(N) - 4.D0*B*T1(N)
      M_D1(2) = -2.D0*B*T(N)
      M_D0(2) = 0.D0

      !ПОСТРОЕНИЕ МАТРИЦЫ A_CD
      A_CD(1,1) = 2*M_D1(1) + B*M_D2(1)
      A_CD(1,2) = M_D2(1)
      A_CD(2,1) = 2*M_D1(2) - B*M_D2(2)
      A_CD(2,2) = M_D2(2)

      !ВЫЧИСЛЕНИЕ ОПРЕДЕЛИТЕЛЯ МАТРИЦЫ A_CD
      DET_A_CD = A_CD(1,1)*A_CD(2,2) - A_CD(1,2)*A_CD(2,1)

      !ВЫЧИСЛЕНИЕ СУММ
      SM_VAL_D2(1) = 0.D0
      SM_VAL_D2(2) = 0.D0
      CALL DIFT(1.D0,N,T,T1,T2,T3,T4)
      DO I = 1, N
          ZI = COS((2*I-1)*PI/2.D0/N)
          DO K = 0, N-1
              IF (K.EQ.0) THEN
                  SM_VAL_D2(1) = SM_VAL_D2(1) + 1.D0/N *
     1            COS(K*(2.D0*I-1.D0)/2.D0/N*PI)*T(K+1) *
     2            2.D0/(ZI**2-B**2) + 1.D0/N *
     3            COS(K*(2.D0*I-1.D0)/2.D0/N*PI)*T1(K+1) * 
     4            4.D0*B/(ZI**2-B**2)
              ELSE
                  SM_VAL_D2(1) = SM_VAL_D2(1) + 2.D0/N *
     1            COS(K*(2.D0*I-1.D0)/2.D0/N*PI)*T(K+1) * 
     2            2.D0/(ZI**2-B**2) + 2.D0/N *
     3            COS(K*(2.D0*I-1.D0)/2.D0/N*PI)*T1(K+1) * 
     4            4.D0*B/(ZI**2-B**2)
              ENDIF

          ENDDO
      ENDDO
      CALL DIFT(-1.D0,N,T,T1,T2,T3,T4)
      DO I = 1, N
          ZI = COS((2*I-1)*PI/2.D0/N)
          DO K = 0, N-1
              IF (K.EQ.0) THEN
                  SM_VAL_D2(2) = SM_VAL_D2(2) + 1.D0/N *
     1            COS(K*(2.D0*I-1.D0)/2.D0/N*PI)*T(K+1) *
     2            2.D0/(ZI**2-B**2) - 1.D0/N *
     3            COS(K*(2.D0*I-1.D0)/2.D0/N*PI)*T1(K+1) *
     4             4.D0*B/(ZI**2-B**2)
              ELSE
                  SM_VAL_D2(2) = SM_VAL_D2(2) + 2.D0/N *
     1            COS(K*(2.D0*I-1.D0)/2.D0/N*PI)*T(K+1) *
     2            2.D0/(ZI**2-B**2) - 2.D0/N *
     3            COS(K*(2.D0*I-1.D0)/2.D0/N*PI)*T1(K+1) *
     4            4.D0*B/(ZI**2-B**2)
              ENDIF
          ENDDO
      ENDDO

      !ВЫЧИСЛЕНИЕ c И d
      C_VAL = 1.D0/DET_A_CD*(-M_D2(2)*SM_VAL_D2(1)+M_D2(1)*SM_VAL_D2(2))
      D_VAL = 1.D0/DET_A_CD*((M_D1(2)- B*M_D2(2)+M_D1(2))*SM_VAL_D2(1)-
     1(M_D1(1)+B*M_D2(1)+M_D1(1))*SM_VAL_D2(2))

      !
      CALL D2D4(DX0,DX2,DX4,N)
      !

      I1=0
      DO MXI = 1, N
          Z = COS((2.*FLOAT(MXI)-1.)*PI/2.D0/FLOAT(N))
          CALL DIFT(Z,N,T,T1,T2,T3,T4)
          DO NU = 1, N !N
              I1=I1+1
              I2=0

              DO I = 1, N
                  ZI = COS((2.*I-1.)*PI/2.D0/N)
                  
                  TI=(2.*I-1.)*PI/2./N
                  
                  STI=SIN(TI)**4
                  
                  M_D0(1) = (Z**2-B**2)*T(N)
                  
                  M_D1(1) = 1.D0/B*(Z**2-B**2)*T1(N)
     1            + 2.D0*Z*T(N)
                  
                  M_D2(1) = 2.D0*T(N) + 4.D0*Z*T1(N) +
     1            1.D0/B**2*T2(N)*(Z**2-B**2)
                  
                  M_D3 = 1.D0/B**3*T3(N)*(Z**2-B**2) +
     1            6.D0*Z*1.D0/B**2*T2(N) +
     2            4.D0*1.D0/B*T1(N) + 2.D0*1.D0/B*T1(N)
                  
                  M_D4 = 1.D0/B**4*T4(N)*(Z**2-B**2) +
     1            6.D0/B**2*T2(N) + 6.D0*Z/B**3*T3(N) +
     2            4.D0/B**2*T2(N) + 2.D0/B**2*T2(N)

                  DO J = 1, N !N
                      SM_VAL_D0 = 0.D0
                      SM_VAL_D1 = 0.D0
                      SM_VAL_D2(1) = 0.D0
                      SM_VAL_D3 = 0.D0
                      SM_VAL_D4 = 0.D0
                      I2=I2+1

                      DO K = 0, N-1
                          IF(K.EQ.0) THEN
                              SM_VAL_D0 = SM_VAL_D0 + 1.D0/N *
     1                        COS(K*(2.D0*I-1.D0)/2.D0/N*PI)*T(K+1) *
     2                        (Z**2-B**2)/(ZI**2-B**2)

                              SM_VAL_D1 = SM_VAL_D1 + 1.D0/B*1.D0/N *
     1                        COS(K*(2.D0*I-1.D0)/2.D0/N*PI)*T(K+1) *
     2                        2.D0*Z/(ZI**2-B**2) + 1.D0/B*1.D0/N *
     4                        COS(K*(2.D0*I-1.D0)/2.D0/N*PI)*T1(K+1) *
     5                        (Z**2-B**2)/(ZI**2-B**2)

                              SM_VAL_D2(1) = SM_VAL_D2(1) + 1.D0/N *
     1                        COS(K*(2.D0*I-1.D0)/2.D0/N*PI)*T(K+1)*
     2                        2.D0/(ZI**2-B**2) + 1.D0/N *
     3                        COS(K*(2.D0*I-1.D0)/2.D0/N*PI) *
     4                        T1(K+1)*4.D0*Z/(ZI**2-B**2) +
     5                        1.D0/B*2.D0/N *
     6                        COS(K*(2.D0*I-1.D0)/2.D0/N*PI)*T2(K+1) *
     7                        (Z**2-B**2)/(ZI**2-B**2)

                              SM_VAL_D3 = SM_VAL_D3 + 1.D0/N *
     1                        COS(K*(2.D0*I-1.D0)/2.D0/N*PI) * (
     2                        6.D0/(ZI**2-1.D0)/B**2*T1(K+1) +
     3                        1.D0/B**2*4.D0*Z/(ZI**2-1.D0)*T2(K+1) +
     4                        2.D0*Z/(ZI**2-1.D0)*T2(K+1) +
     5                        1.D0/B*(Z**2-1.0)/(ZI**2-1.D0) * T3(K+1))

                              SM_VAL_D4 = SM_VAL_D4 + 1.D0/N *
     1                        COS(K*(2.D0*I-1.D0)/2.D0/N*PI) * (
     2                        10.D0/(ZI**2-1.D0)/B**3*T2(K+1) +
     3                        1.D0/B**3*4.D0*Z/(ZI**2-1.D0)*T3(K+1) +
     4                        2.D0/(ZI**2-1.D0)*T2(K+1) +
     5                        4.D0*Z/B/(ZI**2-1.D0)*T3(K+1) +
     6                        1.D0/B**2*(Z**2-1.0)/(ZI**2-1.D0) *
     7                        T4(K+1))

                          ELSE

                              SM_VAL_D0 = SM_VAL_D0 + 2.D0/N *
     1                        COS(K*(2.D0*I-1.D0)/2.D0/N*PI)*T(K+1) *
     2                        (Z**2-B**2)/(ZI**2-B**2)

                              SM_VAL_D1 = SM_VAL_D1 + 1.D0/B*2.D0/N *
     1                        COS(K*(2.D0*I-1.D0)/2.D0/N*PI)*T(K+1) *
     2                        2.D0*Z/(ZI**2-B**2) + 1.D0/B*2.D0/N *
     3                        COS(K*(2.D0*I-1.D0)/2.D0/N*PI)*T1(K+1) *
     4                        (Z**2-B**2)/(ZI**2-B**2)

                              SM_VAL_D2(1) = SM_VAL_D2(1) + 2.D0/N *
     1                        COS(K*(2.D0*I-1.D0)/2.D0/N*PI)*T(K+1)*
     2                        2.D0/(ZI**2-B**2) + 2.D0/N *
     3                        COS(K*(2.D0*I-1.D0)/2.D0/N*PI)*T1(K+1) *
     4                        4.D0*Z/(ZI**2-B**2) + 1.D0/B*2.D0/N *
     5                        COS(K*(2.D0*I-1.D0)/2.D0/N*PI)*T2(K+1) *
     6                        (Z**2-B**2)/(ZI**2-B**2)

                              SM_VAL_D3 = SM_VAL_D3 + 2.D0/N *
     1                        COS(K*(2.D0*I-1.D0)/2.D0/N*PI) * (
     2                        6.D0/(ZI**2-1.D0)/B**2*T1(K+1) +
     3                        1.D0/B**2*4.D0*Z/(ZI**2-1.D0)*T2(K+1) +
     4                        2.D0*Z/(ZI**2-1.D0)*T2(K+1) +
     5                        1.D0/B*(Z**2-1.0)/(ZI**2-1.D0)*T3(K+1))

                              SM_VAL_D4 = SM_VAL_D4 + 2.D0/N *
     1                        COS(K*(2.D0*I-1.D0)/2.D0/N*PI) * (
     2                        10.D0/(ZI**2-1.D0)/B**3*T2(K+1) +
     3                        1.D0/B**3*4.D0*Z/(ZI**2-1.D0)*T3(K+1) +
     4                        2.D0/(ZI**2-1.D0)*T2(K+1) +
     5                        4.D0*Z/B/(ZI**2-1.D0)*T3(K+1) +
     6                        1.D0/B**2*(Z**2-1.0)/(ZI**2-1.D0) *
     7                        T4(K+1))

                          ENDIF

                      ENDDO

                      DY0(MXI,I) =
     1                SM_VAL_D0 + C_VAL*M_D0(1)*Z + D_VAL*M_D0(1)
                      SM_VAL_D0 = 0.D0

                      DY1(MXI,I) =
     1                1.D0/B*SM_VAL_D1+C_VAL*M_D0(1)+C_VAL*Z
     1                *1.D0/B*M_D1(1)+D_VAL*1.D0/B*M_D1(1)
                      SM_VAL_D1 = 0.D0

                      DY2(MXI,I) =
     1                1.D0/B**2*SM_VAL_D2(1) + 1.D0/B*C_VAL*M_D1(1) +
     2                1.D0/B**2*C_VAL*Z*M_D2(1) + C_VAL*M_D1(1)+
     3                1.D0/B**2*D_VAL*M_D2(1)
                      SM_VAL_D2(1) = 0.D0

                      DY3(MXI,I) =
     1                1.D0/B**3*SM_VAL_D3 + 2.D0/B**2*C_VAL*M_D2(1) +
     2                1.D0/B**3*C_VAL*Z*M_D3 + 1.D0/B*C_VAL*M_D2(1) +
     3                1.D0/B**3*D_VAL*M_D3
                      SM_VAL_D3 = 0.D0

                      DY4(MXI,I) =
     1                1.D0/B**4*SM_VAL_D4 + 3.D0/B**3*C_VAL*M_D3 +
     2                1.D0/B**4*Z*C_VAL*M_D4 + 1.D0/B**2*C_VAL*M_D3 +
     3                1.D0/B**4*D_VAL*M_D4
                      SM_VAL_D4 = 0.D0

                      DELTA = 
	1                Dx*DX4(NU,J)*DX0(MXI,I)+Dxy*2.*DX2(NU,J)*DY2(MXI,I)+
     2                Dy*DY0(NU,J)*DY4(MXI,I)

                      H(I1,I2) = 2./N * DELTA/STI

                  ENDDO
              ENDDO
          ENDDO
      ENDDO
      
      
      DO 5 I=1,N*N
          DO 6 J=I+1, N*N
C     IF(J.GE.NT) GO TO 5
                  H(J,I)=(H(J,I)+H(I,J))/2.
    6     H(I,J)=H(J,I)
    5 CONTINUE
      
      END
      !END SUBPROGRMM