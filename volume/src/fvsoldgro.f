      SUBROUTINE FVSOLDGRO(ISPC,VN,D,H,BBFV)
      use prgprm_mod
      use contrl_mod
      IMPLICIT NONE
C----------
C VOLUME $Id$
C----------
C   OLDGRO COMPUTES VOLUMES OF OLD GROWTH TREES WITH
C   (D GT 35  OR H GT 190).   32-foot log rule.
C
C   FROM DON DEMARS --- PNW, JUNEAU AK
C----------
      LOGICAL DEBUG
      INTEGER ISPC,I,K,N,NN,KBD,NNN,JJ,NM,ICK,KB
      REAL DIFICK,FK,SCFT,XINTT,CV4,FF,CL,TAPER,DXL,RXL,DD2MI
      REAL R,HEST,FV,BRATIO,BK,ST,NOL16,NOL32,SH(20),DIB(20),HMM(20)
      REAL VS(20),HH(20),DST(20),DSLO(2),DSHI(2),XLL(20),DS(20)
      REAL SCF(20),XINT(20),VN,D,H,BBFV
      REAL PROF1,PROF2
C-----------
C  SEE IF WE NEED TO DO SOME DEBUG.
C-----------
      CALL DBCHK (DEBUG,'OLDGRO',6,ICYC)
C
  111 CONTINUE
      NOL32=0
      NOL16=0
      DO 15 I=1,20
      VS(I)=0.0
      DS(I)=0.0
      DIB(I)=0.0
      SCF(I)=0.0
      XINT(I)=0.0
      XLL(I)=0.0
   15 CONTINUE
C
      ST=1.0
      IF(D.GT.36)ST=D/36.
      IF(NOL32.EQ.0.AND.NOL16.EQ.0)BK=BRATIO(ISPC,D,H)
      IF(NOL32.EQ.0.AND.NOL16.EQ.0)FV=.005454154*D**2*(H-4.5)*BK
      IF(NOL32.GT.0)NOL16=NOL32*2
      IF(NOL16.GT.0)HH(1)=ST+NOL16*16.3
      IF(NOL16.GT.0)H=0.0
      DO 2 I=1,2
      XLL(I)=1.-(2./3.)*(TOPD(ISPC)/D)
      IF(H.LT.0.01)HEST=HH(1)/XLL(I)
      IF(HEST.GT.0.01)BK=BRATIO(ISPC,D,HEST)
      DST(I)=(TOPD(ISPC)**2)/(BK*D**2)
      DSLO(I)=DST(I)-.00001
      DSHI(I)=DST(I)+.00001
      IF(H.GT.0.01)GO TO 428
      R=(HEST-HH(1))/(HEST-4.5)
C      CALL PROF1(R,D,HEST,DD2MI)
      DS(I)=PROF1(R,D,HEST)
      RXL=.9*R
C      CALL PROF1(RXL,D,HEST,DD2MI)
      DXL=PROF1(RXL,D,HEST)
      TAPER=(DS(I)-DXL)/(.1*R)
      GO TO 429
C
  428 CONTINUE
      HH(I)=H*XLL(I)
      R=(H-HH(I))/(H-4.5)
C      CALL PROF1(R,D,H,DD2MI)
      DS(I)=PROF1(R,D,H)
C----------
C      ESTIMATE TAPER NEAR R
C----------
      RXL=.9*R
C      CALL PROF1(RXL,D,H,DD2MI)
      DXL=PROF1(RXL,D,H)
      TAPER=(DS(I)-DXL)/(.1*R)
  429 CONTINUE
      DO 245 K=1,10
      IF(DS(I).GT.DSLO(I).AND.DS(I).LT.DSHI(I))GO TO 40
      RXL=R+(DST(I)-DS(I))/TAPER
      R=RXL
      IF(H.GT.0.01)GO TO 246
      HEST=(HH(I)-4.5*R)/(1.-R)
C      CALL PROF1(R,D,HEST,DD2MI)
      DS(I)=PROF1(R,D,HEST)
      GO TO 245
  246 CONTINUE
C      CALL PROF1(R,D,H,DD2MI)
      DS(I)=PROF1(R,D,H)
  245 CONTINUE
   40 CONTINUE
      IF(NOL16.GT.0.AND.H.LT.0.01)GO TO 27
      HH(I)=H-(R*(H-4.5))
   27 CONTINUE
      IF(H.LT.0.01)H=HEST
C----------
C     FIND NUMBER (NN) OF LOGS AND CHUNKS AND FIND CHUNK LENGTH
C     FOR LIMD(1) ONLY (I.E., DIB TOP=6.0 INCHES).
C----------
    2 CONTINUE
      DO 50 N=1,15
      HMM(1)=16.3*N+ST+.0001
      NN=N
      IF(HMM(1).GE.HH(1))GO TO 60
   50 CONTINUE
   60 CONTINUE
      KBD=NN-1
C----------
C     CALCULATE CHUNK LENGTH
C----------
      CL=HH(1)-16.3*KBD-ST
C----------
C     WE NOW KNOW (OR AT LEAST WE THINK WE KNOW) DBH, HEIGHT, DIAMETER
C       OUTSIDE BARK AT 6-INCH TOP, MERCH. HEIGHT TO 6-INCH TOP,
C       DIAMETER OUTSIDE BARK AT 4-INCH TOP, MERCH. HEIGHT TO A 4-INCH
C       TOP, NUMBER OF WHOLE LOGS IN THE TREE TO A 6-INCH TOP, AND
C       THE CHUNK LENGTH.
C
C     NEXT, FIND SCALING DIAMETERS AND VOLUMES FOR ALL LOG HEIGHTS TO
C       TIP OF TREE.  START WITH SCALING HEIGHT LIST.
C----------
      SH(1)=0.
      SH(2)=ST
      DO 70 KB=1,KBD
      SH(KB+2)=ST+KB*16.3
   70 CONTINUE
      SH(KBD+3)=HH(1)
      SH(KBD+4)=HH(2)
      SH(KBD+5)=H
      NNN=NN+3
      BK=BRATIO(ISPC,D,H)
      FV=.005454154*D**2*(H-4.5)*BK
      DO 78 JJ=1,NNN
      R=(H-SH(JJ))/(H-4.5)
c      CALL PROF1(R,D,H,DD2MI)
      DIB(JJ)=PROF1(R,D,H)
      IF(DEBUG)WRITE(JOSTND,800)R,D,H,DD2MI,BK
  800 FORMAT(' IN OLDGRO 800 FMT R,D,H,DD2MI,BK= ',5F10.4)
      DIB(JJ)=(DIB(JJ)*BK)**.5*D
C
C     INTEGRAL OF SPRUCE-HEMLOCK PROFILE EQUATION
C
      FF=.4*R**2.5+(-.0052554*H +.000034947*H**2+.104477*
     1H/D)*(.4*R**2.5-.25*R**4.)+(7.76807/D**2-.0000094852*H**2
     2-.011351*H/D)*(.4*R**2.5-(1./33.)*R**33)
      VS(JJ)=FF*FV
C
   78 CONTINUE
C----------
C     NEXT, GET LOG AND TREE VOLUMES
C----------
      CV4=VS(2)-VS(NNN)
C----------
C     PRINT OUT EVERYTHING
C----------
      XINTT=0.0
C----------
C     NOW ADD SCRIBNER FORMULA AND INTERNATIONAL 1/4-INCH SCALE.  START
C     WITH DIB(3) AND GO TO DIB(NNN-1).  DIB(NNN-1) IS 6.0 INCH TOP.
C----------
      SCFT=0.0
      NM=NNN-1
      DO 90 K=3,NM
C----------
C SCRIBNER 32 FOOT LOG RULE
C----------
      FK = FLOAT(K) * 0.5
      ICK = INT(FK)
      DIFICK = FK - FLOAT(ICK) + 0.0001
      IF(DIFICK .GT. 0.1 .AND. K .NE. NM) GO TO 1500
      IF(DIFICK .GT. 0.1 .AND. K .EQ. NM)
     & SCF(K) = CL / 32.6 * (1.58 * DIB(K)*DIB(K) - 4.0 * DIB(K)-8.)
      IF(DIFICK .LE. 0.1 .AND. K .NE. NM)
     & SCF(K) = (1.58 * DIB(K)*DIB(K) - 4. * DIB(K) - 8.)
      IF(DIFICK .LE. 0.1 .AND. K .EQ. NM)
     & SCF(K) = (CL + 16.3) / 32.6 *
     &           (1.58 * DIB(K)*DIB(K) - 4.0 * DIB(K) - 8.)
      GO TO 1600
 1500 CONTINUE
      SCF(K) = 0.0
 1600 CONTINUE
      SCFT = SCFT + SCF(K)
C----------
C INTERNATIONAL 1/4 INCH LOG RULE
C----------
      XINT(K)=(.796*DIB(K)**2-1.374*DIB(K)-1.23)
      IF(K.EQ.NM)GO TO 91
      GO TO 92
   91 CONTINUE
      IF(CL.GT.0.0.AND.CL.LT.4.075)XINT(K)=CL/4.075*(0.199*DIB(K)**2-0.6
     142*DIB(K))
      IF(CL.GE.4.075.AND.CL.LT.8.15)XINT(K)=CL/8.15*(0.398*DIB(K)**2-1.0
     186*DIB(K)-.271)
      IF(CL.GE.8.15.AND.CL.LT.12.225)XINT(K)=CL/12.225*(0.597*DIB(K)**2-
     11.330*DIB(K)-.715)
      IF(CL.GE.12.225.AND.CL.LT.16.3)XINT(K)=CL/16.3*(.796*DIB(K)**2-1.3
     175*DIB(K)-1.230)
   92 CONTINUE
      XINTT=XINTT+XINT(K)
   90 CONTINUE
      VN=CV4
      BBFV=SCFT
  900 RETURN
      END
C
C
      FUNCTION PROF1(R,D,H)
      IMPLICIT NONE
C----------
C  **PROF1--AK    DATE OF LAST REVISION:  02/14/08
C----------
C
C     SPRUCE-HEMLOCK PROFILE EQUATION.
C     CALLED FROM OLDGRO.  USED FOR DIAMETER CALCULATIONS.
      REAL DD2MI,H,D,R,PROF1
C----------
      PROF1=R**1.5 + (-.0052554*H +.000034947*H**2 + .104477
     1*H/D) * (R**1.5-R**3) +(7.76807/D**2 -.0000094852*H**2 -
     2.011351*H/D)*(R**1.5-R**32)
      IF(DD2MI .LT. 0.)DD2MI=0.
      RETURN


      END
