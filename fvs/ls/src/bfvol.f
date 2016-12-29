      SUBROUTINE BFVOL(ISPC,D,H,D2H,BBFV,TKILL,LCONE,BARK,VMAX,ITHT,
     1                 BTKFLG)
      IMPLICIT NONE
C----------
C  **BFVOL--LS     DATE OF LAST REVISION:   07/11/08
C----------
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'COEFFS.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'OUTCOM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'VOLSTD.F77'
C
C
COMMONS
C
C  ************** BOARD FOOT MERCHANTABILITY SPECIFICATIONS ********
C
C  BFVOL CALCULATES BOARD FOOT VOLUME OF ANY TREE LARGER THAN A MINIMUM
C  DBH SPECIFIED BY THE USER.  MINIMUM DBH CAN VARY BY SPECIES,
C  BUT CANNOT BE LESS THAN 2 INCHES.  MINIMUM MERCHANTABLE DBH IS
C  SET WITH THE BFVOLUME KEYWORD.  FOR METHB = 1-4 MERCHANTABLE
C  TOP DIAMETER CAN BE SET TO ANY VALUE BETWEEN 2 IN. AND MINIMUM DBH.
C  MINIMUM DBH AND TOP DIAMETER ARE ASSUMED TO BE MEASURED OUTSIDE
C  BARK--IF DIB IS DESIRED, ALLOW FOR DOUBLE BARK THICKNESS IN
C  SPECIFICATIONS.
C
C  ALL PARAMETERS IN THE KEMP EQUATION FORM CAN BE REPLACED BY THE
C  USER WITH THE BFVOLEQU KEYWORD.  IF A USER ENTERS THEIR OWN EQUATION
C  IT IS ASSUMED TO APPLY FROM THE STUMP TO THE MERCH TOP.
C
C  VOLUME LOSS DUE TO TOP DAMAGE (TKILL=.TRUE.) IS ESTIMATED WITH A
C  BEHRE HYPERBOLA TAPER MODEL, WITH PARAMETERS ESTIMATED FROM TOTAL
C  CUBIC FOOT VOLUME, HEIGHT AND DIAMETER.
C----------
C
      LOGICAL LCONE,TKILL,BTKFLG
      REAL VMAX,BARK,BBFV,D2H,H,D,TSIZE
      INTEGER ITHT,ISPC
C
C----------
C  INITIALIZE VOLUME ESTIMATE.
C----------
      BBFV=0.0
C----------
C  TRANSFER TO STATEMENT 300 TO PROCESS WESTERN SIERRA LOG RULES.
C  (METHB=5)
C----------
      IF(METHB(ISPC).EQ.5) GO TO 300
C----------
C  TRANSFER TO STATEMENT 100 TO PROCESS REGION 6 LOG RULES.
C  (METHB= 3 OR 4).
C----------
      IF(METHB(ISPC).EQ.3 .OR. METHB(ISPC).EQ.4) GO TO 100
C----------
C  ASSIGN TRANSITION SIZE.
C----------
      TSIZE=D
      IF(IBTRAN(ISPC).GT.0) TSIZE=D2H
C
      IF(TSIZE.GE.BTRAN(ISPC)) GO TO 20
C
      BBFV = BFVEQS(1,ISPC)
     &     + BFVEQS(2,ISPC)*D
     &     + BFVEQS(3,ISPC)*D*H
     &     + BFVEQS(4,ISPC)*D2H
     &     + BFVEQS(5,ISPC)*D**BFVEQS(6,ISPC)*H**BFVEQS(7,ISPC)
      GO TO 30
   20 CONTINUE
      BBFV = BFVEQL(1,ISPC)
     &     + BFVEQL(2,ISPC)*D
     &     + BFVEQL(3,ISPC)*D*H
     &     + BFVEQL(4,ISPC)*D2H
     &     + BFVEQL(5,ISPC)*D**BFVEQL(6,ISPC)*H**BFVEQL(7,ISPC)
C----------
C  THE FOLLOWING PIECE OF FOOLISHNESS (J.E.B.) IS REQUIRED
C  BY NATIONAL FOREST SCALING AND CRUISING RULES.  BE CAREFUL IF
C  YOU BUY A CORRAL POLE SALE BASED ON A SCRIBNER CRUISE VOLUME.
C----------
   30 CONTINUE
      IF(BBFV.LT.10.0 .AND. BBFV.NE.0.) BBFV=10.0
C----------
C  SET TOPKILL FLAG AND RETURN.
C----------
      BTKFLG = .TRUE.
      RETURN
C
C
C
C----------
C  METHB = 3 OR 4:  VOLUME COMPUTED USING ONE OF THE REGION 6 LOG
C  RULES.  MINIMUM DBH FOR LOG RULE IS 4 INCHES.
C  NOT APPLICABLE TO EASTERN VARIANTS, SO RETURN 0 VOLUME
C----------
  100 CONTINUE
      RETURN
C----------
C  WESTERN SIERRA LOG RULES.
C  NOT APPLICABLE TO EASTERN VARIANTS, SO RETURN 0 VOLUME
C----------
  300 CONTINUE
      RETURN
C
      END
