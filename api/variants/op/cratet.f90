      SUBROUTINE CRATET
      use findage_mod, only: findag
      use prgprm_mod
      use arrays_mod
      use plot_mod
      use coeffs_mod
      use contrl_mod
      use outcom_mod
      use htcal_mod
      use varcom_mod
      use organon_mod
      implicit none
!----------
!  **CRATET--OP   DATE OF LAST REVISION:  07/16/15
!----------
!  THIS SUBROUTINE IS CALLED PRIOR TO PROJECTION.  IT HAS THE
!  FOLLOWING FUNCTIONS:
!
!    1)  CALL **RCON** TO LOAD SITE DEPENDENT MODEL COEFFICIENTS.
!    2)  REGRESSION TO ESTIMATE COEFFICIENTS OF LOCAL HEIGHT-
!        DIAMETER RELATIONSHIP.
!    3)  DUB IN MISSING HEIGHTS.
!    4)  CALL **DENSE** TO COMPUTE STAND DENSITY.
!    5)  SCALE CROWN RATIOS AND CALL **CROWN** TO DUB IN ANY MISSING
!        VALUES.
!    6)  DEFINE DG BASED ON CALIBRATION CONTROL PARAMETERS AND
!        CALL **DGDRIV** TO CALIBRATE DIAMETER GROWTH EQUATIONS.
!    7)  DELETE DEAD TREES FROM INPUT TREE LIST AND REALIGN IND1
!        AND ISCT.
!    8)  PRINT A TABLE DESCRIBING CONTROL PARAMETERS AND INPUT
!        VARIABLES.
!----------
!
!----------
!  INTERNAL VARIABLES.
!
!      KNT2 -- USED TO STORE COUNTS FOR PRINTING IN CONTROL
!              SUMMARY TABLE.
!     SPCNT -- USED TO ACCUMULATE NUMBER OF TREES PER ACRE BY
!              SPECIES AND TREE CLASS FOR CALCULATION OF
!              INITIAL SPECIES-TREE CLASS COMPOSITION VECTOR.
!----------
      LOGICAL MISSCR,TKILL,DEBUG
      CHARACTER*4 UNDER
      INTEGER KNT(MAXSP),KNT2(MAXSP),I,J,II,ISPC,IPTR
      INTEGER JCR,IICR,IS,IM,I1,I2,I3,K1,K2,K3,K4,NH,JJ
      REAL SITAGE,SITHT,AGMAX,HTMAX,HTMAX2,D1,D2
      REAL SPCNT(MAXSP,3),AX,Q,SUMX,H,BX,XX,YY,XN,HS,D
!----------
!     ORGANON:
!    ACALIB(i,j) IS AN ARRAY IN THE ORGANON.F77 COMMON BLOCK THAT IS
!                USED TO STORE CALIBRATION VALUES READ IN VIA KEYWORDS
!                OR CALCULATED IN **PREPARE** AND PASSED TO **EXECUTE**
!    TMPCAL(i,j) IS A LOCAL VARIABLE USED TO STORE CALIBRATION VALUES RETURNED
!                FROM **PREPARE**. IF THE USER ENTERED CALIBRATION VALUES, THEN
!                KEEP THOSE; IF THE USER DID NOT ENTER CALIBRATION VALUES, USE
!                THE ONES FROM **PREPARE**.
!    NEWCAL(j)   AN ARRAY INDICATING THAT SOME NEW CALIBRATION VALUES CAME
!                FROM **PREPARE** WHICH NEED TO BE PRINTED IN THE
!                CALIBRATION STATISTICS TABLE
!----------
      CHARACTER*31 LABEL(11)
      INTEGER*4   ORGEDIT_STATUS ! STATUS VARIABLE FOR THE DLL
      INTEGER*4   IEVEN          ! ORGANON::INDS(4)
      INTEGER*4   IRAD
      INTEGER     NBIG6,NVALID,NLOAD,NUNLOAD,ORGFIA,IHFLAG
      INTEGER     NEWCAL(18),IHEAD,ITEM(2000),KG
      INTEGER     KNTOHT(MAXSP),KNTOCR(MAXSP)
      REAL*4      RADGRO(2000)   ! DON'T INCLUDE FOR NOW.
      REAL*4      GROWTH(2000)
      REAL*4      TMPCAL(3,18)
!----------
!     THE SPECIES ORDER IS VARIANT SPECIFIC, SEE BLKDATA FOR A LIST.
!----------
!  INITIALIZE INTERNAL VARIABLES:
!----------
      UNDER = '----'
!
      LABEL = (/ &
      '                   DOUGLAS FIR:', &
      '                     GRAND FIR:', &
      '               WESTERN HEMLOCK:', &
      '              WESTERN REDCEDAR:', &
      '                   PACIFIC YEW:', &
      ' WHITE ALDER / PACIFIC MADRONE:', &
      '                 BIGLEAF MAPLE:', &
      '  OREGON WHITE / CAL BLACK OAK:', &
      '                     RED ALDER:', &
      '               PACIFIC DOGWOOD:', &
      '                WILLOW SPECIES:'/)
!-----------
!  SEE IF WE NEED TO DO SOME DEBUG.
!-----------
      CALL DBCHK (DEBUG,'CRATET',6,ICYC)
!----------
!  ORGANON
!  IF NECESSARY, CALL PREPARE TO FILL IN THE MISSING VALUES
!  LORGPREP=.TRUE. IF DATA HAS ALREADY BEEN PREPARED/EDITED
!  IF THIS IS A BARE GROUND PLANT, THEN SKIP THIS SECTION
!----------
      DO I=1,18
         TMPCAL(1,I)  = 1.0
         TMPCAL(2,I)  = 1.0
         TMPCAL(3,I)  = 1.0
         NEWCAL(I)    = 0
      ENDDO
      IF( DEBUG ) THEN
        WRITE(JOSTND,124) ICYC,IMODTY,LORGPREP
  124   FORMAT(' PREPARE FROM CRATET, CYCLE=',I2,', IMODTY= ',I2, &
        ', LORGPREP= ',L7)
      END IF
      DO I=1,MAXSP
        KNTOHT(I) = 0
        KNTOCR(I) = 0
      ENDDO
      IF(LORGPREP .OR. ITRN.LE.0) GO TO 261
!----------
!     ORGANON
!----------
      IF(DEBUG)WRITE(JOSTND,*)' RVARS(1-5)= ',(RVARS(I),I=1,5)
      IF(DEBUG)WRITE(JOSTND,*)' INDS(1-10)= ',(INDS(I),I=1,10)
!----------
!  SET ORGANON TREE INDICATOR (0 = NO, 1 = YES).
!  VALID TREE:
!    SPECIES 3=GF, 16=DF, 18=RC 19=WH, 21=BM, 22=RA, 23=MA, 28=WO,
!           33=PY, 34=DG, 37=WI
!    HEIGHT HT > 4.5 FEET
!    DBH    DBH >= 0.1 INCH
!
! THE CALL TO PREPARE IS TO EDIT THE DATA, DUB MISSING VALUES, AND CALIBRATE THE
! ORGANON EQUATIONS TO THE INPUT DATA. WE WILL ONLY PASS VALID ORGANON TREES TO
! PREPARE FOR THIS PURPOSE.
!
! ALSO CHECK FOR MAJOR TREE SPECIES; IF NONE, SKIP CALL TO PREPARE AND
! USE FVS LOGIC.
!----------
      NVALID = 0
      NBIG6 = 0
      DO I=1,IREC1
!----------
! CHECK TO SEE IF TREE MEETS DIAMETER LOWER LIMITS
! TREES WITH MEASURED HEIGHTS NEED TO MEET HEIGHT LOWER LIMITS FOR CALL TO PREPARE
! IHFLAG = 1 IF TREE MEETS MEASURED HEIGHT REQUIREMENTS OR THE HEIGHT IS MISSING
!----------
        IHFLAG = 0
        IF((HT(I).EQ.0.0) .OR. (HT(I).GT.4.5)) THEN
          IHFLAG = 1
        ENDIF
        IF(DBH(I) .GE. 0.1 .AND. IHFLAG .EQ. 1)THEN
!----------
! CHECK TO SEE IF THIS IS A BIG6 SPECIES
!----------
          SELECT CASE (ISP(I))
          CASE(3,16)
            NBIG6 = NBIG6 + 1
          END SELECT
!----------
! SET VALID SPECIES FLAG, IORG(I).
!----------
          SELECT CASE (ISP(I))
          CASE(3,16,18,19,21,22,23,28,33,34,37)
            IORG(I) = 1
            NVALID = NVALID + 1
          CASE DEFAULT
            IORG(I) = 0
          END SELECT
        ELSE
          IORG(I) = 0
        ENDIF
      ENDDO
      IF(DEBUG)WRITE(JOSTND,*)' IN CRATET IREC1,NVALID,NBIG6= ', &
      IREC1,NVALID,NBIG6
!----------
! IF THERE ARE NO "BIG 6" TREES ORGANON WON'T RUN. FLAG ALL TREES AS
! NON-VALID ORGANON TREES AND SKIP CALL TO PREPARE.
! ALL TREES WILL GO INTO FVS DUBBING AND CALIBRATION LOGIC
!
! NOTE: EVEN TREES THAT GET DUBBED VALUES IN ORGANON WILL GO INTO THE
! FVS DUBBING AND CALIBATION LOGIC. ORGANON DUBBED VALUES WON'T GET
! OVERWRITTEN AND INCLUDING THOSE VALUES IN FVS CALIBRATION SHOULD BRING
! FVS AND ORGANON EQUATIONS MORE IN SYNC
!----------
      IF(NBIG6 .EQ. 0)THEN
        DO I=1,ITRN
        IORG(I) = 0
        END DO
        GO TO 261
      ENDIF
!-----------
!  FOR ALL LIVE TREE RECORDS, MOVE THE DATA FROM THE EXISTING FVS TREE
!  ARRAYS INTO THE FORMAT THAT ORGANON REQUIRES.
!  ASSIGNMENT OF FVS-VARIABLES TO ORGANON-VARIABLES
!  ORGANON VARIABLE = FVS VARIABLE
!-----------
      NLOAD = 0
      DO I = 1, IREC1
        TREENO(I) = I        ! FVS TREE NUMBER
        PTNO(I)= ITRE(I)     ! POINT NUMBER OF THE TREE RECORD
        DBH1(I)= DBH(I)      ! DBH
        IF(DBH1(I) .LT. 0.1) DBH1(I)=0.1
        HT1OR(I)= HT(I)      ! TOTAL HEIGHT AT BEGINNING OF PERIOD
        IF(HT(I).GT.0. .AND. HT1OR(I).LT.4.6) HT1OR(I)=4.6
        CR1(I)= REAL(ICR(I)) / 100.0 ! MEASURED/PREDICTED CROWN RATIO
        EXPAN1(I)= PROB(I)   ! EXPANSION FACTOR
        RADGRO(I)= DG(I)     ! INSIDE BARK RADIAL GROWTH
        USER(I)= ISPECL(I)   ! USER CODE AT BEGINNING OF PERIOD
        NLOAD = NLOAD + 1
        ITEM(NLOAD) = I
!----------
! SET FIA SPECIES CODE (ACTUAL FOR VALID ORGANON SPECIES, SURROGATE
! FOR NON-VALID SPECIES.
!----------
        CALL ORGSPC(ISP(I),ORGFIA)
        SPECIES(I) = ORGFIA
!
        IF(DEBUG)WRITE(JOSTND,*) &
        ' I,PTNO,SPECIES,DBH1,HT1OR,CR1,EXPAN1,USER,NLOAD,ITEM= ', &
        I,PTNO(I),SPECIES(I),DBH1(I),HT1OR(I),CR1(I),EXPAN1(I), &
        USER(I),NLOAD,ITEM(NLOAD)
      ENDDO
      IF(DEBUG)WRITE(JOSTND,*)' NVALID,NLOAD= ',NVALID,NLOAD
!----------
!  TOTAL STAND AGE (EVEN AGED ONLY)
!----------
      IEVEN = INDS(4)
      IF(INDS(4) .EQ. 1 ) THEN               !STAND IS EVEN-AGED
        STAGE = IAGE+IY(ICYC)-IY(1)
        BHAGE     = STAGE - 5                ! BREAST HEIGHT AGE
      ELSE
        STAGE     = 0
        BHAGE     = 0                        ! BREAST HEIGHT AGE
      ENDIF
!----------
!  CALL PREPARE
!----------
      CALL PREPARE(IMODTY, IPTINV, IREC1, STAGE, BHAGE, SPECIES, &
                USER, IEVEN, DBH1, HT1OR, CR1, EXPAN1, &
                RADGRO, RVARS, SERROR, TERROR, &
                SWARNING, TWARNING, IERROR, IRAD, &
                GROWTH, TMPCAL )
      LORGPREP = .TRUE.
!----------
!  IF DEBUGGING, REPORT ANY ERRORS TO THE OUTPUT FILE.
!----------
      IF( DEBUG .AND. IERROR .NE. 0 ) THEN
        WRITE(JOSTND,9125) ICYC, IERROR
 9125   FORMAT(' CRATET ORGANON ERROR CODE, CYCLE= ',I2,' IERROR= ',I2)
        DO I = 1, 9
          IF( SWARNING(I) .NE. 0 ) THEN
            WRITE(JOSTND,9127) ICYC, SWARNING(I)
 9127       FORMAT(' CRATET ORGANON ERROR CODE, CYCLE= ',I2, &
            ' SWARNING= ',I2 )
          END IF
        END DO
!
        DO I = 1, 35
          IF( SERROR(I) .NE. 0 ) THEN
!----------
!  IGNORE THE FOLLOWING ERRORS:
!     6 -- BHAGE HAS BEEN SET TO 0 FOR AN UNEVEN-AGED STAND
!     7 -- BHAGE > 0 FOR AN UNEVEN-AGED STAND
!     8 -- STAGE IS TOO SMALL FOR THE BHAGE
!     9 --
!    11 --
!----------
            IF( ( I .EQ. 6  ) .OR. &
                ( I .EQ. 7  ) .OR. &
                ( I .EQ. 8  ) .OR. &
                ( I .EQ. 9  ) .OR. &
                ( I .EQ. 11 ) ) THEN
              WRITE(JOSTND,9136) ICYC, I, SERROR(I)
 9136         FORMAT(' CRATET ORGANON ERROR CODE, CYCLE= ',I2, &
                 ' IDX= ',I2, ' SERROR= ',I2, ' ERROR IGNORED.')
              IERROR = 0
              SERROR(I) = 0
            ELSE
               
              WRITE(JOSTND,9126) ICYC, I, SERROR(I)
 9126         FORMAT(' CRATET ORGANON ERROR CODE, CYCLE= ',I2, &
                    ' IDX= ',I2, ' SERROR= ',I2 )
            END IF
          END IF
        END DO
!
        DO I = 1, 2000
          IF( TWARNING(I) .NE. 0 ) THEN
            WRITE(JOSTND,9128) ICYC, TWARNING(I)
 9128       FORMAT(' CRATET ORGANON ERROR CODE, CYCLE= ',I2, &
                 ' TWARNING= ',I2 )
          END IF
!
          DO J = 1, 6
            IF( TERROR(I,J) .NE. 0 ) THEN
              WRITE(JOSTND,9129) ICYC, TERROR(I,J), I, J
 9129         FORMAT(' CRATET ORGANON ERROR CODE, CYCLE=' ,I2, &
                    ', TERROR(I,J)= ',I2, &
                    ', TREE NUMBER= ',I4, &
                    ', ERROR NUMBER= ',I4 )
            END IF
          END DO
        END DO
      ELSE
        IF(DEBUG)WRITE(JOSTND,9130) ICYC, IERROR
 9130   FORMAT(' CRATET ORGANON ERROR CODE, CYCLE= ',I2, &
              ' IERROR= ',I2 )
      END IF
!----------
!  END OF THE CRATET ERROR REPORTING SECTION
!
!  NOW RELOAD THE FVS ARRAYS WITH DBH, HT, AND CR FOR ALL VALID
!  ORGANON TREES
!  COUNT THE NUMBER OF RECORDS WITH MISSING CR OR HT FOR VALID ORGANON TREES
!----------
      NUNLOAD = 0
      KG = 0
      DO I = 1, IREC1
        IF(IORG(I) .EQ. 1)THEN
          NUNLOAD = NUNLOAD + 1
          KG = ITEM(I)
          DBH(I)= DBH1(KG)
          IF(HT(I).LE.0.0 .AND. HT1OR(KG).GT.0.0) &
            KNTOHT(ISP(I))=KNTOHT(ISP(I))+1
          HT(I)= HT1OR(KG)
          IF(ICR(I).LE.0 .AND. CR1(KG).GT.0.0) &
            KNTOCR(ISP(I))=KNTOCR(ISP(I))+1
          ICR(I) = ANINT( CR1(KG) * 100.0 )
          IF(DEBUG)WRITE(JOSTND,*) &
          ' KG,I,ITRE,ISP,DBH,HT,ICR,PROB,ISPECL,NUNLOAD,KNTOHT,', &
          'KNTOCR= ',KG,I,ITRE(I),ISP(I),DBH(I),HT(I),ICR(I),PROB(I), &
          ISPECL(I),NUNLOAD,KNTOHT(ISP(I)),KNTOCR(ISP(I))
        ENDIF
      ENDDO
      IF(DEBUG)WRITE(JOSTND,*)' NVALID,NUNLOAD= ',NVALID,NUNLOAD
!----------
!  IF DEBUGGING PRINT OUT THE CALIBRATION VALUES.
!----------
      IF( DEBUG) THEN
        DO I=1,18
          WRITE(JOSTND,9220) I, ACALIB(1,I), &
                 I, ACALIB(2,I), &
                 I, ACALIB(3,I)
 9220       FORMAT ('   ORGANON ', &
                 ' ACALIB(1,',I2,') = ',F9.6, &
                 ' ACALIB(2,',I2,') = ',F9.6, &
                 ' ACALIB(3,',I2,') = ',F9.6 )
          WRITE(JOSTND,9221) I, TMPCAL(1,I), &
                 I, TMPCAL(2,I), &
                 I, TMPCAL(3,I), I, NEWCAL(I)
 9221       FORMAT ('   ORGANON ', &
                 ' TMPCAL(1,',I2,') = ',F9.6, &
                 ' TMPCAL(2,',I2,') = ',F9.6, &
                 ' TMPCAL(3,',I2,') = ',F9.6, &
                 ' NEWCAL('I2') = ',I3)
        ENDDO
      ENDIF
!----------
!  LOAD ACALIB(I,J) WITH ANY CALIBRATION VALUES COMPUTED IN **PREPARE**
!  WHICH WERE NOT LOADED BY KEYWORD ENTRY
!----------
      DO I=1,18
        DO J=1,3
          IF((ACALIB(J,I) .EQ. 1.0) .AND. &
          ((TMPCAL(J,I) .GT. 0.0) .AND. (TMPCAL(J,I) .NE. 1.0))) THEN
            ACALIB(J,I) = TMPCAL(J,I)
            NEWCAL(I) = 1
          ENDIF
        END DO
      END DO
!-------
!     ORGANON - END
!-------
  261 CONTINUE
!-------
!     IF THERE ARE TREE RECORDS, BRANCH TO PREFORM CALIBRATION.
!-------
      AX=0.
      IF (ITRN.GT.0) GOTO 1
!----------
!     CALL MAICAL TO CALCULATE MAI
!----------
      CALL MAICAL
      CALL RCON
      ONTREM(7)=0.
      CALL DENSE
      CALL DGDRIV
      CALL REGENT(.FALSE.,1)
    1 CONTINUE
      DO 5 I=1,MAXSP
      SPCNT(I,1)=0.0
      SPCNT(I,2)=0.0
      SPCNT(I,3)=0.0
      IF (ISCT(I,1).EQ.0) GOTO 5
      J=IREF(I)
      IUSED(J)=NSP(I,1)
    5 CONTINUE
      IF((ITRN.LE.0).AND.(IREC2.GE.MAXTP1))GO TO 245
!----------
!  PRINT SPECIES LABELS AND NUMBER OF OBSERVATIONS IN CONTROL
!  TABLE.  THEN, RESET COUNTERS TO ZERO.
!----------
      WRITE(JOSTND,'(//''CALIBRATION STATISTICS:''//)')
      WRITE(JOSTND,9000) (IUSED(I), I=1,NUMSP)
 9000 FORMAT ((T49,11(1X,A2,3X)/))
      IF(NUMSP .LE. 11) THEN
        WRITE(JOSTND,9001) (UNDER, I=1,NUMSP)
      ELSE
        WRITE(JOSTND,9001) (UNDER, I=1,11)
      ENDIF
 9001 FORMAT (T49,11(A4,2X))
      WRITE(JOSTND,9002) (KOUNT(I), I=1,NUMSP)
 9002 FORMAT(/,'NUMBER OF RECORDS PER SPECIES', &
              ((T49,11(I4,2X)/)))
      DO 10 I=1,MAXSP
      KNT(I)=0
      KNT2(I)=0
   10 CONTINUE
!----------
!   CALL MBACAL TO IDENTIFY SITE SPECIES
!----------
      CALL MBACAL
!----------
!   CALL MAICAL TO CALCULATE MAI
!----------
      CALL MAICAL
!----------
!  CALL **RCON** TO INITIALIZE SITE DEPENDENT MODEL COEFFICIENTS.
!----------
      CALL RCON
!----------
!  CALL **RDPSRT** AND **DENSE** TO COMPUTE INITIAL STAND DENSITY
!  STATISTICS.  ONTREM(7) IS SET TO ZERO HERE TO ASSURE THAT RELDM1
!  WILL BE ASSIGNED IN **DENSE** IN THE FIRST CYCLE.
!----------
      DO 15 I=1,ITRN
      IND(I)=IND1(I)
   15 CONTINUE
      CALL RDPSRT(ITRN,DBH,IND,.FALSE.)
      ONTREM(7)=0.0
!----------
!  PREPARE INPUT DATA FOR DIAMETER GROWTH MODEL CALIBRATION.  IF
!  IDG IS 1, CONVERT THE PAST DIAMETER MEASUREMENT CARRIED IN DG TO
!  DIAMETER GROWTH.  IF IDG IS 3, CONVERT THE CURRENT DIAMETER
!  MEASUREMENT CARRIED IN DG TO DIAMETER GROWTH.  ACTUAL DIAMETER
!  INCREMENT MEASUREMENTS WILL BE CORRECTED FOR BARK GROWTH IN
!  THE CALIBRATION ROUTINE. (THIS CODE WAS MOVED HERE SO THAT THE
!  BACKDATING ALGORITHM IN **DENSE**, INVOKED DURING CALIBRATION,
!  IS CORRECT.)
!----------
      Q=1.0
      IF(IDG.EQ.3) Q=-1.0
      DO 230 II=1,ITRN
      I=IND1(II)
      IF(I.GE.IREC2) GO TO 230
      IF(DG(I).LE.0.0) GO TO 220
      IF(IDG.EQ.0.OR.IDG.EQ.2) GO TO 230
      DG(I)=Q*(DBH(I)-DG(I))
      GO TO 230
  220 CONTINUE
      DG(I)=-1.0
  230 CONTINUE
!---------
!  SET LBKDEN TRUE IF DIAMETERS ARE TO BE BACKDATED FOR DENSITY
!  CALCULATIONS.  AFTER THIS CALL TO DENSE, INSURE LBKDEN=FALSE.
!---------
      LBKDEN= IDG.LT.2
      CALL DENSE
      LBKDEN= .FALSE.
!----------
!  DELETE NON-PROJECTABLE RECORDS, AND REALIGN POINTERS TO THE
!  SPECIES ORDERED SORT.
!----------
      IF(IREC2.EQ.MAXTP1) GO TO 60
      DO 50 I=IREC2,MAXTRE
      ISPC=ISP(I)
      IPTR=IREF(ISPC)
      IF(IMC(I).EQ.7)KNT(IPTR)=KNT(IPTR)+1
      IF (DEBUG) WRITE(JOSTND,9003) I,IMC(I),ISPC
 9003 FORMAT('IN CRATET: DEAD TREE RECORD:  I=',I4,',  IMC=',I2, &
             ',  SPECIES=',I2,' (9003 CRATET)')
      IF(ITRN.GT.0)THEN
        I1=ISCT(ISPC,1)
        I2=ISCT(ISPC,2)
        DO 30 I3=I1,I2
        IF(IND1(I3).EQ.I) GO TO 40
   30   CONTINUE
   40   IND1(I3)=IND1(I2)
        ISCT(ISPC,2)=I2-1
        IF(ISCT(ISPC,2).GE.ISCT(ISPC,1)) GO TO 50
        ISCT(ISPC,1)=0
        ISCT(ISPC,2)=0
      ENDIF
   50 CONTINUE
!----------
!  WRITE CALIBRATION TABLE ENTRY FOR NON-PROJECTABLE RECORDS AND RESET
!  KNT ARRAY TO ZERO.
!----------
      WRITE(JOSTND,9004) (KNT(I),I=1,NUMSP)
 9004 FORMAT(/,'NUMBER OF RECORDS CODED AS RECENT MORTALITY', &
              ((T49,11(I4,2X)/)))
!---------
!  RESET TREE RECORD COUNTERS AND SAVE THE NUMBER OF SPECIES.
!----------
      ITRN=IREC1
      IF((ITRN.LE.0).AND.(IREC2.GE.MAXTP1))GOTO 245
      IF(ITRN.LE.0)GOTO 60
      ISPC=NUMSP
!---------
!  MAKE SURE THAT ALL THE SPECIES ORDER SORTS AND THE IND2 ARRAY
!  ARE IN THE PROPER ORDER. FIRST, SAVE THE SPECIES REFERENCES.
!---------
      DO 51 I=1,MAXSP
      KNT(I)=IREF(I)
   51 CONTINUE
      CALL SPESRT
!---------
!  IF THE NUMBER OF SPECIES HAS CHANGED, WE MUST REWRITE THE
!  COLUMN HEADINGS.
!---------
      IF (ISPC.NE.NUMSP) THEN
         WRITE(JOSTND,52)
   52    FORMAT (/'***** NOTE:  SPECIES HAVE BEEN DROPPED.')
         DO 55 I=1,MAXSP
         IF (ISCT(I,1).EQ.0) GOTO 55
         J=IREF(I)
         IUSED(J)=NSP(I,1)
   55    CONTINUE
         WRITE(JOSTND,9000) (IUSED(I), I=1, NUMSP)
         WRITE(JOSTND,9001) (UNDER, I=1, NUMSP)
      ELSE
!
!        RESET THE REFERENCES.
!
         DO 57 I=1,MAXSP
         IREF(I)=KNT(I)
   57    CONTINUE
      ENDIF
!----------
!  SORT REMAINING TREE RECORDS IN ORDER OF DESCENDING DIAMETER.
!  STORE POINTERS TO SORTED ORDER IN IND.
!----------
      CALL RDPSRT(ITRN,DBH,IND,.TRUE.)
   60 CONTINUE
      DO 65 I=1,MAXSP
      KNT(I)=0
   65 CONTINUE
!----------
!  ENTER LOOP TO ADJUST HEIGHT-DBH MODEL FOR LOCAL CONDITIONS.  IF
!  THERE ARE 3 OR MORE TREES WITH MEASURED HEIGHTS FOR A GIVEN
!  SPECIES, ADJUST THE INTERCEPT (ASYMPTOTE) IN THE MODEL SO THAT
!  THE MEAN RESIDUAL FOR THE MEASURED TREES IS ZERO.
!  IF LHTDRG IS FALSE FOR A GIVEN SPECIES THEN ALL DUBBING IS DONE WITH
!  DEFAULT VALUES.
!----------
      DO 150 ISPC=1,MAXSP
      AA(ISPC)=0.
      BB(ISPC)=0.
      I1=ISCT(ISPC,1)
      IF(I1.LE.0) GO TO 141
      I2=ISCT(ISPC,2)
      IPTR=IREF(ISPC)
!----------
!  INITIALIZE SUMS FOR THIS SPECIES.
!----------
      K1=0
      K2=0
      K3=0
      K4=0
      SUMX=0.0
!----------
!  ENTER TREE LOOP WITHIN SPECIES.
!----------
      DO 80 I3=I1,I2
      I=IND1(I3)
      H=HT(I)
      NH=NORMHT(I)
      D=DBH(I)
      BX=HT2(ISPC)
!----------
!  BYPASS SUMS FOR TREES WITH MISSING HEIGHT OR TRUNCATED TOPS.
!  SEPERATE EQUATIONS ARE USED FOR TREES 5" AND LARGER VS TREES
!  LESS THAN 5" DBH. CALIBRATION OF THE HT-DBH RELATIONSHIP IS
!  ONLY DONE ON THE EQUATION USED FOR TREES 5" AND LARGER.
!----------
      IF(H.LE.4.5 .OR. NH.LT.0 .OR. D.LT.5.0) GO TO 70
      K1=K1+1
      XX = BX/(D+1.)
      YY = ALOG(H-4.5)
      SUMX=SUMX+YY-XX
      GO TO 80
!----------
!  COUNT NUMBER OF MISSING HEIGHTS AND BROKEN OR DEAD TOPS.  LOAD THE
!  ARRAY IND2 WITH SUBSCRIPTS TO THE RECORDS WITH MISSING HEIGHTS.
!----------
   70 CONTINUE
      IF(NH.LT.0) K3=K3+1
      IF(H.GT.0.0 .AND. NH.EQ.0) GO TO 80
      K2=K2+1
      IND2(K2)=I
!----------
!  END OF SUMMATION LOOP FOR THIS SPECIES.
!----------
!*** ONE LINE FIX FOR GROWTH METHOD 1 PROBLEM. DIXON 11-16-90.
      IF(HT(I) .LE. 0.1) HTG(I)=0.0
   80 CONTINUE
!----------
!  IF THERE ARE LESS THAN THREE OBSERVATIONS OR LHTDRG IS FALSE THEN
!   DUB HEIGHTS USING DEFAULT COEFFICIENTS FOR THIS SPECIES.
!----------
      KNT(IPTR)=K3
      IF(K1 .LT.3 .OR. .NOT. LHTDRG(ISPC)) GO TO 100
      XN=FLOAT(K1)
      AA(ISPC)=SUMX/XN
!----------
!  IF THE INTERCEPT IS NEGATIVE, USE THE DEFAULT VALUE.
!----------
      IF(AA(ISPC).GE.0.0)THEN
        IABFLG(ISPC)=0
      ENDIF
!----------
!  DUB IN MISSING HEIGHTS.
!  A VALUE LESS THAN ZERO STORED IN 'HT' => THAT TREE WAS TOP KILLED.
!  CONSEQUENTLY, A VALUE OF 80% OF THE PREDICTED HEIGHT IS STORED AS
!  THE TRUNCATED HEIGHT.
!----------
  100 CONTINUE
      AX=HT1(ISPC)
      BX=HT2(ISPC)
      IF (IABFLG(ISPC) .EQ. 0) AX=AA(ISPC)
      IF(K2.EQ.0) GO TO 140
      DO 130 JJ=1,K2
      II=IND2(JJ)
      D=DBH(II)
      JCR=((ICR(II)-1)/10)+1
      TKILL = NORMHT(II) .LT. 0.
!
      IF(D .LE. 0.1)THEN
        H=1.01
        GO TO 115
      ENDIF
!
      IF(D .LT. 5.0) THEN
        IICR=ICR(II)
        IF(IICR.LE.0)IICR=40
        H = HTT1(ISPC,1) + HTT1(ISPC,2)*D &
            + HTT1(ISPC,3)*FLOAT(IICR) + HTT1(ISPC,4)*D*D &
            + HTT1(ISPC,5)
        IF(ISPC .LE. 15 &
           .OR. (ISPC.GE.17 .AND. ISPC.LE.20) &
           .OR. (ISPC.GE.30 .AND. ISPC.LE.33)) H = EXP(H)
      ELSE
        H=EXP(AX+BX/(D+1.0))+4.5
      ENDIF
      IF (DEBUG) WRITE(JOSTND,88) AX,BX,D,H
  88  FORMAT('CRATET DUBBED HEIGHT: AX,BX,D,H=',4F8.2)
!
! SMALL PP USE A DIFFERENT LOGIC. RJ 7-26-88
! HARDWIRE CROWN AT 6 AND CHANGE SPLINE POINT TO 4.0" TO GET SMOOTH
! TRANSITION BETWEEN CURVES.  GD 3-26-96
!
      IF(ISPC.EQ.15 .AND. D.LT.4.0) THEN
      JCR=6
      H = 8.31485 + 3.03659*D - 0.59200*JCR
      ENDIF
!----------
!  USE INVENTORY EQUATIONS IF CALIBRATION OF THE HT-DBH FUNCTION IS TURNED
!  OFF, OR IF WYKOFF CALIBRATION DID NOT OCCUR.
!  NOTE: THIS SIMPLIFIES TO IF(IABFLB(ISPC).EQ.1) BUT IS SHOWN IN IT'S
!        ENTIRITY FOR CLARITY.
!----------
      IF(.NOT.LHTDRG(ISPC) .OR. &
         (LHTDRG(ISPC) .AND. IABFLG(ISPC).EQ.1))THEN
        CALL HTDBH (IFOR,ISPC,D,H,0)
        IF(DEBUG)WRITE(JOSTND,*)'INVENTORY EQN DUBBING IFOR,ISPC,D,H= ' &
        ,IFOR,ISPC,D,H
      ENDIF
!
      IF(H .LT. 4.5) H=4.5
  115 CONTINUE
      IF(TKILL) GO TO 120
      HT(II)=H
      K4=K4+1
      GO TO 125
  120 CONTINUE
      NORMHT(II)=H*100.0+0.5
      IF(ITRUNC(II).EQ.0) THEN
         IF(HT(II).GT.0.0) THEN
            ITRUNC(II)=80.0*HT(II)+0.5
         ELSE
            ITRUNC(II)=80.0*H+0.5
            HT(II)=H
         ENDIF
      ELSE
         IF(HT(II).GT.0.0) THEN
            IF(HT(II).LT.(ITRUNC(II)*0.01)) HT(II)=ITRUNC(II)*0.01
         ELSE
            HT(II)=ITRUNC(II)*0.01
         ENDIF
      ENDIF
      IF(NORMHT(II)*0.01.LT.HT(II)) NORMHT(II)=HT(II)*100.0
  125 CONTINUE
  130 CONTINUE
  140 CONTINUE
      KNT2(IPTR)=K4 + KNTOHT(ISPC)
      IF(DEBUG)WRITE(JOSTND,*)' ISPC,IPTR,K4,KNTOHT,KNT2= ', &
      ISPC,IPTR,K4,KNTOHT(ISPC),KNT2(IPTR)
!----------
!  END OF SPECIES LOOP.  PRINT HEIGHT-DIAMETER COEFFICIENTS ON
!  DEBUG UNIT IF DESIRED.
!----------
      IF(DEBUG)THEN
      WRITE(JOSTND,9005) ISPC,AX,BX,IABFLG(ISPC)
 9005 FORMAT('HEIGHT-DIAMETER COEFFICIENTS FOR SPECIES ',I2, &
            ':  INTERCEPT=',F10.6,'  SLOPE=',F10.6,'  FLAG=',I3, &
       ' (9005 CRATET)')
      ENDIF
!----------
!  LOOP THROUGH DEAD TREES AND DUB MISSING HEIGHTS FOR THIS SPECIES.
!----------
  141 CONTINUE
      IF(IREC2 .GT. MAXTRE) GO TO 150
      DO 145 II=IREC2,MAXTRE
      IF(ISP(II).NE.ISPC) GO TO 145
      AX=HT1(ISPC)
      BX=HT2(ISPC)
      IF (IABFLG(ISPC) .EQ. 0) AX=AA(ISPC)
      JCR=((ICR(II)-1)/10)+1
      IICR=ICR(II)
      IF(IICR.LE.0)IICR=40
      D=DBH(II)
      TKILL = NORMHT(II) .LT. 0.
      IF(HT(II).GT.0. .AND. TKILL) GO TO 142
      IF(HT(II).GT.0.) GO TO 146
!
      IF(D .LE. 0.1)THEN
        H=1.01
        GO TO 144
      ENDIF
!
      IF(D .LT. 5.0) THEN
        H = HTT1(ISPC,1) + HTT1(ISPC,2)*D &
            + HTT1(ISPC,3)*FLOAT(IICR) + HTT1(ISPC,4)*D*D &
            + HTT1(ISPC,5)
        IF(ISPC .LE. 15 &
           .OR. (ISPC.GE.17 .AND. ISPC.LE.20) &
           .OR. (ISPC.GE.30 .AND. ISPC.LE.33)) H = EXP(H)
      ELSE
        H=EXP(AX+BX/(D+1.0))+4.5
      ENDIF
!
! SMALL PP USE A DIFFERENT LOGIC. RJ 7-26-88
! HARDWIRE CROWN AT 6 AND CHANGE SPLINE POINT TO 4.0" TO GET SMOOTH
! TRANSITION BETWEEN CURVES.  GD 3-26-96
!
      IF(ISPC.EQ.15 .AND. D.LT.4.0) THEN
      JCR=6
      H = 8.31485 + 3.03659*D - 0.59200*JCR
      ENDIF
!----------
!  USE INVENTORY EQUATIONS IF CALIBRATION OF THE HT-DBH FUNCTION IS TURNED
!  OFF, OR IF WYKOFF CALIBRATION DID NOT OCCUR.
!  NOTE: THIS SIMPLIFIES TO IF(IABFLB(ISPC).EQ.1) BUT IS SHOWN IN IT'S
!        ENTIRITY FOR CLARITY.
!----------
      IF(.NOT.LHTDRG(ISPC) .OR. &
         (LHTDRG(ISPC) .AND. IABFLG(ISPC).EQ.1))THEN
        CALL HTDBH (IFOR,ISPC,D,H,0)
        IF(DEBUG)WRITE(JOSTND,*)'INVENTORY EQN DUBBING IFOR,ISPC,D,H= ' &
        ,IFOR,ISPC,D,H
      ENDIF
!
      IF(H .LT. 4.5) H=4.5
  144 CONTINUE
      IF(TKILL) GO TO 142
      HT(II)=H
      GO TO 146
  142 CONTINUE
      IF(HT(II) .GT. 0.) THEN
        NORMHT(II)=HT(II)*100.0+0.5
      ELSE
        NORMHT(II)=H*100.0+0.5
      ENDIF
      IF(ITRUNC(II).EQ.0) THEN
         IF(HT(II).GT.0.0) THEN
            ITRUNC(II)=80.0*HT(II)+0.5
         ELSE
            ITRUNC(II)=80.0*H+0.5
            HT(II)=H
         ENDIF
      ELSE
         IF(HT(II).GT.0.0) THEN
            IF(HT(II).LT.(ITRUNC(II)*0.01)) HT(II)=ITRUNC(II)*0.01
         ELSE
            HT(II)=ITRUNC(II)*0.01
         ENDIF
      ENDIF
      IF(NORMHT(II)*0.01.LT.HT(II)) NORMHT(II)=HT(II)*100.0
!----------
!   CALL FIRE SNAG MODEL TO ADD THE DEAD TREES TO THE
!   SNAG LIST; DEFLATE PROB(II), WHICH WAS TEMPORARILY
!   ADJUSTED TO ALLOW BACKDATING FOR CALIBRATION PURPOSES,
!   IN **NOTRE**
!----------
  146 CONTINUE
      IF (TKILL) THEN
        HS = ITRUNC(II) * 0.01
      ELSE
        HS = HT(II)
      ENDIF
      CALL FMSSEE (II,ISPC,D,HS, &
        (PROB(II)/(FINT/FINTM)),3,.FALSE.,JOSTND)
!
  145 CONTINUE
!----------
  150 CONTINUE
!----------
!  END OF HEIGHT DUBBING SEGMENT.  PRINT CONTROL TABLE ENTRIES FOR
!  USEABLE HEIGHTS AND MISSING HEIGHTS, AND REINITIALIZE COUNTERS.
!----------
      WRITE(JOSTND,9006) (KNT2(I),I=1,NUMSP)
 9006 FORMAT(/,'NUMBER OF RECORDS WITH MISSING HEIGHTS', &
             ((T49,11(I4,2X)/)))
      WRITE(JOSTND,9007) (KNT(I),I=1,NUMSP)
 9007 FORMAT(/,'NUMBER OF RECORDS WITH BROKEN OR DEAD TOPS', &
             ((T49,11(I4,2X)/)))
      DO 160 I=1,MAXSP
      KNT(I)=0
      KNT2(I)=KNTOCR(I)
      IF(DEBUG)WRITE(JOSTND,*)' I,IPTR,KNTOCR,KNT2= ', &
       I,IPTR,KNTOCR(I),KNT2(I)
  160 CONTINUE
!----------
!  CHECK FOR MISSING CROWNS ON LIVE TREES.
!  SAVE PCT IN OLDPCT TO RETAIN AN OLD PCTILE VALUE.
!----------
      MISSCR = .FALSE.
      DO 190 I=1,ITRN
      OLDPCT(I)= PCT(I)
      IF(ICR(I).LE.0)THEN
        MISSCR = .TRUE.
        ISPC=ISP(I)
        IPTR=IREF(ISPC)
        KNT2(IPTR)=KNT2(IPTR)+1
      ENDIF
      IF(ITRE(I).LE.0) ITRE(I) = 9999
  190 CONTINUE
!----------
!  CHECK FOR MISSING CROWNS ON CYCLE 0 DEAD TREES.
!----------
      IF(IREC2 .GT. MAXTRE) GO TO 192
      DO 191 I=IREC2,MAXTRE
      IF(ICR(I).LE.0)MISSCR=.TRUE.
  191 CONTINUE
  192 CONTINUE
!----------
!  CALL **CROWN** IF ANY CROWN RATIOS ARE MISSING.
!----------
      IF(MISSCR)CALL CROWN
!----------
!  PRINT CONTROL TABLE ENTRY FOR MISSING CROWN RATIOS; RESET COUNTERS.
!----------
      WRITE(JOSTND,9008) (KNT2(I),I=1,NUMSP)
 9008 FORMAT(/,'NUMBER OF RECORDS WITH MISSING CROWN RATIOS', &
             ((T49,11(I4,2X)/)))
      DO 200 I=1,MAXSP
      KNT2(I)=0
  200 CONTINUE
!----------
!   CALL AVHT40 TO CALCULATE AVERAGE HEIGHT. THIS CALL IS NEEDED
!   IN SORNEC BECAUSE DGF ROUTINE USES AVH IN CALCULATION OF DDS.
!----------
      CALL AVHT40
!----------
!  CALL DGDRIV TO CALIBRATE DIAMETER GROWTH EQUATIONS.
!----------
      IF(DEBUG)WRITE(JOSTND,*)'CALL DGDRIV FROM CRATET SECOND TIME'
      CALL DGDRIV
!----------
!  PREPARE INPUT DATA FOR HEIGHT GROWTH MODEL CALIBRATION; IT'S DONE
!  THE SAME AS THE DIAMETER GROWTH MODEL SEEN ABOVE.
!----------
      IF(IHTG.EQ.0 .OR. IHTG.EQ.2) GOTO 236
      Q = 1.
      IF(IHTG.EQ.3) Q = -1.
      DO 233 I=1,ITRN
      IF(HTG(I).LE.0.) GOTO 233
      HTG(I) = Q * (HT(I)-HTG(I))
      IF(HT(I) .LE. 0.0) HTG(I)=0.0
  233 CONTINUE
  236 CONTINUE
!----------
!  ESTIMATE MISSING TOTAL TREE AGE
!----------
      IF(DEBUG)WRITE(JOSTND,*)'IN CRATET, CALLING FINDAG'
      DO I=1,ITRN
      IF(ABIRTH(I) .LE. 0.)THEN
        SITAGE = 0.0
        SITHT = 0.0
        AGMAX = 0.0
        HTMAX = 0.0
        HTMAX2 = 0.0
        ISPC = ISP(I)
        D1 = DBH(I)
        H = HT(I)
        D2 = 0.0
        CALL FINDAG(I,ISPC,D1,D2,H,SITAGE,SITHT,AGMAX,HTMAX,HTMAX2, &
                    DEBUG)
        IF(SITAGE .GT. 0.)ABIRTH(I)=SITAGE
      ENDIF
      ENDDO
!----------
!  CALL REGENT TO CALIBRATE THE SMALL TREE HEIGHT INCREMENT MODEL.
!----------
      CALL REGENT(.FALSE.,1)
!----------
!  LOAD SPCNT WITH NUMBER OF TREES PER ACRE BY SPECIES AND TREE
!  CLASS.
!----------
      DO 240 I=1,ITRN
      IS=ISP(I)
      IM=IMC(I)
      SPCNT(IS,IM)=SPCNT(IS,IM)+PROB(I)
  240 CONTINUE
!----------
!  COMPUTE DISTRIBUTION OF TREES PER ACRE AND SPECIES-TREE CLASS
!  COMPOSITION BY TREES PER ACRE.
!----------
  245 CONTINUE
      CALL PCTILE(ITRN,IND,PROB,WK3,ONTCUR(7))
      CALL DIST(ITRN,ONTCUR,WK3)
      CALL COMP(OSPCT,IOSPCT,SPCNT)
      IF (ITRN.LE.0) GO TO 500
!----------
!  CALL **DENSE** TO CALCULATE STAND DENSITY STATISTICS FOR
!  INITIAL INVENTORY.
!----------
      IF(DEBUG) WRITE(JOSTND,9013) ICYC
 9013 FORMAT('CALLING DENSE, CYCLE=',I2)
      CALL DENSE
!----------
!  COUNT AND PRINT NUMBER OF RECORDS WITH MISTLETOE.
!----------
      CALL MISCNT(KNT)
      WRITE(JOSTND,248) (KNT(I),I=1,NUMSP)
  248 FORMAT(/,'NUMBER OF RECORDS WITH MISTLETOE',((T49,11(I4,2X)/)))
!----------
!  PRINT ANY NEW ORGANON CALIBRATION VALUES
!----------
      IHEAD=0
      DO I=1,11
        IF(NEWCAL(I) .NE. 0)THEN
          IF(IHEAD .EQ. 0) THEN
            WRITE(JOSTND,250) LABEL(I),(ACALIB(J,I),J=1,3)
 250        FORMAT(/,'ORGANON NEW CALIBRATION RATIOS:',/, &
            20X,'  SPECIES GROUP   HT/DBH    HTCB    DIAM',/, &
            5X,A31,3F8.2)
            IHEAD=1
          ELSE
            WRITE(JOSTND,251) LABEL(I),(ACALIB(J,I),J=1,3)
 251        FORMAT(5X,A31,3F8.2)
          ENDIF
        ENDIF
      ENDDO
!----------
!  CALL **SDICHK** TO SEE IF INITIAL STAND SDI IS ABOVE THE SPECIFIED
!  MAXIMUM SDI.  RESET MAXIMUM SDI IF THIS IS THE CASE.
!----------
      CALL SDICHK
!
  500 CONTINUE
      IF(DEBUG)WRITE(JOSTND,510)ICYC
  510 FORMAT('LEAVING SUBROUTINE CRATET  CYCLE =',I5)
!
      RETURN
      END
