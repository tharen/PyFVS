      SUBROUTINE FMCROW
      use contrl_mod
      use fmcom_mod
      use arrays_mod
      use fmparm_mod
      use prgprm_mod
      implicit none
C----------
C  **FMCROW  FIRE-IE DATE OF LAST REVISION:  01/10/12
C----------
C     CALLED FROM: FMSDIT, FMPRUN
C     CALLS        RDPSRT
C                  PCTILE
C  PURPOSE:
C     THIS SUBROUTINE CALCULATES CROWNW(TREE,SIZE), THE WEIGHT OF
C     VARIOUS SIZES OF CROWN MATERIAL THAT IS ASSOCIATED WITH EACH TREE
C     RECORD IN THE CURRENT STAND.  THESE WEIGHTS DEPEND ON TREE
C     SPECIES, DIAMETER, HEIGHT, AND CROWN RATIO ACCORDING TO THE
C     RELATIONSHIPS DESCRIBED IN BROWN & JOHNSTON, 1976, 'DEBRIS
C     PREDICTION SYSTEM', FUEL SCIENCE RWU 2104, WHICH IS ITSELF
C     BASED PRIMARILY ON RES PAPER INT-197.
C
C     NOTE:  THE ALLOCATION BETWEEN CROWN SIZE CLASS 4 AND 5 IS
C            SOMEWHAT ARBITRARY, WITH CLASS 5 CURRENTLY NOT RECEIVING ANY.
C----------
C  LOCAL VARIABLE DEFINITIONS:
C     D:        DBH
C     H:        HEIGHT
C     HPCT:     HEIGHT PERCENTILE RANKING OF EACH TREE LIST ELEMENT
C     HPOINT:   HEIGHT POINTER: SPECIFIES WHICH TREE LIST ELEMENT HAS
C               THE TALLEST TREES, NEXT TALLEST, AND SO ON.
C     IC:       LENGTH OF LIVE CROWN
C     SPIE:     EQUAL TO ISPMAP
C     SPIW:     SPECIES NUMBER IS IE VARIANT
C     ISPMAP:   SPECIES (NUMBER) IN FMCROWW OR FMCROWE THAT DIFFERENT
C               SPECIES ARE MAPPED TO
C----------
COMMONS
C----------
C  VARIABLE DECLARATIONS
C----------
      LOGICAL DEBUG
      INTEGER I,J,IC,ISPMAP(MAXSP),HPOINT(MAXTRE)
      INTEGER ITR,SPIW,SPIE
      REAL    D,H,HPCT(MAXTRE),HP,SG,JUNK,XV(0:5)
C----------
C  INDEX TO THE CROWN EQUATIONS USED BY THE WESTERN (FMCROWW) AND
C  EASTERN (FMCROWE) CROWN EQUATION ROUTINES. EASTERN EQUATIONS ARE
C  BASED ON LS-FFE; WESTERN ON CR-FFE (BUT ARE NEARLY UNIFORM ACROSS
C  ALL WESTERN FFE VARIANTS). IN THE TABLE BELOW, A '-' IN THE
C  "MAPS TO" COLUMN INDICATES A SPECIES THAT MAPS TO ITSELF
C
C     I   NAME                     MAPS TO          WEST   EAST
C --------------------------------------------------------------
C     1 = WESTERN WHITE PINE       -                15
C     2 = WESTERN LARCH            -                 8
C     3 = DOUGLAS-FIR              -                 3
C     4 = GRAND FIR                -                 4
C     5 = WESTERN HEMLOCK          -                 6
C     6 = WESTERN REDCEDAR         -                 7
C     7 = LODGEPOLE PINE           -                11
C     8 = ENGELMANN SPRUCE         -                18
C     9 = SUBALPINE FIR            -                 1
C    10 = PONDEROSA PINE           -                13
C    11 = MOUNTAIN HEMLOCK         -                24
C    12 = WHITEBARK PINE           -                14
C    13 = LIMBER PINE           lodgepole pine      11
C    14 = SUBALPINE LARCH       subalpine fir        1
C    15 = PINYON PINE              -                12
C    16 = ROCKY MOUNTAIN JUNIPER   -                16
C    17 = PACIFIC YEW           western redcedar     7
C    18 = QUAKING ASPEN            -                        41
C    19 = COTTONWOODS           eastern cottonwood          17
C    20 = MOUNTAIN MAPLE        bigleaf maple        5
C    21 = PAPER BIRCH              -                        43
C    22 = OTHER HARDWOODS       red alder           23
C    23 = OTHER SOFTWOODS     mountain hemlock      24
C----------
      DATA ISPMAP / 15, 8, 3, 4, 6, 7, 11, 18, 1, 13,
     &              24, 14, 11, 1, 12, 16, 7, 41, 17, 5,
     &             43, 23, 24/

C----------
C  CHECK FOR DEBUG.
C----------
      CALL DBCHK (DEBUG,'FMCROW',6,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC,ITRN
    7 FORMAT(' ENTERING FMCROW CYCLE = ',I2,' ITRN=',I5)
C
      IF (ITRN.EQ.0) RETURN
C----------
C  YOU'LL NEED TO KNOW PERCENTILE HEIGHT OF EACH TREE.
C  TO GET THIS, MAKE AN ARRAY THAT LISTS THE TREE LIST ELEMENTS
C  IN DESCENDING ORDER BY THE HEIGHT OF EACH RECORD:
C----------
      CALL RDPSRT(ITRN,HT,HPOINT,.TRUE.)
C----------
C  NOW CALL PCTILE TO GET THE HEIGHT PERCENTILE OF EACH RECORD.
C  NOTE THAT PCTILE ONLY WORKS IF YOU PASS IT THE DENSITY OF TREES IN
C  EACH RECORD RATHER THAN THE HEIGHT OF THE RECORD. NOTE ALSO THAT
C  ANY RECORDS WITH NO TREES WILL NONETHELESS COME BACK WITH THE
C  PERCENTILE RANKING IMPLIED BY THEIR HEIGHT. SUCH RECORDS WILL NOT
C  INFLUENCE THE PERCENTILE RANKING OF TREES IN OTHER RECORDS.
C----------
      CALL PCTILE (ITRN,HPOINT,PROB,HPCT,JUNK)
C
      DO 999 I = 1,ITRN
C----------
C  INCREMENT GROW TO KEEP TRACK OF WHETHER THIS CROWN IS FREE
C  TO GROW AFTER BEING BURNED IN A FIRE.  SKIP THE REST OF THE LOOP
C  IF GROW IS STILL LESS THAN 1 AFTER THE INCREMENT.
C----------
        IF (GROW(I) .LT. 1) GROW(I) = GROW(I) + 1
        IF (GROW(I) .LT. 1) GOTO 999
C----------
C  ARGUMENTS TO PASS
C----------
        SPIW = ISP(I)
        SPIE = ISPMAP(SPIW)
        D   = DBH(I)
        H   = HT(I)
        IC  = ICR(I)
        ITR = ITRUNC(I)
        HP  = HPCT(I)
        SG  = V2T(SPIW)
C----------
C  INITIALIZE ALL THE CANOPY COMPONENTS TO ZERO, AND SKIP THE REST
C  OF THIS LOOP IF THE TREE HAS NO DIAMETER, HEIGHT, OR LIVE CROWN.
C----------
        DO J = 0,5
        XV(J) = 0.0
        ENDDO
C
        SELECT CASE (SPIW)
        CASE (18,19,21)
          CALL FMCROWE(SPIE,SPIW,D,H,IC,SG,XV)
        CASE DEFAULT
          CALL FMCROWW(SPIE,D,H,ITR,IC,HP,SG,XV)
        END SELECT
C----------
C  COPY TEMPORARY VALUES TO FFE ARRAY
C----------
        DO J = 0,5
          CROWNW(I,J) = XV(J)
          IF (DEBUG) WRITE(JOSTND,*) 'I=',I,' size=',J,
     &    ' CROWNW=',CROWNW(I,J)
        ENDDO
C
  999 CONTINUE
C
      RETURN
      END
C----------
C  PLACEHOLDER FOR UNUSED CALLS IN **FMCROWE**
C----------
      SUBROUTINE HTDBH(I10,I11,X10,X11,I12)
      implicit none
C
      INTEGER I10,I11,I12
      REAL    X10,X11
C
      I10 = 0
      I11 = 0
      I12 = 0
C
      X10 = 0.0
      X11 = 0.0
C
      RETURN
      END

