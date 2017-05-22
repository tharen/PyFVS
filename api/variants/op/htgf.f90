      SUBROUTINE HTGF
      use findage_mod, only: findag

      use prgprm_mod
      use arrays_mod
      use coeffs_mod
      use contrl_mod
      use outcom_mod
      use plot_mod
      use multcm_mod
      use htcal_mod
      use pden_mod
      use varcom_mod
      use organon_mod
      implicit none
!----------
!  **HTGF--OP    DATE OF LAST REVISION:  06/17/15
!----------
!  THIS SUBROUTINE COMPUTES THE PREDICTED PERIODIC HEIGHT
!  INCREMENT FOR EACH CYCLE AND LOADS IT INTO THE ARRAY HTG.
!  HEIGHT INCREMENT IS PREDICTED FROM SPECIES, HABITAT TYPE,
!  HEIGHT, DBH, AND PREDICTED DBH INCREMENT.  THIS ROUTINE
!  IS CALLED FROM **TREGRO** DURING REGULAR CYCLING.  ENTRY
!  **HTCONS** IS CALLED FROM **RCON** TO LOAD SITE DEPENDENT
!  CONSTANTS THAT NEED ONLY BE RESOLVED ONCE. CALLS **FINDAG
!  TO CALCULATE TREE AGE.
!----------
!OMMONS
!----------
      LOGICAL DEBUG
      INTEGER MAPHD(MAXSP)
      INTEGER ITFN,I3,I2,I1,ISPC,I,MAPPHD
      REAL SITAGE,SITHT,AGMAX,HTMAX,HTMAX2,D2,BARK,BRATIO
      REAL RHR(MAXSP),RHYXS(MAXSP),RHM(MAXSP),RHB(MAXSP)
      REAL TEMHTG,HTGMOD,WTRH,WTCR,HGMDRH,FCTRM,FCTRXB,FCTRRB,FCTRKX,RHX
      REAL HGMDCR,RELHT,POTHTG,TEMPD,TEMPH,H,D,XHT,SINDX
      REAL CRC,CRB,CRA,RHXS,RHK,AGP10,HGUESS,HGUESS1,HGUESS2,SCALE
      REAL MAXGUESS
      REAL MISHGF
!----------
!  COEFFICIENTS--CROWN RATIO (CR) BASED HT. GRTH. MODIFIER
!----------
      CRA = 100.0
      CRB = 3.0
      CRC = -5.0
!----------
!  COEFFICIENTS--RELATIVE HEIGHT (RH) BASED HT. GRTH. MODIFIER
!----------
      RHK = 1.0
      RHXS = 0.0
!----------
!  COEFFS BASED ON SPECIES SHADE TOLERANCE AS FOLLOWS:
!                                   RHR  RHYXS    RHM    RHB
!        VERY TOLERANT             20.0   0.20    1.1  -1.10
!        TOLERANT                  16.0   0.15    1.1  -1.20
!        INTERMEDIATE              15.0   0.10    1.1  -1.45
!        INTOLERANT                13.0   0.05    1.1  -1.60
!        VERY INTOLERANT           12.0   0.01    1.1  -1.60
!  IN THE WC VARIANT, SILVICS OF NORTH AMERICA (AG.HNDBK-654)
!  WAS USED TO GET SHADE TOLERANCE. SHADE TOLERANCE COEFFICIENTS WERE
!  DEVEOLPED BY D.DONNELY, TO COMPUTE HGMDRH (RELAT. HT. CONTRIBUTION)
!  SEQ. NO.   CHAR. CODE    SHADE TOL.   SEQ. NO.  CHAR. CODE    SHADE TOL.
!      1      SF            TOLN            21     BM            VTOL
!      2      WF            TOLN            22     RA            INTL
!      3      GF            TOLN            23     MA            INTL
!      4      AF            VTOL            24     TO            INTM
!      5      RF            TOLN            25     GC            INTM
!      6      SS            TOLN            26     AS            VINT
!      7      NF            INTM            27     CW            VINT
!      8      YC            TOLN            28     WO            INTM
!      9      IC            VTOL            29     J             INTL
!      10     ES            TOLN            30     LL            VINT
!      11     LP            VINT            31     WB            INTM
!      12     JP            INTL            32     KP            VINT
!      13     SP            INTM            33     PY            VTOL
!      14     WP            INTM            34     DG            VTOL
!      15     PP            INTL            35     HT            VINT
!      16     DF            INTM            36     CH            INTL
!      17     RW            VTOL            37     WI            VINT
!      18     RC            VTOL            38
!      19     WH            VTOL            39     OT            INTM
!      20     MH            VTOL
!----------
      RHR = (/ 16.0,  16.0,  16.0,  20.0,  16.0, &
               16.0,  15.0,  16.0,  20.0,  16.0, &
               12.0,  13.0,  15.0,  15.0,  13.0, &
               15.0,  20.0,  20.0,  20.0,  20.0, &
               20.0,  13.0,  13.0,  15.0,  15.0, &
               12.0,  12.0,  15.0,  13.0,  12.0, &
               15.0,  12.0,  20.0,  20.0,  12.0, &
               13.0,  12.0,  15.0,  15.0/)
      RHYXS = (/ 0.15,  0.15,  0.15,  0.20,  0.15,  &
                 0.15,  0.10,  0.15,  0.20,  0.15, &
                 0.01,  0.05,  0.10,  0.15,  0.05, &
                 0.10,  0.20,  0.20,  0.20,  0.20, &
                 0.20,  0.05,  0.05,  0.10,  0.10, &
                 0.01,  0.01,  0.10,  0.05,  0.01, &
                 0.10,  0.01,  0.20,  0.20,  0.01, &
                 0.05,  0.01,  0.10,  0.10/)
      RHM(:) = 1.10
      RHB = (/ -1.20, -1.20, -1.20, -1.10, -1.20, &
               -1.20, -1.45, -1.20, -1.10, -1.20, &
               -1.60, -1.60, -1.45, -1.45, -1.60, &
               -1.45, -1.10, -1.10, -1.10, -1.10, &
               -1.10, -1.60, -1.60, -1.45, -1.45, &
               -1.60, -1.60, -1.45, -1.60, -1.60, &
                0.10, -1.60, -1.10, -1.10, -1.60, &
               -1.60, -1.60, -1.45, -1.45/)
!-----------
!  SEE IF WE NEED TO DO SOME DEBUG.
!-----------
      CALL DBCHK (DEBUG,'HTGF',4,ICYC)
      IF(DEBUG) WRITE(JOSTND,3)ICYC
    3 FORMAT(' ENTERING SUBROUTINE HTGF CYCLE =',I5)
!
      IF(DEBUG)WRITE(JOSTND,*) 'IN HTGF AT BEGINNING,HTCON=', &
      HTCON,'RMAI=',RMAI,'ELEV=',ELEV
      SCALE=FINT/YR
!----------
!  GET THE HEIGHT GROWTH MULTIPLIERS.
!----------
      CALL MULTS (2,IY(ICYC),XHMULT)
      IF(DEBUG)WRITE(JOSTND,*)'HTGF- ISPC,IY(ICYC),XHMULT= ',ISPC, &
      IY(ICYC), XHMULT
!----------
!   BEGIN SPECIES LOOP:
!----------
      DO 40 ISPC=1,MAXSP
      I1 = ISCT(ISPC,1)
      IF (I1 .EQ. 0) GO TO 40
      I2 = ISCT(ISPC,2)
      SINDX = SITEAR(ISPC)
      XHT=XHMULT(ISPC)
!-----------
!   BEGIN TREE LOOP WITHIN SPECIES LOOP
!
!   XHT CONTAINS THE HEIGHT GROWTH MULTIPLIER FROM THE HTGMULT KEYWORD
!   HTCON CONTAINS THE HEIGHT GROWTH MULTIPLIER FROM THE READCORH KEYWORD
!-----------
      DO 30 I3 = I1,I2
      I=IND1(I3)
      HTG(I)=0.
!----------
!  START ORGANON
!
      IF(IORG(I) .EQ. 1) THEN
        HTG(I)=SCALE*XHT*HGRO(I)*EXP(HTCON(ISPC))
        IF(DEBUG)WRITE(JOSTND,*)' HTGF ORGANON I,ISP,DBH,HT,HTG,HGRO,', &
        'SCALE,XHT,HTCON,IORG= ',I,ISP(I),DBH(I),HT(I),HTG(I),HGRO(I), &
        SCALE,XHT,HTCON(ISPC),IORG(I)
        GO TO 161
      ENDIF
!
!  END ORGANON
!----------
      H=HT(I)
      D=DBH(I)
!
      SITAGE = 0.0
      SITHT = 0.0
      AGMAX = 0.0
      HTMAX = 0.0
      HTMAX2 = 0.0
      BARK=BRATIO(ISPC,D,H)
      D2 = D + DG(I)/BARK
      IF (PROB(I).LE.0.0) GO TO 161
      IF(DEBUG)WRITE(JOSTND,*)' IN HTGF, CALLING FINDAG I= ',I
      CALL FINDAG(I,ISPC,D,D2,H,SITAGE,SITHT,AGMAX,HTMAX,HTMAX2,DEBUG)
!
!----------
!  CHECK TO SEE IF TREE HT/DBH RATIO IS ABOVE THE MAXIMUM RATIO AT
!  THE BEGINNING OF THE CYCLE. THIS COULD HAPPEN FOR TREES COMING
!  OUT OF THE ESTAB MODEL.  IF IT IS, THEN CHECK TO SEE IF THE
!  HT/NEWDBH RATIO IS ABOVE THE MAXIMUM.  IF THIS IS ALSO TRUE, LIMIT
!  HTG TO 0.1 FOOT OR HALF THE DG, WHICH EVER IS GREATER.
!  IF IT ISN'T, THEN LET HTG BE COMPUTED THE NORMAL
!  WAY AND THEN CHECK IT AGAIN AT THAT POINT.
!----------
      TEMPH=H + HTG(I)
      IF(H .GT.HTMAX) THEN
         IF(H .GE. HTMAX2) THEN
           HTG(I)=0.5 * DG(I)
           IF(HTG(I).LT.0.1)HTG(I)=0.1
           HTG(I)=SCALE*XHT*HTG(I)*EXP(HTCON(ISPC))
           IF(DEBUG)WRITE(JOSTND,*)' I,ISPC,H,HTMAX2,HTG,SCALE,XHT,', &
           'HTCON= ',I,ISPC,H,HTMAX2,HTG(I),SCALE,XHT,HTCON(ISPC)
         ENDIF
         GO TO 161
      END IF
!
!----------
!  NORMAL HEIGHT INCREMENT CALCULATON BASED ON TREE AGE
!  FIRST CHECK FOR MAXIMUM TREE AGE
!----------
      IF (SITAGE .GE. AGMAX) THEN
        POTHTG= 0.10
        IF(ISPC.EQ.2 .OR. ISPC.EQ.3)THEN
          POTHTG=(0.2+0.00264*SINDX)*5.
        ENDIF
        GO TO 1320
      ELSE
        AGP10= SITAGE + 5.0
      ENDIF
!----------
!  CALL HTCALC FOR NORMAL CYCLING
!----------
      IF(DEBUG)WRITE(JOSTND,*)' ISPC,I,HGUESS,AGP10= ', &
      ISPC,I,HGUESS,AGP10
!
      HGUESS = 0.0
      CALL HTCALC(SINDX,ISPC,AGP10,HGUESS,JOSTND,DEBUG)
      POTHTG= HGUESS-SITHT
!----------
!  PATCH FOR OREGON WHITE OAK - WORK BY GOULD AND HARRINGTON, PNW
!  USES A HT-DBH EQUATION MODIFIED BY SI AND BA, FIRST PREDICTS
!  HEIGHT GUESS BASED ON PREVIOUS DIAMETER AND THEN PREDICT THE
!  HEIGHT GUESS BASED ON PRESENT DIAMETER, SUBTRACT GUESSES
!  TO CALCULATE HEIGHT GROWTH.
!----------
      IF (ISPC .EQ. 28) THEN
!----------
!  CALCULATE MAX HEIGHT BASED ON SI, THEN MODIFY BASED ON BA
!----------
        MAXGUESS = SINDX - 18.6024/ALOG(2.7 + BA)
!----------
!  DUB HEIGHT BASED ON PRESENT DBH
!----------
        D2 = DBH(I) + DG(I)
        IF (D2 .LT. 0.) D2 = 0.1
        HGUESS2 = 4.5 + MAXGUESS*(1-EXP(-0.137428*D2))**1.38994
!----------
!  DUB HEIGHT BASED ON PAST DBH
!----------
        HGUESS1 = 4.5 + MAXGUESS*(1-EXP(-0.137428*D))**1.38994
!----------
!  DIFFERENCE OF TWO DUBBED HEIGHTS IS POTENTIAL HEIGHT GROWTH
!
!  SINCE DG IS ON A 10-YEAR BASIS HERE, POTHTG IS A 10-YEAR ESTIMATE.
!  DIVIDE IT BY 2 TO GET IT ON A 5-YEAR BASIS. IT WILL BE SCALED TO
!  THE CYCLE LENGTH BELOW
!----------
        POTHTG = (HGUESS2 - HGUESS1) / 2.0
!      IF(DEBUG)WRITE(JOSTND,*)'ISPC,I,H,PHTG,HGS1,HGS2,MX,D,D2,BA,DG',
!     &ISPC,I,H,POTHTG,HGUESS1,HGUESS2,MAXGUESS,D,D2,BA,DG(I)
      ENDIF
!--End OWO PATCH
!----------
!  HEIGHT GROWTH MUST BE POSITIVE
!----------
      IF(POTHTG .LT. 0.1)POTHTG= 0.1
!---------
! ASSIGN A POTENTIAL HTG FOR THE ASYMPTOTIC AGE
!---------
 1320 CONTINUE
!----------
!  HEIGHT GROWTH MODIFIERS
!----------
      RELHT = 0.0
      IF(AVH .GT. 0.0) RELHT=HT(I)/AVH
      IF(RELHT .GT. 1.5)RELHT=1.5
!-----------
!     REVISED HEIGHT GROWTH MODIFIER APPROACH.
!-----------
!     CROWN RATIO CONTRIBUTION.  DATA AND READINGS INDICATE HEIGHT
!     GROWTH PEAKS IN MID-RANGE OF CR, DECREASES SOMEWHAT FOR LARGE
!     CROWN RATIOS DUE TO PHOTOSYNTHETIC ENERGY PUT INTO CROWN SUPPORT
!     RATHER THAN HT. GROWTH.  CROWN RATIO FOR THIS COMPUTATION NEEDS
!     TO BE IN (0-1) RANGE; DIVIDE BY 100.  FUNCTION IS HOERL'S
!     SPECIAL FUNCTION (REF. P.23, CUTHBERT&WOOD, FITTING EQNS. TO DATA
!     WILEY, 1971).  FUNCTION OUTPUT CONSTRAINED TO BE 1.0 OR LESS.
!-----------
      HGMDCR = (CRA * (ICR(I)/100.0)**CRB) * EXP(CRC*(ICR(I)/100.0))
      IF (HGMDCR .GT. 1.0) HGMDCR = 1.0
!-----------
!     RELATIVE HEIGHT CONTRIBUTION.  DATA AND READINGS INDICATE HEIGHT
!     GROWTH IS ENHANCED BY STRONG TOP LIGHT AND HINDERED BY HIGH
!     SHADE EVEN IF SOME LIGHT FILTERS THROUGH.  ALSO RESPONSE IS
!     GREATER FOR GIVEN LIGHT AS SHADE TOLERANCE INCREASES.  FUNCTION
!     IS GENERALIZED CHAPMAN-RICHARDS (REF. P.2 DONNELLY ET AL. 1992.
!     THINNING EVEN-AGED FOREST STANDS...OPTIMAL CONTROL ANALYSES.
!     USDA FOR. SERV. RES. PAPER RM-307).
!     PARTS OF THE GENERALIZED CHAPMAN-RICHARDS FUNCTION USED TO
!     COMPUTE HGMDRH BELOW ARE SEGMENTED INTO FACTORS
!     FOR PROGRAMMING CONVENIENCE.
!-----------
      RHX = RELHT
      FCTRKX = ( (RHK/RHYXS(ISPC))**(RHM(ISPC)-1.0) ) - 1.0
      FCTRRB = -1.0*( RHR(ISPC)/(1.0-RHB(ISPC)) )
      FCTRXB = RHX**(1.0-RHB(ISPC)) - RHXS**(1.0-RHB(ISPC))
      FCTRM  = -1.0/(RHM(ISPC)-1.0)
      IF (DEBUG) &
      WRITE(JOSTND,*) ' HTGF-HGMDRH FACTORS = ', &
      ISPC, RHX, FCTRKX, FCTRRB, FCTRXB, FCTRM
      HGMDRH = RHK * ( 1.0 + FCTRKX*EXP(FCTRRB*FCTRXB) ) ** FCTRM
!-----------
!     APPLY WEIGHTED MODIFIER VALUES.
!-----------
      WTCR = .25
      WTRH = 1.0 - WTCR
      HTGMOD = WTCR*HGMDCR + WTRH*HGMDRH
!----------
!    MULTIPLIED BY SCALE TO CHANGE FROM A YR. PERIOD TO FINT AND
!    MULTIPLIED BY XHT TO APPLY USER SUPPLIED GROWTH MULTIPLIERS.
!----------
!
      IF(DEBUG) THEN
        WRITE(JOSTND,*)' IN HTGF, I= ',I,' ISPC= ',ISPC,'HTGMOD= ', &
        HTGMOD,' ICR= ',ICR(I),' HGMDCR= ',HGMDCR
        WRITE(JOSTND,*)' HT(I)= ',HT(I),' AVH= ',AVH,' RELHT= ',RELHT, &
       ' HGMDRH= ',HGMDRH
      ENDIF
!
      IF (HTGMOD .GE. 2.0) HTGMOD= 2.0
      IF (HTGMOD .LE. 0.0) HTGMOD= 0.1

!     XMOD=1.0
!      CRATIO=ICR(I)/100.0
!      RELHT=H/AVH
!      IF(RELHT .GT. 1.0)RELHT=1.0
!      IF(PCCF(ITRE(I)) .LT. 100.0)RELHT=1.0
!      XMOD = 1.117148 * (1.0-EXP(-4.26558 * CRATIO))
!     &        *(EXP(2.54119 * (RELHT**0.250537 -1.0)))
 1322  HTG(I) = POTHTG * HTGMOD
!
      IF(DEBUG)WRITE(JOSTND,901)ICR(I),PCT(I),BA,DG(I),HT(I), &
       POTHTG,AVH,HTG(I),PCCF(ITRE(I)),ABIRTH(I),HGUESS,HTGMOD
  901 FORMAT(' HTGF',I5,13F9.2)
!-----------
!  HEIGHT GROWTH GETS EVALUATED FOR EACH TREE EACH CYCLE. HTG GETS
!  MULTIPLIED BY SCALE TO CHANGE FROM A YR  PERIOD TO FINT AND
!  MULTIPLIED BY XHT AND HTCON TO APPLY USER SUPPLIED GROWTH MULTIPLIERS.
!----------
      TEMPH=H + HTG(I)
      IF(TEMPH .GT. HTMAX2) HTG(I)=HTMAX2-H
      IF(HTG(I) .LT. 0.1) HTG(I)=0.1
      HTG(I)=SCALE*XHT*HTG(I)*EXP(HTCON(ISPC))
      IF(DEBUG)WRITE(JOSTND,*)' I,ISPC,TEMPH,TEMPD,DBH,DG,H,HTG,MAP,', &
      'HTMAX2,SCALE,XHT,HTCON= ',I,ISPC,TEMPH,TEMPD,DBH(I),DG(I),H, &
      HTG(I),MAPPHD,HTMAX2,SCALE,XHT,HTCON(ISPC)
!
  161 CONTINUE
!----------
!    APPLY DWARF MISTLETOE HEIGHT GROWTH IMPACT HERE,
!    INSTEAD OF AT EACH FUNCTION IF SPECIAL CASES EXIST.
!----------
      HTG(I)=HTG(I)*MISHGF(I,ISPC)
      TEMHTG=HTG(I)
!----------
! CHECK FOR SIZE CAP COMPLIANCE.
!----------
      IF((HT(I)+HTG(I)).GT.SIZCAP(ISPC,4))THEN
        HTG(I)=SIZCAP(ISPC,4)-HT(I)
        IF(HTG(I) .LT. 0.1) HTG(I)=0.1
      ENDIF
!
      IF(.NOT.LTRIP) GO TO 30
      ITFN=ITRN+2*I-1
      HTG(ITFN)=TEMHTG
!----------
! CHECK FOR SIZE CAP COMPLIANCE.
!----------
      IF((HT(ITFN)+HTG(ITFN)).GT.SIZCAP(ISPC,4))THEN
        HTG(ITFN)=SIZCAP(ISPC,4)-HT(ITFN)
        IF(HTG(ITFN) .LT. 0.1) HTG(ITFN)=0.1
      ENDIF
!
      HTG(ITFN+1)=TEMHTG
!----------
! CHECK FOR SIZE CAP COMPLIANCE.
!----------
      IF((HT(ITFN+1)+HTG(ITFN+1)).GT.SIZCAP(ISPC,4))THEN
        HTG(ITFN+1)=SIZCAP(ISPC,4)-HT(ITFN+1)
        IF(HTG(ITFN+1) .LT. 0.1) HTG(ITFN+1)=0.1
      ENDIF
!
      IF(DEBUG) WRITE(JOSTND,9001) HTG(ITFN),HTG(ITFN+1)
 9001 FORMAT( ' UPPER HTG =',F8.4,' LOWER HTG =',F8.4)
!----------
!   END OF TREE LOOP
!----------
   30 CONTINUE
!----------
!   END OF SPECIES LOOP
!----------
   40 CONTINUE
!
      IF(DEBUG)WRITE(JOSTND,60)ICYC
   60 FORMAT(' LEAVING SUBROUTINE HTGF   CYCLE =',I5)
      RETURN
!
      ENTRY HTCONS
!----------
!  ENTRY POINT FOR LOADING HEIGHT INCREMENT MODEL COEFFICIENTS THAT
!  ARE SITE DEPENDENT AND REQUIRE ONE-TIME RESOLUTION.  HGHC
!  CONTAINS HABITAT TYPE INTERCEPTS, HGLDD CONTAINS HABITAT
!  DEPENDENT COEFFICIENTS FOR THE DIAMETER INCREMENT TERM, HGH2
!  CONTAINS HABITAT DEPENDENT COEFFICIENTS FOR THE HEIGHT-SQUARED
!  TERM, AND HGHC CONTAINS SPECIES DEPENDENT INTERCEPTS.  HABITAT
!  TYPE IS INDEXED BY ITYPE (SEE /PLOT/ COMMON AREA).
!----------
!  LOAD OVERALL INTERCEPT FOR EACH SPECIES.
!----------
      DO 50 ISPC=1,MAXSP
      HTCON(ISPC)=0.0
      IF(LHCOR2 .AND. HCOR2(ISPC).GT.0.0) HTCON(ISPC)= &
          HTCON(ISPC)+ALOG(HCOR2(ISPC))
   50 CONTINUE
!
      RETURN
      END