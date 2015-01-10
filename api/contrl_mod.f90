module contrl_mod
    use prgprm_mod
    implicit none
!CODE SEGMENT CONTRL
!----------
!  **CONTRL   DATE OF LAST REVISION:  07/12/13
!----------
      CHARACTER*160 TREFMT
      CHARACTER*250 KWDFIL
      CHARACTER*4 IUSED(MAXSP)
      CHARACTER*10 NAMGRP(10)
      LOGICAL LDUBDG,LRCOR2,LFLAG,LDCOR2,LDGCAL(MAXSP),LHTDRG(MAXSP), &
			LMORT,LSITE,LFIRE,                                        &
			LSTART,LSUMRY,LTRIP,MORDAT,NOTRIP,LBKDEN,LAUTON,LCVOLS,   &
			LBVOLS,LFIA,LZEIDE
	  logical :: nofortype=.true.
      INTEGER ICCODE,ICFLAG,ICL1,ICL2,ICL3,ICL4,ICL5,ICL6,ICYC,       &
			IDG,IFST,INS(6),IBEGIN(MAXSP),IREAD,IREC1,IREC2,          &
			IRECNT,IRECRD,IREF(MAXSP),ISCT(MAXSP,2),ISTDAT,ITRN,      &
			KPTR(MAXSP),IY(MAXCY1),JOLIST,JOSTND,JOSUM,JOTREE,JOCALB, &
			KOUNT(MAXSP),NSTKNT,NCYC,NUMSP,LSTKNT,METHC(MAXSP),       &
			METHB(MAXSP),ITABLE(7),ISPGRP(10,52),NSPGRP,              &
			ITHNPI,ITHNPN,ITHNPA
      REAL    AUTMAX,AUTMIN,AUTEFF,DGSD,EFF,TRM,FINTM,BAMAX,D0MULT,   &
			D0,TOPD(MAXSP),DBHMIN(MAXSP),STMP(MAXSP),TCWT,CFMIN,      &
			BFMIN,BAMIN,RCOR2(MAXSP),FRMCLS(MAXSP),SPCLWT,YR,TCFMIN,  &
			SIZCAP(MAXSP,4),PBAWT,PCCFWT,PTPAWT,DR016,DBHSTAGE,       &
			DBHZEIDE,DBHSDI
      COMMON /CONCHR/ TREFMT,IUSED,KWDFIL,NAMGRP
      COMMON /CONTRL/ AUTMAX,AUTMIN,AUTEFF,DGSD,EFF,TRM,YR,ICCODE, &
			ICFLAG,ICL1,ICL2,ICL3,ICL4,ICL5,ICL6,FINTM,            &
			ICYC,IDG,IFST,INS,IBEGIN,IREAD,IREC1,IREC2,            &
			IRECNT,IRECRD,IREF,ISCT,ISTDAT,ITRN,KPTR,IY,           &
			JOLIST,JOSTND,JOSUM,JOTREE,JOCALB,KOUNT,LDCOR2,        &
			LDGCAL,LHTDRG,LDUBDG,LFLAG,LSTART,LSTKNT,LMORT,        &
			LSUMRY,NSTKNT,LTRIP,MORDAT,NCYC,NOTRIP,NUMSP,          &
			BAMAX,D0MULT,D0,LBKDEN,LAUTON,TOPD,DBHMIN,STMP,        &
			TCWT,CFMIN,BFMIN,BAMIN,LRCOR2,RCOR2,FRMCLS,METHC,      &
			METHB,LBVOLS,LCVOLS,LSITE,ITABLE,SPCLWT,LFIA,          &
			TCFMIN,LFIRE,SIZCAP,ISPGRP,NSPGRP,PBAWT,PCCFWT,        &
			PTPAWT,ITHNPI,ITHNPN,ITHNPA,LZEIDE,DR016,              &
			DBHSTAGE,DBHZEIDE,DBHSDI
!----------
!  DEFINITIONS OF VARIABLES IN 'CONTRL' COMMON BLOCK:
!----------
!
!    AUTMAX -- UPPER LIMIT FOR AUTOMATIC THINNING FORMULA. DEFAULTED
!              TO 60 IN **GRINIT**.
!    AUTMIN -- LOWER LIMIT FOR AUTOMATIC THINNING FORMULA. DEFAULTED
!              TO 45 IN **GRINIT**.
!    AUTEFF -- CUTTING EFFICIENCE FOR AUTOMATIC THINNING. DEFAULTED
!              TO CURRENT VALUE OF CUTEFF IN **INITRE**.
!     BAMAX -- MAXIMUM ATTAINABLE BASAL AREA BY HABITAT TYPE. DEFAULT
!              SET IN GRINIT & SITSET, IF NOT SET BY KEYWORD.
!     BAMIN -- LIKE CFMIN EXCEPT IN BASAL AREA PER ACRE.
!     BFMIN -- LIKE CFMIN EXCEPT IN BOARD FEET PER ACRE.
!     CFMIN -- MINIMUM ACCEPTABLE HARVEST AS MEASURED IN MERCH CUFT.
!              PRE ACRE. DEFAULTS TO 0 IN GRINIT; CAN BE RESET BY
!              KEYWORD.
!    DBHMIN -- CARRIES MIN TOP DIAMETERS FOR MERCHANTABLE CUBIC VOL.
!              EQUATIONS BY SPECIES.  DEFAULTED TO 6 IN. FOR LP,
!              AND 7 IN FOR ALL OTHER SP. IN GRINIT CAN BE RESET BY
!              KEYWORD.
!      DGSD -- BOUNDS OF THE VARIANCE ABOUT PREDICTED DIAMETER
!              GROWTH IN NUMBER OF STA DEVIATIONS.  SET IN **INITRE**
!       EFF -- CUTTING EFFECTIVENESS (MAXIMUM PROPORTION OF A TREE
!              RECORD REMOVED IN ANY THINNING).  SET TO 0.98 IN
!              **INITRE**, CAN BE RESET BY KEYWORD.
!    IBEGIN -- POINTS TO THE FIRST PROJECTABLE TREE RECORD IN THE
!              TREE ARRAYS BY SPECIES.  USED IN CHAIN SORTING IN
!              **LNKINT** AND **LNKCHN**
!    ICCODE -- ERROR OR WARNING CONDITION CODE. INITIALIZED IN **BLOCK
!              DATA** SET IN **ERRGRO**.
!    ICFLAG -- INDEX TO CUTTING ALGORITHM IN CURRENT CYCLE:
!              1=NOTHIN; 2=AUTOMATIC THINNING; 3=T/A FROM BELOW;
!              4=T/A FROM ABOVE;5=BA FROM BELOW; 6=BA FROM ABOVE;
!              7=PRESCRIPTION THINNING; 8= DIAMETER LIMIT FROM
!              ABOVE; 9=DIAMETER LIMIT FROM BELOW
!      ICL1 -- CONTROL VARIABLE USEFUL FOR SPECIAL PURPOSES.
!      ICL2 -- CONTROL VARIABLE USEFUL FOR SPECIAL PURPOSES.
!      ICL3 -- CONTROL VARIABLE USEFUL FOR SPECIAL PURPOSES.
!      ICL4 -- CONTROL VARIABLE USEFUL FOR SPECIAL PURPOSES. SEE GRINIT
!      ICL5 -- USED TO PASS THE ACTUAL HABITAT CODE READ TO RCON.
!      ICL6 -- USED BY **INITRE** TO SAVE AND PASS THE ADDFILE DATASET
!              REFERENCE NUMBER. SET ZERO IN **GRINIT**, INDECATING THAT
!              NO ADDFILE WAS SPECIFIED. THEN USED TO SIGNAL DISPLY THAT
!              PROJECTION IS OVER. USED TO PASS THE DIAMETER
!              THE GROWTH MEASUREMENT PERIOD FROM **DGDRIV**
!              TO THE TUSSOCK AND MOUNTAIN PINE BEETLE ROUTINES.
!      ICUT -- ARRAY CONTAINING CUTTING ALGORITHM SELECTIONS CODED
!              AS ABOVE.  KEYWORD CONTROLLED.
!      ICYC -- INDEX TO CURRENT CYCLE.  VALUE SET IN **MAIN**.
!       IDG -- INDEX TO METHOD BY WHICH INPUT DIAMETER GROWTH
!              WAS MEASURED:
!              0=PREVIOUS DIAMETER INCREMENT MEASUREMENT.
!              1=PREVIOUS DIAMETER MEASUREMENT (DG IS ASSUMED TO
!                CARRY THIS DIAMETER).
!              2=SUBSEQUENT DIAMETER INCREMENT MEASUREMENT.
!              3=SUBSEQUENT DIAMETER MEASUREMENT (DG IS ASSUMED TO
!                CARRY THIS DIAMETER).
!      IFST -- IF IFST IS EQUAL TO 1, THE SUBSCRIPTS IN THE ARRAY
!              INS WILL BE RELOADED IN THE NEXT CALL TO **DIST**.
!       INS -- SUBSCRIPTS FOR DISPLAYED TREE RECORDS.  SET IN
!              CALL TO **DIST** WHEN IFST IS 1.
!     IREAD -- LOGICAL UNIT NUMBER FOR KEYWORD INPUT.  SET IN
!              **BLOCK DATA**
!     IREC1 -- NUMBER OF PROJECTABLE RECORDS INPUT.  CALCULATED IN
!              **INTREE**
!     IREC2 -- (1351-IREC2)=NUMBER OF INPUT RECORDS WHICH DIED IN
!              LAST 5 YEARS.  SET IN **INTREE**
!    IRECNT -- NUMBER OF KEYWORD AND PARAMETER CARDS READ.
!    IRECRD -- NUMBER OF TREE DATA RECORDS READ.  COMPUTED IN
!              **INTREE**.
!      IREF -- INDEX TO ORDER OF OCCURRENCE OF SPECIES IN SPECIES
!              ORDERED SORT LIST (IND1).  SET IN **INTREE**.
!      ISCT -- INDEX TO FIRST AND LAST OCCURRENCE OF A SPECIES IN
!              SPECIES ORDERED SORT LIST (IND1). SET IN **SETUP**
!    ISPGRP -- ARRAY HOLDING SPECIES THAT HAVE BEEN DEFINED AS A GROUP.
!              FIRST ELEMENT IS THE SPECIES GROUP NUMBER 1-10.
!              SECOND ELEMENT:
!               1 = NUMBER OF SPECIES IN THE GROUP (2-50).
!               2 = NUMERIC SEQUENCE NUMBER OF THE FIRST SPECIES.
!               3 = NUMERIC SEQUENCE NUMBER OF THE SECOND SPECIES.
!               ...
!              51 = NUMERIC SEQUENCE NUMBER OF THE FIFTIETH SPECIES.
!              52 = NUMBER OF CHARACTERS IN THE GROUP NAME.
!    ISTDAT -- LOGICAL UNIT NUMBER FOR TREE RECORD INPUT.  KEYWORD
!              CONTROLLED.
!    ITABLE -- ARRAY THAT HOLDS FLAGS WHICH INDICATE WHICH TABLES IN
!              PROGNOSIS MODEL OUTPUT WILL BE SUPPRESSED.
!    ITHNPA -- POINT THINNING ATTRIBUTE:
!              1=TPA, 2=BA, 3=SDI, 4=%CC
!    ITHNPI -- POINT THINNING INDICATOR:
!              1 = USE POINT NUMBERS FROM THE INPUT DATA
!              2 = USE FVS SEQUENTIAL POINT NUMBERS
!    ITHNPN -- POINT NUMBER THE CURRENT POINT THINNING APPLIES TO
!      ITRN -- CURRENT NUMBER OF TREE RECORDS, INCLUDING TRIPLES.
!              SET IN **SETUP**, UPDATED IN **TRIPLE**.
!     IUSED -- CARRIES ALPHA CODES FOR SPECIES ENCOUNTERED IN
!              TREE LIST IN ORDER OF OCCURENCE.  SET IN **INTREE**.
!        IY -- IY(1)=INVENTORY DATE, IY(2)=ENDPOINT OF FIRST CYCLE,
!              IY(3)=ENDPOINT OF SECOND CYCLE,...,IY(MAXCY1)=ENDPOINT
!              OF FORTIETH CYCLE.  KEYWORD CONTROLLED.
!    JOCALB -- LOGICAL UNIT NUMBER FOR CALIBRATIONS STATISTICS. SET
!              IN **GRINIT**.
!    JOLIST -- LOGICAL UNIT NUMBER FOR SPECIAL TREE LIST OUTPUT.
!              LOADED IN **BLOCK DATA**.
!    JOSTND -- LOGICAL UNIT NUMBER FOR REGULAR STAND OUTPUT AND
!              DEBUG OUTPUT.  LOADED IN **BLOCK DATA**.
!     JOSUM -- LOGICAL UNIT NUMBER FOR 1 LINE/CYCLE SUMMARY OUTPUT.
!              LOADED IN **BLOCK DATA**.
!    JOTREE -- LOGICAL UNIT NUMBER FOR REGULAR TREE OUTPUT.  LOADED
!              IN **BLOCK DATA**.
!     KOUNT -- NUMBER OF RECORDS ENCOUNTERED IN READ LIST FOR EACH
!              SPECIES.  LOADED IN **INTREE**.
!    KWDFIL -- KEY WORD FILE NAME PREFIX (CONTAINS CHARS UP TO FIRST
!              PERIOD) LOADED IN FILOPN.
!    LAUTON -- IF TRUE, AUTOMATIC THINNING WILL BE INVODED IF NO
!              OTHER THINNINGS ARE REQUESTED.
!    LBKDEN -- IF TRUE, DIAMETERS WILL BE BACK DATED IN DENSE PRIOR
!              TO CALCULATION OF DENSITY STATISTICS DURING CALIBRATION.
!    LDEBUG -- INPUT FLAGS THAT CONTROL PRINTING OF DEBUG OUTPUT ON
!              A CYCLE BY CYCLE BASIS.  KEYWORD CONTROLLED.
!    LDCOR2 -- TRUE IF DIAMETER GROWTH CORRECTION TERMS (COR2)
!              ARE READ AND USED OR REUSED. SET FALSE IN GRINIT.
!              SET TRUE BY EITHER REUSCORD OR READCORD KEYWORDS.
!    LDGCAL -- TRUE IF DIAMETER GROWTH MODELS ARE TO BE CALIBRATED.
!              SET TRUE IN GRINIT; FALSE BY NOCALIB KEYWORD.
!    LDUBDG -- IF TRUE, MISSING DIAMETER INCREMENTS WILL BE DUBBED
!              IN FOLLOWING CALIBRATION.  SET FALSE IN **INITRE**.
!    LHTDRG -- SET BY SPECIES; INITIALLY IN **GRINIT**, BUT POSSIBLY
!              CHANGED VIA THE NOHTDREG KEYWORD IN **INITRE**. USED
!              IN CONJUNCTION WITH THE VARIABLE IABFLG CONTAINED IN
!              THE VARCOM COMMON BLOCK TO CONTROL WHICH HT-DBH MODEL
!              WILL BE USED.
!
!              IF FALSE, FVS WILL USE THE CURTIS-ARNEY HT-DBH MODEL IN
!              VARIANTS WHERE IT WAS FIT. IF THE VARIANT DOES NOT HAVE
!              C-A RELATIONSHIPS, FVS WILL USE THE WYKOFF HT-DBH
!              MODEL WITH THE DEFAULT COEFFICIENTS FOR THAT SPECIES.
!              IF LHTDRG IS FALSE, IABFLG WILL ALWAYS BE 1.
!
!              IF TRUE, AND IABFLG=0, FVS WILL USE THE WYKOFF MODEL WITH
!              COEFFICIENTS CALIBRATED TO THE INPUT DATA.
!              IF TRUE, AND IABFLG=1, FVS WILL USE THE C-A MODELS IN
!              VARIANTS THAT HAVE THEM, OR THE WYKOFF MODEL WITH THE
!              DEFAULT COEFFICIENTS IN VARIANTS THAT DON'T.
!
!              USED IN **CRATET** AND **REGENT**.
!      LFIA -- LOGICAL VARIABLE WHICH INDICATES INPUT DATA CONTAINS
!              FIA SPECIES CODES INSTEAD OF ALPHA SPECIES CODES
!     LFLAG -- SIGNALS INITRE TO WRITE A HEADING. OTHERWISE AVAILABLE.
!     LFIRE -- SET TO TRUE IN CVCBMS WHEN A SIMFIRE KEYWORD HAS BEEN
!              USED IN THE CURRENT CYCLE.  FUNCTIONS LIKE LTHIN
!              TO SIGNAL USE OF PAST CYCLE TPA, RMSQD AND BA IN
!              COVER MODEL CALCULATIONS. ALSO USED IN CCFCAL ROUTINES
!      LIST -- LOGICAL ARRAY CONTAINING CYCLE BY CYCLE INSTRUCTIONS
!              FOR PRINTOUT OF SPECIAL TREE LIST.  KEYWORD CONTROLLED.
!     LSITE -- LOGICAL VARIABLE WHICH INDICATES WHETHER SITECOD KEYWORD
!              HAS BEEN USED.
!    LSTART -- LOGICAL FLAG WHICH IS TRUE DURING SUMMARIZATION OF
!              INPUT.  ALLOWS DUBBING OF MISSING VALUES AND LOADING
!              OF COEFFICIENT ARRAYS.  SET AND UPDATED IN **MAIN**.
!    LSTKNT -- COUNT OF POINTS SAMPLED IN PREVIOUS STAND.  THIS
!              VARIABLE IS USED TO ALLOW NON-UNIQUE POINT ID'S
!              IN RECORDS FOR A GROUP OF STANDS WHICH ARE
!              COMBINED AND PROCESSED AS A SINGLE STAND.
!              CALCULATED IN **INTREE**.
!    LSUMRY -- IF TRUE, ONE LINE SUMMARY OUTPUT IS PRINTED (OR
!              PUNCHED) EACH CYCLE.  DEFAULTED TRUE IN **INITRE**,
!              CAN BE SET FALSE BY KEYWORD.
!     LTRIP -- IF LTRIP IS TRUE, RECORDS WILL BE TRIPLED THIS
!              CYCLE.  SET TRUE IN **MAIN** IF NOTRIP IS FALSE,
!              ICYC IS 2 OR LESS, AND ITRN IS 450 OR LESS.
!    MORDAT -- SET TO TRUE ON FIRST CALL TO **INTREE**.  THIS
!              FLAG IS USED TO DETECT GROUPS OF STANDS WHICH
!              ARE TO BE PROCESSED AS A SINGLE STAND AND IS
!              ALSO USED TO ASSURE THAT TREE RECORDS ARE READ
!              (IF PROCESS KEYWORD IS ENCOUNTERED PRIOR TO
!              TREE RECORD READ, **INTREE** IS AUTOMATICALLY
!              CALLED AND A WARNING MESSAGE IS PRINTED).
!    NAMGRP -- ARRAY HOLDING THE NAMES OF DEFINED SPECIES GROUPS.
!      NCYC -- NUMBER OF CYCLES TO BE PROJECTED.  KEYWORD CONTROLLED.
!    NOTRIP -- IF TRUE, TRIPLING IS BYPASSED.  SET FALSE IN
!              **INITRE**.  CAN BE RESET TO TRUE BY KEYWORD.
!    NSPGRP -- NUMBER OF SPECIES GROUPS THAT HAVE BEEN DEFINED.
!    NSTKNT -- NUMBER OF NON-STOCKABLE POINTS IN THE STAND.  CAN BE
!              INPUT BY KEYWORD; IF NOT, COUNTED WHEN TREE RECORDS
!              ARE READ.
!     NUMSP -- NUMBER OF SPECIES ENCOUNTERED IN TREE LIST.  SET IN
!              **INTREE**.
!     PBAWT -- WEIGHT GIVEN TO POINT BASAL AREA IN CALCULATION OF
!              REMOVAL PRIORITY FOR THINNING. DEFALUTS TO 0 IN **GRINIT**.
!              CAN BE RESET BY TCONDMLT KEYWORD.
!    PCCFWT -- WEIGHT GIVEN TO POINT CCF IN CALCULATION OF
!              REMOVAL PRIORITY FOR THINNING. DEFALUTS TO 0 IN **GRINIT**.
!              CAN BE RESET BY TCONDMLT KEYWORD.
!    PTPAWT -- WEIGHT GIVEN TO POINT TPA IN CALCULATION OF
!              REMOVAL PRIORITY FOR THINNING. DEFALUTS TO 0 IN **GRINIT**.
!              CAN BE RESET BY TCONDMLT KEYWORD.
!    SIZCAP -- ARRAY TO CARRY SIZE LIMITS FOR TREES:
!              FIRST ELEMENT IS THE MAXIMUM DBH
!              SECOND ELEMENT IS THE SPECIFIED MORTALITY RATE (0-1)FOR TREES
!              WITH DIAMETERS ABOVE THE CAP. THIS CAN BE USED AS A SURROGATE
!              FOR SENESCENCE MORTALITY WHERE SIZE IS USED INSTEAD OF AGE.
!              THIRD ELEMET IS THE DBH USE FLAG: 0 USE FOR BOTH MORTALITY
!              AND DIAMETER GROWTH; 1 USE FOR DIAMETER GROWTH ONLY;
!              2 USE FOR MORTALITY ONLY
!              FOURTH ELEMENT IS THE MAXIMUM HEIGHT
!    SPCLWT -- WEIGHT GIVEN TO SPECIAL TREE STATUS CODE IN
!              CALCULATION OF REMOVAL PRIORITY FOR THINNING. DEFAULT
!              TO 0 IN **GRINIT**. CAN BE RESET BY TCONDMLT KEYWORD.
!      STMP -- CARRIES STUMP HEIGHTS FOR MERCHANTABLE CUBIC VOLUME
!              EQUATIONS BY SPECIES. DEFAULTS TO 1.0 IN GRINIT;
!              CAN BE RESET BY KEYWORD.
!    TCFMIN -- LIKE CFMIN EXCEPT IN TOTAL CUBIC FEET PER ACRE.
!      TCWT -- WEIGHT GIVEN TO TREE CONDITION CLASS IN CALCULATION
!              OF REMOVAL PRIORITY FOR THINNING.  DEFAULT TO 0 IN
!              **GRINIT**. CAN BE RESET BY TCONDMLT KEYWORD.
!      TOPD -- CARRIES TOP DIAMETERS FOR MERCHANTABLE CUBIC VOLUME
!              EQUATIONS BY SPECIES. DEFAULTS TO 4.5 IN GRINIT. CAN
!              BE RESET BY KEYWORD.
!    TREFMT -- FORMAT FOR READING TREE RECORDS.  DEFAULTED IN
!              **BLOCK DATA**, CAN BE REPLACED BY KEYWORD.
!       TRM -- TREE RECORD MULTIPLIER.  SCALES THE PROB OF A BASE
!              RECORD TO REMOVE IMPACT OF TRIPLING WHEN DISPLAYING
!              TREES.  INITIALIZED IN **INITRE**, UPDATED IN **TRIPLE**
!        YR -- PERIOD LENGTH FOR GROWTH MEASUREMENTS WHICH MODELS
!              WERE FIT TO.  INITIALIZED IN **BLOCK DATA**.
!    LZEIDE -- = FALSE BY DEFAULT, AND IF SET TO TRUE USING
!              THE SDICALC KEYWORD, THE ZIEDE SUMMATION METHOD WILL
!              BE USED TO CALCULATE MORTALITY IN VARIANTS USING
!              THE SDI-BASED MORTALITY ALGORITHM
!    DR016  -- REINEKE DIAMETER CALCULATED USING ZEIDE METHOD,
!              CALCULATED IN DENSE AND USED IN SDICHK
!  DBHSTAGE -- MINIUM DIAMETER USED IN ZEIDE SDI CALCULAIONS, SET
!              BY SDICALC KEYWORD, DEFAULT=0.
!  DBHZEIDE -- MINIUM DIAMETER USED IN ZEIDE SDI CALCULAIONS, SET
!              BY SDICALC KEYWORD, DEFAULT=0.
!    DBHSDI -- THE BREAKPOINT FOR EXCLUDING TREES IN SDI-BASED MORTALITY
!              CALCULATIONS. TREES WITH DBH < DBHSDI WILL NOT BE INCLUDED
!              IN SDI-BASED MORTALITY.
!              CALCULATIONS
!
!-----END SEGMENT

end module contrl_mod
