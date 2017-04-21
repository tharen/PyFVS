module organon_mod
    implicit none
!CODE SEGMENT ORGANON
!----------
!  **ORGANON      DATE OF LAST REVISION:  05/18/2015
!----------

      LOGICAL   LORGANON,LORGVOLS,LORGPREP

      INTEGER*4 CYCLG,VERSION,NPTS,NTREES1,STAGE,BHAGE,TREENO(2000), &
            PTNO(2000),SPECIES(2000),USER(2000),INDS(30), &
            PRAGE(2000,3),BRCNT(2000,3),BRHT(2000,40), &
            BRDIA(2000,40),JCORE(2000,40),NPR(2000),SERROR(35), &
            TERROR(2000,6),SWARNING(9),TWARNING(2000), &
            NTREES2,IERROR,IORG(2000)
      INTEGER ITEST

      REAL*4 DBH1(2000),HT1OR(2000),CR1(2000),EXPAN1(2000), &
            SCR1B(2000),SITE_1,SITE_2,ACALIB(3,18),MSDI_1,MSDI_2, &
            MSDI_3,PN(5),YSF(5),BABT,BART(5),YST(5),PRLH(2000,3), &
            PRDBH(2000,3),PRHT(2000,3),PRCR(2000,3),PREXP(2000,3), &
            MGEXP(2000),DGRO(2000),HGRO(2000),MORTEXP(2000), &
            CRCHNG(2000),SCRCHNG(2000),STOR(30),RVARS(30), &
            DBH2(2000),HT2OR(2000),CR2(2000),EXPAN2(2000),SCR2B(2000), &
            LOGTA,LOGML,LOGLL

!----------
!  DEFINITIONS OF VARIABLES IN 'ORGANON' COMMON BLOCK:
!----------
!  declared but not in ORGANON.F77 list (so I took them out)
!        INTEGER*4 
!       4          NXT,BNXT,
!       5          ONXT,IB,BIG6,OTHER,NSPN,TDATAI(2000,3),SPGRP(2000),
!       6          TCYCLE,FCYCLE,
!  
!        REAL*4 
!       2       PDEN,
!       3       OLD,
!
!  variable       declaration    dimension      description
!
!  ACALIB,         R * 4         (3,18)         ARRAY OF DIAM/HEIGHT/CROWN RECESSION CALIBRATION VALUES
!  BABT,           R * 4                        BASAL AREA REMOVED AFTER LAST THINNING ARRAY
!  BART,           R * 4         (5)            RESIDUAL BASAL AREA AFTER THINNING ARRAY
!  BHAGE,          I * 4                        BREAST HEIGHT AGE FOR EVEN-AGED STANDS
!  BRCNT,          I * 4         (2000,3)       WOOD QUALITY - BRANCH COUNT OF TYPE J FOR TREE I
!  BRDIA,          I * 4         (2000,40)      BRANCH DIAMETER AT THE END OF THE CYCLE
!  BRHT,           I * 4         (2000,40)      WOOD QUALITY - HEIGHT OF BRANCH J FOR TREE I
!  CR1,            R * 4         (2000)         STARTING CROWN RATIO FOR THE TREE RECORD
!  CR2,            R * 4         (2000)         ENDING CROWN RATIO FOR TREE RECORD I
!  CRCHNG,         R * 4         (2000)         CHANGE IN CROWN RATIO FOR TREE I
!  CYCLG,          I * 4                        NUMBER OF CYCLES PROJECTED
!  DBH1,           R * 4         (2000)         STARTING DIAMETER AT BREAST HEIGHT, IN INCHES
!  DBH2,           R * 4         (2000)         ENDING DIAMETER AT BREAST HEIGHT FOR TREE I
!  DGRO,           R * 4         (2000)         DIAMETER GROWTH FOR TREE I, IN INCHES
!  EXPAN1,         R * 4         (2000)         INITIAL TREES/ACRE FOR TREE RECORD I
!  EXPAN2,         R * 4         (2000)         ENDING EXPANSION FACTOR FOR TREE RECORD I
!  HGRO,           R * 4         (2000)         HEIGHT GROWTH INCREMENT FOR TREE I, IN FEET
!  HT1OR,          R * 4         (2000)         STARTING TOTAL HEIGHT, IN FEET
!  HT2OR,          R * 4         (2000)         ENDING TOTAL HEIGHT FOR TREE I
!  IERROR,         I * 4                        INDICATOR FOR ERRORS AT THE END OF THE CYCLE
!  INDS,           I * 4         (30)           STAND LEVEL INDICATOR VARIABLES/SWITCHES
!  IORG,           I * 4         (2000)         FLAG INDICATING A VALID ORGANON TREE (0 = NO, 1 = YES)
!  ITEST,          I                            DEFAULT 0; SET TO 1 IF STAND IS EVEN-AGED BUT STAND AGE WAS
!                                               NOT ENTERED. THIS IS AN ERROR CONDITION IN ORGANON
!  JCORE,          I * 4         (2000,40)      DIAMETER OF JUVINILE COREWOOD AT THE END OF THE CYCLE
!  LOGLL,          R * 4                        TARGET LOG LENGTH, IN FEET
!  LOGML,          R * 4                        MINIMUM LOG LENGTH, IN FEET
!  LOGTA,          R * 4                        LOG TRIM, IN INCHES
!  LORGANON,       L                            LOGICAL SWITCH TO SIGNAL THAT ORGANON EXTENSION IS PRESENT
!  LORGPREP        L                            LOGICAL SWITCH TO SIGNAL THAT PREPARE HAS BEEN CALLED
!  LORGVOLS,       L
!  MGEXP,          R * 4         (2000)         NUMBER OF TREES THAT DIED DURING THE CYCLE
!  MORTEXP,        R * 4         (2000)         CHANGE IN EXPANSION FACTOR FOR TREE I, IN TREES/ACRE
!  MSDI_1          R * 4                        MAXIMUM SDI FOR DOUGLAS-FIR
!  MSDI_2          R * 4                        MAXIMUM SDI FOR PONDEROSA PINE
!  MSDI_3          R * 4                        MAXIMUM SDI FOR WF/GF
!  NPR,            I * 4         (2000)         NUMBER OF PRUNINGS FOR TREE I
!  NPTS,           I * 4                        THE NUMBER OF POINTS IN THE SAMPLE TREE LIST (TREE LIST)
!  NTREES1,        I * 4                        THE NUMBER OF TREE RECORDS IN THE TREE LIST
!  NTREES2,        I * 4                        TOTAL NUMBER OF TREE PER ACRE FOR TREE I AT END OF CYCLE
!  PN,             R * 4         (5)            ARRAY OF POUNDS OF NITROGEN FERTILIZER
!  PRAGE,          I * 4         (2000,3)       AGE OF THE I-TH PRUNING
!  PRCR,           R * 4         (2000,3)       CROWN RATIO OF THE I-TH TREE AT PRUNING J
!  PRDBH,          R * 4         (2000,3)       DBH OF THE J-TH PRUNING FOR TREE I
!  PREXP,          R * 4         (2000,3)       POINT LEVEL EXPANSION FACTOR OF THE I-TH TREE AT PRUNING J
!  PRHT,           R * 4         (2000,3)       TOTAL HEIGHT OF THE I-TH TREE AT PRUNING J
!  PRLH,           R * 4         (2000,3)       LIFT HEIGHT OF THE J-TH PRUNING FOR TREE I
!  PTNO,           I * 4         (2000)         PLOT/POINT NUMBER
!  RVARS,          R * 4         (30)           ARRAY OF FLOATING-POINT (REAL) VARIABLES FOR THE STAND
!  SCR1B,          R * 4         (2000)         SHADOW CROWN RATIO AT THE BEGINGING OF THE GROWTH CYCLE
!  SCR2B,          R * 4         (2000)         ENDING SHADOW CROWN RATIO FOR TREE RECORD I
!  SCRCHNG,        R * 4         (2000)         CHANGE IN SHADOW CROWN RATIO FOR TREE I
!  SERROR,         I * 4         (35)           STAND LEVEL ERRORS AT THE END OF THE PROJECTION
!  SITE_1,         R * 4                        FIRST SITE INDEX FOR THE VARIANT
!  SITE_2,         R * 4                        SECOND SITE INDEX VARIABLE FOR THE VARIANT
!  SPECIES,        I * 4         (2000)         FIA SPECIES CODE
!  STAGE,          I * 4                        THE STAND AGE FOR EVEN AGED STANDS
!  STOR,           R * 4         (30)           ARRAY OF INTERNAL VARIABLES THAT MUST NOT BE CHANGED OVER THE RUNS OF ORGANON
!  SWARNING,       I * 4         (9)            STAND LEVEL WARNINGS AT THE END OF THE PROJECTION
!  TERROR,         I * 4         (2000,6)       TREE LEVEL ERRORS AT THE END OF THE PROJECTION
!  TREENO,         I * 4         (2000)         TREE NUMBER ON THE PLOT
!  TWARNING,       I * 4         (2000)         TREE LEVEL WARNINGS AT THE END OF THE PROJECTION
!  USER,           I * 4         (2000)         USER CODE
!  VERSION,        I * 4                        THE VARIANT NUMBER OF ORGANON TO PROJECT (1=SWO,2=NWO,3=SMC)
!  YSF,            R * 4         (5)            YEARS SINCE LAST FERTILIZATION
!  YST,            R * 4         (5)            YEARS SINCE LAST THINNING ARRAY
!
!
!  
!  INDS(1)   0 = NO CALIB HT   (CALH=.FALSE.)                 1 = CALIB HT  (CALH=.TRUE.)
!  INDS(2)   0 = NO CALIB CR   (CALC=.FALSE.)                 1 = CALIB CR  (CALC=.TRUE.)
!  INDS(3)   0 = NO CALIB D    (CALD=.FALSE.)                 1 = CALIB D   (CALD=.TRUE.)
!  INDS(4)   0 = UNEVEN-AGED   (EVEN=.FALSE.)                 1 = EVEN-AGED (EVEN=.TRUE.)
!  INDS(5)   0 = NO TRIPLE     (TRIPLE=.FALSE.)               1 = TRIPLE    (TRIPLE=.TRUE.)
!  INDS(6)   0 = NO PRUNING    (PRUNE=.FALSE.)                1 = PRUNE     (PRUNE=.TRUE.)
!  INDS(7)   0 = NO THINNING   (THIN=.FALSE.)                 1 = THIN      (THIN=.TRUE.)
!  INDS(8)   0 = NO FERTILIZER (FERT=.FALSE.)                 1 = FERTILIZE (FERT=.TRUE.)
!  INDS(9)   0 = NO ADDITIONAL MORTALITY (MORT=.FALSE.)       1 = USE MAX SDI FOR ADDITIONAL MORTALITY (MORT=.TRUE.)
!  INDS(10)
!  INDS(11)  0 = OVERSTORY NOT REMOVED (OSTORY=.FALSE.)       1 = OVERSTORY REMOVED (OSTORY=.TRUE.)
!  INDS(12)  0 =               (INGRO=.FALSE.)                1 =           (INGRO=.TRUE.)
!  INDS(13)  0 =               (B6THIN=.FALSE.)               1 =           (B6THIN=.TRUE.)
!  INDS(14)  0 = NO GENETICS   (GENETICS=.FALSE.)             1 = GENETICS  (GENETICS=.TRUE.)
!  INDS(15)  0 = NO SWISS NC   (GENETICS=.FALSE.)             1 = SWISS NC  (GENETICS=.TRUE.)
!  INDS(16)
!  INDS(17)
!  INDS(18)
!  INDS(19)
!  INDS(20)
!  INDS(21)
!  INDS(22)
!  INDS(23)
!  INDS(24)
!  INDS(25)
!  INDS(26)
!  INDS(27)
!  INDS(28)
!  INDS(29)
!  INDS(30)
!
!    FVS      EXECUTE
!  RVARS(1)   SITE_1   THE DOUGLAS-FIR SITE INDEX
!  RVARS(2)   SITE_2   THE PP SITE INDEX (SWO); WH SITE INDEX (NWO,SMC)
!  RVARS(3)   MSDI_1   MAX SDI FOR DF IN SWO, NWO, AND SMC
!  RVARS(4)   MSDI_2   MAX SDI FOR WF/GF IN SWO (WF), NWO/SMC (GF)
!  RVARS(5)   MSDI_3   MAX SDI FOR PP/WH IN SWO (PP), NWO/SMC (WH)
!  RVARS(6)   GWDG     DF GENETIC WORTH VALUE FOR DIAMETER GROWTH
!  RVARS(7)   GWHG     DF GENETIC WORTH VALUE FOR HEIGHT GROWTH
!  RVARS(8)   FR       N YRS FOLIAGE RETENTION FOR DF (SWISS NEEDLE CAST)
!  RVARS(9)   PDEN     PLANTING DENSITY OF ALDER RAP VERSION
!  RVARS(10)
!  RVARS(11)
!  RVARS(12)
!  RVARS(13)
!  RVARS(14)
!  RVARS(15)
!  RVARS(16)
!  RVARS(17)
!  RVARS(18)
!  RVARS(19)
!  RVARS(20)
!  RVARS(21)
!  RVARS(22)
!  RVARS(23)
!  RVARS(24)
!  RVARS(25)
!  RVARS(26)
!  RVARS(27)
!  RVARS(28)
!  RVARS(29)
!  RVARS(30)
!
!
!    FVS      EXECUTE
!  STOR(1)    NO   
!  STOR(2)    RD0   
!  STOR(3)    A1   
!  STOR(4)    A2   
!  STOR(5)    A1MAX
!  STOR(6)    PA1MAX
!  STOR(7)    
!  STOR(8)    
!  STOR(9)    
!  STOR(10)
!  STOR(11)
!  STOR(12)
!  STOR(13)
!  STOR(14)
!  STOR(15)
!  STOR(16)
!  STOR(17)
!  STOR(18)
!  STOR(19)
!  STOR(20)
!  STOR(21)
!  STOR(22)
!  STOR(23)
!  STOR(24)
!  STOR(25)
!  STOR(26)
!  STOR(27)
!  STOR(28)
!  STOR(29)
!  STOR(30)
!
!-----END SEGMENT

end module organon_mod