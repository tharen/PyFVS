module findage_mod
    use prgprm_mod
    use contrl_mod, only : jostnd,fast_age_search
    use arrays_mod
    use plot_mod
    
    implicit none

    real :: age_steps(8),max_age(maxsp)

    ! FIXME: max_age could be a program level parameter
    data max_age / &
           200.,  110.,  180.,  130.,  250., &
           130.,  140.,  150.,  150.,  200., &
            200.,  180.,  200.,  200.,  200., &
            130.,  200.,  200.,  200.,  200., &
            200.,  200.,  200.,  200.,  200., &
            200.,  200.,  200.,  200.,  200., &
            180.,  200./
          
    ! Predefined binary search age increments
    data age_steps /0.5,0.25,0.125,0.0625,0.0313,0.0156,0.00781,0.00391/

    contains

      SUBROUTINE FINDAG(I,ISPC,D1,D2,H,SITAGE,SITHT,AGMAX1,HTMAX1, &
                        HTMAX2,DEBUG)
!----------
!  **FINDAG--EC  DATE OF LAST REVISION:  05/09/12
!----------
!  THIS ROUTINE FINDS EFFECTIVE TREE AGE BASED ON INPUT VARIABLE(S)
!  CALLED FROM **COMCUP
!  CALLED FROM **CRATET
!  CALLED FROM **HTGF

!  SITAGE  --  LOADED WITH EFECTIVE AGE BASED ON CURRENT H
!  SITHT   --  LOADED WITH POTENTIAL HEIGHT CORRESPONDING TO
!              AGE IN SITAGE ARRAY
!----------
!  DECLARATIONS
!----------
      LOGICAL DEBUG
      INTEGER I,ISPC
      REAL AGMAX(MAXSP),AHMAX(MAXSP),BHMAX(MAXSP),AGMAX1,HTMAX1,HTMAX2
      REAL AG,DIFF,H,HGUESS,HMAX,SINDX,TOLER,SITAGE,SITHT,D1,D2
!----------
!  SPECIES LIST FOR EAST CASCADES VARIANT.
!
!   1 = WESTERN WHITE PINE      (WP)    PINUS MONTICOLA
!   2 = WESTERN LARCH           (WL)    LARIX OCCIDENTALIS
!   3 = DOUGLAS-FIR             (DF)    PSEUDOTSUGA MENZIESII
!   4 = PACIFIC SILVER FIR      (SF)    ABIES AMABILIS
!   5 = WESTERN REDCEDAR        (RC)    THUJA PLICATA
!   6 = GRAND FIR               (GF)    ABIES GRANDIS
!   7 = LODGEPOLE PINE          (LP)    PINUS CONTORTA
!   8 = ENGELMANN SPRUCE        (ES)    PICEA ENGELMANNII
!   9 = SUBALPINE FIR           (AF)    ABIES LASIOCARPA
!  10 = PONDEROSA PINE          (PP)    PINUS PONDEROSA
!  11 = WESTERN HEMLOCK         (WH)    TSUGA HETEROPHYLLA
!  12 = MOUNTAIN HEMLOCK        (MH)    TSUGA MERTENSIANA
!  13 = PACIFIC YEW             (PY)    TAXUS BREVIFOLIA
!  14 = WHITEBARK PINE          (WB)    PINUS ALBICAULIS
!  15 = NOBLE FIR               (NF)    ABIES PROCERA
!  16 = WHITE FIR               (WF)    ABIES CONCOLOR
!  17 = SUBALPINE LARCH         (LL)    LARIX LYALLII
!  18 = ALASKA CEDAR            (YC)    CALLITROPSIS NOOTKATENSIS
!  19 = WESTERN JUNIPER         (WJ)    JUNIPERUS OCCIDENTALIS
!  20 = BIGLEAF MAPLE           (BM)    ACER MACROPHYLLUM
!  21 = VINE MAPLE              (VN)    ACER CIRCINATUM
!  22 = RED ALDER               (RA)    ALNUS RUBRA
!  23 = PAPER BIRCH             (PB)    BETULA PAPYRIFERA
!  24 = GIANT CHINQUAPIN        (GC)    CHRYSOLEPIS CHRYSOPHYLLA
!  25 = PACIFIC DOGWOOD         (DG)    CORNUS NUTTALLII
!  26 = QUAKING ASPEN           (AS)    POPULUS TREMULOIDES
!  27 = BLACK COTTONWOOD        (CW)    POPULUS BALSAMIFERA var. TRICHOCARPA
!  28 = OREGON WHITE OAK        (WO)    QUERCUS GARRYANA
!  29 = CHERRY AND PLUM SPECIES (PL)    PRUNUS sp.
!  30 = WILLOW SPECIES          (WI)    SALIX sp.
!  31 = OTHER SOFTWOODS         (OS)
!  32 = OTHER HARDWOODS         (OH)
!
!  SURROGATE EQUATION ASSIGNMENT:
!
!  FROM THE EC VARIANT:
!      USE 6(GF) FOR 16(WF)
!      USE OLD 11(OT) FOR NEW 12(MH) AND 31(OS)
!
!  FROM THE WC VARIANT:
!      USE 19(WH) FOR 11(WH)
!      USE 33(PY) FOR 13(PY)
!      USE 31(WB) FOR 14(WB)
!      USE  7(NF) FOR 15(NF)
!      USE 30(LL) FOR 17(LL)
!      USE  8(YC) FOR 18(YC)
!      USE 29(WJ) FOR 19(WJ)
!      USE 21(BM) FOR 20(BM) AND 21(VN)
!      USE 22(RA) FOR 22(RA)
!      USE 24(PB) FOR 23(PB)
!      USE 25(GC) FOR 24(GC)
!      USE 34(DG) FOR 25(DG)
!      USE 26(AS) FOR 26(AS) AND 32(OH)
!      USE 27(CW) FOR 27(CW)
!      USE 28(WO) FOR 28(WO)
!      USE 36(CH) FOR 29(PL)
!      USE 37(WI) FOR 30(WI)
!----------
!  DATA STATEMENTS
!----------
      DATA AGMAX/ &
           200.,  110.,  180.,  130.,  250., &
           130.,  140.,  150.,  150.,  200., &
            200.,  180.,  200.,  200.,  200., &
            130.,  200.,  200.,  200.,  200., &
            200.,  200.,  200.,  200.,  200., &
            200.,  200.,  200.,  200.,  200., &
            180.,  200./

      DATA AHMAX/ &
                  2.3,      12.86,      -2.86,      21.29,      52.27, &
                21.29,        2.3,        20.,      45.27,      -5.00, &
            4.0156013,      -2.06,  3.2412923,  3.2412923,  4.3149844, &
                21.29,  3.2412923,  3.2412923,  3.2412923,  3.9033821, &
            3.9033821,  3.9033821,  3.9033821,  3.9033821,  3.9033821, &
            3.9033821,  3.9033821,  3.9033821,  3.9033821,  3.9033821, &
                -2.06,  3.9033821/

      DATA BHMAX/ &
                 2.39,       1.32,       1.54,       1.24,       1.14, &
                 1.24,       1.75,        1.1,       1.24,       1.30, &
           51.9732476,       1.54, 62.7139427, 62.7139427, 39.6317079, &
                 1.24, 62.7139427, 62.7139427, 62.7139427, 59.3370816, &
           59.3370816, 59.3370816, 59.3370816, 59.3370816, 59.3370816, &
           59.3370816, 59.3370816, 59.3370816, 59.3370816, 59.3370816, &
                 1.54, 59.3370816/
!----------
!  INITIALIZATIONS
!----------
      TOLER=2.0
      SINDX = SITEAR(ISPC)
      AGMAX1 = AGMAX(ISPC)
      HTMAX1 = 0.0
      HTMAX2 = 0.0

      SELECT CASE (ISPC)
!----------
!  SPECIES USING EC LOGIC
!----------
      CASE(1:10,16)
        AG = 0.5
!----------
! THE FOLLOWING LINES ARE AN RJ FIX 7-28-88
!----------
        IF(ISPC .EQ. 10) AG=(98.38*EXP(SINDX*(-0.0422)))+1.0
        IF(AG .LT. 0.5)AG = 0.5
        IF(ISPC .EQ. 3)AG=18.0

        HTMAX1 = AHMAX(ISPC) + BHMAX(ISPC)*SINDX
      CASE(12,31)
        AG=0.5
        HTMAX1 = AHMAX(ISPC) + BHMAX(ISPC)*SINDX*3.281
!----------
!  SPECIES USING WC LOGIC
!----------
      CASE(11,13:15,17:30,32)
        AG=2.0
        HTMAX1=AHMAX(ISPC)*D1 + BHMAX(ISPC)
        HTMAX2=AHMAX(ISPC)*D2 + BHMAX(ISPC)
      END SELECT
!----------
!  CRATET CALLS FINDAG AT THE BEGINING OF THE SIMULATION TO
!  CALCULATE THE AGE OF INCOMING TREES.  AT THIS POINT ABIRTH(I)=0.
!  THE AGE OF INCOMING TREES HAVING H>=HMAX IS CALCULATED BY
!  ASSUMEING A GROWTH RATE OF 0.10FT/YEAR FOR THE INTERVAL H-HMAX.
!  TREES REACHING HMAX DURING THE SIMULATION ARE IDENTIFIED IN HTGF.
!----------
      IF(H .GE. HTMAX1) THEN
        SITAGE = AGMAX1 + (H - HTMAX1)/0.10
        SITHT = H
        IF(DEBUG)WRITE(JOSTND,*)' ISPC,H,HTMAX1,AGMAX1,SITAGE,SITHT= ', &
            ISPC,H,HTMAX1,AGMAX1,SITAGE,SITHT
        GO TO 30
      ENDIF

   75 CONTINUE
!----------
!  CALL HTCALC TO CALCULATE POTENTIAL HT GROWTH
!----------
      HGUESS = 0.
      IF(DEBUG)WRITE(JOSTND,*)' IN FINDAG, CALLING HTCALC'
      CALL HTCALC(SINDX,ISPC,AG,HGUESS,JOSTND,DEBUG)

      IF(DEBUG)WRITE(JOSTND,91200)I,ISPC,AG,HGUESS,H
91200 FORMAT(' FINDAG I,ISPC,AG,HGUESS,H ',2I5,3F10.2)

      DIFF=ABS(HGUESS-H)
      IF(DIFF .LE. TOLER .OR. H .LT. HGUESS)THEN
        SITAGE = AG
        SITHT = HGUESS
        GO TO 30
      END IF
      AG = AG + 2.

      IF(AG .GT. AGMAX1) THEN
!----------
!  H IS TOO GREAT AND MAX AGE IS EXCEEDED
!----------
        SITAGE = AGMAX1
        SITHT = H
        GO TO 30
      ELSE
        GO TO 75
      ENDIF

   30 CONTINUE
      IF(DEBUG)WRITE(JOSTND,50)I,SITAGE,SITHT,AGMAX1,HTMAX1
   50 FORMAT(' LEAVING SUBROUTINE FINDAG  I,SITAGE,SITHT,AGMAX1,', &
          'HTMAX1 = ',I5,4F10.3)

      RETURN
      END SUBROUTINE FINDAG
!**END OF CODE SEGMENT
end module findage_mod