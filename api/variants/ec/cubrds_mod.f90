module cubrds_mod
    contains
    subroutine cubrds()
        use volstd_mod
        use prgprm_mod
        implicit none
        
        integer k
        
        !----------
        !  **CUBRDS--EC  DATE OF LAST REVISION:  05/09/12
        !----------
        !  DEFAULT PARAMETERS FOR THE CUBIC AND BOARD FOOT VOLUME EQUATIONS.
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
        !  COEFFICIENTS FOR CUBIC FOOT VOLUME FOR TREES THAT ARE SMALLER THAN
        !  THE TRANSITION SIZE
        !----------
        CFVEQS = reshape((/ &
               0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0, &
               0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0, &
               0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0, &
               0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0, &
               0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0, &
               0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0, &
               0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0, &
               0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0, &
               0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0, &
          0.030288,      0.0,     0.0,0.002213,     0.0,     0.0,    0.0, &
            (0.0,k=1,35), &
               0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0, &
            (0.0,k=1,98), &
               0.0,      0.0,     0.0,     0.0,     0.0,     0.0,    0.0, &
            (0.0,k=1,7)/), shape(cfveqs))
        !----------
        !  COEFFICIENTS FOR CUBIC FOOT VOLUME FOR TREES THAT ARE LARGER THAN
        !  THE TRANSITION SIZE
        !----------
        CFVEQL = reshape((/ &
               0.0,      0.0,     0.0, 0.00233,     0.0,     0.0,    0.0, &
               0.0,      0.0,     0.0, 0.00184,     0.0,     0.0,    0.0, &
               0.0,      0.0,0.003865,0.001714,     0.0,     0.0,    0.0, &
               0.0,      0.0,     0.0, 0.00234,     0.0,     0.0,    0.0, &
               0.0,      0.0,     0.0, 0.00219,     0.0,     0.0,    0.0, &
               0.0,      0.0,     0.0, 0.00205,     0.0,     0.0,    0.0, &
               0.0,      0.0,     0.0,     0.0,0.002782,  1.9041, 1.0488, &
               0.0,      0.0,0.003865,0.001714,     0.0,     0.0,    0.0, &
               0.0,      0.0,0.003865,0.001714,     0.0,     0.0,    0.0, &
         -1.557103,      0.0,     0.0,0.002474,     0.0,     0.0,    0.0, &
            (0.0,k=1,7), &
               0.0,      0.0,     0.0, 0.00219,     0.0,     0.0,    0.0, &
            (0.0,k=1,21), &
               0.0,      0.0,     0.0, 0.00205,     0.0,     0.0,    0.0, &
            (0.0,k=1,98), &
               0.0,      0.0,     0.0, 0.00219,     0.0,     0.0,    0.0, &
            (0.0,k=1,7)/), shape(cfveql))
        !----------
        !  FLAG IDENTIFYING THE SIZE TRANSITION VARIABLE; 0=D, 1=D2H
        !----------
        ICTRAN = (/ &
                0, 0, 0, 0, 0, 0, 0, 0, 0, 1, &
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
                0, 0/)
        !----------
        !  TRANSITION SIZE.  TREES OF LARGER SIZE (D OR D2H) WILL COEFFICIENTS
        !  FOR LARGER SIZE TREES.
        !----------
        CTRAN = (/ &
               0.0,      0.0,     0.0,     0.0,     0.0, &
               0.0,      0.0,     0.0,     0.0,  6000.0, &
               0.0,      0.0,     0.0,     0.0,     0.0, &
               0.0,      0.0,     0.0,     0.0,     0.0, &
               0.0,      0.0,     0.0,     0.0,     0.0, &
               0.0,      0.0,     0.0,     0.0,     0.0, &
               0.0,      0.0/)
        !----------
        !  COEFFICIENTS FOR BOARD FOOT VOLUME FOR TREES THAT ARE SMALLER THAN
        !  THE TRANSITION SIZE
        !----------
        BFVEQS = reshape((/ &
           -26.729,      0.0,     0.0, 0.01189,     0.0,     0.0,    0.0, &
           -29.790,      0.0,     0.0, 0.00997,     0.0,     0.0,    0.0, &
           -25.332,      0.0,     0.0, 0.01003,     0.0,     0.0,    0.0, &
           -34.127,      0.0,     0.0, 0.01293,     0.0,     0.0,    0.0, &
           -37.314,      0.0,     0.0, 0.01203,     0.0,     0.0,    0.0, &
           -10.742,      0.0,     0.0, 0.00878,     0.0,     0.0,    0.0, &
            -8.085,      0.0,     0.0, 0.01208,     0.0,     0.0,    0.0, &
           -11.851,      0.0,     0.0, 0.01149,     0.0,     0.0,    0.0, &
           -11.403,      0.0,     0.0, 0.01011,     0.0,     0.0,    0.0, &
           -50.340,      0.0,     0.0, 0.01201,     0.0,     0.0,    0.0, &
           (0.0,k=1,7), &
           -37.314,      0.0,     0.0, 0.01203,     0.0,     0.0,    0.0, &
           (0.0,k=1,21), &
           -10.742,      0.0,     0.0, 0.00878,     0.0,     0.0,    0.0, &
            (0.0,k=1,98), &
           -37.314,      0.0,     0.0, 0.01203,     0.0,     0.0,    0.0, &
            (0.0,k=1,7)/), shape(bfveqs))
        !----------
        !  COEFFICIENTS FOR BOARD FOOT VOLUME FOR TREES THAT ARE LARGER THAN
        !  THE TRANSITION SIZE
        !----------
        BFVEQL = reshape((/ &
           -32.516,      0.0,     0.0, 0.01181,     0.0,     0.0,    0.0, &
            85.150,      0.0,     0.0, 0.00841,     0.0,     0.0,    0.0, &
            -9.522,      0.0,     0.0, 0.01011,     0.0,     0.0,    0.0, &
            10.603,      0.0,     0.0, 0.01218,     0.0,     0.0,    0.0, &
           -50.680,      0.0,     0.0, 0.01306,     0.0,     0.0,    0.0, &
            -4.064,      0.0,     0.0, 0.00799,     0.0,     0.0,    0.0, &
            14.111,      0.0,     0.0, 0.01103,     0.0,     0.0,    0.0, &
             1.620,      0.0,     0.0, 0.01158,     0.0,     0.0,    0.0, &
           124.425,      0.0,     0.0, 0.00694,     0.0,     0.0,    0.0, &
          -298.784,      0.0,     0.0, 0.01595,     0.0,     0.0,    0.0, &
            (0.0,k=1,7), &
           -50.680,      0.0,     0.0, 0.01306,     0.0,     0.0,    0.0, &
            (0.0,k=1,21), &
            -4.064,      0.0,     0.0, 0.00799,     0.0,     0.0,    0.0, &
            (0.0,k=1,98), &
           -50.680,      0.0,     0.0, 0.01306,     0.0,     0.0,    0.0, &
            (0.0,k=1,7)/), shape(bfveql))
        !----------
        !  FLAG IDENTIFYING THE SIZE TRANSITION VARIABLE; 0=D, 1=D2H
        !----------
        IBTRAN = (/ &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
            0, 0/)
        !----------
        !  TRANSITION SIZE.  TREES OF LARGER SIZE (D OR D2H) WILL USE COEFFICIENTS
        !  FOR LARGER SIZE TREES.
        !----------
        BTRAN = (/ &
              20.5,     20.5,    20.5,    20.5,    20.5, &
              20.5,     20.5,    20.5,    20.5,    20.5, &
               0.0,     20.5,     0.0,     0.0,     0.0, &
              20.5,      0.0,     0.0,     0.0,     0.0, &
               0.0,      0.0,     0.0,     0.0,     0.0, &
               0.0,      0.0,     0.0,     0.0,     0.0, &
              20.5,      0.0/)
    end subroutine cubrds
end module cubrds_mod
