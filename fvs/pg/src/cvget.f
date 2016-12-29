      SUBROUTINE CVGET (WK3, IPNT, ILIMIT)
      IMPLICIT NONE
C----------
C  $Id$
C----------
C
C     GET THE COVER DATA FOR A GIVEN STAND.
C
C     PART OF THE PARALLEL PROCESSING EXTENSION TO PROGNOSIS.
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'CVCOM.F77'
C
C
COMMONS
C
      INTEGER MXL,MXI,MXR
      PARAMETER (MXL=12,MXI=17,MXR=7)
C
      INTEGER ILIMIT,IPNT,INTS(MXI),IP1,ITRN,ISUB,JSUB
      LOGICAL LOGICS(MXL)
      REAL WK3 (MAXTRE)
      REAL REALS (MXR)
C
C     GET THE INTEGER SCALARS.
C
      CALL IFREAD (WK3, IPNT, ILIMIT, INTS, MXI, 2)
      COVOPT   = INTS ( 1)
      ICVBGN   = INTS ( 2)
      IDIST    = INTS ( 3)
      IHTYPE   = INTS ( 4)
      INF      = INTS ( 5)
      IOV      = INTS ( 6)
      IP1      = INTS ( 7)
      IPHYS    = INTS ( 8)
      ITRN     = INTS ( 9)
      ITUN     = INTS (10)
      IUN      = INTS (11)
      JOSHRB   = INTS (12)
      NKLASS   = INTS (13)
      NSHOW    = INTS (14)
      JCVNOH   = INTS (15)
      ICEHAB   = INTS (16)
      IGFHAB   = INTS (17)
C
C     GET THE LOGICAL SCALARS.
C
      CALL LFREAD (WK3, IPNT, ILIMIT, LOGICS, MXL, 2)
      LBROW  = LOGICS ( 1)
      LCALIB = LOGICS ( 2)
      LCAL1  = LOGICS ( 3)
      LCAL2  = LOGICS ( 4)
      LCNOP  = LOGICS ( 5)
      LCOV   = LOGICS ( 6)
      LCOVER = LOGICS ( 7)
      LCVSUM = LOGICS ( 8)
      LSHOW  = LOGICS ( 9)
      LSHRUB = LOGICS (10)
      LCVNOH = LOGICS (11)
      LSAGE  = LOGICS (12)
C
C     GET THE REAL SCALARS.
C
      CALL BFREAD (WK3, IPNT, ILIMIT, REALS, MXR, 2)
      CRAREA   = REALS (1)
      HTMAX    = REALS (2)
      HTMIN    = REALS (3)
      SAGE     = REALS (4)
      SUMCVR   = REALS (5)
      TALLSH   = REALS (6)
      TCOV     = REALS (7)
C
C     GET THE ONE DIMENSIONAL ARRAYS.
C
      CALL IFREAD (WK3,IPNT,ILIMIT, ISHOW, 6, 2)
      CALL IFREAD (WK3,IPNT,ILIMIT, ILAYR, 31, 2)
      CALL IFREAD (WK3,IPNT,ILIMIT, ICVAGE, IP1, 2)
      CALL IFREAD (WK3,IPNT,ILIMIT, ISHAPE, ITRN, 2)
C
      CALL LFREAD (WK3,IPNT,ILIMIT, LTHIND, IP1, 2)
C
      CALL BFREAD (WK3, IPNT, ILIMIT, AVGBHT, 3, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, AVGBPC, 3, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, CVAVG, 3, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, CVFRAC, 3, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, HTAVG, 3, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, HTFRAC, 3, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, CRXHT, 16, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, SD2XHT, 16, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, BHTCF, 31, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, BPCCF, 31, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, PB, 31, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, PBCV, 31, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, PCON, 31, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, SH, 31, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, SHRBHT, 31, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, SHRBPC, 31, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, XCV, 31, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, XPB, 31, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, XSH, 31, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, TRECW, ITRN, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, TRFBMS, ITRN, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, TCON, 2,  2)
      CALL BFREAD (WK3, IPNT, ILIMIT, CCON, 31, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, HCON, 31, 2)
C
C     GET THE MAXCY1*2 ARRAYS.
C
      DO 1000 ISUB=1,2
      CALL IFREAD (WK3,IPNT,ILIMIT, ISTAGE (1,ISUB), IP1, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, ASHT (1,ISUB), IP1, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, CLOW (1,ISUB), IP1, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, CMED (1,ISUB), IP1, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, CTALL (1,ISUB), IP1, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, PGT0 (1,ISUB), IP1, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, SBMASS (1,ISUB), IP1, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, SDIAM (1,ISUB), IP1, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, STDHT (1,ISUB), IP1, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, TCVOL (1,ISUB), IP1, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, TIMESD (1,ISUB), IP1, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, TOTBMS (1,ISUB), IP1, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, TOTLCV (1,ISUB), IP1, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, TPCTCV (1,ISUB), IP1, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, TPROAR (1,ISUB), IP1, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, TRETOT (1,ISUB), IP1, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, TWIGS (1,ISUB), IP1, 2)
C
C     GET THE MAXCY1*2*6 ARRAYS.
C
      DO 300 JSUB=1,6
      CALL IFREAD (WK3,IPNT,ILIMIT, INDSP (1,ISUB,JSUB), IP1, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, CIND (1,ISUB,JSUB), IP1, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, PIND (1,ISUB,JSUB), IP1, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, HIND (1,ISUB,JSUB), IP1, 2)
  300 CONTINUE
C
C     GET THE MAXCY1*2*MAXSP ARRAYS.
C
      DO 400 JSUB=1,MAXSP
      CALL BFREAD (WK3, IPNT, ILIMIT, SCOV (1,ISUB,JSUB), IP1, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, TRSH (1,ISUB,JSUB), IP1, 2)
  400 CONTINUE
C
C     GET THE MAXCY1*2*12 ARRAYS.
C
      DO 500 JSUB=1,12
      CALL IFREAD (WK3,IPNT,ILIMIT, ISSP (1,ISUB,JSUB), IP1, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, SCV (1,ISUB,JSUB), IP1, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, SHT (1,ISUB,JSUB), IP1, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, SPB (1,ISUB,JSUB), IP1, 2)
  500 CONTINUE
C
C     GET THE MAXCY1*2*16 ARRAYS.
C
      DO 600 JSUB=1,16
      CALL BFREAD (WK3, IPNT, ILIMIT, CFBXHT (1,ISUB,JSUB), IP1, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, PCXHT (1,ISUB,JSUB), IP1, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, PROXHT (1,ISUB,JSUB), IP1, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, TXHT (1,ISUB,JSUB), IP1, 2)
      CALL BFREAD (WK3, IPNT, ILIMIT, VOLXHT (1,ISUB,JSUB), IP1, 2)
  600 CONTINUE
 1000 CONTINUE
      RETURN
      END
