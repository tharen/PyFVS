SUBROUTINE ESCPRS (ITRGT,DEBUG)
      use findage_mod, only: findag

      use prgprm_mod
      use contrl_mod
      use eshap_mod
      use arrays_mod
      use outcom_mod

      IMPLICIT NONE
!----------
!  **ESCPRS DATE OF LAST REVISION:   06/21/11
!----------
!
      LOGICAL DEBUG
      REAL PRMS(2)
      REAL SPCNT(MAXSP,3)
      REAL SITAGE,SITHT,AGMAX,H,HTMAX,HTMAX2,D1,D2
      INTEGER IS,IM,ISPC
      INTEGER MYACT(1),ITRGT,I,N,KDT
      DATA MYACT/250/
!
!     CALL THE COMPRESSION ROUTINE.
!
      I=ITRN
      N=ITRGT
      PRMS(2)=.5
      CALL COMPRS (N,PRMS(2))
!
!     REESTABLISH THE SPECIES-ORDER SORT.
!
      CALL SPESRT
!
!     INITIALIZE SPCNT.
!
      DO I=1,MAXSP
         SPCNT(I,1)=0.
         SPCNT(I,2)=0.
         SPCNT(I,3)=0.
      ENDDO
      IF (ITRN.GT.0) THEN
!
!        RE-COMPUTE THE SPECIES COMPOSITION
!
         DO 70 I=1,ITRN
         IS=ISP(I)
         IM=IMC(I)
         SPCNT(IS,IM)=SPCNT(IS,IM)+PROB(I)
   70    CONTINUE
!
!        REESTABLISH THE DIAMETER SORT.
!
         CALL RDPSRT (ITRN,DBH,IND,.TRUE.)
!
!        RE-COMPUTE THE DISTRIBUTION OF TREES PER ACRE AND
!        SPECIES-TREE CLASS COMPOSITON BY TREES PER ACRE.
!
         CALL PCTILE(ITRN,IND,PROB,WK3,ONTCUR(7))
!-------
!  ESTTE MISSING TOTAL TREE AGES
!-------
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
            CALL FINDAG(I,ISPC,D1,D2,H,SITAGE,SITHT,AGMAX,HTMAX, &
                        HTMAX2,DEBUG)
            IF(SITAGE .GT. 0.)ABIRTH(I)=SITAGE
         ENDIF
         ENDDO
!
      ENDIF
!
!     (MAKE SURE IFST=1, TO GET A NEW SET OF POINTERS TO THE
!      DISTRIBUTIONS).
!
      IFST=1
      CALL DIST(ITRN,ONTCUR,WK3)
      CALL COMP(OSPCT,IOSPCT,SPCNT)
!
!     WRITE A MSG SAYING THAT COMPRESS WAS CALLED.
!
      IF (DEBUG) WRITE (JOREGT,10) I,N,IY(ICYC+1)-1
   10 FORMAT('IN ESCPRS: COMPRS: ',I5,' RECS', &
        ' TO',I5,'.  YEAR:',I5)
!
!     ADD COMPRESS TO THE ACTIVITIY SCHEDULE AND SIGNAL THAT IT IS
!     BY SETTING THE STATUS CODE TO KDT (THIRD ARGUMENT).
!
      PRMS(1)=N
      KDT=IY(ICYC+1)-1
      CALL OPADD (KDT,MYACT(1),KDT,2,PRMS,I)
      RETURN
END
