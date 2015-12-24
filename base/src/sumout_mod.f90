module sumout_mod
    use prgprm_mod
    implicit none

!CODE SEGMENT SUMTAB - SUMTAB.F77
    INTEGER ISDI(MAXCY1),ISDIAT(MAXCY1),IOLDBA(MAXCY1),IBTCCF(MAXCY1) &
            ,IBTAVH(MAXCY1),MAIFLG,NEWSTD
    REAL    QDBHAT(MAXCY1),QSDBT(MAXCY1),BCYMAI(MAXCY1),TOTREM,AGELST
    COMMON /SUMTAB/ QDBHAT,ISDI,QSDBT,ISDIAT,IOLDBA,IBTCCF,IBTAVH,BCYMAI &
            ,TOTREM,MAIFLG,NEWSTD,AGELST
!-----END SEGMENT

    contains

    SUBROUTINE SUMOUT(IOSUM,I20,ICFLAG,JOPRT,JOSTND,JOSUM,LENG,MGMID,NPLT &
            ,SAMWT,ITITLE,IPTINV)
!
!     WRITES SUMMARY OUTPUT.
!
!     IOSUM = THE SUMMARY OUTPUT ARRAY FROM THE PROGNOSIS MODEL.
!              1: YEAR
!              2: AGE
!              3: TREES/ACRE                     START OF PERIOD
!              4: TOTAL CU FT                    START OF PERIOD
!     *        4: MERCH CU FT (PULP AND SAWLOG)  START OF PERIOD
!              5: MERCH CU FT                    START OF PERIOD
!     *        5: MERCH CU FT (SAWLOG)           START OF PERIOD
!              6: MERCH BD FT                    START OF PERIOD
!     *        6: MERCH BD FT (SAWLOG)           START OF PERIOD
!              7: REMOVED TREES/ACRE
!              8: REMOVED TOTAL CU FT
!     *        8: REMOVED MERCH CU FT (PULP AND SAWLOG)
!              9: REMOVED MERCH CU FT
!     *        9: REMOVED MERCH CU FT (SAWLOG)
!             10: REMOVED MERCH BD FT
!     *       10: REMOVED MERCH BD FT (SAWLOG)
!             11: BASAL AREA/ACRE                AFTER TREATMENT
!             12: CCF                            AFTER TREATMENT
!             13: AVERAGE DOMINANT HEIGHT        AFTER TREATMENT
!             14: PERIOD LENGTH (YEARS)
!             15: ACCRETION (ANNUAL IN CU FT/ACRE)
!             16: MORTALITY (ANNUAL IN CU FT/ACRE)
!             17: SAMPLE WEIGHT
!             18: FOREST COVER TYPE CODE
!             19: SIZE CLASS
!             20: STOCKING CLASS
!
!     ICFLAG= CALL FLAG, 0=NORMAL, 1=PPMAIN CALL FOR INITIAL VALUES.
!     JOSTND= DATA SET REFERENCE NUMBER FOR 'PRINTED' COPY (WITH
!             HEADINGS AND CARRAGE CONTROL BYTE).  IF JOSTND=0, NO
!             DATA WILL BE WRITTEN.
!     JOPRT = PRINTER OUTPUT FOR MESSAGES.
!     JOSUM = DATA SET REFERENCE NUMBER FOR 'NON-PRINTED' COPY (WITH
!             OUT HEADINGS, NO CARRAGE CONTROL BYTE). IF JOSUM=0,
!             NO DATA WILL BE WRITTEN.
!     LENG   = NUMBER OF ROWS (ENTRIES) IN IOSUM.
!     MGMID = MANAGEMENT IDENTIFICATION FIELD. ASSUMED ALPHANUMERIC.
!     NPLT  = PLOT IDENTIFICATION FIELD. ASSUMED ALPHANUMERIC.
! NOTE: * Indicates R8 and R9 specific (CS, LS, NE, OZ, SE, SN)
!
!OMMONS
!
!
!      INCLUDE 'PRGPRM.F77'
!
!
!      INCLUDE 'SUMTAB.F77'
!
!OMMONS
!
      CHARACTER CISN*11,NPLT*26,TIM*8,DAT*10,MGMID*4,VVER*7,REV*10
      CHARACTER ITITLE*72,RECORD*250
      INTEGER*4 IOSUM(I20,LENG),IPTINV
      REAL SAMWT
      INTEGER JOSUM,JOSTND,JOPRT,ICFLAG,I20,LENG,ISTLNB,I12,I,K,II
      LOGICAL LNOR,LPRT,LDSK,LCONN
!
!     **************************************************************
!
!     STEP1: SET SWITCHES.
!
      LPRT= JOSTND .GT. 0
      LDSK= JOSUM .GT. 0
      IF (.NOT. (LPRT.OR.LDSK)) RETURN
!
      CALL PPISN (CISN)
      CALL VARVER (VVER)
      CALL REVISE (VVER,REV)
      CALL GRDTIM (DAT,TIM)

      IF(LDSK) THEN
        INQUIRE(UNIT=JOSUM,opened=LCONN)
        IF (.NOT.LCONN) THEN
          CALL fvsGetKeywordFileName(RECORD,len(RECORD),II)
          IF (RECORD.NE.' ') THEN
            II=index(RECORD,".k")
            IF (II == 0) II=index(RECORD,".K")
            IF (II == 0) II=len_trim(RECORD)
            RECORD=TRIM(RECORD(:II-1))//".sum"
            OPEN(UNIT=JOSUM,FILE=TRIM(RECORD),STATUS='replace')
          ENDIF
        ENDIF
        WRITE (JOSUM,2) LENG,NPLT,MGMID,SAMWT,VVER,DAT,TIM,REV,CISN,IPTINV
    2   FORMAT ('-999',I5,1X,A26,1X,A4,E15.7,5(1X,A),I3)
      ENDIF
!
!     STEP2: WRITE VARIANT SPECIFIC HEADING.
!            SKIP A FEW LINES, DO NOT START A NEW PAGE.
!
      IF (LPRT) THEN
      WRITE (JOSTND,5) NPLT,MGMID,ITITLE(1:ISTLNB(ITITLE))
    5 FORMAT(/'STAND ID: ',A26,4X,'MGMT ID: ',A4,4X,A/)
!
      IF ((VVER(:2) .EQ. 'CS') .OR. (VVER(:2) .EQ. 'LS') .OR. &
            (VVER(:2) .EQ. 'NE') .OR. (VVER(:2) .EQ. 'OZ') .OR. &
            (VVER(:2) .EQ. 'SE') .OR. (VVER(:2) .EQ. 'SN')) THEN
!----------
!  WRITE HEADER FOR CS, LS, NE, OZ, SE, SN
!----------
      WRITE (JOSTND,12)
  12  FORMAT(//32X,'SUMMARY STATISTICS (PER ACRE OR STAND BASED ON TOTAL' &
        ' STAND AREA)',/, &
        134(1H-),/15X,'START OF SIMULATION PERIOD',21X,'REMOVALS',13X, &
        'AFTER TREATMENT',4X,'GROWTH THIS PERIOD',/9X,45(1H-),1X, &
        23(1H-),1X,21(1H-),2X,18(1H-),3X,'MAI  ------',/9X, &
        'NO OF',14X,'TOP', &
        6X,'MERCH SAWLG SAWLG NO OF MERCH SAWLG SAWLG',14X,'TOP  RES  ', &
        'PERIOD ACCRE MORT   MERCH FOR SS',/, &
        'YEAR AGE TREES  BA  SDI CCF ', &
        'HT  QMD  CU FT CU FT BD FT TREES CU FT CU FT BD FT  BA  SDI ', &
        'CCF HT   QMD  YEARS   PER  YEAR   CU FT TYP ZT', &
        /'---- --- ----- ', &
        '--- ---- --- --- ---- ',7('----- '),'--- ---- --- --- ----  ', &
        '------ ---- -----   ----- ------')
!
      ELSE
!----------
!  WRITE HEADER FOR ALL OTHER VARIANTS
!----------
      WRITE (JOSTND,14)
   14 FORMAT(//32X,'SUMMARY STATISTICS (PER ACRE OR STAND BASED ON TOTAL' &
        ' STAND AREA)',/, &
        134(1H-),/15X,'START OF SIMULATION PERIOD',21X,'REMOVALS',13X, &
        'AFTER TREATMENT',4X,'GROWTH THIS PERIOD',/9X,45(1H-),1X, &
        23(1H-),1X,21(1H-),2X,18(1H-),3X,'MAI  ------',/9X, &
        'NO OF',14X,'TOP', &
        6X,'TOTAL MERCH MERCH NO OF TOTAL MERCH MERCH',14X,'TOP  RES  ', &
        'PERIOD ACCRE MORT   MERCH FOR SS',/, &
        'YEAR AGE TREES  BA  SDI CCF ', &
        'HT  QMD  CU FT CU FT BD FT TREES CU FT CU FT BD FT  BA  SDI ', &
        'CCF HT   QMD  YEARS   PER  YEAR   CU FT TYP ZT', &
        /'---- --- ----- ', &
        '--- ---- --- --- ---- ',7('----- '),'--- ---- --- --- ----  ', &
        '------ ---- -----   ----- ------')
      ENDIF
      ENDIF
!----------
!     STEP3: LOOP THRU ALL ROWS IN IOSUM...WRITE OUTPUT.
!
!  THIS STEP TAKES JUST THE FIRST 12 ITEMS IN THE IOSUM ARRAY
!----------
      I12=I20-8
      DO 50 I=1,LENG
      IF ((VVER(:2) .EQ. 'CS') .OR. (VVER(:2) .EQ. 'LS') .OR. &
        (VVER(:2) .EQ. 'NE') .OR. (VVER(:2) .EQ. 'OZ') .OR. &
        (VVER(:2) .EQ. 'SE') .OR. (VVER(:2) .EQ. 'SN')) THEN
!
        IF(LPRT) &
        WRITE(JOSTND,20) (IOSUM(K,I),K=1,3),IOLDBA(I),ISDI(I), &
            IBTCCF(I),IBTAVH(I),QSDBT(I),(IOSUM(K,I),K=4,11), &
        ISDIAT(I),IOSUM(12,I),IOSUM(13,I),QDBHAT(I), &
        (IOSUM(K,I),K=14,16),BCYMAI(I),(IOSUM(K,I),K=18,20)
!
        IF(LDSK) &
        WRITE(JOSUM,9014) (IOSUM(K,I),K=1,3),IOLDBA(I),ISDI(I), &
            IBTCCF(I),IBTAVH(I),QSDBT(I),(IOSUM(K,I),K=4,11), &
            ISDIAT(I),IOSUM(12,I),IOSUM(13,I),QDBHAT(I), &
            (IOSUM(K,I),K=14,16),BCYMAI(I),(IOSUM(K,I),K=18,20)
!
      ELSE
!
        IF(LPRT) &
          WRITE(JOSTND,20) (IOSUM(K,I),K=1,3),IOLDBA(I),ISDI(I), &
            IBTCCF(I),IBTAVH(I),QSDBT(I),(IOSUM(K,I),K=4,11), &
            ISDIAT(I),IOSUM(12,I),IOSUM(13,I),QDBHAT(I), &
            (IOSUM(K,I),K=14,16),BCYMAI(I),(IOSUM(K,I),K=18,20)
!
        IF(LDSK) &
          WRITE(JOSUM,9014) (IOSUM(K,I),K=1,3),IOLDBA(I),ISDI(I), &
            IBTCCF(I),IBTAVH(I),QSDBT(I),(IOSUM(K,I),K=4,11), &
            ISDIAT(I),IOSUM(12,I),IOSUM(13,I),QDBHAT(I), &
            (IOSUM(K,I),K=14,16),BCYMAI(I),(IOSUM(K,I),K=18,20)

      ENDIF

!
!      CALL THE DBSSUMRY FOR POPULATING THE DATABASE WITH
!      THE SUMMARY INFORMATION
!
        IF (ICFLAG.EQ.0) CALL DBSSUMRY(IOSUM(1,I), &
           IOSUM(2,I),NPLT,IOSUM(3,I),IOLDBA(I), &
           ISDI(I),IBTCCF(I),IBTAVH(I),QSDBT(I),IOSUM(4,I),IOSUM(5,I), &
           IOSUM(6,I),IOSUM(7,I),IOSUM(8,I),IOSUM(9,I),IOSUM(10,I), &
           IOSUM(11,I),ISDIAT(I),IOSUM(12,I),IOSUM(13,I),QDBHAT(I), &
           IOSUM(14,I),IOSUM(15,I),IOSUM(16,I),BCYMAI(I),IOSUM(18,I), &
           IOSUM(19,I),IOSUM(20,I))
!
   20     FORMAT(2I4,I6,I4,I5,2I4,F5.1,7I6,I4,I5,2I4,F5.1,2X,I6, &
                 I5,I6,2X,F6.1,1X,I3,1X,2I1)
!
 9014     FORMAT(2I4,I6,I4,I5,2I4,F5.1,7I6,I4,I5,2I4,F5.1,2X,I6, &
                 I5,I6,2X,F6.1,1X,I3,1X,2I1)
   50 CONTINUE
!
      IF (.NOT.LDSK) RETURN

      WRITE (JOPRT,60) LENG,JOSUM
   60 FORMAT(/'NOTE:',I3,' LINES OF SUMMARY DATA HAVE BEEN WRITTEN', &
             ' TO THE FILE REFERENCED BY LOGICAL UNIT',I3)
    RETURN
    END

end module sumout_mod
