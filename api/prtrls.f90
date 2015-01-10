      SUBROUTINE PRTRLS (IWHO)
      use tree_data, only: save_tree_data,copy_tree_data,copy_cuts_data

      use prgprm_mod
      use arrays_mod
      use contrl_mod
      use plot_mod
      use estree_mod
      use varcom_mod
      use workcm_mod

      IMPLICIT NONE
!----------
!  $Id: prtrls.f 968 2013-07-12 18:49:16Z rhavis@msn.com $
!----------
!
!     PRINT THE TREE LIST.
!
!     IWHO = 1 IF CALLED NORMALLY, AND 2 OR 3 IF CALLED FROM CUTS.
!
      INTEGER I,J,NUMREQ,NTODO,ITODO,NPRMS,IACTK,IDT,JYR,IP,ITPLAB
      INTEGER IWHO,KOLIST,KNTREC,ISPC,I1,I2,I3,IICR,IDMR,ICDF,IBDF
      INTEGER IPTBAL,MYACT(3)
      CHARACTER CLAB1(4)*4,CLAB2(3)*3
      REAL TEM(6),DUPCHK(5,5),XXWT,P,DP,CW,DGI
      CHARACTER CISN*11,TIM*8,DAT*10,VVER*7,TID*8
      CHARACTER REV*10
      INTEGER*4 IDCMP1,IDCMP2,DBSKODE
      LOGICAL   LHD,LRC,LPPACT,LFORMT,LTREE,LOK
      DATA MYACT/80,199,198/
      DATA IDCMP1,IDCMP2/10000000,20000000/
      DATA CLAB1/'TREE','DEAD','CUT','ATRT'/
      DATA CLAB2/'END','CUT','CUT'/
!---------
!  INITIALIZATION
!----------
      DO 500 I= 1,6
      TEM(I)= 0.
  500 CONTINUE
!----------
!  ARRAY DUPCHK STORES UP TO 5 REQUESTS PER CYCLE AND ELIMINATES
!  DUPLICATES OF THESE 5 REQUESTS.
!----------
      DO 502 I=1,5
      DO 501 J=1,5
      DUPCHK(I,J)=0.
  501 CONTINUE
  502 CONTINUE
      NUMREQ = 0


      ! Process tree details for the API report arrays if requested.
      if (save_tree_data) then
        select case (iwho)
            case (1)
                ! Complete live tree list & mortality
                call copy_tree_data()
            case (2)
                ! Cut tree list
                call copy_cuts_data()
        end select
      endif
!----------
!     FIND OUT IF THERE IS A TREELIST OPTION.
!
      CALL OPFIND (1,MYACT(IWHO),NTODO)
!
!     IF THERE ARE NONE TO DO, THEN: RETURN.
!
      IF (NTODO.EQ.0) RETURN
!
!     ELSE: DO THE OPTIONS PRESENT.
!
      DO 200 ITODO=1,NTODO
      CALL OPGET (ITODO,6,IDT,IACTK,NPRMS,TEM)
      IF (IACTK.LT.0) GOTO 200
!----------
!  STORE THE REQUEST IF IT ISN'T A DUPLICATE. IGNORE THE REQUEST
!  IF IT IS A DUPLICATE. SKIP THE DUPLICATE LOGIC IF THERE IS ONLY
!  ONE REQUEST.
!----------
      IF(NTODO .EQ. 1)GO TO 510
      IF(ITODO .EQ. 1) THEN
        DUPCHK(1,1)=TEM(1)
        DUPCHK(1,2)=TEM(2)
        DUPCHK(1,3)=TEM(3)
        DUPCHK(1,4)=TEM(4)
        DUPCHK(1,5)=TEM(6)
        NUMREQ=1
      ELSE
        DO I=1,NUMREQ
        IF(TEM(1).EQ.DUPCHK(I,1) .AND. TEM(2).EQ.DUPCHK(I,2) .AND. &
           TEM(3).EQ.DUPCHK(I,3) .AND. TEM(4).EQ.DUPCHK(I,4) .AND. &
           TEM(6).EQ.DUPCHK(I,5)) GO TO 200
        ENDDO
        IF(NUMREQ.GE.5)GO TO 510
        NUMREQ=NUMREQ+1
        DUPCHK(NUMREQ,1)=TEM(1)
        DUPCHK(NUMREQ,2)=TEM(2)
        DUPCHK(NUMREQ,3)=TEM(3)
        DUPCHK(NUMREQ,4)=TEM(4)
        DUPCHK(NUMREQ,5)=TEM(6)
      ENDIF
!
!     SET THE TREELIST TYPE FLAG (LET IP BE THE RECORD OUTPUT COUNT).
!     AND THE OUTPUT REPORTING YEAR.
!     ITPLAB: 1=STANDARD COMPLETE LIVE TREE LIST, 2=DEAD TREELIST
!             3=CUT TREE LIST, 4=AFTER TREATMENT TREE LIST.
!
!
  510 CONTINUE
      IF (IWHO.EQ.1) THEN
         JYR=IY(ICYC+1)
         IP=ITRN
         ITPLAB=1
         IF(NPRMS.GE.4 .AND. TEM(4).GT.0.)ITPLAB=2
      ELSEIF(IWHO.EQ.2)THEN
         JYR=IY(ICYC)
         IP=0
         DO 1 I=1,ITRN
         IF (WK3(I).GT.0) IP=IP+1
    1    CONTINUE
         ITPLAB=3
      ELSE
         JYR=IY(ICYC)
         IP=0
         DO I=1,ITRN
         IF (PROB(I).GT.0)IP=IP+1
         ENDDO
         ITPLAB=4
      ENDIF
      LTREE=.FALSE.
!
!     IF AT THE PRE-PROJECTION TIME, FIND OUT IF A TREELIST HAS BEEN
!     SUPPRESSED OR IF A DEAD TREE LIST IS REQUESTED. IF YES, THEN
!     BRANCH TO NOT ACCOMPLISH THE REQUEST.
!
      IF (LSTART) THEN
         IF ((NPRMS.GE.3 .AND. TEM(3).EQ.1.)) GOTO 200
         IF(NPRMS.GE.3 .AND. TEM(3).EQ.2.)CALL OPDONE(ITODO,JYR)
      ELSE
         IF(NPRMS.GE.3 .AND. TEM(3).EQ.2) GO TO 200
!
!        SIGNAL THE OPTION DONE.
!
         IF(IWHO.EQ.1)THEN
           CALL OPDONE (ITODO,JYR-1)
         ELSEIF (IWHO.EQ.2)THEN
           CALL OPDONE (ITODO,JYR)
         ELSE
           CALL OPDONE (ITODO,JYR)
         ENDIF
      ENDIF
!
!     CALL THE DATABASE TREELIST OUTPUT FUNCTION
!
      DBSKODE = 1
      CALL DBSTRLS(IWHO,DBSKODE,TEM(6))
!     RETURN IF WE ARE REDIRECTING OUTPUT EXCLUSIVELY TO THE DATABASE
      IF (DBSKODE.EQ.0) RETURN
!
!     CALL THE DATABASE AFTER TREATMENT TREELIST OUTPUT FUNCTION
!
      DBSKODE = 1
      CALL DBSATRTLS(IWHO,DBSKODE,TEM(6))
!     RETURN IF WE ARE REDIRECTING OUTPUT EXCLUSIVELY TO THE DATABASE
      IF (DBSKODE.EQ.0) RETURN
!
!     CALL THE DATABASE CUTLIST OUTPUT FUNCTION
!
      DBSKODE = 1
      CALL DBSCUTS(IWHO,DBSKODE)
!     RETURN IF WE ARE REDIRECTING OUTPUT EXCLUSIVELY TO THE DATABASE
      IF (DBSKODE.EQ.0) RETURN
!
!     SET VARIABLES AND SWITCHES
!
      LHD=TEM(2).EQ.0.0
      LRC=TEM(2).GE.0.0
      KOLIST=IFIX(TEM(1))
      LFORMT=KOLIST.GT.0
      IF (.NOT.LFORMT) THEN
         LRC=.TRUE.
         LHD=.FALSE.
         KOLIST=IABS(KOLIST)
      ENDIF
!
!     MAKE SURE THE OUTPUT FILE IS OPENNED
!
      CALL openIfClosed (KOLIST,"trl",LOK)
      IF (.NOT.LOK) RETURN
!
!     IF NO HEADING IS BEING WRITTEN, WRITE A MARKER AND THE NUMBER OF
!     RECORDS WHICH FOLLOW.
!
      IF (.NOT.LHD .AND. LRC) THEN
         CALL PPISN (CISN)
!
!        IF THE PPE IS ACTIVE, USE THE PPE STAND WEIGHTING FACTOR FOR
!        THE SAMPLING WEIGHT.
!
         XXWT=SAMWT
         CALL PPEATV (LPPACT)
         IF (LPPACT) CALL PPWEIG (0,XXWT)
         CALL VARVER (VVER)
         CALL REVISE (VVER,REV)
         CALL GRDTIM (DAT,TIM)
         IF (LFORMT) THEN
            WRITE (KOLIST,2) IP,ICYC,JYR,NPLT,MGMID,VVER,DAT,TIM, &
                       CLAB1(ITPLAB)(1:1),IFINT,XXWT,REV,CISN
    2       FORMAT ('-999',3I5,6(1X,A),I3,E14.7,2(1X,A))
         ELSE
            WRITE (KOLIST) IP,ICYC,JYR,NPLT,MGMID,VVER,DAT,TIM, &
                       CLAB1(ITPLAB)(1:1),IFINT,XXWT,REV,CISN
         ENDIF
      ENDIF
      IP=0
      KNTREC=0
!
!     PROCESS TREELIST
!
      DO 60 ISPC=1,MAXSP
      I1=ISCT(ISPC,1)
      IF(I1.EQ.0) GO TO 60
      I2=ISCT(ISPC,2)
      DO 50 I3=I1,I2
      LTREE=.TRUE.
      I=IND1(I3)
      IF (.NOT.LHD) GOTO 120
      IF(KNTREC.NE.0) GO TO 110
      IP=IP+1
      IF (LFORMT) CALL GROHED (KOLIST)
      IF(LFORMT) THEN
          WRITE (KOLIST,11) CLAB1(ITPLAB),NPLT,MGMID, &
                            CLAB2(IWHO),ICYC,IFINT,JYR,IP
   11     FORMAT(/'COMPLETE ',A4,' LIST -- STAND: ',A26,T58,'MGMTID: ', &
           A4,T72,A3,' CYCLE: ',I2,T87,'CYCLE LENGTH: ',I2,' YRS', &
           T109,'YEAR: ',I4,T121,'PAGE: ',I2/ &
           '  TREE   TREE SP SP TR SS PNT  ', &
           'TREES    MORTAL   CURR  DIAM  CURR  HT',5X,'MAX',7X,'BA   ', &
           'POINT TOT CU MCH CU  MCH BD MC BF TRC',/, &
           ' NUMBER  INDX CD NO CL CD NUM PER ACRE PER ACRE  ', &
           'DIAM  INCR   HT  INCR CR  CW  MS ', &
           '%-TILE  BAL  FT VOL FT VOL  FT VOL DF DF  HT',/, &
           '-------- ---- -- -- -- -- --- -------- -------- ----- ', &
           '----- ----- ---- -- ---- -- ------ ----- ------ ', &
           '------ ------- -- -- ---')
      ENDIF
      KNTREC=1
  110 CONTINUE
      IF (ITPLAB.EQ.3) THEN
         IF (WK3(I).GT.0) KNTREC=KNTREC+1
      ELSEIF(ITPLAB.EQ.4)THEN
         IF (PROB(I).GT.0) KNTREC=KNTREC+1
      ELSE
         KNTREC=KNTREC+1
      ENDIF
      IF(KNTREC.GT.50) KNTREC=0
  120 CONTINUE
!
!      ITPLAB: 1=STANDARD COMPLETE LIVE TREE LIST, 2=DEAD TREELIST
!              3=CUT TREE LIST, 4=AFTER TREATMENT TREE LIST.
!
      IF (ITPLAB.EQ.1) THEN
         P = PROB(I) / GROSPC
         IF (ICYC.GT.0) THEN
            DP = WK2(I)/ GROSPC
         ELSE
            DP = 0.0
         ENDIF
      ELSEIF (ITPLAB.EQ.2) THEN
         IF (ICYC.GT.0) THEN
            DP = WK2(I)/ GROSPC
            P= 0.
         ELSE
            DP= 0.
            P = 0.
         ENDIF
      ELSEIF (ITPLAB.EQ.3) THEN
         P = WK3(I)/GROSPC
         DP = 0.0
         IF (P.LE.0.0) GOTO 50
      ELSE
         P=PROB(I)/GROSPC
         DP = 0.0
         IF (P.LE.0.0) GOTO 50
      ENDIF
      IICR=((ICR(I)-1)/10)+1
      IF (IICR.GT.9) IICR=9
!----------
!   TRANSLATE TREE IDS FOR TREES THAT HAVE BEEN COMPRESSED OR
!   GENERATED THROUGH THE ESTAB SYSTEM.
!----------
      IF (IDTREE(I) .GT. IDCMP1) THEN
         IF (IDTREE(I) .GT. IDCMP2) THEN
            WRITE(TID,'(''CM'',I6.6)') IDTREE(I)-IDCMP2
         ELSE
            WRITE(TID,'(''ES'',I6.6)') IDTREE(I)-IDCMP1
         ENDIF
      ELSE
         WRITE(TID,'(I8)') IDTREE(I)
      ENDIF
!----------
!     GET MISTLETOE RATING FOR CURRENT TREE RECORD.
!----------
      CALL MISGET(I,IDMR)
!----------
!     SET CROWN WIDTH.
!----------
      CW=CRWDTH(I)
!----------
!     DECODE DEFECT AND ROUND OFF POINT BAL.
!----------
      ICDF=(DEFECT(I)-((DEFECT(I)/10000)*10000))/100
      IBDF= DEFECT(I)-((DEFECT(I)/100)*100)
      IPTBAL=NINT(PTBALT(I))
!----------
!  CYCLE 0, PRINT INPUT DG ONLY, UNLESS DIRECTED TO PRINT ESTIMATES.
!----------
      DGI=DG(I)
      IF(ICYC.EQ.0 .AND. TEM(6).EQ.0) DGI=WORK1(I)
!
      IF (LFORMT) THEN
        IF(P.LT.9999.9995 .AND. DP.LT.9999.9995)THEN
          WRITE(KOLIST,21) TID,I,NSP(ISP(I),1)(1:2),ISP(I),IMC(I), &
          ISPECL(I),ITRE(I),P,DP,DBH(I),DGI  ,HT(I),HTG(I),ICR(I),CW, &
          IDMR,PCT(I),IPTBAL,CFV(I),WK1(I),BFV(I),ICDF,IBDF, &
          ((ITRUNC(I)+5)/100)
   21     FORMAT(A8,1X,I4,1X,A2,3(1X,I2),1X,I3,1X,F8.3,1X,F8.3,1X, &
             F5.1,1X,F5.2,1X,F5.1,1X,F4.1,1X,I2,1X,F4.1,1X,I2, &
             1X,F6.2,1X,I5,1X,F6.1,1X,F6.1,1X,F7.1,1X,I2,1X,I2, &
             1X,I3)
        ELSE
          WRITE(KOLIST,20) TID,I,NSP(ISP(I),1)(1:2),ISP(I),IMC(I), &
          ISPECL(I),ITRE(I),P,DP,DBH(I),DGI  ,HT(I),HTG(I),ICR(I),CW, &
          IDMR,PCT(I),IPTBAL,CFV(I),WK1(I),BFV(I),ICDF,IBDF, &
          ((ITRUNC(I)+5)/100)
   20     FORMAT(A8,1X,I4,1X,A2,3(1X,I2),1X,I3,1X,F8.2,1X,F8.2,1X, &
             F5.1,1X,F5.2,1X,F5.1,1X,F4.1,1X,I2,1X,F4.1,1X,I2, &
             1X,F6.2,1X,I5,1X,F6.1,1X,F6.1,1X,F7.1,1X,I2,1X,I2, &
             1X,I3)
        ENDIF
      ELSE
          WRITE(KOLIST) TID,I,NSP(ISP(I),1)(1:2),ISP(I),IMC(I), &
          ISPECL(I),ITRE(I),P,DP,DBH(I),DGI  ,HT(I),HTG(I),ICR(I),CW, &
          IDMR,PCT(I),IPTBAL,CFV(I),WK1(I),BFV(I),ICDF,IBDF, &
          ((ITRUNC(I)+5)/100)
      ENDIF
   50 CONTINUE
   60 CONTINUE
!
!  FOR CYCLE 0 TREELIST, PRINT DEAD TREES WHICH WERE PRESENT IN
!  THE INVENTORY DATA AT THE BOTTOM OF THE TREELIST.
!
      IF((IREC2.GE.MAXTP1).OR.(ITPLAB.EQ.3).OR.(ICYC.GE.1))GO TO 200
!
      DO 150 I=IREC2,MAXTRE
!----------
!  WRITE HEADER FOR CASE WHERE NO LIVE TREES IN INVENTORY
!----------
      IF((.NOT.LTREE).AND.LFORMT.AND.LHD) THEN
      IF (LFORMT) CALL GROHED (KOLIST)
        WRITE (KOLIST,11) CLAB1(ITPLAB),NPLT,MGMID, &
                          CLAB2(IWHO),ICYC,IFINT,JYR,IP
        LTREE=.TRUE.
      ENDIF
      P =(PROB(I) / GROSPC) / (FINT/FINTM)
      IICR=((ICR(I)-1)/10)+1
      IF (IICR.GT.9) IICR=9
      WRITE(TID,'(I8)') IDTREE(I)
!----------
!     GET MISTLETOE RATING FOR CURRENT TREE RECORD.
!----------
      CALL MISGET(I,IDMR)
!----------
!     SET CROWN WIDTH.
!----------
      CW=CRWDTH(I)
!----------
!     DECODE DEFECT AND ROUND OFF POINT BAL.
!----------
      ICDF=(DEFECT(I)-((DEFECT(I)/10000)*10000))/100
      IBDF= DEFECT(I)-((DEFECT(I)/100)*100)
      IPTBAL=NINT(PTBALT(I))
!----------
!  CYCLE 0, PRINT INPUT DG ONLY, UNLESS DIRECTED TO PRINT ESTIMATES.
!----------
      DGI=DG(I)
      IF(ICYC.EQ.0 .AND. TEM(6).EQ.0) DGI=WORK1(I)
!----------
!  PUT PROB IN MORTALITY COLUMN
!----------
      DP = P
      P = 0.
!
      IF (LFORMT) THEN
        IF(P.LT.9999.9995 .AND. DP.LT.9999.9995)THEN
          WRITE(KOLIST,21) TID,I,NSP(ISP(I),1)(1:2),ISP(I),IMC(I), &
          ISPECL(I),ITRE(I),P,DP,DBH(I),DGI  ,HT(I),HTG(I),ICR(I),CW, &
          IDMR,PCT(I),IPTBAL,CFV(I),WK1(I),BFV(I),ICDF,IBDF, &
          ((ITRUNC(I)+5)/100)
        ELSE
          WRITE(KOLIST,20) TID,I,NSP(ISP(I),1)(1:2),ISP(I),IMC(I), &
          ISPECL(I),ITRE(I),P,DP,DBH(I),DGI  ,HT(I),HTG(I),ICR(I),CW, &
          IDMR,PCT(I),IPTBAL,CFV(I),WK1(I),BFV(I),ICDF,IBDF, &
          ((ITRUNC(I)+5)/100)
        ENDIF
      ELSE
          WRITE(KOLIST) TID,I,NSP(ISP(I),1)(1:2),ISP(I),IMC(I), &
          ISPECL(I),ITRE(I),P,DP,DBH(I),DGI  ,HT(I),HTG(I),ICR(I),CW, &
          IDMR,PCT(I),IPTBAL,CFV(I),WK1(I),BFV(I),ICDF,IBDF, &
          ((ITRUNC(I)+5)/100)
      ENDIF
  150 CONTINUE

  200 CONTINUE
      RETURN
      END
