      SUBROUTINE CROWN
      use plot_mod
      use arrays_mod
      use contrl_mod
      use coeffs_mod
      use outcom_mod
      use pden_mod
      use varcom_mod
      use prgprm_mod
      implicit none
C----------
C  CS $ID
C----------
C  THIS SUBROUTINE IS USED TO DUB MISSING CROWN RATIOS AND
C  COMPUTE CROWN RATIO CHANGES FOR TREES.  THE EQUATION USED
C  PREDICTS CROWN RATIO FROM BASAL AREA AND TREE DIAMETER.
C  THIS EQUATION AND THE COEFFICIENTS ARE FROM 'A GUIDE TO
C  THE TWIGS PROGRAM FOR THE NORTH CENTRAL U.S.', GENERAL
C  TECHNICAL REPORT, NC-125.  THIS ROUTINE IS CALLED FROM
C  **CRATET** TO DUB MISSING VALUES, AND BY **TREGRO** TO
C  COMPUTE CHANGE DURING REGULAR CYCLING.  ENTRY **CRCONS**
C  IS CALLED BY **RCON** TO LOAD MODEL CONSTANTS THAT ARE SITE
C  DEPENDENT AND NEED ONLYBE RESOLVED ONCE.  PROCESSING OF
C  CROWN CHANGE FOR SMALL TREES IS CONTROLLED BY **REGENT**.
C----------
COMMONS
C
      LOGICAL DEBUG
      REAL CRNEW(MAXTRE),PRM(5),BCR1(MAXSP),BCR2(MAXSP),
     &  BCR3(MAXSP),BCR4(MAXSP),
     &  CRNMLT(MAXSP),DLOW(MAXSP),DHI(MAXSP)
      INTEGER MYACTS(1),ICFLG(MAXSP)
      INTEGER JJ,NTODO,I,NP,IACTK,IDATE,IDT,ISPCC,IGRP,IULIM,IG,IGSP
      INTEGER ISPC,I1,I2,I3,ICRI
      REAL D,DEN,CHG,PDIFPY,CRLN,CRMAX,HN,HD,CL,CR
      DATA MYACTS/81/
C----------
C  SPECIES SPECIFIC COEFFICIENTS
C----------
      DATA BCR1/
     &   2*4.0862089,5*3.8228678,2*5.3257794,4*3.5959606,10*4.0006783,
     &   3.7331997,3*4.741949,4.5859519,3*4.7334243,3.7331997,
     &   8*4.2114308,2*3.7331997,4.5227706,3*4.741949,4.6206732,
     &   2*4.694145,5.6002061,4.1572614,3.6371464,4*4.1897395,
     &   2*3.6935554,5.8825245,8*1.9729133,10*3.7331997,7*4.5859519,
     &   12*4.3509789/
      DATA BCR2/
     &   2*.0095531519,5*.015547623,2*.005878536,4*.024136635,
     &   10*.013160288,.0040149456,3*.074791783,.0045270093,
     &   3*.0051453631,.0040149456,8*.00056984711,2*.0040149456,
     &   .0048820773,3*.074791783,.0041936343,2*.005722237,.0072431708,
     &   .01052377,.0095653844,4*.0089825601,2*.0038755164,.0082426992,
     &   8*.037413753,10*.0040149456,7*.0045270093,12*.001538945/
      DATA BCR3/
     &   2*4.2295403,5*3.6701435,2*187.86445,4*3.3784606,10*3.2410676,
     &   3.6320568,3*3.3270486,4.2754048,3*1.5490425,3.6320568,
     &   8*2.4916599,2*3.6320568,2.3242952,3*3.3270486,2.627181,
     &   2*2.0464785,1.7133082,2.6184562,3.058426,4*3.3907386,
     &   2*2.7331608,332.98337,8*5.3149546,10*3.6320568,7*4.2754048,
     &   12*110.67094/
      DATA BCR4/
     &  2*.65539293,5*.093072521,2*.00032406501,4*.56072582,
     &  10*1.0553702,.041241734,3*.87105638,.019433807,3*.1920458,
     &  .041241734,8*.026575414,2*.041241734,.22893377,3*.87105638,
     &  .16836497,2*.23262439,.16631347,.46232062,.60476971,
     &  4*.15662809,2*.23391404,.00021921946,8*1.0758145,10*.041241734,
     &  7*.019433807,12*.0015352834/
C-----------
C  SEE IF WE NEED TO DO SOME DEBUG.
C-----------
      CALL DBCHK (DEBUG,'CROWN',5,ICYC)
      IF(DEBUG) WRITE(JOSTND,3)ICYC
    3 FORMAT(' ENTERING SUBROUTINE CROWN  CYCLE =',I5)
C----------
C INITIALIZE CROWN VARIABLES TO BEGINNING OF CYCLE VALUES.
C----------
      IF(LSTART)THEN
        DO 10 JJ=1,MAXTRE
        CRNEW(JJ)=0.0
   10   CONTINUE
      ENDIF
C----------
C  DUB CROWNS ON DEAD TREES IF NO LIVE TREES IN INVENTORY
C----------
      IF((ITRN.LE.0).AND.(IREC2.LT.MAXTP1))GO TO 74
C----------
C IF THERE ARE NO TREE RECORDS, THEN RETURN
C----------
      IF(ITRN.EQ.0)THEN
        RETURN
      ELSEIF(TPROB.LE.0.0)THEN
        DO I=1,ITRN
        ICR(I)=ABS(ICR(I))
        ENDDO
        RETURN
      ENDIF
C-----------
C  PROCESS CRNMULT KEYWORD.
C-----------
      CALL OPFIND(1,MYACTS,NTODO)
      IF(NTODO .EQ. 0)GO TO 25
      DO 24 I=1,NTODO
      CALL OPGET(I,5,IDATE,IACTK,NP,PRM)
      IDT=IDATE
      CALL OPDONE(I,IDT)
      ISPCC=IFIX(PRM(1))
C----------
C  ISPCC<0 CHANGE FOR ALL SPECIES IN THE SPECIES GROUP
C  ISPCC=0 CHANGE FOR ALL SPEICES
C  ISPCC>0 CHANGE THE INDICATED SPECIES
C----------
      IF(ISPCC .LT. 0)THEN
        IGRP = -ISPCC
        IULIM = ISPGRP(IGRP,1)+1
        DO 21 IG=2,IULIM
        IGSP = ISPGRP(IGRP,IG)
        IF(PRM(2) .GE. 0.0)CRNMLT(IGSP)=PRM(2)
        IF(PRM(3) .GT. 0.0)DLOW(IGSP)=PRM(3)
        IF(PRM(4) .GT. 0.0)DHI(IGSP)=PRM(4)
        IF(PRM(5) .GT. 0.0)ICFLG(IGSP)=1
   21   CONTINUE
      ELSEIF(ISPCC .EQ. 0)THEN
        DO 22 ISPCC=1,MAXSP
        IF(PRM(2) .GE. 0.0)CRNMLT(ISPCC)=PRM(2)
        IF(PRM(3) .GT. 0.0)DLOW(ISPCC)=PRM(3)
        IF(PRM(4) .GT. 0.0)DHI(ISPCC)=PRM(4)
        IF(PRM(5) .GT. 0.0)ICFLG(ISPCC)=1
   22   CONTINUE
      ELSE
        IF(PRM(2) .GE. 0.0)CRNMLT(ISPCC)=PRM(2)
        IF(PRM(3) .GT. 0.0)DLOW(ISPCC)=PRM(3)
        IF(PRM(4) .GT. 0.0)DHI(ISPCC)=PRM(4)
        IF(PRM(5) .GT. 0.0)ICFLG(ISPCC)=1
      ENDIF
   24 CONTINUE
   25 CONTINUE
      IF(DEBUG)WRITE(JOSTND,9024)ICYC,CRNMLT
 9024 FORMAT(/' IN CROWN 9024 ICYC,CRNMLT= ',
     & I5/((1X,11F6.2)/))
C----------
C  ENTER THE LOOP FOR SPECIES DEPENDENT VARIABLES
C----------
      DO 70 ISPC=1,MAXSP
      I1 = ISCT(ISPC,1)
      IF(I1 .EQ. 0) GO TO 70
      I2 = ISCT(ISPC,2)
      DO 60 I3=I1,I2
      I = IND1(I3)
C----------
C  IF THIS IS THE INITIAL ENTRY TO 'CROWN' AND THE TREE IN QUESTION
C  HAS A CROWN RATIO ASCRIBED TO IT, THE WHOLE PROCESS IS BYPASSED.
C----------
      IF(LSTART .AND. ICR(I).GT.0)GOTO 60
C----------
C  IF ICR(I) IS NEGATIVE, CROWN RATIO CHANGE WAS COMPUTED IN A
C  PEST DYNAMICS EXTENSION.  SWITCH THE SIGN ON ICR(I) AND BYPASS
C  CHANGE CALCULATIONS.
C----------
      IF (LSTART) GOTO 40
      IF (ICR(I).GE.0) GO TO 40
      ICR(I)=-ICR(I)
      IF (DEBUG) WRITE (JOSTND,35) I,ICR(I)
   35 FORMAT (' ICR(',I4,') WAS CALCULATED ELSEWHERE AND IS ',I4)
      GOTO 60
   40 CONTINUE
      D=DBH(I)
      DEN=1+BCR2(ISPC)*BA
      CRNEW(I)=10*(BCR1(ISPC)/DEN+BCR3(ISPC)*(1-EXP(-1.*BCR4(ISPC)*D)))
C----------
C  COMPUTE THE CHANGE IN CROWN RATIO
C CALC THE DIFFERENCE BETWEEN THE MODEL AND THE OLD(OBS)
C  LIMIT CHANGE TO 1% PER YEAR
C----------
      IF(LSTART .OR. ICR(I).EQ.0) GO TO 9052
      CHG=CRNEW(I) - ICR(I)
      PDIFPY=CHG/ICR(I)/FINT
      IF(PDIFPY.GT.0.01)CHG=ICR(I)*(0.01)*FINT
      IF(PDIFPY.LT.-0.01)CHG=ICR(I)*(-0.01)*FINT
      IF(DEBUG)WRITE(JOSTND,9020)I,CRNEW(I),ICR(I),PDIFPY,CHG
 9020 FORMAT(/'  IN CROWN 9020 I,CRNEW,ICR,PDIFPY,CHG =',
     &I5,F10.3,I5,3F10.3)
      IF(DBH(I).GE.DLOW(ISPC) .AND. DBH(I).LE.DHI(ISPC))THEN
        CRNEW(I) = ICR(I) + CHG * CRNMLT(ISPC)
      ELSE
        CRNEW(I) = ICR(I) + CHG
      ENDIF
 9052 ICRI = CRNEW(I)+0.5
      IF(LSTART .OR. ICR(I).EQ.0)THEN
        IF(DBH(I).GE.DLOW(ISPC) .AND. DBH(I).LE.DHI(ISPC))
     &    ICRI = ICRI * CRNMLT(ISPC)
      ENDIF
C----------
C CALC CROWN LENGTH NOW
C----------
      IF(LSTART .OR. ICR(I).EQ.0)GO TO 55
      CRLN=HT(I)*ICR(I)/100.
C----------
C CALC CROWN LENGTH MAX POSSIBLE IF ALL HTG GOES TO NEW CROWN
C----------
      CRMAX=(CRLN+HTG(I))/(HT(I)+HTG(I))*100.0
      IF(DEBUG)WRITE(JOSTND,9004)CRMAX,CRLN,ICRI,I,CRNEW(I),
     & CHG
 9004 FORMAT(' CRMAX=',F10.2,' CRLN=',F10.2,
     &       ' ICRI=',I10,' I=',I5,' CRNEW=',F10.2,' CHG=',F10.3)
C----------
C IF NEW CROWN EXCEEDS MAX POSSIBLE LIMIT IT TO MAX POSSIBLE
C----------
      IF(ICRI.GT.CRMAX) ICRI=CRMAX+0.5
      IF(ICRI.LT.10 .AND. CRNMLT(ISPC).EQ.1.0)ICRI=CRMAX+0.5
C----------
C  REDUCE CROWNS OF TREES  FLAGGED AS TOP-KILLED ON INVENTORY
C----------
   55 IF (.NOT.LSTART .OR. ITRUNC(I).EQ.0) GO TO 59
      HN=NORMHT(I)/100.0
      HD=HN-ITRUNC(I)/100.0
      CL=(FLOAT(ICRI)/100.)*HN-HD
      ICRI=IFIX((CL*100./HN)+.5)
      IF(DEBUG)WRITE(JOSTND,9030)I,ITRUNC(I),NORMHT(I),HN,HD,
     & ICRI,CL
 9030 FORMAT(' IN CROWN 9030 I,ITRUNC,NORMHT,HN,HD,ICRI,CL = ',
     & 3I5,2F10.3,I5,F10.3)
      GO TO 59
C----------
C  BALANCING ACT BETWEEN TWO CROWN MODELS OCCURS HERE
C  END OF CROWN RATIO CALCULATION LOOP.  BOUND CR ESTIMATE AND FILL
C  THE ICR VECTOR.
C----------
   59 CONTINUE
      IF(ICRI.GT.95) ICRI=95
      IF (ICRI .LT. 10 .AND. CRNMLT(ISPC).EQ.1) ICRI=10
      IF(ICRI.LT.1)ICRI=1
      ICR(I)= ICRI
   60 CONTINUE
      IF(LSTART .AND. ICFLG(ISPC).EQ.1)THEN
        CRNMLT(ISPC)=1.0
        ICFLG(ISPC)=0
      ENDIF
   70 CONTINUE
   74 CONTINUE
C----------
C  DUB MISSING CROWNS ON CYCLE 0 DEAD TREES.
C----------
      IF(IREC2 .GT. MAXTRE) GO TO 80
      DO 79 I=IREC2,MAXTRE
      IF(ICR(I) .GT. 0) GO TO 79
      ISPC=ISP(I)
      D=DBH(I)
      DEN=1+BCR2(ISPC)*BA
      CR =10*(BCR1(ISPC)/DEN+BCR3(ISPC)*(1-EXP(-1.*BCR4(ISPC)*D)))
      ICRI=CR + 0.5
      IF(ITRUNC(I).EQ.0) GO TO 78
      HN=NORMHT(I)/100.0
      HD=HN-ITRUNC(I)/100.0
      CL=(FLOAT(ICRI)/100.)*HN-HD
      ICRI=IFIX((CL*100./HN)+.5)
   78 CONTINUE
      IF(ICRI.GT.95) ICRI=95
      IF (ICRI .LT. 10) ICRI=10
      ICR(I)= ICRI
   79 CONTINUE
C
   80 CONTINUE
      IF(DEBUG)WRITE(JOSTND,9010)ITRN,(ICR(JJ),JJ=1,ITRN)
 9010 FORMAT(' LEAVING CROWN 9010 FORMAT ITRN,ICR= ',I10,/,
     & 43(1H ,32I4,/))
      IF(DEBUG)WRITE(JOSTND,90)ICYC
   90 FORMAT(' LEAVING SUBROUTINE CROWN  CYCLE =',I5)
      RETURN
      ENTRY CRCONS
      DATA CRNMLT/MAXSP*1.0/
      DATA ICFLG/MAXSP*0/
      DATA DLOW/MAXSP*0.0/
      DATA DHI/MAXSP*99.0/
      RETURN
      END
