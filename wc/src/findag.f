      SUBROUTINE FINDAG(I,ISPC,D,D2,H,SITAGE,SITHT,AGMAX1,HTMAX,HTMAX2,
     &                  DEBUG)
      use siteht_mod, only: get_siteht
      IMPLICIT NONE
C----------
C  **FINDAG--WC  DATE OF LAST REVISION:  01/12/11
C----------
C  THIS ROUTINE FINDS EFFECTIVE TREE AGE BASED ON INPUT VARIABLE(S)
C  CALLED FROM **COMCUP
C  CALLED FROM **CRATET
C  CALLED FROM **HTGF
C----------
C  COMMONS
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'PLOT.F77'
C----------
C  DECLARATIONS
C----------
      LOGICAL DEBUG
      INTEGER I,ISPC,MAPPHD,MAPHD(MAXSP)
      REAL AGMAX(MAXSP),AG,age_delta,DIFF,H,HGUESS,SINDX,TOLER
      REAL HDRAT1(8),HDRAT2(8)
      REAL SITAGE,SITHT,AGMAX1,HTMAX,HTMAX2,D,D2
C----------
C  DATA STATEMENTS
C----------
      DATA AGMAX  / MAXSP*200. /
      DATA MAPHD  /3*1,2*2,6,2,2*3,2,4,4*5,2*6,3,2*7,8*8,5*3,4*8,2*6/
      DATA HDRAT1 /4.3396271,4.3149844,3.2412923,2.3475244,
     &             5.5324838,6.3657425,4.0156013,3.9033821/
      DATA HDRAT2 /43.9957174,39.6317079,62.7139427,65.7622908,
     &             18.6043842,16.2223589,51.9732476,59.3370816/
C----------
C  INITIALIZATIONS
C----------
      TOLER=2.0
      SINDX = SITEAR(ISPC)
      AGMAX1 = AGMAX(ISPC)
      AG = 100.0
      age_delta = 49.0
      MAPPHD=MAPHD(ISPC)
      HTMAX=HDRAT1(MAPPHD)*D+HDRAT2(MAPPHD)
      HTMAX2=HDRAT1(MAPPHD)*D2+HDRAT2(MAPPHD)
C----------
C  CRATET CALLS FINDAG AT THE BEGINING OF THE SIMULATION TO
C  CALCULATE THE AGE OF INCOMMING TREES.  AT THIS POINT ABIRTH(I)=0.
C  THE AGE OF INCOMMING TREES HAVING H>=HMAX IS CALCULATED BY
C  ASSUMEING A GROWTH RATE OF 0.10FT/YEAR FOR THE INTERVAL H-HMAX.
C  TREES REACHING HMAX DURING THE SIMULATION ARE IDENTIFIED IN HTGF.
C----------
      IF(H .GE. HTMAX) THEN
        SITAGE = AGMAX1 + (H - HTMAX)/0.10
        SITHT = H
        IF(DEBUG)WRITE(JOSTND,*)' H,HTMAX,AGMAX1,SITAGE,SITHT= ',
     &  H,HTMAX,AGMAX1,SITAGE,SITHT
        GO TO 30
      ENDIF
C
   75 CONTINUE
C----------
C  CALL HTCALC TO CALCULATE POTENTIAL HT GROWTH
C----------
!      hguess = htcalc(SINDX,ISPC,AG,JOSTND,DEBUG)

      ! Use a binary search to find the site age for this tree
      sitht = 0.0
      sitage = 2.0

      do while (age_delta >= 2)

        call get_siteht(sindx,ispc,ag,hguess)
!        call htcalc(sindx,ispc,ag,hguess,0,0)

        ! We're done if the current height is within the search tolerance
        if (abs(hguess-h) .le. toler) then
            sitage = ag
            sitht = hguess
            exit

        elseif (hguess .gt. h) then
            ag = ag - age_delta

        else
            ag = ag + age_delta

        endif

        age_delta = age_delta * 0.5

      end do

      if (sitht == 0.0 .and. hguess > h) then
        sitht = h
        sitage = agmax1
      endif

   30 CONTINUE
      IF(DEBUG)WRITE(JOSTND,50)I,SITAGE,SITHT
   50 FORMAT(' LEAVING SUBROUTINE FINDAG  I,SITAGE,SITHT =',
     &I5,2F10.3)
C
      RETURN
      END
C**END OF CODE SEGMENT
