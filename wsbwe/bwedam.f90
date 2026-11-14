!====================!
!   wsbwe : bwedam   !
!====================!
! WSBWE $Id$
subroutine BWEDAM (TOTP,TOTR,AVPRBO)
  implicit none

  !     ANNUAL FOLIAGE DAMAGE MODEL.

  !     PART OF THE WESTERN SPRUCE BUDWORM MODEL.
  !     N.L. CROOKSTON--FORESTRY SCIENCES LAB, MOSCOW, ID--JANUARY 1984
  !     TOP KILL LOGIC ADDED MARCH 1989.

  !     CALLED FROM :

  !       BWEDR - DRIVE THE BUDWORM MODEL FOR ONE STAND IN ONE YEAR.

  !     PARAMETERS :

  !     TOTP   - TOTAL POTENTIAL FOLIAGE, INPUT
  !     TOTR   - TOTAL REMAINING FOLIAGE, INPUT
  !     AVPRBO - AVERAGE PROPORTION RETAINED BIOMASS, OUTPUT

  ! Revision History:
  !   18-MAY-2000 Lance R. David (FHTET)
  !      .Previous noted dates of change 06/10/91 and 7/29/96 KA Sheehan.
  !      .Added debug handling. Debug is the only reason PRGPRM.F77
  !       and CONTROL.F77 FVS common files are included.
  !   16-OCT-2006 Lance R. David (FHTET)
  !      .Added stand ID to Damage report header.
  !   14-JUL-2010 Lance R. David (FMSC)
  !       Added IMPLICIT NONE and declared variables as needed.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'BWESTD.f90'
  include 'BWECOM.f90'

  ! Variables
  character(len=4) :: ISZC(3)
  character(len=4) :: ISPC(6)
  logical :: DEBUG
  integer :: I,ICROWN,IFIFTH,IHOST,INRUN,ISZI
  real    :: AVPRBO(6,3,2),CU,DIV,RDDS,RHTG,STHTGR(6),STREES,TOTP(6,3),TOTR(6,3),XFIFTH,XMULT

  data ISZC/'SMAL','MED','LARG'/
  data ISPC/'WF','DF','GF','AF','ES','WL'/
  data STHTGR/-2.3661,-2.4757,-2.0008,-2.3661,-2.9171,0.0/

  !     ********************** EXECUTION BEGINS **************************

  !.... Check for DEBUG

  call DBCHK(DEBUG,'BWEDAM',6,ICYC)

  if (DEBUG) write (JOSTND,*) 'ENTER BWEDAM: ICYC = ',ICYC
  if (DEBUG) write (JOSTND,*) 'IN BWEDAM: TOTP = ',TOTP
  if (DEBUG) write (JOSTND,*) 'IN BWEDAM: TOTR = ',TOTR
  if (DEBUG) write (JOSTND,*) 'IN BWEDAM: AVPRBO = ',AVPRBO

  !     CHECK THE TREES PER ACRE ARRAY, IF THERE ARE NO TREES, THEN:
  !     BRANCH TO EXIT.

  STREES = 0.0
  do IHOST = 1,6
     if (IFHOST(IHOST).ne.1) goto 20
     do ISZI = 1,3
        STREES = STREES+BWTPHA(IHOST,ISZI)
        10 continue
     end do
     20 continue
  end do
  if (STREES .eq.0.0) goto 350

  !     UPDATE THE YEAR POINTER FOR THE CUMULATIVE AND AVERAGE CURRENT
  !     DEFOLIATION ARRAYS.  THIS UPDATING LOOKS TRICKY...IT IS DESIGNED
  !     TO ALLOW CUMDEF AND APRBYR TO HOLD THE LAST 5 YEARS OF CUMULATIVE
  !     DEFOLIATION AT ONE TIME.  THE ORDER, HOWEVER IS NOT IMPORTANT AND
  !     THEREFORE NOT MAINTAINED.

  if (NCUMYR.lt.5) then
     NCUMYR = NCUMYR+1
     ICUMYR = NCUMYR
  else
     ICUMYR = ICUMYR+1
     if (ICUMYR.gt.5) ICUMYR = 1
  end if

  !     DO FOR ALL HOSTS

  do IHOST = 1,6
     if (IFHOST(IHOST).eq.0) goto 50
     do ISZI = 1,3
        if (BWTPHA(IHOST,ISZI).le.0.0) goto 30

        !     COMPUTE THE CUMULATIVE DEFOLIATION FOR THE YEAR, LOAD INTO THE
        !     APPROPRIATE SECTION OF CUMDEF FOR THIS YEAR.

        DIV = TOTP(IHOST,ISZI)
        CU = 0.
        if (DIV.gt..00001) CU = 100.-((TOTR(IHOST,ISZI)/DIV)*100.)
        if (CU.lt.0.0) CU = 0.0
        CUMDEF(IHOST,ISZI,ICUMYR) = CU

        !     COMPUTE THE AVERAGE PROPORTION OF RETAINED BIOMASS ARRAY (PRBIO).
        !     NOTE THAT ITYPE=1 IS THE TOP OF THE TREES ONLY, ITYPE=2 IS
        !     THE WHOLE TREE.

        APRBYR(IHOST,ISZI,1,ICUMYR) = PRBIO(IHOST,3*ISZI-2,1)
        APRBYR(IHOST,ISZI,2,ICUMYR) = 0.
        30 continue
     end do
     do ICROWN = 1,9
        ISZI = (ICROWN+2)/3
        if (BWTPHA(IHOST,ISZI).le.0.0) goto 40
        APRBYR(IHOST,ISZI,2,ICUMYR) = APRBYR(IHOST,ISZI,2,ICUMYR)+&
             &(PRBIO(IHOST,ICROWN,1)*.3333333)
        40 continue
     end do
     50 continue
  end do

  !     COMPUTE THE CUMULATIVE (5-YR MAX) DEFOLIATION TO DATE
  !     AND AVERAGE THE PROPORTION OF RETAINED BIOMASS (CDEF AND
  !     AVPRBO ARE INITIALIZED ZERO IN CALLING ROUTINE).

  do IHOST = 1,6
     if (IFHOST(IHOST).eq.0) goto 90
     do ISZI = 1,3
        if (BWTPHA(IHOST,ISZI).le.0.0) goto 80
        XMULT = 1./FLOAT(NCUMYR)
        do I = 1,NCUMYR
           CDEF(IHOST,ISZI) = CDEF(IHOST,ISZI)+CUMDEF(IHOST,ISZI,I)
           do ITYPE = 1,2
              AVPRBO(IHOST,ISZI,ITYPE) = AVPRBO(IHOST,ISZI,ITYPE)&
                   &+ (APRBYR(IHOST,ISZI,ITYPE,I)*XMULT)
              70 continue
           end do
        end do
        80 continue
     end do
     90 continue
  end do

  !     COMPUTE THE MAXIMUM AVERAGE DEFOLIATION.  THIS IS COMPUTED
  !     IF THE AVERAGE HAS BEEN COMPUTED FOR 4 OR MORE YEARS.  THESE
  !     DATA ARE USED IN THE TOPKILLING MODELS.

  if (NCUMYR.ge.4) then
     do IHOST = 1,6
        if (IFHOST(IHOST).eq.0) goto 190
        do ISZI = 1,3
           if (BWTPHA(IHOST,ISZI).le.0.0) goto 180
           if (AVYRMX(IHOST,ISZI).lt.1.-AVPRBO(IHOST,ISZI,1))&
                &AVYRMX(IHOST,ISZI) = 1.-AVPRBO(IHOST,ISZI,1)
           180 continue
        end do
        190 continue
     end do
  end if

  !     COMPUTE PROPORTIONAL DDS AND HTG.

  !     FOR THE MEDIUM AND LARGE TREES, THE RDDS AND RHTG MODELS
  !     CAME FROM:  NICHOLS, TOM. 1984. FINAL REPORT FOR PERSONAL
  !     CONTRACT WESTERN SPRUCE BUDWORM IMPACT MODEL CALIBRATIONS.
  !     SUBMITTED TO CANUSA/SPRUCE BUDWORMS PROGRAM-WEST.  MARCH 10 1984.

  !     SEE NICHOLS FOREST SCIENCE 34(1):236-242 AND 34(2):496-504.

  !     FOR THE SMALL TREES, THE RHTG MODEL CAME FROM DENNIS FERGUSON.
  !     SEE FERGUSON (1988) USDA RESEARCH PAPER INT-393.
  !     THIS MODEL MAY ONLY BE APPLIED ON 5-YR INTERVALS...SO STICK TO
  !     5-YR CHUNKS UNLESS YOU NEED A LITTLE AT THE END FOR THE LAST
  !     FEW YEARS OF A CYCLE.  THIS IS ACCOMPLISHED BY COMPUTING XFIFTH
  !     TO SCALL THE PROPORTION OF HEIGHT GROWTH FOR SMALL TREES.

  XFIFTH = 0.0
  INRUN = IYRCUR-IBWYR1+1
  IFIFTH = MOD(INRUN,5)
  if (IFIFTH.eq.0) XFIFTH = 5.0/BWFINT
  if (IFIFTH.ne.0.and. IYRCUR.eq.IBWYR2)&
       &XFIFTH = FLOAT(IFIFTH)/BWFINT

  do IHOST = 1,6
     if (IFHOST(IHOST).eq.0) goto 240
     do ISZI = 1,3
        if (BWTPHA(IHOST,ISZI).le.0.0) goto 230
        RDDS = .083861*(AVPRBO(IHOST,ISZI,2)*100.)**(.4725+.07*&
             &RDDSM1(IHOST,ISZI))*RDDSM1(IHOST,ISZI)**.3241
        if (RDDS.gt.1.) RDDS = 1.0
        RDDSM1(IHOST,ISZI) = RDDS
        RHTG = .193013*(AVPRBO(IHOST,ISZI,1)*100.)**(.3814-.0212*&
             &RHTGM1(IHOST,ISZI))*RHTGM1(IHOST,ISZI)**.5509
        if (RHTG.gt.1.) RHTG = 1.0
        RHTGM1(IHOST,ISZI) = RHTG
        PEDDS(IHOST,ISZI) = PEDDS(IHOST,ISZI)+(RDDS/BWFINT)

        !     IF THE TREE SIZE IS MEDIUM OR LARGE, USE NICHOLS MODEL

        if (ISZI.gt.1) then
           PEHTG(IHOST,ISZI) = PEHTG(IHOST,ISZI)+(RHTG/BWFINT)
        else

           !        THE TREE IS SMALL, COMPUTE HEIGHT GROWTH REDUCTION USING
           !        DENNIS FERGUSON'S MODEL.

           RHTG = 1.0
           if (AVPRBO(IHOST,ISZI,2).lt.0.98) RHTG = EXP(STHTGR(IHOST)*&
                &(1.0-AVPRBO(IHOST,ISZI,1)))
           PEHTG(IHOST,ISZI) = PEHTG(IHOST,ISZI) + (RHTG*XFIFTH)
        end if

        !     SAVE THE MAX CUM DEFOLIATION.  MUST BE GE 5 YRS INTO THE RUN
        !     OR ON THE LAST YEAR OF THE RUN.

        if (INRUN.ge.5.or. IYRCUR .eq. IBWYR2) then
           if (BWMXCD(IHOST,ISZI).lt.CDEF(IHOST,ISZI))&
                &BWMXCD(IHOST,ISZI) = CDEF(IHOST,ISZI)
        end if
        230 continue
     end do
     240 continue
  end do

  !     WRITE DAMAGE VALUES.
  !     ONLY WRITE THE DAMAGE VALUES IF:
  !        THEY ARE REQUESTED,
  !        THE PROJECTION IS AT THE END OF A CYCLE OR PERIOD, AND
  !        THE PROJECTION HAS LASTED A MULTIPLE OF 5 YEARS.

  if (.not.LBWDAM) goto 350
  if (IFIFTH.ne.0.and. IYRCUR.ne.IBWYR2) goto 350
  if (LBWDAM)  write (JOWSBW,250) IYRCUR,NPLT,INRUN
  250 format (/'DAMAGE: ',I4,'; ',A26,'; PERIOD LENGTH TO DATE: ',&
       &I2,'-YEARS'/,47('-'),'  NEW-FOLIAGE DEFOLIATION'&
       &/T13,'PROPORTION OF EXPECTED',T50,&
       &'PERIOD AVERAGE  5-YR MAX'/T13,'PERIODIC GROWTH',T37,&
       &'MAXIMUM',T50,14('-'),'  AVERAGE'/T7,'TREE  ',22('-'),&
       &T37,'CUMULATIVE',T50,'TOP',T59,'WHOLE',T66,'TOP'/&
       &' HOST  SIZE  DIAMETER',T29,'HEIGHT  DEFOLIATION',T50,&
       &'THIRD',T59,'TREE   THIRD'/'----  ----  --------',&
       &T29,'------  -----------  -------  -----  --------')

  !     COMPUTE A SCALING VALUE (XMULT) APPROPREATE TO SCALE THE RATES
  !     STORED IN PEDDS, PEHTG, AND BWMXCD TO END-OF-PERIOD VALUES.
  !     (THIS IS DONE JUST FOR REPORTING PURPOSES).

  XMULT = BWFINT/FLOAT(INRUN)

  !     WRITE THE BODY OF THE OUTPUT TABLE.

  do IHOST = 1,6
     if (IFHOST(IHOST).eq.0) goto 310
     do ISZI = 1,3
        if (BWTPHA(IHOST,ISZI).le.0.0) goto 300
        RDDS = PEDDS(IHOST,ISZI)*XMULT
        RHTG = PEHTG(IHOST,ISZI)*XMULT
        if (LBWDAM) write (JOWSBW,290) ISPC(IHOST),ISZC(ISZI),RDDS,&
             &RHTG,BWMXCD(IHOST,ISZI),(1.-AVPRBO(IHOST,ISZI,I),I = 1,2),&
             &AVYRMX(IHOST,ISZI)
        290 format (1X,A2,3X,A4,T14,F6.3,T29,F6.3,T38,F7.1,T49,F6.3,T58,&
             &F6.3,T66,F6.3)
        300 continue
     end do
     310 continue
  end do
  350 continue

  if (DEBUG) write (JOSTND,*) 'IN BWEDAM: AVYRMX = ',AVYRMX
  if (DEBUG) write (JOSTND,*) 'IN BWEDAM: AVPRBO = ',AVPRBO
  if (DEBUG) write (JOSTND,*) 'EXIT BWEDAM: ICYC= ',ICYC

  return
end
