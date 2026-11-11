!========================!
!   fire/base : fmsout   !
!========================!
subroutine FMSOUT (IYR)
  implicit none

  ! FIRE-BASE

  !     SINGLE-STAND VERSION
  !     CALLED FROM: FMMAIN
  !     CALLS:   FMSVOL

  !  PURPOSE:
  !     PRINT THE SNAG LIST.

  !  CALL LIST DEFINITIONS:
  !     IYR:     CURRENT YEAR

  !  LOCAL VARIABLE DEFINITIONS:
  !     SNVOLH, SNVOLS:  total volume of CURRENTLY-hard and CURRENTLY-soft
  !                      snags, respectively, in each snag record
  !     TOTDH, TOTDS:    total density of all CURRENTLY-hard and CURRENTLY-
  !                      soft snags in each snag output class
  !     TOTVLH, TOTVLS:  total volume of all CURRENTLY-hard and CURRENTLY-
  !                      soft snags in each snag output class
  !     TOTHTH, TOTHTS:  average height of all CURRENTLY hard and soft snags
  !                      in each output snag class

  !  COMMON BLOCK VARIABLES AND PARAMETERS:

  !.... PARAMETER STATEMENTS.

  !.... PARAMETER INCLUDE FILES.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'

  !.... COMMON INCLUDE FILES.

  !sng  INCLUDE 'CONTRL.F77'
  !sng  INCLUDE 'PLOT.F77'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'FMCOM.f90'
  include 'FMFCOM.f90'

  !.... VARIABLE DECLARATIONS.
  ! Variables
  integer :: YRLAST,JYR,II
  real    :: SNVOLS,SNVOLH,TEMPV
  real    :: TOTDH(MAXSP,100,6),TOTDS(MAXSP,100,6)
  real    :: TOTHTH(MAXSP,100,6),TOTHTS(MAXSP,100,6)
  real    :: TOTVLS(MAXSP,100,6),TOTVLH(MAXSP,100,6)
  real    :: TOTDBH(MAXSP,100,6)
  real    :: TOTN
  real    :: PRMS(4)
  logical :: DEBUG,LOK
  integer :: MYACT(1)
  data MYACT/2512/
  integer :: IYR,NTODO,JDO,NPRM,IACTK,IDC,JCL,DBSKODE

  !     CHECK FOR DEBUG.

  call DBCHK (DEBUG,'FMSOUT',6,ICYC)
  if (DEBUG) write(JOSTND,7) ICYC,IYR
  7 format(' ENTERING FMSOUT CYCLE = ',I2,' IYR=',I5)

  !     FIRST CHECK TO SEE IF THE SNAG LIST IS TO BE PRINTED.

  call OPFIND(1,MYACT,NTODO)

  do JDO = 1,NTODO
     call OPGET(JDO,4,JYR,IACTK,NPRM,PRMS)
     !         IF (JYR .NE. IYR) GOTO 5
     ISNAGB = IYR
     ISNAGE = INT(real(IYR) + PRMS(1))
     JSNOUT = INT(PRMS(3))
     LSHEAD = PRMS(4).eq.0
     call OPDONE(JDO,IYR)
     goto 6
     5 continue
  end do
  6 continue
  if (DEBUG) write(JOSTND,9) ISNAGB,ISNAGE
  9 format(' FMSOUT: ISNAGB=',I5,'; ISNAGE=',I5)

  !     CHECK TO MAKE SURE THAT THIS YEAR IS WITHIN THE REQUESTED REPORTING
  !     PERIOD AND THAT IT IS A VALID YEAR (IF WE ARE USING A PRINTING INTERVAL)

  if (IYR .eq.0.and. IYR.eq. ISNAGB) goto 10
  if (IYR .lt. ISNAGB .or. IYR .gt. ISNAGE) return

  10 continue

  !     ZERO OUT THE CUMULATIVE VARIABLES

  do JYR = 1,100
     do IDC = 1,MAXSP
        do JCL = 1,6
           TOTDH(IDC,JYR,JCL) = 0.0
           TOTDS(IDC,JYR,JCL) = 0.0
           TOTHTH(IDC,JYR,JCL) = 0.0
           TOTHTS(IDC,JYR,JCL) = 0.0
           TOTDBH(IDC,JYR,JCL) = 0.0
           TOTVLS(IDC,JYR,JCL) = 0.0
           TOTVLH(IDC,JYR,JCL) = 0.0
        end do
     end do
  end do
  YRLAST = -1
  do II = 1, NSNAG

     !        Skip this snag record if there are no snags in it or the snags
     !        are too small to make it into the smallest snag printing class.

     if ( ((DENIH(II)+DENIS(II)) .le.0.0)&
          &.or. (DBHS(II) .lt. SNPRCL(1)) ) goto 100

     !        Get the total volume of initially-soft snags in this record.

     SNVOLS = 0.0
     if (DENIS(II) .gt.0.0) then
        call FMSVOL (II, HTIS(II), TEMPV,.false.,0)
        SNVOLS = TEMPV * DENIS(II)
     end if

     !        Get the volume of each initially-hard snag in this record, and
     !        add their combined volume to either the currently-hard or
     !        currently-soft totals, whichever is apppropriate.

     SNVOLH = 0.0
     if (DENIH(II) .gt.0.0) then
        call FMSVOL (II, HTIH(II), TEMPV,.false.,0)

        if (HARD(II)) then
           SNVOLH = TEMPV * DENIH(II)
        else
           SNVOLS = SNVOLS + TEMPV * DENIH(II)
        end if
     end if

     !        Determine what snag printing-class this record goes in (on the
     !        basis of species, dhb and age)...

     JYR = IYR - YRDEAD(II) + 1

     if (JYR .gt.100)       JYR = 100
     if (JYR .gt. YRLAST) YRLAST = JYR

     do JCL = 1,5
        if (DBHS(II) .lt. SNPRCL(JCL+1)) goto 81
        80 continue
     end do
     JCL = 6
     81 continue

     !        ...and add all its snags to the appropriate class totals.

     TOTDS(SPS(II),JYR,JCL) = TOTDS(SPS(II),JYR,JCL) + DENIS(II)
     TOTHTS(SPS(II),JYR,JCL) = TOTHTS(SPS(II),JYR,JCL) +&
          &HTIS(II) * DENIS(II)

     if (HARD(II)) then
        TOTDH(SPS(II),JYR,JCL) = TOTDH(SPS(II),JYR,JCL) + DENIH(II)
        TOTHTH(SPS(II),JYR,JCL) = TOTHTH(SPS(II),JYR,JCL) +&
             &HTIH(II) * DENIH(II)
     else
        TOTDS(SPS(II),JYR,JCL) = TOTDS(SPS(II),JYR,JCL) + DENIH(II)
        TOTHTS(SPS(II),JYR,JCL) = TOTHTS(SPS(II),JYR,JCL) +&
             &HTIH(II) * DENIH(II)
     end if

     TOTVLS(SPS(II),JYR,JCL) = TOTVLS(SPS(II),JYR,JCL) + SNVOLS
     TOTVLH(SPS(II),JYR,JCL) = TOTVLH(SPS(II),JYR,JCL) + SNVOLH

     TOTDBH(SPS(II),JYR,JCL) = TOTDBH(SPS(II),JYR,JCL) +&
          &DBHS(II) * (DENIS(II) + DENIH(II))

     100 continue
  end do

  do JYR = 1,YRLAST
     do IDC = 1,MAXSP
        do JCL = 1,6
           TOTN = TOTDH(IDC,JYR,JCL) + TOTDS(IDC,JYR,JCL)
           if (TOTN .eq.0.0) goto 110
           TOTDBH(IDC,JYR,JCL) = TOTDBH(IDC,JYR,JCL) / TOTN
           if (TOTDH(IDC,JYR,JCL) .gt.0.0) then
              TOTHTH(IDC,JYR,JCL) = TOTHTH(IDC,JYR,JCL) /&
                   &TOTDH(IDC,JYR,JCL)
           else
              TOTHTH(IDC,JYR,JCL) = 0.0
           end if

           if (TOTDS(IDC,JYR,JCL) .gt.0.0) then
              TOTHTS(IDC,JYR,JCL) = TOTHTS(IDC,JYR,JCL) /&
                   &TOTDS(IDC,JYR,JCL)
           else
              TOTHTS(IDC,JYR,JCL) = 0.0
           end if

           110 continue
        end do
        120 continue
     end do
     130 continue
  end do

  !     CALL THE DBS MODULE TO OUTPUT DETAILED SNAG REPORT TO A DATABASE

  DBSKODE = 1
  call DBSFMDSNAG(IYR,TOTDBH,TOTHTH,TOTHTS,TOTVLH,&
       &TOTVLS,TOTDH,TOTDS,YRLAST,DBSKODE)
  if (DBSKODE.eq.0) goto 500

  !     Make sure JSNOUT is openned.

  call openIfClosed (JSNOUT,"sng",LOK)
  if (.not.LOK) return

  !     Print the snag output headings.

  if (LSHEAD) then
     write(JSNOUT,200) NPLT
     write(JSNOUT,222)
     write(JSNOUT,210)
     write(JSNOUT,211)
     write(JSNOUT,220)
     write(JSNOUT,222)
     200 format(' ESTIMATED SNAG CHARACTERISTICS ',&
          &'(BASED ON STOCKABLE AREA), STAND ID=',A)
     210 format(13X,'DEATH CURR',&
          &' HEIGHT CURR VOLUME (FT3)      ',&
          &'   DENSITY (SNAGS/ACRE)  ')
     211 format(9X,'DBH  DBH ',1X,&
          &4('-'),'(FT)',3('-'),1X,17('-'),1X,'YEAR',1X,23('-'))
     220 format(' YEAR SP  CL  (IN)',1X,&
          &' HARD  SOFT  HARD  SOFT TOTAL',&
          &1X,'DIED   HARD    SOFT    TOTAL')
     222 format(1X,76('-'))
     LSHEAD = .false.
  end if

  !     Print information on each snag printing-class, first dividing the
  !     the total heights and dbhs to get the class-averages.

  do JYR = 1,YRLAST
     do IDC = 1,MAXSP
        do JCL = 1,6
           TOTN = TOTDH(IDC,JYR,JCL) + TOTDS(IDC,JYR,JCL)
           if (TOTN .eq.0.0) goto 410

           write(JSNOUT,300) IYR,JSP(IDC),JCL,&
                &TOTDBH(IDC,JYR,JCL),&
                &TOTHTH(IDC,JYR,JCL), TOTHTS(IDC,JYR,JCL),&
                &INT(TOTVLH(IDC,JYR,JCL)), INT(TOTVLS(IDC,JYR,JCL)),&
                &INT(TOTVLH(IDC,JYR,JCL)+TOTVLS(IDC,JYR,JCL)),&
                &(IYR-JYR+1), TOTDH(IDC,JYR,JCL), TOTDS(IDC,JYR,JCL),&
                &TOTN
           300 format(1X,I4,1X,A2,1X,I3,1X,3(F5.1,1X),&
                &3(I5,1X),I4,1X,3(F7.2,1X))
           410 continue
        end do
        420 continue
     end do
     430 continue
  end do

  inquire(UNIT = JSNOUT,OPENED = LOK)
  if (LOK) close(UNIT = JSNOUT)

  500 continue

  return
end
