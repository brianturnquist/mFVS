!=================!
!   rd : rdspup   !
!=================!
! RD $Id$
subroutine RDSPUP (I,ISL,IISP,TP,DIAM,RTD)
  implicit none

  !  Purpose :
  !     Adds spore infected stumps to spore stump arrays (If they are
  !     big enough to be centers). Boraxing also occurs here.

  !  Called By :
  !     RDSTR   [ROOT DISEASE]

  !  Calls :
  !     DBCHK   (SUBROUTINE)  [PROGNOSIS]

  !  Arguments :
  !     I      -
  !     ISL    -
  !     IISP   -
  !     TP     -
  !     DIAM   -
  !     RTD    -

  !  Local Variables :

  !  Common Block Variables  Used :
  !     xxxxx:   From ANCOM;

  !  Revision History :
  !     04/03/97 Matt Thompson (FHTET)
  !               Modified the code that stores the information about
  !               when the stand entry occurred.
  !     14-JAN-00 Lance R. David (FHTET)
  !               Removed literals from option processor calls and replaced
  !               with references to MYACT array. Expanded MYACT array to
  !               include activity code 2431.
  !     08-AUG-01 Lance R. David (FHTET)
  !               Declaration and initialization of variable NTODO.
  !   09/03/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'

  !.... Common include files.

  include 'CONTRL.f90'
  include 'RDCOM.f90'
  include 'RDARRY.f90'
  include 'RDADD.f90'

  !.... Local variable declarations.

  ! Variables
  logical :: DEBUG
  integer :: I,IACTK,ICASE,IDI,IISP,IJ,IS,ISL,JRRTYP,KDT,MYACT(2),NPS,NTODO
  real    :: ADJBOR,DENOUT,DENUIN,DIAM,OUTTOT,PRMS(5),RTD,TP,UINTOT

  !.... Data statements.

  data MYACT / 2430, 2431 /

  call DBCHK(DEBUG,'RDSPUP',6,ICYC)

  NTODO = 0
  IDI = MAXRR
  if (MAXRR .lt.3) IDI = IDITYP(IRTSPC(IISP))
  IS = ISPS(IRTSPC(IISP))
  ADJBOR = 0.0
  DENUIN = 0.0
  DENOUT = 0.0
  OUTTOT = 0.0
  UINTOT = 0.0

  !.... Set flags to specify that a stand entry occurred and the year
  !.... in which it occured.

  LSPFLG(1) = .true.
  ISDATE(1) = IY(ICYC)

  !.... Proportion of stumps to receive borax.

  !.... Only call the option processor the first time this routine is
  !.... called.  LBORAX is set to .FALSE. within the root disease model
  !.... after this routine has been called. The first time in a cycle
  !.... that the option processor is called LBORAX is set to .TRUE. to
  !.... indicate the option processor has been called already and the
  !.... variables have been set for this cycle.
  !     (ACTIVITY CODE 2430)

  if (.not. LBORAX(2)) then

     if (LBORAX(1)) call OPFIND (1,MYACT(1),NTODO)
     LBORAX(2) = .true.

     if (NTODO .gt.0) then
        if (NTODO .gt.1) then

           do ICASE = 2, NTODO
              call OPDEL1(ICASE)
              13 continue
           end do
        end if

        call OPGET (1,2,KDT,IACTK,NPS,PRMS)

        BOTRT = PRMS(1)
        BODBH = PRMS(2)

        call OPDONE (1,IY(ICYC))

     end if

     !....    Also call the option processor to see if there are any
     !....    changes to the spore variables (We just need SPDBH at this
     !....    point, this call will be repeated elsewhere in case the user
     !....    wanted to change some parameter in a year that there was no
     !....    harvest)
     !        (ACTIVITY CODE 2431)

     call OPFIND (1,MYACT(2),NTODO)

     if (NTODO .gt.0) then

        do IJ = 1, NTODO
           call OPGET (IJ,5,KDT,IACTK,NPS,PRMS)
           JRRTYP = INT(PRMS(5))
           SPINF(JRRTYP) = PRMS(1)
           SPDBH(JRRTYP) = PRMS(2)
           23 continue
        end do
     end if
  end if

  if (LBORAX(1) .and. DIAM .ge. BODBH) ADJBOR = BOTRT

  !.... Only spore infect those stumps that are big enough to
  !.... become centers.

  if (DIAM .lt. SPDBH(IDI)) goto 70

  !.... Update outside spore stump arrays.

  DENOUT = FPROB(I) * (1.0-TP) * (SAREA-PAREA(IDI)) *&
       &SPINF(IDI) * (1.0-ADJBOR)

  if (DENOUT .le.1E-4) goto 50

  OUTTOT = DENOUT + STOUT(IDI,IS,ISL,1)

  DBHOUT(IDI,IS,ISL,1) = (DBHOUT(IDI,IS,ISL,1) *&
       &STOUT(IDI,IS,ISL,1) + DIAM * DENOUT) / OUTTOT
  RTOUT(IDI,IS,ISL,1) = (RTOUT(IDI,IS,ISL,1) *&
       &STOUT(IDI,IS,ISL,1) + RTD * DENOUT) / OUTTOT
  STOUT(IDI,IS,ISL,1) = STOUT(IDI,IS,ISL,1) + DENOUT

  !.... Update inside spore stump arrays.

  50 continue
  DENUIN = PROBIU(I) * (1.0-TP) * SPINF(IDI) * (1.0-ADJBOR)

  if (DENUIN .le.1E-4) goto 70

  UINTOT = DENUIN + STUIN(IDI,IS,ISL,1)

  DBHUIN(IDI,IS,ISL,1) = (DBHUIN(IDI,IS,ISL,1) *&
       &STUIN(IDI,IS,ISL,1) + DIAM * DENUIN) / UINTOT
  RTUIN(IDI,IS,ISL,1) = (RTUIN(IDI,IS,ISL,1) *&
       &STUIN(IDI,IS,ISL,1) + RTD * DENUIN) / UINTOT
  STUIN(IDI,IS,ISL,1) = STUIN(IDI,IS,ISL,1) + DENUIN

  70 continue
  if (.not. DEBUG) goto 101
  write(JOSTND,*) 'SAREA PAREA FPROB PROBIU',SAREA,PAREA(IDI),&
       &FPROB(I), PROBIU(I)
  write(JOSTND,*) 'BOTRT ADJBOR SPINF', BOTRT, ADJBOR, SPINF(IDI)
  write(JOSTND,*) 'I ISL IISP TP DIAM RTD'
  write(JOSTND,*) I,ISL,IISP,TP,DIAM,RTD
  write(JOSTND,*) 'IDI IS ISL', IDI, IS, ISL
  write(JOSTND,*) 'DENOUT OUTTOT DENUIN UINTOT',DENOUT,OUTTOT,&
       &DENUIN, UINTOT
  write(JOSTND,*) 'DBHOUT', DBHOUT(IDI,IS,ISL,1)
  write(JOSTND,*) 'DBHUIN', DBHUIN(IDI,IS,ISL,1)
  write(JOSTND,*) 'RTOUT', RTOUT(IDI,IS,ISL,1)
  write(JOSTND,*) 'RTUIN', RTUIN(IDI,IS,ISL,1)
  write(JOSTND,*) 'STOUT', STOUT(IDI,IS,ISL,1)
  write(JOSTND,*) 'STUIN', STUIN(IDI,IS,ISL,1)
  101 continue

  return
end
