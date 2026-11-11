!=================!
!   rd : rdowin   !
!=================!
! RD $Id$
subroutine RDOWIN
  implicit none

  !  Purpose :
  !     Subroutine that simulates windthrow.

  !  Called By :
  !     RDOAGM  [ROOT DISEASE]

  !  Calls :
  !     RDSUM   (SUBROUTINE)   [ROOT DISEASE]
  !     OPFIND  (SUBROUTINE)   [PROGNOSIS]
  !     OPGET   (SUBROUTINE)   [PROGNOSIS]
  !     OPDONE  (SUBROUTINE)   [PROGNOSIS]
  !     OPREDT  (SUBROUTINE)   [PROGNOSIS]
  !     OPINCR  (SUBROUTINE)   [PROGNOSIS]

  !  Arguments :

  !  Local Variables :
  !     <incomplete>

  !  Common Block Variables Used :
  !     IIEND  - (RDCOM)   (O)
  !     ILEND  - (RDCOM)   (O)
  !     ISTEMI - (RDARRY)  (O)
  !     ISTEML - (RDARRY)  (O)
  !     STEMSI - (RDCOM)   (O)
  !     STEMSL - (RDCOM)   (O)
  !     <incomplete>

  !  Revision History :
  !    06/12/96 - Matthew K. Thompson
  !       Created this subroutine from the entry point that
  !       was in RDOWI.
  !    26-MAR-2002 Lance R. David (FHTET)
  !       Changed dimension on PROBUN from 1350 to FVS parameter MAXTRE.
  !    14-DEC-2007 Lance R. David (FHTET)
  !       Changed MYACT to MYACT(1) in call to OPREDT (from Don Robinson).
  !   08/29/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'

  !.... Common include files.

  include 'CONTRL.f90'
  include 'RDCOM.f90'
  include 'RDARRY.f90'
  include 'ARRAYS.f90'
  include 'RDADD.f90'

  !.... Local variable declarations.

  ! Variables
  integer :: I,I1,I2,IACTK,IFL,IK,IP,ISPI,J,KDT,MYACT(1),NPS,NREDTS,NTODO,RRTYPE

  real    :: CRASH,THRESH,PRMS(2),PROBUN(MAXTRE),REMNO,RN,RRTEMP,RRTEMS,SNAP,TBPROB,TT,TTIU,TTO,ZAREA

  !.... Data statements.

  data MYACT /2414/

  if (ITRN .le.0) return

  !.... Check to see if windthrow is scheduled.

  call OPFIND (1,MYACT,NTODO)
  if (NTODO .le.0) return

  !.... Get windthrow parameters from the event monitor.

  call OPGET (NTODO,2,KDT,IACTK,NPS,PRMS)
  CRASH = PRMS(1)
  THRESH = PRMS(2)

  ILEND = 0
  IIEND = 0
  do ISPI = 1,MAXSP
     do IK = 1,4
        WINDSP(ISPI,IK) = 0.0
        21 continue
     end do
     22 continue
  end do

  do ISPI = 1,MAXSP
     STEMSL(ISPI) = 0
     STEMSI(ISPI) = 0
     ROCRL(ISPI) = 0
     ROCRI(ISPI) = 0

     if (ISCT(ISPI,1) .eq.0) goto 30

     !....    NB: IF PCT N/A THEN HEIGHT SORT HERE

     I1 = ISCT(ISPI,1)
     I2 = ISCT(ISPI,2)
     RRTYPE = MAXRR

     do J = I1,I2
        I = IND1(J)
        if (PCT(I) .lt. ROWDOM(IRTSPC(ISPI))) goto 29

        !....       LIVE UNINFECTED STEMS (AS A NUMBER OF STEMS)

        if (MAXRR .lt.3) RRTYPE = IDITYP(IRTSPC(ISPI))

        !     Exit loop for non-host trees (RNH June98)

        if (RRTYPE .le.0) go to 29

        ZAREA = SAREA - PAREA(RRTYPE)
        PROBUN(I) = PROBIU(I) + FPROB(I) * ZAREA

        if (PROBUN(I) .eq.0.0) goto 25
        ILEND = ILEND + 1
        ISTEML(ILEND) = I
        STEMSL(ISPI) = STEMSL(ISPI) + PROBUN(I)
        WINDWL(I) = ICR(I) * PROBUN(I)
        ROCRL(ISPI) = ROCRL(ISPI) + WINDWL(I)

        25 continue

        !....       INFECTED STEMS. Susceptibility is increased by the
        !....       proportion of root system infected: 1+PROPI(I,IK)

        IFL = 0
        RRTEMS = 0

        do IK = 1,ISTEP
           do IP = 1,2
              if (PROBI(I,IK,IP) .eq.0) goto 26
              IFL = 1
              STEMSI(ISPI) = STEMSI(ISPI) + PROBI(I,IK,IP)
              RRTEMP = ICR(I) * PROBI(I,IK,IP) *&
                   &(1 + PROPI(I,IK,IP))
              RRTEMS = RRTEMS + RRTEMP
              26 continue
           end do
           27 continue
        end do

        if (IFL .eq.0) goto 29
        IIEND = IIEND + 1
        ISTEMI(IIEND) = I
        ROCRI(ISPI) = ROCRI(ISPI) + RRTEMS
        WINDWI(I) = RRTEMS
        29 continue
     end do
     30 continue
  end do

  !.... End of species loop.

  TSTEMS = 0.0
  TBPROB = 0.0

  do ISPI = 1,MAXSP
     WINDSP(ISPI,3) = STEMSL(ISPI) + STEMSI(ISPI)
     TSTEMS = TSTEMS + WINDSP(ISPI,3)
     WINDSP(ISPI,3) = WINDSP(ISPI,3) * ROWIBP(IRTSPC(ISPI),2)
     TBPROB = TBPROB + WINDSP(ISPI,3)
     WINDSP(ISPI,1) = 0.0
     WINDSP(ISPI,2) = 0.0
     35 continue
  end do

  !.... Total number of stems to windthrow

  WINDN = TSTEMS * CRASH
  if (WINDN .gt. TSTEMS) WINDN = TSTEMS

  !.... If the number of stems windthrown is less then the
  !.... specified minimum then the event does not occur.

  if ((WINDN / SAREA) .ge. THRESH) goto 38

  do ISPI = 1,MAXSP
     do IK = 1,4
        WINDSP(ISPI,IK) = 0.0
        36 continue
     end do
     37 continue
  end do

  WINDN = 0.0
  goto 777

  38 continue
  call OPDONE(NTODO,IY(ICYC))

  !.... Allocate to species.

  do ISPI = 1,MAXSP
     WINDSP(ISPI,3) = WINDN * (WINDSP(ISPI,3) / TBPROB)
     if (ROCRL(ISPI) .eq.0.and. ROCRI(ISPI) .eq.0) goto 40
     WINDSP(ISPI,1) = WINDSP(ISPI,3) * (ROCRL(ISPI) / (ROCRL(ISPI)&
          &+ ROCRI(ISPI)))
     WINDSP(ISPI,2) = WINDSP(ISPI,3) - WINDSP(ISPI,1)
     40 continue
  end do

  !.... Now allocate these proportions back to the tree lists
  !....
  !.... Live uninfected trees.

  RRTYPE = MAXRR
  do J = 1,ILEND
     I = ISTEML(J)
     ISPI = ISP(I)
     TT = WINDSP(ISPI,1) * WINDWL(I) / ROCRL(ISPI)

     !....    INSIDE UNINFECTED TREES - THESE ARE A NUMBER

     TTIU = TT * PROBIU(I) / PROBUN(I)
     if (TTIU .gt. PROBIU(I) * 0.95) TTIU = PROBIU(I) * 0.95
     OAKL(DSIU,I) = OAKL(DSIU,I) + TTIU

     !....    OUTSIDE (UNINFECTED) TREES - FPROB IS A DENSITY
     !....           BUT OAKL IS KEPT AS A NUMBER

     if (MAXRR .lt.3) RRTYPE = IDITYP(IRTSPC(ISPI))

     !     If non-host trees exit loop

     if (RRTYPE .le.0) go to 50

     ZAREA = SAREA - PAREA(RRTYPE)

     !     When SAREA = PAREA, ZAREA > 0.0 and causes devide by zero error
     !     in statement below.  Therefore, assign ZAREA a small number
     !     when SAREA = PAREA (RNH, MAR98)

     if (ZAREA .le.0.0) ZAREA = 1.0E-6

     TTO = (TT - TTIU) / ZAREA
     if (TTO .gt. FPROB(I) * 0.95) TTO = FPROB(I) * 0.95
     OAKL(DSO,I) = OAKL(DSO,I) + TTO * ZAREA

     !....    UPDATE FFPROB FOR THE EFFECTS OF WINDTHROW

     if (ZAREA .gt.1E-6)&
          &FFPROB(I,2) = FFPROB(I,2) - (OAKL(DSO,I) / ZAREA)

     50 continue
  end do

  !.... INFECTED TREES. NOTE THAT THE ACTUAL REMOVAL OF INFECTED TREES TAKES PLACE
  !.... IN RDOAGM AFTER THE BARK BEETLES HAVE ACTED (SINCE THEY WANT
  !.... TO ASSUME THAT THESE TREES ARE STILL ALIVE)

  do J = 1,IIEND
     I = ISTEMI(J)
     ISPI = ISP(I)
     SNAP = 0.0

     do IK = 1,ISTEP
        do IP = 1,2
           if (PROBI(I,IK,IP) .eq.0) goto 60
           RN = PROBI(I,IK,IP)

           !....          REMNO - NUMBER OF STEMS TO REMOVE
           !....          SNAP  - NUMBER OF STEMS WHICH WERE BROKEN OFF?

           REMNO = WINDSP(ISPI,2) * WINDWI(I) / ROCRI(ISPI)
           if (REMNO .gt. RN * .95) REMNO = RN * 0.95
           SNAP = SNAP + (REMNO * (1 - PROPI(I,IK,IP)))
           PROBI(I,IK,IP) = RN - REMNO
           RRKILL(I) = RRKILL(I) + REMNO
           OAKL(DSII,I) = OAKL(DSII,I) + REMNO
           60 continue
        end do
        65 continue
     end do

     70 continue
  end do

  ROWIND = 0

  goto 888

  !.... If no windthrow then reschedule event for next cycle.

  777 continue
  call OPREDT (MYACT(1),IY(ICYC),IY(ICYC+1),NREDTS)
  call OPINCR (IY,ICYC,NCYC)

  888 continue
  return
end
