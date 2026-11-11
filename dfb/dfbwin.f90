!==================!
!   dfb : dfbwin   !
!==================!
! DFB $Id$
subroutine DFBWIN(LDFBGO)
  implicit none

  !  DOUGLAS-FIR BEETLE WINDTHROW MODEL.

  !  CALLED BY :
  !     GRADD  [PROGNOSIS]

  !  CALLS :
  !     OPFIND  (SUBROUTINE)   [PROGNOSIS]
  !     OPGET   (SUBROUTINE)   [PROGNOSIS]
  !     OPDONE  (SUBROUTINE)   [PROGNOSIS]
  !     OPREDT  (SUBROUTINE)   [PROGNOSIS]
  !     OPINCR  (SUBROUTINE)   [PROGNOSIS]

  !  PARAMETERS :
  !     LDFBGO - LOGICAL VARIABLE THAT IS .TRUE. WHEN THERE IS GOING TO
  !              BE A DFB OUTBREAK IN THIS STAND IN THIS CYCLE.  (INPUT)

  !  LOCAL VARIABLES :
  !     CRASH  - USER DEFINED PROPORTION OF ELIGIBLE TREES THAT CAN BE
  !              WINDTHROWN.
  !     DF9KIL - TOTAL NUMBER OF DOUGLAS-FIR TREES/ACRE WITH A DBH >= 9
  !              INCHES THAT WERE WINDTHROWN.
  !     ELIGBL - ARRAY THAT HOLDS THE NUMBER OF TREES/ACRE ELIGIBLE FOR
  !              WINDTHROW FOR EACH TREE SPECIES.
  !     IACTK  - HOLDS THE OPTION PROCESSOR CODE FOR PARAMETER RETRIEVAL.
  !              USED IN CALLS TO OPGET.
  !     I,J    - INDEXES.
  !     I1,I2  - INDEXES FOR THE INDEX ARRAY IND1.  I1 IS THE STARTING
  !              INDEX FOR A SPECIES IN ARRAY IND1.  I2 IS THE ENDING
  !              INDEX FOR A SPECIES IN ARRAY IND1.
  !     ISPI   - TREE SPECIES INDEX.
  !     KDT    - HOLDS THE DATE THE ACTIVITY (WINDTHROW) IS SCHEDULED.
  !              RETURNED FROM OPTION PROCESSOR ROUTINE OPGET.
  !     MYACT  - HOLDS THE OPTION PROCESSOR CODES FOR DFB EVENTS.
  !              USED IN CALLS TO OPFIND.
  !     NPS    - THE NUMBER OF PARAMETERS RETURNED FROM OPTION PROCESSOR
  !              ROUTINE OPGET.
  !     NREDTS - THE NUMBER OF ACTIVITIES THAT WERE RESCHEDULED.  RETURNED
  !              FROM THE OPTION PROCESSOR ROUTINE OPREDT.
  !     NTODO  - VALUE RETURNED FROM OPFIND THAT IS GREATER THEN 1 IF
  !              THERE IS WINDTHROW SCHEDULED FOR THIS CYCLE.
  !     PRMS   - THE PARAMETERS THAT ARE RETURNED FROM OPTION PROCESSOR
  !              ROUTINE OPGET.
  !     SPCNUM - THE NUMBER OF DIFFERENT TREE SPECIES THAT ARE IN THE
  !              STAND.
  !     TELIG  - TOTAL NUMBER OF TREES/ACRE ELIGIBLE FOR WINDTHROW.
  !     THRESH - USER DEFINED MINIMUM DENSITY (TREES/ACRE) NECESSARY FOR
  !              WINDTHROW TO OCCUR.
  !     TOTKLL - TOTAL NUMBER OF TREES/ACRE THAT ARE WINDTHROWN.
  !     TOTSUC - TOTAL FOR ARRAY OF RELATIVE SUSCEPTIBILITY OF TREE
  !              SPECIES TO WINDTHROW (WINSUC).

  !  COMMON BLOCK VARIABLES USED :
  !     DBH    - (ARRAYS)  INPUT
  !     HT     - (ARRAYS)  INPUT
  !     ICYC   - (CONTRL)  INPUT
  !     IDFSPC - (DFBCOM)  INPUT
  !     IFVSSP - (DFBCOM)  INPUT
  !     IND1   - (ARRAYS)  INPUT
  !     ISCT   - (CONTRL)  INPUT
  !     IY     - (CONTRL)  INPUT
  !     JODFB  - (DFBCOM)  INPUT
  !     MAXSP  - (PRGPRM)  INPUT
  !     MWINHT - (DFBCOM)  INPUT
  !     NCYC   - (CONTRL)  INPUT
  !     OKILL  - (DFBCOM)  OUTPUT
  !     PCT    - (ARRAYS)  INPUT
  !     PROB   - (ARRAYS)  INPUT
  !     PRPMRT - (DFBCOM)  OUTPUT
  !     ROWDOM - (DFBCOM)  INPUT
  !     WINSUC - (DFBCOM)  INPUT
  !     WK2    - (ARRAYS)  OUTPUT
  !     WORKIN - (DFBCOM)  OUTPUT

  ! REVISION HISTORY:
  !   06-AUG-2001 LANCE R. DAVID (FHTET)
  !     CORRECTED ARGUMENTS TYPES IN CALL TO OPFIND.
  !     (PREVIOUS REVISION DATE WAS 12/05/95)
  !   26-SEP-2002 Lance R. David (FHTET)
  !     Added debug statements.
  !   26-JAN-2010 Lance R. David (FHTET)
  !     Changed MYACT to MYACT(1) in call to OPREDT (from Don Robinson, ESSA).

  ! Common blocks
  include  'PRGPRM.f90'

  include  'ARRAYS.f90'

  include  'CONTRL.f90'

  include  'DFBCOM.f90'

  ! Variables
  logical :: LDFBGO

  integer :: NTODO,KDT,IACTK,NPS,ISPI,I1,I2,J,I,NREDTS
  integer :: MYACT(1)

  real    :: ELIGBL(MAXSP),PRMS(2),CRASH,TELIG,THRESH,TOTSUC
  real    :: SPCNUM,DF9KIL,TOTKLL

  !.... TEST TO SEE IF WINDTHROW WAS SCHEDULED.  IF NOT THEN GO TO THE END
  !.... OF THIS ROUTINE AND RETURN.

  MYACT(1) = 2210
  call OPFIND (1,MYACT,NTODO)
  if (NTODO .le.0) goto 1000

  !.... GET THE PROPORTION TO WINDTHROW AND THE THRESHOLD VALUE FROM
  !.... THE OPTION PROCESSOR.

  call OPGET (NTODO,2,KDT,IACTK,NPS,PRMS)
  CRASH = PRMS(1)
  THRESH = PRMS(2)

  !.... INITIALIZE THE TOTAL SUSCEPTIBILITY, THE NUMBER OF SPECIES,
  !.... DF TREES GREATER THAN 9 INCH DBH, AND TOTAL TREES KILLED.

  TOTSUC = 0.0
  SPCNUM = 0.0
  DF9KIL = 0.0
  TOTKLL = 0.0

  !.... ENTER LOOP THAT CALCULATES THE NUMBER OF TREE SPECIES IN THE
  !.... STAND, THE TOTAL SUSCEPTIBILITY OF THOSE SPECIES AND THE
  !.... TREES/ACRE ELIGIBLE FOR WINDTHROW IN EACH SPECIES.

  do ISPI = 1,MAXSP
     ELIGBL(ISPI) = 0.0

     if (ISCT(ISPI,1) .gt.0) then
        SPCNUM = SPCNUM + 1.0
        TOTSUC = TOTSUC + WINSUC(IFVSSP(ISPI))

        I1 = ISCT(ISPI,1)
        I2 = ISCT(ISPI,2)

        do J = I1,I2
           I = IND1(J)

           !....          CALCULATE TREES/ACRE ELIGIBLE FOR WINDTHROW.

           if (PCT(I) .ge. ROWDOM(ISPI) .and.&
                &HT(I) .gt. MWINHT) then
              ELIGBL(ISPI) = ELIGBL(ISPI) + PROB(I)
           end if
           200 continue
        end do
     end if
     300 continue
  end do

  !.... CALCULATE THE TOTAL NUMBER OF TREES/ACRE IN THE STAND ELIGIBLE FOR
  !.... WINDTHROW.

  TELIG = 0.0
  do ISPI = 1,MAXSP
     TELIG = TELIG + ELIGBL(ISPI)
     400 continue
  end do

  if (DEBUIN) write(JODFB,*) ' IN DFBWIN: ICYC = ',ICYC,&
       &' TOTAL ELIGIBLE = ',TELIG

  !.... IF THE NUMBER OF ELIGIBLE TREES/ACRE THAT CAN BE WINDTHROWN
  !.... IS GREATER THAN THE THRESHOLD THEN A WINDTHROW WILL OCCUR.

  if (TELIG .ge. THRESH) then

     !....    TELL THE OPTION PROCESSOR THAT THE WINDTHROW OCCURED.

     call OPDONE(NTODO,IY(ICYC))

     !....    CALCULATE THE PROPORTION OF EACH TREE RECORD TO KILL BASED
     !....    ON SPECIES.

     if (DEBUIN) write(JODFB,*) ' IN DFBWIN:  CRASH = ',CRASH,&
          &' TOTSUC  = ',TOTSUC

     do ISPI = 1,MAXSP
        if (ELIGBL(ISPI) .gt.0.0) then
           PRPMRT(ISPI) = (CRASH * ELIGBL(ISPI) *&
                &(WINSUC(IFVSSP(ISPI)) /&
                &(TOTSUC / SPCNUM))) / ELIGBL(ISPI)
           if (PRPMRT(ISPI) .gt.0.95) PRPMRT(ISPI) = 0.95
           if (DEBUIN) write(JODFB,*) ' IN DFBWIN: ISPI = ',&
                &ISPI,' PRPMRT = ',PRPMRT(ISPI)
        end if

        !....       ENTER LOOP TO SET WK2 ARRAY (PROGNOSIS MORTALITY ARRAY) TO
        !....       THE TREES/ACRE KILLED BY WINDTHROW.  ONLY TREE RECORDS THAT
        !....       REPRESENT ELIGIBLE TREES WILL BE MODIFIED.

        !     Revision (RNH 03/98), to correct a run time error, IND1(0) is
        !     out of bounds.  The following IF statement skips species that
        !     are not in the tree list. This is a similar method to that used
        !     on line 132 in the origional code

        if ( ISCT(ISPI,1) .gt.0.) then

           I1 = ISCT(ISPI,1)
           I2 = ISCT(ISPI,2)

           do J = I1,I2
              I = IND1(J)
              if (PCT(I) .ge. ROWDOM(ISPI) .and.&
                   &HT(I) .gt. MWINHT) then

                 !....             THE CURRENT RECORD WILL SHOW WINDTHROW MORTALITY.
                 if (DEBUIN) write(JODFB,*)&
                      &' IN DFBWIN: BEF MRT: I = ',I,&
                      &' WK2 = ',WK2(I),' PROB = ',PROB(I)

                 WK2(I) = WK2(I) + PROB(I) * PRPMRT(ISPI)
                 if (PROB(I) - WK2(I) .lt.1E-6)&
                      &WK2(I) = PROB(I) - 1E-6

                 if (DEBUIN) write(JODFB,*)&
                      &' IN DFBWIN: AFT MRT: I = ',I,&
                      &' WK2 = ',WK2(I),' PROB = ',PROB(I)

                 !....             CALCULATE THE NUMBER OF DF TREES/ACRE WINDTHROWN THAT
                 !....             ARE GREATER THEN OR EQUAL TO 9 INCHES DBH.

                 if (ISPI .eq. IDFSPC .and. DBH(I) .ge.9.0)&
                      &DF9KIL = DF9KIL + WK2(I)

                 !....             CALCULATE THE TOTAL TREES/ACRE WINDTHROWN.

                 TOTKLL = TOTKLL + WK2(I)
              end if
              500 continue
           end do

           !     End of IF statement revision (RNH 03/98)

        end if

        600 continue
     end do

     !....    IF ENOUGH LARGE DF (DBH >= 9 INCHES) ARE KILLED THEN CHECK FOR
     !....    REGIONAL OUTBREAK.  IF REGIONAL OUTBREAK THEN ACTIVATE STAND
     !....    OUTBREAK.

     if (DF9KIL .ge.1.0) then

        !....       IF STAND OUTBREAK NOT ALREADY ACTIVATED THEN TRY TO
        !....       ACTIVATE ONE.

        if (.not. LDFBGO) then

           !....           CALL OPFIND TO SEE IF A REGIONAL OUTBREAK IS OCCURING.

           MYACT(1) = 2208
           call OPFIND (1,MYACT,NTODO)
           if (NTODO .eq.0) then
              MYACT(1) = 2209
              call OPFIND (1,MYACT,NTODO)
           end if

           if (NTODO .gt.0) then

              !....              REGIONAL OUTBREAK IS OCCURING, ACTIVATE A STAND
              !....              OUTBREAK.

              LDFBGO = .true.
              WORKIN(ICYC) = .true.
              call OPDONE (NTODO,IY(ICYC))
           end if
        end if

        !....       IF OUTBREAK ACTIVATED FOR THIS STAND IN THIS CYCLE
        !....       THEN SET OKILL TO EQUAL THE NUMBER OF LARGE DF WINDTHROWN.

        if (LDFBGO) OKILL = DF9KIL
     end if

     write (JODFB,700)
     write (JODFB,725)
     write (JODFB,750) ICYC, IY(ICYC)
     write (JODFB,775) TOTKLL
     write (JODFB,800) DF9KIL

     700 format (///,23X,'WINDTHROW MODEL')
     725 format (1X,58('-'))
     750 format (' CYCLE AND YEAR WHEN WINDTHROW OCCURED : ',I2,&
          &' : ',I4)
     775 format (' TREES/ACRE WINDTHROWN                 :  ',F8.2)
     800 format (' DF TREES/ACRE WINDTHROWN              :  ',F8.2)

  else

     !....    NO OUTBREAK!!!   RESCHEDULE EVENT FOR NEXT CYCLE.

     MYACT(1) = 2210
     call OPREDT (MYACT(1),IY(ICYC),IY(ICYC+1),NREDTS)
     call OPINCR (IY,ICYC,NCYC)
  end if

  1000 continue
  return
end
