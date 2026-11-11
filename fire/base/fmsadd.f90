!========================!
!   fire/base : fmsadd   !
!========================!
! FIRE-BASE $Id$
subroutine FMSADD (YEAR,ITYP)
  implicit none

  ! PURPOSE:
  !     ADDS NEW SNAGS TO THE SNAG LIST, AVERAGING THE APPROPRIATE
  !     SNAGS INTO ONE RECORD FOR EACH SPECIES-, DBH- AND HEIGHT-
  !     CLASS.

  !     **NOTE: THIS ROUTINE ASSUMES THAT MIN AND
  !     MAX HEIGHTS FOR EACH CLASS HAVE ALREADY BEEN CALCULATED, AND THAT
  !     IT WILL BE PASSED VARIABLES CONTAINING ALL NECCESSARY INFO.
  !     **NOTE:  There is no information on cause-of-death at this time.

  !     CALLED FROM: FMEFF
  !                  FMKILL
  !                  FMSDIT  [SERIAL FVS ONLY]
  !                  FMSCUT
  !                  FMSNAG

  !     CALLS:       FMSCRO
  !                  FMCBIO

  !     Local variable definitions:
  !     DENJ:   total DENsity of snags in record J
  !     DBHCUT: the dbh-class number of a snag class considered for disposal
  !     GAPS:   indicates whether there might be some 'gaps' in the snag
  !             list (i.e., previously used records that are now empty)
  !     HTCUT:  the height-class number of a snag class considered for disposal
  !     IGAP:   number of the last empty snag record found
  !     ITYP:   Shows where the routine is called from.  This is so that during
  !             initialization, the loop will go over the entire MAXTRE list.
  !             <0=keyword,1=after a fire,2=after a cut,3=initialization,
  !              4=nat mort
  !     LOSSES: the total number of times that existing snags are overwritten
  !             or new snags discarded during this subroutine call.
  !     MIDHT:  half-way point between MAXHT and MINHT
  !     MINDEN: the minimum density of snags found in any existing record
  !     RECORD: the record number being used for each species- and dbh-class
  !             of new snags
  !     SPCUT:  the species number of a snag class considered for disposal
  !     TAKEN:  indicates whether a snag record is already TAKEN by one of the
  !             new snag classes (0 = not taken)
  !     UNFIRE: the density of snags in the current record that were NOT killed
  !             by fire
  !     YEAR:   the year of death of all the snags to be added during this
  !             subroutine call (during initialization, this may be more than
  !             one year before the next year that will be simulated).

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'FMCOM.f90'
  include 'FMFCOM.f90'

  ! Variables
  real    :: DENJ,MIDHT(MAXSP,19),MINDEN,TOTDEN,UNFIRE
  real    :: ABIO,MBIO,RBIO,XDCAY
  integer :: I,J,YEAR,X,DBH2,SP2,HT2,LOSSES
  integer :: DBHCUT,HTCUT,SPCUT
  integer :: SPCL,DBHCL,HTCL,RECORD(MAXSP,19,2),NHTCL
  integer :: OLDITN,ITYP,IGAP,TAKEN(MXSNAG)
  logical :: GAPS,DEBUG
  real    :: PRMS(6)
  integer :: NPRM,IACTK,JYR

  call DBCHK (DEBUG,'FMSADD',6,ICYC)
  if (DEBUG) write(JOSTND,7) ICYC, YEAR, LFMON, ITYP, ITRN
  7 format(' FMSADD CYCLE=',I2,' YEAR=',I5,' LFMON=',L2,&
       &' ITYP=',I5,' ITRN=',I5)

  !     IF THE FIRE MODEL EXTENSION IS NOT ACTIVE, THEN RETURN

  if (.not. LFMON) return

  !     Initialize some variables

  LOSSES = 0
  GAPS = .true.
  if (NSNAG .le.3) GAPS = .false.
  IGAP = 1

  do I = 1,MXSNAG
     TAKEN(I) = 0
  end do

  do SPCL = 1,MAXSP
     do DBHCL = 1,19
        MIDHT(SPCL,DBHCL) = 0.0
        do HTCL = 1,2
           RECORD(SPCL,DBHCL,HTCL) = 0
        end do
     end do
  end do

  !     Look at each snag species+dbh class.  If there are snags in the class,
  !     and some of them have > 0 height, then check whether the class needs
  !     to be broken into 2 height classes.  (This means those classes that
  !     have a height range of more than 20 feet).

  do SPCL = 1,MAXSP
     do DBHCL = 1,19

        if ((DSPDBH(SPCL,DBHCL) .le.0.0) .or.&
             &(MAXHT(SPCL,DBHCL) .le.0.0)) goto 35

        NHTCL = 1

        if ((MAXHT(SPCL,DBHCL)-MINHT(SPCL,DBHCL)) .gt.20.0) then
           MIDHT(SPCL,DBHCL) = (MAXHT(SPCL,DBHCL) +&
                &MINHT(SPCL,DBHCL)) / 2.0
           NHTCL = 2
        end if

        !         Try to find a record in which to store each species+dbh+height
        !         class of snags.

        do I = 1,NHTCL

           !         First, if you've never tried it or it's worked before, look
           !         for a previously-used record where all the snags have fallen.

           if (GAPS) then
              do J = IGAP,NSNAG
                 DENJ = DENIH(J) + DENIS(J)
                 if ((DENJ .le.0.0) .and. (TAKEN(J) .le.0)) then
                    RECORD(SPCL,DBHCL,I) = J
                    TAKEN(J) = 1
                    IGAP = J
                    goto 29
                 end if
                 21 continue
              end do
              GAPS = .false.
           end if

           !           If that didn't work, and some the available records have never
           !           been used, then just use the next new record.

           if (NSNAG .lt. MXSNAG) then
              NSNAG = NSNAG + 1
              RECORD(SPCL,DBHCL,I) = NSNAG
              TAKEN(NSNAG) = 1
              goto 29
           end if

           !***********THIS PART IS TO DECIDE WHICH SNAGS TO OVERWRITE*************

           !           Otherwise, look through all the possible records from the
           !           beginning.  Figure out which has the fewest snags left in it.
           !           (note:  don't count empty records that you've already TAKEN)

           MINDEN = 99999.9

           do J = 1,MXSNAG
              DENJ = DENIH(J) + DENIS(J)
              if ((DENJ .lt. MINDEN) .and. (TAKEN(J) .le.0)) then
                 MINDEN = DENJ
                 RECORD(SPCL,DBHCL,I) = J
              end if
              22 continue
           end do

           !           If the sp-dbh class that is
           !           looking for a place has as many snags (with its 2 possible
           !           height classes combined) as the existing record with the
           !           fewest snags, then it gets to overwrite that existing record.

           if (DSPDBH(SPCL,DBHCL) .ge. MINDEN) then
              TAKEN(RECORD(SPCL,DBHCL,I)) = 1
              goto 27
           end if

           !           OK, so it still doesn't have a place.  Find out which of the
           !           new snag classes that have already been assigned records has
           !           the fewest snags.

           MINDEN = 99999.9

           do SP2 = 1,SPCL
              do DBH2 = 1,19
                 do HT2 = 1,2
                    if ((RECORD(SP2,DBH2,HT2) .gt.0) .and.&
                         &(DSPDBH(SP2,DBH2) .lt. MINDEN)) then
                       MINDEN = DSPDBH(SP2,DBH2)
                       SPCUT = SP2
                       DBHCUT = DBH2
                       HTCUT = HT2
                    end if
                 end do
              end do
           end do

           !           If this unlucky class has fewer snags than the one still
           !           looking for a record, then confiscate its record.  Otherwise,
           !           it's the new snags that get discarded.

           if (DSPDBH(SPCL,DBHCL) .gt. MINDEN) then
              RECORD(SPCL,DBHCL,I) = RECORD(SPCUT,DBHCUT,HTCUT)
              RECORD(SPCUT,DBHCUT,HTCUT) = 0
           else
              RECORD(SPCL,DBHCL,I) = 0
           end if

           !           When over-writing a record with remaining snags or discarding
           !           new snags, monitor the problem.

           27 continue
           if (LOSSES .eq.0) write (*,*)&
                &'OLD SNAGS OVERWRITTEN OR NEW SNAGS DISCARDED.'
           LOSSES = LOSSES + 1

           !           possible future work: if snags are being overwritten,
           !           we perhaps should get the
           !           overwritten snags and add them to the debris pools.

           !***********THIS IS THE END OF THE DECIDING-T0-OVERWRITE SECTION************

           !           If this snag class earned a record, then initialize the
           !           selected record, and note the species class that will
           !           be associated with it.

           29 continue
           X = RECORD(SPCL,DBHCL,I)
           if (X .gt.0) then
              DEND(X) = 0.0
              DBHS(X) = 0.0
              HTDEAD(X) = 0.0
              SPS(X) = SPCL
              HARD(X) = .true.
              YRDEAD(X) = YEAR
           end if

           30 continue
        end do
        35 continue
     end do
     40 continue
  end do

  !     Loop over all the tree records, calculating snag averages for each
  !     sp+dbh+ht class.
  !     ITYP=3 means that the routine is called during initialization, and needs
  !     to loop over all possible trees (since the ones it wants are between
  !     MAXTRE and ITRN.

  if (ITYP .eq.3) then
     OLDITN = ITRN
     ITRN = MAXTRE
  end if

  !     If this routine is called from keyword initialization, then skip the
  !     whole loop over the tree list.

  if (ITYP .lt.0) goto 51

  do I = 1,ITRN

     !       Skip tree records with no snags.

     if (SNGNEW(I) .le.0.0) goto 50

     !       Find the sp, dbh and ht class of the snags in this record.

     SPCL = ISP(I)

     if (DBH(I) .ge.36.0) then
        DBHCL = 19
     else
        DBHCL = INT( (DBH(I)/2.0) + 1.0)
     end if

     if (MIDHT(SPCL,DBHCL) .le.0.0) then
        HTCL = 1
     else
        if (HT(I) .lt. MIDHT(SPCL,DBHCL)) then
           HTCL = 1
        else
           HTCL = 2
        end if
     end if

     !       Collect all the crown components of the snags NOT killed by fire
     !       into the pools in CWD2B that will become down debris at the
     !       appropriate times in the future. (this has already been done
     !       for the fire-killed snags, to capture their fire-altered canopy
     !       weights)

     if (DEBUG) write (JOSTND,45) I,SNGNEW(I),FIRKIL(I),HTCL,&
          &SPCL,DBHCL,RECORD(SPCL,DBHCL,HTCL)
     45 format (' IN FMSADD, I=',I4,' SNGNEW=',F7.3,' FIRKIL=',F7.3,&
          &' HTCL=',I2,' SPCL=',I2,' DBHCL=',I2,' RECORD=',I6)

     if (SNGNEW(I).gt.FIRKIL(I)) then
        UNFIRE = SNGNEW(I) - FIRKIL(I)
     else
        UNFIRE = 0.0
     end if

     call FMSCRO(I,SPCL,YEAR,UNFIRE,ITYP)

     !       CALCULATE THE ROOT BIOMASS OF SNAGS. WHEN READING FROM
     !       STAND INVENTORY, ASSUME SNAGS HAVE BEEN DEAD 10 YEARS
     !       FOR CALCULATING DECAY OF DEAD ROOTS

     call FMCBIO(DBH(I), ISP(I), ABIO, MBIO, RBIO)
     XDCAY = 1.0
     if (ITYP .eq.3) then
        XDCAY = 1.0
        if (CRDCAY .gt.0.0) then
           XDCAY = (1.0 - CRDCAY)**10
        end if
     end if
     BIOROOT = BIOROOT + (RBIO * SNGNEW(I) * XDCAY)

     !       Move on to the next tree record if the snags in this one did not
     !       earn a record in the snag list (due to being too rare or having
     !       0 height).

     if (RECORD(SPCL,DBHCL,HTCL) .eq.0) goto 50

     !       Average the new snags in with others in their class.  Sum the
     !       densities last. If snag is truncated on FVS input, set height
     !       at death to NORMAL (FVS dubbed) height.

     X = RECORD(SPCL,DBHCL,HTCL)
     TOTDEN = DEND(X) + SNGNEW(I)
     if (ITYP.eq.3) then
        HTDEAD(X) = (HTDEAD(X) * DEND(X) +&
             &MAX(HT(I),NORMHT(I)*.01) * SNGNEW(I)) / TOTDEN
     else
        HTDEAD(X) = (HTDEAD(X) * DEND(X) + HT(I) * SNGNEW(I)) / TOTDEN
     end if
     DBHS(X) = (DBHS(X) * DEND(X) + DBH(I) * SNGNEW(I)) / TOTDEN
     DEND(X) = TOTDEN

     !      If snag is truncated on input to FVS, set actual height here

     if (ITYP.eq.3) then
        if (ITRUNC(I).gt.0) then
           HTIH(X) = ITRUNC(I)*.01
           HTIS(X) = ITRUNC(I)*.01
        else
           HTIH(X) = HTDEAD(X)
           HTIS(X) = HTDEAD(X)
        end if
     end if

     !       Zero out the temporary snag array (This probably isn't really
     !       necessary to do here once we get the get and put running, but
     !       it doesn't hurt anything.)

     SNGNEW(I) = 0.0

     50 continue
  end do
  51 continue

  if (ITYP .eq.3) ITRN = OLDITN

  !     Set up the snag record entered from the keyword. Note that right now
  !     only one snag can be entered this way. This is a start, and we can
  !     see what can be done later.

  if (ITYP .lt.0) then
     J = -ITYP
     call OPGET(J,6,JYR,IACTK,NPRM,PRMS)
     SPCL = INT(PRMS(1))
     do DBHCL = 1,19
        do HTCL = 1,2
           X = RECORD(SPCL,DBHCL,HTCL)
           if (X .le.0) goto 164
           DBHS(X) = PRMS(2)
           HTDEAD(X) = PRMS(3)
           DEND(X) = PRMS(6)

           !           CALCULATE THE ROOT BIOMASS OF THESE SNAGS, TAKING INTO ACCOUNT
           !           YEARS DEAD, IF THE KEYWORDS ARE ORDERED CORRECTLY.

           call FMCBIO(PRMS(2), SPCL, ABIO, MBIO, RBIO)
           XDCAY = 1.0
           if (CRDCAY .gt.0.0.and. PRMS(5) .gt.0.0) then
              XDCAY = (1.0 - CRDCAY)**PRMS(5)
           end if
           BIOROOT = BIOROOT + RBIO * PRMS(6) * XDCAY

           goto 162
           164 continue
        end do
        162 continue
     end do
  end if

  !     Set variables for simulated snag height equal to height at time of death,
  !     divide total snags into initially-hard and initially-soft ones, and
  !     re-set the max- and min-ht arrays so they're ready for the next batch
  !     of snags. (Note that PSOFT is now by species (Feb 2002))

  do SPCL = 1,MAXSP
     do DBHCL = 1,19
        MAXHT(SPCL,DBHCL) = 0.0
        MINHT(SPCL,DBHCL) = 1000.0
        DSPDBH(SPCL,DBHCL) = 0.0

        do HTCL = 1,2
           X = RECORD(SPCL,DBHCL,HTCL)
           if (X .gt.0) then
              if (DEND(X) .gt.0) then

                 !     If snag initalized from FVS treelist current height
                 !     may be different from ht at death

                 if (ITYP.ne.3) then
                    HTIH(X) = HTDEAD(X)
                    HTIS(X) = HTDEAD(X)
                 end if
                 DENIH(X) = (1.0 - PSOFT(SPCL)) * DEND(X)
                 DENIS(X) = PSOFT(SPCL) * DEND(X)
                 if (DEBUG) write (JOSTND,170) X,DEND(X),DENIH(X),&
                      &DENIS(X)
                 170 format (' IN FMSADD, X=',I5,' DEND=',F10.3,&
                      &' DENIH=',F10.3,' DENIS=',F10.3)

                 !               Note that if this was entered via keyword
                 !               then current ht may be different from ht at death:

                 if (ITYP .lt.0) then
                    HTIH(X) = PRMS(4)
                    HTIS(X) = PRMS(4)
                 end if
              end if
           end if
        end do
     end do
  end do

  if (LOSSES .gt.0) then
     write (*,222) LOSSES
     222 format(' THIS HAPPENED ',I4,' TIMES.')
  end if

  return
end
