!===================!
!   wpbr : brcank   !
!===================!
! WPBR $Id$
subroutine BRCANK
  implicit none

  !  Purpose:
  !   BRCANK reads the canker data provided in the keyword list or
  !   canker data file.
  !   This routine is called from the subroutine BRIN.

  !  Revision History:

  !  dd-MMM-YYYY programmer_name
  !     description of change or update.

  !  14-APR-1999 Lance R. David
  !     Added code to process comments marked with * or ! in
  !     the canker data file. * = echoed. ! = skipped.
  !  03-APR-2001 Lance R. David (FHTET)
  !     Changed order of variables and added stock type to canker file
  !     read.

  !.... Common include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'HTCAL.f90'
  include 'ESTREE.f90'
  include 'BRCOM.f90'

  !.... Local variable declarations.

  ! Variables
  character(len=80) :: CREC
  logical :: DEBUG
  integer :: IBRID,ISTK,JJ,NN
  real    :: CAGE,CNKCNT,COUT,CUP,GGIRD

  !.... See if we need to do some debug.

  call DBCHK(DEBUG,'BRCANK',6,ICYC)
  if (DEBUG) write(JOSTND,10) ICYC,ICIN,ICFMT
  10 format('Entering subroutine BRCANK: cycle = ',I2,/,&
       &' ICIN=',I3,' ICFMT=',A)

  !.... Read canker records. Note that all canker records will be read but
  !.... a maximum of 10 cankers/tree will be tracked in the lethal canker
  !.... array.  Also, a total canker count record is required and must be
  !.... the last record listed for the tree.  This count will be set in
  !.... the total canker count array for the tree but not loaded into the
  !.... Blister Rust canker arrays.
  !.... Tree's age is set to Prognosis stand age in subroutine BRSOR if
  !.... tree age is not supplied on the canker record.

  !.... Top of the canker record loop.
  !.... If reading from an external file, read until the end of the file
  !.... is reached.  If reading from supplemental records following the
  !.... CANKDATA keyword, read until a "-999" is reached.
  !.... Comments may exist in the canker data file. Records with a "*" or "!"
  !.... in column 1 are comments. Records with a "*" are written to FVS
  !.... standard output file. Records with "!" are not, just skipped.

  90 continue

  read (ICIN,91,end = 400) CREC
  91 format (A)
  if (CREC(1:1).eq.'!') goto 90
  if (CREC(1:1).eq.'*') then
     write (JOSTND,'(/,T12,A)') CREC
     goto 90
  end if

  IBRID = 0
  ISTK = 0
  CAGE = 0.0
  CUP = 0.0
  COUT = 0.0
  GGIRD = 0.0
  CNKCNT = 0.0

  !     READ(CREC,ICFMT) IBRID,UP,OUT,GGIRD,AGE,CNKCNT
  if (DEBUG) write(JOSTND,*) CREC
  read(CREC,ICFMT) IBRID,ISTK,CAGE,CUP,COUT,GGIRD,CNKCNT
  if (IBRID.eq.-999) go to 400

  !.... If necessary, convert distance up from feet to centimeters
  !.... and distance out from inches to centimeters.

  if (.not.LMETRIC) then
     CUP = CUP*30.48
     COUT = COUT*2.54
  end if

  !.... Keep track of tree records encountered. Canker data will be
  !.... loaded into the blister rust arrays according to unique
  !.... tree IDs and then sorted back into the same arrays according
  !.... to the internal tree numbers generated in the FVS model.

  if (INCAN.gt.0) then
     do JJ = 1,INCAN
        if (IBRID.eq.IBRTID(JJ)) go to 120
        110 continue
     end do
  end if
  INCAN = INCAN+1
  if (INCAN.gt.MAXTRE) call ERRGRO(.false.,13)
  IBRTID(INCAN) = IBRID
  JJ = INCAN
  120 continue

  !.... If UP, OUT, and % girdle are all zeros, this record was
  !.... used to provide the, stock type, age, and total canker count
  !.... for this tree.

  if (CUP.le.0.0.and.COUT.le.0.0.and.GGIRD.le.0.0) then

     !....    Set tree age if not 0.

     if (CAGE.ne.0.0) BRAGE(JJ) = CAGE

     !....    Set Stock type if provided.
     !....    Default stock type is 5 which will be reassigned based on
     !....    proportion of stock values in BRSTYP subroutine.

     if (ISTK.ge.1.and. ISTK.le.5) then
        ISTOTY(JJ) = ISTK
     else
        ISTOTY(JJ) = 5
     end if

     !....    ITCAN keeps a total count of cankers on the tree (lethal and
     !....    non-lethal) no matter how many canker records are entered.
     !....    Set ITCAN to the canker count number read from the canker
     !....    list.  This canker count record is required to be the last one
     !....    listed for the tree - that way we can make sure the count is
     !....    at least at large as the number of lethal cankers read in.

     if (CNKCNT.gt.0) ITCAN(JJ) = NINT(CNKCNT)
     if (ITCAN(JJ).lt.ILCAN(JJ)) ITCAN(JJ) = ILCAN(JJ)
     if (DEBUG) write(JOSTND,*) ' ID=',IBRTID(JJ),' AGE=',BRAGE(JJ),&
          &' STOCK=',ISTOTY(JJ),' ITCAN=',ITCAN(JJ),' ILCAN=',ILCAN(JJ)
     go to 90
  end if

  !.... Increment the counter for "lethal cankers".

  ILCAN(JJ) = ILCAN(JJ)+1
  NN = ILCAN(JJ)
  if (NN.gt.10) then

     !....    A maximum of 10 cankers per tree will be tracked by the model
     !....    in the "lethal cankers" array. If we've passed that maximum
     !....    then subtract one.

     ILCAN(JJ) = ILCAN(JJ)-1
     go to 90
  end if

  !.... Load the canker arrays. This data will be sorted in subroutine
  !.... BRSOR to keep the canker data in synch with FVS tree data.
  !.... Percent girdle for branch cankers is set to zero and cankers
  !.... cannot have a negative distance out.

  DUP(NN,JJ) = CUP

  if (COUT.lt.0.0) then
     DOUT(NN,JJ) = 0.0
  else
     DOUT(NN,JJ) = COUT
  end if

  if (COUT.gt.0.0) then
     GIRDL(NN,JJ) = 0.0
  else
     GIRDL(NN,JJ) = GGIRD
  end if

  !.... Go read another canker record.

  go to 90

  !.... Common return.

  400 continue
  if (DEBUG) write(JOSTND,300) ICYC
  300 format('Leaving subroutine BRCANK: cycle = ',I2)
  return
end
