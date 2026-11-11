!=================================!
!   Stand visualization: SVCUTS   !
!=================================!
! BASE $Id$
subroutine SVCUTS (IVAC,SSNG,DSNG,CTCRWN)
  implicit none

  !     STAND VISUALIZATION GENERATION
  !     N.L.CROOKSTON -- RMRS MOSCOW -- NOVEMBER 1998
  !     J.J.MARCHINEK -- RMRS MOSCOW -- JANUARY 1999

  !     UPDATES THE VISUALIZATION FOR HARVEST, WRITES THE POST
  !     CUTTING REPORT, AND CLEANS UP THE LIST OF REFERENCES
  !     TO "REMOVED" TREE RECORDS.

  !     CALLED FROM CUTS.
  !     INPUT:
  !     IVAC=THE NUMBER OF COMPLETELY CUT TREES

  ! Common blocks
  include 'PRGPRM.F77'
  include 'ARRAYS.F77'
  include 'CONTRL.F77'
  include 'SVDATA.F77'

  !     SSNG  = SNAGS/ACRE CREATED BY CUTTING, BUT LEFT STANDING
  !     DSNG  = SNAGS/ACRE CREATED BY CUTTING, DOWN LEFT IN STAND
  !     CTCRWN= TREES/ACRE REMOVED BUT WHOSE CROWNS ARE STILL IN THE
  !             STAND.

  ! Variables
  integer :: IVAC,I,II,IPUT,ISVOBJ
  real    :: SSNG(*),DSNG(*),CTCRWN(*)
  integer :: ICUTFG(MAXTRE)
  logical :: LDROP

  !     REMOVE THE HARVESTED TREES FROM THE STANDING LIVE TREE LIST

  call SVRMOV (WK3,4,SSNG,DSNG,CTCRWN,IY(ICYC))

  !     PRINT THE VISUALIZATION FOR AFTER CUTS (IF REMOVALS).

  call SVOUT(IY(ICYC),2,'Post cutting')

  !     NOW, REMOVE ALL THE REFERENCES IN THE OBJECT LIST TO RECORDS THAT
  !     HAVE BEEN COMPLETELY REMOVED. THIS IS DONE BY WRITTING OVER
  !     DELETED RECORDS

  if (IVAC.eq.0) return

  !     LOAD ICUTFG SO THAT IT INDICATES THAT A RECORD IS
  !     COMPLETELY CUT.  A 1 MEANS THAT IS IS COMPLETELY CUT.

  do I = 1,ITRN
     II = IND2(I)
     if (II.lt.0) then
        ICUTFG(-II) = II
     else
        ICUTFG(II) = 0
     end if
  end do

  !     COMPRESS THE OBJECT POINTERS. THIS IS DONE TO MAKE SURE THAT
  !     NO MEMBERS OF IS2F POINT TO A TREE DATA RECORD THAT IS TO BE
  !     DELETED.

  IPUT = 0
  do ISVOBJ = 1,NSVOBJ
     LDROP = .false.
     if (IOBJTP(ISVOBJ).ne.4) then
        if (IOBJTP(ISVOBJ).eq.0 .or. &
             & (IOBJTP(ISVOBJ).eq.1 .and. ICUTFG(IS2F(ISVOBJ)).lt.0)) then
           LDROP = .true.
        end if
     end if
     if (LDROP) then
        if (IPUT.eq.0) IPUT = ISVOBJ
     else
        if (IPUT.gt.0 .and. IPUT.lt.ISVOBJ) then
           IS2F(IPUT) = IS2F(ISVOBJ)
           XSLOC(IPUT) = XSLOC(ISVOBJ)
           YSLOC(IPUT) = YSLOC(ISVOBJ)
           IOBJTP(IPUT) = IOBJTP(ISVOBJ)
           IPUT = IPUT+1
        end if
     end if
  end do
  if (IPUT.gt.0) NSVOBJ = IPUT-1

  return
end subroutine SVCUTS
