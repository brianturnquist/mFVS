!============================!
!   metric/ppbase : splard   !
!============================!
! METRIC-PPBASE $Id$
subroutine SPLARD (SCALOC,SCAREA,INPUT,NRECS,IRC)
  implicit none

  !     READS THE LOCATION AND NEIGHBORS DATA.

  !     SCALOC= SCALING OF THE LOCATION DATA.
  !     SCAREA= SCALING OF THE AREA DATA.
  !     INPUT = INPUT DATA SET REFERENCE NUMBER
  !     NRECS = INPUT RECORD COUNT
  !     IRC   = RETURN CODE: 0=OK, 1=ACTUAL END-OF-FILE WAS REACHED,
  !             2=TOO MANY INPUT RECORDS.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PPEPRM.f90'
  include 'PPSPLA.f90'
  include 'METRIC.f90'

  ! Variables
  character(len=26) :: ID
  character(len=10) :: CXL
  character(len=10) :: CYL
  character(len=10) :: CA
  integer :: IRC,NRECS,INPUT
  real    :: SCAREA,SCALOC,SCLOC,SCARE,A,XL,YL

  !     CHECK THE SCALING VALUES

  SCLOC = SCALOC
  if (SCALOC.le.0.0) SCLOC = 1.0
  SCARE = SCAREA*HAtoACR
  if (SCAREA.le.0.0) SCARE = HAtoACR

  !     READ THE DATA...COUNT THE INPUT RECORDS.

  NLAS = 0
  10 continue
  read (INPUT,'(A26,T29,3F10.0,T29,3A10)',end = 30)&
       &ID,XL,YL,A,CXL,CYL,CA
  if (INDEX(ID,'-999').gt.0) goto 20
  if (ID.eq.' ') goto 10
  NLAS = NLAS+1
  if (NLAS.gt.MXSTND) then
     IRC = 2
     goto 40
  end if
  CLAID(NLAS) = ID
  if (CXL.ne.' '.and. CYL.ne.' ') then
     XLOC(NLAS) = XL*SCLOC
     YLOC(NLAS) = YL*SCLOC
  else
     XLOC(NLAS) = -99999.0
     YLOC(NLAS) = -99999.0
  end if
  if (CA.ne.' ') then
     AREA(NLAS) = A*SCARE
  else
     AREA(NLAS) = -99999.0
  end if
  goto 10
  20 continue
  IRC = 0
  goto 40
  30 continue
  IRC = 1
  40 continue
  NRECS = NLAS
  return
end
