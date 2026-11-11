!=======================================!
!   Handle unused habitat type inputs   !
!=======================================!
! CANADA-BC $Id$
subroutine HABTYP (KARD2,ARRAY2)
  implicit none

  !     TRANSLATES BEC STRINGS INTO VARIOUS USEFUL FORMATS;
  !     RETURNS ICHmw2/01 BY DEFAULT.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'BCPLOT.f90'

  ! Variables
  real    :: ARRAY2
  character(len=20) :: KARD2

  logical :: LOK
  character(len=3) :: RGN(3)
  character(len=4) :: ZN(7)
  character(len=2) :: SZ(19)
  character(len=20) :: FULL
  integer :: I,J,J1,J2,K
  TYPE         (BEC_DESCRIPTION) TMP

  data RGN /'CAR',&
       &'KAM',&
       &'NEL'/

  data ZN  /'ESSF',&
       &'ICH',&
       &'IDF',&
       &'MS',&
       &'PP',&
       &'SBS',&
       &'SBPS'/

  data SZ  /'DC',&
       &'DK',&
       &'DM',&
       &'DW',&
       &'MC',&
       &'MH',&
       &'MK',&
       &'MM',&
       &'MW',&
       &'VK',&
       &'WC',&
       &'WK',&
       &'WM',&
       &'WW',&
       &'XC',&
       &'XH',&
       &'XK',&
       &'XM',&
       &'WM' /

  !     IF THIS ISN'T DONE, THE STRINGS ARE ALL '0' NULL, RATHER THAN '32' SPACE

  TMP%Region = ''
  TMP%Zone = ''
  TMP%SubZone = ''
  TMP%Series = ''
  TMP%FullName = ''
  TMP%PrettyName = ''

  !     MERGE FIRST 2 FIELDS AND SHIFT TO UPPERCASE

  FULL = ' '
  K = 1
  do J = 1,20
     if (KARD2(J:J) .ne. ' ') then
        FULL(K:K) = KARD2(J:J)
        call UPCASE(FULL(K:K))
        K = K + 1
     end if
  end do

  !     ZERO DEFAULT WITH CURRENT BEC INFORMATION
  !     POPULATE THE BEC DATA STRUCTURE

  LOK = .false.
  do I = 1,3
     J1 = INDEX(FULL,RGN(I))
     if (J1.gt.0) then
        LOK = .true.
        TMP%Region = RGN(I)
        goto 5
     end if
  end do
  5 if (.not. LOK) goto 3

  LOK = .false.
  do I = 1,7
     K = LEN_TRIM(ZN(I))
     J1 = INDEX(FULL,ZN(I)(:K))
     if (J1.gt.0) then
        LOK = .true.
        TMP%Zone = ZN(I)
        goto 6
     end if
  end do
  6 if (.not. LOK) goto 3

  LOK = .false.
  do I = 1,19
     J1 = INDEX(FULL,SZ(I))
     if (J1.gt.0) then
        LOK = .true.
        TMP%SubZone = SZ(I)
        J2 = INDEX(FULL,'/')
        if (J2.eq. J1+3) TMP%SubZone(3:3) = FULL(J1+2:J1+2)
        goto 7
     end if
  end do
  7 if (.not. LOK) goto 3

  !     LONGEST SERIES IS SOMETHING LIKE '1-YC'

  LOK = .false.
  J2 = INDEX(FULL,'/')
  if (J2.gt.0) then
     LOK = .true.
     K = 1
     do I = J2+1,J2+5
        if (FULL(I:I) .ne. ' ') then
           TMP%Series(K:K) = FULL(I:I)
           K = K + 1
        end if
     end do
  end if
  if (.not. LOK) goto 3

  !     CONVERT SUBZONE AND SERIES TO LOWERCASE

  do I = 1, LEN_TRIM(TMP%SubZone)
     call LOCASE(TMP%SubZone(I:I))
  end do
  do I = 1, LEN_TRIM(TMP%Series)
     call LOCASE(TMP%Series(I:I))
  end do

  !     CREATE A COMBINED BEC IDENTIFIER

  TMP%FullName = ''
  K = 1
  do I = 1,4
     if (TMP%Zone(I:I) .ne. ' ') then
        TMP%FullName(K:K) = TMP%Zone(I:I)
        K = K + 1
     end if
  end do
  do I = 1,3
     if (TMP%SubZone(I:I) .ne. ' ') then
        TMP%FullName(K:K) = TMP%SubZone(I:I)
        K = K + 1
     end if
  end do
  TMP%PrettyName = TMP%FullName

  TMP%PrettyName(K:K) = '/'
  TMP%PrettyName(K+1:K+5) = TMP%Series(1:5)

  !     COPY THE TEMPORARY DESCRIPTION TO THE GLOBAL VARIABLE

  BEC = TMP

  goto 2
  3 call ERRGRO (.true.,3)
  2 continue

  return
end

  !     CONVERT CASE FOR KEYWORD INPUT ARGS: REGION AND ZONE ARE
  !     UPPERCASE; SUBZONE IS LOWERCASE

subroutine LOCASE (C)

  ! Variables
  character(len=1) :: C
  character(len=26) :: UPPER
  character(len=26) :: LOWER
  data UPPER /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
  data LOWER /'abcdefghijklmnopqrstuvwxyz'/
  IP = INDEX(UPPER,C)
  if (IP.gt.0) C = LOWER(IP:IP)
  return

end
