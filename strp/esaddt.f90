!===================!
!   strp : esaddt   !
!===================!
! STRP $Id$
subroutine ESADDT(ICALL)
  implicit none

  !     ADD NEW TREES FROM A FILE OR EXTERNAL DATA BASE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'OUTCOM.f90'
  include 'PLOT.f90'
  include 'ESHAP.f90'
  include 'ESHAP2.f90'
  include 'VARCOM.f90'

  ! Variables
  integer :: ICALL

  logical :: DEBUG
  character(len=250) :: CMDLN
  character(len=250) :: FNAM
  character(len=4) :: CYR
  integer :: I,J,KODE,IKEEP,IPYR
  integer :: MYACT(1),NTODO,ITODO,NP,IACTK,IDT,KDT,IMETH
  integer :: FNO
  real    :: PRMS(2),SUM,SUMSP(8)
  integer :: IDANUW

  !     ADDTREES CODE = 432

  data MYACT/432/
  data FNO  /101/

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  IDANUW = ICALL

  !     SET THE DEBUG OPTION

  call DBCHK (DEBUG,'ESADDT',6,ICYC)

  !     PROCESS ADDTREES KEYWORDS

  call OPFIND (1,MYACT,NTODO)
  if (DEBUG) write (JOSTND,5) NTODO
  5 format ('IN ESADDT: OPTS NTODO=',I2)
  if (NTODO .gt.0) then
     KDT = IY(ICYC+1)-1
     do ITODO = 1,NTODO
        call OPGET(ITODO,2,IDT,IACTK,NP,PRMS)

        !         GET THE METHOD SWITCH

        IMETH = 1
        if (IABS(NP) .ge.2) then
           IPYR = INT(PRMS(1))+KDT ! planting year
           IMETH = INT(PRMS(2))
        end if
        if (DEBUG) write (JOSTND,6) IMETH
        6 format ('IN ESADDT, IMETH=',I2)

        select case (IMETH)

           !           Run external program
        case (1)

           call OPGETC (ITODO,CMDLN)
           if (CMDLN .eq. ' ') goto 101

           !             Construct input filename: Keyfile_StandID_Year_BM.ES1
           J = 0
           do I = 1,250
              FNAM(I:I) = CHAR(32) ! put space code
              if (KWDFIL(I:I) .ne. ' ') then
                 J = J+1
                 FNAM(J:J) = KWDFIL(I:I)
              end if
           end do
           J = J+1
           FNAM(J:J) = '_'
           do I = 1,26
              if (NPLT(I:I) .ne. ' ') then
                 J = J+1
                 FNAM(J:J) = NPLT(I:I)
              end if
           end do
           J = J+1
           FNAM(J:J) = '_'
           write(CYR,'(I4)') KDT
           do I = 1,4
              if (CYR(I:I) .ne. ' ') then
                 J = J+1
                 FNAM(J:J) = CYR(I:I)
              end if
           end do
           J = J+1
           FNAM(J:J) = '_'
           do I = 1,2
              if (VARACD(I:I) .ne. ' ') then
                 J = J+1
                 FNAM(J:J) = VARACD(I:I)
              end if
           end do
           FNAM(J+1:J+5) = ".es1"

           !             open and populate file with information to send to
           !             external regeneration model: suffix = ".es1"

           call MYOPEN (FNO,FNAM(1:LEN_TRIM(FNAM)),&
                &5,150,0,1,1,0,KODE)
           if (KODE .ne.0) goto 101

           ! concatenate the name of the exe and input fn
           J = LEN_TRIM(CMDLN)+1
           CMDLN(J:J) = ' '
           do I = 1,LEN_TRIM(FNAM)
              if (FNAM(I:I) .ne. ' ' .and. J .le.250) then
                 J = J+1
                 CMDLN(J:J) = FNAM(I:I)
              end if
           end do

           ! StandID (1)
           write(FNO,'(A30)',ERR = 101) NPLT(1:LEN_TRIM(NPLT))
           ! Year (2)
           write(FNO,'(I30)',ERR = 101) IPYR
           ! ADDTREES fld2 (3)
           write(FNO,'(I30)',ERR = 101) INT(PRMS(1))

           !             Data requirements for Blue Mountains estab model
           write(FNO,'(I30)',  ERR = 101) KODTYP !hab code (4)
           write(FNO,'(I30)',  ERR = 101) ISLOP !slope    (5)
           write(FNO,'(I30)',  ERR = 101) IASPEC !aspect   (6)
           write(FNO,'(F30.1)',ERR = 101) ELEV*100. !elev     (7)
           write(FNO,'(F30.1)',ERR = 101) SDIBC/GROSPC !bsdi     (8)
           write(FNO,'(F30.1)',ERR = 101) SDIAC/GROSPC !asdi     (9)
           write(FNO,'(F30.1)',ERR = 101) OLDBA/GROSPC !ba       (10)
           write(FNO,'(F30.1)',ERR = 101) ATBA/GROSPC !aba      (11)
           SUM = 0.0
           do I = 1,8
              SUMSP = 0.0
           end do
           do I = 1,ITRN
              if (DBH(I) .gt.1.0) then
                 SUM = SUM + PROB(I)
              else
                 select case (ISP(I))
                 case (4) ! ABGR
                    SUMSP(1) = SUMSP(1) + PROB(I)
                 case (9) ! ABLA
                    SUMSP(2) = SUMSP(2) + PROB(I)
                 case (2) ! LAOC
                    SUMSP(3) = SUMSP(3) + PROB(I)
                 case (7) ! PICO
                    SUMSP(4) = SUMSP(4) + PROB(I)
                 case (1) ! PIMO
                    SUMSP(5) = SUMSP(5) + PROB(I)
                 case (8) ! PIEN
                    SUMSP(6) = SUMSP(6) + PROB(I)
                 case (10) ! PIPO
                    SUMSP(7) = SUMSP(7) + PROB(I)
                 case (3) ! PSME
                    SUMSP(8) = SUMSP(8) + PROB(I)
                 end select
              end if
           end do
           write(FNO,'(F30.1)',ERR = 101) SUM ! stand tpa >1" (12)
           ! spp tpa <1" (13-20)
           do I = 1,8
              write(FNO,'(F30.1)',ERR = 101) SUMSP(I)
           end do

           close(FNO)
           call SYSTEM(CMDLN)

           J = LEN_TRIM(FNAM)
           FNAM(J-3:J) = ".es2"
           call MYOPEN (FNO,FNAM(1:LEN_TRIM(FNAM)),&
                &1,150,0,1,1,0,KODE)
           if (KODE .ne.0) goto 101
           read(FNO,'(I10)',end = 101) IKEEP
           call OPRDAT(FNO,KODE)
           if (KODE .ge.0) then
              call OPDONE(ITODO,IDT)
              goto 202
           end if
           101 call OPDEL1(ITODO)
           202 if (IKEEP .ne.1) then
              close(FNO, STATUS = 'DELETE')
           else
              close(FNO, STATUS = 'KEEP')
           end if
        end select
     end do
  end if

  return
end
