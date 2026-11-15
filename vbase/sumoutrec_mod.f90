!=================================================!
!   Write one cycle-summary text data record      !
!=================================================!
module SUMOUTREC_MOD
  implicit none
  private
  public :: SUMOUTREC

contains

  subroutine SUMOUTREC(JOUT,IOSUM,I20,LENG,I)
    implicit none

    include 'PRGPRM.f90'
    include 'SUMTAB.f90'
    include 'METRIC.f90'

    integer, intent(in) :: JOUT,I20,LENG,I
    integer*4, intent(in) :: IOSUM(I20,LENG)
    integer :: K

    ! Preserve the caller's leading dimension, including metric DISPLY's
    ! existing I20=20 stride (bug 30). Convert the already-rounded integers
    ! only in these expressions; headings and DBSSUMRY remain in SUMOUT.
    if (LMETRICVAR) then
      write(JOUT,20)&
      &IOSUM(1,I),&
      &IOSUM(2,I),&
      &INT(IOSUM(3,I)/ACRtoHA),&
      &INT(IOLDBA(I)*FT2pACRtoM2pHA),&
      &INT(ISDI(I)/ACRtoHA),&
      &IBTCCF(I),&
      &INT(IBTAVH(I)*FTtoM),&
      &QSDBT(I)*INtoCM,&
      &INT(IOSUM(4,I)*FT3pACRtoM3pHA),&
      &INT(IOSUM(5,I)*FT3pACRtoM3pHA),&
      &INT(IOSUM(6,I)*FT3pACRtoM3pHA),&
      &INT(IOSUM(7,I)/ACRtoHA),&
      &INT(IOSUM(8,I)*FT3pACRtoM3pHA),&
      &INT(IOSUM(9,I)*FT3pACRtoM3pHA),&
      &INT(IOSUM(10,I)*FT3pACRtoM3pHA),&
      &INT(IOSUM(11,I)*FT2pACRtoM2pHA),&
      &INT(ISDIAT(I)/ACRtoHA),&
      &IOSUM(12,I),&
      &INT(IOSUM(13,I)*FTtoM),&
      &QDBHAT(I)*INtoCM,&
      &IOSUM(14,I),&
      &INT(IOSUM(15,I)*FT3pACRtoM3pHA),&
      &INT(IOSUM(16,I)*FT3pACRtoM3pHA),&
      &BCYMAI(I)*FT3pACRtoM3pHA,&
      &(IOSUM(K,I),K = 18,20)
    20 format(2I4,I6,I4,I5,2I4,F5.1,7I6,I4,I5,2I4,F5.1,2X,I6,&
         &I5,I6,2X,F6.1,1X,I3,1X,2I1)
    else
      write(JOUT,30) (IOSUM(K,I),K = 1,3),IOLDBA(I),ISDI(I),&
      &IBTCCF(I),IBTAVH(I),QSDBT(I),(IOSUM(K,I),K = 4,5),&
      &IOSUM(21,I),(IOSUM(K,I),K = 6,9),IOSUM(22,I),&
      &(IOSUM(K,I),K = 10,11),&
      &ISDIAT(I),IOSUM(12,I),IOSUM(13,I),QDBHAT(I),&
      &(IOSUM(K,I),K = 14,16),BCYMAI(I),(IOSUM(K,I),K = 18,20)
    30 format(2I4,I6,I4,I5,2I4,F5.1,9I6,I4,I5,2I4,F5.1,2X,I6,&
         &I5,I6,2X,F6.1,1X,I3,1X,2I1)
    end if
  end subroutine SUMOUTREC

end module SUMOUTREC_MOD
