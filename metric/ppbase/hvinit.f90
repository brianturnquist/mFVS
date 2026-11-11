!============================!
!   metric/ppbase : hvinit   !
!============================!
! METRIC-PPBASE $Id$
subroutine HVINIT
  implicit none

  !     CALLED FROM PPINIT.  INITIALIZE THE MULTISTAND POLICY VARIABLES

  !     MULTISTAND POLICY ROUTINE - N.L. CROOKSTON  - JULY 1987
  !     FORESTRY SCIENCES LABORATORY - MOSCOW, ID 83843

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PPEPRM.f90'
  include 'PPHVCM.f90'
  include 'HVDNCM.f90'
  include 'METRIC.f90'

  ! Variables
  integer :: IHV

  LNEDNG = .false.
  HXSIZE = ACRtoHA
  LHVDEB = .false.
  LPRTCT = .true.
  HVMXCC = 40.
  LHVMXC = .false.
  LHVUNT = .false.
  LHIER = .false.
  IXHRVP = 0
  IHVEXT = 0
  do IHV = 1,MXHRVP
     HVPLAB(IHV) = ' '
     LNHPLB(IHV,1) = 0
     LNHPLB(IHV,2) = 0
     IHVTAB(IHV,1) = 0
     IHVTAB(IHV,2) = 0
     IHVTAB(IHV,3) = 0
     TRGSTS(8,IHV) = 0.0
     JOHVDS(IHV) = 0
     10 continue
  end do
  return
end
