!=========================!
!   Compute bark ratios   !
!=========================!
! CANADA-ON $Id: bratio.f 3977 2022-05-04 23:24:47Z donrobinson $
function BRATIO(IS,D,H)
  implicit none

  ! FUNCTION TO COMPUTE BARK RATIOS. THIS ROUTINE IS VARIANT SPECIFIC
  ! AND EACH VARIANT USES ONE OR MORE OF THE ARGUMENTS PASSED TO IT.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'COEFFS.f90'
  include 'METRIC.f90'

  ! Variables
  integer :: IS
  real    :: H,D,BRATIO
  real    :: D_CM,H_M,HDRAT

  if (IS .eq.0) then
     BRATIO = 0.93
  else
     BRATIO = BKRAT(IS)
  end if

  !     CORRECT FOR ONTARIO INFORMATION.
  !        THE VALUES USED HERE ARE FROM Zakrzewski AS PART OF HIS
  !        VOLUME EQUATIONS.

  !     NOTE THAT THE CONSTANT VALUES SHOULD JUST REPLACE THE VALUES
  !     IN BKRAT, RATHER THAN BEING DONE HERE.

  !     REPLACE BRATIO VALUES WITH CONSTANT VALUES, IF APPLICABLE

  if (IS .eq.3.or. IS .eq.4) then
     !       (RED PINE)
     BRATIO = 0.92608

  else if (IS .eq.1.or. IS .eq.69.or. IS .eq.10.or.&
       &IS .eq.13) then
     !       (JACK PINE NAT OR PLANT, TAMARAK OR "OTHER SOFTWOODS")
     BRATIO = 0.9609

  else if (IS .eq.6.or. IS .eq.71.or. IS .eq.7) then
     !       (WHITE SPRUCE OR NORWAY SPRUCE)
     BRATIO = 0.955

  else if (IS .eq.8) then
     !       (BALSAM FIR)
     BRATIO = 0.9456

  else if (IS .eq.24.or. IS .eq.43) then
     !       (BIRCH)
     BRATIO = 0.9451

  else if (IS .eq.15.or. IS .eq.16.or. IS .eq.17.or.&
       &IS .eq.40.or. IS .eq.41.or. IS .eq.42) then
     !       (ASPEN, ALSO USED FOR ASH & COTTONWOOD (Ontario sp. 45, 70-74))
     BRATIO = 0.9365

  end if

  !     IF, FOR SOME STRANGE REASON THE DIAMETER IS ZERO, JUST USE THE
  !     VALUES FROM BEFORE.
  if (D .eq.0) return

  !     THESE ARE THE EQUATIONS FROM ONTARIO, USED FOR SOME SPECIES.
  !     THEY ALL RELY ON A NON-ZERO DIAMETER.
  !     DO THE CALCULATIONS IN METRIC.
  !     Note that several of these equations could easily go negative
  !     if the ratio between height and diameter is too large. If this
  !     happens, recalculate based on a smaller ratio.

  D_CM = D * INtoCM
  H_M = H * FTtoM
  HDRAT = H_M / D_CM

  if (IS .eq.5.or. IS .eq.70) then
     !       (WHITE PINE)
     BRATIO = 1.0 - 0.04513 * D_CM ** (1.168567 - 1.0)

  else if (IS .eq.9.or. IS .eq.72) then
     !       (BLACK SPRUCE)
     !        Note: equation goes negative at HDRAT>14.8679)
     !        IF (HDRAT .LT. 14.8) THEN
     if (HDRAT .lt.14.0) then
        BRATIO = 1.0 - 0.067259 * HDRAT
     else
        BRATIO = 1.0 - 0.067259 * 14.0
     end if
  else if (IS .eq.11.or. IS .eq.14) then
     !       (WHITE CEDAR, ALSO USED FOR NORTHERN RED CEDAR)
     !        Note: equation goes negative at HDRAT>7.31797)
     !        IF (HDRAT .LT. 7.3) THEN
     if (HDRAT .lt.7.0) then
        BRATIO = 1.0 - 0.13665 * HDRAT
     else
        BRATIO = 1.0 - 0.13665 * 7.0
     end if

  else if (IS .eq.18.or. IS .eq.19.or. IS .eq.26.or.&
       &IS .eq.27.or. IS .eq.44.or. IS .eq.46.or.&
       &IS .eq.48.or. IS .eq.49.or. IS .eq.50.or.&
       &IS .eq.51.or. IS .eq.52.or. IS .eq.54.or.&
       &IS .eq.57.or. IS .eq.59.or. IS .eq.64.or.&
       &IS .eq.65.or. IS .eq.66.or. IS .eq.67) then
     !       SUGAR/RED MAPLE (ONT SP: 30,32)
     !           Note: equation goes negative at HDRAT>8.954)
     !        IF (HDRAT .LT. 8.9) THEN
     if (HDRAT .lt.8.0) then
        BRATIO = 1.0 - 0.111681 * HDRAT
     else
        BRATIO = 1.0 - 0.111681 * 8.0
     end if

     !--SARAH TEMP FOR DEBUGGING

     !      WRITE(16,9023) D,H,HDRAT, BRATIO
     ! 9023 FORMAT(' IN BRATIO, DBH=',F7.2,
     !     &       ', HEIGHT=',F7.2,', HDRAT=',F7.4,',BRATIO.=',F7.4)

  end if

  return
end
