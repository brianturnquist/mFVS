!============================!
!   archive/bgc/src : bark   !
!============================!
subroutine BARK(SPEC,DIAM,BRK)

  ! SUBROUTINE CALCULATES BARK THICKNESS. EQUATIONS FIT TO DATA FROM
  ! WESTERN MONTANA. MILNER STEM ANALYSIS DATA. ENGLISH UNITS.

  ! Variables
  character(len=4) :: SPEC
  real    :: DIAM,BRK

  ! SET COEFFICIENTS BY SPECIES. DEFAULT TO LODGEPOLE PINE.

  if (SPEC.eq.'PP') then
     B0 = 0.18281
     B1 = 0.06495
  else if (SPEC.eq.'DF') then
     B0 = -0.05622
     B1 = 0.06698
  else if (SPEC.eq.'WL') then
     B0 = 0.11153
     B1 = 0.06909
  else
     B0 = 0.02179
     B1 = 0.02953
  end if
  ! CALCULATE BARK THICKNESS (in.)
  BRK = B0 + B1*DIAM
  return
end
