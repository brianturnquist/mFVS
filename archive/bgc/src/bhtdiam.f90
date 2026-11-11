!===============================!
!   archive/bgc/src : bhtdiam   !
!===============================!
subroutine HTDIAM(SPEC,DIAM,THT)

  ! SUBROUTINE RETURNS HEIGHT FOR GIVEN DBH. EQUATION FIT TO
  ! DATA FROM WESTERN MONTANA. MILNER STEM ANALYSIS DATA.
  ! MODEL FORM IS LOG10(H) = B0 +B1*LOG10(DBH). ENGLISH UNITS.

  ! Variables
  character(len=4) :: SPEC
  real    :: DIAM,THT
  ! SET COEFFICIENTS
  if (SPEC.eq.'PP') then
     B0 = 0.39835
     B1 = 1.21820
  else if (SPEC.eq.'DF') then
     B0 = 0.80022
     B1 = 0.86325
  else if (SPEC.eq.'WL') then
     B0 = 0.84146
     B1 = 0.93298
  else
     B0 = 0.73723
     B1 = 1.01854
  end if
  ! CALCULATE HEIGHT
  THT = 10**(B0+B1*LOG10(DIAM))
  return
end
