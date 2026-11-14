!=============================!
!   archive/bgc/src : bcvts   !
!=============================!
subroutine BCVTS(SP, DBH, HT, VOL)

  ! Variables
  real    :: HT,DBH,VOL
  character(len=4) :: SP

  ! Calculates cubic foot volume (bole, top and stump) for given DBH and
  ! HT in english units.

  ! SET COEFFICIENTS BY SPECIES. DEFAULT TO DF.
  if (SP.eq.'PP') then
     b0 = -2.35820
     b1 = 1.87254
     b2 = 0.89396
  else if (SP.eq.'DF') then
     b0 = -2.40258
     b1 = 1.82212
     b2 = 0.95883
  else if (SP.eq.'WL') then
     b0 = -2.39117
     b1 = 1.95789
     b2 = 0.86407
  else if (SP.eq.'LP') then
     b0 = -2.53758
     b1 = 1.87765
     b2 = 1.03312
  else
     b0 = -2.40258
     b1 = 1.82212
     b2 = 0.95883
  end if
  ! Calculate volume of bole in cubic feet
  VOL = (10**b0)*(DBH**b1)*(HT**b2)
  return
end
