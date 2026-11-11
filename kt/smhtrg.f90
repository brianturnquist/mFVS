!=================!
!   kt : smhtrg   !
!=================!
! KT $Id$
subroutine SMHTRG(ISPFL,DEBUG,HTKEEP)
  implicit none

  !  THIS SUBROUTINE IS CALLED FROM CRATET.  IF ALL OF THE TREES IN A
  !  STAND ARE LESS THAN 3 INCHES DBH, THEN THIS SUBROUTINE COMPUTES
  !  NEW COEFFICIENTS FOR THE SMALL-TREE LINEAR HEIGHT DUBBING
  !  EQUATION.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'
  include 'COEFFS.f90'
  include 'CONTRL.f90'
  include 'OUTCOM.f90'
  include 'HTCAL.f90'

  !  INTERNAL VARIABLES:

  ! Variables
  integer :: ISPFL(MAXSP),I,ISPC,I1,I2,K1,K2,I3,NH,JJ
  real    :: HTKEEP(MAXTRE),SMSMYY,SMSMXX,SMSMXY,SMSMX2,H,D
  real    :: SMYY,SMXX,SMXY,SMX2,XN,DEN,BB,AA
  logical :: DEBUG

  !  CHECK TO SEE IF STAND IS COMPRISED OF TREES 3 INCHES DBH AND LESS

  do I = 1,ITRN
     if (DBH(I) .ge.3.0) then
        return
     end if
     50 continue
  end do
  do I = 1,MAXSP
     ISPFL(I) = 1
     100 continue
  end do

  !  ENTER LOOP TO ADJUST SMALL-TREE HEIGHT-DBH MODEL FOR LOCAL
  !  CONDITIONS.

  do ISPC = 1,MAXSP
     I1 = ISCT(ISPC,1)
     if (I1.le.0) go to 500
     I2 = ISCT(ISPC,2)

     !  INITIALIZE SUMS FOR THIS SPECIES

     K1 = 0
     K2 = 0
     SMSMYY = 0.0
     SMSMXX = 0.0
     SMSMXY = 0.0
     SMSMX2 = 0.0

     !  ENTER TREE LOOP WITHIN SPECIES

     do I3 = I1,I2
        I = IND1(I3)
        H = HT(I)
        NH = NORMHT(I)
        D = DBH(I)

        !  BYPASS TREES WITH MISSING HEIGHTS

        if (H .gt.0.0) then
           K1 = K1+1
           SMYY = H
           SMXY = D*H
           SMX2 = D*D
           SMXX = D
           SMSMYY = SMSMYY + SMYY
           SMSMXX = SMSMXX + SMXX
           SMSMXY = SMSMXY + SMXY
           SMSMX2 = SMSMX2 + SMX2
        end if
        if (H .gt.0.0.and. NH .eq.0.0) go to 200
        K2 = K2 + 1
        IND2(K2) = I

        !  END OF SUMMATION LOOP FOR THIS SPECIES

        200 continue
     end do
     if (DEBUG)write(JOSTND,*)' SMSMYY=',SMSMYY,' SMSMXX=',SMSMXX,&
          &' SMSMXY=',SMSMXY,' SMSMX2=',SMSMX2,' K2=',K2

     !  IF THERE ARE LESS THAN THREE OBSERVATIONS OR LHTDRG IS FALSE
     !  THEN DUB HEIGHTS USING DEFAULS COEFFICIENTS FOR THIS SPECIES

     if (K1.lt.3.or..not. LHTDRG(ISPC)) then
        ISPFL(ISPC) = 0
        go to 300
     end if
     XN = FLOAT(K1)
     DEN = XN*SMSMX2-SMSMXX*SMSMXX
     if (DEN .eq.0.0) then
        ISPFL(ISPC) = 0
        go to 300
     end if
     BB = (XN*SMSMXY-SMSMXX*SMSMYY)/DEN
     AA = (SMSMX2*SMSMYY-SMSMXX*SMSMXY)&
          &/DEN
     if (BB .le.1.0) ISPFL(ISPC) = 0
     300 continue
     if (DEBUG)write(JOSTND,*)' XN=',XN,' BB=',BB,' AA=',AA,' ISPC=',&
          &ISPC,' DEN=',DEN,' ISPFL=',ISPFL(ISPC)
     do JJ = 1,K2
        I = IND2(JJ)
        H = HT(I)
        if (DEBUG)write(JOSTND,*)' H=',H,' BB=',BB,' DEN=',&
             &DEN,' I=',I,' ISPC=',ISPC
        if (ISPFL(ISPC) .eq.0) go to 400
        if (H .gt.0.0) go to 400
        D = DBH(I)
        H = AA+BB*D
        if (DEBUG)write(JOSTND,*)' I=',I,' D=',D,' H=',H

        !  CHECKS TO ENSURE THAT NEW HEIGHT ISN'T WAY OUT OF LINE

        if (H.lt.1.0.or. H.gt.20.0*D) then
           ISPFL(ISPC) = 0
           go to 400
        else if (D .gt..1.and. H .lt.4.5) then
           ISPFL(ISPC) = 0
           go to 400
        else
           HTKEEP(I) = H
        end if
        400 continue
     end do
     500 continue
  end do
  return
end
