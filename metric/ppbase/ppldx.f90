!===========================!
!   metric/ppbase : ppldx   !
!===========================!
! METRIC-PPBASE $Id$
subroutine PPLDX (X,INSTR,IRC)
  implicit none

  !     CALLED FROM ALGEVL

  !     LOADS X VALUES OWNED BY PPE AS OPPOSED TO THE PROGNOSIS MODEL.

  !     N.L.CROOKSTON - APR 87 - FORESTRY SCIENCES LAB - MOSCOW, ID

  !     X     = THE VALUE REQUESTED.
  !     INSTR = THE CODE THAT SAYS WHICH VALUE IS REQUESTED.
  !     IRC   = RETURN CODE, 0=OK, 1=VARIABLE IS CURRENTLY UNDEFINED,
  !             2=INSTRUCTION CODE COULD NOT BE DECIPHERED.

  ! Common blocks
  include 'PPEXCM.f90'
  include 'METRIC.f90'

  ! Variables
  integer :: IRC,INSTR,I
  real    :: X

  !     DECODE INSTRUCTION AND EXECUTE.

  if (INSTR.gt.7000) then
     if (INSTR.lt.7200) then
        I = INSTR-7000
        if (I.le.NPTST1) then
           if (LPTST1(I)) then
              J = INSTR/100
              goto (1101,1102,1102,1100,1103,&
                   &1102,1102,1100,1100,1100) J
              1100 continue
              X = PTSTV1(I)
              goto 1000
              1101 continue
              X = PTSTV1(I) / ACRtoHA
              goto 1000
              1102 continue
              X = PTSTV1(I) * FT3pACRtoM3pHA
              goto 1000
              1103 continue
              X = PTSTV1(I) * FT2pACRtoM2pHA
              goto 1000
              1000 continue
              IRC = 0
              return
           else
              IRC = 1
              return
           end if
        else
           IRC = 2
           return
        end if
     else if (INSTR.lt.8000) then
        I = INSTR-7200
        if (I.lt.NPTST2) then
           if (LPTST2(I)) then
              X = PTSTV2(I)
              IRC = 0
              return
           else
              IRC = 1
              return
           end if
        else
           IRC = 2
           return
        end if
     else
        I = INSTR-8000
        if (I.lt.NPTST3) then
           X = PTSTV3(I)
           IRC = 0
           return
        else
           IRC = 2
           return
        end if
     end if
  else
     IRC = 2
     return
  end if
end
