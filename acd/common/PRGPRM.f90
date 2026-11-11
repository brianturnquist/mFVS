!===============================!
!   Prognosis model parameters  !
!===============================!
! ACD-COMMON $Id$

! Model dimensions
integer :: MAXTRE,MAXTP1,MAXPLT,MAXSP,MAXCYC,MAXCY1,MAXSTR,MXFRCDS
parameter (MAXTRE=3000)
parameter (MAXTP1=MAXTRE+1)
parameter (MAXPLT=500)
parameter (MAXSP=108)
parameter (MAXCYC=40)
parameter (MAXCY1=MAXCYC+1)
parameter (MAXSTR=20)
parameter (MXFRCDS=20)

! Parameters of option processing are in common OPCOM.
!
! MAXTRE = The max number of tree records that Prognosis can process.
!          Note: a realistic min value for this parameter is about
!          400. If the establishment extension is not being
!          used, a somewhat smaller value may be used. Also
!          note that changing this parameter may change the
!          model's behavior. The normal setting is 1350.
! MAXTP1 = The max number of tree records plus 1.
! MAXPLT = The max number of individual plots that Prognosis can
!          process.
! MAXSP = The max number of species represented in the model.
! MAXCYC = The max number of cycles allowed in the model.
! MAXCY1 = The max number of cycles plus 1.
! MAXSTR = The maximum number of site trees.
! MXFRCDS = Maximum forest codes (ESCOMN).
