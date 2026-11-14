!=================!
! organon : orgspc !
!=================!
! ORGANON $Id$
subroutine ORGSPC(INSPEC,OUTSPC)
  implicit none

  ! THIS SUBROUTINE CONVERTS AN FVS SPECIES SEQUENCE NUMBER TO A VALID
  ! ORGANON SPECIES FIA CODE.

  ! CALLED FROM SUBROUTINES **CRATET** AND **DGDRIV**

  ! Common blocks
  include 'PRGPRM.f90'

  ! Variables
  integer :: OSPMAPOC(50),OSPMAPOP(39),INSPEC,OUTSPC

  !   SPECIES ORDER IN THIS VARIANT

  !  1=PC  2=IC  3=RC  4=GF  5=RF  6=SH  7=DF  8=WH  9=MH 10=WB
  ! 11=KP 12=LP 13=CP 14=LM 15=JP 16=SP 17=WP 18=PP 19=MP 20=GP
  ! 21=JU 22=BR 23=GS 24=PY 25=OS 26=LO 27=CY 28=BL 29=EO 30=WO
  ! 31=BO 32=VO 33=IO 34=BM 35=BU 36=RA 37=MA 38=GC 39=DG 40=FL
  ! 41=WN 42=TO 43=SY 44=AS 45=CW 46=WI 47=CN 48=CL 49=OH

  ! MAPPING OF FVS SPECIES TO VALID ORGANON SPECIES FOR THE
  ! ORGANON SOUTHWEST OREGON MODEL TYPE:

  ! VALID
  ! ORGANON   FVS
  ! 015=WF*   WF IS MAPPED TO GF IN THIS VARIANT
  ! 081=IC*   IC
  ! 242=RC    RC, PC
  ! 017=GF*   GF, BR
  ! 202=DF*   DF, RF, SH
  ! 263=WH    WH, MH
  ! 117=SP*   SP
  ! 122=PP*   PP, WB, KP, LP, CP, LM, JP, WP, MP, GP, GS, RW
  ! 231=PY    PY, WJ, OS
  ! 805=CY    CY, LO, BL, EO, VO, IO
  ! 815=WO    WO
  ! 818=BO    BO
  ! 312=BM    BM, BU, FL, WN, SY, AS, CW
  ! 351=RA    RA
  ! 361=MA    MA
  ! 431=GC    GC
  ! 492=DG    DG, CN, CL, OH
  ! 631=TO    TO
  ! 920=WI    WI
  ! *SPECIES INCLUDED IN ORGANON "BIG 6"

  data OSPMAPOC/&
       !       PC   IC   RC   GF   RF   SH   DF   WH   MH   WB
       &242, 081, 242, 017, 202, 202, 202, 263, 263, 122,&
       !       KP   LP   CP   LM   JP   SP   WP   PP   MP   GP
       &122, 122, 122, 122, 122, 117, 122, 122, 122, 122,&
       !       WJ   BR   GS   PY   OS   LO   CY   BL   EO   WO
       &231, 017, 122, 231, 231, 805, 805, 805, 805, 815,&
       !       BO   VO   IO   BM   BU   RA   MA   GC   DG   FL
       &818, 805, 805, 312, 312, 351, 361, 431, 492, 312,&
       !       WN   TO   SY   AS   CW   WI   CN   CL   OH   RW
       &312, 631, 312, 312, 312, 920, 492, 492, 492, 122/

  !     OP HAS A DIFFERENT SPECIES ORDER AND TABLE LENGTH.

  !   SPECIES ORDER IN THIS VARIANT

  !   1=SF,  2=WF,  3=GF,  4=AF,  5=RF,  6=SS,  7=NF,  8=YC,
  !   9=IC, 10=ES, 11=LP, 12=JP, 13=SP, 14=WP, 15=PP, 16=DF,
  !  17=RW, 18=RC, 19=WH, 20=MH, 21=BM, 22=RA, 23=MA, 24=TO,
  !  25=GC, 26=AS, 27=CW, 28=WO, 29=J , 30=LL, 31=WB, 32=KP,
  !  33=PY, 34=DG, 35=HT, 36=CH, 37=WI, 38=  , 39=OT

  ! MAPPING OF FVS SPECIES TO VALID ORGANON SPECIES FOR THE
  ! ORGANON OP MODEL TYPE (ORIGINAL OP TABLE):

  ! VALID
  ! ORGANON   FVS
  ! 017=GF*   GF, WF, SF, AF, NF, LL
  ! 202=DF*   DF, RF, SS, ES, LP, JP, SP, WP, PP, WB, KP
  ! 231=PY    PY, WJ
  ! 242=RC    RC, YC, IC
  ! 263=WH    WH, RW, MH
  ! 312=BM    BM, TO, AS, CW
  ! 351=RA    RA
  ! 361=MA    MA, GC
  ! 492=DG    DG, HT, CH, OT
  ! 815=WO    WO
  ! 920=WI    WI
  ! *SPECIES INCLUDED IN ORGANON "BIG 6"

  data OSPMAPOP/&
       !       SF   WF   GF   AF   RF   SS   NF   YC   IC   ES
       &017, 017, 017, 017, 202, 202, 017, 242, 242, 202,&
       !       LP   JP   SP   WP   PP   DF   RW   RC   WH   MH
       &202, 202, 202, 202, 202, 202, 263, 242, 263, 263,&
       !       BM   RA   MA   TO   GC   AS   CW   WO   WJ   LL
       &312, 351, 361, 312, 361, 312, 312, 815, 231, 017,&
       !       WB   KP   PY   DG   HT   CH   WI        OT
       &202, 202, 231, 492, 492, 492, 920, 492, 492/

  if (VARIANTCD.eq.'OC') then
     OUTSPC = OSPMAPOC(INSPEC)
  else
     OUTSPC = OSPMAPOP(INSPEC)
  end if

  return
end
