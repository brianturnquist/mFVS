!=============================!
!   fire/base : fmphotocode   !
!=============================!
! FIRE-BASE $Id$
subroutine FMPHOTOCODE (FotoRef, CFotoCode, Fotocode, Icall)
  implicit none

  !     TRANSLATES THE CHARACTER PHOTO SERIES PHOTO CODE INTO AN
  !     INTEGER SUBSCRIPT, FOTOCODE.
  !     FOTOREF - THE PHOTO SERIES REFERENCE NUMBER (1 - 32)
  !                (SEE FMPHOTOVAL FOR CITATIONS)
  !     CFOTOCODE - THE CHARACTER PHOTO CODE ASSOCIATED WITH THAT REFERENCE
  !     FOTOCODE - THE INTEGER PHOTO CODE
  !     ICALL = 1 MEANS THAT YOU ARE GIVING THE CHARACTER CODE AND WANT THE INTEGER CODE
  !     ICALL = 0 MEANS THAT YOU ARE GIVING THE INTEGER CODE AND WANT THE CHARACTER CODE

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'

  ! Variables
  character(len=13) :: CFOTOCODE
  character(len=13) :: CFOTOTMP
  integer :: FOTOCODE,FOTOREF,I,N,ICALL
  logical :: DEBUG
  character(len=3) :: REF1COD(22)
  character(len=3) :: REF2COD(59)
  character(len=3) :: REF3COD(66)
  character(len=3) :: REF32COD(39)
  character(len=3) :: REF29COD(16)
  character(len=6) :: REF5COD(17)
  character(len=6) :: REF21COD(25)
  character(len=6) :: REF28COD(30)
  character(len=6) :: REF30COD(16)
  character(len=12) :: REF6COD(27)
  character(len=4) :: REF7COD(56)
  character(len=4) :: REF14COD(29)
  character(len=4) :: REF15COD(29)
  character(len=4) :: REF17COD(35)
  character(len=4) :: REF18COD(43)
  character(len=4) :: REF16COD(41)
  character(len=10) :: REF8COD(86)
  character(len=11) :: REF9COD(26)
  character(len=11) :: REF23COD(26)
  character(len=8) :: REF11COD(26)
  character(len=8) :: REF20COD(26)
  character(len=8) :: REF13COD(42)
  character(len=13) :: REF12COD(90)
  character(len=5) :: REF19COD(34)
  character(len=5) :: REF27COD(30)
  character(len=5) :: REF24COD(27)
  character(len=5) :: REF22COD(36)
  character(len=7) :: REF25COD(14)
  character(len=7) :: REF26COD(16)
  character(len=2) :: REF31COD(10)

  data REF1COD / '16 ', '15 ', '63 ', '65 ', '67 ',&
       &'25 ', '66 ', '9A ', '4A ', '17A',&
       &'3A ', '13A', '6A ', '18A', '10A',&
       &'7A ', '19A', '11A', '5A ', '8A ',&
       &'16A', '15A' /

  data REF2COD / '24 ', '18 ', '23 ', '33A', '30A',&
       &'32A', '17 ', '31A', '29A', '72 ',&
       &'76 ', '69 ', '80 ', '70 ', '64 ',&
       &'79 ', '75 ', '73 ', '77 ', '84 ',&
       &'74 ', '78 ', '68 ', '31 ', '36A',&
       &'71 ', '14 ', '88 ', '13 ', '9  ',&
       &'89 ', '5  ', '29 ', '30 ', '56 ',&
       &'33 ', '91 ', '7  ', '32 ', '8  ',&
       &'28 ', '43 ', '14A', '39A', '41A',&
       &'49 ', '28A', '27A', '12A', '37A',&
       &'42A', '38A', '86 ', '43A', '34 ',&
       &'40A', '42 ', '48 ', '95 '/

  data REF3COD / '2  ', '26A', '1  ', '35A', '82 ',&
       &'85 ', '25A', '34A', '45A', '47A',&
       &'87 ', '83 ', '92 ', '53 ', '41 ',&
       &'49A', '98 ', '61 ', '60 ', '6  ',&
       &'55 ', '48A', '11 ', '46A', '27 ',&
       &'81 ', '45 ', '1A ', '40 ', '39 ',&
       &'35 ', '2A ', '26 ', '24A', '44 ',&
       &'37 ', '21 ', '21A', '90 ', '54 ',&
       &'36 ', '22A', '50 ', '58 ', '19 ',&
       &'57 ', '96 ', '23A', '46 ', '97 ',&
       &'20 ', '59 ', '44A', '93 ', '47 ',&
       &'4  ', '22 ', '51 ', '62 ', '12 ',&
       &'20A', '94 ', '10 ', '3  ', '38 ',&
       &'52 '/

  data REF5COD /'1WH1TH','2WH1TH','3WH1TH','4WH1TH','5WH1TH',&
       &'6WH1TH','1GF1TH','2GF1TH','3GF1TH','4GF1TH',&
       &'1WC1TH','2WC1TH','3WC1TH','4WC1TH','5WC1TH',&
       &'6WC1TH','7WC1TH'/

  data REF6COD / '1PP4CC      ', '2PP4CC      ', '1PP4PC      ',&
       &'2PP4PC      ', '3PP4PC      ', '4PP4PC      ',&
       &'5PP4PC      ', '1PP1TH      ', '2PP1TH      ',&
       &'3PP1TH      ', '4PP1TH      ', '5PP1TH      ',&
       &'6PP1TH      ', '1PP&ASSOC4PC', '2PP&ASSOC4PC',&
       &'3PP&ASSOC4PC', '4PP&ASSOC4PC', '5PP&ASSOC4PC',&
       &'6PP&ASSOC4PC', '7PP&ASSOC4PC', '8PP&ASSOC4PC',&
       &'1LP3CC      ', '1LP3PC      ', '2LP3PC      ',&
       &'3LP3PC      ', '4LP3PC      ', '5LP3PC      '/

  data REF7COD / '1MP4','2MP4','3MP4','4MP4','5MP4',&
       &'1MF4','2MF4','3MF4','4MF4','5MF4',&
       &'1PP2','2PP2','3PP2','4PP2','1PP3',&
       &'2PP3','3PP3','4PP3','1PP4','2PP4',&
       &'3PP4','1LP2','2LP2','3LP2','4LP2',&
       &'5LP2','1LP3','2LP3','3LP3','4LP3',&
       &'1LP4','1WF2','2WF2','3WF2','4WF2',&
       &'1WF3','2WF3','3WF3','4WF3','5WF3',&
       &'1WF4','2WF4','3WF4','4WF4','5WF4',&
       &'1RF3','2RF3','3RF3','4RF3','5RF3',&
       &'1RF4','2RF4','3RF4','4RF4','5RF4',&
       &'1MH4'/

  data REF8COD / '1DFHD3    ', '2DFHD3    ', '3DFHD3    ',&
       &'1DFHD4    ', '2DFHD4    ', '3DFHD4    ',&
       &'4DFHD4    ', '5DFHD4    ', '1HD2      ',&
       &'2HD2      ', '1DF2      ', '2DF2      ',&
       &'1DF3      ', '2DF3      ', '1DF4      ',&
       &'2DF4      ', '3DF4      ', '4DF4      ',&
       &'5DF4      ', '6DF4      ', '7DF4      ',&
       &'1SA1      ', '2SA1      ', '3SA1      ',&
       &'1SA2      ', '2SA2      ', '1SA3      ',&
       &'2SA3      ', '3SA3      ', '1SA4      ',&
       &'2SA4      ', '1MC2      ', '2MC2      ',&
       &'3MC2      ', '1MC3      ', '2MC3      ',&
       &'3MC3      ', '1MC4      ', '2MC4      ',&
       &'1LP1      ', '2LP1      ', '3LP1      ',&
       &'1LP2      ', '2LP2      ', '3LP2      ',&
       &'4LP2      ', '1LP3      ', '2LP3      ',&
       &'3LP3      ', '1PP&ASSOC3', '2PP&ASSOC3',&
       &'3PP&ASSOC3', '4PP&ASSOC3', '5PP&ASSOC3',&
       &'1PP&ASSOC4', '2PP&ASSOC4', '3PP&ASSOC4',&
       &'1PP1      ', '2PP1      ', '3PP1      ',&
       &'1PP2      ', '2PP2      ', '3PP2      ',&
       &'4PP2      ', '1PP3      ', '2PP3      ',&
       &'3PP3      ', '4PP3      ', '5PP3      ',&
       &'6PP3      ', '7PP3      ', '8PP3      ',&
       &'1PP4      ', '2PP4      ', '3PP4      ',&
       &'4PP4      ', '5PP4      ', '6PP4      ',&
       &'7PP4      ', '8PP4      ', '1BR       ',&
       &'2BR       ', '1JU2      ', '2JU2      ',&
       &'1GR       ', '2GR       '/

  data REF9COD / '1DFWHPRE01 ', '1DFWHPRE02 ', '1DFWHPRE03 ',&
       &'1DFWHPRE04 ', '1DFWHPRE05 ', '1DFWHPRE06 ',&
       &'1DFWHPRE07 ', '1DFWHPRE08 ', '1DFWHPRE09 ',&
       &'2WHSSPRE01 ', '3RAPRE01   ', '3RAPRE02   ',&
       &'3RAPRE03   ', '3RAPRE04   ', '3RAPRE05   ',&
       &'3RAPRE06   ', '3RAPRE07   ', '4DFWHPOST01',&
       &'4DFWHPOST02', '4DFWHPOST03', '4DFWHPOST04',&
       &'5RAPOST01  ', '5RAPOST02  ', '5RAPOST03  ',&
       &'5RAPOST04  ', '5RAPOST05  '/

  data REF11COD / '1PP1TH  ', '2PP1TH  ', '3PP1TH  ', '4PP1TH  ',&
       &'5PP1TH  ', '6PP1TH  ', '7PP1TH  ', '1PP2PC  ',&
       &'2PP2PC  ', '3PP2PC  ', '4PP2PC  ', '5PP2PC  ',&
       &'1PP3PC  ', '2PP3PC  ', '3PP3PC  ', '1PP3CC  ',&
       &'2PP3CC  ', '1PPSP3PC', '2PPSP3PC', '3PPSP3PC',&
       &'1SP3PC  ', '2SP3PC  ', '1PP1    ', '1PP2    ',&
       &'2PP2    ', '1PP3    '/

  data REF12COD /'1PP1TH(BH)   ', '2PP1TH(BH)   ', '3PP1TH(BH)   ',&
       &'4PP1TH(BH)   ', '5PP1TH(BH)   ', '6PP1TH(BH)   ',&
       &'7PP1TH(BH)   ', '1PP2PC       ', '2PP2PC       ',&
       &'3PP2PC       ', '4PP2PC       ', '5PP2PC       ',&
       &'1PP3PC       ', '2PP3PC       ', '3PP3PC       ',&
       &'1PP3CC       ', '2PP3CC       ', '1PPSP3PC     ',&
       &'2PPSP3PC     ', '3PPSP3PC     ', '1SP3PC       ',&
       &'2SP3PC       ', '1PP1(BH)     ', '1PP2(BH)     ',&
       &'2PP2(BH)     ', '1PP3(BH)     ', '1AZPPSPPRE01 ',&
       &'1AZPPSPPRE02 ', '1AZPPSPPRE03 ', '1AZPPSPPRE04 ',&
       &'1MC2         ', '2MC2         ', '3MC2         ',&
       &'1MC3         ', '2MC3         ', '3MC3         ',&
       &'1PP&ASSOC3   ', '2PP&ASSOC3   ', '3PP&ASSOC3   ',&
       &'4PP&ASSOC3   ', '5PP&ASSOC3   ', '1PP&ASSOC4   ',&
       &'2PP&ASSOC4   ', '3PP&ASSOC4   ', '1PP1         ',&
       &'2PP1         ', '3PP1         ', '1PP2(PNW105) ',&
       &'2PP2(PNW105) ', '3PP2(PNW105) ', '4PP2(PNW105) ',&
       &'1PP3(PNW105) ', '2PP3(PNW105) ', '3PP3(PNW105) ',&
       &'4PP3(PNW105) ', '5PP3         ', '6PP3         ',&
       &'7PP3         ', '8PP3         ', '1PP4(PNW105) ',&
       &'2PP4(PNW105) ', '3PP4(PNW105) ', '4PP4         ',&
       &'1JU2         ', '2JU2         ', '1PP4PC       ',&
       &'2PP4PC       ', '3PP4PC       ', '4PP4PC       ',&
       &'5PP4PC       ', '1PP1TH       ', '2PP1TH       ',&
       &'3PP1TH       ', '4PP1TH       ', '5PP1TH       ',&
       &'6PP1TH       ', '1PP2         ', '2PP2         ',&
       &'3PP2         ', '4PP2         ', '1PP3         ',&
       &'2PP3         ', '3PP3         ', '4PP3         ',&
       &'1PP4         ', '2PP4         ', '3PP4         ',&
       &'3WF2         ', '4WF3         ', '3WF3         '/

  data REF13COD / '1DF4CC  ', '2DF4CC  ', '3DF4CC  ', '4DF4CC  ',&
       &'5DF4CC  ', '6DF4CC  ', '7DF4CC  ', '8DF4CC  ',&
       &'9DF4CC  ', '10DF4CC ', '1DF4PC  ', '2DF4PC  ',&
       &'3DF4PC  ', '4DF4PC  ', '5DF4PC  ', '6DF4PC  ',&
       &'7DF4PC  ', '8DF4PC  ', '9DF4PC  ', '1DF3PC  ',&
       &'2DF3PC  ', '3DF3PC  ', '4DF3PC  ', '5DF3PC  ',&
       &'6DF3PC  ', '1DF1TH  ', '2DF1TH  ', '3DF1TH  ',&
       &'4DF1TH  ', '1DFHD4CC', '2DFHD4CC', '3DFHD4CC',&
       &'4DFHD4CC', '5DFHD4CC', '6DFHD4CC', '7DFHD4CC',&
       &'1DFHD4PC', '2DFHD4PC', '3DFHD4PC', '4DFHD4PC',&
       &'5DFHD4PC', '6DFHD4PC'/

  data REF14COD /'BG01','BG02','BG03','BG04','MC01',&
       &'MC02','MC03','MC04','MC05','MC06',&
       &'MC07','MC08','MC09','MC10','MC11',&
       &'MC12','MC13','MC14','MC15','MC16',&
       &'MC17','SB01','SB02','SB03','SB04',&
       &'WJ01','WJ02','WJ03','WJ04'/

  data REF15COD /'BG01','BG02','BG03','BG04','MC01',&
       &'MC02','MC03','MC04','MC05','MC06',&
       &'MC07','MC08','MC09','MC10','MC11',&
       &'MC12','MC13','MC14','MC15','MC16',&
       &'MC17','SB01','SB02','SB03','SB04',&
       &'WJ01','WJ02','WJ03','WJ04'/

  data REF16COD / 'AH01','AH02','AH03','AH04','AH05',&
       &'AH06','AH07','AH08','AH09','AH10',&
       &'AH11','AH12','AH13','AH14','AH15',&
       &'BS01','BS02','BS03','BS04','BS05',&
       &'BS06','BS07','BS08','BS09','BS10',&
       &'BS11','BS12','BS13','BS14','WS01',&
       &'WS02','WS03','WS04','WS05','WS06',&
       &'WS07','WS08','WS09','WS10','WS11',&
       &'WS12'/

  data REF17COD / 'GO01','GO02','GO03','GO04','GO05',&
       &'GO06','GO07','GO08','GO09','LP01',&
       &'LP02','LP03','LP04','LP05','LP06',&
       &'LP07','LP08','LP09','LP10','LP11',&
       &'LP12','LP13','QA01','QA02','QA03',&
       &'QA04','QA05','QA06','QA07','QA08',&
       &'QA09','QA10','QA11','QA12','QA13'/

  data REF18COD / 'JP01', 'JP02', 'JP03', 'JP04', 'JP05',&
       &'JP06', 'JP07', 'JP08', 'JP09', 'JP10',&
       &'JP11', 'JP12', 'JP13', 'JP14', 'JP15',&
       &'JP16', 'JP17', 'JP18', 'JP19', 'MO01',&
       &'MO02', 'MO03', 'MO04', 'MO05', 'MO06',&
       &'MO07', 'MO08', 'MO09', 'MO10', 'MO11',&
       &'MP01', 'MP02', 'MP03', 'MP04', 'MP05',&
       &'MP06', 'MP07', 'MP08', 'MP09', 'MP10',&
       &'MP11', 'MP12', 'MP13'/

  data REF19COD /'HP01 ', 'HP02 ', 'HP03 ', 'HP04 ', 'HP05 ',&
       &'HP06 ', 'HP07 ', 'LLP01', 'LLP02', 'LLP03',&
       &'LLP04', 'LLP05', 'LLP06', 'LLP07', 'LLP08',&
       &'LLP09', 'LLP10', 'PW01 ', 'PW02 ', 'SH01 ',&
       &'SH02 ', 'SH03 ', 'SH04 ', 'SH05 ', 'SH06 ',&
       &'SH07 ', 'SH08 ', 'SH09 ', 'SH10 ', 'SH11 ',&
       &'SPS01', 'SPS02', 'SPS03', 'SPS04'/

  data REF20COD /'1PP1TH  ', '2PP1TH  ', '3PP1TH  ', '4PP1TH  ',&
       &'5PP1TH  ', '6PP1TH  ', '7PP1TH  ', '1PP2PC  ',&
       &'2PP2PC  ', '3PP2PC  ', '4PP2PC  ', '5PP2PC  ',&
       &'1PP3PC  ', '2PP3PC  ', '3PP3PC  ', '1PP3CC  ',&
       &'2PP3CC  ', '1PPSP3PC', '2PPSP3PC', '3PPSP3PC',&
       &'1SP3PC  ', '2SP3PC  ', '1PP1    ', '1PP2    ',&
       &'2PP2    ', '1PP3    '/

  data REF21COD /'PJ01  ', 'PJ02  ', 'PJ03  ', 'PJ04  ', 'PJ05  ',&
       &'PJ06  ', 'PJ07  ', 'PJ08  ', 'PJ09  ', 'PJ10  ',&
       &'PJ11  ', 'PJ12  ', 'PJ13  ', 'PJ14  ', 'SWSB01',&
       &'SWSB02', 'SWSB03', 'SWSB04', 'SWSB05', 'SWSB06',&
       &'SWSB07', 'SWSB08', 'SWSB09', 'SWSB10', 'SWSB11'/

  data REF22COD /'HIF01', 'HIF02', 'HIF03', 'HIF04', 'HIF05',&
       &'HIF06', 'HIF07', 'HIF08', 'HIF09', 'HIG01',&
       &'HIG02', 'HIG03', 'HIG04', 'HIG05', 'HIG06',&
       &'HIG07', 'HIG08', 'HIG09', 'HIG10', 'HIG11',&
       &'HIG12', 'HIG13', 'HIS01', 'HIS02', 'HIS03',&
       &'HIS04', 'HIS05', 'HIS06', 'HIS07', 'HIW01',&
       &'HIW02', 'HIW03', 'HIW04', 'HIW05', 'HIW06',&
       &'HIW07'/

  data REF23COD / '1DFWHPRE01 ', '1DFWHPRE02 ', '1DFWHPRE03 ',&
       &'1DFWHPRE04 ', '1DFWHPRE05 ', '1DFWHPRE06 ',&
       &'1DFWHPRE07 ', '1DFWHPRE08 ', '1DFWHPRE09 ',&
       &'1DFWHPRE10 ', '1DFWHPRE11 ', '1DFWHPRE12 ',&
       &'1DFWHPRE13 ', '1DFWHPRE14 ', '1DFWHPRE15 ',&
       &'1DFWHPRE16 ', '1DFWHPRE17 ', '1DFWHPRE18 ',&
       &'1DFWHPRE19 ', '2DFWHPOST01', '2DFWHPOST02',&
       &'2DFWHPOST03', '2DFWHPOST04', '2DFWHPOST05',&
       &'2DFWHPOST06', '2DFWHPOST07'/

  data REF24COD / '1LL2N', '2LL2H', '3LL3N', '4LL2H', '5LL1P',&
       &'6LL3H', '7LL3H', '8LL3N', '9LL3H', '1WP3N',&
       &'2WP2P', '3WP3N', '4WP3H', '5WP3H', '6WP2H',&
       &'7WP3N', '1PP1N', '2PP2N', '3PP1N', '4PP1N',&
       &'5PP2N', '6PP2N', '7PP3H', '1VP2N', '2VP2N',&
       &'3VP3N', '4VP2N' /

  data REF25COD /'1A21N  ', '2A22N  ', '3B21N  ', '4A22N  ',&
       &'5B12N  ', '6A12N  ', '7B22N  ', '8A22N  ',&
       &'9A11N  ', '10A22CC', '11B22CC', '12A22CC',&
       &'13A22CC', '14B23CC'/

  data REF26COD /'FC1PRE ', 'FC1POST', 'FC2PRE ', 'FC2POST',&
       &'FC3PRE ', 'FC3POST', 'FC4PRE ', 'FC4POST',&
       &'FC5PRE ', 'FC5POST', 'FC6PRE ', 'FC6POST',&
       &'FC7PRE ', 'FC7POST', 'FC8PRE ', 'FC8POST'/

  data REF27COD / 'CDO01', 'CDO02', 'CDO03', 'CDO04', 'CDO05',&
       &'CDO06', 'CDO07', 'CDO08', 'CDO09', 'MCS01',&
       &'MCS02', 'MCS03', 'MCS04', 'MCS05', 'MCS06',&
       &'MCS07', 'MCS08', 'MCS09', 'MCS10', 'MCS11',&
       &'WO01 ', 'WO02 ', 'WO03 ', 'WO04 ', 'WO05 ',&
       &'WO06 ', 'WO07 ', 'WO08 ', 'WO09 ', 'WO10 '/

  data REF28COD / '1MC4RC', '2MC4RC', '3MC4RC',&
       &'1MC4PC', '2MC4PC', '3MC4PC',&
       &'4MC4PC', '5MC4PC', '6MC4PC',&
       &'7MC4PC', '8MC4PC', '1MC3PC',&
       &'2MC3PC', '3MC3PC', '4MC3PC',&
       &'5MC3PC', '6MC3PC', '7MC3PC',&
       &'8MC3PC', '1TF4RC', '2TF4RC',&
       &'3TF4RC', '4TF4RC', '5TF4RC',&
       &'6TF4RC', '1TF4PC', '2TF4PC',&
       &'3TF4PC', '4TF4PC', '5TF4PC'/

  data REF29COD /'6A ', '6B ', '8A ',&
       &'8B ', '10A', '10B',&
       &'12A', '12B', '14A',&
       &'14B', '16A', '16B',&
       &'18A', '18B', '20A',&
       &'20B' /

  data REF30COD / '3D    ', '3DPOST', '2A    ',&
       &'2APOST', '3B    ', '3BPOST',&
       &'2C    ', '2CPOST', '2D    ',&
       &'2DPOST', '1A    ', '1APOST',&
       &'1C    ', '1CPOST', '1D    ',&
       &'1DPOST'/

  data REF31COD / '1 ', '2 ', '3 ', '4 ', '5 ',&
       &'6 ', '7 ', '8 ', '9 ', '10'/

  data REF32COD /'1A ', '1B ', '1C ', '2A ', '2B ', '3A ', '3B ',&
       &'4A ', '4B ', '4C ', '5A ', '5B ', '5C ', '6A ',&
       &'6B ', '6C ', '7A ', '7B ', '7C ', '8A ', '9A ',&
       &'9B ', '10A', '10B', '10C', '11A', '12A', '12B',&
       &'12C', '13A', '13B', '13C', '13D', '13E', '14A',&
       &'14B', '14C', '14D', '14E'/

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'FMPHOTOCODE',11,ICYC)
  if (DEBUG) write(JOSTND,7) ICYC
  7 format(' ENTERING FMPHOTOCODE CYCLE = ',I2)

  if (ICALL .eq.0) go to 70

  !  DECODE FUELS PHOTO CODE FIELD BASED ON THE PHOTO REFERENCE NUMBER.  STRIP OUT ANY DASHES BECAUSE
  !  FVS AND FSVEG ARE NOT CONSISTENT WITH THEM.

  FOTOCODE = -1

  do I = 1,13
     call UPCASE(CFOTOCODE(I:I))
     if (CFOTOCODE(I:I).eq.'-') then
        CFOTOTMP = CFOTOCODE
        if (I.lt.13) then
           CFOTOCODE = CFOTOTMP(1:I-1)//CFOTOTMP(I+1:)
        else
           CFOTOCODE = CFOTOTMP(1:I-1)
        end if
     end if
  end do

  select case (FOTOREF)
  case (1)
     N = 22
     do I = 1, N
        if (CFOTOCODE(1:3) .eq. REF1COD(I)) then
           FOTOCODE = I
           go to 50
        end if
     end do

  case (2)
     N = 59
     do I = 1, N
        if (CFOTOCODE(1:3) .eq. REF2COD(I)) then
           FOTOCODE = I
           go to 50
        end if
     end do

  case (3)
     N = 66
     do I = 1, N
        if (CFOTOCODE(1:3) .eq. REF3COD(I)) then
           FOTOCODE = I
           go to 50
        end if
     end do

  case (5)
     N = 17
     do I = 1, N
        if (CFOTOCODE(1:6) .eq. REF5COD(I)) then
           FOTOCODE = I
           go to 50
        end if
     end do

  case (6)
     N = 27
     do I = 1, N
        if (CFOTOCODE(1:12) .eq. REF6COD(I)) then
           FOTOCODE = I
           go to 50
        end if
     end do

  case (7)
     N = 56
     do I = 1, N
        if (CFOTOCODE(1:4) .eq. REF7COD(I)) then
           FOTOCODE = I
           go to 50
        end if
     end do

  case (8)
     N = 86
     do I = 1, N
        if (CFOTOCODE(1:10) .eq. REF8COD(I)) then
           FOTOCODE = I
           go to 50
        end if
     end do

  case (9)
     N = 26
     do I = 1, N
        if (CFOTOCODE(1:11) .eq. REF9COD(I)) then
           FOTOCODE = I
           go to 50
        end if
     end do

  case (11)
     N = 26
     do I = 1, N
        if (CFOTOCODE(1:8) .eq. REF11COD(I)) then
           FOTOCODE = I
           go to 50
        end if
     end do

  case (12)
     N = 90
     do I = 1, N
        if (CFOTOCODE(1:13) .eq. REF12COD(I)) then
           FOTOCODE = I
           go to 50
        end if
     end do

  case (13)
     N = 42
     do I = 1, N
        if (CFOTOCODE(1:8) .eq. REF13COD(I)) then
           FOTOCODE = I
           go to 50
        end if
     end do

  case (14)
     N = 29
     do I = 1, N
        if (CFOTOCODE(1:4) .eq. REF14COD(I)) then
           FOTOCODE = I
           go to 50
        end if
     end do

  case (15)
     N = 29
     do I = 1, N
        if (CFOTOCODE(1:4) .eq. REF15COD(I)) then
           FOTOCODE = I
           go to 50
        end if
     end do

  case (16)
     N = 41
     do I = 1, N
        if (CFOTOCODE(1:4) .eq. REF16COD(I)) then
           FOTOCODE = I
           go to 50
        end if
     end do

  case (17)
     N = 35
     do I = 1, N
        if (CFOTOCODE(1:4) .eq. REF17COD(I)) then
           FOTOCODE = I
           go to 50
        end if
     end do

  case (18)
     N = 43
     do I = 1, N
        if (CFOTOCODE(1:4) .eq. REF18COD(I)) then
           FOTOCODE = I
           go to 50
        end if
     end do

  case (19)
     N = 34
     do I = 1, N
        if (CFOTOCODE(1:5) .eq. REF19COD(I)) then
           FOTOCODE = I
           go to 50
        end if
     end do

  case (20)
     N = 26
     do I = 1, N
        if (CFOTOCODE(1:8) .eq. REF20COD(I)) then
           FOTOCODE = I
           go to 50
        end if
     end do

  case (21)
     N = 25
     do I = 1, N
        if (CFOTOCODE(1:6) .eq. REF21COD(I)) then
           FOTOCODE = I
           go to 50
        end if
     end do

  case (22)
     N = 36
     do I = 1, N
        if (CFOTOCODE(1:5) .eq. REF22COD(I)) then
           FOTOCODE = I
           go to 50
        end if
     end do

  case (23)
     N = 26
     do I = 1, N
        if (CFOTOCODE(1:11) .eq. REF23COD(I)) then
           FOTOCODE = I
           go to 50
        end if
     end do

  case (24)
     N = 27
     do I = 1, N
        if (CFOTOCODE(1:5) .eq. REF24COD(I)) then
           FOTOCODE = I
           go to 50
        end if
     end do

  case (25)
     N = 14
     do I = 1, N
        if (CFOTOCODE(1:7) .eq. REF25COD(I)) then
           FOTOCODE = I
           go to 50
        end if
     end do

  case (26)
     N = 16
     do I = 1, N
        if (CFOTOCODE(1:7) .eq. REF26COD(I)) then
           FOTOCODE = I
           go to 50
        end if
     end do

  case (27)
     N = 30
     do I = 1, N
        if (CFOTOCODE(1:5) .eq. REF27COD(I)) then
           FOTOCODE = I
           go to 50
        end if
     end do

  case (28)
     N = 30
     do I = 1, N
        if (CFOTOCODE(1:6) .eq. REF28COD(I)) then
           FOTOCODE = I
           go to 50
        end if
     end do

  case (29)
     N = 16
     do I = 1, N
        if (CFOTOCODE(1:3) .eq. REF29COD(I)) then
           FOTOCODE = I
           go to 50
        end if
     end do

  case (30)
     N = 16
     do I = 1, N
        if (CFOTOCODE(1:6) .eq. REF30COD(I)) then
           FOTOCODE = I
           go to 50
        end if
     end do

  case (31)
     N = 10
     do I = 1, N
        if (CFOTOCODE(1:2) .eq. REF31COD(I)) then
           FOTOCODE = I
           go to 50
        end if
     end do

  case (32)
     N = 39
     do I = 1, N
        if (CFOTOCODE(1:3) .eq. REF32COD(I)) then
           FOTOCODE = I
           go to 50
        end if
     end do

  end select

  50 continue

  if (DEBUG) write(JOSTND,58) FOTOREF,FOTOCODE,CFOTOCODE
  58 format(' IN FMPHOTOCODE, FOTOREF=',I4,' FOTOCODE=',I4,&
       &' CFOTOCODE=',A)

  if (FOTOCODE .eq. -1) then
     WRITE (JOSTND,"(/1X,'*** FFE MODEL WARNING: INCORRECT ',        &
          &  'PHOTO REFERENCE OR PHOTO CODE ENTERED.',/1X)")
     call RCDSET (2,.true.)
  end if
  return

  70 continue

  select case (FOTOREF)
  case (1)
     CFOTOCODE = REF1COD(FOTOCODE)

  case (2)
     CFOTOCODE = REF2COD(FOTOCODE)

  case (3)
     CFOTOCODE = REF3COD(FOTOCODE)

  case (5)
     CFOTOCODE = REF5COD(FOTOCODE)

  case (6)
     CFOTOCODE = REF6COD(FOTOCODE)

  case (7)
     CFOTOCODE = REF7COD(FOTOCODE)

  case (8)
     CFOTOCODE = REF8COD(FOTOCODE)

  case (9)
     CFOTOCODE = REF9COD(FOTOCODE)

  case (11)
     CFOTOCODE = REF11COD(FOTOCODE)

  case (12)
     CFOTOCODE = REF12COD(FOTOCODE)

  case (13)
     CFOTOCODE = REF13COD(FOTOCODE)

  case (14)
     CFOTOCODE = REF14COD(FOTOCODE)

  case (15)
     CFOTOCODE = REF15COD(FOTOCODE)

  case (16)
     CFOTOCODE = REF16COD(FOTOCODE)

  case (17)
     CFOTOCODE = REF17COD(FOTOCODE)

  case (18)
     CFOTOCODE = REF18COD(FOTOCODE)

  case (19)
     CFOTOCODE = REF19COD(FOTOCODE)

  case (20)
     CFOTOCODE = REF20COD(FOTOCODE)

  case (21)
     CFOTOCODE = REF21COD(FOTOCODE)

  case (22)
     CFOTOCODE = REF22COD(FOTOCODE)

  case (23)
     CFOTOCODE = REF23COD(FOTOCODE)

  case (24)
     CFOTOCODE = REF24COD(FOTOCODE)

  case (25)
     CFOTOCODE = REF25COD(FOTOCODE)

  case (26)
     CFOTOCODE = REF26COD(FOTOCODE)

  case (27)
     CFOTOCODE = REF27COD(FOTOCODE)

  case (28)
     CFOTOCODE = REF28COD(FOTOCODE)

  case (29)
     CFOTOCODE = REF29COD(FOTOCODE)

  case (30)
     CFOTOCODE = REF30COD(FOTOCODE)

  case (31)
     CFOTOCODE = REF31COD(FOTOCODE)

  case (32)
     CFOTOCODE = REF32COD(FOTOCODE)

  end select

  if (DEBUG) write(JOSTND,58) FOTOREF,FOTOCODE,CFOTOCODE

  return
end
