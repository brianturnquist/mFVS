!=====================!
!   common : METRIC   !
!=====================!
! COMMON $Id$

!     PARAMETERS FOR THE METRIC CONVERSION ARE:

! Variables
logical :: LMTRIC

integer :: N_METRIC_PRMS

real    :: CMtoIN,CMtoFT,MtoIN,MtoFT
real    :: INtoCM,FTtoCM,INtoM,FTtoM
real    :: M2toFT2,HAtoACR
real    :: FT2toM2,ACRtoHA
real    :: M3toFT3
real    :: FT3toM3
real    :: KGtoLB,TMtoTI
real    :: LBtoKG,TItoTM
real    :: M2pHAtoFT2pACR,M3pHAtoFT3pACR
real    :: FT2pACRtoM2pHA,FT3pACRtoM3pHA
real    :: TpHAtoTpACR,TpACRtoTpHA
real    :: MItoKM,KMtoMI
real    :: FtoC1,FtoC2,CtoF1,CtoF2
real    :: KJtoBTU,BTUtoKJ

!     Simple Metric to US Conversions

parameter (N_METRIC_PRMS = 13)
parameter (CMtoIN = 0.3937)
parameter (CMtoFT = 0.0328084)
parameter (MtoIN = 39.37)
parameter (MtoFT = 3.28084)
parameter (KMtoMI = 0.6214)

parameter (M2toFT2 = 10.763867)
parameter (HAtoACR = 2.471)

parameter (M3toFT3 = 35.314455)

parameter (KGtoLB = 2.2046226)
parameter (TMtoTI = 1.102311)

parameter (CtoF1 = 1.8)
parameter (CtoF2 = 32)

parameter (KJtoBTU = 0.9478171)

!     Simple US to Metric Conversions

parameter (INtoCM = 2.54)
parameter (FTtoCM = 30.48)
parameter (INtoM = 0.0254001)
parameter (FTtoM = 0.3048)
parameter (MItoKM = 1.609)

parameter (FT2toM2 = 0.0929034)
parameter (ACRtoHA = 0.4046945)

parameter (FT3toM3 = 0.028317)

parameter (LBtoKG = 0.4535924)
parameter (TItoTM = 0.90718)

parameter (FtoC1 = 0.554)
parameter (FtoC2 = -17.7)

parameter (BTUtoKJ = 1.0550559)

!     Complex Metric to US Conversions

parameter (M2pHAtoFT2pACR = 4.3560773)
parameter (M3pHAtoFT3pACR = 14.291564)
parameter (TpHAtoTpACR = 0.4460896)

!     Complex US to Metric Conversions

parameter (FT2pACRtoM2pHA = 0.2295643)
parameter (FT3pACRtoM3pHA = 0.0699713)
parameter (TpACRtoTpHA = 2.2417023)

!     LEGACY DECLARATION FOR WRD3

common /METRIC/ LMTRIC
