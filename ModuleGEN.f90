    !Eunjin Han
    !
    !Date: 7/24/2014
    !Institution: IRI-Columbia University, NY

    module ModuleGEN

    !DEPOSIT ALL DECLARATIONS HERE!!!
    IMPLICIT NONE
    !SAVE

    real, dimension (1200,1) :: snd1,snd2,snd3,snd4,snd5,snd6
    real, dimension (1200,1) :: slt1,slt2,slt3,slt4,slt5,slt6
    real, dimension (1200,1) :: cly1,cly2,cly3,cly4,cly5,cly6
    real, dimension (1200,1) :: bld1,bld2,bld3,bld4,bld5,bld6
    real, dimension (1200,1) :: cec1,cec2,cec3,cec4,cec5,cec6
    real, dimension (1200,1) :: orc1,orc2,orc3,orc4,orc5,orc6
    real, dimension (1200,1) :: phih1,phih2,phih3,phih4,phih5,phih6
    integer, dimension (1200,1) :: NoData_flag

    integer :: irow, jcol,istat,i,j
    integer :: num_row, num_col
    character(len=200):: line1,line2,line3,line4,line5
    real :: lat, lon, cellsize

    character(len=8):: ID
    character(len=12) :: new_ID
    integer, dimension(6):: LAYER
    real, dimension(6):: BulkD,T33,TS,T15,Ks,SoilC,SoilN !SDUL,SSAT, SLLL and SSKS

    real:: f1,f2,f3,f4
    real:: Int1

    integer :: n_ZLYR !number of soil layers
    real, dimension(6):: ZLYR,SLLL,SDUL,SSAT,SRGF,SSKS,SBDM
    real, dimension(6):: SAND, SLCL, SLSI,SLHW,SCEC,SLOC,SADC !From AfSIS  !SAHW(pH in water)
    real, dimension(6):: SLCF,SLNI,SLHB
    character(len=5), dimension(6):: SLMH

    !varaiables to read from HC.SOL
    character(len=10) :: COM,MHB, MPX, MKE
    real:: ALB, LU1, LDR, LRO, LNF, LPF
    real, dimension(20):: LYR,LLL,DUL,SAT,RGF,SKS,BDM
    real, dimension(20):: OC,LCL,LSI,LCF,LNI,LHW,LHB,CEC,ADC
    character(len=5), dimension(20):: LMH
    character(len=50) :: PEDON,LSOUR,LTXS,LDESC
    real:: LDP  !total soil depth

    character(len=2):: C_ID  !for country acronyms
    character(len=10):: soil_ID

    ! for unique sequential names for AfSIS grids
    integer :: i_CO, i_BR, i_ER, i_ET, i_KY, i_MW, i_MZ, i_RD
    integer :: i_RW, i_SO, i_SU, i_TA, i_UG, i_ZM

    !soil texture classification
    !character  :: s_class*42, what_texture*42
    !character(len=42):: s_class,what_texture



    real, dimension(6):: STONES,TOTN,PHKCL,ADCOEF !CLAY,SILT,PH,CEC,OC,
    character(len=5), dimension(20):: MH
    character(len=50) :: SLSOUR,SLTXS,SLDESC
    real:: SLDP  !total soil depth

    !EJ(4/28/2015)
    integer :: n_country
    integer, dimension(20):: ccode_list, value_list  !ASSUMING MAXIMUM 30 COUNTRRIES WITHIN 10 DEG PIXEL
    character(len=2), dimension(20):: cstr_list !ASSUMING MAXIMUM 30 COUNTRRIES WITHIN 10 DEG PIXEL
    integer, dimension(20):: country_count  !to make sequential numbers for each country's ID in Make_soil_ID.f90

    integer :: row_10km,col_10km  !counter for 10km pixels
    !3D (row, col, depth) array for 10km resolution within a 10deg pixel
    real, dimension(120,120,6) :: sum_SLCL,sum_SLSI,sum_SBDM,sum_SLOC,sum_SCEC,sum_SLHW, sum_SDUL,sum_SLLL,sum_SSAT,sum_SSKS
    integer, dimension(120,120,6) :: SLCL_count,SLSI_count,SBDM_count, SLOC_count,SCEC_count, SLHW_count,SDUL_count,SLLL_count,SSAT_count, SSKS_count
    real, dimension(120,120,6) ::SLCL_5m, SLSI_5m, SBDM_5m,SLOC_5m, SCEC_5m,SLHW_5m, SDUL_5m, SLLL_5m,SSAT_5m,SSKS_5m
    integer, dimension(120,120) :: Data_flag_5m
    integer, dimension (120,1) :: cntry_id
    character(len=10):: rep_HCID  !representative soil type within a HC27 cell
    integer :: num_row_5m, num_col_5m  !10km resolution

    integer, dimension (120,1) :: cell5m_id
    !integer, dimension (1200,1) :: cropland !REMOVE CROPLAND MASK (EJ: 9/28/2015)
    !real, dimension(120,120) :: sum_cland  !to determine cropland at 10km resolution   !REMOVE CROPLAND MASK (EJ: 9/28/2015)
    !integer, dimension(120,120) :: cland_5m  !REMOVE CROPLAND MASK (EJ: 9/28/2015)

    !to get output from Python
    character(len=80):: file_snd1,file_snd2,file_snd3,file_snd4,file_snd5,file_snd6
    character(len=80):: file_slt1,file_slt2,file_slt3,file_slt4,file_slt5,file_slt6
    character(len=80):: file_cly1,file_cly2,file_cly3,file_cly4,file_cly5,file_cly6
    character(len=80):: file_bld1,file_bld2,file_bld3,file_bld4,file_bld5,file_bld6
    character(len=80):: file_cec1,file_cec2,file_cec3,file_cec4,file_cec5,file_cec6
    character(len=80):: file_orcdrc1,file_orcdrc2,file_orcdrc3,file_orcdrc4,file_orcdrc5,file_orcdrc6
    character(len=80):: file_phih1,file_phih2,file_phih3,file_phih4,file_phih5,file_phih6
    character(len=80):: file_cellid,file_HClist,file_HCSOL,file_country,file_AFSOL,file_cropland
    integer :: xllcorner, yllcorner
    character(len=80):: fname_att,file_cell5mid
    character(len=80):: fname_ISOlist

    end module ModuleGEN