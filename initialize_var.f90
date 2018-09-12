    !Author: Eunjin Han
    !Institute: IRI-Columbia University, NY
    !modified: 4/28/2015
    !Date: 7/27/2014
    !!initialize all pixel counters for each country
    !===========================================================

    subroutine initialize_var

    use ModuleGEN
    implicit none

    !initialize count or matrix before staring a loop  !EJ(4/28)
    row_10km=1
    col_10km=1
    sum_SLCL=0.
    sum_SLSI=0.
    sum_SBDM=0.
    sum_SLOC=0.
    sum_SCEC=0.
    sum_SLHW=0.
    sum_SDUL=0.
    sum_SLLL=0.
    sum_SSAT=0.
    sum_SSKS=0.
    SLCL_count=0
    SLSI_count=0
    SBDM_count=0
    SLOC_count=0
    SCEC_count=0
    SLHW_count=0
    SDUL_count=0
    SLLL_count=0
    SSAT_count=0
    SSKS_count=0
    SLCL_5m=-99
    SLSI_5m=-99
    SBDM_5m=-99
    SLOC_5m=-99
    SCEC_5m=-99
    SLHW_5m=-99
    SDUL_5m=-99
    SLLL_5m=-99
    SSAT_5m=-99
    SSKS_5m=-99
     
    !sum_cland=0  !REMOVE CROPLAND MASK (EJ: 9/28/2015)
    !cland_5m=0

    !EJ(4/28) meaningless because I will make soil profile ID as a post-processing
    !i_CO=0
    !i_BR=0
    !i_ER=0
    !i_ET=0
    !i_KY=0
    !i_MW=0
    !i_MZ=0
    !i_RD=0
    !i_RW=0
    !i_SO=0
    !i_SU=0
    !i_TA=0
    !i_UG=0
    !i_ZM=0

    !soil layer depth
    ZLYR(1)=5
    ZLYR(2)=15
    ZLYR(3)=30
    ZLYR(4)=60
    ZLYR(5)=100
    ZLYR(6)=200

    !pre-define soil profile similar to HC27
    SLMH(1)='A'
    SLMH(2)='A'   !15cm
    SLMH(3)='AB'  !30cm
    SLMH(4)='BA'  !60cm
    SLMH(5)='B'   !100cm
    SLMH(6)='BC'  !200cm

    SADC=-99.0
    SLHB=-99.0
    SLCF=-99.0

    SRGF=0.0
    return

    end subroutine initialize_var