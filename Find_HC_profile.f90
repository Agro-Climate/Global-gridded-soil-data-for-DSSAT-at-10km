    !Author: Eunjin Han
    !Institute: IRI-Columbia University, NY
    !Date: 4/28/2015
    !=============================
    !!(1)	Estimate AWC for each soil layers up to 1m and add up for AWC of whole 1m depth
    !!TAW = 1000(FC - WP) Zr
    !    where
    !TAW the total available soil water in the root zone [mm],
    !FC the water content at field capacity [m3 m-3],
    !WP the water content at wilting point [m3 m-3],
    !!Zr the rooting depth [m].
    !(2) Find HWSD (or FAO) AWC class
    !(3) Find appropriate soil depth according to AWC class and soil texture.
    !-Note: IFPRI’s case there is no criteria for medium (~120cm) and deep (~180cm). Therefore, I modified the criteria as follows.
    !E:\IRI\IFPRI\soilgrid1km_SSA\ rooting_depth_estimation.xlsx
    !
    !(4) Once rooting depth is determined (and soil texture and fertility), find corresponding HC27 soil type
    !(5) From the defined HC27 soil profile, assign the first layer of *.SOL and SLNI, SRGF for each layer using weighted avg.
    !=======================================

    subroutine Find_HC_profile(row_i,col_i)
    !
    use ModuleGEN
    IMPLICIT NONE

    real :: sum_awc !availabel water capacity
    integer :: row_i,col_i, ii,hwsd_class
    real, dimension(6):: fc, wp, sdepth
    real :: sand_5m,clay_5m, SOC
    character(len=1):: s_texture,hc_depth,hc_fert
    character  ::  What_tex_4HC*1

    do ii=1,6
        fc(ii)= SDUL_5m(row_i,col_i,ii)
        wp(ii)= SLLL_5m(row_i,col_i,ii)
    end do

    !soil layer depths for SoilGrids
    sdepth(1)=5
    sdepth(2)=15
    sdepth(3)=30
    sdepth(4)=60
    sdepth(5)=100
    sdepth(6)=200

    !initialize
    sum_awc=0

    !(1)find AWC for 1m depth soil layer
    do ii=1,5

        !check error
        if(fc(ii) < 0 .OR. wp(ii) < 0) then
            write(6,*)'**ERROR: In Find_HC_profiel subroutine: fc or wp is less than 0!'
            !  pause;
        end if

        if(ii .eq. 1) then
            if(fc(ii) > 0 .AND. wp(ii) > 0) then  !-99 : EJ(5/2/2015) due to unavailabitlity of SOC, derived variables are not available.
                sum_awc=sum_awc+(fc(ii)-wp(ii))*sdepth(ii)*10  !unit: mm
            end if
        else
            if(fc(ii) > 0 .AND. wp(ii) > 0) then  !-99 : EJ(5/2/2015) due to unavailabitlity of SOC, derived variables are not available.
                sum_awc=sum_awc+(fc(ii)-wp(ii))*(sdepth(ii)-sdepth(ii-1))*10 !unit: mm
            end if
        end if

    end do

    !(2) Find HWSD (or FAO) AWC class
    !Class	AWC
    !1	150 mm/m
    !2	125 mm/m
    !3	100 mm/m
    !4	75 mm/m
    !5	50 mm/m
    !6	15 mm/m
    !7	0 mm/m
    if(sum_awc .gt. 0 .AND. sum_awc .le. 15) then
        hwsd_class=7
    else if(sum_awc .gt. 15 .AND. sum_awc .le. 50) then
        hwsd_class=6
    else if(sum_awc .gt. 50 .AND. sum_awc .le. 75) then
        hwsd_class=5
    else if(sum_awc .gt. 75 .AND. sum_awc .le. 100) then
        hwsd_class=4
    else if(sum_awc .gt. 100 .AND. sum_awc .le. 125) then
        hwsd_class=3
    else if(sum_awc .gt. 125 .AND. sum_awc .le. 150) then
        hwsd_class=2
    else  !greater than 150
        hwsd_class=1
    end if

    !(3) Find appropriate soil depth according to AWC class and soil texture.
    !Find soil tpye from soil texture classification triangle
    sand_5m=(1-SLCL_5m(row_i,col_i,1)-SLSI_5m(row_i,col_i,1))*100
    clay_5m=SLCL_5m(row_i,col_i,1)*100  !%

    if(sand_5m .LT. 0 .OR. clay_5m .LT. 0 ) then
        print *, 'negative sand',sand_5m,SLCL_5m(row_i,col_i,1),SLSI_5m(row_i,col_i,1), row_i,col_i
        pause;
    end if
    !debug
    !print *, 'before what_tex_4HC:', sand_5m, clay_5m
    s_texture = What_tex_4HC(sand_5m, clay_5m)  !return value is "S", "C", "L" => sand, clay, loam

    if(s_texture .eq. 'C') then
        if(hwsd_class .eq. 1) then
            hc_depth='D' !deep
        else if(hwsd_class .eq. 2) then
            hc_depth='M' !medium
        else if(hwsd_class .ge. 3) then
            hc_depth='S' !shallow
        end if
    else if(s_texture .eq. 'L') then
        if(hwsd_class .eq. 1) then
            hc_depth='D' !deep
        else if(hwsd_class .eq. 2 .OR. hwsd_class .eq. 3) then
            hc_depth='M' !medium
        else if(hwsd_class .ge. 4) then
            hc_depth='S' !shallow
        end if
    else if(s_texture .eq. 'S') then
        if(hwsd_class .eq. 1 .OR. hwsd_class .eq. 2) then
            hc_depth='D' !deep
        else if(hwsd_class .eq. 3 .OR. hwsd_class .eq. 4) then
            hc_depth='M' !medium
        else if(hwsd_class .ge. 5) then
            hc_depth='S' !shallow
        end if
    else
        write(6,*)'**Find_HC_profile: ERROR: No soil texture is defined!'
        pause;
    end if

    !Determin soil fertility using SOC
    ! APPENDIX B. BOUNDARY CONDITION: FERTILITY, %[SOC of TOP SOIL]
    ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! HIGH: 1.2 <= SOC
    ! MED:  0.7 <= SOC < 1.2
    ! LOW:  0.0 <= SOC < 0.7
    SOC= SLOC_5m(row_i,col_i,1)*100  !EJ(Nov 12, 2014) Change vol % (cm3/cm3) to % for *.SOL
    if (SOC .ge. 1.2) then
        hc_fert='H'
    else if (SOC .ge. 0.7 .AND. SOC .lt. 1.2) then
        hc_fert='M'
    else if (SOC .ge. 0.0 .AND. SOC .lt. 0.7) then
        hc_fert='L'
    else
        write(6,*)'**ERROR: soil fertility is NOT defined!',SLOC_5m(row_i,col_i,1),row_i,col_i
        pause;
    end if

    !(4) Once rooting depth is determined (and soil texture and fertility), find corresponding HC27 soil type
    if(s_texture .eq. 'C' .AND. hc_fert .eq. 'H' .AND. hc_depth .eq. 'D') then
        rep_HCID='HC_GEN0001'
    else if(s_texture .eq. 'C' .AND. hc_fert .eq. 'H' .AND. hc_depth .eq. 'M') then
        rep_HCID='HC_GEN0002'
    else if(s_texture .eq. 'C' .AND. hc_fert .eq. 'H' .AND. hc_depth .eq. 'S') then
        rep_HCID='HC_GEN0003'
    else if(s_texture .eq. 'C' .AND. hc_fert .eq. 'M' .AND. hc_depth .eq. 'D') then
        rep_HCID='HC_GEN0004'
    else if(s_texture .eq. 'C' .AND. hc_fert .eq. 'M' .AND. hc_depth .eq. 'M') then
        rep_HCID='HC_GEN0005'
    else if(s_texture .eq. 'C' .AND. hc_fert .eq. 'M' .AND. hc_depth .eq. 'S') then
        rep_HCID='HC_GEN0006'
    else if(s_texture .eq. 'C' .AND. hc_fert .eq. 'L' .AND. hc_depth .eq. 'D') then
        rep_HCID='HC_GEN0007'
    else if(s_texture .eq. 'C' .AND. hc_fert .eq. 'L' .AND. hc_depth .eq. 'M') then
        rep_HCID='HC_GEN0008'
    else if(s_texture .eq. 'C' .AND. hc_fert .eq. 'L' .AND. hc_depth .eq. 'S') then
        rep_HCID='HC_GEN0009'
    else if(s_texture .eq. 'L' .AND. hc_fert .eq. 'H' .AND. hc_depth .eq. 'D') then
        rep_HCID='HC_GEN0010'
    else if(s_texture .eq. 'L' .AND. hc_fert .eq. 'H' .AND. hc_depth .eq. 'M') then
        rep_HCID='HC_GEN0011'
    else if(s_texture .eq. 'L' .AND. hc_fert .eq. 'H' .AND. hc_depth .eq. 'S') then
        rep_HCID='HC_GEN0012'
    else if(s_texture .eq. 'L' .AND. hc_fert .eq. 'M' .AND. hc_depth .eq. 'D') then
        rep_HCID='HC_GEN0013'
    else if(s_texture .eq. 'L' .AND. hc_fert .eq. 'M' .AND. hc_depth .eq. 'M') then
        rep_HCID='HC_GEN0014'
    else if(s_texture .eq. 'L' .AND. hc_fert .eq. 'M' .AND. hc_depth .eq. 'S') then
        rep_HCID='HC_GEN0015'
    else if(s_texture .eq. 'L' .AND. hc_fert .eq. 'L' .AND. hc_depth .eq. 'D') then
        rep_HCID='HC_GEN0016'
    else if(s_texture .eq. 'L' .AND. hc_fert .eq. 'L' .AND. hc_depth .eq. 'M') then
        rep_HCID='HC_GEN0017'
    else if(s_texture .eq. 'L' .AND. hc_fert .eq. 'L' .AND. hc_depth .eq. 'S') then
        rep_HCID='HC_GEN0018'
    else if(s_texture .eq. 'S' .AND. hc_fert .eq. 'H' .AND. hc_depth .eq. 'D') then
        rep_HCID='HC_GEN0019'
    else if(s_texture .eq. 'S' .AND. hc_fert .eq. 'H' .AND. hc_depth .eq. 'M') then
        rep_HCID='HC_GEN0020'
    else if(s_texture .eq. 'S' .AND. hc_fert .eq. 'H' .AND. hc_depth .eq. 'S') then
        rep_HCID='HC_GEN0021'
    else if(s_texture .eq. 'S' .AND. hc_fert .eq. 'M' .AND. hc_depth .eq. 'D') then
        rep_HCID='HC_GEN0022'
    else if(s_texture .eq. 'S' .AND. hc_fert .eq. 'M' .AND. hc_depth .eq. 'M') then
        rep_HCID='HC_GEN0023'
    else if(s_texture .eq. 'S' .AND. hc_fert .eq. 'M' .AND. hc_depth .eq. 'S') then
        rep_HCID='HC_GEN0024'
    else if(s_texture .eq. 'S' .AND. hc_fert .eq. 'L' .AND. hc_depth .eq. 'D') then
        rep_HCID='HC_GEN0025'
    else if(s_texture .eq. 'S' .AND. hc_fert .eq. 'L' .AND. hc_depth .eq. 'M') then
        rep_HCID='HC_GEN0026'
    else if(s_texture .eq. 'S' .AND. hc_fert .eq. 'L' .AND. hc_depth .eq. 'S') then
        rep_HCID='HC_GEN0027'
    else
        write(6,*)'**ERROR: NO Rep_HCIC is determined!'
        pause;
    end if

    end subroutine Find_HC_profile