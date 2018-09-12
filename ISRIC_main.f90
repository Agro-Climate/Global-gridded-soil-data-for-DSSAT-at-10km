    !===================================================================================================!
    !PROGRAM NAME: Create_10km_global				                                        !									                                        !
    !DATE        : 9/28/2015
    !===================================================================================================!
    !---------------------------------------------------------------------------------------------------
    !PURPOSE: to create soil database for DSSAT (AF.SOL) using AfSIS soil data
    !         (% Sand, %Silt, Bulk density, Soil pH (1:5 soil/water solution),
    !         Cation Exchange Capacity (soil) in cmol/kg), Organic Carbon in permilles (g/kg)
    !---------------------------------------------------------------------------------------------------
    !===================================================================================================!
    !---------------------------------------------------------------------------------------------------
    program Create_10km_global

    use ModuleGEN !EJ: for common variable, use module
    use DFLIB
    use DFPORT
    IMPLICIT NONE


    integer, dimension(6):: Cell_ID
    character(len=11), dimension(6):: HC27ID
    character(len=7) :: str_id
    real, dimension(6):: share
    real :: max_share,temp,lon_first
    character(len=20)::temp_char, temp_ID

    character  :: s_class*9
    character  ::what_texture*9
    integer :: idepth, ii,i_10km
    character(len=10):: temp_str
    character(len=3):: ISO_A3 !EJ(6/4/2015)
    character(len=2):: ISO_A2  !EJ(6/4/2015)


    !for the command line
    character(len=90)   :: fparam
    integer(kind=2)     :: n1,status
    character(len=90)   :: buf

    logical:: EXISTS

    !retrieves command line using module dflib.f90
    n1=0
    do while(n1 .le. 1)
        call getarg(n1,buf,status)
        !error flag for command line
        if(status == -1)then
            write(6,*)'error in the command line:'
            write(6,*)'usage: <command> <file1>'
            write(6,*)'<file1>: parameter file - where data for simulations are given'
            pause;
            ! stop
        endif
        if(n1==1)fparam=buf
        n1=n1+1
    enddo
    !----end command line syntax-----

    write(6,'(a)')'<!---------------------------!>'
    write(6,'(a)')'<!Developing DSSAT-compatible soil profile based on ISRIC 1km soil information !>'
    write(6,'(a,/)')'<!---------------------------!>'

    !1) Read input from GUI (user-provided input)
    call Read_input_list(fparam)
    !====================================
    ! START THE MAIN PROGRAM
    !====================================

    !====================================
    ! 1. Open  AfSIS data in ASCII format
    !====================================

    open (unit=101, file=trim(file_snd1), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat
    open (unit=102, file=trim(file_snd2), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat
    open (unit=103, file=trim(file_snd3), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat
    open (unit=104, file=trim(file_snd4), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat
    open (unit=105, file=trim(file_snd5), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat
    open (unit=106, file=trim(file_snd6), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat

    open (unit=111, file=trim(file_slt1), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat
    open (unit=112, file=trim(file_slt2), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat
    open (unit=113, file=trim(file_slt3), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat
    open (unit=114, file=trim(file_slt4), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat
    open (unit=115, file=trim(file_slt5), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat
    open (unit=116, file=trim(file_slt6), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat

    open (unit=161, file=trim(file_cly1), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat
    open (unit=162, file=trim(file_cly2), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat
    open (unit=163, file=trim(file_cly3), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat
    open (unit=164, file=trim(file_cly4), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat
    open (unit=165, file=trim(file_cly5), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat
    open (unit=166, file=trim(file_cly6), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat

    open (unit=121, file=trim(file_bld1), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat
    open (unit=122, file=trim(file_bld2), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat
    open (unit=123, file=trim(file_bld3), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat
    open (unit=124, file=trim(file_bld4), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat
    open (unit=125, file=trim(file_bld5), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat
    open (unit=126, file=trim(file_bld6), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat


    open (unit=131, file=trim(file_cec1), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat
    open (unit=132, file=trim(file_cec2), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat
    open (unit=133, file=trim(file_cec3), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat
    open (unit=134, file=trim(file_cec4), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat
    open (unit=135, file=trim(file_cec5), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat
    open (unit=136, file=trim(file_cec6), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat


    open (unit=141, file=trim(file_orcdrc1), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat
    open (unit=142, file=trim(file_orcdrc2), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat
    open (unit=143, file=trim(file_orcdrc3), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat
    open (unit=144, file=trim(file_orcdrc4), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat
    open (unit=145, file=trim(file_orcdrc5), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat
    open (unit=146, file=trim(file_orcdrc6), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat

    open (unit=151, file=trim(file_phih1), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat
    open (unit=152, file=trim(file_phih2), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat
    open (unit=153, file=trim(file_phih3), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat
    open (unit=154, file=trim(file_phih4), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat
    open (unit=155, file=trim(file_phih5), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat
    open (unit=156, file=trim(file_phih6), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat

    open (unit=200, file=trim(file_cellid), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat

    open (unit=201, file=trim(file_cropland), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat

    open (unit=202, file=trim(file_HCSOL), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat

    open (unit=203, file=trim(file_country), STATUS='OLD', IOSTAT=istat )
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat

    open (unit=300, file=trim(file_AFSOL))
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat

    !to check negative cell_5m ID
    open (unit=1000, file=trim(file_cell5mid))
    if(istat /= 0) write (*,*) 'input file OPEN failed: istat=', istat
    write(1000,*)'lat  lon   cell5m_id(jcol,1)'

    !====================================
    ! 2. Read each row of ASCII input data
    !====================================
    num_row=1200
    num_col=1200
    num_row_5m=120  !10km resolution
    num_col_5m=120  !10km resolution

    !!  xllcorner=-20  !**need to be updated
    !! yllcorner=10    !**need to be updated

    !cellsize=0.0083333333333333 !1km resollution
    cellsize=0.083333333333333  !10km resolution
    !lat & lon of the first grid (UPPER left)
    lat=yllcorner+(cellsize*0.5)+cellsize*(num_row_5m-1) !!Center of the first row (latitute)
    lon=xllcorner+(cellsize*0.5) !!Lower left corner of a grid


    lon_first=lon !longitude of the first left cells

    !initialize
    temp=-99.0
    NoData_flag=1  !EJ(4/28): no data flag for 1km resolution - by default, all pixels have valid values (1)
    Data_flag_5m=1 !no data flag for 10km resolution

    !country_count=0 !to make sequential numbers for each country's ID in Make_soil_ID.f90
    !
    !!Find country name list within the 10 deg pixel => 'E:\\IRI\\IFPRI\\country_mask_10deg_tile\\CM_'+tile_name
    !!fname_asc=> ascii file converted from country raster
    !!fname_att => attribute table with actual ISO-N3 country code => 'E:\\IRI\\IFPRI\\country_mask_10deg_tile\\CM_'+tile_name+ '.txt'
    !!ccode_list => 3 digit country code (ISO_N3)
    !!cstr_list => 2 string country acronym (ISO)
    !!value-list => defalut values from Raster => need to find corresponding 3 digit country code (or acronym)
    !!n_country => number of countries within the target 10 deg pixel
    !!fname_asc= 'E:\IRI\IFPRI\country_mask_10deg_tile\cm_t377.asc'
    !!!    fname_att= 'E:\IRI\IFPRI\country_mask_5M_10deg\CM_T377.txt'
    !call configure_country(fname_att,value_list, ccode_list, cstr_list, n_country)

    call initialize_var  !initialize all pixel counters and 3D arrays before staring a loop

    do irow=1,num_row
        read (101, *) (snd1(jcol,1),jcol = 1,num_col)
        read (102, *) (snd2(jcol,1),jcol = 1,num_col)
        read (103, *) (snd3(jcol,1),jcol = 1,num_col)
        read (104, *) (snd4(jcol,1),jcol = 1,num_col)
        read (105, *) (snd5(jcol,1),jcol = 1,num_col)
        read (106, *) (snd6(jcol,1),jcol = 1,num_col)

        read (111, *) (slt1(jcol,1),jcol = 1,num_col)
        read (112, *) (slt2(jcol,1),jcol = 1,num_col)
        read (113, *) (slt3(jcol,1),jcol = 1,num_col)
        read (114, *) (slt4(jcol,1),jcol = 1,num_col)
        read (115, *) (slt5(jcol,1),jcol = 1,num_col)
        read (116, *) (slt6(jcol,1),jcol = 1,num_col)

        read (161, *) (cly1(jcol,1),jcol = 1,num_col)
        read (162, *) (cly2(jcol,1),jcol = 1,num_col)
        read (163, *) (cly3(jcol,1),jcol = 1,num_col)
        read (164, *) (cly4(jcol,1),jcol = 1,num_col)
        read (165, *) (cly5(jcol,1),jcol = 1,num_col)
        read (166, *) (cly6(jcol,1),jcol = 1,num_col)

        read (121, *) (bld1(jcol,1),jcol = 1,num_col)
        read (122, *) (bld2(jcol,1),jcol = 1,num_col)
        read (123, *) (bld3(jcol,1),jcol = 1,num_col)
        read (124, *) (bld4(jcol,1),jcol = 1,num_col)
        read (125, *) (bld5(jcol,1),jcol = 1,num_col)
        read (126, *) (bld6(jcol,1),jcol = 1,num_col)

        read (131, *) (cec1(jcol,1),jcol = 1,num_col)
        read (132, *) (cec2(jcol,1),jcol = 1,num_col)
        read (133, *) (cec3(jcol,1),jcol = 1,num_col)
        read (134, *) (cec4(jcol,1),jcol = 1,num_col)
        read (135, *) (cec5(jcol,1),jcol = 1,num_col)
        read (136, *) (cec6(jcol,1),jcol = 1,num_col)

        read (141, *) (orc1(jcol,1),jcol = 1,num_col)
        read (142, *) (orc2(jcol,1),jcol = 1,num_col)
        read (143, *) (orc3(jcol,1),jcol = 1,num_col)
        read (144, *) (orc4(jcol,1),jcol = 1,num_col)
        read (145, *) (orc5(jcol,1),jcol = 1,num_col)
        read (146, *) (orc6(jcol,1),jcol = 1,num_col)

        read (151, *) (phih1(jcol,1),jcol = 1,num_col)
        read (152, *) (phih2(jcol,1),jcol = 1,num_col)
        read (153, *) (phih3(jcol,1),jcol = 1,num_col)
        read (154, *) (phih4(jcol,1),jcol = 1,num_col)
        read (155, *) (phih5(jcol,1),jcol = 1,num_col)
        read (156, *) (phih6(jcol,1),jcol = 1,num_col)
        
        !read (201, *) (cropland(jcol,1),jcol = 1,num_col) !REMOVE CROPLAND MASK (EJ: 9/28/2015)

        NoData_flag=1 !initialize for every row
        col_10km=1
        do jcol=1,num_col
            !debug
            !  if(irow .eq. 1 .AND. jcol .eq. 555) then
            !if(irow .eq. 301 .AND. jcol .eq. 657) then
            !    print *,bld1(jcol,1),cly1(jcol,1),slt1(jcol,1), orc1(jcol,1)
            !end if
            !==========Convert data from AfSIS to *.SOL parameters (i.e. one array for all soil layers)
            call Convert2SOL  !=> generate NoData_flag based on availability of sand and silt

            !==========Compute soil characteristics using Saxton & Rawls (2006)
            call Compute_soil_param

            !Save the original "1km" soil parameters to 3D array to aggregrate to 10km
            do idepth=1,6
                !original data from AfSIS
                if (SLCL(idepth) >= 0) then
                    sum_SLCL(row_10km,col_10km,idepth)=sum_SLCL(row_10km,col_10km,idepth)+SLCL(idepth)
                    SLCL_count(row_10km,col_10km,idepth)=SLCL_count(row_10km,col_10km,idepth)+1
                end if
                if (SLSI(idepth) >= 0) then
                    sum_SLSI(row_10km,col_10km,idepth)=sum_SLSI(row_10km,col_10km,idepth)+SLSI(idepth)
                    SLSI_count(row_10km,col_10km,idepth)=SLSI_count(row_10km,col_10km,idepth)+1
                end if
                if (SBDM(idepth) >= 0) then
                    sum_SBDM(row_10km,col_10km,idepth)=sum_SBDM(row_10km,col_10km,idepth)+SBDM(idepth)
                    SBDM_count(row_10km,col_10km,idepth)=SBDM_count(row_10km,col_10km,idepth)+1
                end if
                if (SLOC(idepth) >= 0) then
                    sum_SLOC(row_10km,col_10km,idepth)=sum_SLOC(row_10km,col_10km,idepth)+SLOC(idepth)
                    SLOC_count(row_10km,col_10km,idepth)=SLOC_count(row_10km,col_10km,idepth)+1
                end if
                if (SCEC(idepth) >= 0) then
                    sum_SCEC(row_10km,col_10km,idepth)=sum_SCEC(row_10km,col_10km,idepth)+SCEC(idepth)
                    SCEC_count(row_10km,col_10km,idepth)=SCEC_count(row_10km,col_10km,idepth)+1
                end if
                if (SLHW(idepth) >= 0) then
                    sum_SLHW(row_10km,col_10km,idepth)=sum_SLHW(row_10km,col_10km,idepth)+SLHW(idepth)
                    SLHW_count(row_10km,col_10km,idepth)=SLHW_count(row_10km,col_10km,idepth)+1
                end if
                !debug
                !if(row_10km .eq. 31 .AND. col_10km .eq. 66 .AND. idepth .eq. 1) then
                !    print *, irow, jcol,SBDM(idepth),SLCL(idepth),SLSI(idepth), SLOC(idepth)
                !end if
                !Derived parameters
                if (SDUL(idepth) >= 0) then
                    sum_SDUL(row_10km,col_10km,idepth)=sum_SDUL(row_10km,col_10km,idepth)+SDUL(idepth)
                    SDUL_count(row_10km,col_10km,idepth)=SDUL_count(row_10km,col_10km,idepth)+1
                end if
                if (SLLL(idepth) >= 0) then
                    sum_SLLL(row_10km,col_10km,idepth)=sum_SLLL(row_10km,col_10km,idepth)+SLLL(idepth)
                    SLLL_count(row_10km,col_10km,idepth)=SLLL_count(row_10km,col_10km,idepth)+1
                end if
                if (SSAT(idepth) >= 0) then
                    sum_SSAT(row_10km,col_10km,idepth)=sum_SSAT(row_10km,col_10km,idepth)+SSAT(idepth)
                    SSAT_count(row_10km,col_10km,idepth)=SSAT_count(row_10km,col_10km,idepth)+1
                end if
                if (SSKS(idepth) >= 0) then
                    sum_SSKS(row_10km,col_10km,idepth)=sum_SSKS(row_10km,col_10km,idepth)+SSKS(idepth)
                    SSKS_count(row_10km,col_10km,idepth)=SSKS_count(row_10km,col_10km,idepth)+1
                end if
            end do

            !!!REMOVE CROPLAND MASK (EJ: 9/28/2015) aggregate 1km resolution cropland to 10km
            !sum_cland(row_10km,col_10km)=sum_cland(row_10km,col_10km)+cropland(jcol,1)
            !!check
            !if(cropland(jcol,1) < 0) then
            !    !    write(6,*)'**ERROR: negative values on cropland probability map!', cropland(jcol,1)
            !    ! pause;  !EJ(6/8)
            !end if


            if(mod(jcol,10) .eq. 0) then
                col_10km=col_10km+1
            end if

        end do  !do jcol=1,num_col

        !make average of the 1km pixels to get representaive values for 10km (5min) pixel
        if(mod(irow,10) .eq. 0) then
            !Get representative values for 10km grids for the current row_10km
            do i_10km=1,120  !120=col_10km-1
                if(SLCL_count(row_10km,i_10km,1) .eq. 0 .AND. SLSI_count(row_10km,i_10km,1) .eq. 0) then  !assumption: if both clay and silt for the first layer are not available, it is not possible to make soil profile for that pixel
                    !original data from AfSIS
                    SLCL_5m(row_10km,i_10km,:)=-99
                    SLSI_5m(row_10km,i_10km,:)=-99
                    SBDM_5m(row_10km,i_10km,:)=-99
                    SLOC_5m(row_10km,i_10km,:)=-99
                    SCEC_5m(row_10km,i_10km,:)=-99
                    SLHW_5m(row_10km,i_10km,:)=-99
                    !Derived parameters
                    SDUL_5m(row_10km,i_10km,:)=-99
                    SLLL_5m(row_10km,i_10km,:)=-99
                    SSAT_5m(row_10km,i_10km,:)=-99
                    SSKS_5m(row_10km,i_10km,:)=-99

                    Data_flag_5m(row_10km,i_10km)= 0
                else
                    do idepth=1,6
                        !check for errors
                        if (SLCL_count(row_10km,i_10km,idepth) .eq. 0) then
                            print *, 'SLCL_count is zero:',row_10km,i_10km,idepth
                            pause;
                        end if
                        if (SLSI_count(row_10km,i_10km,idepth) .eq. 0) then
                            print *, 'SLSI_count is zero:',row_10km,i_10km,idepth
                            pause;
                        end if
                        if (SBDM_count(row_10km,i_10km,idepth) .eq. 0) then
                            print *, 'SBDM_count is zero:',irow,jcol,row_10km,i_10km,idepth
                            SBDM_5m(row_10km,i_10km,idepth)=-99  !EJ(6/8)
                            ! pause;
                        end if
                        if (SCEC_count(row_10km,i_10km,idepth) .eq. 0) then
                            print *, 'SCEC_count is zero:',row_10km,i_10km,idepth
                            SCEC_5m(row_10km,i_10km,idepth)=-99  !EJ(6/8)
                            ! pause;
                        end if
                        if (SLCL_count(row_10km,i_10km,idepth) .eq. 0) then
                            print *, 'SLCL_count is zero:',row_10km,i_10km,idepth
                            pause;
                        end if
                        !if (SLHW_count(row_10km,i_10km,idepth) .eq. 0) then
                        !    !print *, 'SLHW_count is zero'
                        !    !pause;
                        !end if
                        !if (SDUL_count(row_10km,i_10km,idepth) .eq. 0) then
                        !    !print *, 'SDUL_count is zero'
                        !    !pause;
                        !end if
                        !if (SLLL_count(row_10km,i_10km,idepth) .eq. 0) then
                        !    !print *, 'SLLL_count is zero'
                        !    !pause;
                        !end if
                        !if (SSAT_count(row_10km,i_10km,idepth) .eq. 0) then
                        !    !print *, SSAT_count(row_10km,i_10km,idepth),SLCL_count(row_10km,i_10km,idepth),SLSI_count(row_10km,i_10km,idepth), SLOC_count(row_10km,i_10km,idepth)
                        !    !print *, row_10km,i_10km,idepth
                        !    !print *, SSAT_5m(row_10km,i_10km,idepth),SLCL_5m(row_10km,i_10km,idepth), SLSI_5m(row_10km,i_10km,idepth),SLOC_5m(row_10km,i_10km,idepth)
                        !    !pause;
                        !end if
                        ! if (SSKS_count(row_10km,i_10km,idepth) .eq. 0) pause;

                        if (SLOC_count(row_10km,i_10km,idepth) .gt. 0) then !********EJ(5/2/2015)
                            !original data from AfSIS
                            SLCL_5m(row_10km,i_10km,idepth)=sum_SLCL(row_10km,i_10km,idepth)/real(SLCL_count(row_10km,i_10km,idepth))
                            SLSI_5m(row_10km,i_10km,idepth)=sum_SLSI(row_10km,i_10km,idepth)/real(SLSI_count(row_10km,i_10km,idepth))
                            if (SBDM_count(row_10km,i_10km,idepth) .gt. 0) then !EJ(6/8)
                                SBDM_5m(row_10km,i_10km,idepth)=sum_SBDM(row_10km,i_10km,idepth)/real(SBDM_count(row_10km,i_10km,idepth))
                            else
                                SBDM_5m(row_10km,i_10km,idepth)=-99  !EJ(6/8)
                            end if
                            SLOC_5m(row_10km,i_10km,idepth)=sum_SLOC(row_10km,i_10km,idepth)/real(SLOC_count(row_10km,i_10km,idepth))
                            
                            if (SCEC_count(row_10km,i_10km,idepth) .gt. 0) then !EJ(6/8)
                                SCEC_5m(row_10km,i_10km,idepth)=sum_SCEC(row_10km,i_10km,idepth)/real(SCEC_count(row_10km,i_10km,idepth))
                            else
                                SCEC_5m(row_10km,i_10km,idepth)=-99  !EJ(6/8)
                            end if
                            SLHW_5m(row_10km,i_10km,idepth)=sum_SLHW(row_10km,i_10km,idepth)/real(SLHW_count(row_10km,i_10km,idepth))
                            !Derived parameters
                            SDUL_5m(row_10km,i_10km,idepth)=sum_SDUL(row_10km,i_10km,idepth)/real(SDUL_count(row_10km,i_10km,idepth))
                            SLLL_5m(row_10km,i_10km,idepth)=sum_SLLL(row_10km,i_10km,idepth)/real(SLLL_count(row_10km,i_10km,idepth))
                            SSAT_5m(row_10km,i_10km,idepth)=sum_SSAT(row_10km,i_10km,idepth)/real(SSAT_count(row_10km,i_10km,idepth))
                            SSKS_5m(row_10km,i_10km,idepth)=sum_SSKS(row_10km,i_10km,idepth)/real(SSKS_count(row_10km,i_10km,idepth))
                        else
                            !If organic carbon (matter) is not available, pedotransfer function to derive SDUL, SLLL cannot be applied. Therefore all derived variables will not be created => DSSAT complains if SDUL=-99.
                            !original data from AfSIS
                            SLCL_5m(row_10km,i_10km,idepth)=-99
                            SLSI_5m(row_10km,i_10km,idepth)=-99
                            SBDM_5m(row_10km,i_10km,idepth)=-99
                            SLOC_5m(row_10km,i_10km,idepth)=-99
                            SCEC_5m(row_10km,i_10km,idepth)=-99
                            SLHW_5m(row_10km,i_10km,idepth)=-99
                            !Derived parameters
                            SDUL_5m(row_10km,i_10km,idepth)=-99
                            SLLL_5m(row_10km,i_10km,idepth)=-99
                            SSAT_5m(row_10km,i_10km,idepth)=-99
                            SSKS_5m(row_10km,i_10km,idepth)=-99
                            Data_flag_5m(row_10km,i_10km)= 0    !EJ(5/21/2015)
                        end if

                        !debug
                        if((SLCL_5m(row_10km,i_10km,idepth)+SLSI_5m(row_10km,i_10km,idepth)) .GT. 1) then
                            print *, 'negative sand',SLCL_5m(row_10km,i_10km,idepth),SLSI_5m(row_10km,i_10km,idepth),row_10km,i_10km,idepth
                            pause;
                        end if
                    end do
                end if

                !!REMOVE CROPLAND MASK (EJ: 9/28/2015)    !aggregate 1km resolution cropland to 10km
                !if(sum_cland(row_10km,i_10km) > 0) then  !assuming if the probability is > 0 (even small probability), I assume this is "crop land"
                !    cland_5m(row_10km,i_10km)=1
                !else
                !    cland_5m(row_10km,i_10km)=0
                !end if
            end do !end of do i_10km=1,120  !120=col_10km-1

            row_10km=row_10km+1

        end if  !if(mod(irow,10) .eq. 0) then

        col_10km=1   !start from 1 if a new row (1km) starts
    end do  !do irow=1,num_row

    !=====WRITE VARIALBES (COMPUTED FROM 1KM PIXELS) for 10KM PIXELS
    do irow=1,num_row_5m
        !Read country mask
        read (203, *) (cntry_id(jcol,1),jcol = 1,num_col_5m) !country mask with unique integer for each country
        read (200, *) (cell5m_id(jcol,1),jcol = 1,num_col_5m)

        do jcol=1,num_col_5m
            if (Data_flag_5m(irow,jcol) > 0) then ! .AND. cland_5m(irow,jcol) > 0 ) then  !REMOVE CROPLAND MASK (EJ: 9/28/2015)
                !EJ(4/28)========Find representative HC27 soil type using soil texture and SOC
                !Debug EJ(5/21/2015)
                ! print *,'before Find_HC1', SLCL_5m(irow,jcol,1),SLSI_5m(irow,jcol,1), SLOC_5m(irow,jcol,1)
                ! print *,'before Find_HC2', SDUL_5m(irow,jcol,1), SLLL_5m(irow,jcol,1),Data_flag_5m(irow,jcol)

                call Find_HC_profile(irow,jcol)

                !==========Extract soil info for the found major soil type from HC.SOL
                temp_ID='*'//trim(rep_HCID)
                call Jump_label(202,trim(temp_ID),80)
                !Need soil depth(SLDP)
                READ (202,5030,IOSTAT=istat) PEDON,LSOUR,LTXS,LDP,LDESC
                read(202,'(a)',iostat=istat)line1 !@SITE        COUNTRY
                read(202,'(a)',iostat=istat)line2 !-99         Generic
                read(202,'(a)',iostat=istat)line3 !@ SCOM  SALB  SLU1
                ! call EXtract_HC27(rep_HCID)

                read(202,500,IOSTAT=istat) COM,ALB, LU1, LDR, LRO, LNF, LPF,MHB, MPX, MKE
                read(202,'(a)',iostat=istat)line4 !@  SLB  SLMH  SLLL  SDUL

                !READ soil layer info from HC.SOL
                i=1
                soil_layer: do while (i < 20)
                    READ (202,502,IOSTAT=istat) LYR(i),LMH(i),LLL(i),DUL(i),SAT(i),RGF(i),SKS(i),BDM(i), &
                        & OC(i),LCL(i),LSI(i),LCF(i),LNI(i),LHW(i),LHB(i),CEC(i),ADC(i)
                    if(LYR(i) .eq. LDP) exit soil_layer
                    i=i+1
                end do soil_layer
                read(202,'(a)',iostat=istat)line5 !@  SLB  SLPX  SLPT  SLPO
                n_ZLYR=i !number of soil layers

5030            FORMAT (A11, 2X, A11,1X,A6,1X,f5.0,A20)
501             FORMAT (I6,A6,15F6.2)
502             FORMAT (1X,F5.0,1X,A5, 3F6.3, 6F6.2, F6.1,2F6.2,3F6.1) !*.SOL


                !===============Write into AF.SOL
                ! call write_SOL

                !Retrieve country acronym to make soil ID (country acronym(2 except Ethiopia) + ########(8 digit)
                !EJ(6/4/2015)
                call find_country_acronym (cntry_id(jcol,1),ISO_A2,ISO_A3)  !integer_n3(IN), out_str2(OUT)
                C_ID=ISO_A2

                !do ii=1,n_country
                !    if(cntry_id(jcol,1) .eq. value_list(ii)) then
                !        C_ID=cstr_list(ii)
                !    end if
                !end do

                !   call Make_soil_ID(C_ID,soil_ID)!retrieve sequential number for each country (e.g.,soil_ID=AFKY000001)

                !if (soil_ID .eq. 'AFE0001457') pause

                !Determine rooting depth (SRGF) based on corresponding HC27 soil type
                Call Define_Rootgrowth(LDP)

                !Estimate SLNI(Total nitrogen, % ) based on corresponding HC27 soil type
                Call Estimate_SLNI(LDP)

                ! SLSOUR='AfSIS'
                SLSOUR=ISO_A3  !EJ(6/4/2015)
                !Find soil tpye from soil texture classification triangle
                ! s_class = what_texture(SAND(1)*100, SLCL(1)*100)
                s_class = what_texture((1-SLSI_5m(irow,jcol,1)-SLCL_5m(irow,jcol,1))*100, SLCL_5m(irow,jcol,1)*100)
                SLTXS=trim(s_class)

                SLDP=200.0   !max depth of AfSIS soil
                !SLDESC='AF DATABASE: AfSIS+HC27'
                !convert int to sting
                Write(temp_str, '(i8.8)' ) cell5m_id(jcol,1)
                !check
                if(cell5m_id(jcol,1) < 0) then
                    write(1000,*)lat,lon,cell5m_id(jcol,1)
                    !  write(6,*)'**ERROR: negative values on cell5m_ID!'
                    !  pause;
                end if
                !SLDESC='CELL 5M ID:'//trim(temp_str)
                !SLDESC=LDESC !EJ(4/30) to inform which HC27 soil info was used to create this target AfSIS pixel
                SLDESC='ISRIC soilgrids + HC27'  !EJ(6/4/2015)

                soil_ID=C_ID//temp_str  !EJ(5/21/2015) e.g., Soil profile ID at CELL5M=4426266 in Kenya: KE04426266
                temp_ID='*'//trim(soil_ID)
                write(300,5040)trim(temp_ID),trim(SLSOUR),trim(SLTXS),int(SLDP),trim(SLDESC)
5040            FORMAT (A11, 2X, A5,3X,A9,1X,I5,1X,A25)
                write(300,'(a)')line1 !@SITE        COUNTRY
                !  write(300,5050) '-99',trim(C_ID),lat,lon,trim(SLTXS)
                write(300,5050) '-99',trim(C_ID),lat,lon,trim(rep_HCID)
5050            FORMAT (A4, 2X, A14,4X,2(F9.3),A15)
                write(300,'(a)')line3 !@ SCOM  SALB  SLU1
                write(300,500) COM,ALB, LU1, LDR, LRO, LNF, LPF,MHB, MPX, MKE  !all these are from corresponding HC27 soil type
500             FORMAT (A6,6F6.2,3A6)
                write(300,'(a)')line4 !@  SLB  SLMH  SLLL  SDUL
                i=1
                do while (i <= 6)  !AfSIS has 6 standardized layers
                    if(SLOC_5m(irow,jcol,i) .GE. 0) then
                        SLOC_5m(irow,jcol,i)=SLOC_5m(irow,jcol,i)*100  !EJ(Nov 12, 2014) Change vol % (cm3/cm3) to % for *.SOL
                        ! SLNI(irow,jcol,i)=SLOC(irow,jcol,i)*0.1   !NEED TO CHECK****************************
                    else
                        SLOC_5m(irow,jcol,i)=-99
                        ! SLNI(irow,jcol,i)=-99
                    end if

                    if(SLCL_5m(irow,jcol,i) .gt. 0 .AND. SLSI_5m(irow,jcol,i) .gt. 0) then
                        SLCL_5m(irow,jcol,i)=SLCL_5m(irow,jcol,i)*100  !EJ(Nov 12, 2014) Change vol % (cm3/cm3) to % for *.SOL
                        SLSI_5m(irow,jcol,i)=SLSI_5m(irow,jcol,i)*100  !EJ(Nov 12, 2014) Change vol % (cm3/cm3) to % for *.SOL
                    end if
                    !   SSKS_5m(irow,jcol,i)=-99.0  !EJ(Nov 12, 2014) to prevent too low KS
                    if(SLHW_5m(irow,jcol,i) .GT. 0 .AND. SLOC_5m(irow,jcol,i) .GE. 0 ) then
                        WRITE (300,5112) int(ZLYR(i)),SLMH(i),SLLL_5m(irow,jcol,i),SDUL_5m(irow,jcol,i),SSAT_5m(irow,jcol,i),SRGF(i),SSKS_5m(irow,jcol,i),SBDM_5m(irow,jcol,i), &
                            & SLOC_5m(irow,jcol,i),SLCL_5m(irow,jcol,i),SLSI_5m(irow,jcol,i),SLCF(i),SLNI(i),SLHW_5m(irow,jcol,i),SLHB(i),SCEC_5m(irow,jcol,i),SADC(i)
                    else if(SLOC_5m(irow,jcol,i) .LT. 0) then !if OC is not avaialbe, other derived parameters are also -99
                        WRITE (300,5114) int(ZLYR(i)),SLMH(i),SLLL_5m(irow,jcol,i),SDUL_5m(irow,jcol,i),SSAT_5m(irow,jcol,i),SRGF(i),SSKS_5m(irow,jcol,i),SBDM_5m(irow,jcol,i), &
                            & SLOC_5m(irow,jcol,i),SLCL_5m(irow,jcol,i),SLSI_5m(irow,jcol,i),SLCF(i),SLNI(i),SLHW_5m(irow,jcol,i),SLHB(i),SCEC_5m(irow,jcol,i),SADC(i)
                        !debug
                        !print *,SLCL_5m(irow,jcol,i),SLSI_5m(irow,jcol,i)
                    else
                        WRITE (300,5113) int(ZLYR(i)),SLMH(i),SLLL_5m(irow,jcol,i),SDUL_5m(irow,jcol,i),SSAT_5m(irow,jcol,i),SRGF(i),SSKS_5m(irow,jcol,i),SBDM_5m(irow,jcol,i), &
                            & SLOC_5m(irow,jcol,i),SLCL_5m(irow,jcol,i),SLSI_5m(irow,jcol,i),SLCF(i),SLNI(i),SLHW_5m(irow,jcol,i),SLHB(i),SCEC_5m(irow,jcol,i),SADC(i)
                    end if
                    i=i+1
                end do

                write(300,'(a)')' ' !add one empty line betwen soil types
5112            FORMAT (1X,I5,1X,A5,3(1X,F5.3),1X,F5.2,1X,F5.2,4(1X,F5.2),1X,F5.1,2(1X,F5.2),3(1X,F5.1)) !EJ(11/13/2014) make SSKS=-99
5113            FORMAT (1X,I5,1X,A5,3(1X,F5.3),1X,F5.2,1X,F5.2,4(1X,F5.2),1X,F5.1,1X,F5.2,4(1X,F5.1)) !EJ(11/13/2014) make SSKS=-99
                !5114            FORMAT (1X,I5,1X,A5,3(1X,F5.1),1X,F5.2,1X,F5.1,1X,F5.2,1X,F5.1,2(1X,F5.2),6(1X,F5.1)) !when -99 exists
5114            FORMAT (1X,I5,1X,A5,3(1X,F5.1),1X,F5.1,3(1X,F5.1),1X,F5.1,1X,F5.1,2(1X,F5.1),4(1X,F5.1)) !when -99 exists

            end if  !end of if (Data_flag_5m(irow,jcol) > 0 ) then

            lon=lon+cellsize
        end do   !end of jcol=1,num_col_5m


        lat=lat-cellsize
        lon=lon_first
        write (6,'(A, F9.3)') 'LAT= ', lat
    end do !end of  irow=1,num_row_5m



    close(101);    close(102);    close(103);    close(104) ;    close(105);    close(106)
    close(111);    close(112);    close(113) ;   close(114);    close(115) ;    close(116)
    close(161);    close(162);    close(163) ;   close(164);    close(165) ;    close(166)
    close(121);    close(122);    close(123) ;   close(124);    close(125) ;    close(126)
    close(131);    close(132);    close(133) ;   close(134);    close(135) ;    close(136)
    close(141);    close(142);    close(143) ;   close(144);    close(145) ;    close(146)
    close(151);    close(152);    close(153) ;   close(154);    close(155) ;    close(156)
    close(200);    close(201);    close(202) ;   close(203);    close(300)
    close(1000)
    write(6,'(/,a)')'Processing completed...'
    end program Create_10km_global
    !End of program-------------------------------------------------------------------------------------
