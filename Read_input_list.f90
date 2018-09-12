
    !EJ modified ReeadGUI.f90 from PH CAMDT
    !!Date: 5/1/2015
    !=================
    !EJ modified Amor's ReadInput.f90
    !Author: Eunjin Han, Amor V.M. Ines
    !Institute: IRI-Columbia University, NY
    !subroutine ReadInput(fparam,stationName,bnYear,bnDayOfYear,enYear,enDayOfYear,fbnYear,fbnDayOfYear,fenYear,fenDayOfYear,fbnMonth, fenMonth,nRealz,expName)
    subroutine Read_input_list(fparam)

    use ModuleGEN

    integer:: iost
    character(len=32):: temp
    character(len=200):: temp_line,temp_dir
    character(len=90):: fparam

    open(unit=9,file=fparam,iostat=iost)
    if(iost .gt. 0)then
        write(6,'(a,1x,a,/)') 'ERROR, parser cannot fname_out.TXT from Python'
        pause;
        ! stop
    endif

    rewind(9)
    read(9,'(a)',err=100) file_orcdrc1
    read(9,'(a)',err=100) file_orcdrc2
    read(9,'(a)',err=100) file_orcdrc3
    read(9,'(a)',err=100) file_orcdrc4
    read(9,'(a)',err=100) file_orcdrc5
    read(9,'(a)',err=100) file_orcdrc6

    read(9,'(a)',err=100) file_phih1
    read(9,'(a)',err=100) file_phih2
    read(9,'(a)',err=100) file_phih3
    read(9,'(a)',err=100) file_phih4
    read(9,'(a)',err=100) file_phih5
    read(9,'(a)',err=100) file_phih6

    read(9,'(a)',err=100) file_snd1
    read(9,'(a)',err=100) file_snd2
    read(9,'(a)',err=100) file_snd3
    read(9,'(a)',err=100) file_snd4
    read(9,'(a)',err=100) file_snd5
    read(9,'(a)',err=100) file_snd6

    read(9,'(a)',err=100) file_slt1
    read(9,'(a)',err=100) file_slt2
    read(9,'(a)',err=100) file_slt3
    read(9,'(a)',err=100) file_slt4
    read(9,'(a)',err=100) file_slt5
    read(9,'(a)',err=100) file_slt6

    read(9,'(a)',err=100) file_cly1
    read(9,'(a)',err=100) file_cly2
    read(9,'(a)',err=100) file_cly3
    read(9,'(a)',err=100) file_cly4
    read(9,'(a)',err=100) file_cly5
    read(9,'(a)',err=100) file_cly6

    read(9,'(a)',err=100) file_cec1
    read(9,'(a)',err=100) file_cec2
    read(9,'(a)',err=100) file_cec3
    read(9,'(a)',err=100) file_cec4
    read(9,'(a)',err=100) file_cec5
    read(9,'(a)',err=100) file_cec6

    read(9,'(a)',err=100) file_bld1
    read(9,'(a)',err=100) file_bld2
    read(9,'(a)',err=100) file_bld3
    read(9,'(a)',err=100) file_bld4
    read(9,'(a)',err=100) file_bld5
    read(9,'(a)',err=100) file_bld6

    read(9,*,err=100)xllcorner
    read(9,*,err=100)yllcorner

    read(9,'(a)',err=100) file_cellid
    read(9,'(a)',err=100) file_HCSOL
    read(9,'(a)',err=100) file_country
    read(9,'(a)',err=100) file_AFSOL
    read(9,'(a)',err=100) file_cropland
  !  read(9,'(a)',err=100) fname_att
    read(9,'(a)',err=100) file_cell5mid
    read(9,'(a)',err=100) fname_ISOlist

    return
100 Stop 'Error reading GUI_OUT.TXT file, please check...'
    end subroutine Read_input_list

