    !Author: Eunjin Han
    !Institute: IRI-Columbia University, NY
    !Date: 6/4/2015
    !find country code (integer_n3) list within a target 10 deg tile
    !===========================================================
    !E:\IRI\IFPRI\soilgrid1km_SSA\integer_n3_A2.txt
    !533 AW
    !4   AF
    !24  AO
    !660 AI
    !8   AL
    !248 AX
    !20  AD
    !=======================================
    subroutine find_country_acronym (integer_n3,out_str2,out_str3)  !integer_n3(IN), out_str2(OUT)

    use ModuleGEN
    IMPLICIT NONE

    integer :: icount
    character(len=2) :: out_str2, temp2
    character(len=3) :: temp1, temp_str1,temp_str2 !EJ(6/4/2015)
    integer :: integer_n3, temp_int
    character(len=3) :: out_str3 !EJ(6/4/2015)

    !initialize
    out_str2='ND' ! no data

    !Read template of experiment file
    open(unit=30,file=trim(fname_ISOlist),status='old',iostat=istat)
    if(istat /= 0) write (*,*) 'ISO_N3_A2_A3.txt file OPEN failed: istat=', istat

    find_ison3: do while(.not. EOF(30))
        !read(30,*)temp_int, temp_str1, temp_str2
        read(30,100)temp_int, temp_str1, temp_str2 !EJ(6/4/2015)
        if(temp_int .eq. integer_n3) then
            out_str2=temp_str1
            out_str3=temp_str2
            exit find_ison3
        end if
    end do find_ison3



    close(30)

100 FORMAT (I3, X, A2,X, A3)

    return

    end subroutine find_country_acronym