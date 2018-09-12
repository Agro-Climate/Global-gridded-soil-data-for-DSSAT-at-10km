    !Author: Eunjin Han
    !Institute: IRI-Columbia University, NY
    !modified: 4/29/2015
    !Date: 7/27/2014
    !find unique soil ID starting with country acronym (e.g.,AFKY000001)
    !===========================================================

    subroutine Make_soil_ID(cntry_str,s_ID)

    use ModuleGEN
    implicit none

    character(len=2):: cntry_str
    character(len=10):: s_ID,SZ
    integer :: sizeR, ii
    character(len=5) :: zero

    do ii=1,n_country
        if (cntry_str .eq. cstr_list(ii)) then
            country_count(ii)=country_count(ii)+1
            call ZeroGen(country_count(ii),sizeR,zero)  !out: zero, sizeR
            call INT2CHAR(country_count(ii),sizeR,SZ) !courtesy of NNDas, NASA-JPL.
            s_ID='AF'//trim(cntry_str)//trim(zero)//trim(SZ)
        end if
    end do

    return

    end subroutine Make_soil_ID