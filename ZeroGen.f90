    !Author: Eunjin Han
    !Institute: IRI-Columbia University, NY
    !Date: 7/26/2014
    !=======================================
    !original Author: Amor (IRI) or Narendra (JPL)
    !originally "fileNamGenTool.f90"
    !=========================================
    subroutine ZeroGen(int,sizeR,zero)

    integer(kind=4) :: int,sizeR
    character(len=5) :: zero

    if(int .lt. 10)then
        sizeR=1;zero='00000'
    elseif(int .le. 99 .and. int .ge. 10)then
        sizeR=2;zero='0000'
    elseif (int .le. 999 .and. int .ge. 100)then
        sizeR=3;zero='000'
    elseif (int .le. 9999 .and. int .ge. 1000)then
        sizeR=4;zero='00'
    elseif (int .le. 99999 .and. int .ge. 10000)then
        sizeR=5;zero='0'
    else
        sizeR=6;zero=''
       ! write(6,'(a)')'Max. size of realizations is on the 1 x 10^5 level'
    endif

    return
    end subroutine ZeroGen