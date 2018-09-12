    subroutine Jump_label(inf,name,length)
    ! original code by Amor Ines
    ! This subroutine jumps to any label name specified in the CALL statement
    ! ----------------------------------------------------------------------
    implicit none
    save

    integer				:: inf,nlb,j,k,nl,nn,iost,length
    character*(*)			:: name
    character(len=length)	:: line
    logical				:: blank,comment


    !---business starts here (Ines,2001)
    rewind(inf)

10  read(inf,'(A)',iostat=iost)line
    if(iost.lt.0)then
        stop 'End of file reached, the word not matched, no meaning.' ;pause
    endif
    call analin(line,blank,comment,length)
    if(blank.or.comment)goto 10
    !--- count number ofleading blanks
    nlb=1
35  if(line(nlb:nlb).eq.' ')then
        nlb=nlb+1
        goto 35
    endif

    j=1
    do k = nlb,nlb+len(name)-1
        !--- verify case insensitive label
        nl=ichar(line(k:k))
        if(nl.gt.90) nl=nl-32
        nn=ichar(name(j:j))
        if(nn.gt.90) nn=nn-32
        if(nl.ne.nn)then
            !--- Sorry, no match, next line
            goto 10
        endif
        j = j+1
    enddo

    !--- Whew!, at last found
    backspace(inf) !goes back at the beginning of file, so OK to read current line (Ines, 2012), Chubu Univ.

    !--- returning
    return
    end subroutine Jump_label
