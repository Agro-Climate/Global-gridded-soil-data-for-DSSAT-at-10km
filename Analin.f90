      subroutine analin(line,blank,comment,length)
!----------------------------------------------------------------------
      implicit none
	  integer						:: length
      character(len=length)			:: line
      logical blank,comment
!----------------------------------------------------------------------
!--- local
      integer i
!----------------------------------------------------------------------
      blank = .true.                     
      i = 1
30  if(line(i:i) .ne. ' ')then
        blank = .false.
      elseif(i .lt. length)then
        i = i+1
        goto 30
      endif
!----- check if it is comment if not blank
      comment = .false.
      i = 1
40    if(line(i:i).ne.' ')then
!        if(line(i:i).eq.'*'.or.line(i:i).eq.'!') comment = .true.
        if(line(i:i).eq.'!') comment = .true.
      elseif(i.lt.length)then
        i = i+1
        goto 40
      endif
!-----returning the control
      return 
      end subroutine analin
