    ! Eunjin Han modified original code from "http://nowlin.css.msu.edu/software/triangle_form.htm"
    ! Now output is acrony for each soil texture(e.g, S, SC etc.)
    !* +-----------------------------------------------------------------------
    !* | WHAT TEXTURE?
    !* | Function to classify a soil in the triangle based on sand and clay %
    !* +-----------------------------------------------------------------------
    !* | Created by: aris gerakis, apr. 98
    !* | Modified by: aris gerakis, june 99.  Now check all polygons instead of
    !* | stopping when a right solution is found.  This to cover all borderline
    !* | cases.
    !* +-----------------------------------------------------------------------
    !modified: 4/28/2015
    !============
    
    character*1 function What_tex_4HC(sand, clay)

    implicit none

    ! Declare arguments:

    real, intent(in) :: clay, sand

    ! Declare local variables:

    logical   :: inpoly
    real      :: silty_loam(7,2), sandy(7,2), silty_clay_loam(7,2), loam(7,2), &
        clay_loam(7,2), sandy_loam(7,2), silty_clay(7,2), &
        sandy_clay_loam(7,2), loamy_sand(7,2), clayey(7,2), silt(7,2), &
        sandy_clay(7,2)
    character :: texture*42
    character :: soil_texture*1 !EJ(4/28/2015)

    !Initalize polygon coordinates:
    ! First write all x's, pad with 0s if less than 7 vertices, then
    ! write all y's, pad with 0s if less than 7 vertices:

    data silty_loam/0, 0, 23, 50, 20, 8, 0, 12, 27, 27, 0, 0, 12, 0/
    data sandy/85, 90, 100, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0/
    data silty_clay_loam/0, 0, 20, 20, 0, 0, 0, 27, 40, 40, 27, 0, 0, 0/
    data loam/43, 23, 45, 52, 52, 0, 0, 7, 27, 27, 20, 7, 0, 0/
    data clay_loam/20, 20, 45, 45, 0, 0, 0, 27, 40, 40, 27, 0, 0, 0/
    data sandy_loam/50, 43, 52, 52, 80, 85, 70, 0, 7, 7, 20, 20, 15, 0/
    data silty_clay/0, 0, 20, 0, 0, 0, 0, 40, 60, 40, 0, 0, 0, 0/
    data sandy_clay_loam/52, 45, 45, 65, 80, 0, 0, 20, 27, 35, 35, 20, 0, 0/
    data loamy_sand/70, 85, 90, 85, 0, 0, 0, 0, 15, 10, 0, 0, 0, 0/
    data clayey/20, 0, 0, 45, 45, 0, 0, 40, 60, 100, 55, 40, 0, 0/
    data silt/0, 0, 8, 20, 0, 0, 0, 0, 12, 12, 0, 0, 0, 0/
    data sandy_clay/45, 45, 65, 0, 0, 0, 0, 35, 55, 35, 0, 0, 0, 0/

    ! Find polygon(s) where the point is.

    texture = ' '

    if (sand .ge. 0.0 .and. clay .ge. 0.0) then
        if (inpoly(silty_loam, 6, sand, clay)) then
            texture = trim(texture)//'/silt loam'
            soil_texture='L'  !loam
        endif
        if (inpoly(sandy, 3, sand, clay)) then
            texture = trim(texture)//'/sand'
            soil_texture='S' !sand
        endif
        if (inpoly(silty_clay_loam, 4, sand, clay)) then
            texture = trim(texture)//'/silty clay loam'
            soil_texture='L'  !loam
        endif
        if (inpoly(loam, 5, sand, clay)) then
            texture = trim(texture)//'/loam'
            soil_texture='L'  !loam
        endif
        if (inpoly(clay_loam, 4, sand, clay)) then
            texture = trim(texture)//'/clay loam'
            soil_texture='L'  !loam
        endif
        if (inpoly(sandy_loam, 7, sand, clay)) then
            texture = trim(texture)//'/sandy loam'
            soil_texture='L'  !loam
        endif
        if (inpoly(silty_clay, 3, sand, clay)) then
            texture = trim(texture)//'/silty clay'
            soil_texture='C'  !clay
        endif
        if (inpoly(sandy_clay_loam, 5, sand, clay)) then
            texture = trim(texture)//'/sandy clay loam'
            soil_texture='L'  !loam
        endif
        if (inpoly(loamy_sand, 4, sand, clay)) then
            texture = trim(texture)//'/loamy sand'
            soil_texture='S' !sand
        endif
        if (inpoly(clayey, 5, sand, clay)) then
            texture = trim(texture)//'/clay'
            soil_texture='C'  !clay
        endif
        if (inpoly(silt, 4, sand, clay)) then
            texture = trim(texture)//'/silt'
            soil_texture='L'  !loam
        endif
        if (inpoly(sandy_clay, 3, sand, clay)) then
            texture = trim(texture)//'/sandy clay'
            soil_texture='C'  !clay
        endif
    endif

    if (texture == ' ') then
        texture = '/unknown texture'
        write (unit = 6, fmt = 1000) sand, clay
1000    format (/, 1x, 'What_tex_4HC:Texture not found for ', f5.1, ' sand and ', f5.1, ' clay')
    endif

    !what_texture = texture
    What_tex_4HC = soil_texture
    return
    end function What_tex_4HC
