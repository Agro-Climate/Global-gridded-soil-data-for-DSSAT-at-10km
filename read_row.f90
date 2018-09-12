    !Author: Eunjin Han
    !Institute: IRI-Columbia University, NY
    !Date: 7/24/2014
    !Read "row" of each AfSIS input data in ASCII format
    !=======================================

    subroutine read_row
    !
    use ModuleGEN

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
    
    read (151, *) (phih1(jcol,1),jcol = 1,num_col)
    read (152, *) (phih2(jcol,1),jcol = 1,num_col)
    read (153, *) (phih3(jcol,1),jcol = 1,num_col)
    read (154, *) (phih4(jcol,1),jcol = 1,num_col)
    read (155, *) (phih5(jcol,1),jcol = 1,num_col)
    read (156, *) (phih6(jcol,1),jcol = 1,num_col)
    
    end subroutine read_row