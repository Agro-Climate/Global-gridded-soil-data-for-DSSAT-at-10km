    !Author: Eunjin Han
    !Institute: IRI-Columbia University, NY
    !modified: 4/28/2015
    !modified 2/16/2015 because AfSIS soil data has minimum 0 for all values => no need to worry "if var < 0) then var=-99
    !Date: 7/25/2014
    !Purpose: Convert soil info from AfSIS (where soil information for each layer is saved seperately)
    !         to one variable which has all 6 layer info together
    !=======================================

    subroutine Convert2SOL
    !
    use ModuleGEN

    integer :: ii

    !Sand
    SAND(1)=snd1(jcol,1)/100
    !   if (snd1(jcol,1) .LE. 0) SAND(1)=-99
    SAND(2)=snd2(jcol,1)/100
    !  if (snd2(jcol,1) .LE. 0) SAND(2)=-99
    SAND(3)=snd3(jcol,1)/100
    !  if (snd3(jcol,1) .LE. 0) SAND(3)=-99
    SAND(4)=snd4(jcol,1)/100
    !  if (snd4(jcol,1) .LE. 0) SAND(4)=-99
    SAND(5)=snd5(jcol,1)/100
    !  if (snd5(jcol,1) .LE. 0) SAND(5)=-99
    SAND(6)=snd6(jcol,1)/100
    !  if (snd6(jcol,1) .LE. 0) SAND(6)=-99

    !Silt
    SLSI(1)=slt1(jcol,1)/100
    !  if (slt1(jcol,1) .LE. 0) SLSI(1)=-99
    SLSI(2)=slt2(jcol,1)/100
    !  if (slt2(jcol,1) .LE. 0) SLSI(2)=-99
    SLSI(3)=slt3(jcol,1)/100
    !  if (slt3(jcol,1) .LE. 0) SLSI(3)=-99
    SLSI(4)=slt4(jcol,1)/100
    !  if (slt4(jcol,1) .LE. 0) SLSI(4)=-99
    SLSI(5)=slt5(jcol,1)/100
    ! if (slt5(jcol,1) .LE. 0) SLSI(5)=-99
    SLSI(6)=slt6(jcol,1)/100
    !   if (slt6(jcol,1) .LE. 0) SLSI(6)=-99

    !clay
    SLCL(1)=cly1(jcol,1)/100
    SLCL(2)=cly2(jcol,1)/100
    SLCL(3)=cly3(jcol,1)/100
    SLCL(4)=cly4(jcol,1)/100
    SLCL(5)=cly5(jcol,1)/100
    SLCL(6)=cly6(jcol,1)/100

    do ii=1,6
    !debug
    if((SLSI(ii)+SLCL(ii)).GT. 1) then
        print *, 'convert2SOL:',SLSI(ii),SLCL(ii),SAND(ii)
        pause;
    end if
    end do

    !EJ(4/28) if both sand and silt are not available, turn on NO_DATA flag. Criteria is only the first layer
    if (SAND(1) .lt. 0 .AND. SLSI(1) .lt. 0) then
        NoData_flag(jcol,1)=0
        !assign -99 for all variables
        do ii=1,6
            SAND(ii)=-99
            SLSI(ii)=-99
            SLCL(ii)=-99
            SBDM(ii)=-99
            SLOC(ii)=-99
            SCEC(ii)=-99
            SLHW(ii)=-99
        end do
    else
        !!Clay
        !SLCL(1)=1-SAND(1)-SLSI(1)
        !if (SLCL(1) .eq. 100) pause;
        !!  if (SAND(1) .LE. 0) SLCL(1)=-99
        !SLCL(2)=1-SAND(2)-SLSI(2)
        !if (SLCL(2) .eq. 100) pause;
        !!  if (SAND(2) .LE. 0) SLCL(2)=-99
        !SLCL(3)=1-SAND(3)-SLSI(3)
        !if (SLCL(3) .eq. 100) pause;
        !!   if (SAND(3) .LE. 0) SLCL(3)=-99
        !SLCL(4)=1-SAND(4)-SLSI(4)
        !if (SLCL(4) .eq. 100) pause;
        !!  if (SAND(4) .LE. 0) SLCL(4)=-99
        !SLCL(5)=1-SAND(5)-SLSI(5)
        !if (SLCL(5) .eq. 100) pause;
        !!   if (SAND(5) .LE. 0) SLCL(5)=-99
        !SLCL(6)=1-SAND(6)-SLSI(6)
        !if (SLCL(6) .eq. 100) pause;
        !!  if (SAND(6) .LE. 0) SLCL(6)=-99

        !Bulk density
        !Soil Property Maps of Africa at 1 km: Bulk Density in kg/m3 , e.g., 1375 kg/m3=> 1.375 g/cm3
        !DSSAT: SBDM     Bulk density, moist, g cm-3
        SBDM(1)=bld1(jcol,1)/1000
        if (bld1(jcol,1) .LE. 0) SBDM(1)=-99
        SBDM(2)=bld2(jcol,1)/1000
        if (bld2(jcol,1) .LE. 0) SBDM(2)=-99
        SBDM(3)=bld3(jcol,1)/1000
        if (bld3(jcol,1) .LE. 0) SBDM(3)=-99
        SBDM(4)=bld4(jcol,1)/1000
        if (bld4(jcol,1) .LE. 0) SBDM(4)=-99
        SBDM(5)=bld5(jcol,1)/1000
        if (bld5(jcol,1) .LE. 0) SBDM(5)=-99
        SBDM(6)=bld6(jcol,1)/1000
        if (bld6(jcol,1) .LE. 0) SBDM(6)=-99

        !DSSAT: SAOC     Organic carbon, %
        !AfSIS: Soil Property Maps of Africa at 1 km: Organic Carbon in permilles (g/kg)
        !Unit change (g/kg to g/g (i.e. decimal ratio=> e.g., 0.6 ))
        SLOC(1)=orc1(jcol,1)/1000
        if (orc1(jcol,1) .LE. 0) SLOC(1)=-99
        SLOC(2)=orc2(jcol,1)/1000
        if (orc2(jcol,1) .LE. 0) SLOC(2)=-99
        SLOC(3)=orc3(jcol,1)/1000
        if (orc3(jcol,1) .LE. 0) SLOC(3)=-99
        SLOC(4)=orc4(jcol,1)/1000
        if (orc4(jcol,1) .LE. 0) SLOC(4)=-99
        SLOC(5)=orc5(jcol,1)/1000
        if (orc5(jcol,1) .LE. 0) SLOC(5)=-99
        SLOC(6)=orc6(jcol,1)/1000
        if (orc6(jcol,1) .LE. 0) SLOC(6)=-99

        !DSSAT: SCEC     Cation exchange capacity, cmol kg-1
        !AfSIS: Cation Exchange Capacity (soil) in cmol/kg
        SCEC(1)=cec1(jcol,1)
        if (cec1(jcol,1) .LE. 0) SCEC(1)=-99
        SCEC(2)=cec2(jcol,1)
        if (cec2(jcol,1) .LE. 0) SCEC(2)=-99
        SCEC(3)=cec3(jcol,1)
        if (cec3(jcol,1) .LE. 0) SCEC(3)=-99
        SCEC(4)=cec4(jcol,1)
        if (cec4(jcol,1) .LE. 0) SCEC(4)=-99
        SCEC(5)=cec5(jcol,1)
        if (cec5(jcol,1) .LE. 0) SCEC(5)=-99
        SCEC(6)=cec6(jcol,1)
        if (cec6(jcol,1) .LE. 0) SCEC(6)=-99

        !DSSAT: SAHW     pH in water
        !AfSIS: PHIHOX	Soil pH x 10 in H2O
        SLHW(1)=phih1(jcol,1)/10 !EJ(4/28)
        if (phih1(jcol,1) .LE. 0) SLHW(1)=-99
        SLHW(2)=phih2(jcol,1)/10 !EJ(4/28)
        if (phih2(jcol,1) .LE. 0) SLHW(2)=-99
        SLHW(3)=phih3(jcol,1)/10 !EJ(4/28)
        if (phih3(jcol,1) .LE. 0) SLHW(3)=-99
        SLHW(4)=phih4(jcol,1)/10 !EJ(4/28)
        if (phih4(jcol,1) .LE. 0) SLHW(4)=-99
        SLHW(5)=phih5(jcol,1)/10 !EJ(4/28)
        if (phih5(jcol,1) .LE. 0) SLHW(5)=-99
        SLHW(6)=phih6(jcol,1)/10 !EJ(4/28)
        if (phih6(jcol,1) .LE. 0) SLHW(6)=-99

    end if


    end subroutine Convert2SOL