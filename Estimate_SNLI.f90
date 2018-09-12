    !Author: Eunjin Han
    !Institute: IRI-Columbia University, NY
    !Date: 4/30/2015
    !Estimate SLNI(Total nitrogen, % ) based on corresponding HC27 soil type
    !*Note: same structure as the Define_Rootgrowth (same approach for weighted avg)
    !===========================================================

    subroutine Estimate_SLNI(soil_depth)

    use ModuleGEN !EJ: for common variable, use module
    implicit none

    real:: soil_depth  !total soil depth


    if (soil_depth .GE. 180) then  !deep
        SLNI(1)=LNI(1)  !5cm <- from 10cm layer of HC soil
        SLNI(2)=0.5*LNI(1)+0.5*LNI(2)  !15cm <- from 0.5*10cm + 0.5*30cm layer of HC soil
        SLNI(3)=LNI(2) !30cm <- from 30cm layer of HC soil
        SLNI(4)=LNI(3) !60cm  <- from 60cm layer of HC soil
        SLNI(5)=0.75*LNI(4)+0.25*LNI(5) !100cm  <- from 3/4*90cm +1/4*120 cm layer of HC soil
        SLNI(6)=0.2*LNI(5)+0.3*LNI(6) +0.5*LNI(7) !200cm <- from 0.2*120cm +0.3*150 cm +0.5*180cm layer
    elseif (soil_depth .GE. 120 .AND. soil_depth .LT. 180 ) then  !medium
        SLNI(1)=LNI(1)  !5cm <- from 10cm layer of HC soil
        SLNI(2)=0.5*LNI(1)+0.5*LNI(2)  !15cm <- from 0.5*10cm + 0.5*30cm layer of HC soil
        SLNI(3)=LNI(2) !30cm <- from 30cm layer of HC soil
        SLNI(4)=LNI(3) !60cm  <- from 60cm layer of HC soil
        SLNI(5)=0.75*LNI(4)+0.25*LNI(5) !100cm  <- from 3/4*90cm +1/4*120 cm layer of HC soil
        SLNI(6)=0.05
    elseif (soil_depth .GE. 60 .AND. soil_depth .LT. 120 ) then  !shallow
        SLNI(1)=LNI(1)  !5cm <- from 10cm layer of HC soil
        SLNI(2)=0.5*LNI(1)+0.5*LNI(2)  !15cm <- from 0.5*10cm + 0.5*30cm layer of HC soil
        SLNI(3)=LNI(2) !30cm <- from 30cm layer of HC soil
        SLNI(4)=LNI(3) !60cm  <- from 60cm layer of HC soil
        SLNI(5)=0.0
        SLNI(6)=0.0
    else
        SLNI=-99
    endif

    return

    end subroutine Estimate_SLNI