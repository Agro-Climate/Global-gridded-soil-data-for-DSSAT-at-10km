    !Author: Eunjin Han
    !Institute: IRI-Columbia University, NY
    !Date: 4/30/2015
    ! Determine the root growth distribution function, SRGF
    ! based on data from HC.SOL
    !===========================================================

    subroutine Define_Rootgrowth(soil_depth)

    use ModuleGEN !EJ: for common variable, use module
    implicit none

    real:: soil_depth  !total soil depth


    if (soil_depth .GE. 180) then  !deep
        SRGF(1)=RGF(1)  !5cm <- from 10cm layer of HC soil
        SRGF(2)=0.5*RGF(1)+0.5*RGF(2)  !15cm <- from 0.5*10cm + 0.5*30cm layer of HC soil
        SRGF(3)=RGF(2) !30cm <- from 30cm layer of HC soil
        SRGF(4)=RGF(3) !60cm  <- from 60cm layer of HC soil
        SRGF(5)=0.75*RGF(4)+0.25*RGF(5) !100cm  <- from 3/4*90cm +1/4*120 cm layer of HC soil
        SRGF(6)=0.2*RGF(5)+0.3*RGF(6) +0.5*RGF(7) !200cm <- from 0.2*120cm +0.3*150 cm +0.5*180cm layer
    elseif (soil_depth .GE. 120 .AND. soil_depth .LT. 180 ) then  !medium
        SRGF(1)=RGF(1)  !5cm <- from 10cm layer of HC soil
        SRGF(2)=0.5*RGF(1)+0.5*RGF(2)  !15cm <- from 0.5*10cm + 0.5*30cm layer of HC soil
        SRGF(3)=RGF(2) !30cm <- from 30cm layer of HC soil
        SRGF(4)=RGF(3) !60cm  <- from 60cm layer of HC soil
        SRGF(5)=0.75*RGF(4)+0.25*RGF(5) !100cm  <- from 3/4*90cm +1/4*120 cm layer of HC soil
        SRGF(6)=0.05
    elseif (soil_depth .GE. 60 .AND. soil_depth .LT. 120 ) then  !shallow
        SRGF(1)=RGF(1)  !5cm <- from 10cm layer of HC soil
        SRGF(2)=0.5*RGF(1)+0.5*RGF(2)  !15cm <- from 0.5*10cm + 0.5*30cm layer of HC soil
        SRGF(3)=RGF(2) !30cm <- from 30cm layer of HC soil
        SRGF(4)=RGF(3) !60cm  <- from 60cm layer of HC soil
        SRGF(5)=0.0
        SRGF(6)=0.0
    else
        SRGF=-99
    endif

    return

    end subroutine Define_Rootgrowth