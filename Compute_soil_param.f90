    !Author: Eunjin Han
    !Institute: IRI-Columbia University, NY
    !modified: 4/28/2015
    !============
    !modified on 2/16/2015 to solve the problem with SOM=-99
    ! AfSIS soil data has minimum 0 for all values => no need to worry "if var < 0) then var=-99
    !================================
    !Date: 7/26/2014
    ! Compute soil characteristics using AfSIS data(% sand and clay, bulk density, PH, CEC, Organic Carbon)
    ! Equations for this estimation is from Saxton & Rawls (2006)
    !=======================================

    subroutine Compute_soil_param
    !
    use ModuleGEN
    IMPLICIT NONE

    real, dimension(6):: SOM !soil organic matter = 2*SOC
    integer :: ii
    real :: Theta33t, Theta15t, Thetas_33t, Thetas_33,B,lamda

    !initial condition
    character(len=11)::temp_ID
    character(len=2)::PCR
    integer ::IC_day, IC_year,S_NO,ICDAT,n_lyr
    character(len=3)::ICNAME
    real, dimension(20):: lyr_depth,fc !field capacity from soil.SOL
    real :: SH2O,SNH4,SNO3
    integer :: ICBL,ICRT,ICND,ICRN,ICRE,ICWD,ICRES,ICREN,ICREP,ICRIP,ICRID

    !do ii=1,6 !maximum 6 layers
    !    if(SAND(ii) .GT. 1) then
    !        print *, 'compute soil_param',SAND(ii),SLCL(ii),SOM(ii),Thetas_33t,Thetas_33, SSAT(ii), ii, jcol
    !        pause;
    !    end if
    !end do

    if (NoData_flag(jcol,1) > 0) then !EJ(4/28)
        do ii=1,6 !maximum 6 layers
            !! 1)Compute SOM from SOC
            !According to Priblyl (2010), use conversion factor of 2 instead of conventional 1.724
            !SOC or SOM can be zero! (EJ: 2/16/2015) => The effecto of SOM is minor => see C:\IRI\SERVIR\soil_data\Saxton_paper_test_1113.xls)
            if(SLOC(ii) >= 0) then
                SOM(ii)=2*SLOC(ii)
            else
                SOM(ii)=-99
            end if

            !! 2)Compute field capacity
            if(SOM(ii) >= 0) then
                Theta33t=-0.251*SAND(ii)+0.195*SLCL(ii)+0.011*SOM(ii)+0.006*SAND(ii)*SOM(ii)-0.027*SLCL(ii)*SOM(ii)+0.452*SAND(ii)*SLCL(ii)+0.299
                SDUL(ii)=Theta33t+1.283*Theta33t*Theta33t-0.374*Theta33t-0.015
            else
                SDUL(ii)=-99
            end if

            !! 3)Compute wilting point
            if(SOM(ii) >= 0) then
                Theta15t=-0.024*SAND(ii)+0.487*SLCL(ii)+0.006*SOM(ii)+0.005*SAND(ii)*SOM(ii)-0.013*SLCL(ii)*SOM(ii)+0.068*SAND(ii)*SLCL(ii)+0.031
                SLLL(ii)=Theta15t+0.14*Theta15t-0.02
            else
                SLLL(ii)=-99
            end if

            !! 4)Compute saturated sm
            if(SOM(ii) >= 0) then
                Thetas_33t=0.278*SAND(ii)+0.034*SLCL(ii)+0.022*SOM(ii)-0.018*SAND(ii)*SOM(ii)-0.027*SLCL(ii)*SOM(ii)-0.584*SAND(ii)*SLCL(ii)+0.078
                Thetas_33=Thetas_33t+0.636*Thetas_33t-0.107

                SSAT(ii)=SDUL(ii)+Thetas_33-0.097*SAND(ii)+0.043
                if(SSAT(ii) .LT. 0) then
                    print *, SAND(ii),SLCL(ii),SOM(ii),Thetas_33t,Thetas_33, SSAT(ii)
                    pause;
                end if
            else
                SSAT(ii)=-99
            end if

            !! 5)Compute Ksat
            if(SOM(ii) >= 0) then
                B=(log(1500.0)-log(33.0))/(log(SDUL(ii))-log(SLLL(ii)))
                lamda=1/B
                SSKS(ii)=1930*(SSAT(ii)-SDUL(ii))**(3-lamda) ! mm/hr
                SSKS(ii)=SSKS(ii)*0.1 ! cm/hr for DSSAT
            else
                SSKS(ii)=-99
            end if

            !debug
            if(SDUL(ii) .LT. 0) then
                !print *, 'check negative SDUL',SDUL(ii),SOM(ii),SAND(ii),SLCL(ii), ii, jcol
                !  pause;
            end if

        end do

    else
        !assign -99 for all variables <= due to unavailability of sand/silt, not possible to derive these variables
        do ii=1,6
            SDUL(ii)=-99
            SLLL(ii)=-99
            SSAT(ii)=-99
            SSKS(ii)=-99
        end do

    end if

    end subroutine Compute_soil_param