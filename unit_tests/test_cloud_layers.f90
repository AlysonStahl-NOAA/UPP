! This is a test program for UPP.
!
! This program tests routines from the CloudLayers module.
!
! Alyson Stahl, 9/2026
program test_cloud_layers
    use CloudLayers, only : calc_CloudLayers, clouds_t
    implicit none

    real, parameter :: tol = 1.0e-6
    integer :: res

    res = 0

    ! Test Case 1: Valid & representative multi-layer cloud profile with warm nose. 
    call test_multilayer_and_warmnose(res)
    if (res .ne. 0) stop 10

    print *, "Success!"
contains

    subroutine test_multilayer_and_warmnose(res)
        integer, intent(inout) :: res
        integer, parameter :: nz = 12
        integer :: i
        integer :: topoK
        real :: xlat, xlon
        real :: rh(nz), t(nz), pres(nz), ept(nz), vv(nz)
        !
        integer :: region, exp_region
        type(clouds_t) :: clouds, exp_clouds

        exp_region = 2
        
        exp_clouds%nLayers = 2
        exp_clouds%wmnIdx = 8
        exp_clouds%avv = -0.75

        exp_clouds%topIdx = 0
        exp_clouds%topIdx(1) =1
        exp_clouds%topIdx(2) = 8

        exp_clouds%baseIdx = 0
        exp_clouds%baseIdx(1) = 3
        exp_clouds%baseIdx(2) = 12

        exp_clouds%ctt = 0.0
        exp_clouds%ctt(1) = 235.0
        exp_clouds%ctt(2) = 274.0
 
        allocate(exp_clouds%layerQ(nz))
        exp_clouds%layerQ = 0.0
        exp_clouds%layerQ(8) = 1.81183428E-01
        exp_clouds%layerQ(9) = 3.33446525E-02
        exp_clouds%layerQ(10) = 4.4320303E-01

        topoK = nz
        xlat = 70.0
        xlon = -150.0

        rh = (/ 95.0, 92.0, 68.0, 65.0, 60.0, 66.0, & 
                68.0, 70.0, 70.0, 100.0, 102.0, 98.0 /)
        t = (/ 235.0, 240.0, 245.0, 250.0, 254.0, 258.0, &
                262.0, 274.0, 256.0, 259.0, 265.0, 268.0 /)
        pres = (/ 30000.0, 35000.0, 40000.0, 45000.0, 50000.0, &
                    60000.0, 70000.0, 76000.0, 82000.0, 88000.0, 94000.0, 100000.0 /)
        ept = (/ 320.0, 315.0, 310.0, 307.0, 305.0, 303.0, &
                301.0, 310.0, 312.0, 309.0, 307.0, 300.0 /)
        vv = (/ -0.05, -0.08, -0.10, -0.12, -0.15, -0.20, &
                -0.25, -0.30, -0.40, -0.55, -0.10, -0.05 /)

        region = -1
        clouds%nLayers = 0
        clouds%wmnIdx = -1
        clouds%avv = 0.0
        clouds%topIdx = 0
        clouds%baseIdx = 0
        clouds%ctt = 0.0
        allocate(clouds%layerQ(nz))
        clouds%layerQ = 0.0

        call calc_CloudLayers(rh, t, pres, ept, vv, nz, topoK, xlat, xlon, region, clouds)

        if (clouds%nLayers .ne. exp_clouds%nLayers) then
            print *, "Expected nLayers: ", exp_clouds%nLayers, &
                     " but got: ", clouds%nLayers
        end if

        if (clouds%wmnIdx .ne. exp_clouds%wmnIdx) then
            print *, "Expected wmnIdx: ", exp_clouds%wmnIdx, &
                     " but got: ", clouds%wmnIdx
        end if

        if (abs(clouds%avv - exp_clouds%avv) > tol) then
            print *, "Expected avv: ", exp_clouds%avv, &
                     " but got: ", clouds%avv
        end if

        do i = 1, nz
            if (clouds%topIdx(i) .ne. exp_clouds%topIdx(i)) then
                print *, "Expected topIdx(", i, "): ", exp_clouds%topIdx(i), &
                         " but got: ", clouds%topIdx(i)
            end if
            if (clouds%baseIdx(i) .ne. exp_clouds%baseIdx(i)) then
                print *, "Expected baseIdx(", i, "): ", exp_clouds%baseIdx(i), &
                         " but got: ", clouds%baseIdx(i)
            end if
            if (abs(clouds%ctt(i) - exp_clouds%ctt(i)) > tol) then
                print *, "Expected ctt(", i, "): ", exp_clouds%ctt(i), &
                         " but got: ", clouds%ctt(i)
            end if
            if (abs(clouds%layerQ(i) - exp_clouds%layerQ(i)) > tol) then
                print *, "Expected layerQ(", i, "): ", exp_clouds%layerQ(i), &
                         " but got: ", clouds%layerQ(i)
            end if
        end do

    end subroutine test_multilayer_and_warmnose

    ! abs(xlat) < 23.5
    ! 23.5 <= abs(xlat) < 66


    ! in_cld = 0 (find cloud base?)

    ! clouds%avv = 0

    ! getAverageVertVel
    !   baseIdx_lowest != nz
    !   numVertVel == 0

    ! calc_LayerQ
    !   num_layers = 0

    ! getAverageVertVel(t,vv,nz, topIdx_lowest,baseIdx_lowest)
    !   Set start_base given base_idx_lowest
    !       1. base_idx_lowest == nz
    !           start_base = nz - 1
    !       2. base_idx_lowest != nz
    !           start_base = baseIdx_lowest
    !
    !   Set sumVertVel & numVertVel in loop (k = start_base, topIdx_lowest, -1)
    !       1. 257.15 <= t(k) <= 260.15
    !           sumVertVel += vv(k)
    !           numVertVel += 1 
    !       2. t(k) < 257.15
    !           sumVertVel += vv(k) + vv(k+1)
    !           numVertVel = 2
    !       3. t(k) > 260.15
    !           do nothing
    ! 
    !   Check numVertVel to determine return value
    !       1. numVertVel == 0
    !           Return 0
    !           * Can get this by having t(k) > 260.15 for all k
    !       2. numVertVel != 0
    !           Return sumVertVel / numVertVel
    !

    ! calc_layerQ(t, rh, pres, ept, nz, clouds)
    ! Loop n (cloud layers)
    !   Loop k (index in layer)
    !       Loop m (k, nz-1)
    !           1. ept(k) - ept(m) > 4 && clouds%baseIdx(n) <= m
    !               base_k = m - 1 (then exit)
    !       Loop kk (k, base_k-1)
    !           1. num_layers == 0
    !               mean_rh = 0
    !           2. num_layers != 0
    !               mean_rh = sum_rh / num_layers
    !

    ! calc_CloudLayers(rh,t,pres,ept,vv, nz, topoK, xlat, xlon, region, clouds)
    !   Get global region & set rh threshold
    !       1. abs(xlat) < 23.5
    !           t_rh = 80
    !           region = 1
    !       2. 23.5 <= abs(xlat) < 66
    !           t_rh = 75
    !           region = 2
    !       3. abs(xlat) >= 66
    !           t_rh = 70
    !           region = 2
    !
    !   Identify top layer if rh starts at high value
    !       1. rh(1) >= t_rh && rh(2) >= t_rh
    !           Loop kk (2,topoK-1)
    !               All if statements SHOULD executre if cloud base exists       
    !       2. rh(1) < t_rh || rh(2) < t_rh
    !           No else statement, just does nothing
    !   
    !   Loop k = kstart, topoK-1
    !       1. multiple nested if statements in loop, sets in_cld = 0 if all execute
    !       2. if they don't all execute, in_cld = 1, executing another if statement outside loop
    !
    !   Check num_lyr
    !       1. num_lyr > 0
    !           clouds%avv = getAverageVertVel()
    !       2. num_lyr <= 0
    !           clouds%avv = 0

end program test_cloud_layers