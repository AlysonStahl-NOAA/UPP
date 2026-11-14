! This is a test program for UPP.
!
! This program tests the CALWXT_BOURG_POST() subroutine.
!
! Alyson Stahl, 11/2026
program test_calwxt_bourg
    implicit none

    integer, parameter :: ntests = 1
    integer, parameter :: nlevs = 3
    ! 
    integer :: ista, iend, ista_2l, iend_2u, im
    integer :: jsta, jend, jsta_2l, jend_2u, jm
    integer :: lm, lp1, iseed
    integer :: me = 0 ! unused in the subroutine
    real :: g, pthresh
    real, dimension(1, ntests, nlevs) :: t, q, pmid
    real, dimension(1, ntests, nlevs+1) :: pint, zint
    real, dimension(1, ntests) :: lmh, prec
    integer, dimension(1, ntests) :: ptype, exp_ptype

    interface
        subroutine CALWXT_BOURG_POST(im, ista_2l, iend_2u, ista, iend, jm, &
                                    jsta_2l, jend_2u, jsta, jend, lm, lp1, &
                                    iseed, g, pthresh, t, q, pmid, pint, lmh, &
                                    prec, zint, ptype, me)
            integer, intent(in):: im, jm, jsta_2l, jend_2u, jsta, jend, lm, lp1, &
                                 iseed, me, ista_2l, iend_2u, ista, iend  
            real, intent(in):: g, pthresh
            real, intent(in), dimension(ista_2l:iend_2u,jsta_2l:jend_2u,lm)  :: t, q, pmid
            real, intent(in), dimension(ista_2l:iend_2u,jsta_2l:jend_2u,lp1) :: pint, zint
            real, intent(in), dimension(ista_2l:iend_2u,jsta_2l:jend_2u)     :: lmh, prec  
            integer, intent(out) :: ptype(ista:iend,jsta:jend)  
        end subroutine CALWXT_BOURG_POST
    end interface

    ista = 1
    iend = 1
    jsta = 1
    jend = ntests
    im = iend
    jm = ntests
    ista_2l = ista
    iend_2u = iend
    jsta_2l = jsta
    jend_2u = jend
    lm = nlevs
    lp1 = lm + 1
    iseed = 1
    g = 9.81
    pthresh = 0.0
    me = 0 ! unused in the subroutine

    ! Test Case 1: ptype = 1 (snow)
    exp_ptype(1,1) = 1
    t = 260.0
    q = 0.001
    pint(1,1,1) = 50000.0
    pint(1,1,2) = 70000.0
    pint(1,1,3) = 85000.0
    pint(1,1,4) = 100000.0
    pmid(1,1,1) = 60000.0
    pmid(1,1,2) = 77500.0
    pmid(1,1,3) = 92500.0
    zint(1,1,1) = 5500.0
    zint(1,1,2) = 3000.0
    zint(1,1,3) = 1500.0
    zint(1,1,4) = 0.0
    lmh = nlevs
    prec = 1.0

    call CALWXT_BOURG_POST(im, ista_2l, iend_2u, ista, iend, jm, &
                           jsta_2l, jend_2u, jsta, jend, lm, lp1, &
                           iseed, g, pthresh, t, q, pmid, pint, lmh, &
                           prec, zint, ptype, me)

    print *, ptype

    print *, "SUCCESS!"
end program test_calwxt_bourg
