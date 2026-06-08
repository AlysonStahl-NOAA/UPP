! This is a test program for UPP.
!
! This program tests the TABLE() subroutine.
!
! Alyson Stahl, 6/2026
program test_table
    use table_upp_mod, only: TABLE
    implicit none

    real, parameter :: tol = 1.0e-8
    integer, parameter :: ITB=076, JTB=134
    integer :: i, j, res
    !
    real :: PT, THL
    real :: PTBL(ITB,JTB), TTBL(JTB,ITB)
    real :: QS0(JTB), SQS(JTB), STHE(ITB), THE0(ITB)
    real :: RDQ, RDTH, RDP, RDTHE, PL
    !
    real :: EXP_RDQ, EXP_RDTH, EXP_RDP, EXP_RDTHE, EXP_PL

    ! PL = PT
    ! RDQ = KPM - 1 = ITB - 1 = 75
    ! RDTH = 1./DTH -> DTH = (THH - THL) / REAL(KTHM - 1) = (THH - THL) / REAL(JTB - 1) = (365 - THL) / 133
    ! RDP = 1./DP -> DP = (PH - PL) / REAL(KPM - 1) = (PH - PL) / REAL(ITB - 1) = (105000.- PL) / 75
    ! RDTHE = 1./DTHE -> DTHE = 1 / REAL(KTHM - 1) = 1 / (JTB - 1) = 1 / 133

    ! Same values used in INITPOST_GFS_NEMS_MPIIO()
    PT = 10000.0
    THL = 210.0

    call TABLE(PTBL, TTBL, PT, RDQ, RDTH, RDP, RDTHE, PL, THL, QS0, SQS, STHE, THE0)


    print *, 'SUCCESS!'
end program test_table