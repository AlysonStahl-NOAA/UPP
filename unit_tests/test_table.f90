! This is a test program for UPP.
!
! This program tests the TABLE() subroutine.
!
! Alyson Stahl, 6/2026
program test_table
    ! TODO: Add any necessary set up code here for the other TODOs. You should only add 
    ! needed set up and necessary code. Do not do more than asked.
    use table_upp_mod, only: TABLE
    implicit none

    real, parameter :: tol = 1.0e-6
    integer, parameter :: ITB=076, JTB=134, ntests = 3
    integer :: i, j, res
    ! TODO: Create parameter with file names used in the tasks you will complete below.
    character(len=*), parameter :: ref_file_prefix = 'data/ref_table_case'
    character(len=*), parameter :: ref_file_suffix = '.txt'
    !
    real :: PT, THL
    real :: PTBL(ITB,JTB), TTBL(JTB,ITB)
    real :: QS0(JTB), SQS(JTB), STHE(ITB), THE0(ITB)
    real :: RDQ, RDTH, RDP, RDTHE, PL
    !
    real :: EXP_PTBL(ITB,JTB,ntests), EXP_TTBL(JTB,ITB,ntests)
    real :: EXP_QS0(JTB,ntests), EXP_SQS(JTB,ntests), EXP_STHE(ITB,ntests), EXP_THE0(ITB,ntests)
    real :: EXP_RDQ(ntests), EXP_RDTH(ntests), EXP_RDP(ntests), EXP_RDTHE(ntests), EXP_PL(ntests)

    ! Load expected data.
    !do i = 1, ntests
    !    call load_reference_data(i, EXP_PTBL(:,:,i), EXP_TTBL(:,:,i), EXP_QS0(:,i), &
    !                              EXP_SQS(:,i), EXP_STHE(:,i), EXP_THE0(:,i))
    !end do

    ! PL = PT
    ! RDQ = KPM - 1 = ITB - 1 = 75
    ! RDTH = 1./DTH -> DTH = (THH - THL) / REAL(KTHM - 1) = (THH - THL) / REAL(JTB - 1) = (365 - THL) / 133
    ! RDP = 1./DP -> DP = (PH - PL) / REAL(KPM - 1) = (PH - PL) / REAL(ITB - 1) = (105000.- PL) / 75
    ! RDTHE = 1./DTHE -> DTHE = 1 / REAL(KTHM - 1) = 1 / (JTB - 1) = 1 / 133

    ! Test Case 1: Standard case. Same input values used in INITPOST_GFS_NEMS_MPIIO().
    PT = 10000.0
    THL = 210.0

    EXP_RDQ(1) = 75.0
    EXP_RDTH(1) = 133.0 / (365.0 - THL)
    EXP_RDP(1) = 75.0 / (105000.0 - PT)
    EXP_RDTHE(1) = 133.0
    EXP_PL(1) = PT

    call TABLE(PTBL, TTBL, PT, RDQ, RDTH, RDP, RDTHE, PL, THL, QS0, SQS, STHE, THE0)

    res = 0

    ! TODO: Comment out all of the value checks below.
    ! if (abs(RDQ - EXP_RDQ(1)) > tol) then
    !     print *, 'Test Case 1 Failed: RDQ = ', RDQ, ' Expected: ', EXP_RDQ(1)
    !     res = 1
    ! end if
    ! if (abs(RDTH - EXP_RDTH(1)) > tol) then
    !     print *, 'Test Case 1 Failed: RDTH = ', RDTH, ' Expected: ', EXP_RDTH(1)
    !     res = 1
    ! end if
    ! if (abs(RDP - EXP_RDP(1)) > tol) then
    !     print *, 'Test Case 1 Failed: RDP = ', RDP, ' Expected: ', EXP_RDP(1)
    !     res = 1
    ! end if
    ! if (abs(RDTHE - EXP_RDTHE(1)) > tol) then
    !     print *, 'Test Case 1 Failed: RDTHE = ', RDTHE, ' Expected: ', EXP_RDTHE(1)
    !     res = 1
    ! end if
    ! if (abs(PL - EXP_PL(1)) > tol) then
    !     print *, 'Test Case 1 Failed: PL = ', PL, ' Expected: ', EXP_PL(1)
    !     res = 1
    ! end if

    ! do i = 1, ITB
    !     do j = 1, JTB
    !         if (abs(PTBL(i,j) - EXP_PTBL(i,j,1)) > tol) then
    !             print *, 'Test Case 1 Failed: PTBL(', i, ',', j, ') = ', PTBL(i,j), &
    !                      ' Expected: ', EXP_PTBL(i,j,1)
    !             res = 1
    !         end if
    !         if (abs(TTBL(j,i) - EXP_TTBL(j,i,1)) > tol) then
    !             print *, 'Test Case 1 Failed: TTBL(', j, ',', i, ') = ', TTBL(j,i), &
    !                      ' Expected: ', EXP_TTBL(j,i,1)
    !             res = 1
    !         end if
    !     end do
    !     if (abs(STHE(i) - EXP_STHE(i,1)) > tol) then
    !         print *, 'Test Case 1 Failed: STHE(', i, ') = ', STHE(i), ' Expected: ', EXP_STHE(i,1)
    !         res = 1
    !     end if
    !     if (abs(THE0(i) - EXP_THE0(i,1)) > tol) then
    !         print *, 'Test Case 1 Failed: THE0(', i, ') = ', THE0(i), ' Expected: ', EXP_THE0(i,1)
    !         res = 1
    !     end if
    ! end do

    ! do j = 1, JTB
    !     if (abs(QS0(j) - EXP_QS0(j,1)) > tol) then
    !         print *, 'Test Case 1 Failed: QS0(', j, ') = ', QS0(j), ' Expected: ', EXP_QS0(j,1)
    !         res = 1
    !     end if
    !     if (abs(SQS(j) - EXP_SQS(j,1)) > tol) then
    !         print *, 'Test Case 1 Failed: SQS(', j, ') = ', SQS(j), ' Expected: ', EXP_SQS(j,1)
    !         res = 1
    !     end if
    ! end do

    ! TODO: Replace the ??? with code that calls your subroutine to write the OUTPUT arrays 
    ! to ref_table_case1.txt. 
    call write_reference_data(1, PT, THL, PTBL, TTBL, QS0, SQS, STHE, THE0)

    ! Test Case 2: PT = 0.0 (reaches the p <= 0.0 branch)
    PT = 0.0
    THL = 210.0

    EXP_RDQ(2) = 75.0
    EXP_RDTH(2) = 133.0 / (365.0 - THL)
    EXP_RDP(2) = 75.0 / (105000.0 - PT)
    EXP_RDTHE(2) = 133.0
    EXP_PL(2) = PT

    call TABLE(PTBL, TTBL, PT, RDQ, RDTH, RDP, RDTHE, PL, THL, QS0, SQS, STHE, THE0)

    ! TODO: Replace the ??? with code that calls your subroutine to write the OUTPUT arrays 
    ! to ref_table_case2.txt. 
    call write_reference_data(2, PT, THL, PTBL, TTBL, QS0, SQS, STHE, THE0)

    ! Test Case 3: Low Pressure (reaches DENOM <= EPS branch)
    PT = 100.0
    THL = 210.0

    EXP_RDQ(3) = 75.0
    EXP_RDTH(3) = 133.0 / (365.0 - THL)
    EXP_RDP(3) = 75.0 / (105000.0 - PT)
    EXP_RDTHE(3) = 133.0
    EXP_PL(3) = PT

    call TABLE(PTBL, TTBL, PT, RDQ, RDTH, RDP, RDTHE, PL, THL, QS0, SQS, STHE, THE0)

    ! TODO: Replace the ??? with code that calls your subroutine to write the OUTPUT arrays 
    ! to ref_table_case3.txt. 
    call write_reference_data(3, PT, THL, PTBL, TTBL, QS0, SQS, STHE, THE0)

    print *, 'SUCCESS!'

contains

    subroutine load_reference_data(case_num, ptbl_out, ttbl_out, qs0_out, sqs_out, sthe_out, the0_out)
        integer, intent(in) :: case_num
        real, intent(out) :: ptbl_out(ITB,JTB), ttbl_out(JTB,ITB)
        real, intent(out) :: qs0_out(JTB), sqs_out(JTB), sthe_out(ITB), the0_out(ITB)
        real :: temp_2d(ITB*JTB)
        character(len=100) :: filename, header_line
        integer :: unit_num, j
        
        write(filename, '(a,i1,a)') ref_file_prefix, case_num, ref_file_suffix
        open(newunit=unit_num, file=filename, status='old', action='read')
        
        ! Skip 3 header lines
        read(unit_num, '(a)') header_line
        read(unit_num, '(a)') header_line
        read(unit_num, '(a)') header_line
        
        do j = 1, ITB*JTB
            read(unit_num, *) temp_2d(j)
        end do
        ptbl_out = reshape(temp_2d, [ITB, JTB])
        
        do j = 1, JTB*ITB
            read(unit_num, *) temp_2d(j)
        end do
        ttbl_out = reshape(temp_2d, [JTB, ITB])
        
        do j = 1, JTB
            read(unit_num, *) qs0_out(j)
        end do
        
        do j = 1, JTB
            read(unit_num, *) sqs_out(j)
        end do
        
        do j = 1, ITB
            read(unit_num, *) sthe_out(j)
        end do
        
        do j = 1, ITB
            read(unit_num, *) the0_out(j)
        end do
        
        close(unit_num)
    end subroutine load_reference_data

    ! TODO: Replace the ??? with a subroutine that takes in the case number, PT and THL inputs, and the
    ! expected output arrays. The subroutine should then write the expected output arrays to a text file
    ! that will be read by load_reference_data() in the test program. The file names should be
    ! ref_table_case1.txt, ref_table_case2.txt, etc. The format of the file should be compatible with the way
    ! load_reference_data() reads in the data. Write the values with a precision that is slightly higher 
    ! than the a tolerance of 1.0e-8 (even if 1.0e-6 is used as the tolerance in the test program).
    subroutine write_reference_data(case_num, pt_in, thl_in, ptbl_in, ttbl_in, qs0_in, sqs_in, sthe_in, the0_in)
        integer, intent(in) :: case_num
        real, intent(in) :: pt_in, thl_in
        real, intent(in) :: ptbl_in(ITB,JTB), ttbl_in(JTB,ITB)
        real, intent(in) :: qs0_in(JTB), sqs_in(JTB), sthe_in(ITB), the0_in(ITB)
        character(len=100) :: filename
        integer :: unit_num, i, j
        
        write(filename, '(a,i1,a)') ref_file_prefix, case_num, ref_file_suffix
        open(newunit=unit_num, file=filename, status='replace', action='write')
        
        write(unit_num, '(a)') '# Reference data for TABLE() subroutine test'
        write(unit_num, '(a,f10.1,a,f10.1)') '# Test case with PT=', pt_in, ', THL=', thl_in
        write(unit_num, '(a,i0,a,i0,a,i0,a,i0,a,i0,a,i0,a)') &
            '# Format: PTBL(', ITB*JTB, ' values), TTBL(', JTB*ITB, '), QS0(', JTB, &
            '), SQS(', JTB, '), STHE(', ITB, '), THE0(', ITB, ')'
        
        do j = 1, JTB
            do i = 1, ITB
                write(unit_num, '(es24.16)') ptbl_in(i,j)
            end do
        end do
        
        do i = 1, ITB
            do j = 1, JTB
                write(unit_num, '(es24.16)') ttbl_in(j,i)
            end do
        end do
        
        do j = 1, JTB
            write(unit_num, '(es24.16)') qs0_in(j)
        end do
        
        do j = 1, JTB
            write(unit_num, '(es24.16)') sqs_in(j)
        end do
        
        do i = 1, ITB
            write(unit_num, '(es24.16)') sthe_in(i)
        end do
        
        do i = 1, ITB
            write(unit_num, '(es24.16)') the0_in(i)
        end do
        
        close(unit_num)
    end subroutine write_reference_data

end program test_table