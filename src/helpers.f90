module helpers
    use constants
    use body_mod
    implicit none

    contains
        subroutine calculate_gravity(model)
            type(body_t), intent(inout) :: model
            integer :: i, j, n
            real(dp) :: gravity_prefactor

            n = model%n_layers
            gravity_prefactor = 4*PI*G/3._dp

            model%gravity = 0.0_dp
            
            ! TODO
            do i=1,n
                model%gravity(i) = model%density(i)*model%radius(i)
                do j=n,i+1,-1
                    model%gravity(i) = model%gravity(i) &
                        + (model%density(j) - model%density(j-1)) * model%radius(j) * ((model%radius(j) / model%radius(j-1))**2)
                enddo
            enddo
            ! TODO
            model%gravity = model%gravity * gravity_prefactor
        end subroutine

        function calculate_mu_laplace(mu, viscosity, s) result(lmu)
            real(dp), intent(in) :: mu, viscosity
            complex(dp), intent(in) :: s
            complex(dp) :: lmu

            lmu = mu*s / s + (mu/viscosity)
        end function

        ! create nxn real double precision identity matrix
        function eye(n) result(matrix)
            integer, intent(in) :: n

            real(dp) :: matrix(n,n)
            integer :: i

            matrix = 0._dp
            do i=1,n
                matrix(i,i) = 1._dp
            enddo
        end function

        ! create nxn complex identity matrix
        function ceye(n) result(matrix)
            integer, intent(in) :: n

            complex(dp) :: matrix(n,n)
            integer :: i

            matrix = 0._dp
            do i=1,n
                matrix(i,i) = 1._dp
            enddo
        end function

        subroutine print_matrix(matrix)
            complex(dp) :: matrix(:,:)

            integer :: mshape(2)
            integer :: i

            mshape = shape(matrix)
            do i=1,mshape(1)
                print *, real(matrix(i,:))
            enddo
        end subroutine

        subroutine clean_complex_arr(array)
            complex(dp), intent(inout) :: array(:)

            where (abs(array) < TOL) array = 0.0_dp
            where (abs(real(array)) < TOL) array = cmplx(0.0_dp, aimag(array))
            where (abs(aimag(array)) < TOL) array = cmplx(real(array), 0.0_dp)
        end subroutine

end module
