module propagator
    use constants
    use body_mod
    use helpers
    implicit none

    private
    public :: solve

contains
    subroutine solve(model, degree, s, solution)
        type(body_t), intent(in) :: model
        integer, intent(in) :: degree
        complex(dp), intent(in) :: s
        complex(dp), intent(out) :: solution(6)

        complex(dp) :: f_matrix(6,6), finv_matrix(6,6)
        complex(dp) :: i_matrix(6,3)
        complex(dp) :: propagator(6,6), propagator_bc(6,3), propagator_small(3,3)
        complex(dp) :: rhs(3)
        type(layer_t) :: layer
        integer :: i,n

        ! cgesv variables
        integer :: ipiv(3), info


        n = model%n_layers

        ! assemble propagator
        propagator = ceye(6)
        do i=1,n-1
            call get_layer(model, i, layer)
            if (layer%type .eq. 'v') then
                layer%shear_modulus = calculate_mu_laplace(layer%shear_modulus, layer%viscosity, s)
            endif
            call assemble_fundamental_matrix_solid(layer, degree, f_matrix)
            call assemble_fundamental_matrix_solid_inverse(layer, degree, finv_matrix)
            propagator = matmul(matmul(propagator,f_matrix), finv_matrix)
        enddo

        ! apply boundary conditions
        call assemble_interface_matrix_solid(layer, degree, i_matrix)

        ! solve for vector C
        propagator_bc = matmul(propagator, i_matrix)

        ! copy to 3x3 matrix
        propagator_small(1,:) = propagator_bc(3,:)
        propagator_small(2,:) = propagator_bc(4,:)
        propagator_small(3,:) = propagator_bc(6,:)

        ! geth rhs
        call tidal_forcing_vector(degree, model%radius(1), rhs)

        ! solve system of linear equations
        ! cgesv(n, nrhs, a, lda, ipiv, b, ldb, info)
        ! out: 
        !   A = P*L*U
        !   ipiv: pivot indices defining P
        !   b: solution if info == 0
        !   info: ==0 -> success; >0 -> u(i,i)==0; <0 -> i-th argument invalid
        call cgesv(3, 1, propagator_small, 3, ipiv, rhs, 3, info)

        if (.not. (info .eq. 0) ) then
            error stop "upsik"
        endif

        solution = matmul(propagator_bc, rhs)
    end subroutine

    !-------------------------------------------------------------------------------------------------------------
    ! Assemble the fundamental matrix of the spheroidal part, acc. to Sabadini & Vermeersen (2004)
    !-------------------------------------------------------------------------------------------------------------
    subroutine assemble_fundamental_matrix_solid(layer, degree, fundamental_matrix)
        type(layer_t) :: layer
        integer :: degree
        complex(dp) :: fundamental_matrix(6,6)

        real(dp) :: rho, gr, r
        complex(dp) :: mu
        ! precompute values
        real(dp) :: rl, rlp1, rlp2, rlp3, rlm1, rlm2

        r = layer%inner_radius
        rho = layer%density
        gr = layer%gravity
        mu = layer%shear_modulus

        rl = r**degree
        rlp1 = r**(degree+1)
        rlp2 = r**(degree+2)
        rlp3 = r**(degree+3)
        rlm1 = r**(degree-1)
        rlm2 = r**(degree-2)

        fundamental_matrix = 0._dp
        ! first row
        fundamental_matrix(1,1) = degree * rlp1 / 2._dp / (2*degree + 3._dp)
        fundamental_matrix(1,2) = rlm1
        fundamental_matrix(1,4) = (degree+1) / rl / 2._dp / (2*degree - 1._dp)
        fundamental_matrix(1,5) = 1._dp / rlp2
        ! second row
        fundamental_matrix(2,1) = (degree + 3._dp) * rlp1 / 2._dp / (2*degree+3._dp) / (degree + 1._dp)
        fundamental_matrix(2,2) = rlm1 / degree ! is this still double precision?
        fundamental_matrix(2,4) = (2._dp - degree) / rl / 2._dp / degree / (2*degree - 1._dp)
        fundamental_matrix(2,5) = - 1._dp / rlp2 / (degree + 1._dp)
        ! third row
        fundamental_matrix(3,1) = ( degree * rho * gr * r + 2*(degree**2 - degree - 3._dp)*mu ) * rl / 2._dp / &
            & (2*degree+3._dp)
        fundamental_matrix(3,2) = ( rho*gr*r + 2*(degree - 1._dp)*mu ) * rlm2
        fundamental_matrix(3,3) = -rho*rl
        fundamental_matrix(3,4) = ( (degree+1._dp)*rho*gr*r - 2._dp*(degree**2+3._dp*degree-1)*mu ) / 2._dp / (2*degree - 1._dp) / rlp1
        fundamental_matrix(3,5) = ( rho*gr*r - 2._dp*(degree+2)*mu ) / rlp3
        fundamental_matrix(3,6) = - rho / rlp1
        ! fourth row
        fundamental_matrix(4,1) = degree*(degree+2._dp)*mu*rl / (2*degree+3._dp) / (degree+1._dp)
        fundamental_matrix(4,2) = 2._dp*(degree-1)*mu*rlm2 / degree ! is this still double precision?
        fundamental_matrix(4,4) = ( degree**2 - 1 )*mu / degree / (2*degree - 1._dp) / rlp1
        fundamental_matrix(4,5) = 2*(degree+2._dp)*mu / (degree+1._dp) / rlp3
        ! fifth row
        fundamental_matrix(5,3) = -rl
        fundamental_matrix(5,6) = - 1._dp / rlp1
        ! sixth row
        fundamental_matrix(6,1) = 2*PI*G*rho*degree*rlp1 / (2*degree+3._dp)
        fundamental_matrix(6,2) = 4*PI*G*rho*rlm1
        fundamental_matrix(6,3) = -(2*degree+1._dp) * rlm1
        fundamental_matrix(6,4) = 2*PI*G*rho*(degree+1._dp) / (2*degree-1._dp) / rl
        fundamental_matrix(6,5) = 2*PI*G*rho / rlp2
    end subroutine

    !-------------------------------------------------------------------------------------------------------------
    ! Assemble the inverse of the fundamental matrix of the spheroidal part, acc. to Sabadini & Vermeersen (2004)
    !-------------------------------------------------------------------------------------------------------------
    subroutine assemble_fundamental_matrix_solid_inverse(layer, degree, fundamental_matrix)
        type(layer_t) :: layer
        integer :: degree
        complex(dp) :: fundamental_matrix(6,6)

        complex(dp), dimension(6,6) :: dmatrix, ymatrix
        real(dp) :: rho, gr, r
        real(dp) :: rl, rlp1, rlm1
        complex(dp) :: mu, rmu

        r = layer%outer_radius
        rho = layer%density
        gr = layer%gravity
        mu = layer%shear_modulus

        ! precompute values
        rmu = r/mu
        rl = r**degree
        rlp1 = r**(degree+1)
        rlm1 = r**(degree-1)

        dmatrix = 0._dp
        dmatrix(1,1) = (degree +1._dp) / rlp1
        dmatrix(2,2) = degree * (degree+1._dp) / 2._dp / (2*degree - 1._dp) / rlm1
        dmatrix(3,3) = - 1/rlm1
        dmatrix(4,4) = degree * rl
        dmatrix(5,5) = degree * (degree + 1._dp) * r**(degree+2) / 2._dp / (2*degree + 3._dp)
        dmatrix(6,6) = - rlp1
        dmatrix = dmatrix / (2*degree + 1._dp)

        ! the rest
        ymatrix = 0._dp
        !! first row
        ymatrix(1,1) = rho*gr*rmu - 2._dp*(degree+2)
        ymatrix(1,2) = 2._dp*degree*(degree+2)
        ymatrix(1,3) = -rmu
        ymatrix(1,4) = degree*rmu
        ymatrix(1,5) = rho*rmu
        !! second row
        ymatrix(2,1) = -rho*gr*rmu + 2._dp*(degree**2 + 3*degree - 1) / (degree + 1._dp)
        ymatrix(2,2) = -2._dp * (degree**2 - 1)
        ymatrix(2,3) = rmu
        ymatrix(2,4) = (2._dp - degree)*rmu
        ymatrix(2,5) = -rho*rmu
        !! third row
        ymatrix(3,1) = 4*PI*G*rho
        ymatrix(3,6) = -1._dp
        !! fourth row
        ymatrix(4,1) = rho*gr*rmu + 2._dp*(degree-1)
        ymatrix(4,2) = 2._dp*(degree**2-1)
        ymatrix(4,3) = -rmu
        ymatrix(4,4) = -(degree+1._dp)*rmu
        ymatrix(4,5) = rho*rmu
        !! fifth row
        ymatrix(5,1) = -rho*gr*rmu - 2._dp*(degree**2 - degree - 3) / degree
        ymatrix(5,2) = -2._dp*degree*(degree+2)
        ymatrix(5,3) = rmu
        ymatrix(5,4) = (degree + 3._dp)*rmu
        ymatrix(5,5) = -rho*rmu
        !! sixth row
        ymatrix(6,1) = 4*PI*G*rho*r
        ymatrix(6,5) = 2._dp*degree+1._dp
        ymatrix(6,6) = -r

        fundamental_matrix = matmul(dmatrix,ymatrix)
    end subroutine

    subroutine assemble_interface_matrix_solid(layer, degree, interface_matrix)
        type(layer_t), intent(in) :: layer
        integer, intent(in) :: degree
        complex(dp) :: interface_matrix(6,3)

        real(dp) :: rho, gr, r
        real(dp) :: rl, rlp1, rlp2, rlp3, rlm1, rlm2
        complex(dp) :: mu

        r = layer%outer_radius
        rho = layer%density
        gr = layer%gravity
        mu = layer%shear_modulus

        ! precompute values
        rl = r**degree
        rlp1 = r**(degree+1)
        rlp2 = r**(degree+2)
        rlp3 = r**(degree+3)
        rlm1 = r**(degree-1)
        rlm2 = r**(degree-2)

        interface_matrix = 0._dp
        ! first row
        interface_matrix(1,1) = degree * rlp1 / 2._dp / (2*degree + 3._dp)
        interface_matrix(1,2) = rlm1
        ! second row
        interface_matrix(2,1) = (degree + 3._dp) * rlp1 / 2._dp / (2*degree+3._dp) / (degree + 1._dp)
        interface_matrix(2,2) = rlm1 / degree ! is this still double precision?
        ! third row
        interface_matrix(3,1) = ( degree * rho * gr * r + 2*(degree**2 - degree - 3._dp)*mu ) * rl / 2._dp / &
            & (2*degree+3._dp)
        interface_matrix(3,2) = ( rho*gr*r + 2*(degree - 1._dp)*mu ) * rlm2
        interface_matrix(3,3) = -rho*rl
        ! fourth row
        interface_matrix(4,1) = degree*(degree+2._dp)*mu*rl / (2*degree+3._dp) / (degree+1._dp)
        interface_matrix(4,2) = 2._dp*(degree-1)*mu*rlm2 / degree ! is this still double precision?
        ! fifth row
        interface_matrix(5,3) = -rl
        ! sixth row
        interface_matrix(6,1) = 2*PI*G*rho*degree*rlp1 / (2*degree+3._dp)
        interface_matrix(6,2) = 4*PI*G*rho*rlm1
        interface_matrix(6,3) = -(2*degree+1._dp) * rlm1
    end subroutine

    subroutine tidal_forcing_vector(degree, radius, forcing)
        integer, intent(in) :: degree
        real(dp), intent(in) :: radius
        complex(dp), intent(out) :: forcing(3)

        forcing = 0.0_dp
        forcing(3) = -(2*degree+1._dp)/radius
    end subroutine


end module
