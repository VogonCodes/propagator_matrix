module body_mod
    use constants, only: dp
    implicit none

    type :: layer_t
        real(dp) :: radius, density, shear_modulus, gravity, viscosity, bulk_modulus
    end type

    type :: body_t
        integer :: n_layers
        type(layer_t), allocatable :: layers(:)
        real(dp) :: bottom_radius, surface_radius
    end type

    contains
        subroutine init_body(body, N)
            type(body_t), intent(inout) :: body
            integer, intent(in) :: N

            if (N <= 0) then
                error stop "Number of layers must be positive integer!"
            endif

            if (allocated(body%layers)) then
                error stop "Already allocated!"
            endif

            body%n_layers = N
            allocate(body%layers(N))
        end subroutine

        subroutine destroy_body(body)
            type(body_t), intent(inout) :: body

            if (allocated(body%layers)) then
                deallocate(body%layers)
            end if
            body%n_layers = 0
            body%bottom_radius = 0
            body%surface_radius = 0
        end subroutine

end module
