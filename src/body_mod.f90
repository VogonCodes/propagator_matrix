module body_mod
    use constants, only: dp
    implicit none

    type layer_t
        real(dp) :: outer_radius, inner_radius, density, gravity, shear_modulus, viscosity, bulk_modulus
        character :: type
    end type

    type :: body_t
        integer :: n_layers
        real(dp), allocatable :: radius(:), density(:), shear_modulus(:), gravity(:), viscosity(:), bulk_modulus(:)
    end type

    contains
        subroutine init_body(body, N)
            type(body_t), intent(inout) :: body
            integer, intent(in) :: N

            if (N <= 0) then
                error stop "Number of layers must be positive integer!"
            endif

            ! stupid, but assume for now that is this is allocated, then everything else is too
            if (allocated(body%radius)) then
                error stop "Already allocated!"
            endif

            body%n_layers = N
            allocate(body%radius(N))
            allocate(body%density(N))
            allocate(body%shear_modulus(N))
            allocate(body%gravity(N))
            allocate(body%viscosity(N))
            allocate(body%bulk_modulus(N))
        end subroutine

        subroutine destroy_body(body)
            type(body_t), intent(inout) :: body

            if (allocated(body%radius)) then
                deallocate(body%radius)
            else if (allocated(body%density)) then
                deallocate(body%density)
            else if (allocated(body%shear_modulus)) then
                deallocate(body%shear_modulus)
            else if (allocated(body%gravity)) then
                deallocate(body%gravity)
            else if (allocated(body%viscosity)) then
                deallocate(body%viscosity)
            else if (allocated(body%bulk_modulus)) then
                deallocate(body%bulk_modulus)
            end if
            body%n_layers = 0
        end subroutine

        subroutine get_layer(body, i, layer)
            type(body_t), intent(in) :: body
            integer, intent(in) :: i
            type(layer_t), intent(out) :: layer

            if (i .gt. body%n_layers) then
                error stop "Layer index greater than no of layers!"
            endif

            if (i .lt. body%n_layers) then
                layer%inner_radius = body%radius(i+1)
            else
                layer%inner_radius = 0._dp
            endif
            layer%outer_radius = body%radius(i)
            layer%density = body%density(i)
            layer%gravity = body%gravity(i)
            layer%viscosity = body%viscosity(i)
            layer%shear_modulus = body%shear_modulus(i)
            layer%bulk_modulus = body%bulk_modulus(i)

            if (layer%shear_modulus .eq. 0._dp) then
                layer%type = 'f' ! fluid
            else if (layer%viscosity .eq. 0._dp) then
                layer%type = 'e' ! elastic
            else 
                layer%type = 'v' ! viscoelastic
            endif
        end subroutine

        subroutine write_layer_t(layer, unit)
            type(layer_t), intent(in) :: layer
            integer, intent(in), optional :: unit
            integer :: u

            if (present(unit)) then
                u = unit
            else
                u = 6 ! default stdo
            endif

            write(u, '(A,A)') "type of layer: ", layer%type
            write(u, '(A, F12.4)') "inner radius: ", layer%inner_radius
            write(u, '(A, F12.4)') "outer radius: ", layer%outer_radius
            write(u, '(A, F12.4)') "density: ", layer%density
            write(u, '(A, F12.4)') "shear modulus: ", layer%shear_modulus
            write(u, '(A, F12.6)') "gravity: ", layer%gravity
            write(u, '(A, F12.4)') "viscosity: ", layer%viscosity
            write(u, '(A, F12.4)') "bulk modulus: ", layer%bulk_modulus
        end subroutine


        subroutine write_body_t(body, unit)
            type(body_t), intent(in) :: body
            integer, intent(in), optional :: unit
            integer :: u

            if (present(unit)) then
                u = unit
            else
                u = 6 ! default stdo
            endif

            write(u,* ) "No of layers:", body%n_layers
            write(u, '(A, 5F12.4)') "radii (first 5 values): ", body%radius(1:min(5,body%n_layers))
            write(u, '(A, 5F12.4)') "densities (first 5 values): ", body%density(1:min(5,body%n_layers))
            write(u, '(A, 5F12.4)') "shear moduli (first 5 values): ", body%shear_modulus(1:min(5,body%n_layers))
            write(u, '(A, 5F12.6)') "gravity (first 5 values): ", body%gravity(1:min(5,body%n_layers))
            write(u, '(A, 5F12.4)') "viscosities (first 5 values): ", body%viscosity(1:min(5,body%n_layers))
            write(u, '(A, 5F12.4)') "bulk moduli (first 5 values): ", body%bulk_modulus(1:min(5,body%n_layers))
        end subroutine

end module
