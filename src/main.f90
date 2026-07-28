program love
    use fileio
    use body_mod
    use helpers
    use constants, only: dp
    use propagator
    implicit none

    type(body_t) :: model
    complex(dp) :: s
    complex(dp) :: solution(6)

    s = complex(1.,0.)

    write(6,*) "baf"
    call init_model("earth_prem.dat", model)
    if (allocated(model%radius)) print*, "bafbaf"
    call write_body_t(model)

    call solve(model, 2, s, solution)
    
    print *, solution

contains
    subroutine init_model(filepath, model)
        character(*), intent(in) :: filepath
        type(body_t), intent(out) :: model
        
        call read_model(filepath, model, 1)
        call calculate_gravity(model)
    end subroutine
end program
