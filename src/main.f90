program love
    use fileio
    use body_mod
    use helpers
    use constants, only: dp
    use propagator
    implicit none

    type(body_t) :: model
    complex(dp) :: s
    complex(dp) :: solution(6), loven(3)

    s = complex(1.,0.)

    write(6,*) "baf"
    call init_model("earth_prem.dat", model)
    if (allocated(model%radius)) print*, "bafbaf"
    call write_body_t(model)

    call solve(model, 2, s, solution)
    call calc_love(solution, model%gravity(1), loven)
    
    print *, solution
    print *, loven

contains
    subroutine init_model(filepath, model)
        character(*), intent(in) :: filepath
        type(body_t), intent(out) :: model
        
        call read_model(filepath, model, 1)
        call calculate_gravity(model)
    end subroutine

    subroutine calc_love(solution, surface_gravity, love_numbers)
        complex(dp), intent(in) :: solution(6)
        real(dp), intent(in) :: surface_gravity
        complex(dp), intent(out) :: love_numbers(3)
            
        love_numbers(1) = surface_gravity * solution(1)
        love_numbers(2) = surface_gravity * solution(2)
        love_numbers(3) = -solution(5) - 1
    end subroutine

end program
