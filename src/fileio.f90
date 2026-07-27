module fileio
    use constants, only: dp
    use body_mod
    implicit none

contains
    subroutine read_model(filepath, model, skip)
        character(*), intent(in) :: filepath
        type(body_t), intent(out) :: model
        integer, optional :: skip

        integer :: iostat, funit=12
        integer :: i, nlines, skiplines

        ! destroy in case model has already been initialised
        call destroy_body(model)

        ! open file for reading
        open(funit, file=trim(filepath), status="old")
        ! skip lines
        if (.not. present(skip)) then
            skiplines = 0
        else
            skiplines = skip
        endif
        print *, "Skipping ", skip, "lines"
        do i=1,skiplines
            read(funit,'(A)')
        enddo

        ! read number of layers
        read(funit, *) nlines

        ! initialise model
        call init_body(model,nlines)

        ! read data
        print *, "Reading ", nlines, " lines"
        do i=1,nlines
            read(funit, *, iostat=iostat) model%radius(i), model%density(i), model%shear_modulus(i)
            if (iostat /= 0) then
                print *, "Error reading data point", i, "! ", iostat
                nlines = i - 1
                exit
            endif
        enddo

        close(funit)
        print *, 'Done'
    end subroutine

end module
