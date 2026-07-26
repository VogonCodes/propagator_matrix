module constants
    implicit none
    
    integer, parameter :: dp = 8
    real(dp), parameter :: PI = 3.1415926535897932384626433832795028841971693993751058209749445923078164062_dp
    real(dp), parameter :: G = 6.67430e-11_dp
    real(dp), parameter :: epsilon = 1e-16
    
    real(dp), parameter :: R_EARTH = 6371e3_dp
    real(dp), parameter :: R_VENUS = 6051.8_dp
end module
