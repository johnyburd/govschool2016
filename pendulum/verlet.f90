program VerletPend
implicit none

real tau, t, pi, omega, theta, theta2, alpha, pe, ke, m, r, minertia, g, temp
integer i, n

g = 9.8
m = 1.0
r = 1.0
minertia = m*r**2

pi = atan(1.0) * 4.0
n = 10000
tau = (2.0*pi) / float(n)
t =0.0

theta = ((179)*pi)/180
omega = 0.0
alpha = 0.0

open(unit=20, file='theta.dat')
open(unit=21, file='omega.dat')
open(unit=22, file='alpha.dat')

open(unit=23, file='ke.dat')
open(unit=24, file='pe.dat')
open(unit=25, file="total.dat")

alpha = -(g/r) * sin(theta)
! should be sin(theta)

theta2 = (omega + tau*alpha) * tau + theta
!pos2 = (v + tau * a)*tau + pos

do i = 1, n

    temp = theta2

    theta2 = -theta + 2.0*theta2 + alpha*(tau**2)

    omega = (theta2 - theta)/(2.0*tau)

    ke = 0.5 * minertia * omega**2
    !alpha = (theta2 + theta - 2*temp)/(tau**2)
    pe = m * g * r *(1-cos(theta2))

    alpha = -(g/r) * sin(theta2)

    theta = temp

    write(20, *) t, theta2
    write(21, *) t, omega
    write(22, *) t, alpha

    write(23, *) t, ke
    write(24, *) t, pe
    write(25, *) t, (ke + pe)

    t = t + tau
enddo
end

