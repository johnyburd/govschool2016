program rk4

implicit none
real :: state(6), k1(6), k2(6), k3(6), k4(6), temp(6), a(3)
real :: pi, gm, m
real :: ke, pe, e
real :: t, tau
integer(8) :: i, n


pi = 4 * atan(1.0)
gm = 4 * pi ** 2
m = 1.0

t = 0.0
n = 500000
tau = 500.0/float(n)

state = 0.0
k1 = 0.0
k2 = 0.0
k3 = 0.0
k4 = 0.0

state(4) = 1.0
state(5) = 0.0
state(6) = 0.0

open(unit = 15, file="pos.dat")
open(unit = 16, file="vel.dat")
open(unit = 17, file="ke.dat")
open(unit = 18, file="pe.dat")
open(unit = 19, file="e.dat")

do i = 1, n

    call acceleration(state, a)

    k1(1:3) = state(4:6)
    k1(4:6) = a

    temp = state + 0.5 * tau * k1

    call acceleration(temp, a)

    k2(1:3) = temp(4:6)
    k2(4:6) = a

    temp = state + 0.5 * tau * k2

    call acceleration(temp, a)

    k3(1:3) = temp(4:6)
    k3(4:6) = a

    temp = state + tau * k3

    call acceleration(temp, a)

    k4(1:3) = temp(4:6)
    k4(4:6) = a


    state = state + (1.0/6.0) * tau * (k1 + 2.0 * k2 + 2.0 * k3 + k4)

    t = t + tau

    ke = 0.5 * m * norm2(state(4:6))
    pe = -(gm*m)/norm2(state(4:6))
    e = ke + pe

    write(15, *) state(1), state(2), state(3)
    write(16, *) state(4), state(5), state(6)
    write(17, *) ke
    write(18, *) pe
    write(19, *) e

enddo

end program rk4
subroutine acceleration(pos, acc)
    implicit none
    real, intent(in) :: pos(6)
    real, intent(out) :: acc(3)
    real :: q, m, e(3), b(3), crpdt(3), v(3)
    v = pos(4:6)

    q = 1.0
    m = 1.0
    e = 0.0
    b = 0.0
    e(2) = 0.001
    e(3) = 0.1
    b(3) = 0.1


    crpdt(1) = v(2) * b(3) - b(2) * v(3)
    crpdt(2) = - (v(1) * b(3) - b(1) * v(3))
    crpdt(3) = v(1) * b(2) - b(1) * v(2)

    acc = (q/m) * E + (q/m) * crpdt

end subroutine acceleration


