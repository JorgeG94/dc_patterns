!! Shared, dependency-free harness for the dc_patterns mini-apps: CLI args,
!! timing, an error metric, and the one machine-parseable RESULT line every
!! executable prints. Its own translation unit on purpose (a cross-module
!! `use` in every mini-app).
module dcp_harness
   use, intrinsic :: iso_fortran_env, only: int64
   use dcp_kinds, only: wp
   implicit none
   private
   public :: dcp_args_t, dcp_get_args, dcp_tick, dcp_tock, dcp_max_abs_err, dcp_report

   type :: dcp_args_t
      integer :: n = 4000000     !! problem size (cells)
      integer :: iters = 50      !! timed repetitions
   end type dcp_args_t

contains

   subroutine dcp_get_args(a)
      !! Parse `--n <int>` and `--iters <int>`; unrecognised args ignored.
      type(dcp_args_t), intent(out) :: a
      integer :: i, nargc, ios
      character(len=64) :: arg, val
      nargc = command_argument_count()
      i = 1
      do while (i <= nargc)
         call get_command_argument(i, arg)
         select case (trim(arg))
         case ("--n")
            call get_command_argument(i + 1, val)
            read (val, *, iostat=ios) a%n
            i = i + 2
         case ("--iters")
            call get_command_argument(i + 1, val)
            read (val, *, iostat=ios) a%iters
            i = i + 2
         case default
            i = i + 1
         end select
      end do
   end subroutine dcp_get_args

   function dcp_tick() result(t)
      !! Start a wall-clock interval.
      integer(int64) :: t
      call system_clock(t)
   end function dcp_tick

   function dcp_tock(t0) result(secs)
      !! Seconds elapsed since `t0`.
      integer(int64), intent(in) :: t0
      real(wp) :: secs
      integer(int64) :: t1, rate
      call system_clock(t1, rate)
      secs = real(t1 - t0, wp)/real(rate, wp)
   end function dcp_tock

   pure function dcp_max_abs_err(a, b) result(e)
      !! Max |a-b| — the backend-vs-reference correctness metric.
      real(wp), intent(in) :: a(:), b(:)
      real(wp) :: e
      e = maxval(abs(a - b))
   end function dcp_max_abs_err

   subroutine dcp_report(name, data_name, n, iters, max_abs_err, tol, secs)
      !! Emit the uniform RESULT line and exit non-zero on FAIL.
      character(len=*), intent(in) :: name, data_name
      integer, intent(in) :: n, iters
      real(wp), intent(in) :: max_abs_err, tol, secs
      character(len=4) :: status
      real(wp) :: mcells
      logical :: ok
      ok = (max_abs_err <= tol)
      status = "PASS"
      if (.not. ok) status = "FAIL"
      mcells = 0.0_wp
      if (secs > 0.0_wp) mcells = real(n, wp)*real(iters, wp)/secs/1.0e6_wp
      write (*, "(A)") "RESULT "//trim(name)//" data="//trim(data_name)// &
         " n="//itoa(n)//" iters="//itoa(iters)//" status="//trim(status)// &
         " max_abs_err="//rtoa(max_abs_err)//" mcells_per_s="//rtoa(mcells)
      if (.not. ok) error stop 1
   end subroutine dcp_report

   function itoa(i) result(s)
      integer, intent(in) :: i
      character(len=:), allocatable :: s
      character(len=32) :: buf
      write (buf, "(I0)") i
      s = trim(buf)
   end function itoa

   function rtoa(x) result(s)
      real(wp), intent(in) :: x
      character(len=:), allocatable :: s
      character(len=32) :: buf
      write (buf, "(ES13.6)") x
      s = trim(adjustl(buf))
   end function rtoa

end module dcp_harness
