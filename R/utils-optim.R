# Simple central-difference Jacobian of a gradient function.
# Does 2m evaluations vs numDeriv::jacobian's ~4m (Richardson extrapolation).
fast_jacobian <- function(gr_fn, x, h = 1e-5) {
  cpp_fast_jacobian(gr_fn, x, h)
}

# Central-difference Hessian of a scalar function (no analytic gradient needed).
# Does 4m(m-1)/2 + 2m = 2m^2 evaluations of fn.
fast_hessian <- function(fn, x, h = 1e-5) {
  cpp_fast_hessian(fn, x, h)
}

# Central-difference gradient of a scalar function.
# Does 2m evaluations of fn.
fast_grad <- function(fn, x, h = 1e-5) {
  cpp_fast_grad(fn, x, h)
}
