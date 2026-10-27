## Release summary

* This release fixes the test failure reported in the CRAN additional checks
  for 0.3.3 under alternative BLAS (MKL and BLIS). A test compared two numeric
  vectors at the default tolerance and failed on rounding differences of about
  1e-8. It now uses a tolerance that absorbs them.
* It also contains one minor bug fix, which is why it follows 0.3.3 so soon.

## R CMD check results

0 errors | 0 warnings | 0 notes
