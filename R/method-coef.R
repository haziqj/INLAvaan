setMethod("coef", "INLAvaan", function(object) {
  class(object) <- "lavaan"
  callNextMethod()
})
