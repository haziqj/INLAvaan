# lavaan's Demo.twolevel data set: 2500 observations clustered within 200
# groups, with separate within- and between-level measurement/structural
# models
mod <- "
  level: 1
    fw =~ y1 + y2 + y3
    fw ~ x1 + x2 + x3
  level: 2
    fb =~ y1 + y2 + y3
    fb ~ w1 + w2
"
fit <- asem(mod, lavaan::Demo.twolevel, cluster = "cluster")
