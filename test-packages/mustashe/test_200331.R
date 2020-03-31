# Test R Packages : mustashe
# https://www.r-bloggers.com/mustashe/

setwd("D:/Analysis/R/study_r/test-packages/mustashe")

# install.packages("mustashe")
library(mustashe)

# Basic example -----------------------------------------------------------
# install.packages("tictoc")
library(tictoc)

# case1 : basic usage
tic("long-running computation")
stash("x", {
  Sys.sleep(5)
  x <- 5
})
toc()
# long-running computation: 5.3 sec elapsed

tic("long-running computation")
stash("x", {
  Sys.sleep(5)
  x <- 5
})
toc()
# long-running computation: 0.91 sec elapsed

# case2 : update
tic()
stash("a", {
  Sys.sleep(3)
  a <- runif(5)
})
toc()
# Stashing object.
# 4.59 sec elapsed

tic()
stash("a", {
  Sys.sleep(3)
  a <- runif(10)
})
toc()
# Updating stash.
# 3.41 sec elapsed

# case3 : code from horrible person
stash("a", {
  Sys.sleep(3)
  # Here is a new comment.
  a <- runif(10)
})
a

# styler: off
stash("a", {
  Sys.sleep( 3 )
  # Here is a comment.
  a=runif( 10 ) # Another comment
})

# case4 :
x <- 5
stash("y", depends_on = "x", {
  y <- x + 1
})
y


# case5 :
unstash("a")
unstash(c("a", "y"))
clear_stash()
