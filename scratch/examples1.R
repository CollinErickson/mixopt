library(ggplot2)
library(dplyr)

f1 <- function(x) {ifelse(x[[2]] == 'b', -1, 0) +(4.5-x[[1]])^2}
curve(f1(list(x, 'b')), 2, 8)
curve(f1(list(x, 'a')), 2, 8, add=T, col=2)
mixopt_coorddesc(par=list(mopar_cts(2,8),
                          mopar_unordered(letters[1:6])),
                 fn=f1)

# X1: continuous(2,8)
# X2: discrete(a, b, c, d, e, f)
f2 <- function(x) {
  a <- if (x[[2]] %in% c('c','d','e')) {1} else {1.4}
  b <- if (x[[2]] %in% c('a','d','f')) {1} else {.5}
  c <- if (x[[2]] %in% c('a','b','c')) {4.5} else {5.5}
  d <- if (x[[2]] %in% c('b','d','f')) {2} else {1.5}
  e <- 1+if (x[[2]] %in% c('a','f')) {2} else if (x[[2]] %in% c('e','f')) {-1} else {0}
  a*abs(b*x[[1]]-c)^d + e
}
f2_lwd <- 2.5
curve(f2(list(x, 'a')), 2, 8, ylim=c(0,12), ylab="f2", lwd=f2_lwd)
curve(f2(list(x, 'b')), 2, 8, add=T, col=2, lwd=f2_lwd)
curve(f2(list(x, 'c')), 2, 8, add=T, col=3, lwd=f2_lwd)
curve(f2(list(x, 'd')), 2, 8, add=T, col=4, lwd=f2_lwd)
curve(f2(list(x, 'e')), 2, 8, add=T, col=5, lwd=f2_lwd)
curve(f2(list(x, 'f')), 2, 8, add=T, col=6, lwd=f2_lwd)
legend(x='topleft', legend=letters[1:6], fill=1:6)
mop <- mixopt_coorddesc(par=list(mopar_cts(2,8),
                          mopar_unordered(letters[1:6])),
                 fn=f2)
# Can get stuck in local min
points(mop$par[[1]], mop$val, col=which(letters==mop$par[[2]]), pch=19, cex=3)


mixopt_coorddesc(par=list(mopar_cts(2,8),
                          mopar_unordered(letters[1:6]),
                          mopar_ordered(1:10)),
                 fn=function(x) {ifelse(x[[2]] == 'b', -1, 0) +(4.5-x[[1]])^2 + x[[1]]*(log(x[[3]])-1)^2})

mixopt_coorddesc(par=list(mopar_ordered(3:100)),
                 fn=function(x) {x[[1]]})
mixopt_coorddesc(par=list(mopar_ordered(3:100)),
                 fn=function(x) {-x[[1]]})

# Just go to corner, super easy
ContourFunctions::cf_func(function(x) {-x[[1]]*x[[2]]}, xlim=c(0,1), ylim=c(0,1))
m5 <- mixopt_coorddesc(par=list(mopar_ordered(seq(0,1,l=101)), mopar_ordered(seq(0,1,l=101))),
                 fn=function(x) {-x[[1]]*x[[2]]}, track = T)
plot_track(m5)
ContourFunctions::cf_func(function(x) {-x[[1]]*x[[2]]}, xlim=c(0,1), ylim=c(0,1),
                          pts=matrix(unlist(m5$track$par), ncol=2, byrow=T))


# Difficult: local minima
f6 <- function(x) {-(-x[[1]]*.5*sin(.5*x[[1]])*1 - 1e-2*x[[2]]^2 + .2*x[[1]] - .3*x[[2]])}
ContourFunctions::cf_func(f6, xlim=c(0,100), ylim=c(-100,100))
m6 <- mixopt_coorddesc(par=list(mopar_cts(0,100), mopar_cts(-100,100)),
                       fn=f6, track = T)
plot_track(m6)
ms6 <- mixopt_multistart(par=list(mopar_cts(0,100), mopar_cts(-100,100)),
                       fn=f6, track = T)
plot_track(ms6)
ContourFunctions::cf_func(f6, xlim=c(0,100), ylim=c(-100,100),
                          # pts=matrix(unlist(m8$track$par), ncol=2, byrow=T),
                          gg = T) +
  geom_point(data=as.data.frame(matrix(unlist(ms6$track$par), ncol=2, byrow=T)) %>%
               bind_cols(newbest=ms6$track$newbest),
             aes(V1, V2, color=newbest), alpha=.5)

# Difficult?
f7 <- function(x) {-(x[[1]]+x[[2]]) + (x[[1]] - x[[2]])^2}
ContourFunctions::cf_func(f7, xlim=c(0,100), ylim=c(0,100))
m7 <- mixopt_coorddesc(par=list(mopar_cts(0,100), mopar_cts(0,100)),
                       fn=f7, track = T)
plot_track(m7)
ContourFunctions::cf_func(f7, xlim=c(0,100), ylim=c(0,100),
                          pts=matrix(unlist(m7$track$par), ncol=2, byrow=T))

# Zig zag
f8 <- function(x) {-(x[[1]]+x[[2]]) + .1*(x[[1]] - x[[2]])^2}
ContourFunctions::cf_func(f8, xlim=c(0,100), ylim=c(0,100))
m8 <- mixopt_coorddesc(par=list(mopar_ordered(0:100), mopar_ordered(0:100)),
                       fn=f8, track = T)
plot_track(m8)
ContourFunctions::cf_func(f8, xlim=c(0,100), ylim=c(0,100),
                          # pts=matrix(unlist(m8$track$par), ncol=2, byrow=T),
                          gg = T) +
  geom_point(data=as.data.frame(matrix(unlist(m8$track$par), ncol=2, byrow=T)) %>%
               bind_cols(newbest=m8$track$newbest),
             aes(V1, V2, color=newbest))

# Zig zag with bottom
f9 <- function(x) {a=2*(x[[1]]+x[[2]]); b=2*(x[[1]]-x[[2]]+40); ((a-100)^2+12*(b-40)^2)^.3}
ContourFunctions::cf_func(f9, xlim=c(-50,50), ylim=c(-50,50))
m9 <- mixopt_coorddesc(par=list(mopar_ordered((-50):50), mopar_ordered((-50):50)),
                       fn=f9, track = T)
plot_track(m9)
ContourFunctions::cf_func(f9, xlim=c(-50,50), ylim=c(-50,50),
                          # pts=matrix(unlist(m9$track$par), ncol=2, byrow=T),
                          gg = T) +
  geom_point(data=as.data.frame(matrix(unlist(m9$track$par), ncol=2, byrow=T)) %>%
               bind_cols(newbest=m9$track$newbest),
             aes(V1, V2, color=newbest))
