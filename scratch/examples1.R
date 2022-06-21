mixopt_coorddesc(par=list(mopar_cts(2,8),
                          mopar_unordered(letters[1:6])),
                 fn=function(x) {ifelse(x[[2]] == 'b', -1, 0) +(4.5-x[[1]])^2})

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
                 fn=function(x) {-x[[1]]*x[[2]]}, track_par = T)
plot_track_par(m5)
ContourFunctions::cf_func(function(x) {-x[[1]]*x[[2]]}, xlim=c(0,1), ylim=c(0,1),
                          pts=matrix(unlist(m5$track_par), ncol=2, byrow=T))


# Difficult?
f6 <- function(x) {-x[[1]]*sin(x[[1]])*1+0*x[[2]]}
ContourFunctions::cf_func(f6, xlim=c(0,100), ylim=c(0,100))
m6 <- mixopt_coorddesc(par=list(mopar_cts(0,100), mopar_cts(0,100)),
                       fn=f6, track_par = T)
plot_track_par(m6)
ContourFunctions::cf_func(f6, xlim=c(0,100), ylim=c(0,100),
                          pts=matrix(unlist(m6$track_par), ncol=2, byrow=T))


# Difficult?
f7 <- function(x) {-(x[[1]]+x[[2]]) + (x[[1]] - x[[2]])^2}
ContourFunctions::cf_func(f7, xlim=c(0,100), ylim=c(0,100))
m7 <- mixopt_coorddesc(par=list(mopar_cts(0,100), mopar_cts(0,100)),
                       fn=f7, track_par = T)
plot_track_par(m7)
ContourFunctions::cf_func(f7, xlim=c(0,100), ylim=c(0,100),
                          pts=matrix(unlist(m7$track_par), ncol=2, byrow=T))

# Zig zag
f8 <- function(x) {-(x[[1]]+x[[2]]) + .1*(x[[1]] - x[[2]])^2}
ContourFunctions::cf_func(f8, xlim=c(0,100), ylim=c(0,100))
m8 <- mixopt_coorddesc(par=list(mopar_ordered(0:100), mopar_ordered(0:100)),
                       fn=f8, track_par = T)
plot_track_par(m8)
ContourFunctions::cf_func(f8, xlim=c(0,100), ylim=c(0,100),
                          pts=matrix(unlist(m8$track_par), ncol=2, byrow=T))
