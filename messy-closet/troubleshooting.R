
#species.numbers <- c(10,11)
species.numbers <- 10
#sapply(species.numbers, function(i){data.extract(extract.num=c(1,2,3,5,6,7), spec.num=i)})
source(file.path(main.path, "figures.R"))
sapply(species.numbers, function(i){figures(spec.num=i, fig=c(11))})
sapply(species.numbers, function(i){figures(spec.num=i, fig=c(2,3,4,5,6,7,8,9,12,13,14,15,17,18,19,20))})
# sapply(species.numbers, function(i){figures(spec.num=i, fig=c(2,3,4,5,6,7,8,9,12,13,14,15,17,18,19,20))})
# species.numbers <- 2550
# sapply(species.numbers, function(i){figures(spec.num=i, fig=c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,17,18,19,20))})
# sapply(species.numbers, function(i){figures(spec.num=i, fig=c(16,21,22))}) # 
# sapply(species.numbers, function(i){figures(spec.num=i, fig=c(21))})
# sapply(species.numbers, function(i){figures(spec.num=i, fig=c(22))})

#species.numbers <- 2511
#sapply(species.numbers, function(i){figures(spec.num=i, fig=c(21, 22))})
