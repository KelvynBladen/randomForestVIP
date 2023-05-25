

# # make parallel
#
# # cl<-parallel::makePSOCKcluster(5)
# #
# # doParallel::registerDoParallel(cl)
# #
# # start.time<-proc.time()
# #
# # model<-train(target~., data=trainingset, method='rf')
# #
# # stop.time<-proc.time()
# #
# # run.time<-stop.time -start.time
# #
# # print(run.time)
# #
# # stopCluster(cl)
#
# library(foreach)
# library("randomForest")
# data(iris)
#
# rf <- foreach(ntree=rep(500, 1), .combine=randomForest::combine,
#               .multicombine=TRUE, .packages='randomForest') %dopar% {
#                 randomForest(Species ~ ., iris, ntree=ntree)
#               }
#
# rf <- foreach(mtry = 1:4, .combine=randomForest::combine,
#               .multicombine=TRUE, .packages='randomForest') %dopar% {
#                 randomForest(Species ~ ., iris, mtry = mtry)
#               }
#
# # registerDoMC(4) #number of cores on the machine
# # darkAndScaryForest <- foreach(y=seq(10), .combine=combine ) %dopar% {
# #   rf <- randomForest(Species ~ ., iris, norm.votes=FALSE)
# # }
#
# bag_forest <- function(formula, data = NULL, cores = 6){
#   mf <- model.frame(formula, data = data)
#   m <- ncol(mf) - 1
#
#   cl <- parallel::makeCluster(cores)
#
#   parallel::clusterEvalQ(cl, library("rfvip"))
#
#   parallel::clusterExport(cl,
#                           varlist = c("formula", "data"),
#                           envir = environment()
#   )
#
#
#   start.time<-proc.time()
#
#   model <- parallel::parLapply(cl, seq_len(1), function(x) {
#     randomForest::randomForest(formula = formula, mtry = m, data = data)
#   })
#
#   # model <- foreach(y=seq(1), .combine=combine ) %dopar% {
#   #   rf <- randomForest::randomForest(formula = formula, mtry = m, data = data)
#   # }
#
#   #model <- randomForest(formula = formula, mtry = m, data = data)
#
#   stop.time<-proc.time()
#
#   on.exit(parallel::stopCluster(cl))
#
#   run.time<-stop.time -start.time
#
#   print(run.time)
# }
#
# bag_forest <- function(formula, data = NULL){
#   if (any(grepl("\\.$", formula))){
#     m <- ncol(data) - 1
#   } else {
#     m <- length(attr(terms(formula), "term.labels"))
#   }
#
#   start.time<-proc.time()
#
#   model <- randomForest(formula = formula, mtry = m, data = data)
#
#   stop.time<-proc.time()
#
#   run.time<-stop.time -start.time
#
#   print(run.time)
# }
#
# move_cars_par <- function(rho = 0.3, r = 10, c = 15, p = 0.5, trials = 10,
#                           replicates = 4, cores = 2) {
#   cl <- parallel::makeCluster(cores)
#
#   parallel::clusterEvalQ(cl, library("carsimr.bladen"))
#
#   parallel::clusterExport(cl,
#                           varlist = c(
#                             "rho", "r", "c", "p", "trials"
#                           ),
#                           envir = environment()
#   )
#
#   moves <- parallel::parLapply(cl, seq_len(replicates), function(x) {
#     move_cars(NULL, rho, r, c, p, trials)
#   })
#
#   on.exit(parallel::stopCluster(cl))
#   moves
# }
#
# dim(iris)
# dim(mtcars)
# data("diamonds")
# dim(diamonds[1:10000,])
# library(ggplot2)
# library(randomForest)
# r <- randomForest(price ~ ., data = diamonds)
# s <- bag_forest(price ~ ., data = diamonds[1:10000,])
#
# f <- bag_forest(factor(Species)~ ., data = iris)
# f
#
# f1 <- randomForest(factor(Species)~., data = iris)
# f1


