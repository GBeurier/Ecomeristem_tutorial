library(recomeristem)
library(DEoptim)
library(ggplot2)

workspace <- "D:/Workspace/Seminaire/Tutorial/"
isInit <- FALSE
estimParam <- read.csv(paste0(workspace,"estimparam.csv"), sep=";")
bounds <- as.data.frame(estimParam[,c(2,3)])
paramNames <- as.vector(estimParam[,1])

weather <- recomeristem::getMeteo_from_files(workspace)
param <- recomeristem::getParameters_from_files(workspace)
obs <- recomeristem::get_clean_obs(paste0(workspace,"vobs.txt"))

error_fn <- function(obs, res) {
  diff <- ((((obs - res)/obs)^2))
  diff <- sum((colSums(diff, na.rm=T))/(colSums(!is.na(diff))),na.rm=T)
  return(diff)
}

fitness_fn <- function(p) {
  if(!isInit) {
    recomeristem::init_simu(param, weather, obs, "sim1")
    isInit <- TRUE
  }
  res <- recomeristem::launch_simu("sim1", paramNames, p)
  error <- error_fn(obs, res)
  return(error)
}

optim <- DEoptim(fitness_fn, lower=bounds[,1], upper=bounds[,2],
                 DEoptim.control(itermax = 100))

finalParam <- data.frame(Param = paramNames, Values = optim$optim$bestmem,
                         Lower = bounds[,1], Upper = bounds[,2])

param[match(paramNames, param$Name),"Values"] <- optim$optim$bestmem
finalSim <- recomeristem::rcpp_run_from_dataframe(param, weather)
finalSim$das <- as.numeric(row.names(finalSim))
plt <- ggplot(finalSim, aes(x = das, y=biomaero2)) + geom_line() +
  geom_point(data = obs, aes(x = day, y=biomaero2))
print(plt)

bestmems <- unique(optim$member$bestmemit)
bestRes <- data.frame()
for(i in 1:nrow(bestmems)) {
  param[match(paramNames, param$Name),"Values"] <- bestmems[i,]
  bestSim <- recomeristem::rcpp_run_from_dataframe(param, weather)
  bestRes <- rbind(bestRes, data.frame(das = as.numeric(row.names(bestSim)),
                                       pht = bestSim$pht, it = i))
}

plt2 <- ggplot(bestRes, aes(x=das, y=pht, col=as.factor(it))) +
  geom_line() + geom_point(data = obs, aes(x = day, y=pht), inherit.aes = F)
print(plt2)

