ExperimentLog <- R6Class("ExperimentLog",
    public = list(
      nExperiments = 0,
      nIterations = 0,
      estimation.log = NULL,
      iteration.log = NULL,
      current.index = 0,
      total.time = 0,
      total.experiments = 0,
      
      initialize = function () {
        self$iteration.log <- list()
        self$estimation.log <- list()
        self$current.index <- 0
        self$total.time <- 0
        self$total.experiments <- 0
        self$estimation.log <- matrix(nrow = 0, ncol = 5, 
                 dimnames = list(NULL, c("iteration", "instance", "configuration", "time", "bound")))
      },
      
      addExperimentsMatrix = function (data) {
        if (ncol(data)!=4)
          irace.error("ExperimentLog adding data format is not correct.")
        index <- self$current.index
        if (index == 0) {
          self$estimation.log <- rbind(self$estimation.log,
                                  cbind(rep(0, nrow(data)), data))
        } else {
          self$iteration.log[[index]] <- rbind(self$iteration.log[[index]],
                               cbind(rep(index, nrow(data)),data)) 
        }
        self$total.time <- self$total.time + sum(data[,3])
        self$total.experiments <- self$total.experiments + nrow(data)
      },
      
      addExperiments = function (instance, configuration.id, times, bound) {
        index <- self$current.index
        if (self$current.index == 0) {
          self$estimation.log <- rbind(self$estimation.log,
                                  cbind(0, instance, configuration.id, 
                                        times, bound))
        } else {
          self$iteration.log[[index]] <- rbind(self$iteration.log[[index]],
                               cbind(index, instance, configuration.id, 
                                     times, bound))
        }    
        self$total.time <- self$total.time + sum(times)
        self$total.experiments <- self$total.experiments + length(times)
      },
      
      newIteration = function () {
        self$current.index <- self$current.index + 1
        index <- self$current.index
        self$iteration.log[[index]] <- matrix(nrow = 0, ncol = 5, 
                 dimnames = list(NULL, c("iteration", "instance", "configuration", "time", "bound")))
      },

      getByConfigurations = function (configuration.ids, cols = c("iteration", "instance", "configuration", "time", "bound")) {
        irace.assert(nrow(self$estimation.log) > 0 || self$current.index > 0)
        index <- self$current.index
        data <- NULL
        if (nrow(self$estimation.log) > 0) {
          data <- private$getExpEstimation(configuration.ids)
        }
        
        if (index > 0) {
          aux <- lapply(1:index, private$getExpIteration, ids=configuration.ids)
          data <- rbind(data, do.call("rbind", aux))
        }

        data <- data[, cols, drop=FALSE]
        return (data)
      },
      
      getIterationUsedTime = function (index = self$current.index) {
        if (index==0){
          return(sum(self$estimation.log[,"time"]))
        } else {
          irace.assert (index !=0 && index <= self$current.index)
          return(sum(self$iteration.log[[index]][,"time"]))
        }
      },
      
      getUsedTime = function (){
        return (self$total.time)
      },
      
      getBoundEstimate = function () {
        return (self$total.time/self$total.experiments)
      },
   
      getNExperimentsIteration = function (index = self$current.index) {
        irace.assert (index !=0 && index <= self$current.index)
        return(nrow(self$iteration.log[[index]]))
      },
      
      getNExperiments = function () {
        return (self$total.experiments)
      },
      
      getTimeMatrix = function (configuration.ids) {
        # get data
        experiments <- self$getByConfigurations(configuration.ids = configuration.ids) 
        experiments[, "time"] <- pmin(experiments[,"time"], experiments[, "bound"])
        # FIXME: It would be better to use spread() from tidyr
        resultsTime <- reshape(as.data.frame(experiments), direction = "wide",
                               idvar = "instance", timevar = "configuration",
                               drop = "bound")
        rownames(resultsTime) <- resultsTime$instance
        resultsTime <- resultsTime[order(resultsTime$instance), , drop = FALSE]
        colnames(resultsTime) <- substring(colnames(resultsTime), nchar("time.") + 1)
        resultsTime <- as.matrix(resultsTime[, as.character(configuration.ids), drop = FALSE])
        return(resultsTime)   
      }
    ), 
    
    private = list(
      
      getExpIteration = function (index = self$current.index, ids) {
        irace.assert (index !=0 && index <= self$current.index)
        return(self$iteration.log[[index]][self$iteration.log[[index]][,"configuration"] %in% ids,,drop=FALSE])
      },
      
      getExpEstimation = function (ids) {
        if (nrow(self$estimation.log) < 1) 
          return(NULL)
        return(self$estimation.log[self$estimation.log[,"configuration"] %in% ids,, drop=FALSE])
      }
    )
                         
)
