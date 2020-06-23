ExperimentLog <- R6Class("ExperimentLog",
    public = list(
      estimation_log = NULL,
      iteration_log = NULL,
      current_index = 0L,
      total_experiments = 0L,
      total_time = 0,
      
      initialize = function () {
        # FIXME: This is initialized already above
        self$iteration_log <- list()
        self$current_index <- 0
        self$total_time <- 0
        self$total_experiments <- 0
        self$estimation_log <- matrix(nrow = 0, ncol = 5, 
                 dimnames = list(NULL, c("iteration", "instance", "configuration", "time", "bound")))
      },
      
      addExperimentsMatrix = function (data) {
        if (ncol(data) != 4)
          irace.error("ExperimentLog adding data format is not correct.")
        index <- self$current_index
        if (index == 0) {
          self$estimation_log <- rbind(self$estimation_log,
                                  cbind(rep(0, nrow(data)), data))
        } else {
          self$iteration_log[[index]] <- rbind(self$iteration_log[[index]],
                               cbind(rep(index, nrow(data)),data)) 
        }
        # Column 3 is time
        self$total_time <- self$total_time + sum(data[,3])
        self$total_experiments <- self$total_experiments + nrow(data)
      },
      
      add_experiment = function (instance, configuration_id, time, bound) {
        index <- self$current_index
        if (self$current_index == 0) {
          self$estimation_log <- rbind(self$estimation_log,
                                  cbind(0, instance, configuration_id, 
                                        time, bound))
        } else {
          self$iteration_log[[index]] <- rbind(self$iteration_log[[index]],
                               cbind(index, instance, configuration_id, 
                                     time, bound))
        }    
        self$total_time <- self$total_time + sum(time)
        self$total_experiments <- self$total_experiments + length(time)
      },
      
      newIteration = function () {
        self$current_index <- self$current_index + 1
        index <- self$current_index
        self$iteration_log[[index]] <- matrix(nrow = 0, ncol = 5, 
                 dimnames = list(NULL, c("iteration", "instance", "configuration", "time", "bound")))
      },

      getByConfigurations = function (configuration.ids, cols = c("iteration", "instance", "configuration", "time", "bound")) {
        irace.assert(nrow(self$estimation_log) > 0 || self$current_index > 0)
        index <- self$current_index
        data <- NULL
        if (nrow(self$estimation_log) > 0) {
          data <- private$getExpEstimation(configuration.ids)
        }
        
        if (index > 0) {
          aux <- lapply(1:index, private$getExpIteration, ids=configuration.ids)
          data <- rbind(data, do.call("rbind", aux))
        }

        data <- data[, cols, drop=FALSE]
        return (data)
      },
      
      getIterationUsedTime = function (index = self$current_index) {
        if (index==0){
          return(sum(self$estimation_log[,"time"]))
        } else {
          irace.assert (index !=0 && index <= self$current_index)
          return(sum(self$iteration_log[[index]][,"time"]))
        }
      },
      
      getUsedTime = function (){
        return (self$total_time)
      },
      
      getBoundEstimate = function () {
        return (self$total_time/self$total_experiments)
      },
   
      getNExperimentsIteration = function (index = self$current_index) {
        irace.assert (index !=0 && index <= self$current_index)
        return(nrow(self$iteration_log[[index]]))
      },
      
      getNExperiments = function () {
        return (self$total_experiments)
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
      
      getExpIteration = function (index = self$current_index, ids) {
        irace.assert (index !=0 && index <= self$current_index)
        return(self$iteration_log[[index]][self$iteration_log[[index]][,"configuration"] %in% ids,,drop=FALSE])
      },
      
      getExpEstimation = function (ids) {
        if (nrow(self$estimation_log) < 1) 
          return(NULL)
        return(self$estimation_log[self$estimation_log[,"configuration"] %in% ids,, drop=FALSE])
      }
    )
                         
)
