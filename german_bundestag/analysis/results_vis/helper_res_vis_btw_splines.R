#-------------------------------------------------------------------------------
# Variance and Bias in Multi-Party Election Polls: 
#   Posterior visualization helper functions 
# 
# Author: Sina Chen
#
#-------------------------------------------------------------------------------


# function to compute splines for poll data
# get_splines <- function(par, b_splines, polls) {
#   
#   splines <- rv(length = nrow(polls))
#   
#   for(i in 1:nrow(polls)){
#     b_value <- b_splines[,polls$poll_id_int[i]] * par[polls$r_id[i],, polls$k_id[i]]
#     splines[i] <- Reduce("+", b_value)
#   }
#   
#   return(splines)
# }

get_splines <- function(par, b_splines, polls) {

  splines <- rv(length = nrow(polls))

  for(i in 1:nrow(polls)){
    b_value <- b_splines[,polls$poll_id_int[i]] * par[polls$kr_id[i],]
    splines[i] <- Reduce("+", b_value)
  }

  return(splines)
}

# generate mean predictions polls
generate_predictions <- function(polls, postrv, splines, inc_inst = T){
  
  # compute unscaled party-poll estimate
  unscaled <- rv(length = nrow(polls))
  
  if(inc_inst == T){
    for(i in 1:nrow(polls)) {
      unscaled[[i]] <- (log(polls$voteshare[i]/100) + 
                          postrv$alpha[polls$r_id[i], polls$k_id[i]] + 
                          postrv$alpha2[polls$l_id[i], polls$k_id[i]] +
                          splines[i])[[1]] %>% 
        exp()
    }
  } else if(inc_inst == F){
    for(i in 1:nrow(polls)) {
      unscaled[[i]] <- (log(polls$voteshare[i]/100) + 
                          postrv$alpha[polls$r_id[i], polls$k_id[i]] + 
                          splines[i])[[1]] %>% 
        exp()
    }
  }
  
  
  # compute poll totals
  totals <- rv(length = length(unique(polls$poll_id)))
  for(i in 1:length(unique(polls$poll_id))) {
    totals[i] <- Reduce("+", unscaled[i == polls$poll_id_int])
  }
  
  # compute poll estimate
  predictions <- rv(nrow(polls))
  for(j in 1:nrow(polls)){
    predictions[j] <- unscaled[j]/totals[polls$poll_id_int[j]]
  }
  
  return(predictions)
  
}


# compute estimated bias
estimate_bias <- function(polls, postrv, splines){
  
  # compute unscaled party-poll estimate
  unscaled <- rv(length = nrow(polls))
  
  for(i in 1:nrow(polls)) {
    unscaled[[i]] <- (log(polls$voteshare[i]/100) + 
                        postrv$alpha[polls$r_id[i], polls$k_id[i]] + 
                        postrv$alpha2[polls$l_id[i], polls$k_id[i]] +
                        splines[i])[[1]] %>% exp()
  }
  
  # compute poll totals
  totals <- rv(length = length(unique(polls$poll_id)))
  for(i in 1:length(unique(polls$poll_id))) {
    totals[i] <- Reduce("+", unscaled[i == polls$poll_id_int])
  }
  
  # compute poll bias
  bias <- rv(nrow(polls))
  for(j in 1:nrow(polls)){
    bias[j] <- (unscaled[j]/totals[polls$poll_id_int[j]]) - 
      (polls$voteshare[j]/100)
  }
  
  return(bias)
  
}

# compute estimated election day bias
estimate_bias0 <- function(polls, postrv){
  
  # compute unscaled party-poll estimate
  unscaled <- rv(length = nrow(polls))
  
  for(i in 1:nrow(polls)) {
    unscaled[[i]] <- (log(polls$voteshare[i]/100) + 
                        postrv$alpha[polls$r_id[i], polls$k_id[i]] + 
                        postrv$alpha2[polls$l_id[i], polls$k_id[i]]
                      )[[1]] %>% exp()
  }
  
  # compute poll totals
  totals <- rv(length = length(unique(polls$poll_id)))
  for(i in 1:length(unique(polls$poll_id))) {
    totals[i] <- Reduce("+", unscaled[i == polls$poll_id_int])
  }
  
  # compute poll bias
  bias <- rv(nrow(polls))
  for(j in 1:nrow(polls)){
    bias[j] <- (unscaled[j]/totals[polls$poll_id_int[j]]) - 
      (polls$voteshare[j]/100)
  }
  
  return(bias)
  
}

# compute estimated institute bias
estimate_institute_bias <- function(polls, postrv, splines){
  
  # compute unscaled party-poll estimate
  unscaled <- rv(length = nrow(polls))
  for(i in 1:nrow(polls)) {
    unscaled[[i]] <- (log(polls$voteshare[i]/100) + 
                        postrv$alpha2[polls$l_id[i], polls$k_id[i]]+
                        splines[i])[[1]] %>% 
      exp()
  }
  
  # compute poll totals
  totals <- rv(length = length(unique(polls$poll_id)))
  for(i in 1:length(unique(polls$poll_id))) {
    totals[i] <- Reduce("+", unscaled[i == polls$poll_id_int])
  }
  
  # compute unscaled public opinion
  unscaled_po <- rv(length = nrow(polls))
  for(i in 1:nrow(polls)) {
    unscaled_po[[i]] <- (log(polls$voteshare[i]/100) + 
                        splines[i])[[1]] %>% 
      exp()
  }
  # compute public opinion totals
  totals_po <- rv(length = length(unique(polls$poll_id)))
  for(i in 1:length(unique(polls$poll_id))) {
    totals_po[i] <- Reduce("+", unscaled_po[i == polls$poll_id_int])
  }
  
  # compute poll bias
  bias <- rv(nrow(polls))
  for(j in 1:nrow(polls)){
    bias[j] <- (unscaled[j]/totals[polls$poll_id_int[j]]) - 
      (unscaled_po[j]/totals_po[polls$poll_id_int[j]])
  }
  
  return(bias)
  
}

