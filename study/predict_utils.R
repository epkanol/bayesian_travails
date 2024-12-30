
generate_grid <- function(added=exp(mean(data$logADD))-1,
                          removed=exp(mean(data$logDEL))-1,
                          complexity=exp(mean(data$logCOMPLEX))-1,
                          duplicates=exp(mean(data$logDUP))-1) {
  numbers <- data.frame(A=(log(added+1)-mean(data$logADD))/sd(data$logADD),
                        R=(log(removed+1)-mean(data$logDEL))/sd(data$logDEL),
                        C=(log(complexity+1)-mean(data$logCOMPLEX))/sd(data$logCOMPLEX),
                        D=(log(duplicates+1)-mean(data$logDUP))/sd(data$logDUP),
                        added=added,
                        removed=removed,
                        complexity=complexity,
                        duplicates=duplicates)
  repos <- data.frame(repo=data$repo) |> distinct()
  teams <- data.frame(team=data$committerteam) |> distinct()
  grid <- expand_grid(numbers, repos, teams)
  return(grid)
}


posterior_predict_by_team_and_repo <- function(model,
                                              added=exp(mean(data$logADD))-1,
                                              removed=exp(mean(data$logDEL))-1,
                                              complexity=exp(mean(data$logCOMPLEX))-1,
                                              duplicates=exp(mean(data$logDUP))-1,
                                              summary=function(x) { length(which(x==0))/length(x) }) {
  items <- 10000
  grid <- generate_grid(added, removed, complexity, duplicates)
  summary <- posterior_predict(model, newdata=grid, ndraws=items, allow_new_levels=TRUE) |> data.frame() |> sapply(summary)
  grid$summary <- summary
  return(grid)
}

heatmap_by_team_and_repo <- function(postpredict, summation, decimals=2) {
  added <- postpredict |> select(added) |> distinct()
  removed <- postpredict |> select(removed) |> distinct()
  complexity <- postpredict |> select(complexity) |> distinct()
  duplicates <- postpredict |> select(duplicates) |> distinct()
  p <- postpredict |> ggplot(aes(x=team, y=repo)) +
    geom_tile(aes(fill=summary)) +
    geom_text(aes(label=round(summary, decimals)), color="white") +
    xlab("team") + ylab("repo") +
    ggtitle(paste(summation, "by team and repo"), paste0("added: ", round(added$added, 0), " removed: ", round(removed$removed, 0), " complexity: ", round(complexity$complexity, 0), " duplicates: ", round(duplicates$duplicates, 0)))
  return(p)
}

predict_for_team <- function(model, team, repo, 
                             added=exp(mean(data$logADD))-1,
                             removed=exp(mean(data$logDEL))-1,
                             complexity=exp(mean(data$logCOMPLEX))-1,
                             duplicates=exp(mean(data$logDUP))-1) {
  numbers <- data.frame(A=(log(added+1)-mean(data$logADD))/sd(data$logADD),
                        R=(log(removed+1)-mean(data$logDEL))/sd(data$logDEL),
                        C=(log(complexity+1)-mean(data$logCOMPLEX))/sd(data$logCOMPLEX),
                        D=(log(duplicates+1)-mean(data$logDUP))/sd(data$logDUP),
                        added=added,
                        removed=removed,
                        complexity=complexity,
                        duplicates=duplicates)
  grid <- expand_grid(numbers, repo, team)
  items <- 10000
#  data <- posterior_predict(model, newdata=grid, ndraws=items, allow_new_levels=TRUE) |> data.frame()
  data <- predicted_draws(model, newdata=grid, ndraws=items, allow_new_levels = TRUE ) |> data.frame()
  return(data)
}

# input is the output from predict_for_team
plot_cumulative_prob_of_duplicates <- function(predictions) {
  added <- predictions |> select(added) |> distinct()
  removed <- predictions |> select(removed) |> distinct()
  complexity <- predictions |> select(complexity) |> distinct()
  duplicates <- predictions |> select(duplicates) |> distinct()
  predictions |> mutate(predict_INTROD = .prediction) |> group_by(team, repo, predict_INTROD) |> ggplot(aes(x=predict_INTROD, color=team)) + stat_ecdf() + facet_wrap(~ repo, nrow = 2) + 
    xlab("Maximum number of introduced duplicates") +
    ggtitle("Cumulative probability of introduced duplicates",  
            paste0("added: ", round(added$added, 0), " removed: ", round(removed$removed, 0), " complexity: ", round(complexity$complexity, 0), " duplicates: ", round(duplicates$duplicates, 0))) +
    scale_color_manual(values=COLOR_BY_TEAM) + theme_bw()
}


halfeye_per_team <- function(postpredict) {
  added <- postpredict |> select(added) |> distinct()
  removed <- postpredict |> select(removed) |> distinct()
  complexity <- postpredict |> select(complexity) |> distinct()
  duplicates <- postpredict |> select(duplicates) |> distinct()
  p <- postpredict |> group_by(team, repo) |> ggplot(aes(x=.prediction, color=repo)) + 
    stat_halfeye(fill="black") + facet_wrap(~ team) +
    ggtitle(paste("Prediction by team"), paste0("added: ", round(added$added, 0), " removed: ", round(removed$removed, 0), " complexity: ", round(complexity$complexity, 0), " duplicates: ", round(duplicates$duplicates, 0)))
  
  return (p)
}

histogram_per_team <- function(postpredict) {
  added <- postpredict |> select(added) |> distinct()
  removed <- postpredict |> select(removed) |> distinct()
  complexity <- postpredict |> select(complexity) |> distinct()
  duplicates <- postpredict |> select(duplicates) |> distinct()
  p <- postpredict |> group_by(team, repo) |> ggplot(aes(x=.prediction, color=repo)) + 
    geom_histogram(binwidth = 1) + facet_wrap(~ team) +
    ggtitle(paste("Prediction by team"), paste0("added: ", round(added$added, 0), " removed: ", round(removed$removed, 0), " complexity: ", round(complexity$complexity, 0), " duplicates: ", round(duplicates$duplicates, 0)))
  
  return (p)
}

onepred <- function(added=q99(data$ADD), removed=q99(data$DEL), complexity = q95(data$COMPLEX), duplicates=q95(data$DUP), 
                    teams=c("Arch", "Blue", "Brown","Red", "Green", "Yellow", "Orange", "NewTeam"), 
                    repos=c("Venus", "Jupiter", "IntTest", "Mars", "Neptune", "Saturn", "Uranus", "Mercury")) {
  pred <- predict_for_team(m, teams, repo=repos, added, removed, complexity, duplicates)
  # assumes we know that total number of n is 10000 (hence the n/100) - should really calculate this instead...
  p0 <- pred |> group_by(added, removed, complexity, duplicates, team, repo, .prediction) |> summarize(n = n()) |> filter(.prediction == 0) |> summarize(pred0=100-(n/100)) |> ungroup() |> group_by(repo) |> mutate(rank0 = rank(pred0, ties.method = "min"))
  p5 <- pred |> group_by(added, removed, complexity, duplicates, team, repo, .prediction) |> summarize(n = n()) |> filter(.prediction < 6 ) |> summarize(pred5=100-(sum(n)/100)) |> ungroup() |> group_by(repo) |> mutate(rank5 = rank(pred5, ties.method = "min"))
  merge(p0,p5) 
}

duplicates_probability <- function(predictions) {
  params = predictions |> select(added, removed, complexity, duplicates) |> distinct()
  stopifnot(length(params$added) == 1)
  predictions |> ggplot(aes(x=team, y=pred0/100, color=team)) + geom_boxplot() + facet_wrap(~ repo, nrow=2) + scale_color_manual(values=COLOR_BY_TEAM) + theme_bw() + ggtitle("Probability of any duplicate per team and repository", paste("added", params$added, "removed", params$removed, "complexity", params$complexity, "duplicates", params$duplicates)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylab("P(INTROD > 0)")
}

more_five_probability <- function(predictions) {
  params = predictions |> select(added, removed, complexity, duplicates) |> distinct()
  stopifnot(length(params$added) == 1)
  predictions |> ggplot(aes(x=team, y=pred5/100, color=team)) + geom_boxplot() + facet_wrap(~ repo) + scale_color_manual(values=COLOR_BY_TEAM) + theme_bw() + ggtitle("Probability of more than five duplicates per team and repository", paste("added", params$added, "removed", params$removed, "complexity", params$complexity, "duplicates", params$duplicates)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylab("P(INTROD > 5)")
}

