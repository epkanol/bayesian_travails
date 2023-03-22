
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
