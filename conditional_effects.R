# Note that `fitted` will return a sample of the _expected_ value ($\mu$), not incorporating any information about the variance
# The variance will be included via the `predict` function, so that is what we use now. We use 
#

condeffect_logCOMPLEX_by_logADD <- function(model, someData, aTeam, aRepo, duplicates=q50(data$DUP), removed=q50(data$DEL), robust=F) {
  items <- 2000
  scalars <- data.frame(R=scale_removed(removed),
                        D=scale_duplicates(duplicates),
                        team=aTeam,
                        repo=aRepo)
  added_points <- c(P25=q25(data$ADD), P50=q50(data$ADD), P75=q75(data$ADD), P99=q99(data$ADD), SMAX=max(data$ADD))
  complex_points <- seq(from=scale_complexity(min(data$COMPLEX)), to=scale_complexity(max(data$COMPLEX)), length.out=items)
  added <- data.frame(added=added_points, A=scale_added(added_points), added_category=names(added_points))
  complexity <- data.frame(C=complex_points, complex=unscale_complexity(complex_points))  
  grid <- expand_grid(scalars, added, complexity)
  f <- predict(model, newdata=grid, probs=c(.055, .945), allow_new_levels=T, robust=robust) |> data.frame() |> bind_cols(grid)
  return(f)
}

plot_logCOMPLEX_by_logADD <- function(model, someData, ftot) {
  scalars <- ftot |> select(R, D, team, repo) |> distinct()
  stopifnot(length(scalars[1]) == 1)
  removed <- unscale_removed(scalars$R)
  duplicates <- unscale_duplicates(scalars$D)
  aTeam <- scalars$team
  aRepo <- scalars$repo
  added_points <- c(P25=q25(data$ADD), P50=q50(data$ADD), P75=q75(data$ADD), P99=q99(data$ADD), SMAX=max(data$ADD))
  cutpoints <- append(scale_added(added_points), -Inf, after=0)
  d <- someData |> mutate(added_category=cut(d$A, cutpoints, labels=c("P25", "P50", "P75", "P99", "SMAX")))
  observed <- bind_cols(d, model$criteria$loo$diagnostics) |> filter(team == aTeam, repo == aRepo)
  return(ftot |> ggplot(aes(x=complex)) +
    geom_smooth(aes(y=Estimate, ymin=Q5.5, ymax=Q94.5, group=added_category, color=added_category), stat="identity", alpha=.25, linewidth=.5) +
    geom_point(data=observed, aes(y=y, x=unscale_complexity(C), size = pareto_k, color=added_category), alpha=0.2) +
      ggtitle(paste0("Conditional effects of team ", aTeam, " in repo ", aRepo),
              paste0("removed lines: ", removed, ", existing duplicates: ", duplicates))
  )
}

condeffect_logADD_by_logCOMPLEX <- function(model, someData, aTeam, aRepo, duplicates=q50(data$DUP), removed=q50(data$DEL), robust=F) {
  items <- 2000
  scalars <- data.frame(R=scale_removed(removed),
                        D=scale_duplicates(duplicates),
                        team=aTeam,
                        repo=aRepo)
  added_points <- seq(from=scale_added(min(data$ADD)), to=scale_added(roundUpNice(max(data$ADD))), length.out=items)
  complex_points <- c(P25=q25(data$COMPLEX), P50=q50(data$COMPLEX), P75=q75(data$COMPLEX), P99=q99(data$COMPLEX), SMAX=max(data$COMPLEX))
  added <- data.frame(A=added_points, added=unscale_added(added_points))
  complexity <- data.frame(C=scale_complexity(complex_points), complex=complex_points, complex_category=names(complex_points))  
  grid <- expand_grid(scalars, added, complexity)
  f <- predict(model, newdata=grid, probs=c(.055, .945), allow_new_levels=T, robust=robust) |> data.frame() |> bind_cols(grid)
  return(f)
}

plot_logADD_by_logCOMPLEX <- function(model, someData, ftot) {
  scalars <- ftot |> select(R, D, team, repo) |> distinct()
  stopifnot(length(scalars[1]) == 1)
  removed <- unscale_removed(scalars$R)
  duplicates <- unscale_duplicates(scalars$D)
  aTeam <- scalars$team
  aRepo <- scalars$repo
  complex_points <- c(P25=q25(data$COMPLEX), P50=q50(data$COMPLEX), P75=q75(data$COMPLEX), P99=q99(data$COMPLEX), SMAX=max(data$COMPLEX))
  cutpoints <- append(scale_complexity(complex_points), -Inf, after=0)
  d <- someData |> mutate(complex_category=cut(d$C, cutpoints, labels=c("P25", "P50", "P75", "P99", "SMAX")))
  observed <- bind_cols(d, model$criteria$loo$diagnostics) |> filter(team == aTeam, repo == aRepo)
  return(ftot |> ggplot(aes(x=added)) +
           geom_smooth(aes(y=Estimate, ymin=Q5.5, ymax=Q94.5, group=complex_category, color=complex_category), stat="identity", alpha=.25, linewidth=.5) +
           geom_point(data=observed, aes(y=y, x=unscale_added(A), size = pareto_k, color=complex_category), alpha=0.2) +
           ggtitle(paste0("Conditional effects of team ", aTeam, " in repo ", aRepo),
                   paste0("removed lines: ", removed, ", existing duplicates: ", duplicates)))
}

condeffect_logADD_by_logDUP <- function(model, someData, aTeam, aRepo, complexity=q50(data$COMPLEX), removed=q50(data$DEL), robust=F) {
  items <- 2000
  scalars <- data.frame(R=scale_removed(removed),
                        C=scale_complexity(complexity),
                        team=aTeam,
                        repo=aRepo)
  added_points <- seq(from=scale_added(min(data$ADD)), to=scale_added(roundUpNice(max(data$ADD))), length.out=items)
  dup_points <- c(P50=q50(data$DUP), P75=q75(data$DUP), P99=q99(data$DUP), SMAX=max(data$DUP))
  added <- data.frame(A=added_points, added=unscale_added(added_points))
  duplicates <- data.frame(D=scale_duplicates(dup_points), duplicates=dup_points, duplicates_category=names(dup_points))
  grid <- expand_grid(scalars, added, duplicates)
  f <- predict(model, newdata=grid, probs=c(.055, .945), allow_new_levels=T, robust=robust) |> data.frame() |> bind_cols(grid)
  return(f)
}

plot_logADD_by_logDUP <- function(model, someData, ftot) {
  scalars <- ftot |> select(R, C, team, repo) |> distinct()
  stopifnot(length(scalars[1]) == 1)
  removed <- unscale_removed(scalars$R)
  complex <- unscale_complexity(scalars$C)
  aTeam <- scalars$team
  aRepo <- scalars$repo
  dup_points <- c(P50=q50(data$DUP), P75=q75(data$DUP), P99=q99(data$DUP), SMAX=max(data$DUP))
  cutpoints <- append(scale_duplicates(dup_points), -Inf, after=0)
  d <- someData |> mutate(duplicates_category=cut(d$D, cutpoints, labels=c("P50", "P75", "P99", "SMAX")))
  observed <- bind_cols(d, model$criteria$loo$diagnostics) |> filter(team == aTeam, repo == aRepo)
  return(ftot |> ggplot(aes(x=added)) +
           geom_smooth(aes(y=Estimate, ymin=Q5.5, ymax=Q94.5, group=duplicates_category, color=duplicates_category), stat="identity", alpha=.25, linewidth=.5) +
           geom_point(data=observed, aes(y=y, x=unscale_added(A), size = pareto_k, color=duplicates_category), alpha=0.2) +
           ggtitle(paste0("Conditional effects of team ", aTeam, " in repo ", aRepo),
                   paste0("removed lines: ", removed, ", complexity: ", complex)))
}

condeffect_logADD_by_logREMOVED <- function(model, someData, aTeam, aRepo, duplicates=q50(data$DUP), complexity=q50(data$COMPLEX), robust=F) {
  items <- 2000
  scalars <- data.frame(D=scale_duplicates(duplicates),
                        C=scale_complexity(complexity),
                        team=aTeam,
                        repo=aRepo)
  added_points <- seq(from=scale_added(min(data$ADD)), to=scale_added(roundUpNice(max(data$ADD))), length.out=items)
  removed_points <- c(P25=q25(data$DEL), P50=q50(data$DEL), P75=q75(data$DEL), P99=q99(data$DEL), SMAX=max(data$DEL))
  added <- data.frame(A=added_points, added=unscale_added(added_points))
  removed <- data.frame(R=scale_removed(removed_points), removed=removed_points, removed_category=names(removed_points))
  grid <- expand_grid(scalars, added, removed)
  f <- predict(model, newdata=grid, probs=c(.055, .945), allow_new_levels=T, robust=robust) |> data.frame() |> bind_cols(grid)
}

plot_logADD_by_logREMOVED <- function(model, someData, ftot, aTeam, aRepo) {
  scalars <- ftot |> select(D, C, team, repo) |> distinct()
  stopifnot(length(scalars[1]) == 1)
  duplicates <- unscale_duplicates(scalars$D)
  complex <- unscale_complexity(scalars$C)
  aTeam <- scalars$team
  aRepo <- scalars$repo
  removed_points <- c(P25=q25(data$DEL), P50=q50(data$DEL), P75=q75(data$DEL), P99=q99(data$DEL), SMAX=max(data$DEL))
  cutpoints <- append(scale_removed(removed_points), -Inf, after=0)
  d <- someData |> mutate(removed_category=cut(d$R, cutpoints, labels=names(removed_points)))
  observed <- bind_cols(d, model$criteria$loo$diagnostics) |> filter(team == aTeam, repo == aRepo)
  return(ftot |> ggplot(aes(x=added)) +
           geom_smooth(aes(y=Estimate, ymin=Q5.5, ymax=Q94.5, group=removed_category, color=removed_category), stat="identity", alpha=.25, linewidth=.5) +
           geom_point(data=observed, aes(y=y, x=unscale_added(A), size = pareto_k, color=removed_category), alpha=0.2) +
           ggtitle(paste0("Conditional effects of team ", aTeam, " in repo ", aRepo),
                   paste0("existing duplicates: ", duplicates, ", complexity: ", complex)))
}


## Not converted below...
condeffect_logCOMPLEX_by_logREMOVED <- function(model, someData, aTeam, aRepo) {
  items <- 10000
  nd <- data.frame(R=0, A=0, D=0, team=aTeam, repo=aRepo, C=seq(from=-2, to=4, length.out=items))
  nd1 <- data.frame(R=1, A=0, D=0, team=aTeam, repo=aRepo, C=seq(from=-2, to=4, length.out=items))
  nd2 <- data.frame(R=2, A=0, D=0, team=aTeam, repo=aRepo, C=seq(from=-2, to=4, length.out=items))
  nd3 <- data.frame(R=3, A=0, D=0, team=aTeam, repo=aRepo, C=seq(from=-2, to=4, length.out=items))
  f <- predict(model, newdata=nd, probs=c(.055, .945)) |> data.frame() |> bind_cols(nd)
  f1 <- predict(model, newdata=nd1, probs=c(.055, .945)) |> data.frame() |> bind_cols(nd1)
  f2 <- predict(model, newdata=nd2, probs=c(.055, .945)) |> data.frame() |> bind_cols(nd2)
  f3 <- predict(model, newdata=nd3, probs=c(.055, .945)) |> data.frame() |> bind_cols(nd3)
  f$R <- "0"
  f1$R <- "1"
  f2$R <- "2"
  f3$R <- "3"
  ftot <- rbind(f, f1, f2, f3)
  return(ftot)
}

plot_logCOMPLEX_by_logREMOVED <- function(model, someData, ftot, aTeam, aRepo) {
  observed <- bind_cols(someData, model$criteria$loo$diagnostics) |> filter(team == aTeam, repo == aRepo) |> mutate(truncR=as.factor(round(R)))
  return(ftot |> ggplot(aes(x=C)) +
           geom_smooth(aes(y=Estimate, ymin=Q5.5, ymax=Q94.5, group=R, color=R), stat="identity", alpha=.25, linewidth=.5) +
           geom_point(data=observed, aes(y=y, size = pareto_k, color=truncR), alpha=0.2) +
           ggtitle(paste0("Conditional effects of team ", aTeam, " in repo ", aRepo))
  )
}

condeffect_logREMOVED_by_logCOMPLEX <- function(model, someData, aTeam, aRepo) {
  items <- 10000
  ndmin1 <- data.frame(C=-1, A=0, D=0, team=aTeam, repo=aRepo, R=seq(from=-2, to=4, length.out=items))
  nd <- data.frame(C=0, A=0, D=0, team=aTeam, repo=aRepo, R=seq(from=-2, to=4, length.out=items))
  nd1 <- data.frame(C=1, A=0, D=0, team=aTeam, repo=aRepo, R=seq(from=-2, to=4, length.out=items))
  nd2 <- data.frame(C=2, A=0, D=0, team=aTeam, repo=aRepo, R=seq(from=-2, to=4, length.out=items))
  nd3 <- data.frame(C=3, A=0, D=0, team=aTeam, repo=aRepo, R=seq(from=-2, to=4, length.out=items))
  fmin1 <- predict(model, newdata=ndmin1, probs=c(.055, .945)) |> data.frame() |> bind_cols(ndmin1)
  f <- predict(model, newdata=nd, probs=c(.055, .945)) |> data.frame() |> bind_cols(nd)
  f1 <- predict(model, newdata=nd1, probs=c(.055, .945)) |> data.frame() |> bind_cols(nd1)
  f2 <- predict(model, newdata=nd2, probs=c(.055, .945)) |> data.frame() |> bind_cols(nd2)
  f3 <- predict(model, newdata=nd3, probs=c(.055, .945)) |> data.frame() |> bind_cols(nd3)
  fmin1$C <- "-1"
  f$C <- "0"
  f1$C <- "1"
  f2$C <- "2"
  f3$C <- "3"
  ftot <- rbind(fmin1, f, f1, f2, f3)
  return(ftot)
}

plot_logREMOVED_by_logCOMPLEX <- function(model, someData, ftot, aTeam, aRepo) {
  observed <- bind_cols(someData, model$criteria$loo$diagnostics) |> filter(team == aTeam, repo == aRepo)|> mutate(truncD=as.factor(round(D)))
  return(ftot |> ggplot(aes(x=R)) +
           geom_smooth(aes(y=Estimate, ymin=Q5.5, ymax=Q94.5, group=C, color=C), stat="identity", alpha=.25, linewidth=.5) +
           geom_point(data=observed, aes(y=y, size = pareto_k, color=truncD), alpha=0.2) +
           ggtitle(paste0("Conditional effects of team ", aTeam, " in repo ", aRepo))
  )
}

condeffect_logREMOVED_by_logDUP <- function(model, someData, aTeam, aRepo) {
  items <- 10000
  ndmin1 <- data.frame(D=-1, A=0, C=0, team=aTeam, repo=aRepo, R=seq(from=-2, to=4, length.out=items))
  nd <- data.frame(D=0, A=0, C=0, team=aTeam, repo=aRepo, R=seq(from=-2, to=4, length.out=items))
  nd1 <- data.frame(D=1, A=0, C=0, team=aTeam, repo=aRepo, R=seq(from=-2, to=4, length.out=items))
  nd2 <- data.frame(D=2, A=0, C=0, team=aTeam, repo=aRepo, R=seq(from=-2, to=4, length.out=items))
  nd3 <- data.frame(D=3, A=0, C=0, team=aTeam, repo=aRepo, R=seq(from=-2, to=4, length.out=items))
  fmin1 <- predict(model, newdata=ndmin1, probs=c(.055, .945)) |> data.frame() |> bind_cols(ndmin1)
  f <- predict(model, newdata=nd, probs=c(.055, .945)) |> data.frame() |> bind_cols(nd)
  f1 <- predict(model, newdata=nd1, probs=c(.055, .945)) |> data.frame() |> bind_cols(nd1)
  f2 <- predict(model, newdata=nd2, probs=c(.055, .945)) |> data.frame() |> bind_cols(nd2)
  f3 <- predict(model, newdata=nd3, probs=c(.055, .945)) |> data.frame() |> bind_cols(nd3)
  fmin1$D <- "-1"
  f$D <- "0"
  f1$D <- "1"
  f2$D <- "2"
  f3$D <- "3"
  ftot <- rbind(fmin1, f, f1, f2, f3)
  return(ftot)
}

plot_logREMOVED_by_logDUP <- function(model, someData, ftot, aTeam, aRepo) {
  observed <- bind_cols(someData, model$criteria$loo$diagnostics) |> filter(team == aTeam, repo == aRepo) |> mutate(truncD=as.factor(round(D)))
  return(ftot |> ggplot(aes(x=R)) +
           geom_smooth(aes(y=Estimate, ymin=Q5.5, ymax=Q94.5, group=D, color=D), stat="identity", alpha=.25, linewidth=.5) +
           geom_point(data=observed, aes(y=y, size = pareto_k, color=truncD), alpha=0.2) +
           ggtitle(paste0("Conditional effects of team ", aTeam, " in repo ", aRepo))
  )
}

