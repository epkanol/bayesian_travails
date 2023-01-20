condeffect_logCOMPLEX_by_logADD <- function(model, someData, aTeam, aRepo) {
  items <- length(someData[,1])
  nd <- someData |> mutate(A=0, D=0, team=aTeam, repo=aRepo, C=seq(from=-2, to=4, length.out=items))
  nd1 <- someData |> mutate(A=1, D=0, team=aTeam, repo=aRepo, C=seq(from=-2, to=4, length.out=items))
  nd2 <- someData |> mutate(A=2, D=0, team=aTeam, repo=aRepo, C=seq(from=-2, to=4, length.out=items))
  nd3 <- someData |> mutate(A=3, D=0, team=aTeam, repo=aRepo, C=seq(from=-2, to=4, length.out=items))
  f <- fitted(model, newdata=nd, probs=c(.055, .945)) |> data.frame() |> bind_cols(nd)
  f1 <- fitted(model, newdata=nd1, probs=c(.055, .945)) |> data.frame() |> bind_cols(nd1)
  f2 <- fitted(model, newdata=nd2, probs=c(.055, .945)) |> data.frame() |> bind_cols(nd2)
  f3 <- fitted(model, newdata=nd3, probs=c(.055, .945)) |> data.frame() |> bind_cols(nd3)
  f$A <- "0"
  f1$A <- "1"
  f2$A <- "2"
  f3$A <- "3"
  ftot <- rbind(f, f1, f2, f3)
  return(ftot)
}

plot_logCOMPLEX_by_logADD <- function(model, someData, ftot, aTeam, aRepo) {
  observed <- bind_cols(someData, model$criteria$loo$diagnostics) |> filter(team == aTeam, repo == aRepo) |> mutate(truncA=as.factor(round(A)))
  return(ftot |> ggplot(aes(x=C)) +
    geom_smooth(aes(y=Estimate, ymin=Q5.5, ymax=Q94.5, group=A, color=A), stat="identity", alpha=.25, linewidth=.5) +
    geom_point(data=observed, aes(y=y, size = pareto_k, color=truncA), alpha=0.2) +
      ggtitle(paste0("Conditional effects of team ", aTeam, " in repo ", aRepo))
  )
}

condeffect_logADD_by_logCOMPLEX <- function(model, someData, aTeam, aRepo) {
  items <- length(someData[,1])
  ndmin1 <- someData |> mutate(C=-1, D=0, team=aTeam, repo=aRepo, A=seq(from=-2, to=4, length.out=items))
  nd <- someData |> mutate(C=0, D=0, team=aTeam, repo=aRepo, A=seq(from=-2, to=4, length.out=items))
  nd1 <- someData |> mutate(C=1, D=0, team=aTeam, repo=aRepo, A=seq(from=-2, to=4, length.out=items))
  nd2 <- someData |> mutate(C=2, D=0, team=aTeam, repo=aRepo, A=seq(from=-2, to=4, length.out=items))
  nd3 <- someData |> mutate(C=3, D=0, team=aTeam, repo=aRepo, A=seq(from=-2, to=4, length.out=items))
  fmin1 <- fitted(model, newdata=ndmin1, probs=c(.055, .945)) |> data.frame() |> bind_cols(ndmin1)
  f <- fitted(model, newdata=nd, probs=c(.055, .945)) |> data.frame() |> bind_cols(nd)
  f1 <- fitted(model, newdata=nd1, probs=c(.055, .945)) |> data.frame() |> bind_cols(nd1)
  f2 <- fitted(model, newdata=nd2, probs=c(.055, .945)) |> data.frame() |> bind_cols(nd2)
  f3 <- fitted(model, newdata=nd3, probs=c(.055, .945)) |> data.frame() |> bind_cols(nd3)
  fmin1$C <- "-1"
  f$C <- "0"
  f1$C <- "1"
  f2$C <- "2"
  f3$C <- "3"
  ftot <- rbind(fmin1, f, f1, f2, f3)
  return(ftot)
}

plot_logADD_by_logCOMPLEX <- function(model, someData, ftot, aTeam, aRepo) {
  observed <- bind_cols(someData, model$criteria$loo$diagnostics) |> filter(team == aTeam, repo == aRepo)|> mutate(truncD=as.factor(round(D)))
  return(ftot |> ggplot(aes(x=A)) +
    geom_smooth(aes(y=Estimate, ymin=Q5.5, ymax=Q94.5, group=C, color=C), stat="identity", alpha=.25, linewidth=.5) +
    geom_point(data=observed, aes(y=y, size = pareto_k, color=truncD), alpha=0.2) +
      ggtitle(paste0("Conditional effects of team ", aTeam, " in repo ", aRepo))
  )
}

condeffect_logADD_by_logDUP <- function(model, someData, aTeam, aRepo) {
  items <- length(someData[,1])
  ndmin1 <- someData |> mutate(D=-1, C=0, team=aTeam, repo=aRepo, A=seq(from=-2, to=4, length.out=items))
  nd <- someData |> mutate(D=0, C=0, team=aTeam, repo=aRepo, A=seq(from=-2, to=4, length.out=items))
  nd1 <- someData |> mutate(D=1, C=0, team=aTeam, repo=aRepo, A=seq(from=-2, to=4, length.out=items))
  nd2 <- someData |> mutate(D=2, C=0, team=aTeam, repo=aRepo, A=seq(from=-2, to=4, length.out=items))
  nd3 <- someData |> mutate(D=3, C=0, team=aTeam, repo=aRepo, A=seq(from=-2, to=4, length.out=items))
  fmin1 <- fitted(model, newdata=ndmin1, probs=c(.055, .945)) |> data.frame() |> bind_cols(ndmin1)
  f <- fitted(model, newdata=nd, probs=c(.055, .945)) |> data.frame() |> bind_cols(nd)
  f1 <- fitted(model, newdata=nd1, probs=c(.055, .945)) |> data.frame() |> bind_cols(nd1)
  f2 <- fitted(model, newdata=nd2, probs=c(.055, .945)) |> data.frame() |> bind_cols(nd2)
  f3 <- fitted(model, newdata=nd3, probs=c(.055, .945)) |> data.frame() |> bind_cols(nd3)
  fmin1$D <- "-1"
  f$D <- "0"
  f1$D <- "1"
  f2$D <- "2"
  f3$D <- "3"
  ftot <- rbind(fmin1, f, f1, f2, f3)
  return(ftot)
}

plot_logADD_by_logDUP <- function(model, someData, ftot, aTeam, aRepo) {
  observed <- bind_cols(someData, model$criteria$loo$diagnostics) |> filter(team == aTeam, repo == aRepo) |> mutate(truncD=as.factor(round(D)))
  return(ftot |> ggplot(aes(x=A)) +
    geom_smooth(aes(y=Estimate, ymin=Q5.5, ymax=Q94.5, group=D, color=D), stat="identity", alpha=.25, linewidth=.5) +
    geom_point(data=observed, aes(y=y, size = pareto_k, color=truncD), alpha=0.2) +
      ggtitle(paste0("Conditional effects of team ", aTeam, " in repo ", aRepo))
  )
}
