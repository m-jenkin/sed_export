# A translation of Stuart Lane's diurnal discharge entropy (Lane et al 2017) code for Matlab.
# Needs commenting but running fine.

entropy_func <- function(x) {
  shannon_all <- tibble()

  for (i in unique(x$year)) {
    L_GS1 <- x %>% # get the data
      filter(year == i) %>%
      drop_na(Qw)

    # Extract time and discharge
    tim <- L_GS1$JD
    Q <- L_GS1$Qw

    # Initialise variables
    dayd <- floor(tim)
    minday <- min(dayd)
    maxday <- max(dayd)
    k <- 1
    dt <- 1
    day <- numeric()
    tmax1 <- numeric()
    tmax2 <- numeric()

    # Loop through each day
    for (i in minday:(maxday - 1)) {
      day[k] <- i

      poss <- (tim > (i + 0.4)) * (tim < (i + dt))
      poss <- poss * Q

      Bmax <- max(poss)
      if (Bmax > 0) {
        tmax1[k] <- tim[which(poss == Bmax)[1]] - i
      } else {
        tmax1[k] <- 0
      }

      k <- k + 1
    }

    n <- length(day)
    tmax1 <- tmax1 + day
    se1 <- numeric()
    dur1 <- numeric()

    for (i in 1:(n - 1)) {
      ts <- which(tim == tmax1[i])
      te <- which(tim == tmax1[i + 1])
      ql <- Q[ts:te]

      if (sum(ql) > 0) {
        se1[i] <- (1 / (te - ts)) * sum((ql / mean(ql)) * log((ql / mean(ql) + 1)))
        dur1[i] <- 100 * sum(ql > 4.5) / (te - ts)
      } else {
        se1[i] <- 0
        dur1[i] <- 0
      }
    }

    day <- day[-n]
    se1[se1 == 0] <- NA

    # Final outputs: day, se1 (Shannon entropy)
    temp <- tibble(
      day = day,
      entropy = se1,
      year = L_GS1$year[1]
    )

    shannon_all <- bind_rows(shannon_all, temp) # data into main table
    rm(temp)
  }
  return(shannon_all)
}