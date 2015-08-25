# helpers.R
# Tim Finney, July 2015
# Helpers for mkmss.

# Classes

Text <- setRefClass(
  "Text",
  fields = list(
    chh = "numeric", # characters (i.e. sites where variation might occur)
    events = "list", # events affecting this text and its ancestors
    level = "numeric", # reporting level
    name = "character", # identifier (e.g. Re.2.2)
    n.chh = "numeric", # no. of characters
    n.trend = "numeric", # strength of local bias
    p.change = "numeric" # P(change) per character per generation
  ),
  methods = list(
    do.corr = function(p.n, n.g, ex) {
      "Do correction
        * p.n = place name
        * n.g = which generation copying event occurred
        * ex = exemplar
        * uniform distn used to choose characters to correct 
        * chosen characters are copied from exemplar
      "
      select <- sample(n.chh, sample(n.chh, 1))
      chh[select] <<- ex$chh[select]
      report(level, NULL, NULL, sprintf(
        "%s corrected against %s (%d places)", name, ex$name, length(select)
      ))
      .self$event(sprintf(
        "corrected against %s at %s (gen. %d)", ex$name, p.n, n.g
      ))
      .self
    },
    do.cp = function(p.n, n.g, n, stt) {
      "Do copy
        * p.n = place name
        * n.g = which generation copying event occurred
        * n = sequence number
        * stt = states ordered by local preference
        * P(change) and uniform distn used to choose characters to change
      "
      cp <- .self$copy()
      cp$mk.name(p.n, n.g, n)
      cp$event(sprintf("copied from %s at %s", .self$name, p.n))
      # change states of selected characters
      ii <- which(runif(n.chh) < p.change)
      lapply(ii, function(i) {
        st.old <- cp$chh[i]
        options <- stt[[i]]
        st.new <- sample(options[options!=st.old], 1)
        cp$chh[i] <- st.new
      })
      report(level, NULL, NULL, sprintf(
        "%s copied from %s (%d changes)", cp$name, .self$name, length(ii)
      ))
      cp
    },
    do.edit = function(p.n, n.g, stt) {
      "Do edition
      * p.n = place name
      * n.g = which generation copying event occurred
      * stt = states ordered by local preference
      * uniform distn used to choose characters to edit 
      * Zipf distn used to choose among locally preferred states
      * Zipf distn exponent (s = n.trend) determines strength of local bias
      "
      select <- sort(sample(n.chh, sample(n.chh, 1)))
      chh[select] <<- sapply(select, function(s) {
        prefs <- unlist(stt[s])
        prefs[rzipf(1, length(prefs), n.trend)]
      })
      report(level, NULL, NULL, sprintf(
        "%s edited (%d places)", .self$name, length(select)
      ))
      .self$event(sprintf(
        "edited at %s (gen. %d)", p.n, n.g
      ))
      .self
    },
    do.loss = function(p.n, n.g) {
      "Do loss:
      * p.n = place name
      * n.g = which generation copying event occurred
      * report loss of a text
      "
      report(level, NULL, NULL, sprintf(
        "%s lost", .self$name
      ))
      .self$event(sprintf(
        "lost at %s (gen. %d)", p.n, n.g
      ))
    },
    event = function(ev) {
      events <<- c(events, ev)
    },
    initialize = function(..., name = "Init", n.chh = 10) {
      "Initialize text"
      # pass on initialized values
      callSuper(..., name = name, n.chh = n.chh, n.trend = n.trend,
                p.change = p.change, level = level)
      # set characters
      chh <<- rep(1, n.chh)
    },
    mk.name = function(p.n, n.g, n) {
      "Make name for this text
        * p.n = place name
        * n.g = which generation labelling event occurred
        * n = sequence number
        String is contracted to make first element of name (e.g. Rome -> Re).
      "
      name <<- sprintf(
        "%s.%s.%s",
        gsub("^(.).+(.)$", "\\1\\2", p.n), n.g, n
      )
    },
    print = function() {
      cat(
        sprintf("%s:", name),
        sprintf("characters: %s", toString(chh)),
        sprintf("events: %s", toString(unlist(events))),
        sep="\n"
      )
    }
  )
)

Place <- setRefClass(
  "Place",
  fields = list(
    level = "numeric", # reporting level
    name = "character", # name of place
    n.cap = "numeric", # carrying capacity (for extant texts)
    nn.stt = "numeric", # nos. of states per character
    n.dem = "numeric", # current demand (for extant texts)
    n.grow = "numeric", # growth rate per generation
    n.lat = "numeric", # latitude (radians)
    n.lon = "numeric", # longitude (radians)
    n.rec = "numeric", # no. of texts to recover from this place
    p.corr = "numeric", # P(correction) per text per generation
    p.edit = "numeric", # P(edition) per text per generation
    p.loss = "numeric", # P(loss) per text per generation
    stt = "list", # states ordered by local preference
    tt.extant = "list", # collection of extant texts
    tt.lost = "list" # collection of lost texts
  ),
  methods = list(
    grow = function() {
      "Grow demand using logistic growth"
      n.dem.new <- n.dem + floor(n.dem * n.grow * (1 - n.dem / n.cap))
      report(level, NULL, sprintf(
        "demand increased from %d to %d", n.dem, n.dem.new
      ))
      n.dem <<- n.dem.new
    },
    initialize = function(...) {
      "Initialize place"
      # pass on initialized values
      callSuper(
        ..., n.grow = n.grow, p.corr = p.corr, p.edit = p.edit,
        p.loss = p.loss, level = level
      )
      # set local preferences
      stt <<- lapply(nn.stt, function(n) { sample(n) })
    },
    tt.corr = function(n.g) {
      "
      Correct local texts against local exemplars:
        * n.g = generation no.
        * P(correction) used to choose no. of texts to correct 
        * uniform distn used to choose texts to correct 
        * Zipf distn (s = 1) used to choose exemplars
        * uniform distn used to choose characters to correct 
        * chosen characters are copied from exemplar
      "
      n.extant <- length(tt.extant)
      n.corr <- rbinom(1, n.extant, p.corr)
      if (n.extant > 0 && n.corr > 0) {
        select <- sample(n.extant, n.corr)
        tt.extant[select] <<- lapply(tt.extant[select], function(t) {
          t$do.corr(name, n.g, tt.extant[[rzipf(1, n.corr, 1)]])
        })
      }
    },
    tt.cp = function(n.g) {
      "
      Copy local texts and add to extant collection:
        * n.g = generation no.
        * the states of a selection of characters are changed
      "
      n.extant <- length(tt.extant)
      n.cpp <- n.dem - n.extant
      # can't make copies unless there is at least one exemplar
      if (n.extant > 0 && n.cpp > 0) {
        select <- rzipf(n.cpp, n.extant, 1)
        exemplars <- tt.extant[select]
        # use mapply to include copy's sequence number
        cpp <- mapply(
          function(ex, n) { ex$do.cp(name, n.g, n, stt) },
          exemplars,
          1:n.cpp
        )
        tt.put(cpp, "end")
      }
    },
    tt.edit = function(n.g) {
      "
      Edit local texts using local preferences:
      * n.g = generation no.
      * P(edition) used to choose no. of texts to edit 
      * uniform distn used to choose texts to edit 
      * uniform distn used to choose characters to edit 
      * Zipf distn used to choose among locally preferred states
      "
      n.extant <- length(tt.extant)
      n.edit <- rbinom(1, n.extant, p.edit)
      if (n.extant > 0 && n.edit > 0) {
        select <- sample(n.extant, n.edit)
        tt.extant[select] <<- lapply(tt.extant[select], function(t) {
          t$do.edit(name, n.g, stt)
        })
      }
    },
    tt.lose = function(n.g) {
      "
      Lose texts:
      * n.g = generation no.
      * P(loss) used to choose no. of texts to lose 
      * uniform distn used to choose texts to lose 
      * lost texts moved from tt.extant to tt.lost
      "
      n.extant <- length(tt.extant)
      n.lose <- rbinom(1, n.extant, p.loss)
      tt <- .self$tt.take(n.lose)
      lapply(tt, function(t) { t$do.loss(name, n.g) })
      tt.lost <<- c(tt.lost, tt)
    },
    tt.put = function(tt, loc) {
      "
      Put texts into extant collection:
        * tt = texts to add
        * loc = end or spread
        * end: add texts at end
        * spread : add texts throughout
      "
      if (loc == "end") {
        tt.extant <<- c(tt.extant, tt)
      } else if (loc == "spread") {
        n.extant <- length(tt.extant)
        n.put <- length(tt)
        n.new <- n.extant + n.put
        select <- logical(n.new)
        select[sample(n.new, n.put)] <- TRUE
        tt.new <- list()
        tt.new[select] <- tt
        tt.new[!select] <- tt.extant
        tt.extant <<- tt.new
      } else stop("tt.put: invalid loc value")
    },
    tt.recover = function(n.ratio) {
      "Recover texts"
      nl <- floor(n.rec * n.ratio / (1 + n.ratio))
      ne <- n.rec - nl
      rec.e <- if (length(tt.extant) > ne) sample(tt.extant, ne) else tt.extant
      rec.l <- if (length(tt.lost) > nl) sample(tt.lost, nl) else tt.lost
      report(level, NULL, sprintf(
        "recover %d extant and %d lost texts from %s",
        length(rec.e), length(rec.l), name
      ))
      c(rec.e, rec.l)
    },
    tt.take = function(n.take) {
      "
      Take texts from extant collection
        * only act if n.take < n.extant
      "
      n.extant <- length(tt.extant)
      if (n.take < n.extant) {
        report(level, NULL, sprintf(
          "take %d of %d texts from %s", n.take, n.extant, name
        ))
        select <- logical(n.extant)
        select[sample(n.extant, n.take)] <- TRUE
        take <- tt.extant[select]
        tt.extant <<- tt.extant[!select]
        take
      }
    }
  )
)

Domain <- setRefClass(
  "Domain",
  fields = list(
    level = "numeric", # reporting level
    nn.stt = "numeric", # nos. of states per character
    n.chh = "numeric", # no. of characters per text
    n.gg = "numeric", # no. of generations per simulation
    n.grow = "numeric", # growth rate per generation
    n.ratio = "numeric", # ratio of lost/extant texts recovered
    n.seed = "numeric", # seed for random number generator
    n.trend = "numeric", # strength of local bias
    pll = "list", # places constituting the domain
    pll.ranked = "list", # places ranked by distance from each place
    p.change = "numeric", # P(change) per character per text
    p.corr = "numeric", # P(correction) per text per generation
    p.edit = "numeric", # P(edition) per text per generation
    p.import = "numeric", # P(import) per unit of demand
    p.loss = "numeric", # P(loss) per text per generation
    t.init = "Text", # initial text
    tt.rec = "list" # recovered texts
  ),
  methods = list(
    initialize = function(...) {
      "Initialize domain"
      # pass on initialized values
      callSuper(
        ..., n.chh = n.chh, n.gg = n.gg, n.grow = n.grow, n.ratio = n.ratio,
        n.seed = n.seed, n.trend = n.trend, p.change = p.change,
        p.corr = p.corr, p.edit = p.edit, p.import = p.import, p.loss = p.loss,
        pll = pll, level = level
      )
      # set pll.ranked
      names <- lapply(pll, function(p) { p$name })
      lats <- as.numeric(lapply(pll, function(p) { pi * p$n.lat / 180 }))
      lons <- as.numeric(lapply(pll, function(p) { pi * p$n.lon / 180 }))
      outer1 <- outer(lats, lats, function(x, y) { sin(x) * sin(y) })
      outer2 <- outer(lats, lats, function(x, y) { cos(x) * cos(y) })
      outer3 <- outer(lons, lons, function(x, y) { cos(x - y) })
      angle <- acos(outer1 + outer2 * outer3)
      mx <- round(6371 * angle, digits = 1)
      dimnames(mx) <- list(names, names)
      pll.ranked <<- lapply(pll, function(pl) {
        sort(mx[pl$name,])[2:length(pll)]
      })
      # set random number generator seed
      set.seed(n.seed)
      # set nos. of states per character using negative binom distn
      # See M. Spencer et al. JThBiol 227 (2004) 503-11.
      # rnbinom(N, 7, 0.865) is not a bad fit to UBS4 data sets.
      nn.stt <<- rnbinom(n.chh, 7, 0.865) + 2
      # do additional initializion for each place
      lapply(pll, function(pl) {pl$initialize(
        nn.stt = nn.stt, n.grow = n.grow, p.corr = p.corr, p.edit = p.edit,
        p.loss = p.loss, level = level
      )})
      # initialize initial text
      t.init <<- Text$new(
        n.chh = n.chh, n.trend = n.trend, p.change = p.change,
        level = level
      )
    },
    propagate = function() {
      "
      Propagate texts:
        * for each generation and each place:
          * import, copy, correct, edit, and lose texts then grow demand.
      "
      report(level, "Do propagation...")
      # cycle through each generation
      lapply(1:n.gg, function(n.g) {
        report(level, sprintf("Do generation %d...", n.g))
        # cycle through each place
        lapply(pll, function(pl) {
          report(level, sprintf("Do %s...", pl$name))
          # cycle through each operation
          # import texts
          report(level, "import texts")
          tt.import(pl, n.g)
          # copy texts
          report(level, "copy texts")
          pl$tt.cp(n.g)
          # correct texts
          report(level, "correct texts")
          pl$tt.corr(n.g)
          # edit texts
          report(level, "edit texts")
          pl$tt.edit(n.g)
          # lose texts
          report(level, "lose texts")
          pl$tt.lose(n.g)
          # grow demand
          report(level, "grow demand")
          pl$grow()
        })
      })
    },
    publish = function(places) {
      "
      Publish initial text at specified places
        * places = places where initial text is published
      "
      report(level, "Do publication...")
      lapply(places, function(pl) {
        cp <- t.init$copy()
        # this happens before first generation cycle so n.g = 0
        cp$mk.name(pl$name, 0, 1)
        cp$event(sprintf("published at %s", pl$name))
        pl$tt.put(cp, "end")
      })
    },
    recover = function() {
      "Recover texts from each place"
      report(level, "Do recovery...")
      tt.rec <<- unlist(c(
        t.init,
        lapply(pll, function(pl) { pl$tt.recover(n.ratio) })
      ))
    },
    tt.import = function(pl, n.g) {
      "
      Import texts to a place from other places in the domain:
        * pl = place to get texts for
        * n.g = generation number
        * This fn has to be in the domain so it knows other places.
        * Total no. of texts to import is based on P(import).
        * No. of texts to import from each other place is based on Zipf distn
          (s = 1). Rank is determined by distance from reference place.
      "
      names.other <- names(pll.ranked[[pl$name]])
      n.other <- length(names.other)
      n.want <- pl$n.dem - length(pl$tt.extant)
      n.get <- sum(runif(n.want) < p.import)
      nn <- tabulate(rzipf(n.get, n.other, 1), nbins=n.other)
      nn.tt <- lapply(1:n.other, function(n) { nn[n] } )
      names(nn.tt) <- names.other
      lapply(names(nn.tt), function(from) {
        tt.from <- pll[[from]]$tt.take(nn.tt[[from]])
        lapply(tt.from, function(t) { t$event(
          sprintf("taken from %s to %s (gen. %s)", from, pl$name, n.g)
        ) })
        pl$tt.put(tt.from, "end")
      })
    }
  )
)

# Functions

foo <- function(input) {
  result = list()
  result[["foo"]]="bar"
  return (result)
}

mkmss <- function(input) {
  
  # start timer
  start <- proc.time()

  # specify places
  places <- list(
    Rome =
      Place$new(name = "Rome", n.cap = 300, n.dem = 5,
                n.lat = 41.9, n.lon = 12.5, n.rec = 15),
    Ephesus =
      Place$new(name = "Ephesus", n.cap = 300, n.dem = 5,
                n.lat = 37.9, n.lon = 27.3, n.rec = 15),
    Antioch =
      Place$new(name = "Antioch", n.cap = 300, n.dem = 5,
                n.lat = 36.2, n.lon = 36.1, n.rec = 15),
    Alexandria =
      Place$new(name = "Alexandria", n.cap = 300, n.dem = 5,
                n.lat = 31.2, n.lon = 29.9, n.rec = 15)
  )
  
  # make domain
  d <- Domain$new(
    level = 0,
    n.chh = input$n.chh,
    n.gg = as.numeric(input$n.gg),
    n.grow = input$n.grow,
    n.ratio = input$n.ratio,
    n.seed = input$n.seed,
    n.trend = input$n.trend,
    pll = places,
    p.change = input$p.change,
    p.corr = input$p.corr,
    p.edit = input$p.edit,
    p.import = input$p.import,
    p.loss = input$p.loss
  )
  
  # publish initial text
  # d$publish(d$pll) = publish at each place
  # d$publish(d$pll[N]) = publish at place N
  d$publish(d$pll)
  
  # do propagation phase
  d$propagate()
  
  # do recovery phase
  d$recover()
  
  # get elapsed time
  elapsed = proc.time() - start

  # make data matrix
  data.mx <- t(sapply(
    d$tt.rec,
    function(t) { as.factor(t$chh) }
  ))
  rownames(data.mx) <- sapply(d$tt.rec, function(t) {t$name})
  colnames(data.mx) <- paste("c", 1:ncol(data.mx), sep="")
  
  # make distance matrix
  require("cluster")
  dd.dist <- daisy(as.data.frame(data.mx))
  dist.mx <- as.matrix(dd.dist)
  colnames(dist.mx) <- rownames(dist.mx)
  
  # make summary
  summary <- cat(
    #unlist(sapply(d$tt.rec, function(t) { t$print() })),
    sprintf("Elapsed time: %.1f secs", elapsed[3]),
    sprintf("No. of recovered texts: %d", nrow(data.mx)),
    sprintf("Distances: mean = %.3f; std dev. = %.3f",
            mean(dd.dist), sd(dd.dist)
    ),
    sep="\n"
  )
  
  # return results
  mss <- list()
  mss[["data.mx"]] <- data.mx
  mss[["dist.mx"]] <- dist.mx
  mss[["domain"]] <- d
  mss[["summary"]] <- summary
  return(mss)
}

rzipf <- function(n, N = 2, s = 1) {
  "rzipf(n, N, s)
  Generate n random variates from a Zipf distribution.
  n, N, s: no of observations, no of elements, exponent.
  "
  cdf <- cumsum((1:N)^(-s))
  cdf <- cdf / cdf[N]
  findInterval(runif(n), cdf, rightmost.closed=TRUE) + 1
}

report <- function(level, ...) {
  "
  Return a message corresponding to the level
    * level = numeric selector (0 returns NULL)
    * ... = messages corresponding to levels
    * if level > no. messages then last message returned
  "
  if (level) {
    mm <- list(...)
    n.mm <- length(mm)
    if (level > n.mm) message(mm[length(mm)])
    else {
      msg <- switch(level, ...)
      if (!is.null(msg)) message(msg)
    }
  }
}


# Script

# Run simulation with parameters set using the input variable.
# [1] = run; [2] = do not run.
# The domain variable from the simulation is accessed as mss$domain.
# This contains a complete record of all virtual copies made. 
if (c(TRUE, FALSE)[2]) {
  
  # set input parameters
  input <- list(
    n.chh=30,
    n.gg=5,
    n.grow=1,
    n.ratio=1,
    n.seed=2,
    n.trend=10,
    p.change=0.1,
    p.corr=0,
    p.edit=0.5,
    p.import=0.1,
    p.loss=0.5
  )
  
  # do simulation
  mss <- mkmss(input)
  
  # write output
  message(mss$summary)
  
}

