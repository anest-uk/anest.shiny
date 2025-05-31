#source: chatgpt
#250526

# Foreign Key Constraints
# You’ve implied three composite relationships:
# 
# geo + dfn → estdt via {nx, date}
# 
# geo → forall via nx
# 
# geo + dfn → sse via {nx, rc9}
# 
# Let’s rephrase these as joins that should never drop rows:

# FK1: geo+dfn -> estdt
stopifnot(all(
  estdt[, .N],
  nrow(merge(geo, dfn, allow.cartesian = TRUE)[, .(nx, date)]
       [estdt, on = .(nx, date), nomatch = 0L]) == nrow(estdt)
))

# FK2: geo -> forall
stopifnot(all(
  nrow(merge(geo[, .(nx)], forall, by = "nx", nomatch = 0L)) == nrow(forall)
))

# FK3: geo+dfn -> sse via {nx, rc9}
stopifnot(all(
  nrow(merge(merge(geo, dfn, allow.cartesian = TRUE),
             sse, by = c("nx", "rc9"), nomatch = 0L)) == nrow(sse)
))


#gh comment: these are just scripted - they need to go into an environment where geo, estdt, dfn, forall, sse exist
#where is that?  do the keys have to be defined in data.table formally? [it looks like yes]