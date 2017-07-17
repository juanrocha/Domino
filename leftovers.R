# # calculate closeness
# clos <- lapply(nets, closeness, gmode ='digraph', cmode='suminvdir' )
# df.clos <- list()
#
# for (i in 1:length(clos)){
#     d <- data.frame(vertex = network.vertex.names(nets[[i]]),
#     rs = rep(nets[[i]] %n% 'name', network.size(nets[[i]])),
#     clos = clos[[i]]
#     )
#
#     df.clos[[i]] <- d
# }
#
# df.clos <- bind_rows(df.clos)
# df.clos$rs <- as.factor(df.clos$rs)
# df.clos$vertex <- as.factor(df.clos$vertex)
#
#
# clos <- spread(df.clos, key = rs, value = clos)
# clos$mean <- rowMeans(clos[-1], na.rm=T)
# plot(clos$mean[as.character(clos$vertex) %in% network.vertex.names(bip1)[1:74]], nest[[1]])

# J161114: closeness is not working well, I get lots of zeroes and sometimes only one value. When setting closeness cmode = muminvdir then I getn more numeric values, but at the end there is no interesting relationship between cloness and nestedness. Or any other feature I can relate to a driver being important because direct or indirect. Tested.

>   + Discussion point: regardless of the language and number of variables used in the CLD, the community detection algorithm should separate feedbacks from drivers and allow the identification of missing drivers by comparing causal branches with set operators, as well as drivers than belong to feedbacks in different settings.
>   + missing links from RSDB:
>      + evapotransporation = rainfall = precipitationsheds
>      + trade
>   + {NOT SURE: A community detection algorithm based on a random walker [@Rosvallt:2008p6652] allows us to separate communities of feedback loops and causal branches}. Causal branches are sets of drivers connected to each other that can affect feedback loops in one or more pathways.


>+ Discussion point: synch is contested, but possible until observed. Here we don't claim that they will sync but rather that there is a higher likelihood of synching given the shared drivers.
>+ ref for correlation: causality by judea perl.


+ Correlation depends on contextual settings.
+ Annecdotal examples of cascading effects
  + Precipitationsheds
  + Fire frequency and Arctic
  + Permafrost
  + Lake eutrophication and coastal hypoxic zones
+ The search space for pair-wise combinations is 30^2/2
+ modeling is a way forwards, but it needs to be generalized and
+ trade is not included - empirical explorations.
