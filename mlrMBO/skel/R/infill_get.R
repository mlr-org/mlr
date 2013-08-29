# trivial dispatcher
getInfillCritFunction = function(infill.crit) {
  switch(infill.crit,
    mean = infillCritMeanResponse,
    ei = infillCritEI,
    aei = infillCritAEI,
    lcb = infillCritLCB
  )
}
  