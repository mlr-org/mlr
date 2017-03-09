genFDAFormula = function(fdn, Qtransform, mgcv.s.k ){
  # FIXME: this function should return all possible smooth functions/surfaces supported by 
  # mgcv but 
  # for simplicity we only implement the af(FGAM) with s function, but there are a lot left
  # arg.vals indices of evaluation of x 
  #sprintf("af(%s, basistype = 'te', Qtransform = TRUE, k=%d, bs =%s, m =%d)", fdn, mgcv.te.k, mgcv.te.bs, mgcv.te.m)
  #sprintf("af(%s, basistype = 'ti', Qtransform = TRUE, k=%d, bs =%s, m =%d)", fdn, mgcv.ti.k, mgcv.te.bs, mgcv.ti.m)
  #sprintf("fpc(%s)", fdn), re, lf, lf.vd()
  #sprintf("peer(%s, argvals = seq(0, 1, length = %d), integration = %s, pentype =%s)",fdn, peer.length, peer.integration, peer.pentype)
}
