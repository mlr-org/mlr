# Takes a bit string and flips its elements based on a given mutation rate.
#

mutateBits = function(x, rate = 1 / length(x)) {
  n = length(x)
  flip = rbinom(n, 1, rate)
  (x + flip) %% 2
}
