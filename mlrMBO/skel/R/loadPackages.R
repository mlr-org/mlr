# load extra packages for some options / user choices
loadPackages = function(control) {
  if (control$infill.opt == "cmaes") 
    requirePackages("cmaes", "proposePoints")
  if (control$multipoint.method == "multicrit") 
    requirePackages("emoa", "proposePoints")
  #if (control$infill.opt == "EI")
  #  requirePackages("DiceOptim")
}    