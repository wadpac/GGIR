parametersVignette = function(params = "sleep") {
  
  if (!grepl("params_", params)) params = paste0("params_", params)
  arguments = names(load_params()[[params]])
  vignette = character()
  for (i in 1:length(arguments)) {
    vignette = paste0(vignette, "\n### ", arguments[i], " {-}\n",
                      ShellDoc2Vignette(arguments[i]), "\n\n ")
  }
  return(vignette)
}
