g.downsample = function(sig,fs,ws3,ws2) {
	#averaging per second => var1
	sig2 =cumsum(c(0,sig))
	select = seq(1,length(sig2),by=fs)
	var1 = diff(sig2[round(select)]) / abs(diff(round(select[1:(length(select))])))
	#averaging per ws3 => var2 (e.g. 5 seconds)
	select = seq(1,length(sig2),by=fs*ws3)
	var2 = diff(sig2[round(select)]) / abs(diff(round(select[1:(length(select))])))
	#averaging per ws2 => var3 (e.g. 15 minutes)
	select = seq(1,length(sig2),by=fs*ws2)
  var3 = diff(sig2[round(select)]) / abs(diff(round(select[1:(length(select))])))
  invisible(list(var1=var1,var2=var2,var3=var3))
}
