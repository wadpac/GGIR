g.abr.day.names = function(daynames) {
  tmp =rep(0,length(daynames))
  daynamesnew =rep("",length(daynames))
  tmp[which(daynames=="Monday")] = 1
  tmp[which(daynames=="Tuesday")] = 2
  tmp[which(daynames=="Wednesday")] = 3
  tmp[which(daynames=="Thursday")] = 4
  tmp[which(daynames=="Friday")] = 5
  tmp[which(daynames=="Saturday")] = 6
  tmp[which(daynames=="Sunday")] = 7
  daynamesnew[which(tmp == 1)] = "MON"
  daynamesnew[which(tmp == 2)] = "TUE"
  daynamesnew[which(tmp == 3)] = "WED"
  daynamesnew[which(tmp == 4)] = "THU"
  daynamesnew[which(tmp == 5)] = "FRI"
  daynamesnew[which(tmp == 6)] = "SAT"
  daynamesnew[which(tmp == 7)] = "SUN"
  g.convert.day.names= daynamesnew
}