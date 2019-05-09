read.myacc.csv = function(file=c(), nrow=c(), skip=c(), sep=c(),
                                firstraw.acc=10, firstrow.header=1,
                                col.acc=2:4, col.temp=5, col.time=1,
                                unit.acc="g", unit.temp="C", format.time=="%Y-%m-%d %H:%M:%OS",
                                origin = "1970-1-1",
                                desiredtz = "Europe/London", samplefrequency=100,
                                headername.samplefrequency= "sample_frequency",
                                headername.deviceserialnumber="serial_number",
                                headername.recordingid = "ID") # not included yet, optionally additonal columns
{
 
  
  
   # Output: data.frame with raw acceleration and timestamps
  # All data converted into a standard format
}