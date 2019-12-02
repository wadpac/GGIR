import numpy

def dominant_frequency(y, sf):
  fourier = numpy.fft.fft(y)
  frequencies = numpy.fft.fftfreq(len(y), 1/sf)  # 1/sf is the inter-sample time difference
  magnitudes = abs(fourier[numpy.where(frequencies > 0)])  # magnitude spectrum
  peak_frequency = frequencies[numpy.argmax(magnitudes)]
  return peak_frequency
