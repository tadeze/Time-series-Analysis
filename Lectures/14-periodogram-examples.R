library(ggplot2)
load(url("http://www.stat.pitt.edu/stoffer/tsa3/tsa3.rda"))
source(url("http://stat565.cwick.co.nz/code/fortify-ts.r"))

qplot(time, x, data = fortify(soi), geom = "line") +
  ylab("SOI") + xlab("Year") +
  theme_bw(base_size = 24)

spectrum(soi)
# freq = 1, corresponds to 1 cycle per year

# if you use a time series object the x-axis corresponds to cycles per time period
spectrum(fortify(soi)$x)
# freq = 0.1, corresponds to 0.1 cycle per measurement unit = 1 month, ~ 1 cycle/10months

# no log scale, no taper, remove mean not trend
spectrum(soi, log = "no", taper = 0, demean = TRUE, detrend = FALSE)

# averaged periodogram (average over 5 values) 
spectrum(soi, spans = 5, log = "no", taper = 0)

# if you use the log scale you get a confidence band estimate
spectrum(soi, spans = 5, taper = 0)
spectrum(soi, taper = 0)

# a look at the kernel's spectrum uses
plot(kernel("modified.daniell",  c(2)),  ylim = c(0, 0.3))
plot(kernel("modified.daniell",  c(2, 2)),  ylim = c(0, 0.3))
plot(kernel("modified.daniell",  c(7, 7)),  ylim = c(0, 0.3))

# try some bandwidths
spectrum(soi, taper = 0)
spectrum(soi, spans = c(3, 3), taper = 0)
spectrum(soi, spans = c(5, 5), taper = 0)
spectrum(soi, spans = c(7,7), taper = 0)
spectrum(soi, spans = c(15, 15), taper = 0)

# other examples
# wave tank data C&M
wavetank <- read.table(url("http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/wave.dat"),
                       header = TRUE)
wavetank$time <- seq(1/10, 39.6, 1/10)
qplot(time, waveht, data = wavetank, geom = "line") +
  ylab("Waveheight") + xlab("Seconds") +
  theme_bw(base_size = 24)

spectrum(wavetank$waveht, taper = 0)
spectrum(wavetank$waveht, taper = 0, spans = c(5, 5))
spectrum(wavetank$waveht, taper = 0, spans = c(3, 3))
spectrum(wavetank$waveht, taper = 0, spans = c(7, 7))
spectrum(wavetank$waveht, taper = 0, spans = c(15, 15))
spectrum(wavetank$waveht, taper = 0, spans = c(21, 21))
# motor data C&M
motor <- read.table(url("http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/imotor.txt"),
                       header = TRUE)
motor$time <- seq(0.0025, 100, 0.0025)
qplot(time, good, data = subset(motor, time < 1), geom = "line")+
  theme_bw(base_size = 24)
qplot(time, broken, data = subset(motor, time < 1), geom = "line")+
  theme_bw(base_size = 24)

# do both spectrums in a single plot
spectrum(motor$good, taper = 0, span = c(51, 51))
spectrum(motor$broken, taper = 0, span = c(51, 51))

spgood <- spectrum(motor$good, taper = 0, span = c(51, 51))
spbroken <- spectrum(motor$broken, taper = 0, span = c(51, 51))

str(spgood)
names(spgood)

sp_both <- data.frame(freq = c(spgood$freq, spbroken$freq), 
           spec = c(spgood$spec, spbroken$spec),
           motor = rep(c("good", "broken"), c(length(spgood$freq), length(spbroken$freq))))
qplot(freq, spec, data = sp_both, log = "y", geom = "line", linetype = motor)+
  xlim(0.1, 0.2)

