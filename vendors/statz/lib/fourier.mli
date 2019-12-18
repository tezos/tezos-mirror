open Numerics

type time_signal = (Grid.regular, float, Float64.Vec.t) Series.t

type freq_signal = (Grid.regular, Complex.t, Complex64.Vec.t) Series.t

val fft : time_signal -> freq_signal

val ifft : ?t0:float -> freq_signal -> time_signal

val convolution : time_signal -> time_signal -> time_signal

val reverse : time_signal -> time_signal

val cross_correlation : time_signal -> time_signal -> time_signal
