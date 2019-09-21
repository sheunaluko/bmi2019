#https://hansenjohnson.org/post/spectrograms-in-r/

spectro = function(data, nfft=1024, window=256, overlap=128, t0=0, normalize = F){
    library(signal)

                                        # extract signal
    snd = data@left

                                        # demean to remove DC offset
    snd = snd-mean(snd)

                                        # determine duration
    dur = length(snd)/data@samp.rate

                                        # create spectrogram
    spec = specgram(x = snd,
                    n = nfft,
                    Fs = data@samp.rate,
                    window = window,
                    overlap = overlap
                    )

                                        # discard phase info
    P = abs(spec$S)

                                        # normalize
    if(normalize){
        P = P/max(P)
    }

                                        # convert to dB
    P = 10*log10(P)

                                        # config time axis
    if(t0==0){
        t = as.numeric(spec$t)
    } else {
        t = as.POSIXct(spec$t, origin = t0)
    }

                                        # rename freq
    f = spec$f


    # prep output
        spec = list(
            t = t,
            f = f,
            p = t(P)
        )

        return(spec)

}
