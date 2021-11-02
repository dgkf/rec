# :red_circle: rec

Record and play back your R

## Quick Start

`rec` is centrally a tool for capturing expression evaluation, trying to
preserve the R object structures emitted during evaluation, including conditions
(message, warning and error objects), console output and output objects.

`rec` operates in one of two ways, either recording an expression running in the
background, or recording the top level interactive console. In both, cases, you
can use the singular interface, `rec()`. Once you've recorded evaluation, you
can also play it back!

[![asciicast](https://asciinema.org/a/MvqJkDVvH4gTBz618RCU58l0m.svg)](https://asciinema.org/a/MvqJkDVvH4gTBz618RCU58l0m)

### Record your console

Recording your console is started and stopped by running `rec()`. While
recording, you'll see a ":red_circle:" in front of your command prompt,
indicating that you're in a recording session.

```r
rec()
print(1:3)
rec()  # stop recording

play()
# print(1:3)
# [1] 1 2 3
```

### Record expression evaluation

Alternatively, you may want to capture evaluation in the background. This can be
A helpful alternative to `capture.output`, for situations where you want to
capture messages, but want to restructure the formatting or operate 

```r
recording <- rec(print(1:3))
# [1] 1 2 3

play(recording)
# print(1:3)
# [1] 1 2 3
```


