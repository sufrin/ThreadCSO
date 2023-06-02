
## EIEIO

The project `ox.eieio` was a throwaway attempt at a networking
module for `threadcso`. Much was learned (including the fiendishly
delicate nature of `ByteBuffer`s). Design and implementation stalled
in 2015 because it was proving very incovenient to incorporate
SSL/TLS into the framework, and because I could not resolve the
tension between flexibility and efficiency in the definition
of Codecs.

It has now been superseded by `ox.net`.

Bernard Sufrin, May 2023.

