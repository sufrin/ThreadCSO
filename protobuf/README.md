
## A Protocol Buffer Channel Factory for `io.threadcso.net.factory`

This mini-project implements an
`io.threadcso.net.factory.TypedChannelFactory` suitable for use in
`io.threadcso.net` channels and connections.

Here is an extract from its use in the construction of a network
connection that sends and receives streams of protocol
buffer messages of type `Person`

      val PCF = new ProtocolBufferChannel[Person,Person](Person)
               
      val netCon = withOptions(inBufSize=10*1024, outBufSize=10*1024, inChanSize=2) {
          TCPConnection.connected(new java.net.InetSocketAddress(host, port), PCF, "")
      }

 

### Wire encoding

It is necessary to delimit the wire encoding of each message sent on
a stream, so that it can be properly decoded as it is received from a
stream.

There appears to be no standard convention for delimiting wire encodings.
Here we precede each encoding with its length in bytes, encoded
as a 4-byte little-endian integer.  This would be very straightforward
to change.


      