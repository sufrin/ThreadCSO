
## A Protocol Buffer Transport Factory for `io.threadcso.net.factory`

This mini-project implements an
`io.threadcso.net.factory.TypedTransportFactory` suitable for use in
`io.threadcso.net` transports and connections.

Here is an extract from its use in the construction of a network
connection that sends and receives streams of protocol
buffer messages of type `Person`

      val PCF = new ProtocolBufferTransport[Person,Person](Person)
                
      val netCon = withOptions(inBufSize=10*1024, outBufSize=10*1024, inChanSize=2) {
          TCPConnection.connected(new java.net.InetSocketAddress(host, port), PCF, "")
      }

### Byte-stream encoding

It is necessary to delimit the byte-stream encoding of each message sent 
so that it can be properly decoded as it is received.

There appears to be no standard convention for delimiting protocol buffer
encodings of single messages.  Here we precede each encoding with its 
length in bytes, encoded  as a 4-byte integer in bigendian order.  
This would be very straightforward  to change.


      