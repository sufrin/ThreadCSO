## The `io.threadcso.net` Package

This package provides relatively lightweight means for
deriving cross-network transport between `io.threadcso.net` programs
communicating  via `threadcso` channels. It provides two analogous
APIs that provide for UDP (Datagram) transport,
encrypted SSL/TLS transport, and unencrypted TCP transport. It
also provides a rich collection of ways of building "wire-encodings"
compatible with other programs and services.

### Connection: a high level API
Cross-network transport is typed (by output, and input type), and presents
as a `Connection` -- with a pair of channels to and from which data is sent once the
connection has been started.

Here's a little `threadcso.net` program that opens a network connection to `localhost:port`, and a connection
to the controlling terminal. The latter sends `"EOF\n"` when the terminal stream is
ended by the user typing the end of file character (control-d on Unix machines). 
It relays lines in both directions, so if there's a server at the port
that responds to the outgoing lines it will show these on the terminal.


    import io.threadcso.net.factory.StringChannelCRLF
    import io.threadcso.net.channels.Connection
    import io.threadcso._
    import java.net.{InetSocketAddress => NetAddr}

    def main(args: Array[String]): Unit = {
        var host = "localhost"
        var port = 10000
        val netCon: Connection[String, String] =
            TCPConnection.connected(new NetAddr(host, port), 
                                        StringChannelCRLF, "Chat Connection")
        val kbdCon: Connection[String,String] = new TerminalConnection("EOF\n")
        netCon.open()
        kbdCon.open()
        serve(  kbdCon.in =?=> { 
                   case "EOF\n"   => netCon.close()
                   case line      => netCon.out ! line 
                }
             |  netCon.in =?=> { line => kbdCon.out ! s"\t$line" }
             )
        netCon.close()
        kbdCon.close()
    }

Several simple programs like this can be found in the package `io.threadcso.net.tests`. Some
use the `Connection` APIs and others use the lower-level APIs whose descriptions are given in
detail below. Start by reading `io.threadcso.net.tests.chat` and 
`io.threadcso.net.tests.reflectcon` -- the latter is an example of a server.

### Lower-level API
Cross-network transport is typed (by output, and input type), and 
presents to an application as a pair of  proxy processes whose
purpose is to copy data from and to the application itself:

    trait   NetProxy[-OUT,+IN] 
            // toStream data from `in` and write the encoded form to the net
            def CopyToNet  (in: ??[OUT]): PROC = ...
            // read encoded data from the network, then fromStream and write it to `out`
            def CopyFromNet(out: !![IN]): PROC = ...

It is the application's responsibility to start the proxies before it gets down
to business, and (if necessary) to terminate them when its business is over. 
Here's a simple server that responds to each `s: String` with `f(s)`, and
terminates if the peer (client) process closes its output or input channel.

    def application(transport: NetProxy[String,String], f: String=>String): Unit = {
        val in  = OneOne[String]
        val out = OneOne[String]
        CopyToNet(out).fork
        CopyFromNet(in).fork
        // 
        repeat {
           in ? {
              case argument => out ! f(result)
           }
        }
        in.closeIn()
        out.closeOut()
    }

There are three main forms of `NetProxy` -- they correspond to the three main
forms of network transport:

    trait TypedTCPChannel[-OUT,+IN] extends NetProxy[OUT,IN] with TCPChannelInterface
    trait TypedUDPChannel[-OUT,+IN] extends NetProxy[OUT,IN] with UDPChannelInterface
    trait TypedSSLChannel[-OUT,+IN] extends NetProxy[OUT,IN] with SSLChannelInterface

A transport is constructed by specifying its form (`TCP`, `UDP`, `SSL`, `...`) together
with the details of the "wire encoding". The latter is specified by providing
a `ChannelFactory` that does the work of composing an untyped network transport interface
with the encoding and decoding machinery.

## Channel Examples
Here, for example, are declarations of a couple of TCP channels
that will be connected to the given `host` at its given `port`, and
used both to send and receive strings. The first uses a wire encoding
for strings that is consistent with `http` with strings terminated by
the doublet `"\r\n"`

    val channel1: TypedTCPChannel[String,String] = 
            TCPChannel.connected(host, port, StringChannelCRLF)

The second uses java's built-in wire representation for strings, as 
encoded in `java.io.DataOutputStream`. 

    val channel2: TypedTCPChannel[String,String] = 
            TCPChannel.connected(host, port, StringChannelUTF8)

The following constructs a non-credentialled TLS/SSL client channel to `host` at its `port` -- the
`TLS` negotiation for wire-level encryption is conducted ("out of sight, out of mind") by
the `client` method.

    val channel3: TypedSSLChannel[String,String] = 
            SSLChannel.client(TLSCredential(null, null), host, port, StringChannelCRLF)

Finally, the following constructs a UDP datagram channel that has a "bespoke" 
wire encoding for messages that consist of sequences of records. 
The bespoke encoding for `Type` is determined implicitly by composing 
bespoke encodings (the implicit objects) for its component types: 

    import io.threadcso.net.streamer.Encoding._
    case class Record(name: String, value: Seq[Int))
    type Type = Seq[Record]
    implicit object `Seq[Int]*` extends `Seq*`[Int]
    implicit object `Record*`   extends `2-Case*`[Record, String, Seq[Int]](Record.apply, Record.unapply)
    implicit object `Type*`     extends `Seq*`[Record]
    val channel4: TypedUDPChannel[Type, Type] = 
           UDPChannel.bind(host, port, new StreamerChannel[Type]())


What all the above channel constructions have in common is that they declare the 
wire protocol by passing a `TypedChannelFactory` of the appropriate kind: 

    io.threadcso.net.factory.StringChannelCRLF
    io.threadcso.net.factory.StringChannelUTF8
    io.threadcso.net.factory.StreamerChannel[Type]

to a method that determines the transport and an endpoint port.

    TCPChannel.connected(host, port, StringChannelUTF8) 
    SSLChannel.client(TLSCredential(null, null), host, port, StringChannelCRLF)
    UDPChannel.bind(host, port, new StreamerChannel[Type]())

Two simple factories determine wire encodings for
`String`; 

### Predefined Wire-Encodings

#### Strings
The simplest predefined wire encodings are for strings. They are defined by:

    io.threadcso.net.StringChannelCRLF
    io.threadcso.net.StringChannelUTF8

The former encodes strings as a sequence of (UTF8-encoded) characters followed by the 
the `"\r\n"` (CRLF). It is suitable for use in `http` communication.
The latter encodes strings by their UTF8 representation as defined in
`scala.io.DataOutputStream.writeUTF8`. A glance at their source code
shows the detail of what is, in general,  expected  of a factory, and 
the extent to which account can be taken  within a factory of the specific 
form of transport that will be used. Any factory / wire-encoding can be defined 
ad-hoc by following this pattern,  but there are better (and more reliable) ways of 
doing this.

### User-definable Wire-Encodings
There is a straightforwardly extensible encoder/decoder framework 
for use in implementing the factories the specify "wire-level" 
representations, and two -- structurally similar -- APIs are provided 
(in due course the APIs will be harmonized).

* The first API uses the very compact standard `msgpack` representation 
documented in https://msgpack.org/index.html. 
The implementation detail is presented in `io.threadcso.net.factory.VelviaChannel.`
The detailed work done by the  factory is specified implicitly.
Here, for example, is the definition of a factory that builds a wire-encoding 
for `(Int,Int)` and a demonstration of 
its use to construct a UDP (Datagram) channel for that type.
`````
  import org.velvia.msgpack._
  import SimpleCodecs._
  import TupleCodecs._
  type Ty = (Int, Int)
  implicit object `Ty*` extends `2-Tuple*`[Int,Int]
  val channel = io.threadcso.net.interfaces.netchannels.UDPChannel.bind(host, port, new VelviaChannelFactory[Ty])
`````

* The second API doesn't adhere to any particular standard, 
and isn't particularly compact, but is straightforward to use and 
to extend. The detailed  work done by the 
factory is specified implicitly. Here, for example, is the definition
of a factory that builds a wire-encoding for `Seq[String]` -- together with its
use to construct a UDP (Datagram) channel for that type.
`````
  import io.threadcso.net.interfaces.netchannels.channelfactory.{DataStreamChannelFactory => Factory}
  import io.threadcso.net.streamer.Encoding.{Sequence, Stream}
  import io.threadcso.net.streamer.Encoding.Primitive._
  implicit object StringSeq extends Sequence[String]
  val channel = io.threadcso.net.interfaces.netchannels.UDPChannel.bind(host, port, new Factory[Seq[String]])
`````

Although it may not be obvious here, both APIs can be straightforwardly adapted to the
task of "pickling" program representations as filestore representations.

#### Protocol Buffers
There is also a wire encoding for messages specified using the `Protocol Buffers`
notation. Its source code is very simple and is provided here for adoption by
those who don't mind adding a protocol buffers dependency to their projects.
(See the `protobuf` project folder)

