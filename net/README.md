## The `io.threadcso.net` Package

This package provides relatively lightweight means for
deriving cross-network transport between `io.threadcso.net` programs
communicating  via `threadcso` channels. It provides two analogous
APIs that provide for UDP (Datagram) transport,
encrypted SSL/TLS transport, and unencrypted TCP transport. It
also provides a rich collection of ways of building "wire-encodings"
compatible with other programs and services.

### Connection: a high level API
Cross-network transport is typed (by output, and input type `[O, I]`), and presents
as a `Connection`. This has an `OutPort[O]` named `out` to which values can be sent,
and an `InPort[I]` named `in` from which values can be received once the
connection has been started.

Here's a little `threadcso.net` program that opens a network connection using `TCP` transport 
to `localhost:port`, and a connection  to the controlling terminal. The latter sends `"EOF\n"` 
when the terminal stream  is ended by the user typing the end of file character 
(control-d on Unix machines). 

The program relays lines in both directions, so if there's a server at the port
that responds to the outgoing lines it will show the responses on the terminal.

    import io.threadcso.net.factory.StringTransportCRLF
    import io.threadcso.net.transport.Connection
    import io.threadcso._
    import java.net.{InetSocketAddress => NetAddr}

    def main(args: Array[String]): Unit = {
        var host = "localhost"
        var port = 10000
        val netCon: Connection[String, String] =
            TCPConnection.connected(new NetAddr(host, port), 
                                        StringTransportCRLF, "Chat Connection")
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
presents to an application as a pair of transport daemon processes whose
purpose is to copy data from and to the application itself:

    trait   TypedTransport[-OUT,+IN] 
            // toStream data from `in` and write the encoded form to the net
            def transportToNet  (in: ??[OUT]): PROC = ...
            // read encoded data from the network, then fromStream and write it to `out`
            def transportFromNet(out: !![IN]): PROC = ...

It is the application's responsibility to start the transport daemons before it gets down
to business, and (if necessary) to terminate them when its business is over. 

***ASIDE***: the `Connection` api provides `open` and `close` methods that deal
with the details. It also provides methods by which the channels may be
allocated whose ports are passed to the `transportToNet` and `transportFromNet`
daemons.

Here's a simple applicarion that responds to each `s: String` with `f(s)`, and
terminates if the peer (client) process closes its `out` or `in` `Chan`.

    def application(transport: TypedTransport[String,String], f: String=>String): Unit = {
        val in  = OneOne[String]
        val out = OneOne[String]
        transportToNet(out).fork
        transportFromNet(in).fork
        // 
        repeat {
           in ? {
              case argument => out ! f(result)
           }
        }
        in.closeIn()
        out.closeOut()
    }

There are three main forms of `TypedTransport` -- they correspond to the three main
forms of network transport:

    trait TypedTCPTransport[-OUT,+IN] extends TypedTransport[OUT,IN] with TCPTransportInterface
    trait TypedUDPTransport[-OUT,+IN] extends TypedTransport[OUT,IN] with UDPTransportInterface
    trait TypedSSLTransport[-OUT,+IN] extends TypedTransport[OUT,IN] with SSLTransportInterface

A transport is constructed by specifying its form (`TCP`, `UDP`, `SSL`, `...`) together
with the details of the "wire" (byte-stream). The latter is specified by providing
a `TypedTransportFactory` that does the work of composing an untyped network transport 
interface with mixins that provide the encoding and decoding machinery.

## Transport Examples
Here, for example, are declarations of a couple of TCP transports
that will be connected to the given `host` at its given `port`, and
used both to send and receive strings. The first uses a wire encoding
for strings that is consistent with `http` with strings terminated by
the doublet `"\r\n"`

    val transport1: TypedTCPTransport[String,String] = 
            TCPChannel.connected(host, port, StringTransportCRLF)

The second uses java's built-in wire representation for strings, as 
encoded in `java.io.DataOutputStream`. 

    val transport2: TypedTCPTransport[String,String] = 
            TCPChannel.connected(host, port, StringChannelUTF8)

The following constructs a non-credentialled TLS/SSL client transport to `host` at its `port` -- the
`TLS` negotiation for wire-level encryption is conducted ("out of sight, out of mind") by
the `client` method.

    val transport3: TypedSSLTransport[String,String] = 
            SSLChannel.client(TLSCredential(null, null), host, port, StringTransportCRLF)

Finally, the following constructs a UDP datagram transport that has a "bespoke" 
byte-stream encoding for messages that consist of sequences of records. 
The bespoke encoding for `Type` is determined implicitly by composing 
bespoke encodings (the implicit objects) for its component types: 

    import io.threadcso.net.streamer.Encoding._
    case class Record(name: String, value: Seq[Int))
    type Type = Seq[Record]
    implicit object `Seq[Int]*` extends `Seq*`[Int]
    implicit object `Record*`   extends `2-Case*`[Record, String, Seq[Int]](Record.apply, Record.unapply)
    implicit object `Type*`     extends `Seq*`[Record]
    val transport4: TypedUDPTransport[Type, Type] = 
           UDPChannel.bind(host, port, new StreamerChannel[Type]())


What all the above transport constructions have in common is that they declare the 
wire protocol by passing a `TypedTransportFactory` of the appropriate kind: 

    io.threadcso.net.factory.StringTransportCRLF
    io.threadcso.net.factory.StringTransportUTF8
    io.threadcso.net.factory.StreamerTransport[Type]

to a method that determines the transport and an endpoint port.

    TCPChannel.connected(host, port, StringTransportUTF8) 
    SSLChannel.client(TLSCredential(null, null), host, port, StringTransportCRLF)
    UDPChannel.bind(host, port, new StreamerChannel[Type]())

### Predefined Byte-Stream Encodings

#### String Transport Factories
The simplest predefined byte-stream encodings are for strings. They are defined by
the `TypedTransportFactory`s:

    io.threadcso.net.StringTransportCRLF
    io.threadcso.net.StringTransportUTF8

The former encodes strings as a sequence of (UTF8-encoded) characters followed by the 
the `"\r\n"` (CRLF). It is suitable for use in `http` communication.
The latter encodes strings by their UTF8 representation as defined in
`scala.io.DataOutputStream.writeUTF8`. A glance at their source code
shows the detail of what is, in general,  expected  of a factory, and 
the extent to which account can be taken  within a factory of the specific 
form of transport that will be used. Any factory / wire-encoding can be defined 
ad-hoc by following this pattern,  but there are better (and more reliable) ways of 
doing this.

### User-definable Byte-Stream Encodings
There is a straightforwardly extensible encoder/decoder framework 
for use in implementing the factories the specify "wire-level" (byte-stream)
representations, and two -- structurally similar -- APIs are provided 
(in due course the APIs will be harmonized).

* The first API uses the very compact standard `msgpack` representation 
documented in https://msgpack.org/index.html. 
The implementation detail is presented in `io.threadcso.net.factory.VelviaTransport.`
The detailed work done by the  factory is specified implicitly.
Here, for example, is the definition of a factory that builds a wire-encoding 
for `(Int,Int)` and a demonstration of 
its use to construct a UDP (Datagram) transport for that type.
`````
  import org.velvia.msgpack._
  import SimpleCodecs._
  import TupleCodecs._
  import io.threadcso.net.factory.VelviaTransport
  
  type Ty = (Int, Int)
  implicit object `Ty*` extends `2-Tuple*`[Int,Int]
  val transport = io.threadcso.net.UDPTransport.bind(host, port, new VelviaTransport[Ty])
`````

* The second API doesn't adhere to any particular standard, 
and isn't particularly compact, but is straightforward to use and 
to extend. The detailed  work done by the 
factory is specified implicitly. Here, for example, is the definition
of a factory that builds a wire-encoding for `Seq[String]` -- together with its
use to construct a UDP (Datagram) transport for that type.
`````
  import io.threadcso.net.factory.StreamerTransport
  import io.threadcso.net.streamer.Encoding.{Sequence, Stream}
  import io.threadcso.net.streamer.Encoding.Primitive._
  implicit object StringSeq extends Sequence[String]
  val transport = io.threadcso.net.UDPTransport.bind(host, port, new StreamerTransport[Seq[String]])
`````

Although it may not be obvious here, both APIs can be straightforwardly adapted to the
task of "pickling" program representations as filestore representations.

#### Protocol Buffers
There is also a data stream encoding for messages specified using the `Protocol Buffers`
notation. Its source code is very simple and is provided here for adoption by
those who don't mind adding a protocol buffers dependency to their projects.
(See the `protobuf` project folder)

