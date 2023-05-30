## The `ox.net` Package

This package provides relatively lightweight means for
deriving cross-network transport between `ox.threadcso` programs
communicating via channels. It provides a more-or-less uniform
API that is suitable for UDP (Datagram) connections,
encrypted SSL/TLS connections, and unencrypted TCP connections. 

Cross-network transport is typed (by output, and input type), and 
presents to an application as a pair of  proxy processes whose
purpose is to copy data from and to the application itself:

    trait   NetProxy[-OUT,+IN] 
            // encode data from `in` and write the encoded form to the net
            def CopyToNet  (in: ??[OUT]): PROC = ...
            // read encoded data from the network, then decode and write it to `out`
            def CopyFromNet(out: !![IN]): PROC = ...

It is the application's responsibility to start the proxies before it gets down
to business, and (if necessary) to terminate them when its business is over. 
Here's a simple server that responds to each `s: String` with `f: String`, and
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
a `ChannelFactory` that does the work of composing an untyped network interface
with the encoding and decoding machinery.

## Channel Examples
Here, for example, are declarations of a couple of TCP channels
that will be connected to the given `host` at its given `port`, and
used both to send and receive strings. The first uses a wire encoding
for strings that is consistent with `http` with strings terminated by
the doublet `"\r\n"`

    val channel1: TypedTCPChannel[String,String] = 
            TCPChannel.connected(host, port, CRLFChannelFactory)

The second uses java's built-in `DataOutputStream` representation for strings. 

    val channel2: TypedTCPChannel[String,String] = 
            TCPChannel.connected(host, port, UTF8ChannelFactory)

The following constructs a non-credentialled TLS/SSL client channel to `host` at its `port`. The
`TLS` negotiation for wire-level encryption is conducted ("out of sight, out of mind") by
the `client` method.

    val channel3: TypedSSLChannel[String,String] = 
            SSLChannel.client(TLSCredential(null, null), host, port, CRLFChannelFactory)

Finally, the following constructs a UDP datagram channel that has a "bespoke" 
wire encoding for messages that consist of sequences of records. 
The bespoke encoding is determined implicitly.

    import ox.net.codec.DataStreamEncoding.Primitive._
    import ox.net.codec.DataStreamEncoding._
    case class Record(name: String, value: Seq[Int))
    type Type = Seq[Record]
    implicit object `IntSeq*`     extends Sequence[Int]
    implicit object `Record*`     extends `2cons`[Record, String, Seq[Int]](Record.apply, Record.unapply)
    implicit object `RecordSeq*`  extends Sequence[Record]
    val channel4: TypedUDPChannel[Type, Type] = 
           UDPChannel.bind(host, port, new DataStreamChannelFactory[Type]())


What the above channel constructions have in common is that they declare the 
wire protocol by passing a `ChannelFactory` of the appropriate kind to a method 
that determines the transport. 

Two simple factories determine wire encodings for
`String`; they are provided to show the detail of what is, in general, 
expected  of a factory, and the extent to which account can be taken
in a factory of the specific form of transport that will be used. Any
factory / wire-encoding can be defined ad-hoc by following this pattern, 
but there are better (and more reliable) ways of doing this.

### Defining Wire-Encodings
There is a straightforwardly extensible encoder/decoder framework 
for use in implementing the factories the specify "wire-level" 
representations, and two -- structurally similar -- APIs are provided 
(in due course the APIs will be harmonized).

* The first API uses the very compact standard `msgpack`
representation documented in https://msgpack.org/index.html. 
The implementation
detail is presented in `ox.net.channelfactory.VelviaChannelFactory`,
and the detailed work done by the  factory is specified implicitly.
Here, for example, is the definition of a factory that builds a wire-encoding for `(Int,Int)`, 
and its use to construct a UDP (Datagram) channel for that type.
`````
  import org.velvia.msgpack._
  import SimpleCodecs._
  import TupleCodecs._
  type Ty = (Int, Int)
  implicit object TyCodec extends TupleCodec2[Int,Int]
  val channel = ox.net.UDPChannel.bind(host, port, new VelviaChannelFactory[Ty])
`````

* The second API doesn't adhere to any particular standard, 
and isn't particularly compact, but is straightforward to use and 
to extend. Most of the detailed  work done by the 
factory is specified implicitly. Here, for example, is the definition
of a factory that builds a wire-encoding for `Seq[String]`, and its
use to construct a UDP (Datagram) channel for that type.
`````
  import ox.net.channelfactory.{DataStreamChannelFactory => Factory}
  import ox.net.codec.DataStreamEncoding.{Sequence, Stream}
  import ox.net.codec.DataStreamEncoding.Primitive._
  implicit object StringSeq extends Sequence[String]
  val channel = ox.net.UDPChannel.bind(host, port, new Factory[Seq[String]])
`````

Although it may not be obvious here, both APIs can be straightforwardly adapted to the
task of "pickling" program representations as filestore representations.

