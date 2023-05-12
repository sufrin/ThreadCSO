package ox.eieio


import java.net.{SocketOption, StandardProtocolFamily}
import java.nio.channels.SocketChannel
  /**
  {{{
          @version 1
          @author Bernard Sufrin, Oxford
          \$Revision: 65 $
          \$Date: 2015-04-29 17:22:38 +0100 (Wed, 29 Apr 2015) $
  }}}
  
  `java.net`-defined options.
  */
  object options
  {
    val SO_RCVBUF         = java.net.StandardSocketOptions.SO_RCVBUF
    val SO_SNDBUF         = java.net.StandardSocketOptions.SO_SNDBUF
    val SO_KEEPALIVE      = java.net.StandardSocketOptions.SO_KEEPALIVE
    val SO_REUSEADDR      = java.net.StandardSocketOptions.SO_REUSEADDR
    val SO_BROADCAST      = java.net.StandardSocketOptions.SO_BROADCAST
    val TCP_NODELAY       = java.net.StandardSocketOptions.TCP_NODELAY
    val IP_TOS            = java.net.StandardSocketOptions.IP_TOS
    val IP_MULTICAST_IF   = java.net.StandardSocketOptions.IP_MULTICAST_IF
    val IP_MULTICAST_TTL  = java.net.StandardSocketOptions.IP_MULTICAST_TTL
    val IP_MULTICAST_LOOP = java.net.StandardSocketOptions.IP_MULTICAST_LOOP
    val IPv4              = StandardProtocolFamily.INET
    val IPv6              = StandardProtocolFamily.INET6

    /** Uniform reference for option-setting  */
    def setOption(channel: SocketChannel, opt: SocketOption[Integer], value: Int) = channel.setOption(opt, value.asInstanceOf[Integer])
    def setOption(channel: SocketChannel, opt: SocketOption[java.lang.Boolean], value: Boolean) = channel.setOption(opt, if (value) java.lang.Boolean.TRUE else java.lang.Boolean.FALSE)
    def setOption[T](channel: SocketChannel, opt: SocketOption[T], value: T) = channel.setOption(opt, value)
    def getOption[T](channel: SocketChannel, opt: SocketOption[T]): T = channel.getOption(opt)
  }


