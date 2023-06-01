import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.NetworkInterface;
import java.net.StandardProtocolFamily;
import java.net.StandardSocketOptions;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.channels.DatagramChannel;
import java.nio.channels.MembershipKey;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;

public class multilisten {
  public static void main(String[] args) {
    CharBuffer charBuffer = null;
    Charset charset = Charset.defaultCharset();
    CharsetDecoder decoder = charset.newDecoder();
    ByteBuffer datetime = ByteBuffer.allocateDirect(65507);
    try (DatagramChannel datagramChannel = DatagramChannel
        .open(StandardProtocolFamily.INET)) {
      InetAddress group = InetAddress.getByName("224.14.51.6");
      if (group.isMulticastAddress()) {
        if (datagramChannel.isOpen()) {
          NetworkInterface networkInterface = NetworkInterface
              .getByName("lo0");
          datagramChannel.setOption(StandardSocketOptions.SO_REUSEADDR, true);
          datagramChannel.bind(new InetSocketAddress(5555));
          MembershipKey key = datagramChannel.join(group, networkInterface);
          System.out.print("Listening: key="); System.out.println(key);
          while (true) {
            if (key.isValid()) {
              datagramChannel.receive(datetime);
              datetime.flip();/*from   w  ww  . j  a  v a2 s . c o  m*/
              charBuffer = decoder.decode(datetime);
              System.out.println(charBuffer.toString());
              datetime.clear();
            } else {
              break;
            }
          }
        } else {
          System.out.println("The channel cannot be opened!");
        }
      } else {
        System.out.println("This is not  multicast address!");
      }
    } catch (IOException ex) {
      System.err.println(ex);
    }
  }
}


