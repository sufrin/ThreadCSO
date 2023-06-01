
import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.NetworkInterface;
import java.net.StandardProtocolFamily;
import java.net.StandardSocketOptions;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.util.Date;

public class multisend {
  public static void main(String[] args) throws InterruptedException {
    ByteBuffer datetime;//from ww w  . jav  a2 s.  co m
    try (DatagramChannel datagramChannel = DatagramChannel
        .open(StandardProtocolFamily.INET)) {
      if (datagramChannel.isOpen()) {
        NetworkInterface networkInterface = NetworkInterface.getByName("lo0");
        datagramChannel.setOption(StandardSocketOptions.IP_MULTICAST_IF,
            networkInterface);
        datagramChannel.setOption(StandardSocketOptions.SO_REUSEADDR, true);
        datagramChannel.bind(new InetSocketAddress(5555));
        while (true) {
          Thread.sleep(5000);
          System.out.println("Sending data ...");
          datetime = ByteBuffer.wrap((new Date().toString()+"\r\n").getBytes());
          datagramChannel
              .send(datetime,
                  new InetSocketAddress(InetAddress.getByName("224.14.51.6"),
                      5555));
          datetime.flip();
        }
      } else {
        System.out.println("The channel cannot be opened!");
      }
    } catch (IOException ex) {
      System.err.println(ex);
    }
  }
}


