package ox.eieio

import java.nio.ByteBuffer

object types
{
  type BUFFER  = ByteBuffer
  type BUFFERS = Array[ByteBuffer]
  type PACKET  = Array[Byte]
  
  /** Summary of the state of a `BUFFERS` */
  def buffersToString(buffers: BUFFERS): String =
    { buffers.length match
      { case 0 => "NO BUFFERS"
        case 1 => buffers(0).toString
        case 2 => "0: %s / 1: %s".format(buffers(0).toString, buffers(1).toString)
        case n => "0: %s / 1: %s / ...".format(buffers(0).toString, buffers(1).toString)
      }
    }
     

  
  /**   
        A `DecodeResult[T]` is the result of the `decode`
        method of a `Decoder` being applied to the `BUFFER` 
        of that `Decoder`. 
  */
  abstract class  DecodeResult[-T] {}
  /**   
        The `buffer: BUFFER` of the `Decoder` contained a
        prefix that completely represents a value `v` of
        type `T`, and that prefix has been removed. The
        network copier sends `v` to the output stream that
        was specified in the call of `CopyFromNet` that gave
        rise to this decoder being invoked.
  */
  case     class  Decoded[T](result: T) extends DecodeResult[T]
  /**   
        The `buffer: BUFFER` of the `Decoder` contained a
        prefix that completely represents a value of type
        `T`, and that prefix has been removed, and the value
        it represented has been passed on to another
        process. This kind of decoder result ''might'' be
        useful in the low-level ''server-free''
        implementation of multiplexed channels.
  */
  case     class  Completed[T](stillOpen: Boolean) extends DecodeResult[T]
  /**   
        In this case the `buffer: BUFFER` of the `Decoder`
        did not contain a prefix that completely represents
        a value of type `T`, and more bytes will need to be
        read into the buffer before it might do so.
  */  
  case     object ReadMore              extends DecodeResult[Any]
  
  /**   
        In this case the `buffer: BUFFER` of the `Decoder` did not
        contain a prefix that completely represents a value of type
        `T`, and more bytes will need to be read into the buffer
        before might do so.  In this case the continuation `cont`
        is intended to be invoked after more bytes have been read;
        it must be a function (closure) that embodies the information
        discovered so far from the `buffer`. Using a continuation
        avoids having redo work done so far; it is particularly
        useful when the size of a wire representation cannot be
        inferred from its first few bytes. 
        
        Before returning such a result, a decoder must leave the
        buffer in a state such that the next read to the buffer
        is ''appended'' to the bytes that have currently been
        identified as being part of the representation of the value 
        being read. 
        
        For example, here is a decoder for strings delimited by the character `delim`
   {{{        
        class Decoder(bufferSize: Int) extends SimpleDecoder[String](bufferSize)
        { 
          def decode(): DecodeResult[String] =
          { buffer.mark
            decoder(0, false)
          }
          
          def decoder(_length: Int, continuation: Boolean): DecodeResult[String] =
          {  var length = _length
             if (continuation) {
                buffer.position(length)
             }
             while (buffer.remaining>0) 
             {
                   if (buffer.get == delim)
                   { val array = Array.ofDim[Byte](length)
                     if (continuation) buffer.position(0) else buffer.reset
                     buffer.get(array) // the body
                     buffer.get        // but not the newline
                     val res = new String(array, UTF8)
                     return Decoded(res)
                   }
                   else
                   { length=length+1
                   }
             }
             // Avoid reparsing strings that span network packets
             {  buffer.position(buffer.limit-length) // we want to keep length bytes
                buffer.compact()                     // in the compacted buffer
                buffer.position(length)              // and start reading from there
                ReadThen(()=>decoder(length, true))
             }
          }
        }
   }}}      
        
 */  
  case     class ReadThen[T](cont: ()=>DecodeResult[T]) extends DecodeResult[T]
  
  abstract class Decoder[T]
  {  val buffer:   BUFFER
     def decode(): DecodeResult[T]
  }
  
  abstract class Encoder[T]
  {  val buffers:  BUFFERS
     /** 
         Set the sequence of bytes represented by `buffers`
         to the wire encoding of the `value`. There may be
         more than a single buffer involved -- for example
         because it may make sense to encode some header
         information in first buffer that is derived from
         material that is first written to a subsequent
         buffer.
     */ 
     def encode(value: T): Unit
     
     def clear(): Unit = { for (b<-buffers) b.clear() }
     def flip()  = { for (b<-buffers) b.flip() }
  }
  
  abstract class SimpleEncoder[T](bufferSize: Int) extends Encoder[T]
  { 
    val buffer  = ByteBuffer.allocateDirect(bufferSize)
    val buffers = Array(buffer)
  }
  
  abstract class SimpleDecoder[T](bufferSize: Int) extends Decoder[T]
  { val buffer = ByteBuffer.allocateDirect(bufferSize)
  }
  
  /** Package a simple encoder/decoder pair */
  trait SimpleCodec[T] 
  { def Encoder(bufferSize: Int): Encoder[T]
    def Decoder(bufferSize: Int): Decoder[T]
  }
  
  
}









