package io

import io.threadcso.alternation.AlternationExports
import io.threadcso.alternation.ChannelExports
import io.threadcso.lock.BarrierExports
import io.threadcso.monitor.MonitorExports
import io.threadcso.semaphore.SemaphoreExports
import io.threadcso.debug.DebugExports
import io.threadcso.basis.TimeExports
import io.threadcso.process.ProcessExports

/** The standard `threadCSO` API. Most modules using `CSO` will need only the
  * declaration:
  * {{{
  * import io.threadcso._
  * }}}
  *
  * The present version of the ThreadCSO library API is `1.2R`''r'' (for some
  * number ''r'')
  *
  * The revision number (`R`''r'') will change if bugs are corrected but the
  * code remains consistent with the previous API. Its minor version number will
  * change if there is a correction to the code that breaks consistency with the
  * previous API. Its major version will change if there is a substantial change
  * in the semantics of an important CSO construct.
  *
  * August 2017: changes 1.1 => 1.2
  *
  *   - renaming of very many internal classes and packages
  *
  *   - basic channel implementations are more efficient, in some case much more
  *     so
  *
  *   - alternation reliability improved
  *
  *   - debugger registration of alternations is no longer needed
  *
  *   - home-grown semaphores can specify which component they are part of: this
  *     makes interpreting a stack backtrace very much easier
  *
  *   - there is a flexible logging system that is compatible with the debugger
  *
  * April 2016: changes 1.0 => 1.1
  *
  *   - Extended rendezvous read operator is now ??` (was `?`)
  *
  *   - Extended rendezvous read event notation is now `=??=>` (was `=?=>>`)
  *
  *   - The notation ``inport ? f`` is now equivalent to `f(inport?())` This
  *     makes for a tidier layout when the function `f` is an explicit
  *     functional expression.
  *
  * Feb 1 2017: changes 1.1R1 => 1.1R2
  *
  *   - Removed dependencies on deprecated Java->Scala functions: replaced with
  *     .asJava
  *     {{{
  *         @author Bernard Sufrin, Oxford
  *         \$Revision: 286 $
  *         \$Date: 2017-11-18 17:41:30 +0000 (Sat, 18 Nov 2017) $
  *     }}}
  */

package object threadcso
    extends BarrierExports
    with MonitorExports
    with SemaphoreExports
    with DebugExports
    with TimeExports
    with ProcessExports
    with AlternationExports
    with ChannelExports {}
