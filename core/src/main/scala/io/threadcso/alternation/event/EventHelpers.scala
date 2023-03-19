package io.threadcso.alternation.event

import io.threadcso.basis.Nanoseconds

/** <p> This package implements the syntax of the CSO alternation notations, and
  * provides hooks for the semantics of that notation.
  *
  * The body of an '''alternation''' specification consists of a sequence
  * (possibly empty) of ''executable event'' specifications separated by `|` and
  * possibly followed by: `| after(timeout) ==> { ... }` or `| orelse ==> { ...
  * }` or `| after(timeout) ==> { ... } | orelse ==> { ... }`
  *
  * An '''executable event specification''' takes one of the following forms:
  * {{{
  * outport              =!=>  OutEvent
  * (guard && outport)   =!=>  OutEvent
  * inport               =?=>  InEvent
  * (guard && inport)    =?=>  InEvent
  * inport               =??=> InEvent     // extended rendezvous form
  * (guard && inport)    =??=> InEvent     // extended rendezvous form
  * }}}
  *
  * Concessions to readability: a `Chan` expression may appear in place of a
  * port expression.
  *
  * ==Events and their effects==
  * {{{
  * OutEvent form:                        Effect when triggered
  * {expr}                                outport!expr
  * {expr} ==> { command: Unit }          {outport!expr; command}
  * }}}
  *
  * {{{
  * InEvent form:                         Effect when triggered
  * { bv => body: Unit }                  {val bv=inport?(); body }
  * }}}
  *
  * For an extended rendezvous `InEvent` the correspondence is
  * {{{
  * { bv => body: Unit }                  { inport??({ bv => body}) }
  * }}}
  */

object EventHelpers {

  /** Start of '''after'''`(deadline)` notation (unit is nanoseconds). */
  def after(deadline: => Nanoseconds) = new AfterDeadline(() => deadline)

  private[this] def mkInfix(
      l: ExecutableEventSyntax,
      r: ExecutableEventSyntax
  ): ExecutableEventSyntax =
    InfixEventSyntax(l, r)

  /** Prefix notation for `|`: `|(e,,1,,, ... e,,n,,)` = `e,,1,, | ... | e,,n,,`
    */
  def |(events: collection.Seq[ExecutableEvent]): ExecutableEventSyntax =
    events.reduce(mkInfix)
}
