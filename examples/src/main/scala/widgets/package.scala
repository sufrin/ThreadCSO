import javax.swing._
import java.awt.{BorderLayout, Dimension}
import java.awt.event._
import javax.swing.event._
import javax.swing.border.Border

/** The classes of this package provide a small selection of widgets derived
  * from `javax.swing` and set up for easy declarative specification of simple
  * Interfaces. There are two main differences:
  *
  *   1. The widgets have a number of convenience methods that permit various
  *      ''optional'' attributes to be set as they are constructed. This
  *      replaces the imperative style of specifying optional attributes.
  *
  *   1. Java-style "Listeners" are not used. Instead ''action'' widget classes
  *      (those that register ActionListeners or ChangeListeners) are either
  *      specified by an Act (an abstract class that must define an
  *      `action:Unit=>Unit` method at the point of construction, or earlier by
  *      subclassing) or (more conveniently) accept a suitably-typed action
  *      method as a parameter when they are constructed.
  *
  * We make no pretence of having produced a complete toolkit; simplicity has
  * been our watchword. Moreover, we have -- through haste not conviction --
  * taken a somewhat cavalier approach to the subtleties of the Scala type
  * system.
  */

package object widgets {

  /** Vertical or Horizontal orientation: used in JSlider, JToolBar, ... */
  class Orientation(_rep: Int) { val rep = _rep }
  case object Vertical extends Orientation(SwingConstants.VERTICAL)
  case object Horizontal extends Orientation(SwingConstants.HORIZONTAL)

  trait Widget[+T] extends JComponent {
    val self = this.asInstanceOf[T]

    /** Set the tooltip */
    def withTip(tip: String): T = { setToolTipText(tip); self}

    /** Set the X alignment (for use in Col layouts), and return this */
    def withAlignmentX(f: Double): T = { setAlignmentX(f.floatValue); self }


    /** Set the X alignment (for use in Col layouts), and return self */
    def alignX(f: Double): T = { setAlignmentX(f.floatValue); self }

    /** Set the Y alignment (for use in Row layouts), and return self */
    def withAlignmentY(f: Double): T = { setAlignmentY(f.floatValue); self }

    /** Set the Y alignment (for use in Row layouts), and return self */
    def alignY(f: Double): T = { setAlignmentY(f.floatValue); self }

    /** Set the preferred width and height from <code>w, h</code>; if either are
      * negative use the existing value
      */
    def withPreferredSize(w: Int, h: Int): T = {
      setPreferredSize(
        new Dimension(
          if (w < 0) getPreferredSize.width else w,
          if (h < 0) getPreferredSize.height else h
        )
      )
      self
    }

    /** Set the preferred width, and return self */
    def preferWidth(w: Int): T =
      withPreferredSize(w, -1)

    /** Set the preferred height, and return self */
    def preferHeight(h: Int): T =
      withPreferredSize(-1, h)

    /** Set the border and return self (inadvisable for most non-container
      * components)
      */
    def withBorder(border: Border): T = { setBorder(border); self }

    /** Set an etched border and return self (inadvisable for most non-container
      * components)
      */
    def withEtchedBorder(): T = withBorder(etchedBorder)

    /** Set an etched and titled border and return self (inadvisable for most
      * non-container components)
      */
    def withTitledBorder(title: String): T =
      withBorder(BorderFactory.createTitledBorder(etchedBorder, title))

    /** Embed in a component with a titled, etched border, and return that
      * component
      */
    def inFrameTitled(title: String): JCol =
      new JCol(List(this)).withTitledBorder(title)

    /** Embed in a component with an etched border, and return that component
      */
    def inFrame: JCol = new JCol(List(this)).withEtchedBorder()

    /** Create an etched border */
    protected def etchedBorder: Border =
      (BorderFactory.createCompoundBorder(
        BorderFactory.createEtchedBorder(),
        BorderFactory.createEmptyBorder(2, 2, 2, 2)
      ))
  }

  /** A ButtonWidget can be placed in a ButtonGroup */
  trait ButtonWidget[+T] extends AbstractButton with Widget[T] {
    def withSelected(state: Boolean): T = {
      setSelected(state); this.asInstanceOf[T]
    }
  }

  /** An ActionWidget must specify a method <tt>action:Unit=>Unit</tt> to be
    * called when the widget's action is invoked. This trait takes care of the
    * details of registering a listener to call this method.
    */
  trait ActionWidget[T] extends Widget[T] with ActionSpec {
    def addActionListener(listener: java.awt.event.ActionListener): Unit

    addActionListener(new java.awt.event.ActionListener() {
      def actionPerformed(ev: ActionEvent): Unit = { action }
    })

  }

  trait ActionSpec {
    def action: Unit
  }

  class JButton extends javax.swing.JButton with ButtonWidget[JButton] {

    /** A button based on a given ACT */
    def this(act: Act) = { this(); setAction(act) }

    /** A button whose action method is passed as a parameter */
    def this(name: String, act: Unit => Unit) = {
      this()
      setAction(new Act(name) { def action = act(()) })
    }
  }

  class JCheckBox extends javax.swing.JCheckBox with ButtonWidget[JCheckBox] {

    /** A checkbox based on a given ACT */
    def this(act: Act) = { this(); setAction(act) }

    /** A checkbox whose action method is passed as a parameter. The parameter
      * of the action is the state of the checkbox.
      */
    def this(name: String, act: Boolean => Unit) = {
      this()
      val self = this
      setAction(new Act(name) { def action = act(self.isSelected) })
    }
  }

  class JRadioButton
      extends javax.swing.JRadioButton
      with ButtonWidget[JRadioButton] {

    /** A checkbox based on a given ACT */
    def this(act: Act) = { this(); setAction(act) }

    /** A radiobutton whose action method is passed as a parameter. The
      * parameter of the action is the state of the checkbox.
      */
    def this(name: String, act: Boolean => Unit) = {
      this()
      val self = this
      setAction(new Act(name) { def action = act(self.isSelected) })
    }
  }

  /** MenuItem variant of a JCheckBox */
  class JCheckBoxMenuItem
      extends javax.swing.JCheckBoxMenuItem
      with ButtonWidget[JCheckBoxMenuItem] {
    def this(act: Act) = { this(); setAction(act) }
    def this(name: String, act: Boolean => Unit) = {
      this()
      val self = this
      setAction(new Act(name) { def action = act(self.isSelected) })
    }
  }

  /** MenuItem variant of a JRadioButton */
  class JRadioButtonMenuItem
      extends javax.swing.JRadioButtonMenuItem
      with ButtonWidget[JRadioButtonMenuItem] {
    def this(act: Act) = { this(); setAction(act) }
    def this(name: String, act: Boolean => Unit) = {
      this()
      val self = this
      setAction(new Act(name) { def action = act(self.isSelected) })
    }
  }

  /** MenuItem variant of a JButton */
  class JMenuItem extends javax.swing.JMenuItem with ButtonWidget[JMenuItem] {
    def this(act: Act) = { this(); setAction(act) }
    def this(name: String, act: Unit => Unit) = {
      this()
      setAction(new Act(name) { def action = act(()) })
    }
  }

  /** A menu with the given caption and the given menu items. Within this class
    * we define methods button, checkBox, radioButton to construct their
    * MenuItem counterparts and also add them to the menu under construction
    */
  class JMenu(caption: String, items: JMenuItem*)
      extends javax.swing.JMenu(caption)
      with Widget[JMenu] {
    private val cont = this

    /** Locally we define button, checkBox, radioButton to be constructors for
      * their MenuItem counterparts that also add them to the menu under
      * construction
      */
    def menuItem(name: String)(act: => Unit) = {
      val item = new JMenuItem(name, { case () => act })
      cont.add(item)
      item
    }

    def button(name: String)(act: => Unit) = {
      val item = new JMenuItem(name, { case () => act })
      cont.add(item)
      item
    }

    def checkBox(name: String, state: Boolean*)(act: Boolean => Unit) = {
      val initialState = state.length > 0 && state(0)
      val item = new JCheckBoxMenuItem(name, act) withSelected initialState
      cont.add(item)
      item
    }

    def radioButton(name: String, state: Boolean*)(act: Boolean => Unit) = {
      val initialState = state.length > 0 && state(0)
      val item = new JRadioButton(name, act) withSelected initialState
      cont.add(item)
      item
    }

    /** Construct a (nested) menu and add it to the <code>_cont</code>ainer menu
      */
    class Menu(_cont: JMenu, caption: String) extends JMenu(caption) {
      _cont.add(this)
    }

    def separator = addSeparator
    for (it <- items) add(it)
  }

  /** A menubar with the given menus. Within this class the class Menu behaves
    * as JMenu does outside, except it automatically adds the menu it is
    * constructing to the MenuBar under construction
    */
  class JMenuBar(menus: JMenu*)
      extends javax.swing.JMenuBar
      with Widget[JMenuBar] {
    private val cont = this

    /** Construct and add a menu */
    class Menu(caption: String) extends JMenu(caption) { cont.add(this) }

    for (m <- menus) { add(m) }
  }

  /** A tool bar with the given orientation and the given widgets. Within this
    * class we define methods button, checkBox, radioButton that construct
    * appropriate widgets and add them to the menu under construction
    */
  class JToolBar(orientation: Orientation, widgets: Iterable[Widget[Any]])
      extends javax.swing.JToolBar(orientation.rep)
      with Widget[JToolBar] {
    val cont = this

    for (w <- widgets) add(w)

    /** Construct and add a button */
    def button(name: String)(act: => Unit) = {
      val item = new JButton(name, { case () => act })
      cont.add(item)
      item
    }

    /** Construct and add a checkbox (initial state parameter is optional) */
    def checkBox(name: String, state: Boolean*)(act: Boolean => Unit) = {
      val initialState = state.length > 0 && state(0)
      val item = new JCheckBox(name, act) withSelected initialState
      cont.add(item)
      item
    }

    /** Construct and add a radiobutton (initial state parameter is optional) */
    def radioButton(name: String, state: Boolean*)(act: Boolean => Unit) = {
      val initialState = state.length > 0 && state(0)
      val item = new JRadioButton(name, act) withSelected initialState
      cont.add(item)
      item
    }

    def separator = addSeparator
  }

  class JSpinner[D](model: javax.swing.SpinnerModel, act: D => Unit)
      extends javax.swing.JSpinner(model)
      with Widget[JSpinner[D]] {
    addChangeListener(new ChangeListener {
      override def stateChanged(e: ChangeEvent): Unit =
        act(model.getValue.asInstanceOf[D])
    })
    def alignLeft: Widget[JSpinner[D]] = {
      import javax.swing.SwingConstants.LEFT
      getEditor match {
        case ed: javax.swing.JSpinner.DefaultEditor =>
          ed.getTextField.setHorizontalAlignment(LEFT)
        case other => assert(false, s"Not a default spinner editor: $other")
      }
      this
    }
  }

  def spinner(
      strings: Iterable[String]
  )(act: String => Unit): JSpinner[String] =
    new JSpinner[String](
      new javax.swing.SpinnerListModel(
        strings.toArray.asInstanceOf[Array[Object]]
      ),
      act
    )

  def spinner(strings: String*)(act: String => Unit): JSpinner[String] =
    spinner(strings.toList)(act)

  def spinner(value: Double, min: Double, max: Double, stepSize: Double)(
      act: Double => Unit
  ): JSpinner[Double] =
    new JSpinner[Double](
      new javax.swing.SpinnerNumberModel(value, min, max, stepSize),
      act
    )

  def spinner(value: Int, min: Int, max: Int, stepSize: Int)(act: Int => Unit) =
    new JSpinner[Int](
      new javax.swing.SpinnerNumberModel(value, min, max, stepSize),
      act
    )

  /** A Swing <code>JTextField</code> with the given name and initial text; it
    * invokes its action method when Enter is pressed.
    */
  abstract class JTextField(text: String, cols: Int)
      extends javax.swing.JTextField(text, cols)
      with ActionWidget[JTextField] {}

  /** A label (with an optional Icon) */
  class JLabel(name: String, icon: Icon*)
      extends javax.swing.JLabel(name)
      with Widget[JLabel] {
    for (ic <- icon) setIcon(ic)
  }

  /** A JFrame with a Border layout that exits the application when closed.
    * Usage: <pre><code> new JApplication { North=...; South=... (etc) }
    * </code></pre> One or more of North, South, East, West, Center must be
    * assigned components. MenuBar may be assigned a JMenuBar.
    */
  class JApplication(title: String) extends JFrame(title) {
    var North, South, East, West, Center: JComponent = _
    var MenuBar: JMenuBar = _
    setLayout(new BorderLayout)
    setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

    override def pack = {
      if (North != null) add(North, "North")
      if (South != null) add(South, "South")
      if (East != null) add(East, "East")
      if (West != null) add(West, "West")
      if (Center != null) add(Center, "Center")
      if (MenuBar != null) setJMenuBar(MenuBar)
      if (
        North == null && South == null &&
        East == null && West == null && Center == null
      )
        throw new Error(
          "JApplication with null North, South, East, West, and Center"
        )
      super.pack
    }
  }

  /** A Panel with a Border layout. Usage: <code> new JBorder { north=...;
    * south=... (etc) } </code>
    */
  class JBorder extends JPanel(new BorderLayout) with Widget[JBorder] {
    var North, South, East, West, Center: JComponent = _
    override def doLayout = {
      if (North != null) add(North, "North")
      if (South != null) add(South, "South")
      if (East != null) add(East, "East")
      if (West != null) add(West, "West")
      if (Center != null) add(Center, "Center")
      super.doLayout
    }
  }

  /** A column constructed by packing <code>cs</code> in a vertical Box */
  class JCol(cs: Iterable[Widget[Any]])
      extends Box(SwingConstants.VERTICAL)
      with Widget[JCol] {
    for (c <- cs) add(c)
  }

  /** A row constructed by packing <code>cs</code> in a horizontal Box */
  class JRow(cs: Iterable[Widget[Any]])
      extends Box(SwingConstants.HORIZONTAL)
      with Widget[JRow] {
    for (c <- cs) add(c)
  }

  /** A JSlider with the given name orientation and associated range and value.
    * It invokes its <tt>action</tt> method when its value is changed: while the
    * slider is being adjusted if the slider is in instant mode, otherwise when
    * the mouse button is released at the end of an adjustment. <p> Default
    * major ticks are at 10%
    */
  abstract class JSlider(
      min: Int,
      max: Int,
      initialValue: Int,
      or: Orientation = Horizontal
  ) extends javax.swing.JSlider(or.rep, min, max, initialValue)
      with Widget[JSlider] {
    protected var instant = false

    /** Invoked by the slider when its value changes */
    def onChange(value: Int): Unit

    /** Current value of the slider */
    def value = getValue

    /** Force a change to the current vale of the slider */
    def value_=(v: Int) = setValue(v)

    addChangeListener(new ChangeListener() {
      def stateChanged(ev: ChangeEvent): Unit = {
        if (instant || !getModel.getValueIsAdjusting) onChange(getValue)
      }
    })

    def withMajor(n: Int) = { setMajorTickSpacing(n); this }
    def withMinor(n: Int) = { setMinorTickSpacing(n); this }
    def withTicks(b: Boolean) = { setPaintTicks(b); this }
    def withLabels(b: Boolean) = { setPaintLabels(b); this }
    def withInstant(b: Boolean) = { instant = b; this }
    def withFontScaled(scale: Double) = {
      val f = if (getFont == null) java.awt.Font.decode(null) else getFont
      setFont(f.deriveFont(scale.floatValue * f.getSize2D))
      this
    }
    // withTicks(true)
    // withLabels(true)
    // withMajor((max-min)/10)
  }

  /** <tt>Cswing</tt> button and menu item components are constructed from
    * descriptors -- which are objects of class <tt>Act</tt>. These specify the
    * name of the component and the code to be run when the button is pressed.
    */
  abstract class Act(name: String)
      extends javax.swing.AbstractAction(name)
      with ActionSpec {
    def actionPerformed(ev: ActionEvent): Unit = { action }

    /** Set the short and Long descriptions and return this */
    def withHint(s: String) = {
      withShortDescription(s)
      withLongDescription(s)
    }

    /** Set the short description and return this */
    def withShortDescription(s: String) = {
      putValue(javax.swing.Action.SHORT_DESCRIPTION, s)
      this
    }

    /** Set the Long description and return this */
    def withLongDescription(s: String) = {
      putValue(javax.swing.Action.LONG_DESCRIPTION, s)
      this
    }

    /** Set an icon to be placed in components built from this and return this
      */
    def withIcon(icon: Icon) = {
      putValue(javax.swing.Action.SMALL_ICON, icon)
      this
    }

    /** Set the (initial) selection state of components to be built from this
      * and return this
      */
    def withSelected(state: Boolean) = {
      // putValue(javax.swing.Action.SELECTED_KEY, state) // 1.6 only
      putValue("SwingSelectedKey", state)
      this
    }

    withSelected(false)
    putValue(javax.swing.Action.NAME, name)
  }

  /** A group of selectable ButtonWidgets of which at most one can be selected
    * at the same time. Objects of this class implement the iterable Interface;
    * so they can be used as arguments to Col and Row constructors.
    */
  class ButtonGroup(buttons: Seq[ButtonWidget[Any]])
      extends javax.swing.ButtonGroup
      with Iterable[ButtonWidget[Any]] {
    for (b <- buttons) add(b)

    /** Iterator over the buttons of this group
      */
    def iterator = buttons.iterator

    /** Returns the selected button, if any
      */
    def getSelected: Option[ButtonWidget[Any]] = {
      val bs = for (b <- buttons if b.isSelected) yield b
      if (bs.length == 0) None else Some(bs(0))
    }

  }

  class HGlue extends JPanel with Widget[HGlue] {
    add(Box.createHorizontalGlue)
  }

  class VGlue extends JPanel with Widget[VGlue] { add(Box.createVerticalGlue) }

  def menuItem(name: String)(act: => Unit): JMenuItem =
    new JMenuItem(name, { case () => act })

  def checkBoxMenuItem(name: String)(act: Boolean => Unit) =
    new JCheckBoxMenuItem(name, act)

  def radioButtonMenuItem(name: String)(act: Boolean => Unit) =
    new JRadioButtonMenuItem(name, act)

  def checkBox(name: String)(act: Boolean => Unit) = new JCheckBox(name, act)

  def checkBox(name: String, state: Boolean)(act: Boolean => Unit) =
    new JCheckBox(name, act) withSelected state

  def button(name: String)(act: => Unit) = new JButton(name, { case () => act })

  def radioButton(name: String)(act: Boolean => Unit) =
    new JRadioButton(name, act)

  def radioButton(name: String, state: Boolean)(act: Boolean => Unit) =
    new JRadioButton(name, act) withSelected state

  def label(text: String, icon: Icon) = new JLabel(text, icon)

  def label(text: String) = new JLabel(text)

  def buttonGroup[W <: ButtonWidget[Any]](buttons: W*) = new ButtonGroup(
    buttons
  )

  def slider(min: Int, max: Int, value: Int, or: Orientation = Horizontal)(
      act: Int => Unit
  ) =
    new JSlider(min, max, value, or) { def onChange(value: Int) = act(value) }

  def textField(text: String, cols: Int)(act: String => Unit) =
    new JTextField(text, cols) { def action = act(getText) }

  def col(cs: Widget[Any]*) = new JCol(cs)

  def col(cs: Iterable[Widget[Any]]) = new JCol(cs)

  def row(cs: Widget[Any]*) = new JRow(cs)

  def row(cs: Iterable[Widget[Any]]) = new JRow(cs)

  def hGlue = new HGlue
  def vGlue = new VGlue

}
