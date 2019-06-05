import Element.elem

abstract class Element {

  def contents: Array[String] // each String in the Array represents a line

  def height: Int = contents.length  // parameterless method
  // val height = contents.length // implemented as a field

  def width: Int = if (height == 0) 0 else contents(0).length // parameterless method
  // val width = if (height == 0) 0 else contents(0).length // implemented as a field

  def above(that: Element): Element = elem(this.contents ++ that.contents)

  def besides(that: Element) : Element = elem(
    for (
      (line1, line2) <- this.contents zip that.contents
    ) yield line1 + line2
  )

  override def toString = contents mkString "\n"
}



object Element {

  private class ArrayElement(val contents: Array[String]) extends Element {
  }

  private class LineElement(s: String) extends Element {
    val contents = Array(s)
    override def width = s.length
    override def height = 1
  }

  private class UniformElement (ch: Char, override val width:Int, override val height: Int) extends Element {
    private val line = ch.toString * width
    def contents = Array.fill(height)(line)
  }


  def elem(contents: Array[String]): Element = new ArrayElement(contents)

  def elem(chr: Char, width: Int, height: Int): Element = new UniformElement(chr, width, height)

  def elem(line: String): Element = new LineElement(line)
}