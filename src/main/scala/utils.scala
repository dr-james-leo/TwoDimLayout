
abstract class Element {
  def contents: Array[String] // each String in the Array represents a line

  def height: Int = contents.length  // parameterless method
  // val height = contents.length // implemented as a field
  
  def width: Int = if (height == 0) 0 else contents(0).length // parameterless method
  // val width = if (height == 0) 0 else contents(0).length // implemented as a field

  def above(that: Element): Element = new ArrayElement(this.contents ++ that.contents)

  def besides(that: Element) : Element = new ArrayElement(
    for (
      (line1, line2) <- this.contents zip that.contents
    ) yield line1 + line2
  )

  override def toString = contents mkString "\n"
}

class ArrayElement(const: Array[String]) extends Element {
  val contents: Array[String] = const
}

class LineElement(s: String) extends Element {
  val contents = Array(s)
  override def width = s.length
  override def height = 1
}