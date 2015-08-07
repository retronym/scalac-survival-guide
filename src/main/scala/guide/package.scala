package object guide {
  val Width = 80
  def --------------- = println("\n" + ("-" * Width) + "\n")

  def ===(s: String) = p(title('=', s))
  def ---(s: String) = p(title('-', s))

  def title(filler: Char, s: String) = {
    val diff = Width - s.length - 1
    val l = diff / 2
    val r = diff - l
    "\n" + ("=" * l) + " " + s + " " + ("=" * r) + "\n"
  }

  def p(a: Any) = println(a)

}
