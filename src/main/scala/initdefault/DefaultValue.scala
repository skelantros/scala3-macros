package initdefault

final case class DefaultValue[A](x: A)

object DefaultValue {
    given defInt: DefaultValue[Int] = DefaultValue(3)
    given defBool: DefaultValue[Boolean] = DefaultValue(true)
    given defStr: DefaultValue[String] = DefaultValue("hello")
}
