package Channel

sealed trait Action
case class Channel(name: String)
case class Subscribe(channel: Channel) extends Action
case class Unsubscribe(channel: Channel) extends Action
case class PostMessage(channel: Channel, message: String) extends Action

val subscribeEffectiveScala =
  Subscribe(Channel("effective-scala"))

val scala: Channel = Channel("scala")

@main def main(): Unit =
  println(scala)
  println(scala.name)
