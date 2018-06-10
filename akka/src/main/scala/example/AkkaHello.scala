package example

import akka.actor.{Actor, ActorSystem, Props}

class HelloWorldActor extends Actor {

  override def receive: Receive = {
    case message => println(s"Hello world $message !!!")
  }
}

object AkkaHello extends App {

  val system = ActorSystem("actor-system")

  val actor = system.actorOf(Props[HelloWorldActor], "helloWorldActor")

  actor ! "Hi"

}
