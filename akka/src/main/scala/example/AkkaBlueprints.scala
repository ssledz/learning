package example

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.util.Timeout
import scala.concurrent.duration._
import scala.language.postfixOps

object EchoActor {

  sealed trait Message

  case class Echo(message: String) extends Message

  case class ReversEcho(message: String) extends Message

  def props(printer: ActorRef) = Props(classOf[EchoActor], printer)

}

class ConsolePrinter extends Actor {
  def receive: Receive = {
    case message => println(s"printing: $message")
  }
}

class EchoActor(printer: ActorRef) extends Actor {

  import EchoActor._

  override def preStart(): Unit = {
    println(s"EchoActor: Pre start")
  }

  override def postStop(): Unit = {
    println("EchoActor: Post stop")
  }

  def receive: Receive = {

    case Echo(m) => {
      sender() ! s"Echoed: $m"
      printer ! m
    }

  }
}

object AkkaBlueprints extends App {

  val system = ActorSystem("actor-system")

  val printer = system.actorOf(Props[ConsolePrinter], "console-printer")

  val echoActor = system.actorOf(EchoActor.props(printer), "echo-actor")

  implicit val timeout = Timeout(5 seconds)

  import akka.pattern.ask
  import scala.concurrent.ExecutionContext.Implicits.global

  val response =  ask(echoActor, EchoActor.Echo("a message"))

  response.foreach(resp => println(s"Response from actor: $resp"))

  system.terminate()
  
  Thread.sleep(2000)


}
