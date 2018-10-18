package pl.softech

import java.time._

import io.circe.syntax._

object DemoAutoDerivation extends App {

  import io.circe.generic.auto._
  import io.circe.java8.time._

  val movie = Movie("Coherence", Drama, List(
    RemoteReview(Some("http://www.imdb.com/title/tt2866360/reviews?ref_=tt_ov_rt"), 9),
    UserReview(UserId("ab38d19dj"), "Absolutely brilliant", 10)
  ), LocalDate.of(2014, 8, 6))

  println(movie.asJson.spaces2)

}
