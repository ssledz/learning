package pl.softech

import java.time._

sealed trait Genre
case object Action extends Genre
case object Comedy extends Genre
case object Drama extends Genre

case class CriticId(value: String) extends AnyVal
case class UserId(value: String) extends AnyVal

sealed trait Review
case class RemoteReview(url: Option[String], score: Int) extends Review
case class CriticReview(cricitId: CriticId, review: String, score: Int) extends Review
case class UserReview(userId: UserId, review: String, score: Int) extends Review

case class Movie(name: String, genre: Genre, reviews: List[Review], releaseDate: LocalDate)
