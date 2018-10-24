package pl.softech

import io.circe._
import io.circe.generic.extras._
import io.circe.generic.semiauto._
import io.circe.java8.time.TimeInstances

object json {

  object codecs {
    import auto._
    implicit val movieEncoder: Encoder[Movie] = deriveEncoder[Movie]
    implicit val movieDecoder: Decoder[Movie] = deriveDecoder[Movie]
  }

  private object auto extends AutoDerivation with TimeInstances {

    import shapeless._

    implicit def encoderValueClass[T <: AnyVal, V](implicit
                                                   g: Lazy[Generic.Aux[T, V :: HNil]],
                                                   e: Encoder[V]
                                                  ): Encoder[T] = Encoder.instance { value =>
      e(g.value.to(value).head)
    }

    implicit def decoderValueClass[T <: AnyVal, V](implicit
                                                   g: Lazy[Generic.Aux[T, V :: HNil]],
                                                   d: Decoder[V]
                                                  ): Decoder[T] = Decoder.instance { cursor =>
      d(cursor).map { value ⇒
        g.value.from(value :: HNil)
      }
    }

    implicit val configuration: Configuration = Configuration.default
      .withDiscriminator("type")
  }

}
