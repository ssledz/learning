package io.github.ssledz.domain

case class User(
                 userName: String,
                 email: String,
                 firstName: String,
                 lastName: String,
                 hash: String,
                 id: Option[Long] = None
               )
