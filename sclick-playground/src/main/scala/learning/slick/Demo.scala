package learning.slick

import learning.slick.domain.MessageRepository
import learning.slick.mysql.MySqlDBComponent

object Demo extends App {




  MySqlMessageRepository.createSchema


}

object MySqlMessageRepository extends MessageRepository with MySqlDBComponent