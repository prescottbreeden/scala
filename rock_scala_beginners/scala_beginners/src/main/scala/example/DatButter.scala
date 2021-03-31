package example

object social_network_exercise {
  val GenderOptions = Set(
    Some("Toast"),
    Some("Butter"),
    Some("Toasty Butter"),
    Some("Buttery Toast"),
    Some("Burnt Toast"),
    Some("Melted Butter"),
    Some("Lettuce"),
    Some("Butter Lettuce"),
    None
  )
  case class Friend(
      id: Int,
      name: String,
      gender: Option[String],
      friends: Set[Friend]
  )

  object SocialNetwork {
    val toast = """
            /' `\              /'
          /'     )         --/'--
        /'      /'____     /'    
      /'      /'/'    )  /'      
    /'      /'/'    /' /'        
(,/' (___,/' (___,/(__(__        
                                 
              _                                      
            /' `\              /'    /'              
          /'     )         --/'----/'--              
        /' (___,/'         /'    /' ____      ____   
      /'     )  /'    /  /'    /' /'    )   )'    )--
    /'      /'/'    /' /'    /' /(___,/'  /'         
(,/' (___,/' (___,/(__(__   (__(________/'           

♥♥♥ Social Networking for Toast That Need Love ♥♥♥
"""
    private var index = 0
    private var storage: Map[Int, Friend] = Map()

    def searchById(id: Int) = {
      val person = storage(id)
      val friends = person.friends map { f => s"\n\t${f.id}: ${f.name}" }
      s"\n${person.id}:: Name: ${person.name}, Gender: ${person.gender}, Friends: [${friends}]"
    }

    def addPerson(name: String, gender: Option[String]): Unit = {
      index = index + 1
      storage = storage + (index -> Friend(index, name, gender, Set()))
    }

    def removePerson(id: Int): Unit = {
      storage(id).friends foreach { f => unfriend(id, f.id) }
      storage = storage - id
    }

    def friend(id1: Int, id2: Int): Unit = {
      val person1 = storage(id1)
      val person2 = storage(id2)
      storage = storage +
        (id1 -> person1.copy(friends = person1.friends + person2)) +
        (id2 -> person2.copy(friends = person2.friends + person1))
    }

    def unfriend(id1: Int, id2: Int): Unit = {
      val person1 = storage(id1)
      val person2 = storage(id2)
      storage = storage +
        (id1 -> person1.copy(friends =
          person1.friends.filter(_.id != person2.id)
        )) +
        (id2 -> person2.copy(friends =
          person2.friends.filter(_.id != person1.id)
        ))
    }

    def numberOfFriends(id: Int): Int =
      if (!storage.contains(id)) 0
      else storage(id).friends.size

    def mostFriends: String = storage.maxBy(_._2.friends.size)._2.name
    def numberWithNoFriends: Int = storage.count(_._2.friends.isEmpty)

    def socialConnection(id1: Int, id2: Int): Boolean = {
      def bfs(
          target: Friend,
          consideredPeople: Set[Friend],
          discoveredPeople: Set[Friend]
      ): Boolean = {
        if (discoveredPeople.isEmpty) false
        else {
          val person = discoveredPeople.head
          if (person.id == target.id) true
          else if (consideredPeople.contains(person))
            bfs(target, consideredPeople, discoveredPeople.tail)
          else
            bfs(
              target,
              consideredPeople + person,
              discoveredPeople.tail ++ storage(person.id).friends
            )
        }
      }
      bfs(storage(id2), Set(), storage(id1).friends)
    }

  }

  val DatButter = SocialNetwork
  DatButter.addPerson("Bob Ross", Some("Burnt Toast"))
  DatButter.addPerson("Ross Bob", Some("Lettuce"))
  DatButter.addPerson("Furry Dinosaur", Some("Toast"))
  DatButter.addPerson("Flippant Flamingo", Some("Butter"))
  DatButter.addPerson("Dingo", gender = None)

  DatButter.friend(1, 2)
  DatButter.friend(1, 3)
  DatButter.friend(1, 4)
  DatButter.friend(2, 3)
  DatButter.friend(3, 4)

  DatButter.unfriend(3, 4)

  DatButter.numberOfFriends(1)
  DatButter.mostFriends
  DatButter.numberWithNoFriends

}
