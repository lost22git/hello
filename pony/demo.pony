
interface Named
  fun name(): String

trait HasName
  fun name() => "DEFAULT NAME"

class Book is HasName
  let name: String = "DEFAULT BOOK NAME"
  fun name() => name

primitive H
  fun get_name(a: Named) => a.name()


actor Main
  new create(env: Env) =>
    env.out.print("Hello, Pony")
    env.out.print(H.get_name(Book()))
