import gleam/dynamic
import gleam/int
import gleam/list
import lustre
import lustre/attribute
import lustre/effect
import lustre/element
import lustre/element/html
import lustre/event
import lustre_http

/// register this component to luster runtime with a given name
///
pub fn register(name: String) -> Result(Nil, lustre.Error) {
  lustre.application(init, update, view)
  |> lustre.register(name)
}

// === model ===

pub type Model {
  Model(count: Int, cats: List(Cat))
}

pub type Cat {
  Cat(id: String, url: String)
}

pub fn init(_) -> #(Model, effect.Effect(Msg)) {
  #(Model(0, []), effect.none())
}

pub type Msg {
  UserIncrementedCount
  UserDecrementedCount
  ApiReturnedCats(Result(List(Cat), lustre_http.HttpError))
}

// === update ===

pub fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    UserIncrementedCount -> #(
      Model(..model, count: model.count + 1),
      get_cats(),
    )
    UserDecrementedCount ->
      case model.count == 0 {
        True -> #(model, effect.none())
        False -> #(
          Model(count: model.count - 1, cats: list.drop(model.cats, 1)),
          effect.none(),
        )
      }
    ApiReturnedCats(Ok(api_cats)) -> {
      let assert [cat, ..] = api_cats
      #(Model(..model, cats: [cat, ..model.cats]), effect.none())
    }
    ApiReturnedCats(Error(_)) -> #(model, effect.none())
  }
}

fn get_cats() -> effect.Effect(Msg) {
  let decoder =
    dynamic.decode2(
      Cat,
      dynamic.field("id", dynamic.string),
      dynamic.field("url", dynamic.string),
    )

  let expect = lustre_http.expect_json(dynamic.list(decoder), ApiReturnedCats)
  lustre_http.get("https://api.thecatapi.com/v1/images/search", expect)
}

// === view ===

pub fn view(model: Model) -> element.Element(Msg) {
  let count = int.to_string(model.count)

  html.div([], [
    html.button([event.on_click(UserIncrementedCount)], [element.text("+")]),
    html.text(count),
    html.button(
      [
        attribute.disabled(model.count <= 0),
        event.on_click(UserDecrementedCount),
      ],
      [element.text("-")],
    ),
    html.div([], {
      use Cat(id: _, url:) <- list.map(model.cats)
      html.img([attribute.src(url), attribute.width(400), attribute.height(400)])
    }),
  ])
}
