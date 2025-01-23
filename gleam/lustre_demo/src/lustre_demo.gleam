import lustre
import lustre/element
import my_component

pub fn main() {
  // register name must not be underlined `_`
  let assert Ok(_) = my_component.register("my-component")
  let app = lustre.element(element.element("my-component", [], []))
  let assert Ok(_) = lustre.start(app, "#app", Nil)
  Nil
}
