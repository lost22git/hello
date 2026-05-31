mod Pokemon {
  use Net.Http
  use Net.HttpResponse

  pub def findPokemon(name: String): Result[IoError, String] \ { Http, Logger } = {
    run {
      // forM - do notation in Haskell.
      // yield - return in Haskell.
      forM (
          response <- Http.get("/pokemon/${name}");
          statusOkResponse <- HttpResponse.expect(response)
      ) yield {
        HttpResponse.body(statusOkResponse)
      }
    } with Http.withBaseUrl("https://pokeapi.co/api/v2")
      with Http.withLogging
  }
}
