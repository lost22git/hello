app [Model, server] { pf: platform "https://github.com/roc-lang/basic-webserver/releases/download/0.9.0/taU2jQuBf-wB8EJb0hAkrYLYOGacUU5Y9reiHG45IY4.tar.br" }

import pf.Stdout
import pf.Http exposing [Request, Response]
import pf.Url

Model : {}

server = { init: Task.ok {}, respond }

respond : Request, Model -> Task Response [ServerErr Str]_
respond = \req, _ -> apiProxy req |> Task.mapErr handleErr

AppError : [
    ParamNotFound Str,
    HttpErr Http.Err,
    StdoutErr Stdout.Err,
]

handleErr : AppError -> [ServerErr Str]
handleErr = \e -> ServerErr (Inspect.toStr e)

apiProxy : Request -> Task Response AppError
apiProxy = \req ->
    target =
        req.url
            |> Url.fromStr
            |> Url.queryParams
            |> Dict.get "target"
            |> Task.fromResult
            |> Task.mapErr! \_ -> ParamNotFound "target"

    Stdout.line! "Fetching [$(Http.methodToStr req.method)] $(target)"

    targetRes = { req & url: target, timeout: NoTimeout } |> Http.send!
    dbg targetRes

    Task.ok targetRes

# Task.ok { targetRes & status: 200 }

