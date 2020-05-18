module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as D


baseUrl =
    "/"


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Domain =
    { name : String
    , price : Int
    , sold : Bool
    }


type alias Domains =
    List Domain


type Model
    = Loading
    | Failure
    | Success Domains


type Msg
    = Loaded (Result Http.Error ListedDomains)


apiToDomains : ListedDomains -> Domains
apiToDomains lds =
    let
        price : ListedDomain -> Int
        price ld =
            String.toInt ld.amount
                |> Maybe.map (\a -> a // 1000000)
                |> Maybe.withDefault 987654321

        apiToDomain : ListedDomain -> Domain
        apiToDomain ld =
            Domain ld.name (price ld) False
    in
    List.map apiToDomain lds.domains
        |> List.sortBy (\d -> d.price)


type alias ListedDomain =
    { name : String
    , amount : String
    }


type alias ListedDomains =
    { domains : List ListedDomain
    }


domainDecoder : D.Decoder (List ListedDomain)
domainDecoder =
    D.list <|
        D.map2 ListedDomain
            (D.field "name" D.string)
            (D.field "amount" D.string)


listedDecoder : D.Decoder ListedDomains
listedDecoder =
    D.map ListedDomains (D.field "domains" domainDecoder)


getDomains : Cmd Msg
getDomains =
    Http.get
        { url = baseUrl ++ "/listed.json"
        , expect = Http.expectJson Loaded listedDecoder
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getDomains )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Loaded result ->
            case result of
                Ok listedDomains ->
                    ( Success (apiToDomains listedDomains), Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )


subscriptions : model -> Sub msg
subscriptions m =
    Sub.none


view : Model -> Html Msg
view model =
    div [ id "domains" ] (viewModel model)


viewModel : Model -> List (Html msg)
viewModel model =
    case model of
        Failure ->
            [ h2 [] [ text "Something's wrong :(" ]
            , p [] [ text "Could not load the list of domains... this is too bad really." ]
            ]

        Success domains ->
            viewDomains domains

        _ ->
            [ h2 [] [ text "Hello Happy Handshake Heroes" ]
            , p [] [ text "Loading the list of domains, this shouldn't take long..." ]
            ]


viewDomains : Domains -> List (Html msg)
viewDomains domains =
    [ div [ class "pure-g" ] (List.map viewDomain domains) ]


viewDomain : Domain -> Html msg
viewDomain domain =
    div [ class "domain" ]
        [ div [ class "pure-u-1-2 name" ] [ a [ href <| "https://www.namebase.io/domains/" ++ domain.name ] [ text domain.name ] ]
        , div [ class "pure-u-1-4 goog" ] [ a [ href <| "https://www.google.com/search?q=" ++ domain.name ] [ text "goog" ] ]
        , div [ class "pure-u-1-4 price" ] [ text <| String.fromInt domain.price ]
        ]
