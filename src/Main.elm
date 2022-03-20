module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as D
import List.Extra


baseUrl =
    "http://domains.tasuki/"



-- model


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


type alias GroupedDomains =
    List ( Int, Domains )


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


groupDomains : Domains -> GroupedDomains
groupDomains domains =
    let
        nextIndex : Int -> Int
        nextIndex i =
            if String.startsWith "1" (String.fromInt i) then
                i * 5

            else
                i * 2

        helper : Domains -> Int -> GroupedDomains -> GroupedDomains
        helper ds limit acc =
            case List.Extra.splitWhen (\d -> d.price >= limit) ds of
                Just ( lower, bigger ) ->
                    helper bigger (nextIndex limit) (( limit, lower ) :: acc)

                Nothing ->
                    ( limit, ds ) :: acc
    in
    helper domains 100 [] |> List.reverse



-- http things


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



-- init/update


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



-- view


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
            [ h2 [] [ text "Hello Happy Handshake Heroes. Here, Have Heaps:" ] ]
                ++ (groupDomains domains |> viewGroups)

        _ ->
            [ h2 [] [ text "Hello Happy Handshake Heroes" ]
            , p [] [ text "Loading the list of domains, this shouldn't take long..." ]
            ]


viewGroups : GroupedDomains -> List (Html msg)
viewGroups grouped =
    List.map viewGroup grouped


viewGroup : ( Int, Domains ) -> Html msg
viewGroup group =
    case group of
        ( under, domains ) ->
            div []
                [ div [ class "group" ] [ text <| "<" ++ String.fromInt under ]
                , div [ class "pure-g" ] (List.map viewDomain domains)
                ]


viewDomain : Domain -> Html msg
viewDomain domain =
    div [ class "domain" ]
        [ div [ class "pure-u-2-3 name" ] [ a [ href <| "https://www.namebase.io/domains/" ++ domain.name ] [ text domain.name ] ]
        , div [ class "pure-u-1-6 goog" ] [ a [ href <| "https://www.google.com/search?q=" ++ domain.name ] [ text "g" ] ]
        , div [ class "pure-u-1-6 price" ] [ text <| String.fromInt domain.price ]
        ]
