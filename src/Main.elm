module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import Json.Decode as D
import List.Extra
import Regex



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


type Status
    = Loading
    | Failure
    | Success


type alias Model =
    { status : Status
    , domains : Domains
    , search : String
    , regex : Maybe Regex.Regex
    , shownDomains : Domains
    }


type alias LoadedDomains =
    Result Http.Error ListedDomains


type Msg
    = Loaded LoadedDomains
    | Search String


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


filterDomains : Regex.Regex -> Domains -> Domains
filterDomains regex domains =
    List.filter (\d -> Regex.contains regex d.name) domains


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
        { url = "/listed.json"
        , expect = Http.expectJson Loaded listedDecoder
        }



-- init/update


emptyRegex : Regex.Regex
emptyRegex =
    Maybe.withDefault Regex.never <| Regex.fromString ""


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Loading [] "" (Just emptyRegex) [], getDomains )


updateLoaded : LoadedDomains -> Model -> ( Model, Cmd msg )
updateLoaded result model =
    case result of
        Ok listedDomains ->
            let
                domains =
                    apiToDomains listedDomains
            in
            ( { model
                | status = Success
                , domains = domains
                , shownDomains = domains
              }
            , Cmd.none
            )

        Err _ ->
            ( { model | status = Failure }, Cmd.none )


updateSearch : String -> Model -> ( Model, Cmd msg )
updateSearch search model =
    let
        maybeRegex =
            Regex.fromString search

        shownDomains =
            case maybeRegex of
                Just regex ->
                    filterDomains regex model.domains

                Nothing ->
                    model.shownDomains
    in
    ( { model
        | search = search
        , regex = maybeRegex
        , shownDomains = shownDomains
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Loaded result ->
            updateLoaded result model

        Search search ->
            updateSearch search model


subscriptions : model -> Sub msg
subscriptions m =
    Sub.none



-- view


view : Model -> Html Msg
view model =
    div [ id "domains" ] (viewModel model)


regexClass : Maybe Regex.Regex -> String
regexClass regex =
    case regex of
        Just _ ->
            ""

        Nothing ->
            "error"


viewModel : Model -> List (Html Msg)
viewModel model =
    case model.status of
        Failure ->
            [ h2 [] [ text "Something's wrong :(" ]
            , p [] [ text "Could not load the list of domains... this is too bad really." ]
            ]

        Success ->
            [ h2 [] [ text "Hello Happy Handshake Heroes. Here, Have Heaps:" ]
            , input
                [ placeholder "Regex search"
                , value model.search
                , onInput Search
                , class (regexClass model.regex)
                ]
                []
            ]
                ++ (model.shownDomains |> groupDomains |> viewGroups)

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
