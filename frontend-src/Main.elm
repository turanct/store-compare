import Html exposing (..)
import Html.Attributes exposing (class, placeholder, href)
import Html.Events exposing (onInput, onClick, onSubmit)
import Http
import Json.Decode as Decode


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Product =
  { name : String
  , price : String
  , url : String
  }

type alias Store =
  { name : String
  , products : List Product
  }

type alias Model =
  { searchTerm : String
  , storesAndProducts : List Store
  }

init : (Model, Cmd Msg)
init =
  ( { searchTerm = "" , storesAndProducts = [] }
  , Cmd.none
  )


-- UPDATE

type Msg
  = Change String
  | Search
  | NewData (Result Http.Error (List Store))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Change newContent ->
      ({ model | searchTerm = newContent }, Cmd.none)

    Search ->
      (model, searchProducts model.searchTerm)

    NewData (Ok stores) ->
      (Model model.searchTerm stores, Cmd.none)

    NewData (Err _) ->
      (model, Cmd.none)


-- VIEW

view : Model -> Html Msg
view model =
  div [ class "wrapper"]
    [ div [ class "search"]
      [ form [ onSubmit Search ]
        [ input [ placeholder "Product to compare", onInput Change ] []
        , button [ onClick Search ] [ text "compare stores" ]
        ]]
    , div [ class "stores"]
        (List.map
          (\store -> div [ class "store" ]
            [ div [ class "store-name" ] [ h1 [] [ text store.name ] ]
            , div [ class "store-products" ]
              (List.map
                (\product -> div [ class "product" ]
                    [ div [ class "product-price" ] [ text product.price ]
                    , div [ class "product-name" ] [ text product.name ]
                    , div [ class "product-link" ] [ a [ href product.url ] [ text "url" ] ] ])
                store.products
              )
            ])
          (List.filter
            (\store -> List.length store.products > 0)
            model.storesAndProducts))
    ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- HTTP

searchProducts : String -> Cmd Msg
searchProducts searchTerm =
  let
    url =
      "/product/" ++ searchTerm
  in
    Http.send NewData (Http.get url decodeStores)


decodeStores : Decode.Decoder (List Store)
decodeStores =
  Decode.list store

store : Decode.Decoder Store
store =
  Decode.map2 Store
    (Decode.at ["name"] Decode.string)
    (Decode.at ["products"] (Decode.list product))

product : Decode.Decoder Product
product =
  Decode.map3 Product
    (Decode.at ["name"] Decode.string)
    (Decode.at ["price"] Decode.string)
    (Decode.at ["url"] Decode.string)
