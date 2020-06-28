module Pages.DevTools (Output(..), Slot, Query(..), component) where

import Prelude
import API.Url (UrlRepository)
import Classes as C
import Data.Array (cons)
import Data.Article as A
import Data.Const (Const)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Root (Root(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS
import Utils as Utils
import Web.Event.Internal.Types (Event)
import Web.UIEvent.MouseEvent as ME

type Input
  = Record ( urls :: UrlRepository, perPage :: A.PerPage )

data Output
  = RootChanged Root
  | PerPageChanged A.PerPage

type Query
  = Const Void

type Slot
  = H.Slot Query Output

data Option
  = Public
  | Localhost
  | Custom

derive instance eqOption :: Eq Option

data Action
  = ChangeRoot
  | ChangeCustomRootText String
  | ChangeLocalhostPort Int
  | Select Option
  | PreventDefault Event (Maybe Action)
  | ChangePerPage A.PerPage
  | ApplyPerPageChange

type State
  = Record
      ( urls :: UrlRepository
      , customRootText :: String
      , localHostPort :: Int
      , localSelection :: Option
      , perPage :: A.PerPage
      )

type ChildSlots
  = ()

component :: forall q m. MonadEffect m => H.Component HH.HTML q Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              }
    }
  where
  initialState :: Input -> State
  initialState { urls, perPage } =
    let
      (customRootText /\ localHostPort /\ localSelection) = case urls.root of
        PublicApi -> (show PublicApi) /\ 8080 /\ Public
        LocalHost port -> (show PublicApi) /\ port /\ Localhost
        CustomBackend url -> url /\ 8080 /\ Custom
    in
      { urls
      , customRootText
      , localSelection
      , localHostPort
      , perPage
      }

  render :: State -> H.ComponentHTML Action () m
  render s =
    let
      radio opt title content =
        HH.div [ HP.class_ C.radio ]
          ( HH.label_
              [ HH.input
                  [ HP.type_ HP.InputRadio
                  , HP.checked (opt == s.localSelection)
                  , HE.onChecked \_ -> Just (Select opt)
                  ]
              , HH.text " "
              , HH.text title
              ]
              `cons`
                content
          )

      public =
        radio Public "Public API"
          [ HH.p_
              [ HH.text
                  """The default API for Conduit.
              It is shared between all conduit front end implementations and contains mostly junk content."""
              ]
          ]

      localhost =
        radio Localhost "Local Host"
          [ HH.div [ HP.class_ BS.formGroup ]
              [ HH.label [] [ HH.text "Port" ]
              , HH.input
                  [ HP.class_ BS.formControl
                  , HP.type_ HP.InputNumber
                  , HE.onValueChange \v -> fromString v <#> ChangeLocalhostPort
                  , HP.value (show s.localHostPort)
                  ]
              ]
          ]

      custom =
        radio Custom "Custom backend"
          [ HH.div [ HP.class_ BS.formGroup ]
              [ HH.label [] [ HH.text "Url" ]
              , HH.input
                  [ HP.class_ BS.formControl
                  , HP.type_ HP.InputUrl
                  , HE.onValueChange \v -> Just $ ChangeCustomRootText v
                  , HP.value s.customRootText
                  ]
              ]
          ]
    in
      HH.div [ HP.classes [ BS.container, C.page ] ]
        [ HH.div [ HP.class_ BS.row ]
            [ HH.div [ HP.classes [ BS.colMd10, BS.offsetMd1 ] ]
                [ HH.form
                    [ HP.id_ "root-control" ]
                    [ public
                    , localhost
                    , custom
                    , HH.button [ HP.classes [ BS.btn, BS.btnPrimary ], HE.onClick $ preventDefault ChangeRoot ]
                        [ HH.text "Apply" ]
                    ]
                , HH.hr_
                , HH.form [ HP.class_ BS.formInline ]
                    [ HH.div [ HP.class_ BS.formGroup ]
                        [ HH.label_
                            [ HH.text "Articles per pages: "
                            , HH.input
                                [ HP.type_ HP.InputNumber
                                , HP.min 1.0
                                , HP.class_ BS.formControl
                                , HE.onValueChange $ fromString >=> (Just <<< ChangePerPage <<< A.perPage)
                                , HP.value (show s.perPage)
                                ]
                            ]
                        , HH.button
                            [ HP.classes [ BS.btn, BS.btnPrimary ]
                            , HE.onClick $ preventDefault ApplyPerPageChange
                            ]
                            [ HH.text "Apply" ]
                        ]
                    ]
                ]
            ]
        ]

  handleAction :: Action -> H.HalogenM State Action ChildSlots Output m Unit
  handleAction = case _ of
    ChangeRoot -> do
      { localSelection, customRootText, localHostPort } <- H.get
      let
        root = case localSelection of
          Public -> PublicApi
          Localhost -> LocalHost localHostPort
          Custom -> CustomBackend customRootText
      H.raise $ RootChanged root
    ChangeCustomRootText s -> do
      H.modify_ _ { customRootText = s }
    ChangeLocalhostPort port -> do
      H.modify_ _ { localHostPort = port }
    Select option -> H.modify_ _ { localSelection = option }
    PreventDefault event action -> Utils.preventDefault event action handleAction
    ChangePerPage perPage -> H.modify_ _ { perPage = perPage }
    ApplyPerPageChange -> do
      perPage <- H.gets _.perPage
      H.raise $ PerPageChanged perPage

  preventDefault :: Action -> ME.MouseEvent -> Maybe Action
  preventDefault action event = Just $ PreventDefault (ME.toEvent event) $ Just action
