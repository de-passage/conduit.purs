module Authentication where

import Classes as C
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS

render :: forall w i. HH.HTML w i
render =
  HH.div [ HP.class_ C.authPage ]
    [ HH.div [ HP.classes [ BS.container, C.page ] ]
        [ HH.div [ HP.class_ BS.row ]
            [ HH.div [ HP.classes [ BS.colMd6, BS.offsetMd3, C.colXs12 ] ]
                [ HH.h1 [ HP.class_ C.textXsCenter ] [ HH.text "Sign up" ]
                , HH.p [ HP.class_ C.textXsCenter ]
                    [ HH.a [ HP.href "" ] [ HH.text "Have an account?" ]
                    ]
                , HH.ul [ HP.class_ C.errorMessages ]
                    [ HH.li_ [ HH.text "That email is already taken" ]
                    ]
                , HH.form_
                    [ HH.fieldset [ HP.class_ BS.formGroup ]
                        [ HH.input
                            [ HP.classes [ BS.formControl, BS.formControlLg ]
                            , HP.type_ HP.InputText
                            , HP.placeholder "Your Name"
                            ]
                        ]
                    , HH.fieldset [ HP.class_ BS.formGroup ]
                        [ HH.input
                            [ HP.classes [ BS.formControl, BS.formControlLg ]
                            , HP.type_ HP.InputEmail
                            , HP.placeholder "Email"
                            ]
                        ]
                    , HH.fieldset [ HP.class_ BS.formGroup ]
                        [ HH.input
                            [ HP.classes [ BS.formControl, BS.formControlLg ]
                            , HP.type_ HP.InputPassword
                            , HP.placeholder "Password"
                            ]
                        ]
                    , HH.button [ HP.classes [ BS.btn, BS.btnLg, BS.btnPrimary, C.pullXsRight ] ]
                        [ HH.text "Sign up"
                        ]
                    ]
                ]
            ]
        ]
    ]
