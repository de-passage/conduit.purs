module Pages.Settings where

import Classes as C
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS

render :: forall w i. HH.HTML w i
render =
  HH.div [ HP.class_ C.settingsPage ]
    [ HH.div [ HP.classes [ BS.container, C.page ] ]
        [ HH.div [ HP.class_ BS.row ]
            [ HH.div [ HP.classes [ BS.colMd6, BS.offsetMd3, C.colXs12 ] ]
                [ HH.h1 [ HP.class_ C.textXsCenter ] [ HH.text "Your Settings" ]
                , HH.form_
                    [ HH.fieldset_
                        [ HH.fieldset [ HP.class_ BS.formGroup ]
                            [ HH.input
                                [ HP.class_ BS.formControl
                                , HP.type_ HP.InputText
                                , HP.placeholder "URL of profile picture"
                                ]
                            ]
                        , HH.fieldset [ HP.class_ BS.formGroup ]
                            [ HH.input
                                [ HP.classes [ BS.formControl, BS.formControlLg ]
                                , HP.type_ HP.InputText
                                , HP.placeholder "Your Name"
                                ]
                            ]
                        , HH.fieldset [ HP.class_ BS.formGroup ]
                            [ HH.textarea
                                [ HP.classes [ BS.formControl, BS.formControlLg ]
                                , HP.placeholder "Short bio about you"
                                , HP.rows 8
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
                            [ HH.text "Update Settings"
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]
