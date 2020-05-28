module Pages.Edition where

import Classes as C
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS

render :: forall w i. HH.HTML w i
render =
  HH.div [ HP.class_ C.editorPage ]
    [ HH.div [ HP.classes [ BS.container, C.page ] ]
        [ HH.div [ HP.class_ BS.row ]
            [ HH.div [ HP.classes [ BS.colMd10, BS.offsetMd1 ] ]
                [ HH.form_
                    [ HH.fieldset_
                        [ HH.fieldset [ HP.class_ BS.formGroup ]
                            [ HH.input
                                [ HP.classes [ BS.formControl, BS.formControlLg ]
                                , HP.type_ HP.InputText
                                , HP.placeholder "Article Title"
                                ]
                            ]
                        , HH.fieldset [ HP.class_ BS.formGroup ]
                            [ HH.input
                                [ HP.class_ BS.formControl
                                , HP.type_ HP.InputText
                                , HP.placeholder "What's this article about?"
                                ]
                            ]
                        , HH.fieldset [ HP.class_ BS.formGroup ]
                            [ HH.textarea
                                [ HP.class_ BS.formControl
                                , HP.rows 8
                                , HP.placeholder "Write your article (in markdown)"
                                ]
                            ]
                        , HH.fieldset [ HP.class_ BS.formGroup ]
                            [ HH.input
                                [ HP.class_ BS.formControl
                                , HP.type_ HP.InputText
                                , HP.placeholder "Enter tags"
                                ]
                            , HH.div [ HP.class_ C.tagList ] []
                            ]
                        , HH.button [ HP.classes [ BS.btn, BS.btnLg, BS.btnPrimary, C.pullXsRight ] ]
                            [ HH.text "Publish Article"
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]
