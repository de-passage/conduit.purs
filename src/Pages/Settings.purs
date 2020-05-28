module Pages.Settings where

import Classes as C
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as BS

render :: forall w i. HH.HTML w i
render =
-- <div class="settings-page">
    HH.div [ HP.class_ C.settingsPage ] [
--   <div class="container page">
        HH.div [ HP.class_ BS.container ] [
--     <div class="row">
            HH.div [ HP.class_ BS.row ] [
--       <div class="col-md-6 offset-md-3 col-xs-12">
                HH.div [ HP.classes [ BS.colMd6, BS.offsetMd3, C.colXs12 ] ] [
--         <h1 class="text-xs-center">Your Settings</h1>
                    HH.h1 [ HP.class_ C.textXsCenter ] [ HH.text "Your Settings" ]
--         <form>
                    , HH.form_ [
--           <fieldset>
                        HH.fieldset_ [
--               <fieldset class="form-group">
                            HH.fieldset [ HP.class_ BS.formGroup ] [
--                 <input class="form-control" type="text" placeholder="URL of profile picture">
                                HH.input [ HP.class_ BS.formControl, HP.type_ HP.InputText
                                         , HP.placeholder "URL of profile picture" ]
--               </fieldset>
                            ]
--               <fieldset class="form-group">
                            , HH.fieldset [ HP.class_ BS.formGroup ] [
--                 <input class="form-control form-control-lg" type="text" placeholder="Your Name">
                                HH.input [ HP.classes [ BS.formControl, BS.formControlLg ]
                                         , HP.type_ HP.InputText
                                         , HP.placeholder "Your Name" ]
--               </fieldset>
                            ]
--               <fieldset class="form-group">
                            , HH.fieldset [ HP.class_ BS.formGroup ] [
--                 <textarea class="form-control form-control-lg" rows="8" placeholder="Short bio about you"></textarea>
                                HH.textarea [ HP.classes [ BS.formControl, BS.formControlLg ]
                                         , HP.placeholder "Short bio about you"
                                         , HP.rows 8 ]
--               </fieldset>
                            ]
--               <fieldset class="form-group">
                            , HH.fieldset [ HP.class_ BS.formGroup ] [
--                 <input class="form-control form-control-lg" type="text" placeholder="Email">
                                HH.input [ HP.classes [ BS.formControl, BS.formControlLg ]
                                         , HP.type_ HP.InputEmail
                                         , HP.placeholder "Email" ]
--               </fieldset>
                            ]
--               <fieldset class="form-group">
                            , HH.fieldset [ HP.class_ BS.formGroup ] [
--                 <input class="form-control form-control-lg" type="password" placeholder="Password">
                                HH.input [ HP.classes [ BS.formControl, BS.formControlLg ]
                                         , HP.type_ HP.InputPassword
                                         , HP.placeholder "Password" ]
--               </fieldset>
                            ]
--               <button class="btn btn-lg btn-primary pull-xs-right">
                            , HH.button [ HP.classes [ BS.btn, BS.btnLg, BS.btnPrimary, C.pullXsRight ] ]
--                 Update Settings
                                [ HH.text "Update Settings"
--               </button>
                                ]
--           </fieldset>
                        ]
--         </form>
                    ]
--       </div>
                ]
--     </div>
            ]
--   </div>
        ]
-- </div>
    ]