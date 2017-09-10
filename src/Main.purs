module Main where

import Prelude
import Control.Monad.Eff (Eff)

import Thermite as T

import React (ReactElement, createFactory) as R
import React.DOM as R
import React.DOM.Props as RP
import ReactDOM as RDOM

import DOM (DOM) as DOM
import DOM.HTML (window) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.NonElementParentNode (getElementById) as DOM
import DOM.HTML.Types (htmlDocumentToDocument) as DOM
import DOM.Node.Types (ElementId(..), documentToNonElementParentNode) as DOM

import Data.Foldable (traverse_)
import Unsafe.Coerce (unsafeCoerce) -- This seems to be the standard way
                                    -- to get to events :- /
import Global (readFloat)
import Data.Int (ceil)

import Factorio


-- State captures only values of the inputs
type State = { quantity :: Number, time :: Number, item :: Item }

initialState :: State
initialState = {quantity: 100.0, time: 60.0, item: rocketPart}


-- Event types used to control the interface
data Action = QuantityChange Number | TimeChange Number | SelectItem Item


-- Event handler
performAction :: T.PerformAction _ State _ Action
performAction (QuantityChange q) _ _ = void $ T.modifyState (_ {quantity = q })
performAction (TimeChange t) _ _ = void $ T.modifyState (_ {time = t })
performAction (SelectItem i) _ _ = void $ T.modifyState (_ {item = i})


render :: T.Render State _ Action
render dispatch _ state _ =
  [ R.form [ RP.className "form-inline" ] $
    quantityInput -- "I want this many"
    <>
    [ R.span [ RP.className "mx-sm-2" ] [ R.text " of " ] ]
    <> map createSelectionItem compositeItems
    <> timeInput -- "in .. seconds."
    <>
    [ R.div
      [ RP.className "result-wrapper" ]
      [ factoryComponent state.item state.quantity state.time ]
    ]
  ]
  where
    -- there seems to be a space to reduce redundancy in the inputs
    -- below
    quantityInput =
      [ R.label
        [ RP.className "mr-sm-2"
        , RP.htmlFor "quantity"
        ]
        [ R.text "I want " ]
      , R.input
        [ RP._id "quantity"
        , RP.className "form-control"
        , RP.defaultValue (show <<< ceil $ state.quantity)
        , RP.size 5
        , RP.onChange \e -> dispatch (QuantityChange (readFloat (unsafeCoerce e).target.value))
        ] []
      ]

    timeInput =
      [ R.label
        [ RP.className "mx-sm-2"
        , RP.htmlFor "time"
        ]
        [ R. text " in " ]
      , R.input
        [ RP._id "time"
        , RP.className "form-control"
        , RP.defaultValue (show <<< ceil $ state.time)
        , RP.size 5
        , RP.onChange \e -> dispatch (TimeChange (readFloat (unsafeCoerce e).target.value))
        ] []
      , R.span [ RP.className "ml-sm-2" ] [ R.text " seconds. " ]
      ]

    -- One element in the item selection
    createSelectionItem item = R.a
      [ RP.onClick \e -> dispatch (SelectItem item)
      , RP.className ("item-selection " <> selected)
      ]
      [ itemImageTag item ]
      where selected = if state.item == item then "selected" else ""

    -- One block describing how many factories are needed
    factoryComponent item@(Atom atom) quantity time =
      R.div
        [ RP.className "factory-block" ]
        [ R.text $ show quantity <> " "
        , itemImageTag item
        , R.text $ " " <> atom.name <> " in " <> show time <> " seconds."
        ]
    factoryComponent item@(Composite comp) quantity time =
      R.div
        [ RP.className "factory-block" ] $
        [ R.text (show quantity <> " ")
        , itemImageTag item
        , R.text $ " " <> comp.name <> " in " <> show time <> " seconds requires "
            <> show (factories item quantity time) <> " "
        , factoryImageTag comp.madeIn
        , R.text $ " " <> factoryNamePlural comp.madeIn <> " and also "
        ]
        <> map go comp.requirements
      where
        -- Create factory components for all the requirements
        go (Requirement req) = factoryComponent req.item totalReqQuantity time
          where totalReqQuantity = req.quantity * (processes quantity item)


itemImageTag :: Item -> R.ReactElement
itemImageTag (Atom item) = itemImageTag' item
itemImageTag (Composite item) = itemImageTag' item
itemImageTag' item = R.img ([ RP.src item.image ] <> altAndTitle item.name) []


factoryImageTag :: Factory -> R.ReactElement
factoryImageTag Assembler = R.img
  ([ RP.src "img/Assembling_machine_3.png" ] <> altAndTitle "Assembling machine 3") []
factoryImageTag ChemicalPlant = R.img
  ([ RP.src "img/Chemical_plant.png" ] <> altAndTitle "Chemical plant") []
factoryImageTag OilRefinery = R.img
  ([ RP.src "img/Oil_refinery.png" ] <> altAndTitle "Oil refinery") []
factoryImageTag ElectricFurnace = R.img
  ([ RP.src "img/Electric_furnace.png" ] <> altAndTitle "Electric furnace") []
factoryImageTag RocketSilo = R.img
  ([ RP.src "img/Rocket_silo.png" ] <> altAndTitle "Rocket silo") []


factoryNamePlural :: Factory -> String
factoryNamePlural Assembler       = "assembling machines"
factoryNamePlural ChemicalPlant   = "chemical plants"
factoryNamePlural OilRefinery     = "oil refineries"
factoryNamePlural ElectricFurnace = "electric furnaces"
factoryNamePlural RocketSilo      = "rocket silos"


-- Creates alternative and title attributes for <img> with the same value
altAndTitle :: String -> Array RP.Props
altAndTitle s = [ RP.alt s, RP.title s]


main :: forall eff. Eff ( dom :: DOM.DOM | eff ) Unit
main = void do
  let component = T.createClass (T.simpleSpec performAction render) initialState
  let props = unit
  document <- DOM.window >>= DOM.document
  container <- DOM.getElementById (DOM.ElementId "calculator") (DOM.documentToNonElementParentNode (DOM.htmlDocumentToDocument document))
  traverse_ (RDOM.render (R.createFactory component props)) container
