module Factorio where

import Prelude
import Data.String (joinWith)

-- "process" is one run of factory to create item.quantity pieces


data Factory = Assembler | ChemicalPlant | OilRefinery | ElectricFurnace | RocketSilo

derive instance eqFactory :: Eq Factory


-- How many productivity modules fit in a factory
productivityModules :: Factory -> Number
productivityModules Assembler = 4.0
productivityModules ChemicalPlant = 3.0
productivityModules OilRefinery = 3.0
productivityModules ElectricFurnace = 2.0
productivityModules RocketSilo = 4.0


-- Factory speed factor
factorySpeed :: Factory -> Number
factorySpeed Assembler = 1.25
factorySpeed ChemicalPlant = 1.25
factorySpeed OilRefinery = 1.0
factorySpeed ElectricFurnace = 2.0
factorySpeed RocketSilo = 1.0


-- Speed of factory with productivity modules
factorySpeedWithProductivity :: Factory -> Number
factorySpeedWithProductivity factory =
  speed - (speed * 0.15 * modules)
  where
    speed = factorySpeed factory
    modules = productivityModules factory


-- A single required component for one creation process of a composite
-- item to be run in a factory
data Requirement = Requirement { quantity :: Number, item :: Item }

instance showRequirement :: Show Requirement where
    show (Requirement r) = show r.quantity <> "x" <> show r.item

derive instance eqRequirement :: Eq Requirement


data Item =
    Composite
      { name :: String
      , image :: String
      , quantity :: Number -- how many are produced from one set of inputs
      , intermediate :: Boolean -- can productivity modules be used?
      , requirements :: Array Requirement
      , madeIn :: Factory
      , time :: Number
      }
  | Atom
      { name :: String
      , image :: String
      }


derive instance eqItem :: Eq Item

instance showItem :: Show Item where
    show (Atom item)      = item.name
    show (Composite item) = item.name
                          <> " ["
                          <> joinWith ", " (map show item.requirements)
                          <> "]"


-- Number of assembly processes to run in order to create the desired
-- quantity of products
processes :: Number -> Item -> Number
processes quantity (Composite item) =
  (if item.intermediate
     then quantity / (1.0 + (productivityModules item.madeIn) / 10.0)
     else quantity)
  / item.quantity
processes quantity (Atom _) = quantity


-- Time to run one process
productionProcessSpeed :: Item -> Number
productionProcessSpeed (Atom _) = 1.0
productionProcessSpeed (Composite item) =
  if item.intermediate
    then item.time / factorySpeedWithProductivity item.madeIn
    else item.time / factorySpeed item.madeIn


-- Number of factories to create the given number of pieces of the given
-- item in the given time
factories :: Item -- create this item
          -> Number -- in given numbers
          -> Number -- in given time (in seconds)
          -> Number -- (returns) in how many factories
factories item@(Composite c) quantity time =
  (processes quantity item * productionProcessSpeed item) / time
factories (Atom item) _ _ = 1.0


-- List of composite items displayed to the user
compositeItems :: Array Item
compositeItems =
  [ ironGearWheel
  , pipe
  , sciencePack1
  , copperCable
  , lubricant
  , stoneBrick
  , electronicCircuit
  , assemblingMachine1
  , engineUnit
  , electricEngineUnit
  , plasticBar
  , solidFuel
  , advancedCircuit
  , electricFurnace
  , productionSciencePack
  , speedModule
  , lowDensityStructure
  , rocketControlUnit
  , rocketFuel
  , rocketPart
  ]


-- Listing of Factorio items follows until the end of file

ironPlate :: Item
ironPlate = Atom { name: "Iron Plate", image: "img/Iron_plate.png" }


copperPlate :: Item
copperPlate = Atom { name: "Copper Plate", image: "img/Copper_plate.png" }


ironGearWheel :: Item
ironGearWheel = Composite
  { name: "Iron Gear Wheel"
  , image: "img/Iron_gear_wheel.png"
  , quantity: 1.0
  , intermediate: true
  , requirements: [ Requirement { quantity: 2.0, item: ironPlate } ]
  , madeIn: Assembler
  , time: 0.5
  }


sciencePack1 :: Item
sciencePack1 = Composite
  { name: "Science Pack 1"
  , image: "img/Science_pack_1.png"
  , quantity: 1.0
  , intermediate: true
  , requirements:
    [ Requirement { quantity: 1.0, item: copperPlate }
    , Requirement { quantity: 1.0, item: ironGearWheel }
    ]
  , madeIn: Assembler
  , time: 5.0
  }


copperCable :: Item
copperCable = Composite
  { name: "Copper Cable"
  , image: "img/Copper_cable.png"
  , quantity: 2.0
  , time: 0.5
  , intermediate: true
  , madeIn: Assembler
  , requirements: [ Requirement {quantity: 1.0, item: copperPlate} ]
  }


electronicCircuit :: Item
electronicCircuit = Composite
  { name: "Electronic Circuit"
  , image: "img/Electronic_circuit.png"
  , quantity: 1.0
  , time: 0.5
  , intermediate: true
  , madeIn: Assembler
  , requirements:
    [ Requirement { quantity: 3.0, item: copperCable }
    , Requirement { quantity: 1.0, item: ironPlate }
    ]
  }


assemblingMachine1 :: Item
assemblingMachine1 = Composite
  { name: "Assembling Machine 1"
  , image: "img/Assembling_machine_1.png"
  , quantity: 1.0
  , time: 0.5
  , madeIn: Assembler
  , intermediate: false -- TODO: check this
  , requirements:
    [ Requirement { quantity: 3.0, item: electronicCircuit }
    , Requirement { quantity: 5.0, item: ironGearWheel }
    , Requirement { quantity: 9.0, item: ironPlate }
    ]
  }


-- TODO: oil production is more complicated
heavyOil :: Item
heavyOil = Atom { name: "Heavy Oil", image: "img/Heavy_oil.png" }


steelPlate :: Item
steelPlate = Composite
  { name: "Steel plate"
  , image: "img/Steel_plate.png"
  , quantity: 1.0
  , time: 17.5
  , intermediate: true
  , madeIn: ElectricFurnace
  , requirements: [ Requirement { quantity: 5.0, item: ironPlate } ]
  }


pipe :: Item
pipe = Composite
  { name: "Pipe"
  , image: "img/Pipe.png"
  , quantity: 1.0
  , time: 0.5
  , intermediate: false
  , madeIn: Assembler
  , requirements: [ Requirement { quantity: 1.0, item: ironPlate } ]
  }


engineUnit :: Item
engineUnit = Composite
  { name: "Engine unit"
  , image: "img/Engine_unit.png"
  , quantity: 1.0
  , time: 10.0
  , madeIn: Assembler
  , intermediate: true
  , requirements:
    [ Requirement { quantity: 1.0, item: ironGearWheel }
    , Requirement { quantity: 2.0, item: pipe }
    , Requirement { quantity: 1.0, item: steelPlate }
    ]
  }


lubricant :: Item
lubricant = Composite
  { name: "Lubricant"
  , image: "img/Lubricant.png"
  , quantity: 10.0
  , time: 1.0
  , intermediate: true
  , madeIn: ChemicalPlant
  , requirements: [ Requirement { quantity: 10.0, item: heavyOil } ]
  }


electricEngineUnit :: Item
electricEngineUnit = Composite
  { name: "Electric Engine Unit"
  , image: "img/Electric_engine_unit.png"
  , quantity: 1.0
  , time: 10.0
  , intermediate: true
  , madeIn: Assembler
  , requirements:
    [ Requirement { quantity: 2.0, item: electronicCircuit }
    , Requirement { quantity: 1.0, item: engineUnit }
    , Requirement { quantity: 15.0, item: lubricant }
    ]
  }


coal :: Item
coal = Atom { name: "Coal", image: "img/Coal.png" }


petroleumGas :: Item
petroleumGas = Atom { name: "Petroleum gas", image: "img/Petroleum_gas.png" }


lightOil :: Item
lightOil = Atom { name: "Light oil", image: "img/Light_oil.png" }

plasticBar :: Item
plasticBar = Composite
  { name: "Plastic bar"
  , image: "img/Plastic_bar.png"
  , quantity: 2.0
  , time: 1.0
  , madeIn: ChemicalPlant
  , intermediate: true
  , requirements:
    [ Requirement { quantity: 1.0, item: coal }
    , Requirement { quantity: 20.0, item: petroleumGas }
    ]
  }


advancedCircuit :: Item
advancedCircuit = Composite
  { name: "Advanced circuit"
  , image: "img/Advanced_circuit.png"
  , quantity: 1.0
  , time: 6.0
  , madeIn: Assembler
  , intermediate: true
  , requirements:
    [ Requirement { quantity: 4.0, item: copperCable }
    , Requirement { quantity: 2.0, item: electronicCircuit }
    , Requirement { quantity: 2.0, item: plasticBar }
    ]
  }


stone :: Item
stone = Atom { name: "Stone", image: "img/Stone.png" }


stoneBrick :: Item
stoneBrick = Composite
  { name: "Stone brick"
  , image: "img/Stone_brick.png"
  , quantity: 1.0
  , time: 3.5
  -- interestingly productivity modules can be used
  , intermediate: true
  , madeIn: ElectricFurnace
  , requirements: [ Requirement { quantity: 2.0, item: stone } ]
  }


electricFurnace :: Item
electricFurnace = Composite
  { name: "Electric furnace"
  , image: "img/Electric_furnace.png"
  , quantity: 1.0
  , time: 5.0
  , madeIn: Assembler
  , intermediate: true
  , requirements:
    [ Requirement { quantity: 5.0, item: advancedCircuit }
    , Requirement { quantity: 10.0, item: steelPlate }
    , Requirement { quantity: 10.0, item: stoneBrick }
    ]
  }


productionSciencePack :: Item
productionSciencePack = Composite
  { name: "Production science pack"
  , image: "img/Production_science_pack.png"
  , quantity: 2.0
  , time: 14.0
  , intermediate: true
  , madeIn: Assembler
  , requirements:
    [ Requirement { quantity: 1.0, item: assemblingMachine1 }
    , Requirement { quantity: 1.0, item: electricEngineUnit }
    , Requirement { quantity: 1.0, item: electricFurnace }
    ]
  }


lowDensityStructure :: Item
lowDensityStructure = Composite
  { name: "Low density structure"
  , image: "img/Low_density_structure.png"
  , quantity: 1.0
  , time: 30.0
  , intermediate: true
  , madeIn: Assembler
  , requirements:
    [ Requirement { quantity: 5.0, item: copperPlate }
    , Requirement { quantity: 5.0, item: plasticBar }
    , Requirement { quantity: 10.0, item: steelPlate }
    ]
  }


water :: Item
water = Atom { name: "Water", image: "img/Water.png" }


sulfur :: Item
sulfur = Composite
  { name: "Sulfur"
  , image: "img/Sulfur.png"
  , quantity: 2.0
  , time: 1.0
  , intermediate: true
  , madeIn: ChemicalPlant
  , requirements:
    [ Requirement { quantity: 30.0, item: petroleumGas }
    , Requirement { quantity: 30.0, item: water }
    ]
  }


sulfuricAcid :: Item
sulfuricAcid = Composite
  { name: "Sulfuric acid"
  , image: "img/Sulfuric_acid.png"
  , quantity: 50.0
  , time: 1.0
  , intermediate: true
  , madeIn: ChemicalPlant
  , requirements:
    [ Requirement { quantity: 1.0, item: ironPlate }
    , Requirement { quantity: 100.0, item: water }
    , Requirement { quantity: 5.0, item: sulfur }
    ]
  }


processingUnit :: Item
processingUnit = Composite
  { name: "Processing unit"
  , image: "img/Processing_unit.png"
  , quantity: 1.0
  , time: 10.0
  , intermediate: true
  , madeIn: Assembler
  , requirements:
    [ Requirement { quantity: 2.0, item: advancedCircuit }
    , Requirement { quantity: 20.0, item: electronicCircuit }
    , Requirement { quantity: 5.0, item: sulfuricAcid }
    ]
  }


speedModule :: Item
speedModule = Composite
  { name: "Speed module"
  , image: "img/Speed_module.png"
  , quantity: 1.0
  , time: 15.0
  , intermediate: true
  , madeIn: Assembler
  , requirements:
    [ Requirement { quantity: 5.0, item: advancedCircuit }
    , Requirement { quantity: 5.0, item: electronicCircuit }
    ]
  }


rocketControlUnit :: Item
rocketControlUnit = Composite
  { name: "Rocket control unit"
  , image: "img/Rocket_control_unit.png"
  , quantity: 1.0
  , time: 30.0
  , intermediate: true
  , madeIn: Assembler
  , requirements:
    [ Requirement { quantity: 1.0, item: processingUnit }
    , Requirement { quantity: 1.0, item: speedModule }
    ]
  }


-- this is only one of the options how to make solidFuel but arguably the best one
solidFuel :: Item
solidFuel = Composite
  { name: "Solid fuel"
  , image: "img/Solid_fuel.png"
  , quantity: 1.0
  , time: 3.0
  , intermediate: true
  , madeIn: ChemicalPlant
  , requirements: [ Requirement { quantity: 10.0, item: lightOil } ]
  }


rocketFuel :: Item
rocketFuel = Composite
  { name: "Rocket fuel"
  , image: "img/Rocket_fuel.png"
  , quantity: 1.0
  , time: 30.0
  , intermediate: true
  , madeIn: Assembler
  , requirements:
    [ Requirement { quantity: 10.0, item: solidFuel } ]
  }


rocketPart :: Item
rocketPart = Composite
  { name: "Rocket part"
  , image: "img/Rocket_part.png"
  , quantity: 1.0
  , time: 3.0
  , intermediate: true
  , madeIn: RocketSilo
  , requirements:
    [ Requirement { quantity: 10.0, item: lowDensityStructure }
    , Requirement { quantity: 10.0, item: rocketControlUnit }
    , Requirement { quantity: 10.0, item: rocketFuel }
    ]
  }
