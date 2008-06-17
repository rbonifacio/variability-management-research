
module FeatureCarSample where

import FeatureModel


-- CAR feature model
fm = FeatureModel car

car = Feature "Car" "Car Feature Model" mandatory basicFeature [engine, air, dir] []

engine = Feature "CarEngine" "Engine feature" mandatory basicFeature[engineType, engineFuel][]

engineType = Feature "EngineType" "Engine type feature" mandatory alternativeFeature [low, mid, high][]
low = Feature "LowPower" "Low power" optional basicFeature [][]
mid = Feature "MidPower" "Mid power" optional basicFeature [][]
high = Feature "HighPower" "High power" optional basicFeature [][]

engineFuel = Feature "EngineFuel" "Engine fuel" mandatory orFeature [gas, alc][]
gas = Feature "GasFuel" "Gas fuel" optional basicFeature [][]
alc = Feature "AlcFuel" "Alchool fuel" optional basicFeature [][]

air = Feature "Air" "Air feature" optional basicFeature [][]
dir = Feature "Dir" "Dir feature" optional basicFeature [][]

-- First model
fc1 = FeatureConfiguration car1

car1 = Feature "Car" "Carro1" mandatory basicFeature [engine1, air, dir] []

engine1 = Feature "CarEngine" "Engine1" mandatory basicFeature[engineType1, engineFuel1][]

engineType1 = Feature "EngineType" "Engine type 1" mandatory alternativeFeature [low1][]
low1 = Feature "LowPower" "Low power" optional basicFeature [][]

engineFuel1 = Feature "EngineFuel" "Engine fuel 1" mandatory orFeature [gas1][]
gas1 = Feature "GasFuel" "Gas fuel 1" optional basicFeature [][]

-- Second model
fc2 = FeatureConfiguration car2

car2 = Feature "Car2" "Carro2" mandatory basicFeature [engine2, air, dir] []

engine2 = Feature "CarEngine" "Engine2" mandatory basicFeature[engineType2, engineFuel2][]

engineType2 = Feature "EngineType" "Engine type 2" mandatory alternativeFeature [low2][]
low2 = Feature "LowPower" "Low power" optional basicFeature [][]

engineFuel2 = Feature "EngineFuel" "Engine fuel 2" mandatory orFeature [gas2][]
gas2 = Feature "GasFuel" "Gas fuel 2" optional basicFeature [][]

-- third model
fc3 = FeatureConfiguration car3

car3 = Feature "Car" "Carro3" mandatory basicFeature [engine3, air, dir] []

engine3 = Feature "CarEngine" "Engine3" mandatory basicFeature[engineType3, engineFuel3][]

engineType3 = Feature "EngineType" "Engine type 3" mandatory alternativeFeature [gas3][]
low3 = Feature "LowPower" "Low power" optional basicFeature [][]

engineFuel3 = Feature "EngineFuel" "Engine fuel 3" mandatory orFeature [gas3][]
gas3 = Feature "GasFuel" "Gas fuel 3" optional basicFeature [][]

-- fourth model
fc4 = FeatureConfiguration car4
-- car4 = Feature "Car" "Carro4" mandatory basicFeature [air, dir] []
car4 = Feature "Car" "Carro4" mandatory basicFeature [engine4, air, dir] []

engine4 = Feature "CarEngine" "Engine4" mandatory basicFeature[engineType4, engineFuel4][]

engineType4 = Feature "EngineType" "Engine type 4" mandatory alternativeFeature [gas4][]
low4 = Feature "LowPower" "Low power" optional basicFeature [][]

engineFuel4 = Feature "EngineFuel" "Engine fuel 4" mandatory orFeature [][]
gas4 = Feature "GasFuel" "Gas fuel 4" optional basicFeature [][]
