module FeatureSample where

import FeatureModel

fm = FeatureModel message

message = Feature "FEA-01" "Message Application" mandatory basicFeature [messageType, saveMessage, folderManagement] []

-- Features related with the message types (SMS, MMS, Email)
messageType = Feature "FEA-02" "Message Type" mandatory orFeature [mms, sms, email] []
mms = Feature "FEA-03" "MMS" optional basicFeature [] []
sms = Feature "FEA-04" "SMS" optional basicFeature [] []
email = Feature "FEA-05" "Email" optional basicFeature [] []

-- Features related with the folderManagement
folderManagement =
 Feature "FEA-06" "Folder Management" mandatory basicFeature [size, multipleFolders] []

size = Feature "FEA-07" "Folder size" mandatory alternativeFeature [extendedSize, defaultSize] []
extendedSize = Feature "FEA-08" "Extended folder size" optional basicFeature [] [("size","1000")]
defaultSize = Feature "FEA-09" "Default folder size" optional basicFeature [] [("size", "500")]
multipleFolders = Feature "FEA-10" "Support for multiple folders" optional basicFeature [] []

saveMessage = Feature "FEA-11" "Save to Draft" optional basicFeature [compressMessageOnSave] []

compressMessageOnSave = Feature "FEA-12" "Compress message on save" optional basicFeature [] []



