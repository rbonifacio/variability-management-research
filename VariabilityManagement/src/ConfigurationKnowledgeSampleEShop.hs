module ConfigurationKnowledgeSampleEShop where

import ConfigurationKnowledge
import FeatureModel
import UseCaseModel
import FeatureSampleEShop
import UseCaseSampleEShop

exp1 = FeatureRef "FEA-03" -- message feature
exp2 = FeatureRef "FEA-03" -- mms feature
exp3 = FeatureRef "FEA-11" -- save to draft feature
exp4 = FeatureRef "FEA-10" -- multiple folders
exp5 = FeatureRef "FEA-12" -- compressed messages

-- message feature and not save expression
exp6 = AndExpression exp1 (NotExpression exp3) 
-- (message feature and save to draft) and (not multiple folders)
exp7 = AndExpression (AndExpression exp1 exp3) (NotExpression exp4)
-- (message feature and save to draft and multiple folders)
exp8 = AndExpression (AndExpression exp1 exp3) exp4
-- message feature and save message compressed message
exp9 = AndExpression exp1 exp5

-- Configurations that relate a feature expression with 
-- a set of artifacts (scenarios, in this case)
conf1 = (exp2, [scenario4])
conf2 = (exp6, [scenario1])
conf3 = (exp7, [scenario1, scenario2, scenario3])
conf4 = (exp8, [scenario1, scenario5, scenario6, scenario7, scenario8])
conf5 = (exp9, [scenario9])

configuration = CK [conf1, conf2, conf3, conf4,conf5]


fcMessage1 = Feature "FEA-01" "Message Application" mandatory basicFeature [fcAllMessageType, fcMultipleFolders] []
fcMessage2 = Feature "FEA-01" "Message Application" mandatory basicFeature [fcNotAllMessageType, fcFolderManagement, fcSaveToDraft] []


fcAllMessageType = Feature "FEA-02" "Message Type" mandatory orFeature [fcMms, fcSms, fcEmail] []
fcNotAllMessageType = Feature "FEA-02" "Message Type" mandatory orFeature [fcSms, fcEmail] []

fcMms = Feature "FEA-03" "MMS" optional basicFeature [] []
fcSms = Feature "FEA-04" "SMS" optional basicFeature [] []
fcEmail = Feature "FEA-05" "Email" optional basicFeature [] []

fcFolderManagement = Feature "FEA-06" "Folder Management" mandatory basicFeature [fcMultipleFolders] []

fcSize1 = Feature "FEA-07" "Folder size" mandatory alternativeFeature [fcExtendedSize] []
fcSize2 = Feature "FEA-07" "Folder size" mandatory alternativeFeature [fcDefaultSize] []

fcExtendedSize = Feature "FEA-08" "Extended folder size" optional basicFeature [] [("size","1000")]
fcDefaultSize = Feature "FEA-09" "Default folder size" optional basicFeature [] [("size", "500")]

fcMultipleFolders = Feature "FEA-10" "Support for multiple folders" optional basicFeature [] []

fcSaveToDraft = Feature "FEA-11" "Save to Draft" optional basicFeature [fcCompressMessageOnSave] []
fcCompressMessageOnSave = Feature "FEA-12" "Compress message on save" optional basicFeature [] []

fc1 = FeatureConfiguration fcMessage1
fc2 = FeatureConfiguration fcMessage2