program TempSensors;

uses
  Forms,
  CBrowseFieldHints in 'DatabaseFrameWork\CBrowseFieldHints.pas',
  CBrowseFieldImages in 'DatabaseFrameWork\CBrowseFieldImages.pas',
  CBrowseFields in 'DatabaseFrameWork\CBrowseFields.pas',
  CConditionGroups in 'DatabaseFrameWork\CConditionGroups.pas',
  CConditions in 'DatabaseFrameWork\CConditions.pas',
  CDataBaseBrowse in 'DatabaseFrameWork\CDataBaseBrowse.pas',
  CDataBaseStructure in 'DatabaseFrameWork\CDataBaseStructure.pas',
  CFields in 'DatabaseFrameWork\CFields.pas',
  CQueryConstructor in 'DatabaseFrameWork\CQueryConstructor.pas',
  CRecordFields in 'DatabaseFrameWork\CRecordFields.pas',
  CSortingFields in 'DatabaseFrameWork\CSortingFields.pas',
  CTableBrowse in 'DatabaseFrameWork\CTableBrowse.pas',
  CTableRecords in 'DatabaseFrameWork\CTableRecords.pas',
  CTableStructure in 'DatabaseFrameWork\CTableStructure.pas',
  CTemplateEntity in 'DatabaseFrameWork\CTemplateEntity.pas',
  FChangePassword in 'Forms\FChangePassword.pas' {FormChangePassword},
  FEventlogs in 'Forms\FEventlogs.pas' {FormEventLogs},
  FExportToCSV in 'Forms\FExportToCSV.pas' {FormExportToCSV},
  FGraphHistory in 'Forms\FGraphHistory.pas' {FormGraphHistory},
  FInputPassword in 'Forms\FInputPassword.pas' {FormInputPassword},
  FMain in 'Forms\FMain.pas' {FormMain},
  FTemperatureRanges in 'Forms\FTemperatureRanges.pas' {FormTemperatureRanges},
  CBoxes in 'Classes\CBoxes.pas',
  CEventLog in 'Classes\CEventLog.pas',
  CGraph in 'Classes\CGraph.pas',
  CIncomingComPortMessage in 'Classes\CIncomingComPortMessage.pas',
  COutgoingComPortMessage in 'Classes\COutgoingComPortMessage.pas',
  CProgramGraphSettings in 'Classes\CProgramGraphSettings.pas',
  CProgramSettings in 'Classes\CProgramSettings.pas',
  CProgramUserSettings in 'Classes\CProgramUserSettings.pas',
  CRows in 'Classes\CRows.pas',
  CTempValueBuffers in 'Classes\CTempValueBuffers.pas',
  CTempValues in 'Classes\CTempValues.pas',
  CTempValuesBuffer in 'Classes\CTempValuesBuffer.pas',
  LHash in 'Libraries\LHash.pas',
  LUtils in 'Libraries\LUtils.pas',
  uTDurationProcess in 'Libraries\uTDurationProcess.pas',
  FDebugPanel in 'Forms\FDebugPanel.pas' {FormDebugPanel},
  LApplicationGlobals in 'Libraries\LApplicationGlobals.pas',
  CBasicComPortMessage in 'Classes\CBasicComPortMessage.pas',
  CController in 'Classes\CController.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
