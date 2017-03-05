program TempSensors;

uses
  Forms,
  FMain in 'Forms\FMain.pas' {FormMain},
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
  LApplicationGlobals in 'LApplicationGlobals.pas',
  uTDurationProcess in 'uTDurationProcess.pas',
  CGraph in 'CGraph.pas',
  FTemperatureRanges in 'Forms\FTemperatureRanges.pas' {FormTemperatureRanges},
  FEventlogs in 'Forms\FEventlogs.pas' {FormEventLogs},
  FGraphHistory in 'Forms\FGraphHistory.pas' {FormGraphHistory},
  CTempValues in 'CTempValues.pas',
  CBoxes in 'CBoxes.pas',
  COutgoingComPortMessage in 'COutgoingComPortMessage.pas',
  CBasicComPortMessage in 'CBasicComPortMessage.pas',
  CIncomingComPortMessage in 'CIncomingComPortMessage.pas',
  CProgramSettings in 'CProgramSettings.pas',
  CProgramGraphSettings in 'CProgramGraphSettings.pas',
  CTempValuesBuffer in 'CTempValuesBuffer.pas',
  CController in 'CController.pas',
  CRows in 'CRows.pas',
  FInputPassword in 'FInputPassword.pas' {FormInputPassword},
  LHash in 'LHash.pas',
  FChangePassword in 'FChangePassword.pas' {FormChangePassword},
  CProgramUserSettings in 'CProgramUserSettings.pas',
  FExportToCSV in 'Forms\FExportToCSV.pas' {FormExportToCSV},
  LUtils in 'LUtils.pas',
  CEventLog in 'CEventLog.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
