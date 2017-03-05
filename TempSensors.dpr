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
  LApplicationGlobals in 'Libraries\LApplicationGlobals.pas',
  uTDurationProcess in 'Libraries\uTDurationProcess.pas',
  CGraph in 'Classes\CGraph.pas',
  FTemperatureRanges in 'Forms\FTemperatureRanges.pas' {FormTemperatureRanges},
  FEventlogs in 'Forms\FEventlogs.pas' {FormEventLogs},
  FGraphHistory in 'Forms\FGraphHistory.pas' {FormGraphHistory},
  CTempValues in 'Classes\CTempValues.pas',
  CBoxes in 'Classes\CBoxes.pas',
  COutgoingComPortMessage in 'Classes\COutgoingComPortMessage.pas',
  CBasicComPortMessage in 'Classes\CBasicComPortMessage.pas',
  CIncomingComPortMessage in 'Classes\CIncomingComPortMessage.pas',
  CProgramSettings in 'Classes\CProgramSettings.pas',
  CProgramGraphSettings in 'Classes\CProgramGraphSettings.pas',
  CTempValuesBuffer in 'Classes\CTempValuesBuffer.pas',
  CController in 'Classes\CController.pas',
  CRows in 'Classes\CRows.pas',
  FInputPassword in 'Forms\FInputPassword.pas' {FormInputPassword},
  LHash in 'Libraries\LHash.pas',
  FChangePassword in 'Forms\FChangePassword.pas' {FormChangePassword},
  CProgramUserSettings in 'Classes\CProgramUserSettings.pas',
  FExportToCSV in 'Forms\FExportToCSV.pas' {FormExportToCSV},
  LUtils in 'Libraries\LUtils.pas',
  CEventLog in 'Classes\CEventLog.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
