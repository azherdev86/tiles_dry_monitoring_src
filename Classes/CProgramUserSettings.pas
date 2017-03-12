unit CProgramUserSettings;


interface

uses Classes,
     IniFiles;

type
  TMProgramUserSettings = class
    constructor Create();
    destructor Destroy(); override;
  public
    PasswordHash: string;
    Port        : string;
    BaudRate    : string;
    DataBits    : string;
    StopBits    : string;
    Parity      : string;
    FlowControl : string;

    ComPortSendMessagesInterval           : integer;
    CreateBoxMessageInterval              : integer;
    CreateCheckSignalModeMessagesInterval : integer;
    RefreshViewInterval                   : integer;

    EnableComPortSendMessages                   : boolean;
    EnableCreateBoxMessages                     : boolean;
    EnableCreateCheckSignalModeMessagesInterval : boolean;

  public
    function SaveToIniFile(IniFile : TIniFile) : boolean;
    function LoadFromIniFile(IniFile : TIniFile)  : boolean;

  public
    procedure Reset;

end;

implementation


uses SysUtils;

constructor TMProgramUserSettings.Create();
begin
  Reset;
end;


destructor TMProgramUserSettings.Destroy();
begin
  Reset;
  inherited;
end;


procedure TMProgramUserSettings.Reset;
begin
  PasswordHash := '';
  Port         := '';
  BaudRate     := '';
  DataBits     := '';
  StopBits     := '';
  Parity       := '';
  FlowControl  := '';

  ComPortSendMessagesInterval           := 0;
  CreateBoxMessageInterval              := 0;
  CreateCheckSignalModeMessagesInterval := 0;
  RefreshViewInterval                   := 0;

  EnableComPortSendMessages                   := False;
  EnableCreateBoxMessages                     := False;
  EnableCreateCheckSignalModeMessagesInterval := False;
end;

function TMProgramUserSettings.SaveToIniFile(IniFile : TIniFile) : boolean;
begin
  IniFile.WriteString('User', 'Pass',        PasswordHash);
  IniFile.WriteString('User', 'Port',        Port);
  IniFile.WriteString('User', 'BaudRate',    BaudRate);
  IniFile.WriteString('User', 'DataBits',    DataBits);
  IniFile.WriteString('User', 'StopBits',    StopBits);
  IniFile.WriteString('User', 'Parity',      Parity);
  IniFile.WriteString('User', 'FlowControl', FlowControl);

  IniFile.WriteInteger('User', 'ComPortSendMessagesInterval',           ComPortSendMessagesInterval);
  IniFile.WriteInteger('User', 'CreateBoxMessageInterval',              CreateBoxMessageInterval);
  IniFile.WriteInteger('User', 'CreateCheckSignalModeMessagesInterval', CreateCheckSignalModeMessagesInterval);
  IniFile.WriteInteger('User', 'RefreshViewInterval',                   RefreshViewInterval);

  IniFile.WriteBool('User', 'EnableComPortSendMessages',                   EnableComPortSendMessages);
  IniFile.WriteBool('User', 'EnableCreateBoxMessages',                     EnableCreateBoxMessages);
  IniFile.WriteBool('User', 'EnableCreateCheckSignalModeMessagesInterval', EnableCreateCheckSignalModeMessagesInterval);

  Result := TRUE;
end;


function TMProgramUserSettings.LoadFromIniFile(IniFile : TIniFile)  : boolean;
begin
  PasswordHash := IniFile.ReadString('User', 'Pass', '');
  Port         := IniFile.ReadString('User', 'Port', '');
  BaudRate     := IniFile.ReadString('User', 'BaudRate', '');
  DataBits     := IniFile.ReadString('User', 'DataBits', '');
  StopBits     := IniFile.ReadString('User', 'StopBits', '');
  Parity       := IniFile.ReadString('User', 'Parity', '');
  FlowControl  := IniFile.ReadString('User', 'FlowControl', '');

  ComPortSendMessagesInterval           := IniFile.ReadInteger('User','ComPortSendMessagesInterval', 200);
  CreateBoxMessageInterval              := IniFile.ReadInteger('User','CreateBoxMessageInterval', 60000);
  CreateCheckSignalModeMessagesInterval := IniFile.ReadInteger('User','CreateCheckSignalModeMessagesInterval', 20000);
  RefreshViewInterval                   := IniFile.ReadInteger('User','RefreshViewInterval', 5000);

  EnableComPortSendMessages                   := IniFile.ReadBool('User','EnableComPortSendMessages', True);
  EnableCreateBoxMessages                     := IniFile.ReadBool('User','EnableCreateBoxMessages', True);
  EnableCreateCheckSignalModeMessagesInterval := IniFile.ReadBool('User','EnableCreateCheckSignalModeMessagesInterval', True);

  Result := TRUE;
end;

end.
