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

  Result := TRUE;
end;

end.
