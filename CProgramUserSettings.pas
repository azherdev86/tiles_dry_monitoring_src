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
end;

function TMProgramUserSettings.SaveToIniFile(IniFile : TIniFile) : boolean;
begin
  IniFile.WriteString('User', 'Pass', PasswordHash);

  Result := TRUE;
end;


function TMProgramUserSettings.LoadFromIniFile(IniFile : TIniFile)  : boolean;
begin
  PasswordHash := IniFile.ReadString('User', 'Pass', '');

  Result := TRUE;
end;

end.
