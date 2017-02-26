unit CProgramSettings;

interface
uses Classes, CProgramGraphSettings;

const
  DEFAULT_INI_FILENAME = 'config.ini';

type
  TMProgramSettings = class
    constructor Create;
    destructor Destroy; override;
  public
    GraphSettings : TMProgramGraphSettings;

  private
    function GetIniFileDir:string;
  public
    function  LoadFromInifile:boolean;
    function  SaveToInifile:boolean;
    function  GetIniFileName:string;
end;


implementation
uses SysUtils, IniFiles, Forms;

{ TMProgramSettings }

constructor TMProgramSettings.Create;
begin
  GraphSettings := TMProgramGraphSettings.Create;
end;

destructor TMProgramSettings.Destroy;
begin
  GraphSettings.Free;

  inherited;
end;


function TMProgramSettings.GetIniFileDir: string;
begin
  Result := ExtractFilePath(Application.ExeName);
end;


function TMProgramSettings.GetIniFileName: string;
begin
  Result := GetIniFileDir + DEFAULT_INI_FILENAME;
end;

function TMProgramSettings.LoadFromInifile: boolean;
var
  IniFile : TIniFile;
  FullName : string;
begin
  Result:=false;
  FullName := GetIniFileName;

  if FullName = '' then Exit;

  if not FileExists(FullName)
    then Exit;
  

  IniFile:=TIniFile.Create(FullName);
  if not Assigned(IniFile) then exit;
  try
    GraphSettings.LoadFromIniFile(IniFile);

    Result:=true;
  finally
    IniFile.Free;
  end;
end;

function TMProgramSettings.SaveToInifile: boolean;
var
  IniFile : TIniFile;
  FullName : string;
begin
  Result:=false;
  FullName:=GetIniFileName;

  IniFile:=TIniFile.Create(FullName);
  if not Assigned(IniFile) then exit;
  try
    GraphSettings.SaveToInifile(IniFile);

    Result := TRUE;
  finally
    IniFile.Free;
  end;
end;

end.
