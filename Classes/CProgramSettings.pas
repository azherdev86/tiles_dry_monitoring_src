unit CProgramSettings;

interface
uses Classes, CProgramGraphSettings, CProgramUserSettings;

const
  DEFAULT_INI_FILENAME = 'config.ini';

type
  TMProgramSettings = class
    constructor Create;
    destructor Destroy; override;
  public
    GraphSettings : TMProgramGraphSettings;
    UserSettings  : TMProgramUserSettings;

  private
    function GetIniFileDir:string;
  public
    function  LoadFromInifile:boolean;
    function  SaveToInifile:boolean;
    function  GetIniFileName:string;
end;

function GetSectionYMaxValue(ASectionNumber : integer) : single;
function GetSectionYMinValue(ASectionNumber : integer) : single;


implementation
uses SysUtils, IniFiles, Forms, LApplicationGlobals;

//External methods

function GetSectionYMinValue(ASectionNumber : integer) : single;
var
  Value : single;
begin
  Value := 0;

  case ASectionNumber of
    1:  Value := ApplicationProgramSettings.GraphSettings.Section1RangeMinYValue;
    2:  Value := ApplicationProgramSettings.GraphSettings.Section2RangeMinYValue;
    3:  Value := ApplicationProgramSettings.GraphSettings.Section3RangeMinYValue;
    4:  Value := ApplicationProgramSettings.GraphSettings.Section4RangeMinYValue;
    5:  Value := ApplicationProgramSettings.GraphSettings.Section5RangeMinYValue;
    6:  Value := ApplicationProgramSettings.GraphSettings.Section6RangeMinYValue;
    7:  Value := ApplicationProgramSettings.GraphSettings.Section7RangeMinYValue;
    8:  Value := ApplicationProgramSettings.GraphSettings.Section8RangeMinYValue;
    9:  Value := ApplicationProgramSettings.GraphSettings.Section9RangeMinYValue;
    10: Value := ApplicationProgramSettings.GraphSettings.Section10RangeMinYValue;
  end;

  Result := Value;
end;

function GetSectionYMaxValue(ASectionNumber : integer) : single;
var
  Value : single;
begin
  Value := 0;

  case ASectionNumber of
    1:  Value := ApplicationProgramSettings.GraphSettings.Section1RangeMaxYValue;
    2:  Value := ApplicationProgramSettings.GraphSettings.Section2RangeMaxYValue;
    3:  Value := ApplicationProgramSettings.GraphSettings.Section3RangeMaxYValue;
    4:  Value := ApplicationProgramSettings.GraphSettings.Section4RangeMaxYValue;
    5:  Value := ApplicationProgramSettings.GraphSettings.Section5RangeMaxYValue;
    6:  Value := ApplicationProgramSettings.GraphSettings.Section6RangeMaxYValue;
    7:  Value := ApplicationProgramSettings.GraphSettings.Section7RangeMaxYValue;
    8:  Value := ApplicationProgramSettings.GraphSettings.Section8RangeMaxYValue;
    9:  Value := ApplicationProgramSettings.GraphSettings.Section9RangeMaxYValue;
    10: Value := ApplicationProgramSettings.GraphSettings.Section10RangeMaxYValue;
  end;

  Result := Value;
end;



{ TMProgramSettings }

constructor TMProgramSettings.Create;
begin
  GraphSettings := TMProgramGraphSettings.Create;
  UserSettings  := TMProgramUserSettings.Create;
end;

destructor TMProgramSettings.Destroy;
begin
  GraphSettings.Free;
  UserSettings.Free;

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
    UserSettings.LoadFromIniFile(IniFile);

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
    UserSettings.SaveToIniFile(IniFile);

    Result := TRUE;
  finally
    IniFile.Free;
  end;
end;

end.
