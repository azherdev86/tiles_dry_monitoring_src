unit CProgramGraphSettings;

interface

uses Classes,
     IniFiles;

type
  TMProgramGraphSettings = class
    constructor Create();
    destructor Destroy(); override;
  public
    AxisMinYValue : single;
    AxisMaxYValue : single;

    Section1RangeMinYValue : integer;
    Section1RangeMaxYValue : integer;

    Section2RangeMinYValue : integer;
    Section2RangeMaxYValue : integer;

    Section3RangeMinYValue : integer;
    Section3RangeMaxYValue : integer;

    Section4RangeMinYValue : integer;
    Section4RangeMaxYValue : integer;

    Section5RangeMinYValue : integer;
    Section5RangeMaxYValue : integer;

    Section6RangeMinYValue : integer;
    Section6RangeMaxYValue : integer;

    Section7RangeMinYValue : integer;
    Section7RangeMaxYValue : integer;

    Section8RangeMinYValue : integer;
    Section8RangeMaxYValue : integer;

    Section9RangeMinYValue : integer;
    Section9RangeMaxYValue : integer;

    Section10RangeMinYValue : integer;
    Section10RangeMaxYValue : integer;
  public
    function SaveToIniFile(IniFile : TIniFile) : boolean;
    function LoadFromIniFile(IniFile : TIniFile)  : boolean;

  public
    procedure Reset;


end;

implementation


uses SysUtils;

constructor TMProgramGraphSettings.Create();
begin
  Reset;
end;


destructor TMProgramGraphSettings.Destroy();
begin
  Reset;
  inherited;
end;


procedure TMProgramGraphSettings.Reset;
begin
  AxisMinYValue := 0;
  AxisMaxYValue := 0;

  Section1RangeMinYValue := 0;
  Section1RangeMaxYValue := 0;

  Section2RangeMinYValue := 0;
  Section2RangeMaxYValue := 0;

  Section3RangeMinYValue := 0;
  Section3RangeMaxYValue := 0;

  Section4RangeMinYValue := 0;
  Section4RangeMaxYValue := 0;

  Section5RangeMinYValue := 0;
  Section5RangeMaxYValue := 0;

  Section6RangeMinYValue := 0;
  Section6RangeMaxYValue := 0;

  Section7RangeMinYValue := 0;
  Section7RangeMaxYValue := 0;

  Section8RangeMinYValue := 0;
  Section8RangeMaxYValue := 0;

  Section9RangeMinYValue := 0;
  Section9RangeMaxYValue := 0;

  Section10RangeMinYValue := 0;
  Section10RangeMaxYValue := 0;
end;

function TMProgramGraphSettings.SaveToIniFile(IniFile : TIniFile) : boolean;
begin
  IniFile.WriteFloat('Graph', 'AxisMinYValue', AxisMinYValue);
  IniFile.WriteFloat('Graph', 'AxisMaxYValue', AxisMaxYValue);

  IniFile.WriteInteger('Graph', 'Section1RangeMinYValue', Section1RangeMinYValue);
  IniFile.WriteInteger('Graph', 'Section1RangeMaxYValue', Section1RangeMaxYValue);

  IniFile.WriteInteger('Graph', 'Section2RangeMinYValue', Section2RangeMinYValue);
  IniFile.WriteInteger('Graph', 'Section2RangeMaxYValue', Section2RangeMaxYValue);

  IniFile.WriteInteger('Graph', 'Section3RangeMinYValue', Section3RangeMinYValue);
  IniFile.WriteInteger('Graph', 'Section3RangeMaxYValue', Section3RangeMaxYValue);

  IniFile.WriteInteger('Graph', 'Section4RangeMinYValue', Section4RangeMinYValue);
  IniFile.WriteInteger('Graph', 'Section4RangeMaxYValue', Section4RangeMaxYValue);

  IniFile.WriteInteger('Graph', 'Section5RangeMinYValue', Section5RangeMinYValue);
  IniFile.WriteInteger('Graph', 'Section5RangeMaxYValue', Section5RangeMaxYValue);

  IniFile.WriteInteger('Graph', 'Section6RangeMinYValue', Section6RangeMinYValue);
  IniFile.WriteInteger('Graph', 'Section6RangeMaxYValue', Section6RangeMaxYValue);

  IniFile.WriteInteger('Graph', 'Section7RangeMinYValue', Section7RangeMinYValue);
  IniFile.WriteInteger('Graph', 'Section7RangeMaxYValue', Section7RangeMaxYValue);

  IniFile.WriteInteger('Graph', 'Section8RangeMinYValue', Section8RangeMinYValue);
  IniFile.WriteInteger('Graph', 'Section8RangeMaxYValue', Section8RangeMaxYValue);

  IniFile.WriteInteger('Graph', 'Section9RangeMinYValue', Section9RangeMinYValue);
  IniFile.WriteInteger('Graph', 'Section9RangeMaxYValue', Section9RangeMaxYValue);

  IniFile.WriteInteger('Graph', 'Section10RangeMinYValue', Section10RangeMinYValue);
  IniFile.WriteInteger('Graph', 'Section10RangeMaxYValue', Section10RangeMaxYValue);

  Result := TRUE;
end;


function TMProgramGraphSettings.LoadFromIniFile(IniFile : TIniFile)  : boolean;
begin
  AxisMinYValue := IniFile.ReadFloat('Graph', 'AxisMinYValue', 90);
  AxisMaxYValue := IniFile.ReadFloat('Graph', 'AxisMaxYValue', 170);

  Section1RangeMinYValue := IniFile.ReadInteger('Graph', 'Section1RangeMinYValue', 0);
  Section1RangeMaxYValue := IniFile.ReadInteger('Graph', 'Section1RangeMaxYValue', 0);

  Section2RangeMinYValue := IniFile.ReadInteger('Graph', 'Section2RangeMinYValue', 0);
  Section2RangeMaxYValue := IniFile.ReadInteger('Graph', 'Section2RangeMaxYValue', 0);

  Section3RangeMinYValue := IniFile.ReadInteger('Graph', 'Section3RangeMinYValue', 0);
  Section3RangeMaxYValue := IniFile.ReadInteger('Graph', 'Section3RangeMaxYValue', 0);

  Section4RangeMinYValue := IniFile.ReadInteger('Graph', 'Section4RangeMinYValue', 0);
  Section4RangeMaxYValue := IniFile.ReadInteger('Graph', 'Section4RangeMaxYValue', 0);

  Section5RangeMinYValue := IniFile.ReadInteger('Graph', 'Section5RangeMinYValue', 0);
  Section5RangeMaxYValue := IniFile.ReadInteger('Graph', 'Section5RangeMaxYValue', 0);

  Section6RangeMinYValue := IniFile.ReadInteger('Graph', 'Section6RangeMinYValue', 0);
  Section6RangeMaxYValue := IniFile.ReadInteger('Graph', 'Section6RangeMaxYValue', 0);

  Section7RangeMinYValue := IniFile.ReadInteger('Graph', 'Section7RangeMinYValue', 0);
  Section7RangeMaxYValue := IniFile.ReadInteger('Graph', 'Section7RangeMaxYValue', 0);

  Section8RangeMinYValue := IniFile.ReadInteger('Graph', 'Section8RangeMinYValue', 0);
  Section8RangeMaxYValue := IniFile.ReadInteger('Graph', 'Section8RangeMaxYValue', 0);

  Section9RangeMinYValue := IniFile.ReadInteger('Graph', 'Section9RangeMinYValue', 0);
  Section9RangeMaxYValue := IniFile.ReadInteger('Graph', 'Section9RangeMaxYValue', 0);

  Section10RangeMinYValue := IniFile.ReadInteger('Graph', 'Section10RangeMinYValue', 0);
  Section10RangeMaxYValue := IniFile.ReadInteger('Graph', 'Section10RangeMaxYValue', 0);

  Result := TRUE;
end;

end.
