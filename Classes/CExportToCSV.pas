unit CExportToCSV;

interface

uses ZDataSet;

const
  CExportPath = 'export\';
  CBackupExportPath = 'export\backup\';


type
  TMExportToCSV = class
    constructor Create;
    destructor Destroy; override;
  private
    FQuery : TZQuery;

    function GetReportFileName() : string;
//    function GetReportFilePath() : string;
    function SaveDataSetToCSV(DataSet: TZQuery; FileName: String) : integer;

  public
    function SaveToCSVFile(ADateSince, ADateTo : TDateTime) : integer;
  end;

implementation

uses LApplicationGlobals, LUtils, SysUtils, DateUtils, Classes;

constructor TMExportToCSV.Create;
begin
  FQuery := TZQuery.Create(ApplicationDBConnection);
  FQuery.Connection := ApplicationDBConnection;
end;

destructor TMExportToCSV.Destroy;
begin
  FQuery.Active := False;
  FQuery.Free;

  inherited;
end;

function TMExportToCSV.GetReportFileName() : string;
var
  year,
  month,
  day : word;

  year_str,
  month_str,
  day_str : string;
begin
  DecodeDate(Now, year, month, day);

  year_str  := IntToStr(year);
  month_str := IntToStr(month);
  day_str   := IntToStr(day);

  if month < 10
    then month_str := '0' + month_str;

  if day < 10
    then day_str := '0' + day_str;

  Result := year_str + month_str + day_str + '.csv';
end;

//function TMExportToCSV.GetReportFilePath() : string;
//var
//  year,
//  month,
//  day : word;
//
//  year_str,
//  month_str : string;
//begin
//  DecodeDate(Now, year, month, day);
//
//  year_str  := IntToStr(year);
//  month_str := IntToStr(month);
//
//  if month < 10
//    then month_str := '0' + month_str;
//
//  Result := CBackupExportPath + year_str + '\' + month_str + '\';
//end;

function TMExportToCSV.SaveToCSVFile(ADateSince, ADateTo : TDateTime) : integer;
var
  path_year,
  path_month,
  path,
  filename,
  sql_text : string;

  year,
  month : integer;
begin
  if not DirectoryExists(CExportPath)
    then CreateDir(CExportPath);

  if not DirectoryExists(CBackupExportPath)
    then CreateDir(CBackupExportPath);

  year :=  DecodeYear(Now);
  month := DecodeMonth(Now);

  path_year  := IntToStr(year);
  path_month := IntToStr(month);

  if month < 10
    then path_month := '0' + path_month;

  path := CBackupExportPath + path_year + '\';

  if not DirectoryExists(path)
    then CreateDir(path);

  path := path + path_month + '\';

  if not DirectoryExists(path)
    then CreateDir(path);

  filename := GetReportFileName;
  filename := path + filename;

  sql_text := 'SELECT ConveyorNumber, SectionNumber, BoxNumber, SensorPosition, TempValue, TempTime' + sLineBreak +
              'FROM TempValues ' + sLineBreak +
              'LEFT JOIN SENSORS ON (TempValues.SensorId = Sensors.SensorId)' + sLineBreak +
              'WHERE (TempTime >= ''' + DateTimeToFirebirdString(ADateSince) + ''') AND ' + sLineBreak +
              '(TempTime < ''' + DateTimeToFirebirdString(ADateTo) + ''');';

  FQuery.SQL.Text := sql_text;

  FQuery.Active := True;

  Result := SaveDataSetToCSV(FQuery, filename);
end;


function TMExportToCSV.SaveDataSetToCSV(DataSet: TZQuery; FileName: String) : integer;
var
  List: TStringList;
  S: String;
  I: Integer;
  Count : Integer;
begin
  List := TStringList.Create;
  try
    S := 'FloorNumber; SectionNumber; BoxNumber; SensorPosition; TempValue; DateTime';
    List.Add(S);

    DataSet.First;
    while not DataSet.Eof do
    begin
      S := '';
      Count := DataSet.FieldCount;
      for I := 0 to Count - 1 do
      begin
        if S > '' then
          S := S + ';';
        S := S + '"' + DataSet.Fields[I].AsString + '"';
      end;
      List.Add(S);
      DataSet.Next;
    end;
  finally
    List.SaveToFile(FileName);
    Result := List.Count - 1;
    List.Free;
  end;
end;


end.
