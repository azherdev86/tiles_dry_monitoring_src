unit FExportToCSV;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Gauges, ZDataset, ExtCtrls, FTerminalForm;

type
  TFormExportToCSV = class(TFormTerminal)
    LabelSince: TLabel;
    LabelTo: TLabel;
    ButtonExport: TButton;
    DatePickerSince: TDateTimePicker;
    DatePickerTo: TDateTimePicker;
    Gauge: TGauge;
    SaveDialog: TSaveDialog;
    LabeledEditSinceHours: TLabeledEdit;
    LabeledEditSinceMinutes: TLabeledEdit;
    LabeledEditToHours: TLabeledEdit;
    LabeledEditToMinutes: TLabeledEdit;
    procedure ButtonExportClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LabeledEditSinceHoursChange(Sender: TObject);
    procedure LabeledEditToHoursChange(Sender: TObject);
    procedure LabeledEditSinceMinutesChange(Sender: TObject);
    procedure LabeledEditToMinutesChange(Sender: TObject);
  private
    { Private declarations }
    FQuery : TZQuery;
    FDateTimeSince,
    FDateTimeTo : TDateTime;

    procedure SetTimeRanges();
    function CheckValues() : boolean;
    function CheckRecordsCount(out ARecordsCount : integer) : boolean;
    function GetReportFileName() : string;
  public
    { Public declarations }
    procedure SaveDataSetToCSV(DataSet: TZQuery; FileName: String; RecordsCount : integer);
  end;

var
  FormExportToCSV: TFormExportToCSV;

implementation

uses LApplicationGlobals, DateUtils, LUtils;

{$R *.dfm}


function TFormExportToCSV.GetReportFileName() : string;
var
  tmpResult : string;
begin
  tmpResult := DateTimeToFirebirdString(Now);

  tmpResult := StringReplace(tmpResult, ':', '', [rfReplaceAll]);
  tmpResult := StringReplace(tmpResult, ' ', '', [rfReplaceAll]);
  tmpResult := StringReplace(tmpResult, '.', '', [rfReplaceAll]);
  tmpResult := 'report_' + tmpResult + '.csv';

  Result := tmpResult;
end;


procedure TFormExportToCSV.LabeledEditSinceHoursChange(Sender: TObject);
var
  value : integer;
begin
  if TryStrToInt((Sender as TLabeledEdit).Text, value)
    then
      begin
        if (value < 0)
          then value := 0;

        if (value > 23)
          then value := 23;

        (Sender as TLabeledEdit).Text := IntToStr(value);
      end
    else
      (Sender as TLabeledEdit).Text := '0';
end;

procedure TFormExportToCSV.LabeledEditSinceMinutesChange(Sender: TObject);
var
  value : integer;
begin
  if TryStrToInt((Sender as TLabeledEdit).Text, value)
    then
      begin
        if (value < 0)
          then value := 0;

        if (value > 59)
          then value := 59;

        (Sender as TLabeledEdit).Text := IntToStr(value);
      end
    else
      (Sender as TLabeledEdit).Text := '0';
end;

procedure TFormExportToCSV.LabeledEditToHoursChange(Sender: TObject);
var
  value : integer;
begin
  if TryStrToInt((Sender as TLabeledEdit).Text, value)
    then
      begin
        if (value < 0)
          then value := 0;

        if (value > 23)
          then value := 23;

        (Sender as TLabeledEdit).Text := IntToStr(value);
      end
    else
      (Sender as TLabeledEdit).Text := '0';
end;

procedure TFormExportToCSV.LabeledEditToMinutesChange(Sender: TObject);
var
  value : integer;
begin
  if TryStrToInt((Sender as TLabeledEdit).Text, value)
    then
      begin
        if (value < 0)
          then value := 0;

        if (value > 59)
          then value := 59;

        (Sender as TLabeledEdit).Text := IntToStr(value);
      end
    else
      (Sender as TLabeledEdit).Text := '0';
end;

function TFormExportToCSV.CheckValues() : boolean;
var
  tmpDateTimeSince,
  tmpDateTimeTo : TDateTime;

  iHours,
  iMinutes : integer;
begin
  Result := False;

  iHours   := StrToInt(LabeledEditSinceHours.Text);
  iMinutes := StrToInt(LabeledEditSinceMinutes.Text);

  tmpDateTimeSince := Trunc(DatePickerSince.Date) + iHours/24 + iMinutes/24/60;

  iHours   := StrToInt(LabeledEditToHours.Text);
  iMinutes := StrToInt(LabeledEditToMinutes.Text);

  tmpDateTimeTo    := Trunc(DatePickerTo.Date)    + iHours/24 + iMinutes/24/60;

  if tmpDateTimeTo > Now
    then
      begin
        ShowMessage('Time end range couldn''t be more than present moment');
        SetTimeRanges;  //сброс значений
        Exit;
      end;

  if tmpDateTimeTo <= tmpDateTimeSince
    then
      begin
        ShowMessage('Time start range should be less than time end range');
        SetTimeRanges;  //сброс значений
        Exit;
      end;

  FDateTimeSince := tmpDateTimeSince;
  FDateTimeTo    := tmpDateTimeTo;

  Result := True;
end;

function TFormExportToCSV.CheckRecordsCount(out ARecordsCount : integer) : boolean;
function FormatString(Astr : string) : string;
var
  i, k, count : integer;
begin
  Result := '';
  count := Length(Astr);

  k := 1;
  for i := count downto 1 do
  begin
    Result := Astr[i] + Result;
    if k mod 3 = 0
      then Result := ' ' + Result;

    Inc(k);
  end;
end;
var
  SQLText : string;
  count : integer;
begin
  Result := False;

  //Вычисляем количество записей
  SQLText := 'SELECT count(*) as "count"' + sLineBreak +
             'FROM TempValues ' + sLineBreak +
             'WHERE (TempTime >= ''' + DateTimeToFirebirdString(FDateTimeSince) + ''') AND ' + sLineBreak +
             '(TempTime <= ''' + DateTimeToFirebirdString(FDateTimeTo) + ''');';

  FQuery.SQL.Text := SQLText;

  FQuery.Active := True;

  count := FQuery.FieldByName('count').AsInteger;

  ARecordsCount := count;

  if count = 0
    then
      begin
        ShowMessage('There are no data to export');
        Exit;
      end;

  if count > 1000000
    then
      begin
        ShowMessage('Requested too many records to export. Requested: ' + FormatString(IntToStr(count)) + '.' + sLineBreak +
                    'Max allowed: 1 000 000 (approximately 3 days data)');
        Exit;
      end;

  Result := True;
end;

procedure TFormExportToCSV.FormCreate(Sender: TObject);
begin
  inherited;

  FQuery := TZQuery.Create(ApplicationDBConnection);
  FQuery.Connection := ApplicationDBConnection;

  FDateTimeSince := 0;
  FDateTimeTo := 0;

  SetTimeRanges;

  LabeledEditSinceHours.EditLabel.Caption   := '';
  LabeledEditSinceMinutes.EditLabel.Caption := '';
  LabeledEditToHours.EditLabel.Caption      := '';
  LabeledEditToMinutes.EditLabel.Caption    := '';
end;

procedure TFormExportToCSV.FormDestroy(Sender: TObject);
begin
  FQuery.Active := False;
  FQuery.Free;
end;

procedure TFormExportToCSV.SaveDataSetToCSV(DataSet: TZQuery; FileName: String; RecordsCount : integer);
var
  List: TStringList;
  S: String;
  I: Integer;
begin
  List := TStringList.Create;
  try
    Gauge.Progress := 0;
    Gauge.MaxValue := RecordsCount;
    Gauge.Visible  := True;
    Application.ProcessMessages;
    Sleep(200);
    S := 'FloorNumber; SectionNumber; BoxNumber; SensorPosition; TempValue; DateTime';
    List.Add(S);

    DataSet.First;
    while not DataSet.Eof do
    begin
      S := '';
      for I := 0 to DataSet.FieldCount - 1 do
      begin
        if S > '' then
          S := S + ';';
        S := S + '"' + DataSet.Fields[I].AsString + '"';
      end;
      List.Add(S);
      Gauge.Progress := Gauge.Progress + 1;
      DataSet.Next;
    end;
  finally
    List.SaveToFile(FileName);
    List.Free;
    Application.ProcessMessages;
    Sleep(500);

    Gauge.Visible := False;
  end;
end;


procedure TFormExportToCSV.ButtonExportClick(Sender: TObject);
var
  SQLText : string;
  count : integer;

  initial_dir,
  file_name : string;
begin
  if not CheckValues
    then Exit;

  if not CheckRecordsCount(count)
    then Exit;

  initial_dir := ExtractFilePath(Application.ExeName) + 'export';

  if not DirectoryExists(initial_dir)
    then CreateDir(initial_dir);

  if not DirectoryExists(initial_dir)
    then Exit;

  file_name := GetReportFileName();

  SaveDialog.InitialDir := initial_dir;
  SaveDialog.FileName   :=  file_name;

  if not SaveDialog.Execute
    then Exit;

  if FileExists(file_name)
    then
      begin
        ShowMessage('File already exists. Choose another file name');
        Exit;
      end;

  SQLText := 'SELECT ConveyorNumber, SectionNumber, BoxNumber, SensorPosition, TempValue, TempTime' + sLineBreak +
             'FROM TempValues ' + sLineBreak +
             'LEFT JOIN SENSORS ON (TempValues.SensorId = Sensors.SensorId)' + sLineBreak +
             'WHERE (TempTime >= ''' + DateTimeToFirebirdString(FDateTimeSince) + ''') AND ' + sLineBreak +
             '(TempTime <= ''' + DateTimeToFirebirdString(FDateTimeTo) + ''');';

  FQuery.SQL.Text := SQLText;

  FQuery.Active := True;

  SaveDataSetToCSV(FQuery, SaveDialog.FileName, count);
end;

procedure TFormExportToCSV.SetTimeRanges();
begin
  if not (FDateTimeTo = 0)
    then
      begin
        DatePickerTo.DateTime := FDateTimeTo;

        LabeledEditToHours.Text   := IntToStr(DecodeHour(FDateTimeTo));
        LabeledEditToMinutes.Text := IntToStr(DecodeMinute(FDateTimeTo));
      end
    else
      begin
        DatePickerTo.DateTime := Now;

        LabeledEditToHours.Text   := IntToStr(DecodeHour(Now));
        LabeledEditToMinutes.Text := IntToStr(DecodeMinute(Now));
      end;

  if not (FDateTimeSince = 0)
    then
      begin
        DatePickerSince.DateTime := FDateTimeSince;

        LabeledEditSinceHours.Text   := IntToStr(DecodeHour(FDateTimeSince));
        LabeledEditSinceMinutes.Text := IntToStr(DecodeMinute(FDateTimeSince));
      end
    else
      begin
        DatePickerSince.DateTime := Now - 1;

        LabeledEditSinceHours.Text   := IntToStr(DecodeHour(Now - 1));
        LabeledEditSinceMinutes.Text := IntToStr(DecodeMinute(Now - 1));
      end;
end;


end.
