unit FGraphHistory;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, TeEngine, Series, TeeProcs, Chart,
  ZDataset;

type
  TFormGraphHistory = class(TForm)
    ButtonApply: TButton;
    DatePickerSince: TDateTimePicker;
    TimePickerSince: TDateTimePicker;
    LabelSince: TLabel;
    DatePickerTo: TDateTimePicker;
    TimePickerTo: TDateTimePicker;
    LabelTo: TLabel;
    Chart: TChart;
    sRangeMin: TLineSeries;
    sRangeMax: TLineSeries;
    sLeftTop: TLineSeries;
    sLeftBottom: TLineSeries;
    sRightTop: TLineSeries;
    sRightBottom: TLineSeries;
    LabelConveyor: TLabel;
    LabelSection: TLabel;
    ImageGraphLegend: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonApplyClick(Sender: TObject);
  private
    { Private declarations }
    FQuery : TZQuery;
    FDateTimeSince,
    FDateTimeTo : TDateTime;

    procedure ClearSeries();
    procedure DrawSeries(Series : TLineSeries);
    procedure DrawTempRanges;

    procedure SetSeriesSettings();

    procedure UpdateTimeRanges();
    procedure SetTimeRanges();

    procedure DrawChart();

    function GetSectionYMinValue() : single;
    function GetSectionYMaxValue() : single;

  public
    { Public declarations }
    SectionNumber,
    ConveyorNumber : integer;
  end;

var
  FormGraphHistory: TFormGraphHistory;

implementation

{$R *.dfm}

uses DateUtils, LApplicationGlobals;

function TimeToFirebirdString(const Date: TDateTime): string;
var
  Hour,
  Minute,
  Second,
  MSeconds : word;
  HourStr,
  MinuteStr,
  SecondStr : string;
  Delimiter : string;
begin
  Delimiter := ':';

  DecodeTime(Date, Hour, Minute, Second, MSeconds);

  if Hour < 10
    then HourStr := '0' + IntToStr(Hour)
    else HourStr := IntToStr(Hour);

  if Minute < 10
    then MinuteStr := '0' + IntToStr(Minute)
    else MinuteStr := IntToStr(Minute);

  if Second < 10
    then SecondStr := '0' + IntToStr(Second)
    else SecondStr := IntToStr(Second);

  Result := HourStr + Delimiter + MinuteStr + Delimiter + SecondStr;
end;


function DateToFirebirdString(const Date: TDateTime): string;
var
  Year,
  Day,
  Month : word;
  YearStr,
  DayStr,
  MonthStr : string;
  Delimiter : string;
begin
  Delimiter := '.';

  DecodeDate(Date, Year, Month, Day);

  YearStr := IntToStr(Year);
  if Month < 10
    then MonthStr := '0' + IntToStr(Month)
    else MonthStr := IntToStr(Month);

  if Day < 10
    then DayStr := '0' + IntToStr(Day)
    else DayStr := IntToStr(Day);

  Result := YearStr + Delimiter + MonthStr + Delimiter + DayStr;
end;


function DateTimeToFirebirdString(const Date: TDateTime): string;
begin
  Result := DateToFirebirdString(Date) + ' ' +
            TimeToFirebirdString(Date);
end;


procedure TFormGraphHistory.FormCreate(Sender: TObject);
begin
  Position := poDesktopCenter;

  FDateTimeTo    := Now;
  FDateTimeSince := FDateTimeTo - 1/24;

  SetTimeRanges;

  FQuery := TZQuery.Create(ApplicationDBConnection);
  FQuery.Connection := ApplicationDBConnection;
end;

procedure TFormGraphHistory.FormDestroy(Sender: TObject);
begin
  FQuery.Active := FALSE;
  FQuery.Free;
end;

procedure TFormGraphHistory.FormShow(Sender: TObject);
begin
  LabelConveyor.Caption := 'Floor: '   + IntToStr(ConveyorNumber);
  LabelSection.Caption  := 'Section: ' + IntToStr(SectionNumber);

  DrawChart;
end;

procedure TFormGraphHistory.DrawSeries(Series : TLineSeries);
const
  CMaxSeriesPointsCount = 30;
var
  SQLQueryText,
  SensorPosition : string;

  TempTime  : TDateTime;
  TempValue : single;

  CurrentRecordIndex,
  RecordCount,
  ShowRatio : integer;
begin
  SensorPosition := '';

  if Series.Name = 'sLeftTop'
    then SensorPosition := 'LeftTop';

  if Series.Name = 'sLeftBottom'
    then SensorPosition := 'LeftBottom';

  if Series.Name = 'sRightTop'
    then SensorPosition := 'RightTop';

  if Series.Name = 'sRightBottom'
    then SensorPosition := 'RightBottom';

  if SensorPosition = ''
    then Exit;

  SQLQueryText := 'SELECT TempTime, TempValue' + #13 +
                  'FROM   TempValues' + #13 +
                  'LEFT JOIN Sensors ON (TempValues.SensorId = Sensors.SensorId)' + sLineBreak +
                  'WHERE (ConveyorNumber=' + IntToStr(ConveyorNumber) + ') AND ' + sLineBreak +
                        '(SectionNumber='  + IntToStr(SectionNumber) + ') AND ' + sLineBreak +
                        '(SensorPosition=''' + SensorPosition + ''') AND ' + sLineBreak +
                        '(TempTime >= ''' + DateTimeToFirebirdString(FDateTimeSince) + ''') AND ' + sLineBreak +
                        '(TempTime <= ''' + DateTimeToFirebirdString(FDateTimeTo) + ''')' + sLineBreak +
                  'ORDER BY TempTime';

  FQuery.Active := False;

  FQuery.SQL.Text := SQLQueryText;

  FQuery.Active := True;

  RecordCount := FQuery.RecordCount;

  ShowRatio := 1;

  if RecordCount > CMaxSeriesPointsCount
    then ShowRatio := Round(RecordCount/CMaxSeriesPointsCount);

  CurrentRecordIndex := 1;

  while not FQuery.Eof do
    begin
      if (CurrentRecordIndex mod ShowRatio) = 0
        then
          begin
            TempTime  := FQuery.FieldByName('TempTime').AsDateTime;
            TempValue := FQuery.FieldByName('TempValue').AsFloat;

            Series.AddXY(TempTime, TempValue);
          end;

      Inc(CurrentRecordIndex);
      FQuery.Next;
    end;
end;


procedure TFormGraphHistory.DrawTempRanges;
var
  TempValue : Single;
begin
  TempValue := GetSectionYMinValue;

  sRangeMin.AddXY(FDateTimeSince, TempValue);
  sRangeMin.AddXY(FDateTimeTo,    TempValue);

  TempValue := GetSectionYMaxValue;

  sRangeMax.AddXY(FDateTimeSince, TempValue);
  sRangeMax.AddXY(FDateTimeTo,    TempValue);
end;

procedure TFormGraphHistory.SetSeriesSettings();
begin
  Chart.BottomAxis.DateTimeFormat := 'HH:mm';

  //Формат данных Дата/время для каждой серии
  sRangeMin.XValues.DateTime := TRUE;
  sRangeMax.XValues.DateTime := TRUE;

  sLeftTop.XValues.DateTime     := TRUE;
  sLeftBottom.XValues.DateTime  := TRUE;
  sRightTop.XValues.DateTime    := TRUE;
  sRightBottom.XValues.DateTime := TRUE;

  //Устанавливаем цвета для серий
  sRangeMin.Color := clRed;
  sRangeMax.Color := clRed;

  sLeftTop.Color     := clRed;
  sLeftBottom.Color  := clGreen;
  sRightTop.Color    := clBlue;
  sRightBottom.Color := clAqua;

  //Устанавливаем границы по шкале Y
  Chart.LeftAxis.Minimum := ApplicationProgramSettings.GraphSettings.AxisMinYValue;
  Chart.LeftAxis.Maximum := ApplicationProgramSettings.GraphSettings.AxisMaxYValue;
end;

procedure TFormGraphHistory.ButtonApplyClick(Sender: TObject);
var
  tmpDateTimeSince,
  tmpDateTimeTo : TDateTime;
begin
  tmpDateTimeSince := Trunc(DatePickerSince.Date) + TimeOf(TimePickerSince.Time);
  tmpDateTimeTo    := Trunc(DatePickerTo.Date)    + TimeOf(TimePickerTo.Time);

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

  DrawChart;
end;

procedure TFormGraphHistory.ClearSeries();
begin
  sRangeMin.Clear;
  sRangeMax.Clear;

  sLeftTop.Clear;
  sLeftBottom.Clear;
  sRightTop.Clear;
  sRightBottom.Clear;

  Application.ProcessMessages;
end;

procedure TFormGraphHistory.UpdateTimeRanges();
var
  SQLQueryText : string;
begin
  SQLQueryText := 'SELECT MIN(TempTime) as "MinTempTime", MAX(TempTime) as "MaxTempTime"' + #13 +
                  'FROM   TempValues' + #13 +
                  'LEFT JOIN Sensors ON (TempValues.SensorId = Sensors.SensorId)' + sLineBreak +
                  'WHERE (ConveyorNumber=' + IntToStr(ConveyorNumber) + ') AND ' + sLineBreak +
                        '(SectionNumber='  + IntToStr(SectionNumber) + ') AND ' + sLineBreak +
                        '(TempTime >= ''' + DateTimeToFirebirdString(FDateTimeSince) + ''') AND ' + sLineBreak +
                        '(TempTime <= ''' + DateTimeToFirebirdString(FDateTimeTo) + ''');';


  FQuery.Active := False;

  FQuery.SQL.Text := SQLQueryText;

  FQuery.Active := True;

  FDateTimeSince := 0;
  FDateTimeTo    := 0;

  while not FQuery.Eof do
    begin
      FDateTimeSince := FQuery.FieldByName('MinTempTime').AsDateTime;
      FDateTimeTo    := FQuery.FieldByName('MaxTempTime').AsDateTime;

      FQuery.Next;
    end;
end;

procedure TFormGraphHistory.DrawChart();
begin
  ClearSeries;

  SetSeriesSettings;

  UpdateTimeRanges;

  DrawSeries(sLeftTop);
  DrawSeries(sLeftBottom);
  DrawSeries(sRightTop);
  DrawSeries(sRightBottom);

  DrawTempRanges;

  SetTimeRanges;
end;

procedure TFormGraphHistory.SetTimeRanges();
begin
  if not (FDateTimeTo = 0)
    then
      begin
        DatePickerTo.DateTime := FDateTimeTo;
        TimePickerTo.DateTime := FDateTimeTo;
      end
    else
      begin
        DatePickerTo.DateTime := Now;
        TimePickerTo.DateTime := Now;
      end;


  if not (FDateTimeSince = 0)
    then
      begin
        DatePickerSince.DateTime := FDateTimeSince;
        TimePickerSince.DateTime := FDateTimeSince;
      end
    else
      begin
        DatePickerSince.DateTime := Now - 1/24;
        TimePickerSince.DateTime := Now - 1/24;
      end;
end;


function TFormGraphHistory.GetSectionYMinValue() : single;
var
  Value : single;
begin
  Value := 0;

  case SectionNumber of
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

function TFormGraphHistory.GetSectionYMaxValue() : single;
var
  Value : single;
begin
  Value := 0;

  case SectionNumber of
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

end.
