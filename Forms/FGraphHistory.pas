unit FGraphHistory;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, TeEngine, Series, TeeProcs, Chart,
  ZDataset, Gauges, FTerminalForm, Buttons;

type
  TFormGraphHistory = class(TFormTerminal)
    DatePickerSince: TDateTimePicker;
    LabelSince: TLabel;
    DatePickerTo: TDateTimePicker;
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
    Gauge: TGauge;
    LabeledEditSinceHours: TLabeledEdit;
    Label1: TLabel;
    LabeledEditSinceMinutes: TLabeledEdit;
    LabeledEditToHours: TLabeledEdit;
    Label2: TLabel;
    LabeledEditToMinutes: TLabeledEdit;
    BitBtnCancel: TBitBtn;
    BitBtnOk: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LabeledEditSinceHoursChange(Sender: TObject);
    procedure LabeledEditToHoursChange(Sender: TObject);
    procedure LabeledEditSinceMinutesChange(Sender: TObject);
    procedure LabeledEditToMinutesChange(Sender: TObject);
    procedure BitBtnOkClick(Sender: TObject);
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

    function CheckValues() : boolean;


  public
    { Public declarations }
    SectionNumber,
    ConveyorNumber : integer;
  end;

var
  FormGraphHistory: TFormGraphHistory;

implementation

{$R *.dfm}

uses DateUtils, LApplicationGlobals, CProgramSettings, LUtils,
  FUserDigitalKeyboard;


procedure TFormGraphHistory.FormCreate(Sender: TObject);
begin
  inherited;

  Position := poDesktopCenter;

  FDateTimeTo    := Now;
  FDateTimeSince := FDateTimeTo - 1/24;

  SetTimeRanges;

  FQuery := TZQuery.Create(ApplicationDBConnection);
  FQuery.Connection := ApplicationDBConnection;

  LabeledEditSinceHours.EditLabel.Caption   := '';
  LabeledEditSinceMinutes.EditLabel.Caption := '';
  LabeledEditToHours.EditLabel.Caption      := '';
  LabeledEditToMinutes.EditLabel.Caption    := '';
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

procedure TFormGraphHistory.LabeledEditSinceHoursChange(Sender: TObject);
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

        if value < 10
          then (Sender as TLabeledEdit).Text := '0' + IntToStr(value)
          else (Sender as TLabeledEdit).Text := IntToStr(value);
      end
    else
      (Sender as TLabeledEdit).Text := '0';
end;

procedure TFormGraphHistory.LabeledEditSinceMinutesChange(Sender: TObject);
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

        if value < 10
          then (Sender as TLabeledEdit).Text := '0' + IntToStr(value)
          else (Sender as TLabeledEdit).Text := IntToStr(value);
      end
    else
      (Sender as TLabeledEdit).Text := '0';
end;

procedure TFormGraphHistory.LabeledEditToHoursChange(Sender: TObject);
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

        if value < 10
          then (Sender as TLabeledEdit).Text := '0' + IntToStr(value)
          else (Sender as TLabeledEdit).Text := IntToStr(value);
      end
    else
      (Sender as TLabeledEdit).Text := '0';
end;

procedure TFormGraphHistory.LabeledEditToMinutesChange(Sender: TObject);
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

        if value < 10
          then (Sender as TLabeledEdit).Text := '0' + IntToStr(value)
          else (Sender as TLabeledEdit).Text := IntToStr(value);
      end
    else
      (Sender as TLabeledEdit).Text := '0';
end;

procedure TFormGraphHistory.DrawSeries(Series : TLineSeries);
const
  CMaxSeriesPointsCount = 30;
var
  SQLQueryText,
  SensorPosition : string;

  TempTime,
  AvgTempTime : TDateTime;

  TempValue,
  AvgTempValue : single;

  CurrentRecordIndex,
  RecordCount,
  AvgRatio,
  AvgCount : integer;
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

  //На графиках много значений не поместится.
  //Выводим только CMaxSeriesPointsCount с осреднением.
  AvgRatio := 1;

  if RecordCount > CMaxSeriesPointsCount
    then AvgRatio := Round(RecordCount/CMaxSeriesPointsCount);

  CurrentRecordIndex := 1;

  AvgTempTime  := 0;
  AvgTempValue := 0;
  AvgCount     := 0;

  while not FQuery.Eof do
    begin
      TempTime  := FQuery.FieldByName('TempTime').AsDateTime;
      TempValue := FQuery.FieldByName('TempValue').AsFloat;

      AvgTempTime  := AvgTempTime + TempTime;
      AvgTempValue := AvgTempValue + TempValue;
      Inc(AvgCount);

      if ((CurrentRecordIndex mod AvgRatio) = 0) or
         (CurrentRecordIndex = RecordCount)
        then
          begin
            AvgTempTime  := AvgTempTime/AvgCount;
            AvgTempValue := AvgTempValue/AvgCount;

            Series.AddXY(AvgTempTime, AvgTempValue);

            AvgTempTime  := 0;
            AvgTempValue := 0;
            AvgCount     := 0;
          end;

      Inc(CurrentRecordIndex);
      FQuery.Next;
    end;
end;


procedure TFormGraphHistory.DrawTempRanges;
var
  TempValue : Single;
begin
  TempValue := GetSectionYMinValue(SectionNumber);

  sRangeMin.AddXY(FDateTimeSince, TempValue);
  sRangeMin.AddXY(FDateTimeTo,    TempValue);

  TempValue := GetSectionYMaxValue(SectionNumber);

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

procedure TFormGraphHistory.BitBtnOkClick(Sender: TObject);
begin
  if CheckValues
    then DrawChart;
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

  Gauge.Visible := True;
  Gauge.MaxValue := 7;      Gauge.Progress := 0;

  SetSeriesSettings;        Gauge.Progress := 1;

  UpdateTimeRanges;         Gauge.Progress := 2;

  DrawSeries(sLeftTop);     Gauge.Progress := 3;
  DrawSeries(sLeftBottom);  Gauge.Progress := 4;
  DrawSeries(sRightTop);    Gauge.Progress := 5;
  DrawSeries(sRightBottom); Gauge.Progress := 6;

  DrawTempRanges;

  SetTimeRanges;            Gauge.Progress := 7;

  Sleep(250);

  Gauge.Visible := False;
end;

procedure TFormGraphHistory.SetTimeRanges();
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
        DatePickerSince.DateTime := Now - 1/24;

        LabeledEditSinceHours.Text   := IntToStr(DecodeHour(Now - 1/24));
        LabeledEditSinceMinutes.Text := IntToStr(DecodeMinute(Now - 1/24));
      end;
end;

function TFormGraphHistory.CheckValues() : boolean;
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




end.
